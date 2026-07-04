(* git.sml -- pure Standard ML git plumbing (object/pack/index/ref formats).

   See git.sig for the contract. The single dependency is the vendored
   sml-deflate, which supplies `Zlib.inflateZlib`/`Zlib.deflateZlib` (string to
   string) for git's zlib streams and `Sha1`/`Base16` (via the bundled
   sml-codec) for object ids and hex.

   Everything is byte-string in / byte-string out and deterministic. Integers
   stay small (MLton's default Int is 32-bit), so the binary readers avoid any
   value >= 2^31; 64-bit packfile offsets (packs > 2 GiB) are explicitly
   rejected rather than silently overflowing. The one size field parsed from
   untrusted decimal ASCII (a loose object's header size) is parsed through
   arbitrary-precision IntInf and range-checked against the fixed 32-bit bound,
   so an oversized size is a clean `Git` rejection rather than an Overflow
   crash that would diverge between compilers. *)

structure Git :> GIT =
struct
  exception Git of string

  type treeEntry = { mode : string, name : string, id : string }
  type commit =
    { tree : string, parents : string list, author : string
    , committer : string, message : string }
  type tag =
    { object : string, typ : string, tag : string
    , tagger : string, message : string }

  datatype obj =
      Blob of string
    | Tree of treeEntry list
    | Commit of commit
    | Tag of tag

  (* ---- byte / hex helpers ----------------------------------------------- *)

  fun byte s i = Char.ord (String.sub (s, i))

  (* big-endian unsigned reads. Only used on values that fit in a 32-bit Int
     within this library's scope (counts, small sizes/offsets). *)
  fun be16 s i = byte s i * 256 + byte s (i + 1)
  fun be32 s i =
    byte s i * 16777216 + byte s (i + 1) * 65536
    + byte s (i + 2) * 256 + byte s (i + 3)

  (* b << n, for byte-sized b and small n; via Word to stay portable. *)
  fun shl (b, n) = Word.toInt (Word.<< (Word.fromInt b, Word.fromInt n))

  fun findChar (s, c, start) =
    let
      val n = String.size s
      fun go i = if i >= n then ~1 else if String.sub (s, i) = c then i else go (i + 1)
    in go start end

  fun hexToRaw id =
    case Base16.decode id of
        SOME r => if String.size r = 20 then r else raise Git "oid must be 20 bytes"
      | NONE => raise Git ("malformed oid hex: " ^ id)
  val rawToHex = Base16.encode

  (* ---- object framing / hashing ----------------------------------------- *)

  fun objectType (Blob _)   = "blob"
    | objectType (Tree _)   = "tree"
    | objectType (Commit _) = "commit"
    | objectType (Tag _)    = "tag"

  fun payload (Blob s) = s
    | payload (Tree entries) =
        String.concat
          (List.map
             (fn {mode, name, id} => mode ^ " " ^ name ^ "\000" ^ hexToRaw id)
             entries)
    | payload (Commit {tree, parents, author, committer, message}) =
        String.concat
          ([ "tree ", tree, "\n" ]
           @ List.map (fn p => "parent " ^ p ^ "\n") parents
           @ [ "author ", author, "\n", "committer ", committer, "\n", "\n", message ])
    | payload (Tag {object, typ, tag, tagger, message}) =
        String.concat
          [ "object ", object, "\n", "type ", typ, "\n", "tag ", tag, "\n"
          , "tagger ", tagger, "\n", "\n", message ]

  fun serialize obj =
    let val p = payload obj
    in objectType obj ^ " " ^ Int.toString (String.size p) ^ "\000" ^ p end

  fun hashObject obj = Sha1.hexDigest (serialize obj)

  fun encodeLoose obj = Zlib.deflateZlib {level = 6} (serialize obj)

  (* ---- payload parsing --------------------------------------------------- *)

  fun parseTree raw =
    let
      val n = String.size raw
      fun loop (pos, acc) =
        if pos >= n then List.rev acc
        else
          let
            val sp = findChar (raw, #" ", pos)
            val () = if sp < 0 then raise Git "tree: missing mode separator" else ()
            val mode = String.substring (raw, pos, sp - pos)
            val nul = findChar (raw, #"\000", sp + 1)
            val () = if nul < 0 then raise Git "tree: missing name terminator" else ()
            val name = String.substring (raw, sp + 1, nul - (sp + 1))
            val idStart = nul + 1
            val () = if idStart + 20 > n then raise Git "tree: truncated entry id" else ()
            val id = rawToHex (String.substring (raw, idStart, 20))
          in
            loop (idStart + 20, {mode = mode, name = name, id = id} :: acc)
          end
    in loop (0, []) end

  (* split a payload into (header, body) at the first blank line. *)
  fun splitHeaderBody s =
    let
      val n = String.size s
      fun go i =
        if i + 1 >= n then NONE
        else if String.sub (s, i) = #"\n" andalso String.sub (s, i + 1) = #"\n"
        then SOME i else go (i + 1)
    in
      case go 0 of
          SOME i => (String.substring (s, 0, i), String.substring (s, i + 2, n - (i + 2)))
        | NONE => (s, "")
    end

  fun splitKV line =
    case findChar (line, #" ", 0) of
        ~1 => (line, "")
      | i => (String.substring (line, 0, i),
              String.substring (line, i + 1, String.size line - (i + 1)))

  fun headerLines h = String.fields (fn c => c = #"\n") h

  fun parseCommit raw =
    let
      val (h, body) = splitHeaderBody raw
      val tree = ref ""
      val parents = ref ([] : string list)
      val author = ref ""
      val committer = ref ""
      fun handleLine line =
        if line = "" then ()
        else if String.sub (line, 0) = #" " then ()  (* header continuation *)
        else
          let val (k, v) = splitKV line in
            case k of
                "tree" => tree := v
              | "parent" => parents := v :: !parents
              | "author" => author := v
              | "committer" => committer := v
              | _ => ()
          end
      val () = List.app handleLine (headerLines h)
    in
      { tree = !tree, parents = List.rev (!parents)
      , author = !author, committer = !committer, message = body }
    end

  fun parseTag raw =
    let
      val (h, body) = splitHeaderBody raw
      val object = ref ""
      val typ = ref ""
      val tag = ref ""
      val tagger = ref ""
      fun handleLine line =
        if line = "" then ()
        else if String.sub (line, 0) = #" " then ()
        else
          let val (k, v) = splitKV line in
            case k of
                "object" => object := v
              | "type" => typ := v
              | "tag" => tag := v
              | "tagger" => tagger := v
              | _ => ()
          end
      val () = List.app handleLine (headerLines h)
    in
      { object = !object, typ = !typ, tag = !tag, tagger = !tagger, message = body }
    end

  fun parseObject {typ, payload = p} =
    case typ of
        "blob" => Blob p
      | "tree" => Tree (parseTree p)
      | "commit" => Commit (parseCommit p)
      | "tag" => Tag (parseTag p)
      | _ => raise Git ("unknown object type: " ^ typ)

  fun parseFramed framed =
    let
      val sp = findChar (framed, #" ", 0)
      val () = if sp < 0 then raise Git "object: missing type/size separator" else ()
      val typ = String.substring (framed, 0, sp)
      val nul = findChar (framed, #"\000", sp + 1)
      val () = if nul < 0 then raise Git "object: missing header terminator" else ()
      val sizeStr = String.substring (framed, sp + 1, nul - (sp + 1))
      (* The header size is unbounded decimal ASCII, so a corrupt or hostile
         object can carry a value past 2^31. It is only ever compared to
         `String.size p` (a machine `int`, since no in-memory string can be
         larger), so it stays a bounded `int` -- but we must not let
         `Int.fromString` raise `Overflow` on MLton's 32-bit `int` (a crash that
         also diverges from Poly/ML's 63-bit `int`). Parse through
         arbitrary-precision `IntInf` and range-check against the FIXED 32-bit
         signed range, rejecting anything out of range as a clean `Git` error --
         identically on both compilers. *)
      val size =
        case IntInf.fromString sizeStr of
            NONE => raise Git "object: malformed size"
          | SOME k =>
              if k >= 0 andalso k <= 2147483647
              then IntInf.toInt k
              else raise Git "object: size out of range"
      val p = String.substring (framed, nul + 1, String.size framed - (nul + 1))
      val () = if String.size p <> size then raise Git "object: size mismatch" else ()
    in parseObject {typ = typ, payload = p} end

  fun decodeLoose s =
    case Zlib.inflateZlib s of
        NONE => raise Git "loose object: zlib inflate failed"
      | SOME framed => parseFramed framed

  (* ---- packfiles --------------------------------------------------------- *)

  structure Pack =
  struct
    type pack = { version : int, count : int, objs : (string * obj) list }

    (* variable-length LEB128, little-endian, low group first *)
    fun rdVarint (s, pos) =
      let
        fun go (pos, shift, v) =
          let
            val b = byte s pos
            val v' = v + shl (b mod 128, shift)
          in if b >= 128 then go (pos + 1, shift + 7, v') else (v', pos + 1) end
      in go (pos, 0, 0) end

    (* pack object header: type (3 bits) + size (rest), size varint LSB-first
       with a 4-bit seed in the first byte. Returns (type, size, nextPos). *)
    fun rdObjHeader (s, pos) =
      let
        val b0 = byte s pos
        val typ = (b0 div 16) mod 8
        fun go (pos, shift, v) =
          let
            val b = byte s pos
            val v' = v + shl (b mod 128, shift)
          in if b >= 128 then go (pos + 1, shift + 7, v') else (v', pos + 1) end
      in
        if b0 >= 128
        then let val (sz, p) = go (pos + 1, 4, b0 mod 16) in (typ, sz, p) end
        else (typ, b0 mod 16, pos + 1)
      end

    (* ofs-delta negative base offset encoding. Returns (offset, nextPos). *)
    fun rdOfs (s, pos) =
      let
        val b0 = byte s pos
        fun go (pos, v) =
          let
            val b = byte s pos
            val v' = (v + 1) * 128 + (b mod 128)
          in if b >= 128 then go (pos + 1, v') else (v', pos + 1) end
      in
        if b0 >= 128 then go (pos + 1, b0 mod 128) else (b0 mod 128, pos + 1)
      end

    (* apply a git delta (RFC: copy / insert instructions) to a base string. *)
    fun applyDelta (base, delta) =
      let
        val dn = String.size delta
        val (_, p1) = rdVarint (delta, 0)       (* source size  *)
        val (_, p2) = rdVarint (delta, p1)      (* target size  *)
        fun loop (pos, acc) =
          if pos >= dn then String.concat (List.rev acc)
          else
            let val opc = byte delta pos in
              if opc >= 128 then
                let
                  fun rd (m, sh, (pos, v)) =
                    if Word.andb (Word.fromInt opc, m) <> 0w0
                    then (pos + 1, v + shl (byte delta pos, sh))
                    else (pos, v)
                  val st = (pos + 1, 0)
                  val (posO, cpOff) =
                    rd (0wx8, 24, rd (0wx4, 16, rd (0wx2, 8, rd (0wx1, 0, st))))
                  val (posS, cpSz0) =
                    rd (0wx40, 16, rd (0wx20, 8, rd (0wx10, 0, (posO, 0))))
                  val cpSize = if cpSz0 = 0 then 65536 else cpSz0
                in
                  loop (posS, String.substring (base, cpOff, cpSize) :: acc)
                end
              else if opc > 0 then
                loop (pos + 1 + opc, String.substring (delta, pos + 1, opc) :: acc)
              else raise Git "delta: reserved zero opcode"
            end
      in loop (p2, []) end

    fun sortInts xs =
      let
        fun ins (x, []) = [x]
          | ins (x, y :: ys) = if x <= y then x :: y :: ys else y :: ins (x, ys)
      in List.foldr (fn (x, acc) => ins (x, acc)) [] xs end

    fun parse {pack, idx} =
      let
        (* ---- pack header ---- *)
        val () =
          if String.size pack < 12 orelse String.substring (pack, 0, 4) <> "PACK"
          then raise Git "pack: bad signature" else ()
        val version = be32 pack 4
        val () = if version <> 2 then raise Git "pack: unsupported version" else ()
        val nObj = be32 pack 8
        val packBodyEnd = String.size pack - 20   (* trailing pack checksum *)

        (* ---- idx v2 ---- *)
        val () =
          if String.size idx < 8
             orelse not (byte idx 0 = 255 andalso byte idx 1 = 116
                         andalso byte idx 2 = 79 andalso byte idx 3 = 99)
          then raise Git "idx: bad magic (only v2 supported)" else ()
        val () = if be32 idx 4 <> 2 then raise Git "idx: unsupported version" else ()
        val n = be32 idx (8 + 255 * 4)            (* fanout[255] = object count *)
        val oidBase = 8 + 256 * 4
        val crcBase = oidBase + n * 20
        val offBase = crcBase + n * 4
        fun idxOid i = rawToHex (String.substring (idx, oidBase + i * 20, 20))
        fun idxOff i =
          (* high bit set => index into the 8-byte large-offset table, which
             only occurs for packs > 2 GiB (not representable in a 32-bit Int
             anyway). Detect via the top byte to avoid a 2^31 literal. *)
          if byte idx (offBase + i * 4) >= 128
          then raise Git "pack: 64-bit offsets unsupported (pack > 2 GiB)"
          else be32 idx (offBase + i * 4)

        val entries = List.tabulate (n, fn i => (idxOid i, idxOff i))
        val offToOid = List.map (fn (oid, off) => (off, oid)) entries
        val sortedOffs = sortInts (List.map #2 entries)

        fun nextOffset off =
          let
            fun go [] = packBodyEnd
              | go (x :: xs) = if x > off then x else go xs
          in go sortedOffs end

        fun oidToOff oid =
          case List.find (fn (k, _) => k = oid) entries of
              SOME (_, off) => off
            | NONE => raise Git "pack: ref-delta base not present in pack"
        fun oidOfOff off =
          case List.find (fn (k, _) => k = off) offToOid of
              SOME (_, oid) => oid
            | NONE => raise Git "pack: offset has no oid in idx"

        fun inflateRegion (a, b) =
          case Zlib.inflateZlib (String.substring (pack, a, b - a)) of
              SOME x => x
            | NONE => raise Git "pack: zlib inflate failed"

        val memo = ref ([] : (int * (string * string)) list)

        fun resolveOff off =
          case List.find (fn (k, _) => k = off) (!memo) of
              SOME (_, r) => r
            | NONE => let val r = reconstruct off
                      in memo := (off, r) :: !memo; r end

        and reconstruct off =
          let
            val (typ, _, dataStart) = rdObjHeader (pack, off)
            val rend = nextOffset off
          in
            case typ of
                1 => ("commit", inflateRegion (dataStart, rend))
              | 2 => ("tree",   inflateRegion (dataStart, rend))
              | 3 => ("blob",   inflateRegion (dataStart, rend))
              | 4 => ("tag",    inflateRegion (dataStart, rend))
              | 6 =>
                  let
                    val (neg, p) = rdOfs (pack, dataStart)
                    val delta = inflateRegion (p, rend)
                    val (bt, bp) = resolveOff (off - neg)
                  in (bt, applyDelta (bp, delta)) end
              | 7 =>
                  let
                    val baseOid = rawToHex (String.substring (pack, dataStart, 20))
                    val delta = inflateRegion (dataStart + 20, rend)
                    val (bt, bp) = resolveOff (oidToOff baseOid)
                  in (bt, applyDelta (bp, delta)) end
              | _ => raise Git ("pack: unknown object type " ^ Int.toString typ)
          end

        val ordered = sortInts (List.map #2 entries)
        val objs =
          List.map
            (fn off =>
               let val (t, p) = resolveOff off
               in (oidOfOff off, parseObject {typ = t, payload = p}) end)
            ordered
      in
        { version = version, count = nObj, objs = objs }
      end

    fun objects (p : pack) = #objs p
    fun lookup (p : pack) oid =
      Option.map #2 (List.find (fn (k, _) => k = oid) (#objs p))
    fun count (p : pack) = #count p
    fun version (p : pack) = #version p
  end

  (* ---- index (dircache, v2) ---------------------------------------------- *)

  structure Index =
  struct
    type entry = { path : string, id : string, mode : int, size : int }

    fun version s =
      if String.size s < 12 orelse String.substring (s, 0, 4) <> "DIRC"
      then raise Git "index: bad signature" else be32 s 4

    fun parse s =
      let
        val () =
          if String.size s < 12 orelse String.substring (s, 0, 4) <> "DIRC"
          then raise Git "index: bad signature" else ()
        val ver = be32 s 4
        val () = if ver < 2 orelse ver > 3
                 then raise Git ("index: unsupported version " ^ Int.toString ver)
                 else ()
        val count = be32 s 8
        fun entryAt pos =
          let
            val mode = be32 s (pos + 24)
            val size = be32 s (pos + 36)
            val id = rawToHex (String.substring (s, pos + 40, 20))
            val flags = be16 s (pos + 60)
            val nameLenField = flags mod 4096          (* low 12 bits *)
            val nameStart = pos + 62
            val nameLen =
              if nameLenField < 4095 then nameLenField
              else findChar (s, #"\000", nameStart) - nameStart
            val name = String.substring (s, nameStart, nameLen)
            (* v2/v3 entries are padded so (62 + nameLen + 8) rounds down to a
               multiple of 8 -- i.e. 1..8 NUL bytes, name kept NUL-terminated. *)
            val b = 62 + nameLen + 8
            val entryLen = b - (b mod 8)
          in
            ({ path = name, id = id, mode = mode, size = size }, pos + entryLen)
          end
        fun loop (0, _, acc) = List.rev acc
          | loop (k, pos, acc) =
              let val (e, next) = entryAt pos
              in loop (k - 1, next, e :: acc) end
      in
        loop (count, 12, [])
      end
  end

  (* ---- refs -------------------------------------------------------------- *)

  structure Ref =
  struct
    datatype ref = Direct of string | Symbolic of string

    fun isWS c = c = #" " orelse c = #"\n" orelse c = #"\r" orelse c = #"\t"
    fun trim s =
      Substring.string (Substring.dropr isWS (Substring.dropl isWS (Substring.full s)))

    fun parse s =
      let val t = trim s in
        if String.isPrefix "ref:" t
        then Symbolic (trim (String.extract (t, 4, NONE)))
        else Direct t
      end

    fun parsePacked s =
      let
        val lines = String.fields (fn c => c = #"\n") s
        fun step (line, acc) =
          let val t = trim line in
            if t = "" orelse String.sub (t, 0) = #"#" orelse String.sub (t, 0) = #"^"
            then acc
            else
              case findChar (t, #" ", 0) of
                  ~1 => acc
                | i =>
                    let
                      val oid = String.substring (t, 0, i)
                      val name = trim (String.extract (t, i + 1, NONE))
                    in (name, oid) :: acc end
          end
      in List.rev (List.foldl step [] lines) end
  end
end
