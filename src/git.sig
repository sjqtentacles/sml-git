(* git.sig

   Pure Standard ML git plumbing: the object/pack/index/ref *formats*.

   This library is the byte-level FORMAT layer that every git implementation
   shares - it has NO networking (a fetch/push transport would be a separate,
   quarantined IO tool). Everything here is a pure, deterministic codec over
   byte strings, so it is testable against real `git` output and is
   byte-identical under MLton and Poly/ML.

   Conventions
   -----------
   - Bytes are `string`: one character per byte, 0-255, exactly as in the rest
     of the sjqtentacles crypto/codec family (`Sha1.digest`, `Zlib.*`, ...).
   - Object ids ("oids") are 40-character lowercase hex strings, exactly the
     form `git rev-parse` / `git hash-object` print. The 20 raw bytes that git
     stores inside tree entries and pack ref-deltas are converted to/from hex
     at the library boundary, so callers only ever see hex.
   - Parsing of malformed input raises `Git`. Total/optional variants are noted.
*)

signature GIT =
sig
  exception Git of string

  (* ---- object model ------------------------------------------------------ *)

  (* A tree entry: an octal mode string as git stores it ("100644", "40000",
     "100755", "120000", "160000"), the entry name, and the referenced oid. *)
  type treeEntry = { mode : string, name : string, id : string }

  type commit =
    { tree : string                 (* oid of the root tree *)
    , parents : string list          (* 0 (root), 1 (normal) or >1 (merge) oids *)
    , author : string                (* the full "Name <email> <when>" line value *)
    , committer : string
    , message : string }             (* the commit message (everything after the blank line) *)

  type tag =
    { object : string                (* oid the tag points at *)
    , typ : string                   (* "commit" | "tree" | "blob" | "tag" *)
    , tag : string                   (* the tag name *)
    , tagger : string
    , message : string }

  datatype obj =
      Blob of string                 (* raw blob bytes *)
    | Tree of treeEntry list
    | Commit of commit
    | Tag of tag

  (* "blob" | "tree" | "commit" | "tag" *)
  val objectType : obj -> string

  (* ---- hashing / loose objects ------------------------------------------- *)

  (* The raw, UNcompressed canonical object bytes: "<type> <len>\000<payload>". *)
  val serialize  : obj -> string
  (* Just the payload (no "<type> <len>\000" header). *)
  val payload    : obj -> string
  (* The 40-hex SHA-1 object id over `serialize obj`, identical to git's oid. *)
  val hashObject : obj -> string

  (* A loose object as stored on disk: zlib-compressed `serialize obj`. *)
  val encodeLoose : obj -> string
  (* Inflate + parse a loose object's on-disk bytes. Raises `Git` if malformed. *)
  val decodeLoose : string -> obj

  (* ---- parsing ----------------------------------------------------------- *)

  (* Parse a raw (already-unframed) payload given its type string. *)
  val parseObject : { typ : string, payload : string } -> obj
  (* Parse a raw tree payload into its entries. *)
  val parseTree   : string -> treeEntry list
  (* Parse a raw commit payload. *)
  val parseCommit : string -> commit
  (* Parse a raw tag payload. *)
  val parseTag    : string -> tag

  (* ---- packfiles --------------------------------------------------------- *)

  structure Pack :
  sig
    type pack

    (* Parse a packfile given the `.pack` bytes and its companion `.idx`
       bytes. The idx supplies each object's byte offset (used to bound the
       per-object zlib streams) and the authoritative oid list; ofs-deltas and
       ref-deltas are fully reconstructed against their base objects. Raises
       `Git` on malformed input. *)
    val parse   : { pack : string, idx : string } -> pack

    (* The fully-reconstructed objects as (oid, object) pairs, in pack order.
       Each oid is recomputed with `hashObject` and equals git's oid. *)
    val objects : pack -> (string * obj) list

    (* Look up a single reconstructed object by its 40-hex oid. *)
    val lookup  : pack -> string -> obj option

    val count   : pack -> int        (* number of objects *)
    val version : pack -> int        (* pack version (2) *)
  end

  (* ---- index (dircache / .git/index) ------------------------------------- *)

  structure Index :
  sig
    (* path = repo-relative path; id = 40-hex oid; mode = raw 32-bit mode
       (e.g. 33188 = 0o100644); size = file size in bytes. *)
    type entry = { path : string, id : string, mode : int, size : int }

    (* Parse a v2 `.git/index` (dircache). Raises `Git` on malformed input. *)
    val parse   : string -> entry list
    val version : string -> int      (* index format version (2/3/4) *)
  end

  (* ---- refs -------------------------------------------------------------- *)

  structure Ref :
  sig
    datatype ref =
        Direct of string             (* a 40-hex oid *)
      | Symbolic of string           (* a symref target, e.g. "refs/heads/main" *)

    (* Parse the contents of a single loose ref file (`refs/heads/main`,
       `HEAD`, ...): either "ref: <target>" or a raw 40-hex oid. *)
    val parse : string -> ref

    (* Parse a `packed-refs` file into (refname, oid) pairs. Comment lines and
       peeled-tag ("^<oid>") lines are skipped. *)
    val parsePacked : string -> (string * string) list
  end
end
