(* sml-git demo: open the real git fixture objects committed under
   test/fixtures/ and walk them with the pure library -- decode a loose
   commit, print its tree/parent/message, verify its oid reproduces git's,
   then reconstruct a delta-encoded blob straight out of a packfile and
   confirm ITS oid too. Output is printed and fully deterministic. *)

fun line s = print (s ^ "\n")

fun readFile path =
  let
    val ins = BinIO.openIn path
    val bytes = BinIO.inputAll ins
    val () = BinIO.closeIn ins
  in Byte.bytesToString bytes end

val fixtures = "test/fixtures/"
fun loosePath oid =
  fixtures ^ "loose/" ^ String.substring (oid, 0, 2) ^ "/"
          ^ String.substring (oid, 2, String.size oid - 2)

val headCommit = "94c43da0c98a3a96c58f00d6e5a06aa70c0dd410"
val bigV1Blob  = "f0d872f6993b3b84102595978057dc29962f45a6"

val () = line "sml-git demo"
val () = line "============"

(* ---- a loose commit object ---- *)
val () = line ("loose object    : " ^ headCommit)
val commit = Git.decodeLoose (readFile (loosePath headCommit))
val () =
  case commit of
      Git.Commit {tree, parents, author, committer, message} =>
        ( line ("  type          : " ^ Git.objectType commit)
        ; line ("  tree          : " ^ tree)
        ; line ("  parent        : " ^ String.concatWith ", " parents)
        ; line ("  author        : " ^ author)
        ; line ("  message       : " ^ message)
        ; line ("  hashObject    : " ^ Git.hashObject commit)
        ; line ("  oid matches   : " ^ Bool.toString (Git.hashObject commit = headCommit)) )
    | _ => line "  (not a commit?!)"

(* ---- walk the commit's tree ---- *)
val () = line ""
val () =
  case commit of
      Git.Commit {tree, ...} =>
        (case Git.decodeLoose (readFile (loosePath tree)) of
             Git.Tree entries =>
               ( line ("tree " ^ tree ^ " :")
               ; List.app
                   (fn {mode, name, id} =>
                      line ("  " ^ mode ^ " " ^ id ^ "  " ^ name))
                   entries )
           | _ => ())
    | _ => ()

(* ---- reconstruct a delta-encoded blob from the packfile ---- *)
val () = line ""
val pack =
  Git.Pack.parse
    { pack = readFile (fixtures ^ "pack-ofs/pack.pack")
    , idx  = readFile (fixtures ^ "pack-ofs/pack.idx") }
val () = line ("packfile        : " ^ Int.toString (Git.Pack.count pack)
               ^ " objects (version " ^ Int.toString (Git.Pack.version pack) ^ ")")
val () =
  case Git.Pack.lookup pack bigV1Blob of
      SOME obj =>
        ( line ("  delta object  : " ^ bigV1Blob)
        ; line ("  reconstructed : " ^ Int.toString (String.size (Git.payload obj)) ^ " bytes")
        ; line ("  oid matches   : " ^ Bool.toString (Git.hashObject obj = bigV1Blob)) )
    | NONE => line "  (delta object missing?!)"

fun main () = ()
