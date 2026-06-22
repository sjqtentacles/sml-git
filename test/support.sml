(* support.sml -- shared helpers + the expected values for the REAL git
   fixtures committed under test/fixtures/.

   The fixtures were produced by `test/fixtures/generate.sh` with pinned
   identities and dates (GIT_AUTHOR_DATE = GIT_COMMITTER_DATE = 1700000000),
   so every object id below is exactly what the system `git` computed:

     hello.txt blob   ce013625030ba8dba906f756967f9e9ca394464a
     docs/note.txt    4dadd7266be5a243edf601ed1f94461de3feae89
     big.txt (v2)     ee16ef7b005794b50717c165021d16044454c00b   (HEAD)
     big.txt (v1)     f0d872f6993b3b84102595978057dc29962f45a6   (delta base = v2)
     root tree (HEAD) 86fa20b61dbf28683a0ae91e87fcdab4e0854186
     root tree (v1)   8aa0f6dbf7d1d079e8a7adc3f404253bd923bec5
     docs tree        ebe2cbff43af78278245c9487c67456f2f3eb4e0
     HEAD commit      94c43da0c98a3a96c58f00d6e5a06aa70c0dd410
     first commit     0f0b31f0d114015c2b3de56e88c2781c8564b2de
     tag v1.0         3d9b4020689662d5f97193db44b2fa43482d8459

   The fixtures are read from disk relative to the repository root (where the
   test binary is invoked by the Makefile). *)

structure Support =
struct
  (* read a file as raw bytes (one char per byte) *)
  fun readFile path =
    let
      val ins = BinIO.openIn path
      val bytes = BinIO.inputAll ins
      val () = BinIO.closeIn ins
    in
      Byte.bytesToString bytes
    end

  val fixtures = "test/fixtures/"

  (* path of a loose object given its 40-hex oid *)
  fun loosePath oid =
    fixtures ^ "loose/" ^ String.substring (oid, 0, 2) ^ "/"
            ^ String.substring (oid, 2, String.size oid - 2)

  fun readLoose oid = readFile (loosePath oid)

  (* ---- expected oids ---- *)
  val helloBlob   = "ce013625030ba8dba906f756967f9e9ca394464a"
  val noteBlob    = "4dadd7266be5a243edf601ed1f94461de3feae89"
  val bigV2Blob   = "ee16ef7b005794b50717c165021d16044454c00b"
  val bigV1Blob   = "f0d872f6993b3b84102595978057dc29962f45a6"
  val headTree    = "86fa20b61dbf28683a0ae91e87fcdab4e0854186"
  val firstTree   = "8aa0f6dbf7d1d079e8a7adc3f404253bd923bec5"
  val docsTree    = "ebe2cbff43af78278245c9487c67456f2f3eb4e0"
  val headCommit  = "94c43da0c98a3a96c58f00d6e5a06aa70c0dd410"
  val firstCommit = "0f0b31f0d114015c2b3de56e88c2781c8564b2de"
  val tagObj      = "3d9b4020689662d5f97193db44b2fa43482d8459"

  (* the every-object inventory of the packfiles (10 objects) *)
  val allOids =
    [ headCommit, firstCommit, tagObj, headTree, firstTree, docsTree
    , bigV2Blob, bigV1Blob, noteBlob, helloBlob ]

  (* ---- expected line values (verbatim as stored in the objects) ---- *)
  val author    = "Fixture Author <author@example.com> 1700000000 +0000"
  val committer = "Fixture Committer <committer@example.com> 1700000000 +0000"
  val tagger    = "Fixture Committer <committer@example.com> 1700000000 +0000"

  fun objType (Git.Blob _)   = "blob"
    | objType (Git.Tree _)   = "tree"
    | objType (Git.Commit _) = "commit"
    | objType (Git.Tag _)    = "tag"

  fun optStr NONE = "NONE"
    | optStr (SOME _) = "SOME"
end
