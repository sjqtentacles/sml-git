(* test_object.sml -- loose object model: hashing, canonical framing, parsing,
   and round-trips, all checked against the real git fixtures. *)

structure ObjectTests =
struct
  open Support
  structure H = Harness

  val helloObj = Git.Blob "hello\n"

  val headCommitObj =
    Git.Commit
      { tree = headTree
      , parents = [firstCommit]
      , author = author
      , committer = committer
      , message = "second commit: edit big.txt and add docs/note.txt\n" }

  val firstCommitObj =
    Git.Commit
      { tree = firstTree
      , parents = []
      , author = author
      , committer = committer
      , message = "initial commit\n" }

  val tagObjVal =
    Git.Tag
      { object = headCommit
      , typ = "commit"
      , tag = "v1.0"
      , tagger = tagger
      , message = "release v1.0\n" }

  val headTreeObj =
    Git.Tree
      [ { mode = "100644", name = "big.txt",   id = bigV2Blob }
      , { mode = "40000",  name = "docs",      id = docsTree  }
      , { mode = "100644", name = "hello.txt", id = helloBlob } ]

  val docsTreeObj =
    Git.Tree [ { mode = "100644", name = "note.txt", id = noteBlob } ]

  fun run () =
    ( H.section "object: hashing matches git's oids"
    ; H.checkString "hashObject (Blob \"hello\\n\") = git oid"
        (helloBlob, Git.hashObject helloObj)
    ; H.checkString "serialize (Blob \"hello\\n\") canonical framing"
        ("blob 6\000hello\n", Git.serialize helloObj)
    ; H.checkString "payload (Blob \"hello\\n\")" ("hello\n", Git.payload helloObj)
    ; H.checkString "objectType blob"   ("blob", Git.objectType helloObj)
    ; H.checkString "objectType tree"   ("tree", Git.objectType headTreeObj)
    ; H.checkString "objectType commit" ("commit", Git.objectType headCommitObj)
    ; H.checkString "objectType tag"    ("tag", Git.objectType tagObjVal)

    ; H.section "object: decode real loose objects + reproduce oids"
    (* The killer check: for every object git wrote, inflating + parsing +
       re-hashing must reproduce git's 40-hex oid byte-for-byte. *)
    ; List.app
        (fn oid =>
           H.checkString ("hashObject (decodeLoose loose/" ^ oid ^ ") = oid")
             (oid, Git.hashObject (Git.decodeLoose (readLoose oid))))
        allOids

    ; H.section "object: parse real loose objects"
    ; H.checkEq "decodeLoose hello blob" (helloObj, Git.decodeLoose (readLoose helloBlob))
    ; H.checkEq "decodeLoose HEAD commit" (headCommitObj, Git.decodeLoose (readLoose headCommit))
    ; H.checkEq "decodeLoose first commit" (firstCommitObj, Git.decodeLoose (readLoose firstCommit))
    ; H.checkEq "decodeLoose tag v1.0" (tagObjVal, Git.decodeLoose (readLoose tagObj))
    ; H.checkEq "decodeLoose HEAD tree" (headTreeObj, Git.decodeLoose (readLoose headTree))
    ; H.checkEq "decodeLoose docs tree" (docsTreeObj, Git.decodeLoose (readLoose docsTree))

    ; H.section "object: parse raw payloads directly"
    ; H.checkEq "parseTree HEAD tree payload"
        ( [ { mode = "100644", name = "big.txt",   id = bigV2Blob }
          , { mode = "40000",  name = "docs",      id = docsTree  }
          , { mode = "100644", name = "hello.txt", id = helloBlob } ]
        , Git.parseTree (Git.payload (Git.decodeLoose (readLoose headTree))) )
    ; let val c = Git.parseCommit (Git.payload (Git.decodeLoose (readLoose headCommit)))
      in
        ( H.checkString "parseCommit tree" (headTree, #tree c)
        ; H.checkStringList "parseCommit parents" ([firstCommit], #parents c)
        ; H.checkString "parseCommit author" (author, #author c)
        ; H.checkString "parseCommit committer" (committer, #committer c)
        ; H.checkString "parseCommit message"
            ("second commit: edit big.txt and add docs/note.txt\n", #message c) )
      end
    ; let val t = Git.parseTag (Git.payload (Git.decodeLoose (readLoose tagObj)))
      in
        ( H.checkString "parseTag object" (headCommit, #object t)
        ; H.checkString "parseTag typ" ("commit", #typ t)
        ; H.checkString "parseTag tag" ("v1.0", #tag t)
        ; H.checkString "parseTag tagger" (tagger, #tagger t)
        ; H.checkString "parseTag message" ("release v1.0\n", #message t) )
      end

    ; H.section "object: encodeLoose round-trips (inflate o deflate = id)"
    ; H.checkEq "round-trip blob" (helloObj, Git.decodeLoose (Git.encodeLoose helloObj))
    ; H.checkEq "round-trip commit" (headCommitObj, Git.decodeLoose (Git.encodeLoose headCommitObj))
    ; H.checkEq "round-trip tree" (headTreeObj, Git.decodeLoose (Git.encodeLoose headTreeObj))
    ; H.checkEq "round-trip tag" (tagObjVal, Git.decodeLoose (Git.encodeLoose tagObjVal))
    ; H.checkString "encodeLoose preserves oid"
        (Git.hashObject headCommitObj,
         Git.hashObject (Git.decodeLoose (Git.encodeLoose headCommitObj)))

    ; H.section "object: malformed input is rejected"
    ; H.checkRaises "decodeLoose garbage" (fn () => Git.decodeLoose "not a git object")
    ; H.checkRaises "parseTree truncated" (fn () => Git.parseTree "100644 big.txt")

    (* An object header size is decimal ASCII with no fixed width, so a corrupt
       or hostile loose object can carry a size well past 2^31. MLton's default
       `int` is 32-bit, so `Int.fromString` on such a numeral raises `Overflow`
       -- an *uncontrolled* crash that also diverges from Poly/ML (63-bit int,
       which would instead reach the size-mismatch check). The parser must
       reject an out-of-range size as a clean `Git` error (never `Overflow`),
       identically on both compilers. We assert the specific `Git` constructor,
       not merely "some exception", so an `Overflow` crash still fails. *)
    ; H.section "object: oversized size header is rejected cleanly"
    ; let
        fun raisesGit thunk =
          (ignore (thunk ()); false)
          handle Git.Git _ => true
               | _ => false   (* Overflow or anything else -> not a clean reject *)
        val loose10 = Zlib.deflateZlib {level = 6} "blob 9999999999\000hello\n"
        val loose20 = Zlib.deflateZlib {level = 6} "blob 99999999999999999999\000hello\n"
      in
        H.check "oversized 10-digit size -> Git (not Overflow)"
          (raisesGit (fn () => Git.decodeLoose loose10));
        H.check "oversized 20-digit size -> Git (not Overflow)"
          (raisesGit (fn () => Git.decodeLoose loose20));
        (* a well-formed small object with a size that MATCHES its payload still
           decodes cleanly -- the fix must not regress the happy path *)
        H.checkEq "well-formed small object still decodes"
          (Git.Blob "hi", Git.decodeLoose (Zlib.deflateZlib {level = 6} "blob 2\000hi"))
      end
    )
end
