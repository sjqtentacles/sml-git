(* test_pack.sml -- packfile decoding, including ofs-delta and ref-delta
   reconstruction, against two real packs of the SAME repo:
     pack-ofs : produced by `git repack` (OBJ_OFS_DELTA)
     pack-ref : produced by `git pack-objects --no-delta-base-offset`
                (OBJ_REF_DELTA)
   Both contain 10 objects with 2 deltas (the first commit deltifies against
   HEAD, and big.txt v1 deltifies against v2). *)

structure PackTests =
struct
  open Support
  structure H = Harness

  fun blobBytes (Git.Blob s) = s
    | blobBytes _ = "<not a blob>"

  fun loadPack dir =
    Git.Pack.parse
      { pack = readFile (fixtures ^ dir ^ "/pack.pack")
      , idx  = readFile (fixtures ^ dir ^ "/pack.idx") }

  fun checkVariant (label, dir) =
    let
      val p = loadPack dir
      val objs = Git.Pack.objects p
    in
      ( H.section ("pack[" ^ label ^ "]: header + inventory")
      ; H.checkInt (label ^ " object count") (10, Git.Pack.count p)
      ; H.checkInt (label ^ " version") (2, Git.Pack.version p)
      ; H.checkInt (label ^ " objects length") (10, List.length objs)

      ; H.section ("pack[" ^ label ^ "]: every object reconstructs to its oid")
      (* For each (oid, obj) pair the idx claims, re-hashing the reconstructed
         object must reproduce that oid -- this exercises the full decode:
         header varint, zlib stream, and delta copy/insert application. *)
      ; List.app
          (fn (oid, obj) =>
             H.checkString (label ^ " reconstruct " ^ oid)
               (oid, Git.hashObject obj))
          objs

      ; H.section ("pack[" ^ label ^ "]: lookup by oid")
      ; List.app
          (fn oid =>
             case Git.Pack.lookup p oid of
                 SOME obj => H.checkString (label ^ " lookup " ^ oid)
                               (oid, Git.hashObject obj)
               | NONE => H.check (label ^ " lookup " ^ oid ^ " present") false)
          allOids
      ; H.checkString (label ^ " lookup missing oid")
          ("NONE", optStr (Git.Pack.lookup p "0000000000000000000000000000000000000000"))

      ; H.section ("pack[" ^ label ^ "]: delta objects reconstruct exactly")
      (* big.txt v1 is delta-encoded; its reconstructed bytes must equal the
         loose object's bytes, and its oid must match. *)
      ; (case Git.Pack.lookup p bigV1Blob of
             SOME obj =>
               ( H.checkString (label ^ " delta blob oid") (bigV1Blob, Git.hashObject obj)
               ; H.checkString (label ^ " delta blob bytes match loose object")
                   (blobBytes (Git.decodeLoose (readLoose bigV1Blob)), blobBytes obj) )
           | NONE => H.check (label ^ " delta blob present") false)
      (* the first commit is also delta-encoded (against HEAD) *)
      ; (case Git.Pack.lookup p firstCommit of
             SOME (Git.Commit c) =>
               ( H.checkString (label ^ " delta commit tree") (firstTree, #tree c)
               ; H.checkStringList (label ^ " delta commit parents") ([], #parents c)
               ; H.checkString (label ^ " delta commit message")
                   ("initial commit\n", #message c) )
           | _ => H.check (label ^ " delta commit reconstructs") false)
      )
    end

  fun run () =
    ( checkVariant ("ofs", "pack-ofs")
    ; checkVariant ("ref", "pack-ref")
    ; H.section "pack: malformed input is rejected"
    ; H.checkRaises "parse non-pack bytes"
        (fn () => Git.Pack.parse { pack = "not a pack", idx = "not an idx" })
    )
end
