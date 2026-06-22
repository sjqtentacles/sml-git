(* test_ref.sml -- loose ref + packed-refs parsing. *)

structure RefTests =
struct
  open Support
  structure H = Harness

  fun refStr (Git.Ref.Direct oid)   = "direct:" ^ oid
    | refStr (Git.Ref.Symbolic t)   = "sym:" ^ t

  fun run () =
    ( H.section "ref: loose ref files"
    ; H.checkString "HEAD is a symref to refs/heads/main"
        ("sym:refs/heads/main", refStr (Git.Ref.parse (readFile (fixtures ^ "refs/HEAD"))))
    ; H.checkString "refs/main is a direct oid"
        ("direct:" ^ headCommit, refStr (Git.Ref.parse (readFile (fixtures ^ "refs/main"))))
    ; H.checkString "trailing-whitespace tolerated"
        ("direct:" ^ headCommit, refStr (Git.Ref.parse (headCommit ^ "\n")))
    ; H.checkString "inline symref"
        ("sym:refs/heads/topic", refStr (Git.Ref.parse "ref: refs/heads/topic\n"))

    ; H.section "ref: packed-refs"
    ; let val pairs = Git.Ref.parsePacked (readFile (fixtures ^ "packed-refs"))
      in
        ( H.checkInt "packed-refs entry count" (2, List.length pairs)
        ; H.checkStringList "packed-refs names"
            (["refs/heads/main", "refs/tags/v1.0"], List.map #1 pairs)
        ; H.checkStringList "packed-refs oids"
            ([headCommit, tagObj], List.map #2 pairs) )
      end
    (* comment lines and the peeled-tag line ("^<oid>") must be ignored *)
    ; H.checkInt "comment + peeled lines ignored"
        (1, List.length (Git.Ref.parsePacked
                           ("# pack-refs with: peeled fully-peeled sorted\n"
                            ^ "1111111111111111111111111111111111111111 refs/tags/x\n"
                            ^ "^2222222222222222222222222222222222222222\n")))
    )
end
