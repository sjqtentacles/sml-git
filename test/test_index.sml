(* test_index.sml -- dircache (.git/index) v2 parsing against the real index
   git wrote for the fixture repo: 3 staged entries. *)

structure IndexTests =
struct
  open Support
  structure H = Harness

  fun run () =
    let
      val raw = readFile (fixtures ^ "index")
      val entries = Git.Index.parse raw
    in
      ( H.section "index: header"
      ; H.checkInt "version" (2, Git.Index.version raw)
      ; H.checkInt "entry count" (3, List.length entries)

      ; H.section "index: entries (sorted by path, as git stores them)"
      ; H.checkStringList "paths"
          (["big.txt", "docs/note.txt", "hello.txt"], List.map #path entries)
      ; H.checkStringList "oids"
          ([bigV2Blob, noteBlob, helloBlob], List.map #id entries)
      (* every staged file is a regular non-exec blob: mode 0o100644 = 33188 *)
      ; H.checkIntList "modes" ([33188, 33188, 33188], List.map #mode entries)
      ; H.checkIntList "sizes" ([32499, 11, 6], List.map #size entries)
      )
    end
end
