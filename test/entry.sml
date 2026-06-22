(* entry.sml -- runs every suite and reports an exit status.

   Poly/ML (tools/polybuild) exports the `main` defined here; MLton calls it
   from main.sml. *)

fun runAllSuites () =
  ( Harness.reset ()
  ; ObjectTests.run ()
  ; PackTests.run ()
  ; IndexTests.run ()
  ; RefTests.run ()
  ; Harness.run () )

fun main () =
  OS.Process.exit
    (if runAllSuites () then OS.Process.success else OS.Process.failure)
