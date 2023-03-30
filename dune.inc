(rule (target deep-lense.%{version:deep-lense}.opam)
 (deps (:opam deep-lense.opam))
 (action
  (with-stdout-to %{target}
   (progn (cat %{opam})
    (echo
     "url { src: \"git://git@gitlab.routine.co:routine/deep-lense#%{version:deep-lense}\" }")))))
(rule (alias default) (target deep-lense-ios.opam)
 (action
  (with-stdout-to %{target}
   (run %{dep:.logistic/dune/extdeps/extdeps.exe} rewrite-ios --input
    %{dep:deep-lense.opam} --cross deep-lense --cross-both "" --cross-exclude
    ""))))
(rule (target deep-lense-ios.%{version:deep-lense}.opam)
 (deps (:opam deep-lense-ios.opam))
 (action
  (with-stdout-to %{target}
   (progn (cat %{opam})
    (echo
     "url { src: \"git://git@gitlab.routine.co:routine/deep-lense#%{version:deep-lense}\" }")))))
(rule (deps (universe)) (target deep-lense.opam.locked)
 (action (run %{bin:opam} lock deep-lense)))
(rule (alias extdeps) (mode (promote (until-clean)))
 (target deep-lense.opam.extdeps)
 (action
  (with-stdout-to %{target}
   (run %{dep:.logistic/dune/extdeps/extdeps.exe} rewrite --input
    %{dep:deep-lense.opam.locked} --local deep-lense))))
