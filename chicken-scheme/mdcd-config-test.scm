#!/usr/local/bin/csi

; (require-extension test)
(use test); ^^ same effect
(import mdcd-config)
(import mdcd)
(use files)
(test-group "mdcd-config"
  (test "set-mdcd-home"
        "/foo/bar/baz/"
         (set-mdcd-home '("foo" "bar" "baz")) 
         ; setting sets it and then calls 
         ; get-mdcd-home for its response value
  )
  (set-mdcd-home '("foo" "bar" "baz"))
  (test-assert "enabled?"
      (mdcd-enabled?))
  (mdcd-disable)
  (test-assert "disable"
      (not (mdcd-enabled?)))


  ; reset it to what it was (FIXME)

)

;; IMPORTANT! The following ensures nightly automated tests can
;; distinguish failure from success.  Always end tests/run.scm with this.
(test-exit)
