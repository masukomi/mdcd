;#!/usr/local/bin/csi

; (require-extension test)
(import test); ^^ same effect
;(import files)
(import mdcd-config)
(import mdcd)


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
)

;; IMPORTANT! The following ensures nightly automated tests can
;; distinguish failure from success.  Always end tests/run.scm with this.
; (test-exit)
