#!/usr/local/bin/csi

; (require-extension test)
(use test); ^^ same effect
(import srfi-13)
(use files)
(use directory-utils)
(import mdcd-config)
(use shell)
(load "mdcd-wiki-export.scm")
(import (prefix mdcd-wiki-export mw:))
(let ( (temp-home (string-split (create-temporary-directory) "/")) )
  (let ((temp-home-path (conc "/" (string-join temp-home "/")))) 
    ;(display (conc "XXX " temp-home-path))
    (create-pathname-directory temp-home-path)
    (set-mdcd-home temp-home)

    ;;; just confirming it's all wired up correctly
    (test-group "sanity"
      "confirm the test docs dir exists"
      (test-assert (directory-exists?  temp-home-path))
      ; (test "mdcd home is temp path"
      ;       (conc temp-home-path "/")
      ;       (get-mdcd-home)
      ;       )
    )

    ;;; end sanity check

    (test-group "read-file"
      (let ((bs-file-path (conc temp-home-path "/bs.txt"))
            (bs-file-contents "bs-file contents"))
        (test "reading file doesn't exist"
              ""
              (mw:read-file bs-file-path))
        (call-with-output-file bs-file-path
          (lambda(output-port)(display bs-file-contents output-port)))

        ; (test-assert "confirm test dir exists now"
        ;     (file-exists? (pathname? bs-file-path)))
        ; (test-assert "confirm test file exists now"
        ;     (file-exists? bs-file-path))
        (test "reading file that exists"
              bs-file-contents
               (mw:read-file bs-file-path))
        ; (run (cat ,bs-file-path))
        ;(delete-file bs-file-path)
      )
    )
    (let ((syntax-dir (conc temp-home-path "/syntax")))
      (test-group "has-x"
        (test "no x dir"
              #f
              (mw:has-x? "syntax"))
        (create-directory syntax-dir #t)
        ; (test-assert "syntax dir exists"
        ;       (directory-exists? syntax-dir))
        (test "has x dir but no contents"
              #f
              (mw:has-x? "syntax"))
        (call-with-output-file (conc syntax-dir "/foo-syntax.md")
          (lambda(output-port)(write "some contents" output-port)))
        (test "has x dir with files"
              #t
              (mw:has-x? "syntax"))

      )
    )
    (delete-directory (pathname? temp-home-path) #t)


    (test-group "the doc"
      (test "adding to the doc"
         #t
         (mw:append-to-doc "foo" "" "bar\nbaz" "" )
        )
      (test "retrieval"
"foo
bar
baz"
        (mw:get-the-doc))
    )
  )
)
;; IMPORTANT! The following ensures nightly automated tests can
;; distinguish failure from success.  Always end tests/run.scm with this.
(test-exit)

