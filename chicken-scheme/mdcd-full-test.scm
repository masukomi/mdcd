#!/usr/local/bin/csi


; a central file where i can run all the 
; other tests from, instead of running them 
; individually.

(load "mdcd-config-test.scm")
(load "mdcd-sections-test.scm")
(load "mdcd-test.scm")
(test-exit)
