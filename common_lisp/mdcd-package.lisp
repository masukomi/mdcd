(defpackage :mdcd
  (:use #:cl #:cl-utilities)
  (:export
      :doc
      :show-doc
      :show-params
      :show-returns
      :show-notes
      :show-examples
      :show-section

      :set-mdcd-home
      :get-mdcd-home
      :path-for
      :enable-writes
      :disable-writes
    ))
