#|
  This file is a part of mp3-duration project.
  Copyright (c) 2016 Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage mp3-duration-test-asd
  (:use :cl :asdf))
(in-package :mp3-duration-test-asd)

(defsystem mp3-duration-test
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on (:mp3-duration
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "mp3-duration"))))
  :description "Test system for mp3-duration"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
