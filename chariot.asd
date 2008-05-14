;;; -*- lisp -*-

(in-package #:cl-user)

(defpackage #:chariot-system
  (:use #:cl #:asdf))

(in-package #:chariot-system)

(defsystem "chariot"
    :description "A symplistic forth implementation for the Nintendo DS"
    :version "0.1"
    :author "Ties Stuij <ties@stuij.se>"
    :license "LLGPL"
    :depends-on (:arnesi :umpa-lumpa :fiveam :armish :liards)
    :components
    ((:module :src
              :components
              ((:file "packages")
               (:file "start" :depends-on ("packages"))))))