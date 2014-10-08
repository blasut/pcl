(in-package :cl-user)

(defpackage :pcl.id3v2
  (:use :common-lisp
        :pcl.binary-data
        :pcl.pathnames)
  (:export
   :read-id3
   :mp3-p
   :id3-p
   :album
   :composer
   :genre
   :encoding-program
   :artist
   :part-of-set
   :track
   :song
   :year
   :size
   :translated-genre))
