;;; pseudo-plist.el --- Provides operations on pseudo plists  -*- lexical-binding: t; -*- <emacs@haukerehfeld.de>
;; Copyright (C) 2025  Hauke Rehfeld

;; Author: Hauke Rehfeld
;; Keywords: lisp
;; Version: 0.0.1
;; URL: https://github.com/hrehfeld/emacs-pseudo-plist
;; Package-Requires: ((emacs "28.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; pseudo-plists are plists that can have multiple instances of a key and multiple values per key
;;

;;; Code:

(require 'cl-lib)

(defun pseudo-plist-get (plist key &optional keywordp keyword-eq nth)
  "Get all values for KEY from pseudo-plist PLIST as a list.

KEYWORDP is used to distinguish keywords from values, defaulting to `keywordp`.
KEYWORD-EQ is used to compare keywords, defaulting to `eq`.

With NTH, only the NTH value for KEY is returned.
If NTH is negative, the NTH value from the end is returned."
  (cl-check-type plist list)
  (cl-block plist-get
    (let ((keywordp (or keywordp #'keywordp))
          (keyword-eq (or keyword-eq #'eq)))
      (cl-assert (funcall keywordp key) nil "KEY must be a keyword")
      (let (key-values
            in-key)
        (cl-loop
         for el in plist
         do (if (funcall keywordp el)
                (setq in-key (funcall keyword-eq el key))
              (when in-key
                (if (and (numberp nth)
                         (length= key-values nth))
                    (cl-return-from plist-get el)
                  (push el key-values)))))
        (when (and (numberp nth) (< nth 0))
          (let* ((nth (1- (abs nth))))
            (cl-assert (>= nth 0) t)
            (cl-assert (< nth (length key-values)) nil "NTH out of bounds")
            (cl-return-from plist-get (nth nth key-values))))
        (nreverse key-values)))))

;; (pseudo-plist-get '(:foo 1 :foo 2 :bar nil :foo 4) :foo)
;; (pseudo-plist-get '(:foo 1 :foo 2 :bar nil :foo 4) :bar)
;; (pseudo-plist-get '(:foo 1 :foo 2 :bar nil :foo 4) :key)
;; (pseudo-plist-get '(:foo 1 :foo 2 :bar nil :foo 4) :foo nil nil 0)
;; (pseudo-plist-get '(:foo 1 :foo 2 :bar nil :foo 4) :foo nil nil 2)
;; (pseudo-plist-get '(:foo 1 :foo 2 :bar nil :foo 4) :foo nil nil -3)

(defun pseudo-plist-remove (plist key &optional keywordp keyword-eq)
  "Remove all elements with KEY from pseudo-plist PLIST."
  (cl-check-type plist list)
  (let ((keywordp (or keywordp #'keywordp))
        (keyword-eq (or keyword-eq #'eq)))
    (cl-assert (funcall keywordp key) nil "KEY must be a keyword")
    (let (in-key
          res)
      (cl-loop
       for el in plist
       do (when (funcall keywordp el)
            (setq in-key (funcall keyword-eq el key)))
       (unless in-key
         (push el res)))
      (nreverse res))))

;; (pseudo-plist-remove '(:foo 1 :foo 2 :bar nil :foo 4) :foo)
;; (pseudo-plist-remove '(:foo 1 :foo 2 :bar nil :foo 4) :bar)
;; (pseudo-plist-remove '(:foo 1 :foo 2 :bar nil :foo 4) :key)


(defun pseudo-plist-put (plist key value &optional keywordp keyword-eq)
  "Add KEY VALUE pair to pseudo-plist PLIST."
  (cl-check-type plist list)
  (let ((keywordp (or keywordp #'keywordp))
        (keyword-eq (or keyword-eq #'eq)))
    (cl-assert (funcall keywordp key) nil "KEY must be a keyword")
    (let (in-key
          res)
      (cl-loop
       for el in plist
       do (if (funcall keywordp el)
              (setq in-key (funcall keyword-eq el key)))
       (unless in-key
         (push el res)))
      (push key res)
      (push value res)
      (nreverse res))))


(provide 'pseudo-plist)
;;; pseudo-plist.el ends here
