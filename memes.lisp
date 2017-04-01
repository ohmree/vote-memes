;;;; memes.lisp

(in-package #:memes)
(use-package '(hunchentoot parenscript cl-who))

;;; "memes" goes here. Hacks and glory await!

(setf (html-mode) :html5) ; output in HTML5

(defvar *memes* '())

(defclass meme ()
  ((name  :reader   name
          :initarg  :name)
   (votes :accessor votes
          :initform 0)))

(defmethod vote-for (meme)
  (incf (votes meme)))

(defun meme-from-name (name)
  (find name *memes* :test #'string-equal
                     :key #'name))

(defun meme-stored-p (meme-name)
  (if (meme-from-name meme-name)
      t
      nil))

(defun sort-memes-copy ()
  (sort (copy-list *memes*) #'> :key #'votes))

(defun add-meme (name)
  (unless (meme-stored-p name)
    (push (make-instance 'meme :name name) *memes*)))

(defmethod print-object ((object meme) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (name votes) object
      (format stream "name: ~s with ~d votes" name votes))))

(defmacro standard-page ((&key title) &body body)
  `(with-html-output-to-string
     (*standard-output* nil :prologue t :indent t)
     (:html :lang "en"
            (:head
             (:meta :charset "utf-8")
             (:title ,title)
             (:link :type "text/css"
                    :rel  "stylesheet"
                    :href "/memes.css"))
            (:body
             (:div :id "header" ; Memes header
               (:img :src "/logo.png"
                     :alt "Some dank ass Meme"
                     :class "logo")
               (:span :class "strapline"
                      "Vote on on your favorite Memes"))
             ,@body))))

(defun start-server (port)
  (start (make-instance 'easy-acceptor :port port)))

(define-easy-handler (memes :uri "/memes") ()
  (standard-page
    (:title "Top Memes")
    (:h1 "Vote on your favorite memes!")
    (:p "Missing a meme? Make it available for votes "
        (:a :href "new-meme" "here"))
    (:h2 "Current stand")
    (:div :id "chart" ; for CSS styling of the links.
      (:ol
       (dolist (meme (sort-memes-copy))
         (htm
          (:li (:a :href (format nil "vote?name=~a"
                           (url-encode ; avoid injection attacks
                            (name meme))) "Vote!")
               (fmt "~A with ~D votes" (escape-string (name meme))
                                       (votes meme)))))))))

(define-easy-handler (vote :uri "/vote") (name)
  (when (meme-stored-p name)
    (vote-for (meme-from-name name)))
  (redirect "/memes"))

(define-easy-handler (new-meme :uri "/new-meme") ()
  (standard-page (:title "Add a new meme")
    (:h1 "Add a new meme to the chart")
    (:form :action "/meme-added" :method "post" :id "addform"
           (:p "What is the name of the meme?" (:br)
             (:input :type "text" :name "name" :class "txt"))
           (:p (:input :type "submit" :value "Add" :class "btn")))))

(define-easy-handler (meme-added :uri "/meme-added") (name)
  (unless (or (null name) (zerop (length name)))
    (add-meme name))
  (redirect "/memes"))
