;;; youtube-comments.el --- Youtube comments front-end  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Valeriy Litkovskyy

;; Author: Valeriy Litkovskyy <>
;; Keywords: multimedia

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

;;

;;; Code:

;;;; REQUIRES

(require 'url)
(require 'url-queue)
(require 'url-parse)
(require 'url-http)
(require 'seq)
(require 'subr-x)
(require 'shr)

;;;; TYPES

(cl-defstruct (ytcom-header (:constructor nil)
                            (:constructor ytcom-header-create
                                          (title comment-count))
                            (:copier nil))
  title comment-count)

(cl-defstruct (ytcom-button (:constructor nil)
                            (:constructor ytcom-button-create
                                          (continuation &optional repliesp reply-count))
                            (:copier nil))
  continuation repliesp reply-count)

(cl-defstruct (ytcom-comment (:constructor ytcom-comment-create) (:copier nil))
  author-id
  author
  content
  published
  like-count
  ownerp
  heart
  thumbnail-urls
  (thumbnail nil)
  (replyp nil))

;;;; VARIABLES

(defvar youtube-comments-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'youtube-comments-next-button)
    (define-key map (kbd "p") 'youtube-comments-previous-button)
    map))

(defvar-local ytcom-id nil)
(defvar-local ytcom-ewoc nil)

;;;; CUSTOM

(defgroup youtube-comments nil
  "Youtube comments front-end."
  :group 'applications)

(defcustom youtube-comments-invidious-hosts
  '(;; "invidious.namazso.eu"
    "tube.connect.cafe" "ytprivate.com" "invidious.tube"
    "vid.puffyan.us" "invidious.himiko.cloud" "invidious.048596.xyz"
    "invidious.zee.li" "inv.skyn3t.in" "invidiou.site" "au.ytprivate.com")
  "Invidious hosts."
  :group 'youtube-comments)

(defface youtube-comments-date-time-face
  '((t :inherit font-lock-builtin-face))
  ""
  :group 'youtube-comments)

(defface youtube-comments-likes-face
  '((t :inherit font-lock-doc-face))
  ""
  :group 'youtube-comments)

(defface youtube-comments-owner-face
  '((t :inherit hl-line))
  ""
  :group 'youtube-comments)

;;;; FUNCTIONS

;;;;; UTILS

(defun ytcom-parse-id-from-url (url)
  (let ((url-object (url-generic-parse-url (string-trim url))))
    (if (string-equal "youtu.be" (url-host url-object))
        (substring (url-filename url-object) 1 12)
      (pcase-let* ((`(,path . ,query) (url-path-and-query url-object))
                   (query (url-parse-query-string query)))
        (unless (string-equal "/watch" path)
          (error "Invalid youtube url"))
        (if-let ((id (assoc "v" query #'string-equal)))
            (cadr id)
          (error "Invalid youtube url"))))))

(defun ytcom-format-content (content padding)
  (with-temp-buffer
    (insert content)
    (fill-region (point-min) (point-max) nil t)
    (replace-regexp "^" padding nil (point-min) (point-max))
    (buffer-substring (point-min) (point-max))))

(defun ytcom-comments-and-buttons-create (json)
  (seq-mapcat
   (lambda (comment)
     (cons
      (ytcom-comment-create
       :author-id (gethash "authorId" comment)
       :author (gethash "author" comment)
       :content (gethash "content" comment)
       :published (gethash "published" comment)
       :like-count (gethash "likeCount" comment)
       :ownerp (not (eq :false (gethash "authorIsChannelOwner" comment)))
       :heart (when-let ((heart (gethash "creatorHeart" comment)))
                (gethash "creatorName" heart))
       :thumbnail-urls (thread-last (gethash "authorThumbnails" comment)
                         (seq-sort-by (apply-partially #'gethash "width") #'>)
                         (seq-map (apply-partially #'gethash "url"))))
      (when-let ((replies (gethash "replies" comment)))
        (list (ytcom-button-create (gethash "continuation" replies)
                                   t (gethash "replyCount" replies))))))
   json))

(defun ytcom-button-action (_)
  (message "Loading...")
  (let* ((ewoc ytcom-ewoc)
         (node (ewoc-locate ewoc))
         (button (ewoc-data node))
         (continuation (ytcom-button-continuation button))
         (repliesp (ytcom-button-repliesp button)))
    (ytcom-retrieve-entities
     ytcom-id continuation
     (lambda (entities)
       (when repliesp
         (dolist (entity entities)
           (cond ((ytcom-comment-p entity)
                  (setf (ytcom-comment-replyp entity) t))
                 ((ytcom-button-p entity)
                  (setf (ytcom-button-repliesp entity) t)))))
       (let ((previous-node (ewoc-prev ewoc node)))
         (with-current-buffer (ewoc-buffer ewoc)
           (let ((inhibit-read-only t))
             (ewoc-delete ewoc node)
             (ytcom-draw-entities ewoc previous-node entities))))))))

;;;;; NETWORK

(cl-defun ytcom-retrieve-url ((url . urls) callback &optional queuep)
  (let ((url-queue-parallel-processes 1))
    (funcall (if queuep #'url-queue-retrieve #'url-retrieve)
             url
             (lambda (status)
               (if (bound-and-true-p url-http-response-status)
                   (let ((current-buffer (current-buffer)))
                     (unwind-protect
                         (if (and (/= 200 url-http-response-status) urls)
                             (ytcom-retrieve-url urls callback queuep)
                           (goto-char url-http-end-of-headers)
                           (forward-char)
                           (funcall callback))
                       (kill-buffer current-buffer)))
                 (setq-local temp-status status)
                 (switch-to-buffer (current-buffer))
                 (debug))))))

(defun ytcom-retrieve-json (method id query callback)
  (ytcom-retrieve-url
   (mapcar
    (lambda (host)
      (let ((f (format "/api/v1/%s/%s?%s" method id (url-build-query-string query))))
        (url-parse-make-urlobj "https" nil nil host nil f nil nil t)))
    youtube-comments-invidious-hosts)
   (lambda () (funcall callback (json-parse-buffer)))))

(defun ytcom-retrieve-title (id callback)
  (ytcom-retrieve-json "videos" id '(("fields" "title"))
                       (lambda (json)
                         (funcall callback (gethash "title" json)))))

(defun ytcom-retrieve-entities (id continuation callback &optional title)
  (ytcom-retrieve-json
   "comments" id
   `(("fields" "commentCount,continuation,comments(author,authorThumbnails,authorId,content,published,likeCount,authorIsChannelOwner,creatorHeart,replies)")
     . ,(when continuation
          `(("continuation" ,continuation))))
   (lambda (json)
     (funcall
      callback
      (append
       (when title (list (ytcom-header-create title (gethash "commentCount" json))))
       (ytcom-comments-and-buttons-create (gethash "comments" json))
       (when-let ((c (gethash "continuation" json))) (list (ytcom-button-create c))))))))

(defun ytcom-retrieve-image (urls callback)
  (ytcom-retrieve-url
   urls
   (lambda ()
     (let* ((data (buffer-substring-no-properties (point) (point-max))))
       (funcall callback (create-image data nil t))))
   t))

;;;;; DRAW

(defun ytcom-draw-url (label url)
  (insert-text-button label
                      'follow-link t
                      'mouse-face 'highlight
                      'shr-url url
                      'keymap shr-map))

(defun ytcom-draw-button (label)
  (insert-text-button label 'follow-link t 'action 'ytcom-button-action))

(defmethod ytcom-draw-entity ((header ytcom-header))
  (insert "\n")
  (ytcom-draw-url (ytcom-header-title header)
                  (concat "https://www.youtube.com/watch?v=" ytcom-id))
  (insert (format "\n%d comments\n" (ytcom-header-comment-count header))))

(defmethod ytcom-draw-entity ((button ytcom-button))
  (insert "\n")
  (ytcom-draw-button
   (if (ytcom-button-repliesp button)
      (if-let ((reply-count (ytcom-button-reply-count button)))
          (format "View %d replies" reply-count)
        "Show more replies")
     "Load more"))
  (insert "\n"))

(defun ytcom-draw-author-url (comment)
  (ytcom-draw-url (ytcom-comment-author comment)
                  (concat "https://www.youtube.com/channel/"
                          (ytcom-comment-author-id comment))))

(defun ytcom-draw-date-time (comment)
  (let* ((published (ytcom-comment-published comment))
         (time (seconds-to-time published))
         (s (format-time-string "%Y-%m-%d %H:%M:%S" time)))
    (insert (propertize s 'face 'youtube-comments-date-time-face))))

(defun ytcom-draw-likes (comment)
  (let* ((likes (number-to-string (ytcom-comment-like-count comment)))
         (s (concat likes " likes")))
    (insert (propertize s 'face 'youtube-comments-likes-face))))

(defun ytcom-draw-owner (comment)
  (when (ytcom-comment-ownerp comment)
    (insert "  -  " (propertize "OWNER" 'face 'youtube-comments-owner-face))))

(defun ytcom-draw-heart (comment)
  (when-let (creator (ytcom-comment-heart comment))
    (insert "\n")
    (let ((s (concat creator " likes this comment")))
      (insert (propertize s 'face 'youtube-comments-owner-face) "\n"))))

(defmethod ytcom-draw-entity ((comment ytcom-comment))
  (let ((padding (if (ytcom-comment-replyp comment) "|   " "")))
    (insert padding "\n" padding)
    (if-let ((thumbnail (ytcom-comment-thumbnail comment)))
        (insert-image thumbnail)
      (insert "*"))
    (insert " ")
    (ytcom-draw-author-url comment)
    (insert "  -  ")
    (ytcom-draw-date-time comment)
    (insert "  -  ")
    (ytcom-draw-likes comment)
    (ytcom-draw-owner comment)
    (insert "\n" (ytcom-format-content (ytcom-comment-content comment) padding) "\n")
    (ytcom-draw-heart comment)))

(defun ytcom-draw-thumbnail (ewoc node)
  (let* ((comment (ewoc-data node))
         (urls (ytcom-comment-thumbnail-urls comment)))
    (ytcom-retrieve-image
     urls
     (lambda (image)
       (setf (ytcom-comment-thumbnail comment) image)
       (with-current-buffer (ewoc-buffer ewoc)
         (let ((inhibit-read-only t))
           (ewoc-invalidate ewoc node)))))))

(defun ytcom-draw-entities (ewoc node entities)
  (let ((nodes nil))
    (dolist (entity entities)
      (setq node (ewoc-enter-after ewoc node entity))
      (when (ytcom-comment-p entity)
        (push node nodes)))
    (dolist (node (nreverse nodes))
      (ytcom-draw-thumbnail ewoc node))))

;;;; COMMANDS

(defun youtube-comments-next-button ()
  (interactive)
  (when-let ((marker (next-button (point))))
    (goto-char marker)
    (set-marker marker nil)))

(defun youtube-comments-previous-button ()
  (interactive)
  (when-let ((marker (previous-button (point))))
    (goto-char marker)
    (set-marker marker nil)))

(define-derived-mode youtube-comments-mode special-mode "Youtube-Comments"
  :group 'youtube-comments)

;;;###autoload
(defun youtube-comments (url)
  (interactive "SYoutube (or invidious) url: ")
  (youtube-comments-id
   (ytcom-parse-id-from-url (read-string "Youtube (or invidious) url: "))))

;;;###autoload
(defun youtube-comments-id (id)
  (interactive "SYoutube video id: ")
  (ytcom-retrieve-title
   id
   (lambda (title)
     (ytcom-retrieve-entities
      id nil
      (lambda (entities)
        (let ((buffer (get-buffer-create "*Youtube-Comments*")))
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (kill-all-local-variables)
              (youtube-comments-mode)
              (erase-buffer)
              (buffer-disable-undo)
              (let* ((ewoc (ewoc-create 'ytcom-draw-entity nil nil t))
                     (node (ewoc-enter-last ewoc (car entities))))
                (setq-local ytcom-id id ytcom-ewoc ewoc)
                (ytcom-draw-entities ewoc node (cdr entities)))))
          (switch-to-buffer buffer)))
      title))))

(provide 'youtube-comments)
;;; youtube-comments.el ends here
