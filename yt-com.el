;;; yt-com.el --- Reliable YouTube comments front-end  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Valeriy Litkovskyy

;; Author: Valeriy Litkovskyy <vlr.ltkvsk@protonmail.com>
;; Version: 0.1.0
;; Keywords: comm, data, games, hypermedia, mouse, multimedia
;; URL: https://github.com/xFA25E/youtube-comments
;; Package-Requires: ((emacs "25.1"))

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

;; M-x yt-com

;; It take a YouTube (or Invidious) video URL.

;; M-x yt-com-id

;; It takes a YouTube video ID.

;;; Code:

;;;; CUSTOMIZE

(defgroup yt-com nil
  "YouTube comments front-end."
  :group 'applications)

(defcustom yt-com-invidious-hosts
  '("invidio.us")
  "A list of Invidious hosts that will be used to query data."
  :type '(repeat :tag "Hosts" string)
  :group 'yt-com)

(defcustom yt-com-date-time-format "%Y-%m-%d %H:%M:%S"
  "Format time string.
See `format-time-string' function."
  :type '(string :tag "Date-Time format")
  :group 'yt-com)

;;;;; FACES

(defface yt-com-date-time-face
  '((t :inherit font-lock-builtin-face))
  "YouTube comment date-time face."
  :group 'yt-com)

(defface yt-com-likes-face
  '((t :inherit font-lock-doc-face))
  "YouTube comment likes face."
  :group 'yt-com)

(defface yt-com-owner-face
  '((t :inherit hl-line))
  "YouTube comment owner face."
  :group 'yt-com)

;;;; DEPENDENCIES

(require 'url)
(require 'url-queue)
(require 'url-parse)
(require 'url-http)
(require 'seq)
(require 'subr-x)
(require 'shr)
(require 'ewoc)

;;;; VARIABLES

(defvar-local yt-com--video-id nil)

(defvar-local yt-com--ewoc nil)

;;;; EWOC WIDGETS

(defclass yt-com--button ()
  ((continuation :initarg :continuation)))

(defclass yt-com--replies-button (yt-com--button)
  ((reply-count :initarg :reply-count :initform nil)))

(defclass yt-com--comment ()
  ((author-id :initarg :author-id)
   (author :initarg :author)
   (content :initarg :content)
   (published :initarg :published)
   (like-count :initarg :like-count)
   (ownerp :initarg :ownerp)
   (heart :initarg :heart)
   (thumbnail-urls :initarg :thumbnail-urls :initform nil)
   (thumbnail :initarg :thumbnail :initform nil)
   (padding :initform "")))

(defclass yt-com--reply (yt-com--comment)
  ((padding :initform "|   ")))

;;;; UTILS

(defun yt-com--parse-thumbnail-urls (json-comment)
  (let ((get-width (apply-partially #'gethash "width"))
        (get-url (apply-partially #'gethash "url"))
        (json-thumbnails (gethash "authorThumbnails" json-comment)))
    (seq-map get-url (seq-sort-by get-width #'> json-thumbnails))))

(defun yt-com--parse-comment (json-comment &optional replyp)
  (let* ((json-is-owner (gethash "authorIsChannelOwner" json-comment))
         (ownerp (not (eq :false json-is-owner)))
         (json-heart (gethash "creatorHeart" json-comment))
         (heart (when json-heart (gethash "creatorName" json-heart))))
    (funcall
     (if replyp #'yt-com--reply #'yt-com--comment)
     :author-id      (gethash "authorId" json-comment)
     :author         (gethash "author" json-comment)
     :content        (gethash "content" json-comment)
     :published      (gethash "published" json-comment)
     :like-count     (gethash "likeCount" json-comment)
     :ownerp         ownerp
     :heart          heart
     :thumbnail-urls (yt-com--parse-thumbnail-urls json-comment))))

(defun yt-com--parse-replies-button (json-comment)
  (when-let ((json-replies (gethash "replies" json-comment)))
    (yt-com--replies-button
     :continuation (gethash "continuation" json-replies)
     :reply-count  (gethash "replyCount" json-replies))))

(defun yt-com--parse-widgets (json-comments &optional repliesp)
  (let ((parse (lambda (json-comment)
                 (list (yt-com--parse-comment json-comment repliesp)
                       (yt-com--parse-replies-button json-comment)))))
    (seq-filter #'identity (seq-mapcat parse json-comments))))

(defun yt-com--button-action (_)
  (message "Loading...")
  (let* ((ewoc yt-com--ewoc)
         (node (ewoc-locate ewoc))
         (button (ewoc-data node))

         (continuation (oref button continuation))
         (repliesp (yt-com--replies-button-p button)))

    (yt-com--retrieve-widgets
     yt-com--video-id continuation repliesp
     (lambda (widgets continuation comment-count)
       (message "Comment count %s" comment-count)
       (with-current-buffer (ewoc-buffer ewoc)
         (let ((inhibit-read-only t))
           (oset button continuation continuation)

           (when (and (yt-com--replies-button-p button)
                      (oref button reply-count))
             (let ((new-reply-count (- (oref button reply-count)
                                       (seq-count #'yt-com--reply-p widgets))))
               (when (< 0 new-reply-count)
                 (oset button reply-count new-reply-count))))

           (ewoc-invalidate ewoc node)
           (yt-com--draw-widgets ewoc node widgets)))))))

(defun yt-com--build-urls (method id query)
  (let* ((query (when query (concat "?" (url-build-query-string query))))
         (filename (concat "/api/v1/" method "/" id query)))
    (mapcar
     (lambda (host)
       (url-parse-make-urlobj "https" nil nil host nil filename nil nil t))
     yt-com-invidious-hosts)))

;;;; NETWORK

(defvar url-http-end-of-headers)
(defun yt-com--retrieve-url (url cb &optional cberr)
  (url-retrieve
   url
   (lambda (_status)
     (let ((current-buffer (current-buffer)))
       (unwind-protect
           (cond ((and (bound-and-true-p url-http-response-status)
                       (= 200 url-http-response-status))
                  (goto-char url-http-end-of-headers)
                  (forward-char)
                  (funcall cb))
                 (cberr
                  (funcall cberr)))
         (kill-buffer current-buffer))))
   nil t))

(cl-defun yt-com--retrieve-urls ((url . urls) cb &optional cberr)
  (yt-com--retrieve-url
   url cb
   (lambda ()
     (cond (urls (yt-com--retrieve-urls urls cb cberr))
           (cberr (funcall cberr))))))

(defun yt-com--retrieve-json (method id query callback)
  (yt-com--retrieve-urls
   (yt-com--build-urls method id query)
   (lambda () (funcall callback (json-parse-buffer)))
   (lambda () (error "All urls returned error"))))

(defun yt-com--retrieve-title (id cb)
  (yt-com--retrieve-json
   "videos" id '(("fields" "title"))
   (lambda (json) (funcall cb (gethash "title" json)))))

(defun yt-com--retrieve-widgets (id continuation repliesp cb)
  (yt-com--retrieve-json
   "comments" id (when continuation `(("continuation" ,continuation)))
   (lambda (json)
     (funcall
      cb
      (yt-com--parse-widgets (gethash "comments" json) repliesp)
      (gethash "continuation" json)
      (gethash "commentCount" json)))))

(defun yt-com--retrieve-images (urls cb cbafter)
  (if (not urls)
      (funcall cbafter)
    (pcase-let ((`(,url . ,urls) urls))
      (yt-com--retrieve-url
       url
       (lambda ()
         (let* ((data (buffer-substring-no-properties (point) (point-max))))
           (funcall cb (create-image data nil t)))
         (yt-com--retrieve-images urls cb cbafter))
       (lambda () (yt-com--retrieve-images urls cb cbafter))))))

;;;; DRAW

(defun yt-com--draw-url (label url)
  (insert-text-button
   label 'follow-link t 'mouse-face 'highlight 'shr-url url 'keymap shr-map))

(defun yt-com--draw-button (label)
  (insert-text-button label 'follow-link t 'action 'yt-com--button-action))

(defun yt-com--draw-header (title id &optional comment-count)
  (let ((url (format "https://www.youtube.com/watch?v=%s" id)))
    (yt-com--draw-url title url))
  (insert "\n")
  (when comment-count
    (insert (number-to-string comment-count)
            " comments\n")))

(defmethod yt-com--draw-widget :before (_)
  (insert "\n"))

(defmethod yt-com--draw-widget :after (_)
  (insert "\n"))

(defmethod yt-com--draw-widget ((button yt-com--button))
  (if (oref button continuation)
      (yt-com--draw-button "Load more")
    (insert "No more comments")))

(defmethod yt-com--draw-widget ((button yt-com--replies-button))
  (if (oref button continuation)
      (let ((count (or (oref button reply-count) "more")))
        (yt-com--draw-button (format "Show %s replies" count)))
    (insert "No more replies")))

(defun yt-com--draw-author-url (comment)
  (let ((url (format "https://www.youtube.com/channel/%s"
                     (oref comment author-id))))
    (yt-com--draw-url (oref comment author) url)))

(defun yt-com--draw-date-time (comment)
  (let* ((time (seconds-to-time (oref comment published)))
         (s (format-time-string yt-com-date-time-format time)))
    (insert (propertize s 'face 'yt-com-date-time-face))))

(defun yt-com--draw-likes (comment)
  (let ((s (format "%s likes" (oref comment like-count))))
    (insert (propertize s 'face 'yt-com-likes-face))))

(defun yt-com--draw-owner (comment)
  (when (oref comment ownerp)
    (insert "  -  " (propertize "OWNER" 'face 'yt-com-owner-face))))

(defun yt-com--draw-heart (comment)
  (when-let ((creator (oref comment heart)))
    (let ((s (format "\n%s likes this comment" creator)))
      (insert (propertize s 'face 'yt-com-owner-face)))))

(defmethod yt-com--draw-widget ((comment yt-com--comment))
  (let ((padding (oref comment padding)))
    (insert padding)
    (if-let ((thumbnail (oref comment thumbnail)))
        (insert-image thumbnail)
      (insert "*"))
    (insert " ")
    (yt-com--draw-author-url comment)
    (insert "  -  ")
    (yt-com--draw-date-time comment)
    (insert "  -  ")
    (yt-com--draw-likes comment)
    (yt-com--draw-owner comment)
    (let ((content (replace-regexp-in-string
                    "^" padding (oref comment content))))

      (insert "\n" content))
    (yt-com--draw-heart comment)))

(defun yt-com--draw-thumbnail (ewoc node &optional nodes)
  (let* ((comment (ewoc-data node))
         (urls (oref comment thumbnail-urls)))
    (yt-com--retrieve-images
     urls
     (lambda (image)
       (oset comment thumbnail image)
       (with-current-buffer (ewoc-buffer ewoc)
         (let ((inhibit-read-only t))
           (ewoc-invalidate ewoc node))))
     (lambda ()
       (when nodes
         (yt-com--draw-thumbnail ewoc (car nodes) (cdr nodes)))))))

(defun yt-com--draw-widgets (ewoc node widgets)
  (let ((nodes nil))
    (dolist (widget widgets)
      (let ((new-node (ewoc-enter-before ewoc node widget)))
        (when (cl-typep new-node 'yt-com--comment)
          (push new-node nodes))))
    (when nodes
      (let ((nodes (nreverse nodes)))
        (yt-com--draw-thumbnail ewoc (car nodes) (cdr nodes))))))

;;;; COMMANDS

(defun yt-com-next-button ()
  (interactive)
  (when-let ((marker (next-button (point))))
    (goto-char marker)
    (set-marker marker nil)))

(defun yt-com-previous-button ()
  (interactive)
  (when-let ((marker (previous-button (point))))
    (goto-char marker)
    (set-marker marker nil)))

(define-derived-mode yt-com-mode special-mode "Yt-Com"
  :group 'yt-com)

;;;###autoload
(defun yt-com (url)
  (interactive "sYoutube (or invidious) url: ")
  (pcase (url-generic-parse-url (string-trim url))
    ((cl-struct url (host "youtu.be") filename)
     (substring filename 1 12))

    ((app url-path-and-query `("/watch" . ,(app url-parse-query-string query)))
     (if-let ((id (cadr (assoc "v" query))))
         (yt-com-id id)
       (user-error "Invalid url")))

    (_ (user-error "Invalid url"))))

;;;###autoload
(defun yt-com-id (id)
  (interactive "sYoutube video id: ")
  (yt-com--retrieve-title
   id
   (lambda (title)
     (yt-com--retrieve-widgets
      id nil nil
      (lambda (widgets continuation comment-count)
        (let ((buffer (get-buffer-create "*Yt-Com*")))
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (kill-all-local-variables)
              (yt-com-mode)
              (erase-buffer)
              (buffer-disable-undo)
              (setq-local yt-com--video-id id)
              (let* ((header (with-temp-buffer
                               (print (yt-com--draw-header title id comment-count))
                               (buffer-substring (point-min) (point-max))))
                     (ewoc (ewoc-create 'yt-com--draw-widget header nil t))
                     (node (ewoc-enter-last
                            ewoc (yt-com--button :continuation continuation))))
                (yt-com--draw-widgets ewoc node widgets)
                (setq-local yt-com--ewoc ewoc))))
          (switch-to-buffer buffer)))))))

;;;; BINDS

(define-key yt-com-mode-map (kbd "n") 'yt-com-next-button)
(define-key yt-com-mode-map (kbd "p") 'yt-com-previous-button)

(provide 'yt-com)
;;; yt-com.el ends here
