;;; yt-com.el --- Youtube comments                   -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Valeriy Litkovskyy

;; Author: Valeriy Litkovskyy <vlr.ltkvsk@protonmail.com>
;; Keywords: comm, data, games, hypermedia, mouse, multimedia
;; Version: 1.0.0
;; URL: https://github.com/xFA25E/yt-com
;; Package-Requires: ((emacs "27.1"))

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

;;; Code:

;;;; REQUIRES

(require 'cl-lib)
(require 'ewoc)
(require 'map)
(require 'seq)
(require 'shr)
(require 'url)

;;;; SILENCE BYTE-COMPILER

(defvar url-http-end-of-headers)

;;;; VARIABLES

(defvar-local yt-com--ewoc nil
  "Ewoc data of Yt-Com buffer.")

(defvar-local yt-com--id nil
  "Video id.")

;;;; CUSTOMIZE

(defgroup yt-com nil
  "YouTube comments front-end."
  :group 'applications)

(defcustom yt-com-buffer-name "*Yt-Com*"
  "Yt-Com buffer name."
  :type 'string
  :group 'yt-com)

(defface yt-com-time-face
  '((t :inherit font-lock-builtin-face))
  "YouTube comment time face."
  :group 'yt-com)

(defface yt-com-like-face
  '((t :inherit font-lock-doc-face))
  "YouTube comment like face."
  :group 'yt-com)

(defface yt-com-owner-face
  '((t :inherit font-lock-type-face))
  "YouTube comment owner face."
  :group 'yt-com)

;;;; UTILS

(defun yt-com-extract-id-from-url (url-or-id)
  "Extract video id from URL-OR-ID.
URL-OR-ID can be a string of 11 characters or a url.  If it's a
url, it can be a youtube.be link (protocol://youtu.be/<id>) or
any link with the right path to video id
\(protocol://some.host/watch?v=<id>)."
  (if (= 11 (length url-or-id))
      url-or-id
    (pcase (url-generic-parse-url url-or-id)
      ((cl-struct url (host "youtu.be") filename)
       (substring filename 1 12))
      ((app url-path-and-query `("/watch" . ,(app url-parse-query-string query)))
       (or (cadr (assoc "v" query)) (user-error "Invalid url")))
      (_ (user-error "Invalid url")))))

(defun yt-com-build-url (host method id params)
  "Build invidious api url based on HOST, METHOD, ID and PARAMS."
  (url-parse-make-urlobj
   "https" nil nil host nil
   (concat "/api/v1/" method "/" id "?" (url-build-query-string params))
   nil nil t))

(defun yt-com-invidious-hosts ()
  "Retrieve invidious hosts ordered by health."
  (let ((parse-entry
         (lambda (entry)
           (pcase-let (((seq host (map ("type" type) ("monitor" monitor))) entry))
             (when (string= "https" type)
               (when-let ((health (map-nested-elt monitor ["dailyRatios" 0 "ratio"])))
                 (let ((health (string-to-number health)))
                   (when (< 90 health)
                     (cons host health))))))))
        (buffer (url-retrieve-synchronously "https://api.invidious.io/instances.json" t t))
        (instance-entries nil))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char url-http-end-of-headers)
          (setq instance-entries (json-parse-buffer :null-object nil)))
      (kill-buffer buffer))
    (thread-last instance-entries
      (seq-map parse-entry)
      (delq nil)
      (seq-sort-by #'cdr #'>)
      (seq-map #'car))))

(defun yt-com--comment-cache (comment)
  "Get COMMENT cache."
  (let ((cache (map-elt comment "cache")))
    (unless cache
      (setq cache (make-hash-table :test 'equal :size 3))
      (map-put! comment "cache" cache))
    cache))

(defun yt-com--invalidate (ewoc nodes)
  "Invalidate EWOC NODES saving point in buffer."
  (with-current-buffer (ewoc-buffer ewoc)
    (let ((position (point)))
      (seq-doseq (node nodes)
        (ewoc-invalidate ewoc node))
      (goto-char position))))

;;;; NETWORK - Routines that interact with invidious instances

;;;;; URL - Base for every other network routines

(defun yt-com--get (url callback callback-error &rest cbargs)
  "Retrieve URL like `url-retrieve'.
Call CALLBACK with CBARGS on success.  On any kind of error, or
timeout, call CALLBACK-ERROR with CBARGS."
  (let* ((cbargs (cl-list* callback callback-error cbargs))
         (buffer (url-retrieve url #'yt-com--post-get cbargs t t)))
    (run-with-timer 11 nil #'yt-com--post-get-timeout buffer)))

(defun yt-com--post-get (status callback callback-error &rest cbargs)
  "Callback of `yt-com--get'.
On success, call CALLBACK with CBARGS, otherwise call
CALLBACK-ERROR with CBARGS.  STATUS is used to detect errors."
  (let ((buffer (current-buffer)))
    (unwind-protect
        (if (plist-member status :error)
            (apply callback-error cbargs)
          (apply callback cbargs))
      (kill-buffer buffer))))

(defun yt-com--post-get-timeout (buffer)
  "Callback of `yt-com--get' after timeout.
Check if BUFFER is live and it's process is live and delete it."
  (when (buffer-live-p buffer)
    (delete-process (buffer-local-value 'url-http-process buffer))))

;;;;; DATA - Network routine that parses json

(defun yt-com--get-data (method id params callback &rest cbargs)
  "Call invidious METHOD with ID and PARAMS.
On success call CALLBACK with parsed json and CBARGS."
  (pcase-let (((seq host &rest hosts) (yt-com-invidious-hosts)))
    (apply #'yt-com--get (yt-com-build-url host method id params)
           #'yt-com--post-get-data
           #'yt-com--post-get-data-error
           hosts (list method id params) callback cbargs)))

(defun yt-com--post-get-data (hosts request callback &rest cbargs)
  "Extract data from json in http buffer.
Call CALLBACK with data and CBARGS on success.  REQUEST and HOSTS
are used to retry on json parse error."
  (goto-char url-http-end-of-headers)
  (condition-case nil
      (apply callback (json-parse-buffer :false-object nil) cbargs)
    (json-error
     (apply #'yt-com--post-get-data-error hosts request callback cbargs))))

(defun yt-com--post-get-data-error (hosts request callback &rest cbargs)
  "Function called on `yt-com--get' error.
If HOSTS is not empty, retry calling `yt-com--get' with next
host and the same REQUEST, CALLBACK and CBARGS."
  (if (seq-empty-p hosts)
      (error "No more hosts to try")
    (pcase-let (((seq host &rest hosts) hosts))
      (apply #'yt-com--get (apply #'yt-com-build-url host request)
             #'yt-com--post-get-data
             #'yt-com--post-get-data-error
             hosts request callback cbargs))))

;;;;; TITLE - Network routine that gets video title

(defun yt-com--get-title (id callback &rest cbargs)
  "Get title of video with ID.
Call CALLBACK with title and CBARGS on success."
  (apply #'yt-com--get-data "videos" id '(("fields" "title"))
         #'yt-com--post-get-title
         callback cbargs))

(defun yt-com--post-get-title (data callback &rest cbargs)
  "Get title from DATA and call CALLBACK with CBARGS on it."
  (apply callback (map-elt data "title") cbargs))

;;;;; COMMENTS - Network routine that gets video comments

(defun yt-com--get-comments (id continuation callback &rest cbargs)
  "Get comments of video with ID.
If CONTINUATION is not null, pass it as url parameter.  Call
CALLBACK with comments, continuation, comment count and CBARGS on
success."
  (apply #'yt-com--get-data
         "comments" id (when continuation `(("continuation" ,continuation)))
         #'yt-com--post-get-comments callback cbargs))

(defun yt-com--post-get-comments (data callback &rest cbargs)
  "Extract comment information from DATA.
Call CALLBACK with comments, continuation, comment count and
CBARGS."
  (pcase-let (((map ("comments" comments)
                    ("continuation" continuation)
                    ("commentCount" comment-count))
               data))
    (apply callback comments continuation comment-count cbargs)))

;;;;; THUMBNAILS - Network routine that gets thumbnails of comments

(defun yt-com--get-thumbnails (comments callback &rest cbargs)
  "Get thumbnails for COMMENTS.
Call CALLBACK after thumbnail retrieval with CBARGS."
  (if (seq-empty-p comments)
      (apply callback cbargs)
    (pcase-let* (((seq comment &rest comments) comments)
                 ((map ("authorThumbnails" thumbnails)) comment)
                 (get-url (pcase-lambda ((map ("url" url))) url))
                 ((seq url &rest urls) (seq-map get-url thumbnails)))
      (apply #'yt-com--get url
             #'yt-com--post-get-thumbnails
             #'yt-com--post-get-thumbnails-error
             comment urls comments callback cbargs))))

(defun yt-com--post-get-thumbnails (comment _urls comments callback &rest cbargs)
  "Subroutine of `yt-com--post-get-thumbnails'.
Put downloaded thumbnail to COMMENT.  Get thumbnails for other
COMMENTS.  Call CALLBACK with CBARGS afterwards."
  (let* ((start (1+ url-http-end-of-headers))
         (end (point-max))
         (data (buffer-substring-no-properties start end))
         (image (create-image data nil t))
         (cache (yt-com--comment-cache comment)))
    (map-put! cache "thumbnail" image))
  (apply #'yt-com--get-thumbnails comments callback cbargs))

(defun yt-com--post-get-thumbnails-error (comment urls comments callback &rest cbargs)
  "Subroutine of `yt-com--post-get-thumbnails'.
Retry download of thumbnails with remaining URLS.  Put it to
COMMENT.  Continue with remaining COMMENTS.  Call CALLBACK with
CBARGS afterwards."
  (if (seq-empty-p urls)
      (apply #'yt-com--get-thumbnails comments callback cbargs)
    (pcase-let (((seq url &rest urls) urls))
      (apply #'yt-com--get url
             #'yt-com--post-get-thumbnails
             #'yt-com--post-get-thumbnails-error
             comment urls comments callback cbargs))))

;;;; DRAW - Routines that display data on screen

(defun yt-com--draw-url (label url)
  "Draw a link on screen with LABEL and URL."
  (insert-text-button label 'shr-url url 'keymap shr-map
                      'follow-link t 'mouse-face 'highlight))

(defun yt-com--draw-button (label action &rest properties)
  "Draw a button with LABEL and ACTION.
Apply additional PROPERTIES."
  (apply #'insert-text-button label 'follow-link t 'action action properties))

(defun yt-com--draw-header (comment)
  "Draw COMMENT header on screen."
  (pcase-let* ((cache (yt-com--comment-cache comment))
               ((map ("thumbnail" thumbnail) ("header" header)) cache))
    (if thumbnail (insert-image thumbnail) (insert "*"))
    (unless header
      (pcase-let (((map ("author" author) ("authorUrl" author-url)
                        ("publishedText" published-text)
                        ("likeCount" like-count)
                        ("authorIsChannelOwner" ownerp)
                        ("creatorHeart" (map ("creatorName" heart))))
                   comment))
        (setq header
              (with-temp-buffer
                (insert " ")
                (yt-com--draw-url author (concat "https://youtube.com" author-url))
                (insert " - " (propertize published-text 'face 'yt-com-time-face))
                (insert " - " (propertize (number-to-string like-count) 'face 'yt-com-like-face))
                (when heart (insert " " (propertize heart 'face 'yt-com-owner-face)))
                (when ownerp (insert " - " (propertize "OWNER" 'face 'yt-com-owner-face)))
                (buffer-string))))
      (map-put! cache "header" header))
    (insert header)))

(defun yt-com--draw-content (comment)
  "Draw COMMENT content on screen."
  (pcase-let* ((cache (yt-com--comment-cache comment))
               ((map ("content" content)) cache))
    (unless content
      (pcase-let (((map ("contentHtml" content-html)) comment))
        (setq content
              (with-temp-buffer
                (let ((start (point)))
                  (insert "<base href=\"https://youtube.com\"><p>" content-html "</p>")
                  (shr-render-region start (point)))
                (buffer-string))))
      (map-put! cache "content" content))
    (insert content)))

(defun yt-com--draw (comment)
  "Draw COMMENT on screen."
  (pcase-let (((map ("replies" (map ("replyCount" reply-count)
                                    ("replies" replies)
                                    ("continuation" continuation))))
               comment))
    (yt-com--draw-header comment)
    (insert "\n")
    (yt-com--draw-content comment)
    (when replies
      (let ((shr-indentation 3))
        (seq-doseq (reply replies)
          (insert "\n   ")
          (yt-com--draw reply))))
    (when continuation
      (insert "\n")
      (yt-com--draw-button
       (format "Show %d replies" (- reply-count (length replies)))
       #'yt-com--load-more-replies 'continuation continuation)
      (insert "\n"))))

(defun yt-com--buffer-header (id title comment-count)
  "Return a string with Yt-Com buffer header.
It's a link with TITLE that leads to video with ID.  Display
COMMENT-COUNT below."
  (with-temp-buffer
    (yt-com--draw-url title (concat "https://www.youtube.com/watch?v=" id))
    (insert "\n" (number-to-string comment-count) " comments\n")
    (buffer-string)))

(defun yt-com--buffer-footer (&optional continuation)
  "Return a string with Yt-Com buffer footer.
It's a button that loads more comments.  Use CONTINUATION to make
a request."
  (if continuation
      (with-temp-buffer
        (yt-com--draw-button "Load more" #'yt-com--load-more-comments
                             'continuation continuation)
        (buffer-string))
    "No more comments"))

;;;; ACTIONS - Routines of all possible actions that Yt-Com is capable of

;;;;; MAIN - Initial display of comments

(defun yt-com--main (id)
  "Display comments of video with ID."
  (yt-com--get-title id #'yt-com--post-main-title id))

(defun yt-com--post-main-title (title id)
  "Subroutine of `yt-com--main'.
Fetch comments of video with ID.  Display them on screen with
TITLE."
  (yt-com--get-comments id nil #'yt-com--post-main-comments id title))

(defun yt-com--post-main-comments (comments continuation comment-count id title)
  "Subroutine of `yt-com--main'.
Set up `yt-com-mode' in a buffer, display COMMENTS, CONTINUATION,
COMMENT-COUNT and TITLE in it and pop up the buffer.  Set ID as
buffer-local variable."
  (with-current-buffer (get-buffer-create yt-com-buffer-name)
    (yt-com-mode)
    (setq-local yt-com--id id)
    (let ((header (yt-com--buffer-header id title comment-count))
          (footer (yt-com--buffer-footer continuation)))
      (ewoc-set-hf yt-com--ewoc header footer))
    (let* ((enter-last (apply-partially #'ewoc-enter-last yt-com--ewoc))
           (nodes (seq-map enter-last comments)))
      (pop-to-buffer (current-buffer))
      (yt-com--get-thumbnails comments #'yt-com--invalidate yt-com--ewoc nodes))))

;;;;; LOAD MORE COMMENTS - Button action that fetches next page of comments

(defun yt-com--load-more-comments (button)
  "Act on BUTTON to load more comments."
  (yt-com--get-comments yt-com--id (button-get button 'continuation)
                        #'yt-com--post-load-more-comments
                        yt-com--ewoc))

(defun yt-com--post-load-more-comments (comments continuation _count ewoc)
  "Subroutine of `yt-com--load-more-comments'.
Add new COMMENTS to screen.  Update CONTINUATION of \"Load more\"
button.  EWOC is the ewoc data of Yt-Com buffer."
  (let* ((enter-last (apply-partially #'ewoc-enter-last ewoc))
         (nodes (seq-map enter-last comments))
         (header (car (ewoc-get-hf ewoc))))
    (ewoc-set-hf ewoc header (yt-com--buffer-footer continuation))
    (yt-com--get-thumbnails comments #'yt-com--invalidate ewoc nodes)))

;;;;; LOAD MORE REPLIES - Button action that fetches next page of replies

(defun yt-com--load-more-replies (button)
  "Act on BUTTON to load more replies."
  (let ((continuation (button-get button 'continuation))
        (node (ewoc-locate yt-com--ewoc)))
    (yt-com--get-comments yt-com--id continuation
                          #'yt-com--post-load-more-replies
                          yt-com--ewoc node)))

(defun yt-com--post-load-more-replies (replies continuation _count ewoc node)
  "Subroutine of `yt-com--load-more-replies'.
Add new REPLIES to screen.  Update CONTINUATION of a comment in
NODE.  EWOC is ewoc data of Yt-Com buffer."
  (let* ((data (ewoc-data node))
         (replies-data (map-elt data "replies"))
         (present-replies (map-elt replies-data "replies"))
         (replies (seq-concatenate 'vector present-replies replies)))
    (map-put! replies-data "replies" replies)
    (map-put! replies-data "continuation" continuation))
  (yt-com--invalidate ewoc (list node))
  (yt-com--get-thumbnails replies #'yt-com--invalidate ewoc (list node)))

;;;; COMMANDS

(define-derived-mode yt-com-mode special-mode "Yt-Com"
  "Mode used to visualize youtube comments."
  :group 'yt-com
  (buffer-disable-undo)
  (with-silent-modifications (erase-buffer))
  (setq yt-com--ewoc (ewoc-create #'yt-com--draw)))

;;;###autoload
(defun yt-com (url-or-id)
  "View comments of video with URL-OR-ID."
  (interactive "sYoutube (or invidious) video url or id: ")
  (yt-com--main (string-trim (yt-com-extract-id-from-url url-or-id))))

(provide 'yt-com)
;;; yt-com.el ends here
