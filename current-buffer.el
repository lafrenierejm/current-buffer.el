;;; current-buffer.el --- Utilities for operating on the current buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Joseph M LaFreniere

;; Author: Joseph M LaFreniere <joseph@lafreniere.xyz>
;; Maintainer: Joseph M LaFreniere <joseph@lafreniere.xyz>
;; License: GPL3+
;; URL: https://github.com/lafrenierejm/current-buffer.el
;; Version: 0.1.0
;; Keywords: convenience
;; Package-Requires: ((emacs "28.1") (f "0.20.0"))

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; No keybindings are added by this package for any of its functions.  To
;; benefit from muscle memory, it is recommended to bind them to key chords
;; similar to their more generic counterparts. For example, for
;; `current-buffer-kill' one might do:
;; (keymap-global-set "C-x C-k" #'current-buffer-kill)

;;; Code:
(require 'f)
(require 'files)
(require 'project)
(require 'vc)
(require 'vc-hooks)

(defun current-buffer--obj-to-buffer (&optional buffer)
  "Get the BUFFER to use.

If BUFFER is nil, use the current buffer.
Else if BUFFER is a string, use that string as the name of the buffer.
Else throw an error."
  (cond
   ;; If no BUFFER was provided...
   ((not buffer)
    ;; Return the current buffer.
    (get-buffer (buffer-name)))
   ;; Else if BUFFER is an editor buffer...
   ((bufferp buffer)
    ;; Return BUFFER as-is.
    buffer)
   ;; Else if BUFFER is a string...
   ((stringp buffer)
    ;; Return the corresponding buffer object.
    (get-buffer buffer))
   ;; Else...
   (t
    ;; Throw an error.
    (error "Invalid BUFFER argument %s of type %s passed"
           buffer
           (type-of buffer)))))

(defun current-buffer--get-buffer (verb)
  (if current-prefix-arg
      (get-buffer (read-buffer (format "Buffer to %s with visiting file: "
                                       verb)
                               (get-buffer (buffer-name))))))

(defun current-buffer--read-file-name (verb buffer-name)
  "Prompt the user for the destination of applying VERB to BUFFER-NAME's visiting file."
  (read-file-name (format "%s the file visited by buffer %s to: "
                          verb
                          buffer-name)))

;;;###autoload
(defun current-buffer-rename (new-file-name &optional buffer ok-if-already-exists)
  "Rename a buffer and the file it's visiting, if any, to NEW-FILE-NAME.

If BUFFER is not provided, use the current buffer.
If BUFFER is a string, use that string as the name of the buffer to rename.
If BUFFER is a buffer object, use it as the subject of the rename.

OK-IF-ALREADY-EXISTS is passed directly to `rename-file'."
  ;; If called interactively...
  (interactive (let* ((buffer (current-buffer--get-buffer "rename"))
                      (new-file-name
                       (current-buffer--read-file-name "rename"
                                                       (buffer-name buffer)))
                      (ok-if-already-exists 1)) ; Request confirmation before overwrite.
                 (list new-file-name buffer ok-if-already-exists)))
  (let* ((buffer (current-buffer--obj-to-buffer buffer))
         (file (buffer-file-name buffer))
         (orig-buffer-name (buffer-name buffer))
         (new-buffer-name nil))
    (mkdir (f-parent new-file-name) t)
    (cond
     ;; If FILE is tracked in VC...
     ((vc-backend file)
      ;; Rename FILE through VC.
      (vc-rename-file file new-file-name))
     ;; Else if FILE exists at all...
     ((file-exists-p file)
      ;; Rename FILE normally.
      (rename-file file new-file-name ok-if-already-exists)))
    ;; Rename BUFFER, using `generate-new-buffer-name' if necessary.
    ;; Record the name actually assigned.
    (setq new-buffer-name (rename-buffer new-file-name t))
    (set-visited-file-name new-file-name t t)
    (message "Renamed buffer %s to %s and its file to %s."
             orig-buffer-name new-buffer-name
             new-file-name)))

;;;###autoload
(defun current-buffer-delete (&optional buffer)
  "Delete the file BUFFER is visiting, if any, then kill the buffer.

If BUFFER is not provided, use the current buffer.
If BUFFER is a string, use that string as the name of the buffer to use.
If BUFFER is a symbol, prompt the user for the name of the buffer to use."
  ;; If called interactively...
  (interactive (list (current-buffer--get-buffer "delete")))
  (let* ((buffer (current-buffer--obj-to-buffer buffer))
         (file (buffer-file-name buffer)))
    (when file
      (cond
       ;; If FILE is tracked in VC, delete it through VC.
       ((vc-backend file)
        (vc-delete-file file))
       ;; Else if FILE exists, delete it normally.
       ((file-exists-p file)
        (delete-file file 'trash)))
      (message "Deleted file %s" file))
    (kill-buffer buffer)))

;;;###autoload
(defun current-buffer-copy
    (new-file-name
     &optional
     buffer
     confirm-overwrite
     keep-time
     preserve-uid-gid
     perserve-permissions)
  "Copy the file a buffer is visiting, if any, to NEW-FILE-NAME and open the copy in a new buffer.

If BUFFER is not provided, use the current buffer.
If BUFFER is a string, use that string as the name of the buffer to rename.
If BUFFER is a buffer object, use it as the subject of the rename.

If the specified buffer is not visiting a file, write the buffer's contents to NEW-FILE-NAME.

If CONFIRM-OVERWRITE is non-nil, this function asks for confirmation before overwiting an existing file at NEW-FILE-NAME.
Interactively, confirmation is required unless a prefix argument is supplied.

KEEP-TIME, PRESERVE-UID-GID, and PRESERVE-PERMISSIONS are passed directly to `copy-file'."
  ;; If called interactively...
  (interactive (let* ((buffer (current-buffer--get-buffer "copy"))
                      (new-file-name (current-buffer--read-file-name
                                      "copy" (buffer-name buffer)))
                      ;; confirm before overwriting an existing file,
                      ;; unless called with a prefix argument.
                      (confirm-overwrite (if current-prefix-arg t 1)))
                 (list new-file-name buffer confirm-overwrite)))
  (let* ((buffer (current-buffer--obj-to-buffer buffer))
         (file (buffer-file-name buffer))
         (new-buffer-name (buffer-name buffer)))
    ;; If BUFFER is not visiting a file...
    (if (not (file-exists-p file))
        ;; Write the content of BUFFER to NEW-FILE-NAME.
        (write-file new-file-name confirm-overwrite)
      ;; Else copy the visited file FILE to NEW-FILE-NAME.
      (copy-file file
                 new-file-name
                 ;; If CONFIRM-OVERWRITE is non-nil...
                 (if confirm-overwrite
                     ;; Ask for confirmation before overwriting.
                     1
                   ;; Else overwrite without prompting.
                   t)
                 keep-time
                 preserve-uid-gid
                 perserve-permissions)
      ;; Open NEW-FILE-NAME in a separate buffer.
      (setq new-buffer-name (buffer-name (find-file-noselect new-file-name))))
    (message "Wrote buffer %s to %s and opened the new file as %s."
             (buffer-name buffer) new-file-name new-buffer-name)))

;;;###autoload
(defun current-buffer-revert (&optional noconfirm)
  "Replace current buffer text with the text of the visited file on disk.

If NOCONFIRM is nil, prompt the user before reverting a modified buffer.
NOCONFIRM is set to t when called interactively with a prefix argument."
  (interactive (list current-prefix-arg))
  (revert-buffer :ignore-auto (or noconfirm (not (buffer-modified-p)))))

;;;###autoload
(defun current-buffer-kill (&optional ignore-buffer-modified-p)
  "Kill the current buffer.

If IGNORE-BUFFER-MODIFIED-P is nil, prompt the user before killing a modified buffer.
IGNORE-BUFFER-MODIFIED-P is set to t when called interactively with a prefix argument."
  (interactive (list current-prefix-arg))
  (when ignore-buffer-modified-p
    (set-buffer-modified-p nil))
  (kill-buffer (current-buffer)))

;;;###autoload
(defun current-buffer-yank-name ()
  "Place the name of the current buffer on the kill ring."
  (interactive)
  (let ((buffer-name (kill-new (buffer-name (current-buffer)))))
    (message "Copied buffer name '%s' to the clipboard." buffer-name)))

;;;###autoload
(defun current-buffer-yank-path (&optional PROJECT-RELATIVE)
  "Place the path of the current buffer's visited file on the kill ring.

The default behavior is to use the current file's absolute path.  If optional
argument PROJECT-RELATIVE is set, use the path relative to the current project's
root, if any.  PROJECT-RELATIVE is set when called interactively with a prefix
argument."
  (interactive (list current-prefix-arg))
  (when-let* ((path (or (buffer-file-name) default-directory))
              (path-absolute (expand-file-name path))
              (path-final (if-let* ((PROJECT-RELATIVE)
                                    (project (project-current nil))
                                    (project-root (project-root project)))
                              (f-relative path-absolute project-root)
                            path-absolute)))
    (kill-new path-final)
    (message "Copied buffer path '%s' to the clipboard." path-final)))

;;;###autoload
(defun current-buffer-dired ()
  "Open the current directory in Dired."
  (interactive)
  (when-let* ((file-path (or (buffer-file-name) default-directory))
              (directory-path (file-name-directory file-path)))
    (dired directory-path)))

(provide 'current-buffer)
;;; current-buffer.el ends here
