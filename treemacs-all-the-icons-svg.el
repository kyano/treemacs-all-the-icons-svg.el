;;; treemacs-all-the-icons-svg.el --- all-the-icons integration for treemacs -*- lexical-binding: t -*-

;; Copyright (C) 2024 Anho Ki

;; Author: Anho Ki
;; Maintainer: Anho Ki
;; URL: https://github.com/kyano/treemacs-all-the-icons-svg.el
;; Version: 0.0.4
;; Package-Requires: ((emacs "27.1") (all-the-icons "6.0.0") (treemacs "0.0"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; all-the-icons integration.

;;; Code:

(require 'dired)
(require 'all-the-icons)
(require 'treemacs)

(defface tatis/root-face
  '((t (:inherit treemacs-root-face :underline nil)))
  "Face used for the root icon in all-the-icons-svg theme.")
(defface tatis/hidden-face
  `((t :foreground ,(face-background 'default)))
  "Face used for the dummy spaces in all-the-icons-svg theme.")

;; FIXME: To get proper align, it draws useless hidden `chevron-right' characters.
;; If the formatted string is started with whitespaces(` ' or `\t'), the SVG icons are not drawn.
(defconst tatis/padding-char-left
  (all-the-icons-octicons "chevron-right" :face 'tatis/hidden-face))
(defconst tatis/padding-char-right "  ")

(defun tatis/icon-with-padding (name &optional fn &rest rest-arg)
  "Icon of NAME with left and right paddings.

`tatis/padding-char-left' and `tatis/padding-char-right' are used
for padding characters.  When FN is nil,
`all-the-icons-icon-for-file' is used.  REST-ARG will be passed
to FN as extra arguments too."

  (concat tatis/padding-char-left
          (if fn
              (apply fn name rest-arg)
            (all-the-icons-icon-for-file name))
          tatis/padding-char-right))

(defun tatis/icon-with-chevron (direction name &optional fn face &rest rest-arg)
  "Icon of NAME with the chevron mark of DIRECTION.

`tatis/padding-char-right' is appended as a right padding.  When
FN is nil, `all-the-icons-icon-for-dir' is used.  When FACE is
nil, `dired-directory' is used.  REST-ARG will be passed to FN as
extra arguments too."

  (concat (all-the-icons-octicons
           (concat "chevron-" direction)
           :face (or face 'dired-directory))
          (if fn
              (apply fn name :face (or face 'dired-directory) rest-arg)
            (all-the-icons-icon-for-dir name :face (or face 'dired-directory)))
          tatis/padding-char-right))

(defun tatis/icons-for-file (list)
  "Generate `treemacs-create-theme' statements for files.

The first item of LIST is for icon, and others are used for
extensions list."

  (let* ((icon (car list))
         (extensions (cdr list)))
    (treemacs-create-icon
     :icon (tatis/icon-with-padding icon)
     :extensions extensions
     :fallback 'same-as-icon)))

(defun tatis/icons-for-directory (list)
  "Generate `treemacs-create-theme' statements for directories.

The first item of LIST is for icon, and others are used for
directories list."

  (let* ((icon (car list))
         (dirs (cdr list))
         (extensions-open (mapcar (lambda (x) (concat x "-open")) dirs))
         (extensions-closed (mapcar (lambda (x) (concat x "-closed")) dirs)))
    (treemacs-create-icon
     :icon (tatis/icon-with-chevron "down" icon)
     :extensions extensions-open
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (tatis/icon-with-chevron "right" icon)
     :extensions extensions-closed
     :fallback 'same-as-icon)))

(treemacs-create-theme "all-the-icons-svg"
  :config
  (progn
    (dolist (item all-the-icons-extension-icon-alist)
      (let ((extension (nth 0 item))
            (extensions (list (nth 0 item))))
        (treemacs-create-icon
         :icon (tatis/icon-with-padding (concat "filename." extension))
         :extensions extensions
         :fallback 'same-as-icon)))
    (dolist (suffix '("spec.js" "spec.jsx" "spec.ts" "test.js" "test.jsx" "test.ts" ".npmignore" "Brewfile"
                      "CMakeCache.txt" "CMakeLists.txt" "Gemfile" "Gemfile.lock" "LICENSE" "Makefile" "TAGS" "TODO"
                      "bower.json" "go.mod" "go.sum" "go.work" "meson.build" "meson_options.txt" "mix.lock"
                      "package.json" "package.lock.json" "README" "README.org" "README.md" "README.rst" "README.txt"
                      "serverless.yml" "spec.rb" "spec_heeler.rb" "test.rb" "test_helper.rb" "apache" "nginx" "~"))
      (let ((extensions (list (downcase suffix))))
        (treemacs-create-icon
         :icon (tatis/icon-with-padding suffix)
         :extensions extensions
         :fallback 'same-as-icon)))
    (tatis/icons-for-file '("LICENSE" "copying"))
    (tatis/icons-for-file '("eslint" "eslintrc" "eslintrc.json" ".eslintrc.json"))
    (tatis/icons-for-file '(".git" "gitconfig" "gitignore" "gitattributes"))
    (tatis/icons-for-file '("package.lock.json" "package-lock.json"))
    (tatis/icons-for-file '("filename.sh" "profile"))
    (tatis/icons-for-file '("filename.bashrc" "bash" "bash_logout"))
    (tatis/icons-for-file '("filename.key" "netrc"))

    ;; directories
    (treemacs-create-icon
     :icon (tatis/icon-with-chevron "down" "folder_open"
                                    #'all-the-icons-fluentui-system-icons
                                    'dired-directory
                                    :style 'filled)
     :extensions (dir-open)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (tatis/icon-with-chevron "right" "folder"
                                    #'all-the-icons-fluentui-system-icons
                                    'dired-directory
                                    :style 'filled)
     :extensions (dir-closed)
     :fallback 'same-as-icon)
    (tatis/icons-for-directory '("code" "src"))
    (tatis/icons-for-directory '("desktop" "desktop" "바탕화면"))
    (tatis/icons-for-directory '("documents" "docs" "documents" "문서"))
    (tatis/icons-for-directory '("download" "download" "downloads" "다운로드"))
    (tatis/icons-for-directory '("dropbox" "dropbox"))
    (tatis/icons-for-directory '("onedrive" "onedrive"))
    (tatis/icons-for-directory '("movies" "movies" "videos" "비디오"))
    (tatis/icons-for-directory '("music" "music" "음악"))
    (tatis/icons-for-directory '("photos" "photos" "사진"))
    (tatis/icons-for-directory '("pictures" "pictures"))
    (tatis/icons-for-directory '("test" "test"))
    (tatis/icons-for-directory '("trash" "trash"))
    (tatis/icons-for-directory '("workspace" "workspace"))
    (tatis/icons-for-directory '(".git" "git"))

    ;; Treemacs-specific
    (treemacs-create-icon
     :icon (tatis/icon-with-chevron "down" "repo"
                                    #'all-the-icons-octicons
                                    'tatis/root-face)
     :extensions (root-open)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (tatis/icon-with-chevron "right" "repo"
                                    #'all-the-icons-octicons
                                    'tatis/root-face)
     :extensions (root-closed)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (tatis/icon-with-chevron "down" "package"
                                    #'all-the-icons-octicons
                                    'treemacs-tags-face)
     :extensions (tag-open)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (tatis/icon-with-chevron "right" "package"
                                    #'all-the-icons-octicons
                                    'treemacs-tags-face)
     :extensions (tag-closed)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (tatis/icon-with-padding "tag"
                                    #'all-the-icons-octicons
                                    :face 'treemacs-tags-face)
     :extensions (tag-leaf)
     :fallback 'same-as-icon)

    ;; error, warning, and info
    (treemacs-create-icon
     :icon (tatis/icon-with-padding "flame"
                                    #'all-the-icons-octicons
                                    :face 'all-the-icons-red)
     :extensions (error)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (tatis/icon-with-padding "stop"
                                    #'all-the-icons-octicons
                                    :face 'all-the-icons-yellow)
     :extensions (warning)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (tatis/icon-with-padding "info"
                                    #'all-the-icons-octicons
                                    :face 'all-the-icons-blue)
     :extensions (info)
     :fallback 'same-as-icon)

    ;; fallback
    (treemacs-create-icon
     :icon (tatis/icon-with-padding "filename")
     :extensions (fallback)
     :fallback 'same-as-icon)))

(provide 'treemacs-all-the-icons-svg)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; treemacs-all-the-icons-svg.el ends here
