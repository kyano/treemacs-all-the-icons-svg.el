;;; treemacs-all-the-icons-svg.el --- all-the-icons integration for treemacs -*- lexical-binding: t -*-

;; Copyright (C) 2023 Anho Ki

;; Author: Anho Ki
;; Maintainer: Anho Ki
;; URL: https://github.com/kyano/treemacs-all-the-icons-svg.el
;; Version: 0.0.2
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
(require 'modus-themes)
(require 'treemacs)

;; FIXME: To get proper align, it draws useless hidden `chevron-right' characters.
;; If the formatted string is started with whitespaces(` ' or `\t'), the SVG icons are not drawn.

;; FIXME: `foreground-color' should not be the fixed value.
(defface hidden-face
  `((t :foreground ,(modus-themes-with-colors bg-main)))
  "Face used for the dummy spaces.")

(defconst padding-char
  (if (string-equal "darwin" (symbol-name system-type))
      " "
    "\t"))

(treemacs-create-theme "all-the-icons-svg"
  :config
  (progn
    (dolist (item all-the-icons-extension-icon-alist)
      (let ((extension (nth 0 item))
            (extensions (list (nth 0 item))))
        (treemacs-create-icon
         :icon (format "%s%s%s"
                       (all-the-icons-octicons "chevron-right" :face 'hidden-face)
                       (all-the-icons-icon-for-file (format "filename.%s" extension))
                       padding-char)
         :extensions extensions
         :fallback 'same-as-icon)))

    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'hidden-face)
                   (all-the-icons-icon-for-file "LICENSE")
                   padding-char)
     :extensions ("license" "copying")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'hidden-face)
                   (all-the-icons-icon-for-file "readme")
                   padding-char)
     :extensions ("readme" "readme.org" "readme.md" "readme.rst" "readme.txt")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'hidden-face)
                   (all-the-icons-icon-for-file "Makefile")
                   padding-char)
     :extensions ("makefile")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'hidden-face)
                   (all-the-icons-icon-for-file "go.mod")
                   padding-char)
     :extensions ("go.mod" "go.sum")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'hidden-face)
                   (all-the-icons-icon-for-file "package.json")
                   padding-char)
     :extensions ("package.json")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'hidden-face)
                   (all-the-icons-icon-for-file "filename.sh")
                   padding-char)
     :extensions ("bash" "csh" "profile")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'hidden-face)
                   (all-the-icons-icon-for-file "filename.bashrc")
                   padding-char)
     :extensions ("bash_logout")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'hidden-face)
                   (all-the-icons-devopicons "vim")
                   padding-char)
     :extensions ("vimrc")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'hidden-face)
                   (all-the-icons-devopicons "git")
                   padding-char)
     :extensions ("gitignore" "gitconfig")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'hidden-face)
                   (all-the-icons-icon-for-file "filename.key")
                   padding-char)
     :extensions ("netrc")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-down" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "documents" :face 'dired-directory)
                   padding-char)
     :extensions ("docs-open" "documents-open" "문서-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "documents" :face 'dired-directory)
                   padding-char)
     :extensions ("docs-closed" "documents-closed" "문서-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-down" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "code" :face 'dired-directory)
                   padding-char)
     :extensions ("src-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "code" :face 'dired-directory)
                   padding-char)
     :extensions ("src-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-down" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "desktop" :face 'dired-directory)
                   padding-char)
     :extensions ("desktop-open" "바탕화면-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "desktop" :face 'dired-directory)
                   padding-char)
     :extensions ("desktop-closed" "바탕화면-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-down" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "download" :face 'dired-directory)
                   padding-char)
     :extensions ("download-open" "downloads-open" "다운로드-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right")
                   (all-the-icons-icon-for-dir "download")
                   padding-char)
     :extensions ("download-closed" "downloads-closed" "다운로드-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-down" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "movies" :face 'dired-directory)
                   padding-char)
     :extensions ("movies-open" "비디오-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "movies" :face 'dired-directory)
                   padding-char)
     :extensions ("movies-closed" "비디오-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-down" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "music" :face 'dired-directory)
                   padding-char)
     :extensions ("music-open" "음악-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "music" :face 'dired-directory)
                   padding-char)
     :extensions ("music-closed" "음악-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-down" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "photos" :face 'dired-directory)
                   padding-char)
     :extensions ("photos-open" "사진-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "photos" :face 'dired-directory)
                   padding-char)
     :extensions ("photos-closed" "사진-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-down" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "pictures" :face 'dired-directory)
                   padding-char)
     :extensions ("pictures-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "pictures" :face 'dired-directory)
                   padding-char)
     :extensions ("pictures-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-down" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "workspace" :face 'dired-directory)
                   padding-char)
     :extensions ("workspace-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "workspace" :face 'dired-directory)
                   padding-char)
     :extensions ("workspace-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-down" :face 'dired-directory)
                   (all-the-icons-devopicons "git" :face 'dired-directory)
                   padding-char)
     :extensions ("git-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'dired-directory)
                   (all-the-icons-devopicons "git" :face 'dired-directory)
                   padding-char)
     :extensions ("git-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-down" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "trash" :face 'dired-directory)
                   padding-char)
     :extensions ("trash-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "trash" :face 'dired-directory)
                   padding-char)
     :extensions ("trash-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-down" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "dropbox" :face 'dired-directory)
                   padding-char)
     :extensions ("dropbox-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "dropbox" :face 'dired-directory)
                   padding-char)
     :extensions ("dropbox-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-down" :face 'dired-directory)
                   (all-the-icons-devopicons "onedrive" :face 'dired-directory)
                   padding-char)
     :extensions ("onedrive-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'dired-directory)
                   (all-the-icons-devopicons "onedrive" :face 'dired-directory)
                   padding-char)
     :extensions ("onedrive-closed")
     :fallback 'same-as-icon)

    (treemacs-create-icon
     :icon (format "%s%s"
                   (all-the-icons-octicons "repo" :face 'dired-directory)
                   padding-char)
     :extensions (root-open root-closed)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-down" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "dirname" :face 'dired-directory)
                   padding-char)
     :extensions (dir-open)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "dirname" :face 'dired-directory)
                   padding-char)
     :extensions (dir-closed)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'hidden-face)
                   (all-the-icons-icon-for-file "filename")
                   padding-char)
     :extensions (fallback)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-down" :face 'dired-directory)
                   (all-the-icons-octicons "package" :face 'dired-directory)
                   padding-char)
     :extensions (tag-open)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'dired-directory)
                   (all-the-icons-octicons "package" :face 'dired-directory)
                   padding-char)
     :extensions (tag-closed)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'hidden-face)
                   (all-the-icons-octicons "tag")
                   padding-char)
     :extensions (tag-leaf)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'hidden-face)
                   (all-the-icons-octicons "flame" :face 'all-the-icons-red)
                   padding-char)
     :extensions (error)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'hidden-face)
                   (all-the-icons-octicons "stop" :face 'all-the-icons-yellow)
                   padding-char)
     :extensions (warning)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'hidden-face)
                   (all-the-icons-octicons "info" :face 'all-the-icons-blue)
                   padding-char)
     :extensions (info)
     :fallback 'same-as-icon)))

(provide 'treemacs-all-the-icons-svg)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; treemacs-all-the-icons-svg.el ends here
