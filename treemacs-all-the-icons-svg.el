;;; treemacs-all-the-icons-svg.el --- all-the-icons integration for treemacs -*- lexical-binding: t -*-

;; Copyright (C) 2023 Anho Ki

;; Author: Anho Ki
;; Maintainer: Anho Ki
;; URL: https://github.com/kyano/treemacs-all-the-icons-svg.el
;; Version: 0.0.3
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

;; FIXME: `foreground-color' should not be the fixed value.
(defface atis/hidden-face
  `((t :foreground ,(face-background 'default)))
  "Face used for the dummy spaces.")

;; FIXME: To get proper align, it draws useless hidden `chevron-right' characters.
;; If the formatted string is started with whitespaces(` ' or `\t'), the SVG icons are not drawn.
(defconst atis/padding-char-left
  (all-the-icons-octicons "chevron-right"
                          :face 'atis/hidden-face))
(defconst atis/padding-char-right "\t")

(treemacs-create-theme "all-the-icons-svg"
  :config
  (progn
    (dolist (item all-the-icons-extension-icon-alist)
      (let ((extension (nth 0 item))
            (extensions (list (nth 0 item))))
        (treemacs-create-icon
         :icon (format "%s%s%s"
                       atis/padding-char-left
                       (all-the-icons-icon-for-file (format "filename.%s" extension))
                       atis/padding-char-right)
         :extensions extensions
         :fallback 'same-as-icon)))

    (treemacs-create-icon
     :icon (format "%s%s%s"
                   atis/padding-char-left
                   (all-the-icons-icon-for-file "LICENSE")
                   atis/padding-char-right)
     :extensions ("license" "copying")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   atis/padding-char-left
                   (all-the-icons-icon-for-file "readme")
                   atis/padding-char-right)
     :extensions ("readme" "readme.org" "readme.md" "readme.rst" "readme.txt")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   atis/padding-char-left
                   (all-the-icons-icon-for-file "Makefile")
                   atis/padding-char-right)
     :extensions ("makefile")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   atis/padding-char-left
                   (all-the-icons-icon-for-file "go.mod")
                   atis/padding-char-right)
     :extensions ("go.mod" "go.sum")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   atis/padding-char-left
                   (all-the-icons-icon-for-file "package.json")
                   atis/padding-char-right)
     :extensions ("package.json")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   atis/padding-char-left
                   (all-the-icons-icon-for-file "filename.sh")
                   atis/padding-char-right)
     :extensions ("bash" "csh" "profile")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   atis/padding-char-left
                   (all-the-icons-icon-for-file "filename.bashrc")
                   atis/padding-char-right)
     :extensions ("bash_logout")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   atis/padding-char-left
                   (all-the-icons-devopicons "vim")
                   atis/padding-char-right)
     :extensions ("vimrc")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   atis/padding-char-left
                   (all-the-icons-devopicons "git")
                   atis/padding-char-right)
     :extensions ("gitignore" "gitconfig")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   atis/padding-char-left
                   (all-the-icons-icon-for-file "filename.key")
                   atis/padding-char-right)
     :extensions ("netrc")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-down" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "documents" :face 'dired-directory)
                   atis/padding-char-right)
     :extensions ("docs-open" "documents-open" "문서-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "documents" :face 'dired-directory)
                   atis/padding-char-right)
     :extensions ("docs-closed" "documents-closed" "문서-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-down" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "code" :face 'dired-directory)
                   atis/padding-char-right)
     :extensions ("src-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "code" :face 'dired-directory)
                   atis/padding-char-right)
     :extensions ("src-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-down" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "desktop" :face 'dired-directory)
                   atis/padding-char-right)
     :extensions ("desktop-open" "바탕화면-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "desktop" :face 'dired-directory)
                   atis/padding-char-right)
     :extensions ("desktop-closed" "바탕화면-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-down" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "download" :face 'dired-directory)
                   atis/padding-char-right)
     :extensions ("download-open" "downloads-open" "다운로드-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "download" :face 'dired-directory)
                   atis/padding-char-right)
     :extensions ("download-closed" "downloads-closed" "다운로드-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-down" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "movies" :face 'dired-directory)
                   atis/padding-char-right)
     :extensions ("movies-open" "비디오-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "movies" :face 'dired-directory)
                   atis/padding-char-right)
     :extensions ("movies-closed" "비디오-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-down" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "music" :face 'dired-directory)
                   atis/padding-char-right)
     :extensions ("music-open" "음악-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "music" :face 'dired-directory)
                   atis/padding-char-right)
     :extensions ("music-closed" "음악-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-down" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "photos" :face 'dired-directory)
                   atis/padding-char-right)
     :extensions ("photos-open" "사진-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "photos" :face 'dired-directory)
                   atis/padding-char-right)
     :extensions ("photos-closed" "사진-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-down" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "pictures" :face 'dired-directory)
                   atis/padding-char-right)
     :extensions ("pictures-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "pictures" :face 'dired-directory)
                   atis/padding-char-right)
     :extensions ("pictures-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-down" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "workspace" :face 'dired-directory)
                   atis/padding-char-right)
     :extensions ("workspace-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "workspace" :face 'dired-directory)
                   atis/padding-char-right)
     :extensions ("workspace-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-down" :face 'dired-directory)
                   (all-the-icons-icon-for-dir ".git" :face 'dired-directory)
                   atis/padding-char-right)
     :extensions ("git-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'dired-directory)
                   (all-the-icons-icon-for-dir ".git" :face 'dired-directory)
                   atis/padding-char-right)
     :extensions ("git-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-down" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "trash" :face 'dired-directory)
                   atis/padding-char-right)
     :extensions ("trash-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "trash" :face 'dired-directory)
                   atis/padding-char-right)
     :extensions ("trash-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-down" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "dropbox" :face 'dired-directory)
                   atis/padding-char-right)
     :extensions ("dropbox-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "dropbox" :face 'dired-directory)
                   atis/padding-char-right)
     :extensions ("dropbox-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-down" :face 'dired-directory)
                   (all-the-icons-devopicons "onedrive" :face 'dired-directory)
                   atis/padding-char-right)
     :extensions ("onedrive-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'dired-directory)
                   (all-the-icons-devopicons "onedrive" :face 'dired-directory)
                   atis/padding-char-right)
     :extensions ("onedrive-closed")
     :fallback 'same-as-icon)

    (treemacs-create-icon
     :icon (format "%s%s"
                   (all-the-icons-octicons "repo" :face 'dired-directory)
                   atis/padding-char-right)
     :extensions (root-open root-closed)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-down" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "dirname" :face 'dired-directory)
                   atis/padding-char-right)
     :extensions (dir-open)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'dired-directory)
                   (all-the-icons-icon-for-dir "dirname" :face 'dired-directory)
                   atis/padding-char-right)
     :extensions (dir-closed)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   atis/padding-char-left
                   (all-the-icons-icon-for-file "filename")
                   atis/padding-char-right)
     :extensions (fallback)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-down" :face 'dired-directory)
                   (all-the-icons-octicons "package" :face 'dired-directory)
                   atis/padding-char-right)
     :extensions (tag-open)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   (all-the-icons-octicons "chevron-right" :face 'dired-directory)
                   (all-the-icons-octicons "package" :face 'dired-directory)
                   atis/padding-char-right)
     :extensions (tag-closed)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   atis/padding-char-left
                   (all-the-icons-octicons "tag")
                   atis/padding-char-right)
     :extensions (tag-leaf)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   atis/padding-char-left
                   (all-the-icons-octicons "flame" :face 'all-the-icons-red)
                   atis/padding-char-right)
     :extensions (error)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   atis/padding-char-left
                   (all-the-icons-octicons "stop" :face 'all-the-icons-yellow)
                   atis/padding-char-right)
     :extensions (warning)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s"
                   atis/padding-char-left
                   (all-the-icons-octicons "info" :face 'all-the-icons-blue)
                   atis/padding-char-right)
     :extensions (info)
     :fallback 'same-as-icon)))

(provide 'treemacs-all-the-icons-svg)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; treemacs-all-the-icons-svg.el ends here
