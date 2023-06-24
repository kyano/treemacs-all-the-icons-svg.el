;;; treemacs-all-the-icons-svg.el --- all-the-icons integration for treemacs -*- lexical-binding: t -*-

;; Copyright (C) 2023 Anho Ki

;; Author: Anho Ki
;; Maintainer: Anho Ki
;; URL: https://github.com/kyano/treemacs-all-the-icons-svg.el
;; Version: 0.0.1
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

(require 'all-the-icons)
(require 'treemacs)
(require 'modus-themes)

;; FIXME: To get proper align, it draws useless hidden `chevron-right' characters.

;; FIXME: Hard-coded color value!
(defface hidden-face
  '((t :foreground "#fffcff"))
  "Face used for the dummy spaces.")

(treemacs-create-theme "all-the-icons-svg"
  :config
  (progn
    (dolist (item all-the-icons-extension-icon-alist)
      (let ((extension (nth 0 item))
            (extensions (list (nth 0 item))))
        (treemacs-create-icon
         :icon (format "%s%s "
                       (all-the-icons-octicons "chevron-right" :face 'hidden-face)
                       (all-the-icons-icon-for-file (format "filename.%s" extension)))
         :extensions extensions
         :fallback 'same-as-icon)))

    (treemacs-create-icon
     :icon (format "%s%s "
                   (all-the-icons-octicons "chevron-right" :face 'hidden-face)
                   (all-the-icons-icon-for-file "LICENSE"))
     :extensions ("license" "copying")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s "
                   (all-the-icons-octicons "chevron-right" :face 'hidden-face)
                   (all-the-icons-icon-for-file "readme"))
     :extensions ("readme" "readme.org" "readme.md" "readme.rst" "readme.txt")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s "
                   (all-the-icons-octicons "chevron-right" :face 'hidden-face)
                   (all-the-icons-icon-for-file "Makefile"))
     :extensions ("makefile")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s "
                   (all-the-icons-octicons "chevron-right" :face 'hidden-face)
                   (all-the-icons-icon-for-file "go.mod"))
     :extensions ("go.mod" "go.sum")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s "
                   (all-the-icons-octicons "chevron-right" :face 'hidden-face)
                   (all-the-icons-icon-for-file "package.json"))
     :extensions ("package.json")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s "
                   (all-the-icons-octicons "chevron-right" :face 'hidden-face)
                   (all-the-icons-icon-for-file "filename.sh"))
     :extensions ("bash" "csh")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s "
                   (all-the-icons-octicons "chevron-right" :face 'hidden-face)
                   (all-the-icons-devopicons "vim"))
     :extensions ("vimrc")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s "
                   (all-the-icons-octicons "chevron-right" :face 'hidden-face)
                   (all-the-icons-devopicons "git"))
     :extensions ("gitignore" "gitconfig")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s "
                   (all-the-icons-octicons "chevron-right" :face 'hidden-face)
                   (all-the-icons-icon-for-file "filename.key"))
     :extensions ("netrc")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s "
                   (all-the-icons-octicons "chevron-down")
                   (all-the-icons-icon-for-dir "documents"))
     :extensions ("docs-open" "documents-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s "
                   (all-the-icons-octicons "chevron-right")
                   (all-the-icons-icon-for-dir "documents"))
     :extensions ("docs-closed" "documents-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s "
                   (all-the-icons-octicons "chevron-down")
                   (all-the-icons-icon-for-dir "code"))
     :extensions ("src-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s "
                   (all-the-icons-octicons "chevron-right")
                   (all-the-icons-icon-for-dir "code"))
     :extensions ("src-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s "
                   (all-the-icons-octicons "chevron-down")
                   (all-the-icons-icon-for-dir "desktop"))
     :extensions ("desktop-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s "
                   (all-the-icons-octicons "chevron-right")
                   (all-the-icons-icon-for-dir "desktop"))
     :extensions ("desktop-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s "
                   (all-the-icons-octicons "chevron-down")
                   (all-the-icons-icon-for-dir "download"))
     :extensions ("download-open" "downloads-open" "다운로드-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s "
                   (all-the-icons-octicons "chevron-right")
                   (all-the-icons-icon-for-dir "download"))
     :extensions ("download-closed" "downloads-closed" "다운로드-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s "
                   (all-the-icons-octicons "chevron-down")
                   (all-the-icons-icon-for-dir "movies"))
     :extensions ("movies-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s "
                   (all-the-icons-octicons "chevron-right")
                   (all-the-icons-icon-for-dir "movies"))
     :extensions ("movies-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s "
                   (all-the-icons-octicons "chevron-down")
                   (all-the-icons-icon-for-dir "music"))
     :extensions ("music-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s "
                   (all-the-icons-octicons "chevron-right")
                   (all-the-icons-icon-for-dir "music"))
     :extensions ("music-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s "
                   (all-the-icons-octicons "chevron-down")
                   (all-the-icons-icon-for-dir "photos"))
     :extensions ("photos-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s "
                   (all-the-icons-octicons "chevron-right")
                   (all-the-icons-icon-for-dir "photos"))
     :extensions ("photos-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s "
                   (all-the-icons-octicons "chevron-down")
                   (all-the-icons-icon-for-dir "pictures"))
     :extensions ("pictures-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s "
                   (all-the-icons-octicons "chevron-right")
                   (all-the-icons-icon-for-dir "pictures"))
     :extensions ("pictures-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s "
                   (all-the-icons-octicons "chevron-down")
                   (all-the-icons-icon-for-dir "workspace"))
     :extensions ("workspace-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s "
                   (all-the-icons-octicons "chevron-right")
                   (all-the-icons-icon-for-dir "workspace"))
     :extensions ("workspace-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s "
                   (all-the-icons-octicons "chevron-down")
                   (all-the-icons-icon-for-dir ".git"))
     :extensions ("git-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s "
                   (all-the-icons-octicons "chevron-right")
                   (all-the-icons-icon-for-dir ".git"))
     :extensions ("git-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s "
                   (all-the-icons-octicons "chevron-down")
                   (all-the-icons-icon-for-dir "trash"))
     :extensions ("trash-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s "
                   (all-the-icons-octicons "chevron-right")
                   (all-the-icons-icon-for-dir "trash"))
     :extensions ("trash-closed")
     :fallback 'same-as-icon)

    (treemacs-create-icon
     :icon (format "%s " (all-the-icons-octicons "repo"))
     :extensions (root-open root-closed)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s "
                   (all-the-icons-octicons "chevron-down")
                   (all-the-icons-icon-for-dir "dirname"))
     :extensions (dir-open)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s "
                   (all-the-icons-octicons "chevron-right")
                   (all-the-icons-icon-for-dir "dirname"))
     :extensions (dir-closed)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s "
                   (all-the-icons-octicons "chevron-right" :face 'hidden-face)
                   (all-the-icons-icon-for-file "filename"))
     :extensions (fallback)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s "
                   (all-the-icons-octicons "chevron-down")
                   (all-the-icons-octicons "package"))
     :extensions (tag-open)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s "
                   (all-the-icons-octicons "chevron-right")
                   (all-the-icons-octicons "package"))
     :extensions (tag-closed)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s "
                   (all-the-icons-octicons "chevron-right" :face 'hidden-face)
                   (all-the-icons-octicons "tag"))
     :extensions (tag-leaf)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s "
                   (all-the-icons-octicons "chevron-right" :face 'hidden-face)
                   (all-the-icons-octicons "flame" :face 'all-the-icons-red))
     :extensions (error)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s "
                   (all-the-icons-octicons "chevron-right" :face 'hidden-face)
                   (all-the-icons-octicons "stop" :face 'all-the-icons-yellow))
     :extensions (warning)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s "
                   (all-the-icons-octicons "chevron-right" :face 'hidden-face)
                   (all-the-icons-octicons "info" :face 'all-the-icons-blue))
     :extensions (info)
     :fallback 'same-as-icon)))

(provide 'treemacs-all-the-icons-svg)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; treemacs-all-the-icons-svg.el ends here
