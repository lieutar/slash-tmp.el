;; -*- lexical-binding: t -*-
;;; batch.el --- Setups package environment for batch process

;;; Code:
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(use-package radish
  :ensure nil
  :load-path "~/work/emacs/radish.el")
