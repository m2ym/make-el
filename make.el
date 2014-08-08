(require 'cl-lib)

(defvar make--cores nil)

(defun make--cores ()
  (or make--cores
      (let ((cores (cond ((and (eq system-type 'gnu/linux)
                               (file-exists-p "/proc/cpuinfo"))
                          (shell-command-to-string
                           "awk '/^cpu cores/ { print $4; exit }' /proc/cpuinfo"))
                         ((and (eq system-type 'darwin)
                               (file-executable-p "/usr/sbin/sysctl"))
                          (shell-command-to-string
                           "/usr/sbin/sysctl hw.physicalcpu | awk '{ print $2 }'"))
                         (t "1"))))
        (setq make--cores (max 1 (string-to-number cores))))))

(defun make--canonical-filename (filename)
  (expand-file-name filename))

(defun make--canonical-dirname (dirname)
  (file-name-as-directory (expand-file-name dirname)))

(defun make--directory-basename (dirname)
  (file-name-nondirectory
   (directory-file-name
    (make--canonical-dirname dirname))))

(defun make--directory-dirname (dirname)
  (file-name-directory
   (directory-file-name
    (make--canonical-dirname dirname))))

(defun make--directory-parents (filename)
  (cl-loop for current-dirname = (make--canonical-filename filename) then parent-dirname
           for parent-dirname = (make--directory-dirname current-dirname)
           while (and parent-dirname (< (length parent-dirname) (length current-dirname)))
           collect parent-dirname))

(defun make-locate-makefile (filename)
  (cl-loop for parent-dirname in (if (file-directory-p filename)
                                     (cons filename
                                           (make--directory-parents filename))
                                   (make--directory-parents filename))
           for makefile = (expand-file-name "Makefile" parent-dirname)
           if (file-exists-p makefile)
           return makefile))

(defun make-read-db (makefile)
  (with-temp-buffer
    (let ((default-directory (file-name-directory makefile)))
      (shell-command "make -q -p -n | grep -v '^#' | grep -v '='" (current-buffer)))
    (goto-char (point-min))
    (let ((db (make-hash-table :test 'equal)))
      (while (re-search-forward "^\\(.+?\\) *: *\\(.+?\\)$" nil t)
        (let ((target (match-string 1))
              (prerequisites (split-string (match-string 2) " ")))
          (dolist (prerequisite prerequisites)
            (unless (gethash prerequisite db)
              (puthash prerequisite target db)))))
      db)))

(defgroup make nil
  "GNU make frontend."
  :group 'convenience
  :prefix "make-")

(defcustom make-program "make"
  "The name of make program."
  :type 'string
  :group 'make)

(defcustom make-jobs 'cores
  "The number of jobs make runs simultaneously.  `cores' implies
the number of cores."
  :type '(or number
             (const :tag "The number of cores" cores))
  :group 'make)

(defvar make-makefile nil)

(defvar make-directory nil)

(cl-defun make-command (&key target)
  (format "%s -k -j%d %s"
          make-program
          (make--cores)
          (or target "")))

(cl-defun make-run-make (&key target)
  (cl-assert make-makefile)
  (cl-assert make-directory)
  (let ((default-directory make-directory)
        (command (make-command :target target)))
    (compile command)))

(defvar make-db nil)

(defun make-lookup-target-for-prerequisite (prerequisite)
  (cl-assert make-db)
  (gethash prerequisite make-db))

(defun make-lookup-prerequisite-for-file (filename)
  (cl-assert make-directory)
  (when (and filename make-directory)
    (setq filename (make--canonical-filename filename))
    (when (and (file-exists-p filename)
               (string-prefix-p make-directory filename))
      (substring filename (length make-directory)))))

(defun make-for-file (&optional filename)
  (interactive)
  (setq filename (or filename (buffer-file-name)))
  (unless filename
    (error "Invalid file name"))
  (let ((makefile (make-locate-makefile filename)))
    (unless makefile
      (error "Makefile not found"))
    (unless (equal make-makefile makefile)
      (setq make-makefile makefile)
      (setq make-directory (file-name-directory makefile))
      (setq make-db nil)))
  (unless make-db
    (setq make-db (make-read-db make-makefile)))
  (let ((prerequisite (make-lookup-prerequisite-for-file filename)))
    (unless prerequisite
      (error "No prerequisite for %s" filename))
    (let ((target (make-lookup-target-for-prerequisite prerequisite)))
      (unless target
        (error "No target for %s" prerequisite))
      (make-run-make :target target))))

(defun make-after-save ()
  (let ((filename (buffer-file-name)))
    (when (and filename
               make-directory
               (string-prefix-p make-directory filename))
      (ignore-errors
        (with-no-warnings
          (let ((compilation-auto-jump-to-first-error nil)
                (compilation-always-kill t)
                (compilation-ask-about-save nil)
                ;; TODO use display-buffer-alist
                (display-buffer-function (lambda (&rest ignore) nil)))
            (make-for-file filename)))))))

(defun make-forget ()
  (interactive)
  (setq make-makefile nil)
  (setq make-directory nil)
  (setq make-db nil))

(defun make-target (target)
  (interactive "s")
  (let ((makefile (make-locate-makefile default-directory)))
    (unless makefile
      (error "Makefile not found"))
    (unless (equal make-makefile makefile)
      (setq make-makefile makefile)
      (setq make-directory (file-name-directory makefile))
      (setq make-db nil))
    (make-run-make :target target)))

(defun make ()
  (interactive)
  (make-target nil))

(provide 'make)
