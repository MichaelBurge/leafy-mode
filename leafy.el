(define-minor-mode leafy-mode
  "Leafy mode"
  :init-value nil
  :lighter " Leafy"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") 'request-completion-at-point)
	    (define-key map (kbd "C-c c") 'leafy-log-context)
            map))

(require 'org)
(require 'org-element)
(require 'leafy--utils)
(require 'leafy--org-helpers)
;;(require 'leafy--tools)
(require 'cl-lib)
(require 'python)

(defvar leafy-mode-hook nil  "Hook for enabling Leafy mode.")
;; (defvar mode-line-format-leafy nil "Mode line format for leafy-mode.")
(defvar leafy-record-token-statistics t "Whether to record token statistics in a meta property drawer")
(defvar leafy-chatgpt-context-size 4096 "Context size tokens, including input and output")
(defvar leafy-chatgpt-output-size-reservation 1024 "In a context which shares prompt and output, how many tokens to reserve for output?")
(defvar leafy-priority-key "CONTEXT-PRIORITY" "Property name used to prioritize which sections are dropped first when context fills up")
(defvar leafy-token-counting-method 'regex "One of '(exact regex) used to decide how to count tokens")
(defvar leafy-chatgpt-context-buffer "*ChatGPT-Context*" "Name of the temporary buffer used by leafy-dump-context")
(defvar leafy-current-model "GPT3.5" "Name of the model currently being used to make API requests")
(defvar leafy-meta-drawer "meta" "Holds global statistics like token counts, etc.")
(defvar leafy-timestamp-format "%F %T")

(defun leafy-mode-line ()
  "Generate the Leafy mode-line string."
  (let* ((current-model leafy-current-model) ;; Retrieve the current model here
         (project-cost-alist (leafy-project-cost))
	 (project-cost (alist-get 'total-cost project-cost-alist))
	 )
    (propertize
     (format " Leafy: %s | Cost: $%.2f" current-model project-cost)
     'local-map (let ((map (make-sparse-keymap)))
                  (define-key map [mode-line mouse-1] 'leafy-select-model)
                  map)
     'mouse-face 'mode-line-highlight
     'help-echo "mouse-1: Select model")))

(defun leafy-select-model ()
  "Display a list of available models in a dropdown menu and allow the user to select one."
  (interactive)
  (let* ((model-names (mapcar 'car leafy-model-info-alist))
	 (menu-items (mapcar (lambda (model) (cons model model)) model-names))
         (menu (list "Select a model" (cons "keymap" menu-items)))
         (selected-model (x-popup-menu t menu)))
    (when selected-model
      (message "Selected model: %s" selected-model)
      (leafy-set-current-model selected-model)
      (force-mode-line-update)
      )))

(defun leafy-mode-line-exists-p ()
  "Check if the Leafy mode-line display is already present."
  (seq-find (lambda (x) (and (listp x) (eq (car x) :eval) (equal (cadr x) '(leafy-mode-line))))
            mode-line-format))

(defun leafy-enable-mode-line ()
  "Enables the leafy-mode model-line that allows easily switching between models"
  (interactive)
  (unless (leafy-mode-line-exists-p)
    (setq mode-line-format (append mode-line-format (list '(:eval (leafy-mode-line)))))
    (force-mode-line-update)))

(defun leafy-remove-mode-line ()
  "Remove the Leafy mode-line display from the current buffer."
  (interactive)
  (setq mode-line-format (remove '(:eval (leafy-mode-line)) mode-line-format))
  (force-mode-line-update))


;;(load-file (expand-file-name "./org-helpers.el"));; (file-name-directory buffer-file-name)))

;; Load the python.el library

;; Start a Python shell for leafy
;; (defvar leafy-python-shell (python-shell-get-or-create-process))

(defun leafy-set-current-model (model-name)
  "Set the current model to MODEL-NAME."
  (setq leafy-current-model model-name)
  (setq leafy-chatgpt-context-size (leafy-model-context-size model-name)))


(defun leafy-max-prompt-size () (- leafy-chatgpt-context-size leafy-chatgpt-output-size-reservation))

(defvar leafy-model-info-alist
  `(("GPT3.5" . ((model-name . "gpt-3.5-turbo") 
		 (context-size . 4096)
		 (price . ,(/ 0.002 1000)) ;; $0.002 / 1K tokens
		 ))
    ("GPT4" . ((model-name . "gpt-4")
	       (context-size . 8096)
	       (input-price . ,(/ 0.03 1000))
	       (output-price . ,(/ 0.06 1000))
	       ))
    ;; Add more models here
    ))

(defun leafy-get-model-info (model-name)
  "Get the model information alist for the given MODEL-NAME."
  (alist-get model-name leafy-model-info-alist nil nil #'equal))

(defun leafy-model-openai-id (model-name)
  "Get the OpenAI model id for the human-readable name MODEL-NAME"
  (alist-get 'model-name (leafy-get-model-info model-name)))

(defun leafy-model-context-size (model-name)
  "Get the context size for the given MODEL-NAME."
  (alist-get 'context-size (leafy-get-model-info model-name)))

(defun leafy-model-price (model-name)
  "Get the (input-price . output-price) for the given MODEL-NAME."
  (let ((price (alist-get 'price (leafy-get-model-info model-name)))
	(input-price (alist-get 'input-price (leafy-get-model-info model-name)))
	(output-price (alist-get 'output-price (leafy-get-model-info model-name)))
	)
    (cons (or input-price price) (or output-price price))))
	 

(defun leafy-model-supports-images (model-name)
  "Check if the given MODEL-NAME supports images."
  (alist-get 'supports-images (leafy-get-model-info model-name)))

(defun start-python-process ()
  "Starts a new Python process if one doesn't exist and sets `leafy-python-shell` variable."
  (when (process-live-p leafy-python-shell)
    (kill-process leafy-python-shell))
  (unless (process-live-p leafy-python-shell)
    (setq leafy-python-shell (run-python))
    ;; Disable native Python completion for the leafy-python-shell process
    (set-process-query-on-exit-flag leafy-python-shell nil)
    ;; (set-process-filter leafy-python-shell (symbol-value 'python-shell-filter))
    ;; (set-process-filter leafy-python-shell 'python-shell-filter)
    (with-current-buffer (process-buffer leafy-python-shell)
      (setq-local python-shell-completion-native-enable nil))))

(defun start-python-process ()
  "Starts a new Python process if one doesn't exist and sets `my-python-shell` variable."
  (unless (process-live-p leafy-python-shell)
    (setq leafy-python-shell (run-python))))

(defun run-python-command (command)
  "Run a Python command and return its output as a string."
  (shell-command-to-string (concat "python3 -c " (shell-quote-argument command))))

(defvar run-python-command-cache (make-hash-table :test 'equal :size 1000)
  "Hash table to store the last 1000 command results.")

(defun run-python-command-pure (command)
  "Run a Python command and return its output as a string.
Memoize the result and store the last 1000 command results."
  (let ((cached-result (gethash command run-python-command-cache)))
    (if cached-result
        cached-result
      (let ((result (run-python-command command)))
        (puthash command result run-python-command-cache)
        result))))

(ert-deftest test-run-python-command-pure ()
  (should (equal (run-python-command-pure "print(1 + 2)") "3\n"))
  (should (equal (gethash "print(1 + 2)" run-python-command-cache) "3\n")))

;; On Mac OSX, they use a BSD readline instead of GNU readline
;; So, (python-shell-internal-send-string) will return the input too, separated by ^M
;; So, use the RESULT: indicator to tag the result and regex for it.
;; Command should be "pure" - side-effect free
(defun eval-python-integer (python-cmd)
  (let* (;;(res (python-shell-internal-send-string python-cmd))
	 (res (run-python-command-pure python-cmd))
         (result-regexp "RESULT:\\s-*\\([[:digit:]]+\\)")
         (val (if (string-match result-regexp res)
		  (string-to-number (match-string 1 res))
		(user-error "eval-python-integer: Unable to find result in string: %S" res)
		)))
    val))

;;
(defun python-quote-argument (string)
  (replace-regexp-in-string "['\n]" (lambda (match) (if (string= match "'") "\\\\'" "\\\\n")) (or string "")))

(ert-deftest test-python-quote-argument ()
  (should (equal (python-quote-argument nil) ""))
  )

;; Define a function to count the number of tokens in a string
(defun leafy-count-tokens (string)
  "Count the number of tokens in a string using tiktoken."
  (let ((cmd (concat "import tiktoken; enc = tiktoken.get_encoding('gpt2'); print('RESULT:',len(enc.encode('" (python-quote-argument string) "')));\n")))
    (let ((result (eval-python-integer cmd)))
      (when (> result 10000)
	(user-error "ERROR: Tokens too large! Tokens:%d string:<%s> cmd:<%s>" result string cmd))
      result)))

(ert-deftest test-leafy-count-tokens ()
  (should (equal (leafy-count-tokens "the quick brown fox") 4))
  (should (equal (leafy-count-tokens "a b c d e") 5))
  (should (equal (leafy-count-tokens "") 0))
  (should (equal (leafy-count-tokens "Section 1\nText for section 1.") 8))
  (should (equal (leafy-count-tokens "(Leafy Metadata)\n") 8))
  (should (equal (leafy-count-tokens "(Leafy Metadata)\nnil") 9))
  ;;(should (equal (leafy-count-tokens nil) 0))
  )

(defun leafy-estimate-tokens (string)
  (pcase leafy-token-counting-method
    ('exact (leafy-count-tokens string))
    ('regex (leafy-estimate-tokens-regex string))
    (_ (user-error "Unknown token counting method: '" leafy-token-counting-method))))

(defun leafy-estimate-tokens-regex (string)
  (let ((pos 0)
        (count 0)
        (str-len (length string)))
    (while (< pos str-len)
      (let ((char (aref string pos)))
        (cond
         ;; Rule 1: Every punctuation is a separate token.
         ((string-match-p "[:punct:]" (char-to-string char))
          (setq count (1+ count))
          (setq pos (1+ pos)))
         ;; Rule 2: Every 4 letters is a separate token.
         ((string-match-p "[a-zA-Z]" (char-to-string char))
          (setq count (1+ count))
          (setq pos (+ pos 4))
          (when (> pos str-len)
            (setq pos str-len)))
         ;; Rule 3: Whitespace is a token.
         ((string-match-p "\\s-" (char-to-string char))
          (setq count (1+ count))
          (setq pos (1+ pos)))
         ;; Rule 4: Every 2 numbers is a token.
         ((string-match-p "[0-9]" (char-to-string char))
          (setq count (1+ count))
          (setq pos (+ pos 2))
          (when (> pos str-len)
            (setq pos str-len)))
         (t
          (setq pos (1+ pos))))))
    count))

(defun leafy-estimate-tokens-regex (string)
  (let ((pos 0)
        (count 0)
        (str-len (length string))
        (letter-group 0))
    (while (< pos str-len)
      (let ((char (aref string pos)))
        (cond
         ;; Rule 1: Every punctuation is a separate token.
         ((string-match-p "[:punct:]" (char-to-string char))
          (setq count (1+ count))
          (setq pos (1+ pos))
          (setq letter-group 0))
         ;; Rule 2: Every 4 letters is a separate token.
         ((string-match-p "[a-zA-Z]" (char-to-string char))
          (setq letter-group (1+ letter-group))
          (when (= letter-group 4)
            (setq count (1+ count))
            (setq letter-group 0))
          (setq pos (1+ pos)))
         ;; Rule 3: Whitespace is a token.
         ((string-match-p "\\s-" (char-to-string char))
          (setq count (1+ count))
          (setq pos (1+ pos))
          (setq letter-group 0))
         ;; Rule 4: Every 2 numbers is a token.
         ((string-match-p "[0-9]" (char-to-string char))
          (setq count (1+ count))
          (setq pos (+ pos 2))
          (setq letter-group 0)
          (when (> pos str-len)
            (setq pos str-len)))
         (t
          (setq pos (1+ pos))
          (setq letter-group 0)))))
    (when (> letter-group 0)
      (setq count (1+ count)))
    count))

(defun leafy-get-sections ()
  "Extracts each section in the buffer as a (level, property alist, tag, title, content) tuple."
  (let* ((sections '())
	 (whole-org-tree (org-element-parse-buffer))
	 )
    (org-element-map whole-org-tree 'headline
      (lambda (headline)
        (let* ((title (substring-no-properties (car (org-element-property :title headline))))
               (level (org-element-property :level headline))
	       (tags (org-element-property :tags headline))
	       ;; Sections can be marked "assistant", "system", "user", or empty(defaults to "user")
               (tag (if (member "assistant" tags)
                        "assistant"
                      (if (member "system" tags)
                          "system"
                        "user")))
               (properties (leafy-headline-to-alist headline))
               (section (org-element-map (org-element-contents headline) 'section 'identity nil t 'headline))
	       (content (or (when section
                              (leafy-section-body section))
                            ""))
	       )
	  (unless (org-element-has-inherited-tag headline "ignore")
            (push (list level properties tag title (format "%s" content)) sections)))))
    (reverse sections)))

(defun leafy-make-chatgpt-entry (tag message)
  (list tag message))

(defun leafy-section-to-chatgpt (section)
  "Returns a (tag, title + content) for a section"
  (pcase section
    (`(,_level ,_properties ,tag ,title ,content)
     (leafy-make-chatgpt-entry tag (leafy-join (list title content) "\n")))))

(defun leafy-sections-to-chatgpt (sections)
  "Returns a list of (tag, title + content) for a list of sections."
  (mapcar #'leafy-section-to-chatgpt sections))

(defun leafy-dump-context ()
  "Fetches the context at point that ChatGPT would receive, and sends it to a temporary buffer. Useful for the web UI"
  (interactive)
  (let* ((context (leafy-get-context))
	 (text (mapconcat (lambda (x) (nth 1 x)) context "\n"))
	 )
    (with-output-to-temp-buffer leafy-chatgpt-context-buffer
      (princ text)
      (pop-to-buffer leafy-chatgpt-context-buffer))))

;;(let ((process-name "Python"))
;;  (when (get-process process-name)
;;    (delete-process (get-process process-name))))

(defvar chatgpt-buffer
  (get-buffer-create "*ChatGPT*")
  "The buffer to log messages from the `do-chatgpt-request` function.")

(defun leafy-validate-chatgpt-response (response)
  (when (eq (caar response) 'error)
      (user-error "ChatGPT API Error: %S" (alist-get 'message (car response)))))

(defun leafy-extract-chatgpt-usage-statistics (response)
  (let* ((usage (alist-get 'usage response)))
    (unless (null usage)
      (let* ((input-tokens (alist-get 'prompt_tokens usage))
	     (output-tokens (alist-get 'completion_tokens usage))
	     (billed-tokens (alist-get 'total_tokens usage))
	     )
	`((:input-tokens . ,input-tokens)
	  (:output-tokens . ,output-tokens)
	  (:billed-tokens . ,billed-tokens))))))

;; (tick-meta-counter "input-tokens" (alist-get :input-tokens '((:input-tokens . 2924) (:output-tokens . 28) (:billed-tokens . 2952))))
(defun leafy-tick-token-counters (response extras)
  (when leafy-record-token-statistics
    (let ((statistics (leafy-extract-chatgpt-usage-statistics response)))
      (unless (null statistics)
	(let ((input-tokens (alist-get :input-tokens statistics))
	      (output-tokens (alist-get :output-tokens statistics))
	      (billed-tokens (alist-get :billed-tokens statistics))
	      (model-name leafy-current-model)
	      (elapsed-time (float-time (alist-get :elapsed-time extras)))
	      )
	  (tick-meta-model-counter model-name "input-tokens" input-tokens)
	  (tick-meta-model-counter model-name "output-tokens" output-tokens)
	  (tick-meta-model-counter model-name "billed-tokens" billed-tokens)
	  (tick-meta-model-counter model-name "num-invocations" 1)
	  (tick-meta-model-counter model-name "elapsed-time" elapsed-time)
	  )))))

(defun request-completion-at-point-synchronous ()
  "Send the current node and main text of every node up to the top-level to ChatGPT for completion."
  (interactive)
  (let* ((context (leafy-get-context))
	 (placeholder (make-marker))
	 )
    (leafy-insert-placeholder)
    (let* ((response (leafy-do-chatgpt-request chatgpt-buffer context))
	   (api-response (alist-get 'api-response response))
	   (start-time (format-time-string leafy-timestamp-format (alist-get :start-time response)))
	   (elapsed-time (float-time (alist-get :elapsed-time response)))
	   (statistics `((estimated-tokens . ,(apply #'+ (mapcar #'leafy-context-entry-tokens context)))
			 (:start-time . ,start-time)
			 (:elapsed-time . ,elapsed-time)))
	   )
      
      (leafy-validate-chatgpt-response api-response)
      (leafy-tick-token-counters api-response statistics)
      (leafy-insert-chatgpt-response-after "ChatGPT response" api-response statistics))))
  
(defun leafy-do-chatgpt-request-synchronous (chatgpt-buffer nodes)
  "Send the given list of nodes to the OpenAI Chat API and return a list of completions.
Messages are logged to the `chatgpt-buffer`."
  (let* ((messages (mapcar (lambda (node) `((role . ,(or (nth 0 node) "user"))
					    (content . ,(nth 1 node))))
			   nodes))
	 (model-id (leafy-model-openai-id leafy-current-model))
         (payload `((model . ,model-id)
                    (messages . ,messages)
                    (n . 1)
                    ;;(stop . [".", "!", "?"])
                    (max_tokens . ,leafy-chatgpt-output-size-reservation)))
         (url "https://api.openai.com/v1/chat/completions")
         (headers `(("Content-Type" . "application/json")
                    ("Authorization" . ,(concat "Bearer " leafy-api-key))))
         (url-request-method "POST")
         (url-request-extra-headers headers)
         (url-request-data (json-encode payload))
	 )
    ;; Send the request to the OpenAI API
    (with-current-buffer (url-retrieve-synchronously url t t 5)
      ;; Parse the response and return the completions
      (goto-char (point-min))
      (re-search-forward "^$")
      (let ((result (json-read)))
        ;; Write the response to the chatgpt-buffer	
	(princ (format "Request: %S\n" payload) chatgpt-buffer)
	(princ (format "Response: %S\n" result) chatgpt-buffer)
	`((:api-response . ,result)
	  (:start-time . ,start-time)
	  (:elapsed-time . ,elapsed-time)
	  )))))

(defun leafy-do-chatgpt-request (chatgpt-buffer context cbargs callback)
  "Asynchronously send the given list of context nodes to the OpenAI Chat API and return a list of completions.
Messages are logged to the `chatgpt-buffer`. Calls `callback` with the API response."
  (let* ((messages (mapcar (lambda (node) `((role . ,(or (nth 0 node) "user"))
                                            (content . ,(nth 1 node))))
                            context))
         (model-id (leafy-model-openai-id leafy-current-model))
         (payload `((model . ,model-id)
                    (messages . ,messages)
                    (n . 1)
                    ;;(stop . [".", "!", "?"])
                    (max_tokens . ,leafy-chatgpt-output-size-reservation)))
         (url "https://api.openai.com/v1/chat/completions")
         (headers `(("Content-Type" . "application/json")
                    ("Authorization" . ,(concat "Bearer " leafy-api-key))))
         (url-request-method "POST")
         (url-request-extra-headers headers)
         (url-request-data (json-encode payload))
	 (start-time (current-time))
         )
    ;; Send the request to the OpenAI API
    (url-retrieve url (lambda (status chatgpt-buffer payload callback context cbargs start-time)
                        ;; Parse the response and return the completions
                        (goto-char (point-min))
                        (re-search-forward "^$")
                        (let* ((result (json-read))
			       (elapsed-time (time-subtract (current-time) start-time))
			       (extras `((:start-time . ,start-time)
					 (:elapsed-time . ,elapsed-time)))
			       )
                          ;; Write the response to the chatgpt-buffer
                          (with-current-buffer chatgpt-buffer
                            (princ (format "Request: %S\n" payload) chatgpt-buffer)
                            (princ (format "Response: %S\n" result) chatgpt-buffer))
                          (funcall callback result context cbargs extras)))
                  (list chatgpt-buffer payload callback context cbargs start-time))))

(defun request-completion-at-point ()
  "Asynchronously send the current node and main text of every node up to the top-level to ChatGPT for completion."
  (interactive)
  (let* ((context (leafy-get-context))
         (placeholder (make-marker)))
    (set-marker placeholder (point))
    (leafy-insert-placeholder)
    (leafy-do-chatgpt-request
     chatgpt-buffer
     context
     (list placeholder (current-buffer))
     (lambda (response context cbargs extras)
       (pcase cbargs
	 (`(,placeholder ,org-buffer)
	  (with-current-buffer org-buffer
	    (save-excursion
	      (leafy-validate-chatgpt-response response)
	      (leafy-tick-token-counters response extras)
	      (let* ((statistics `((estimated-tokens . ,(apply #'+ (mapcar #'leafy-context-entry-tokens context)))
				   (:start-time . ,(format-time-string leafy-timestamp-format (alist-get :start-time extras)))
				   (:elapsed-time . ,(float-time (alist-get :elapsed-time extras)))))
		     (insertion-point (marker-position placeholder))
		     )
		(when (null insertion-point)
		  (message "ERROR"))
		(leafy-update-placeholder placeholder "")
		(goto-char insertion-point)
		;; (message "@@@@@ placeholder:%S insertion-point:%S point:%S" placeholder insertion-point (point))
		(leafy-insert-chatgpt-response-after "ChatGPT response" response statistics)
		(set-marker placeholder nil))))))))))

(defun leafy-insert-placeholder ()
  "Insert a placeholder at the current position."
  (insert "WAITING-ON-REQUEST"))

(defun leafy-update-placeholder (placeholder response)
  "Update the placeholder with the received response."
  (save-excursion
    (goto-char (marker-position placeholder))
    (delete-region (point) (progn (forward-sexp 2) (point)))
    (insert response)))

(defun leafy-chatgpt-callback (status placeholder)
  "Callback function for the ChatGPT API request."
  (search-forward "\n\n")
  (let ((response (buffer-substring (point) (point-max))))
    (leafy-update-placeholder placeholder response)))

(defun leafy-send-chatgpt-request (request-data)
  "Send an asynchronous request to the ChatGPT API."
  (let* ((url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data (json-encode request-data))
         (api-url "https://api.openai.com/v1/engines/davinci-codex/completions") ; Replace with the correct API URL
         (placeholder (make-marker)))
    (leafy-insert-placeholder)
    (set-marker placeholder (point))
    (url-retrieve api-url #'leafy-chatgpt-callback (list placeholder))))

(defun extract-chatgpt-response-message (response)
  (let* ((choices (cdr (assoc 'choices response)))
	 (choice (aref choices 0))
	 (message (cdr (assoc 'message choice)))
	 (content (cdr (assoc 'content message))))
    content))

;; (extract-chatgpt-response-message '((id . "chatcmpl-81LLbRO80CrTlu0u7A2t8qIQDYOqT") (object . "chat.completion") (created . 1695331079) (model . "gpt-4-0613") (choices . [((index . 0) (message (role . "assistant") (content . "Message acknowledged.")) (finish_reason . "stop"))]) (usage (prompt_tokens . 111) (completion_tokens . 3) (total_tokens . 114))))

;; Test the do-chatgpt-request function
;; (leafy-do-chatgpt-request chatgpt-buffer '((nil "Hello, how are you?") (nil "I'm doing well, thank you.")))
;; 
;; (with-output-to-temp-buffer "*ChatGPT Result*"
;;   (let* ((response (leafy-do-chatgpt-request chatgpt-buffer
;; 				       '((nil "Hello, how are you?") ("assistant" "I'm doing well, thank you."))))
;; 	(result (extract-chatgpt-response-message response)))
;;     (prin1 result)))


(defun leafy-context-entry-tokens (ctx)
  (unless (= (length ctx) 2)
    (user-error "leafy-context-entry-tokens: Strange context:<%S>" ctx))
  (let ((body (leafy-join ctx " ")))
    (leafy-estimate-tokens body)))

(defun leafy-log-context ()
  "Log the given context to the `chatgpt-buffer`."
  (interactive)
  (save-excursion
    (cl-flet ((join-line (chatgpt-context) (leafy-join chatgpt-context ":")))
      (let* ((ctxs (leafy-get-context))
	     (token-counts (mapcar #'leafy-context-entry-tokens ctxs))
	     (tokens-count (apply #'+ token-counts))
	     )
	;;(message "Bodies:<%S>" bodies)
	(message "Tokens: %d(%S)" tokens-count token-counts)
	(princ (format "Context: %S\n" ctxs) chatgpt-buffer)))))

(defun leafy-intervals-contain-p (start1 end1 start2 end2)
  "Check if the interval [START1, END1] is contained within [START2, END2]."
  (and (<= start2 start1)
       (<= end1 end2)))

(defun leafy-get-section-for-element (element)
  "Given an Org mode paragraph element, return the section element that encloses it."
  (let ((section nil))
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (headline)
        (when (and (leafy-intervals-contain-p (org-element-property :begin element) (org-element-property :end element)
					      (org-element-property :begin headline) (org-element-property :end headline))
                                               
                   (or (null section)
                       (leafy-intervals-contain-p (org-element-property :begin headline) (org-element-property :end headline)
						  (org-element-property :begin section) (org-element-property :end section))))
                                                  
          (setq section headline)))
      nil nil t)
    section))

(defun leafy-get-context (&optional buffer)
  "Return the ChatGPT context for the current cursor point."
  (interactive)
  (let ((element (org-element-at-point)))
    (if (null element)
	(user-error "Null element")
      (leafy-get-context-at-element element buffer))))

(defun leafy-get-context-at-element (element &optional buffer)
  "Return a list of all headings and their titles up to the top-level heading, along with their paragraphs."
  (with-current-buffer (or buffer (current-buffer))
    (let* (
	   ;; (whole-buffer-text (buffer-substring-no-properties 1 (point-max)))
	   (all-sections (leafy-get-sections))
	   (cursor-section (leafy-get-section-for-element element))
	   (cursor-section-text (buffer-substring-no-properties (org-element-property :begin cursor-section)
								(org-element-property :end cursor-section)))
	   ;; Always include the last section, so deduct its tokens from max allowed.
	   (max-prompt-size (- (leafy-max-prompt-size) (leafy-estimate-tokens cursor-section-text)))
	   (dropped-sections (leafy-drop-sections all-sections max-prompt-size))
	   (chatgpt-sections (leafy-sections-to-chatgpt dropped-sections))
	   )
      `(,@chatgpt-sections
	;; ("user" ,whole-buffer-text)
	("user" ,cursor-section-text)))))

(defun leafy-insert-chatgpt-response-after (title response &optional extra)
  "Insert a new section with TITLE and CONTENT immediately after the current element."
  (let* ((section (org-element-context))
	 (response-body (extract-chatgpt-response-message response))
	 (usage (alist-get 'usage response))
	 (prompt-tokens (alist-get 'prompt_tokens usage))
	 (completion-tokens (alist-get 'completion_tokens usage))
	 (billed-tokens (alist-get 'total_tokens usage))
	 (estimated-tokens (and extra (alist-get 'estimated-tokens extra)))
	 (start-time (and extra (alist-get :start-time extra)))
	 (elapsed-time (and extra (alist-get :elapsed-time extra)))
	)
    (org-insert-heading-after-current)
    (let ((new-heading (org-element-at-point)))
      (insert title)
      (insert "\n")
      (insert response-body)
      (save-excursion
	(org-with-point-at (org-element-property :begin new-heading)
	  (org-set-tags-to "assistant")
	  (org-set-property "input-tokens" (format "%S" prompt-tokens))
	  (org-set-property "output-tokens" (format "%S" completion-tokens))
	  (org-set-property "billed-tokens" (format "%S" billed-tokens))
	  (when start-time (org-set-property "start-time" (format "%S" start-time)))
	  (when elapsed-time (org-set-property "elapsed-time" (format "%S" elapsed-time)))
	  (when estimated-tokens (org-set-property "estimated-tokens" (format "%d" estimated-tokens)))
	  )))
    ;;(org-entry-put (point) "LEAFY_CONTEXT" nil)
    (outline-show-subtree)))

(defun leafy-get-element-context (element)
  "Context for the given element"
  (let* ((node (org-element-context element))
	 (begin-idx (org-element-property :contents-begin node))
	 (end-idx (org-element-property :contents-end node))
	 (text (buffer-substring begin-idx end-idx))
	 )
    text
    ))

(defun leafy-get-parents (node)
  "Collects the parent hierarchy of NODE in the form of a list of chat turns."
  (let ((parents nil))
    (while (setq node (org-up-heading-safe node))
      (push (cons 'heading (org-element-property :title node)) parents))
    parents))

;; Define a function to search for a tag within an org-heading
(defun heading-with-tag-p (tag)
  (member "my-tag" (org-get-tags)))

(defun get-tagged-section (tag)
  "Returns the section with the specified TAG."
  (interactive "sTag: ")
  (let ((sections '()))
    (org-map-entries
     (lambda ()
       (when (org-at-heading-p)
	 (when (member tag (org-get-tags))
	   (push (org-element-subtree-complete (org-element-at-point)) sections)))))
    sections))

;; Use org-map-entries to retrieve a list of headings with the "my-tag" tag
(defun get-headings-with-tag ()
  (let ((tagged-headings '()))
    (org-map-entries
     (lambda ()
       (when (heading-with-tag-p)
         (push org-heading-components tagged-headings))))
    tagged-headings))

(defun get-property-drawer-pom (drawer-name)
  (let ((found-pom nil))
    (org-element-map (org-element-parse-buffer) 'property-drawer
      (lambda (drawer)
	(let ((pom (org-element-property :begin drawer)))
	  (when (string= drawer-name (org-entry-get pom "drawer-name"))
	    (setq found-pom pom)))))
    found-pom))


(defun get-property-drawer-value (drawer-name key &optional default)
  "Returns the value of the first property drawer with the specified TAG."
  (let ((pom (get-property-drawer-pom drawer-name)))
    (or (when pom (org-entry-get pom key))
	default)))

(cl-defun set-property-drawer-value (drawer-name key value &optional (create-drawer? t))
  "Sets the value of the first property drawer with the specified TAG"
  (let ((pom (get-property-drawer-pom drawer-name)))
    (if pom
	(org-entry-put pom key value)
      (when create-drawer?
	(org-insert-property-drawer)
	(org-entry-put (point) "drawer-name" drawer-name)
	(org-entry-put (point) key value))
      )))


(defun get-drawer-alist-property (drawer-name alist-key property-key &optional default)
  "Parses an alist in a drawer and retrieves a key. Value is returned as a Lisp value."
  (let* ((alist-str (get-property-drawer-value drawer-name alist-key "()"))
	 (alist (read alist-str)))
    (alist-get property-key alist default nil #'equal)))

(defun set-drawer-alist-property (drawer-name alist-key property-key value)
  (let* ((alist-str (get-property-drawer-value drawer-name alist-key "()"))
	 (alist (read alist-str))
	 (new-alist (alist-set property-key value alist)))
    (set-property-drawer-value drawer-name alist-key (format "%S" new-alist))))

(defun get-meta-property (key) (get-property-drawer-value "meta" key))
(defun set-meta-property (key value) (set-property-drawer-value "meta" key value))
(defun tick-meta-counter (key dx)
  (let* ((x0 (string-to-number (or (get-meta-property key) "0")))
	 (x1 (+ x0 dx)))
    (set-meta-property key (number-to-string x1))))

(defun tick-meta-model-counter (model-name key dx)
  "Increments a model-specific counter"
  (let* ((x (get-drawer-alist-property "meta" model-name key 0))
	 (x-new (+ x dx)))
    (set-drawer-alist-property "meta" model-name key x-new)))

(defun leafy-get-sibling-nodes (node)
  "Get all sibling nodes of a given NODE."
  (when node
    (let ((parent (org-element-property :parent node)))
      (org-element-map parent (org-element-type node)
        (lambda (sibling)
          (unless (eq sibling node)
            sibling))
	nil 'tree))))

(defun leafy-prioritize (ctx)
  "Drops sections according to priority list until token limit is reached, and returns the updated context."
  (let ((cur-section (car ctx))
        (limit leafy-chatgpt-context-size)
        (tokens 0)
        (result nil))
    (dolist (section (cdr ctx))
      (let ((priority (leafy-get-priority section))
            (level (car section)))
        (when (or (not (equal level (car cur-section)))
                  (not (member section (leafy-get-sibling-nodes cur-section)))
                  (< priority (leafy-get-priority cur-section)))
          (setq ctx (delete section ctx))))
      (setq tokens (+ tokens (length section)))
      (when (> tokens limit)
        (setq tokens limit))
      (setq result (cons section result)))
    (reverse result)))

(defun leafy-get-property-from-element-section (element property)
  "Extracts PROPERTY from an `org-element-property' section of ELEMENT."
  (let* ((section (leafy-get-section-for-element element))
         (section-property (org-element-property property section)))
    (when section-property
      (org-strip-properties section-property))))

(defun leafy-with-property-drawer (element action)
  (save-excursion
    (org-with-point-at (org-element-property :begin (org-element-for-section element))
      (action))))

(defun leafy-get-property-drawer-under-element (element)
  "Get the first property drawer in the section that encloses the given ELEMENT."
  (let* ((drawer (org-element-map element 'drawer
                   (lambda (d) (when (string= "PROPERTIES" (org-element-property :drawer-name d)) d))
                   nil t)))
    (when drawer
      (buffer-substring (org-element-property :contents-begin drawer) (org-element-property :contents-end drawer)))))

(defun leafy-get-property-drawer-for-element-section (element)
  "Get the first property drawer in the section that encloses the given ELEMENT."
  (leafy-get-property-drawer-under-element (leafy-get-section-for-element element)))

;; (leafy-get-property-drawer-for-element-section (leafy-get-section-for-element (org-element-at-point)))

(defun leafy-get-property-drawer-for-element-section (element)
  "Get the first property drawer in the section that encloses the given ELEMENT.
Returns the properties as an alist."
  (let* ((section (leafy-get-section-for-element element))
         (drawer (org-element-map section 'drawer
                   (lambda (d) (when (string= "PROPERTIES" (org-element-property :drawer-name d)) d))
                   nil t)))
    drawer))

(defun leafy-extract-properties (property-drawer)
  "Extract key-value pairs from the given PROPERTY-DRAWER string."
  (let ((properties ()))
    (while (string-match "^\\s-*:\\([[:alnum:]\\-]+\\):\\s-*\\(.+\\)\\s-*$" property-drawer)
      (push (cons (intern (downcase (match-string 1 property-drawer)))
		  (match-string 2 property-drawer)) properties)
      (setq property-drawer (substring property-drawer (match-end 0))))
    (reverse properties)))

(require 'ert)
(require 'org)

(defun leafy-get-priority (props)
  (string-to-number (leafy-get-key props leafy-priority-key "0")))

(ert-deftest leafy-test-get-priority ()
  (with-temp-buffer
    (insert "
* Top-level heading
  :PROPERTIES:
  :CONTEXT-PRIORITY: 3
  :END:
** Subheading
*** Sub-subheading
    ")
    (goto-char (point-min))
    (let ((sections (leafy-get-sections)))
      (ert-info ((format "Sections: %S" sections))
	(should (equal 3 (length sections))))
      (dolist (section sections)
	(ert-info ((format "Section: %S" section))
	  (pcase section
	    (`(1 ,props ,tag ,title ,content)
	     (should (equal 3 (leafy-get-priority props))))
	    (`(2 ,props ,tag ,title ,content)
	     (should (equal 0 (leafy-get-priority props))))
	    (`(3 ,props ,tag ,title ,content)
	     (should (equal 0 (leafy-get-priority props))))))))))

(defun leafy-set-priority (props priority)
  "Set the PRIORITY property of the given HEADLINE to the specified value."
  (leafy-set-key props leafy-priority-key (number-to-string priority)))

(ert-deftest test-leafy-set-priority ()
  "Test if leafy-set-priority sets the priority property of a section correctly."
  (cl-flet ((test-case (org-content section-id initial-priority new-priority)
                       (with-temp-buffer
                         (org-mode)
                         (insert org-content)
                         (goto-char (point-min))
                         (let* ((sections (leafy-get-sections))
                                (section (nth (- section-id 1) sections)))
                           (pcase section
                             (`(,_level ,props ,_tag ,_title ,_content)
			      (should (equal initial-priority (leafy-get-priority props)))
			      (leafy-set-priority props new-priority)
			      (should (equal new-priority (leafy-get-priority props)))))))))
    ;; Case 1: Top-level heading
    (test-case "* Top-level heading
:PROPERTIES:
:CONTEXT-PRIORITY: 3
:END:"
               1 3 5)
    ;; Case 2: Subheading with no property drawer
    (test-case "* Top-level heading
** Subheading"
               2 0 10)
    ;; Case 3: Sub-subheading
    (test-case "* Top-level heading
** Subheading
*** Sub-subheading
:PROPERTIES:
:CONTEXT-PRIORITY: 5
:END:"
               3 5 15)))

(defun leafy-project-cost ()
  (let* ((total-cost 0)
         (total-input-tokens 0)
	 (total-output-tokens 0)
         (total-billed-tokens 0))
    ;; Iterate over leafy-model-info-alist
    (dolist (model leafy-model-info-alist)
      (let* ((model-name (car model))
             (price (leafy-model-price model-name))
	     (input-price (car price))
	     (output-price (cdr price))
	     (model-input-tokens (get-drawer-alist-property "meta" model-name "input-tokens" 0))
	     (model-output-tokens (get-drawer-alist-property "meta" model-name "output-tokens" 0))
	     (model-billed-tokens (get-drawer-alist-property "meta" model-name "billed-tokens" 0))
	     (model-input-cost (* input-price model-input-tokens))
	     (model-output-cost (* output-price model-output-tokens))
             (model-cost (+ model-input-cost model-output-cost))
	     )
	;; (message "@@@@ model-name:%s info:%S" model-name
	;; 	 `((price . ,price)
	;; 	   (cost . ,model-cost)
	;; 	   (mic . ,model-input-cost)
	;; 	   (mit . ,model-input-tokens)
	;; 	   (mot . ,model-output-tokens)
	;; 	   (mbt . ,model-billed-tokens)))
        (setq total-cost (+ total-cost model-cost))
        (setq total-input-tokens (+ total-input-tokens model-input-tokens))
	(setq total-output-tokens (+ total-output-tokens model-output-tokens))
        (setq total-billed-tokens (+ total-billed-tokens model-billed-tokens))
	;;(unless (= 0 model-billed-tokens)
        ;;  (message "Model %s has cost $%.2f" model-name model-cost))
	))
    `((total-cost . ,total-cost)
      (total-input-tokens . ,total-input-tokens)
      (total-output-tokens . ,total-output-tokens)
      (total-billed-tokens . ,total-billed-tokens))
    ))
    

(defun leafy-print-costs ()
  "Estimate how much $$$$ you've spent on the OpenAI so far"
  (interactive)
  (let* ((costs (leafy-project-cost))
	 (total-cost (alist-get 'total-cost costs))
	 (total-input-tokens (alist-get 'total-input-tokens costs))
	 (total-output-tokens (alist-get 'total-output-tokens costs))
	 (total-billed-tokens (alist-get 'total-billed-tokens costs))
	 )
    ;; TODO: This calculation is somewhat misleading, since some models bill different amounts for input/output. Maybe "% of cost spent on context" should be weighted.
    (message "Total cost: $%.2f - %.0f%% of tokens were context" total-cost (/ (* 100.0 total-input-tokens) total-billed-tokens))))

(cl-defun leafy-drop-one-section (sections)
  "Returns the index to remove of the last section that increases indentation."
  (let ((len (length sections))
        (indentation-level -1)
        (found-section-index nil))
    (dotimes (i len)
      (let ((j (- len i 1)))
        (when (> (car (nth j sections)) indentation-level)
          (setq found-section-index j)
          (setq indentation-level (car (nth j sections))))))
    (or found-section-index 0)))

(ert-deftest test-leafy-drop-one-section ()
  "Test for `leafy-drop-one-section` function."
  (let ((sections '((1 nil nil "Section 1" "Text for section 1.")
                    (2 nil nil "Subsection 1" "Text for subsection 1.")
                    (2 nil nil "Subsection 2" "Text for subsection 2.")
                    (3 nil nil "Subsubsection 1" "Text for subsubsection 1.")
                    (1 nil nil "Section 2" "Text for section 2."))))
    ;; Drop non-keeper
    (let* ((idx (leafy-drop-one-section sections))
           (modified-sections (leafy-remove-at sections idx)))
      (should (equal idx 3))
      (should (equal modified-sections
                     '((1 nil nil "Section 1" "Text for section 1.")
                       (2 nil nil "Subsection 1" "Text for subsection 1.")
                       (2 nil nil "Subsection 2" "Text for subsection 2.")
                       (1 nil nil "Section 2" "Text for section 2.")))))))

(defun leafy-section-count-tokens (section)
  "Count the number of tokens in a section using `leafy-count-tokens`."
  (leafy-context-entry-tokens (leafy-section-to-chatgpt section)))

 (defun leafy-drop-sections (sections context-size)
  "Drops sections one by one until the list fits within the CONTEXT-SIZE limit.
  If the original list is smaller than the limit, returns the original list.
  Otherwise, returns the new list with dropped sections."
  (let ((new-sections (copy-sequence sections)))
    (cl-flet ((count-sections (ctx) (apply #'+ (mapcar #'leafy-section-count-tokens ctx))))
      (while (and (> (length new-sections) 1)
		  (> (count-sections new-sections) context-size))
	(let* ((index-to-remove (leafy-drop-one-section new-sections))
	       (section (nth index-to-remove new-sections))
	       (section-tokens (leafy-section-count-tokens section))
	       )
	  ;; (message "Dropping section with %d tokens" section-tokens)
          (setq new-sections (leafy-remove-at new-sections index-to-remove))))
      (message "leafy-drop-sections: Before:%d After:%d context:%d" (count-sections sections) (count-sections new-sections) context-size)
      new-sections)))

(ert-deftest test-leafy-drop-sections-1 ()
  (let ((leafy-token-counting-method 'exact))
    (let* ((sections '((1 nil nil "Section 1" "Text for section 1.")
                       (2 nil nil "Subsection 1" "Text for subsection 1.")
                       (2 nil nil "Subsection 2" "Text for subsection 2.")
                       (3 nil nil "Subsubsection 1" "Text for subsubsection 1.")
                       (1 nil nil "Section 2" "Text for section 2.")))
           (context-size 25) ;; Set the context size to 35, forcing at least two sections to be dropped
           (result (leafy-drop-sections sections context-size)))
      (should (equal 46 (apply #'+ (mapcar #'leafy-section-count-tokens sections)))) ;; Before dropping any sections
      ;; (message "result: %S" result)
      (should (equal (length result) 3))))) ;; Expecting only 3 sections to remain

(ert-deftest test-leafy-drop-sections-2 ()
  "Test for `leafy-drop-sections` function."
  (let ((sections '((0 nil "user" "(Leafy Metadata)" "")
                    (0 nil "user" "(Project Summary)" "This project involves using the ChatGPT model from OpenAI to assist with project planning and organization within an org-mode file.")
                    (0 nil "user" "(Functions:)" "`leafy-get-section-for-element`: Given an Org mode paragraph element, returns the section element that encloses it.")
                    (0 nil "user" "(Project)" "Hello ChatGPT. Today we are working on the first item in the todo list that I expect is in your context. Can you repeat it so I know you can see it?")))
        (context-size 3072))
    (let ((result (leafy-drop-sections sections context-size)))
      (should (equal (length result) 4))
      (should (equal result sections)))))

(ert-deftest test-leafy-drop-sections-3 ()
  (with-temp-buffer
    (insert "
* Leafy Metadata
* Project Summary
* todo items
** Immediate items
*** TODO Prepare a context in a temporary buffer.
*** TODO Automatically strip out context to fit within the limit.
** Backlog
*** TODO Include current price of project in the status bar.
*** TODO Allow selecting between different OpenAI models.
*** TODO ChatGPT operator templates acting on code blocks
*** TODO Recursive ChatGPT templates
* Chat
** Project
*** ChatGPT Response :assistant:
aoeu
")
    (org-mode)
    (let ((sections (leafy-get-context (current-buffer)))
          (expected '(("user" "Leafy Metadata")
                      ("user" "Project Summary")
                      ("user" "todo items")
                      ("user" "Immediate items")
                      ("user" "Prepare a context in a temporary buffer.")
                      ("user" "Automatically strip out context to fit within the limit.")
                      ("user" "Backlog")
                      ("user" "Include current price of project in the status bar.")
                      ("user" "Allow selecting between different OpenAI models.")
                      ("user" "ChatGPT operator templates acting on code blocks")
                      ("user" "Recursive ChatGPT templates")
                      ("user" "Chat")
                      ("user" "Project")
                      ("assistant" "ChatGPT Response\naoeu\n")
		      ("user" "*** ChatGPT Response :assistant:\naoeu\n")
		      )))
      ;; (message "test-leafy-drop-sections-3 sections:<%S>" (leafy-get-context))
      (should (equal sections expected)))))

(ert-deftest test-minified-org-document ()
  (with-temp-buffer
    (insert "
* Leafy Metadata
:PROPERTIES:
:drawer-name: meta
:input-tokens: 215172
:output-tokens: 35780
:billed-tokens: 250952
:END:

* Project Summary

* todo items
** Immediate items
*** TODO Prepare a context in a temporary buffer.
*** TODO Automatically strip out context to fit within the limit.

** Backlog
*** TODO Include current price of project in the status bar.
*** TODO Allow selecting between different OpenAI models.
*** TODO ChatGPT operator templates acting on code blocks
*** TODO Recursive ChatGPT templates

* Chat

** Project

*** ChatGPT Response                                               :assistant:
:PROPERTIES:
:input-tokens: 422
:output-tokens: 42
:billed-tokens: 464
:END:
aoeu
")
    (org-mode)
    (let ((sections (leafy-get-context))
          (expected '(("user" "Leafy Metadata")
                      ("user" "Project Summary")
                      ("user" "todo items")
                      ("user" "Immediate items")
                      ("user" "Prepare a context in a temporary buffer.")
                      ("user" "Automatically strip out context to fit within the limit.")
                      ("user" "Backlog")
                      ("user" "Include current price of project in the status bar.")
                      ("user" "Allow selecting between different OpenAI models.")
                      ("user" "ChatGPT operator templates acting on code blocks")
                      ("user" "Recursive ChatGPT templates")
                      ("user" "Chat")
                      ("user" "Project")
                      ("assistant" "ChatGPT Response\naoeu\n")
		      ("user" "*** ChatGPT Response                                               :assistant:\n:PROPERTIES:
:input-tokens: 422
:output-tokens: 42
:billed-tokens: 464
:END:
aoeu
")
		      )))
      (should (equal sections expected)))))

(provide 'leafy)
