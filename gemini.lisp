;;; -*- Lisp -*-

(in-package "GEMINI")

(defun googleapis-pathname ()
  (uiop/configuration:xdg-config-pathname "googleapis/"))

(defun default-apikey-pathname ()
  (merge-pathnames "apikey" (googleapis-pathname)))

(defun default-project-pathname ()
  (merge-pathnames "default-project" (googleapis-pathname)))

(defun default-project ()
  (let ((pathname (default-project-pathname)))
    (when (probe-file pathname)
      (with-open-file (stream pathname :direction :input)
        (let ((line (read-line stream nil)))
          (when line
            (str:trim line))))))) 

(defun project-apikey-pathname (project)
  (merge-pathnames (make-pathname :directory (list :relative project)
                                  :name "apikey")
                   (googleapis-pathname)))

(defun apikey-pathname ()
  (or (let ((project (default-project)))
        (when project
          (probe-file (project-apikey-pathname project))))
      (probe-file (default-apikey-pathname))))

(defun google-api-key ()
  (or (let ((pathname (apikey-pathname)))
        (and pathname
            (with-open-file (stream pathname :direction :input)
              (let ((line (read-line stream nil)))
                (when line
                  (str:trim line))))))
      (uiop:getenv "GOOGLE_API_KEY")
      (error "No Google API key found. Set the environment variable GOOGLE_API_KEY or create a file at ~a."
             (namestring (apikey-pathname)))))

(defparameter
  +gemini-api-base-url+
  "https://generativelanguage.googleapis.com/v1beta/models/")

(defun %invoke-gemini (model-id payload)
  "Invokes the Gemini API with the specified MODEL-ID and PAYLOAD.
   Returns the response from the API as a decoded JSON object."
  (let ((response (dex:post (concatenate 'string +gemini-api-base-url+ model-id ":generateContent")
                            :headers `(("Content-Type" . "application/json")
                                       ("x-goog-api-key" . ,(google-api-key)))
                            :content (cl-json:encode-json-to-string payload))))
    (if (stringp response)
        (cl-json:decode-json-from-string response)
        (cl-json:decode-json-from-string
         (flex:octets-to-string response :external-format :utf-8)))))

(defun key? (thing)
  "Checks if THING is a valid key (keyword or string)."
  (or (keywordp thing)
      (stringp thing)))

(defun alist? (thing)
  "Checks if THING is an alist."
  (or (and (consp thing)
           (every (lambda (entry)
                    (and (consp entry)
                         (key? (car entry))))
                  thing))
      (null thing)))

(defun object-ref-function (keyword string)
  (lambda (thing)
    (cond ((alist? thing)
           (cdr (find-if (lambda (entry)
                           (and (consp entry)
                                (or (and (keywordp (car entry))
                                         (eq (car entry) keyword))
                                    (and (stringp (car entry))
                                         (equal (car entry) string)))))
                         thing)))
          ((hash-table-p thing)
           (cond ((member keyword (hash-table-keys thing) :test 'equal)
                  (gethash keyword thing))
                 ((member string (hash-table-keys thing) :test 'equal)
                  (gethash string thing))
                 (t nil))))))

(defun list-of-test (predicate)
  (lambda (thing)
    "Returns a predicate that checks if THING is a list of elements satisfying PREDICATE."
    (and (consp thing)
         (every predicate thing))))

(defun singleton-list-of-test (predicate)
  "Returns a predicate that checks if THING is a singleton list of an element satisfying PREDICATE."
  (lambda (thing)
    "Checks if THING is a singleton list of an element satisfying PREDICATE."
    (and (consp thing)
         (null (cdr thing))
         (funcall predicate (car thing)))))

(defun key-test (keyword string)
  "Returns predicate to checks if THING is a valid key based on the provided KEYWORD and STRING."
  (lambda (thing) 
    (or (and (keywordp thing) (eq thing keyword))
        (and (stringp thing) (equal thing string)))))

(defun required-key-test (required-key)
  (let ((has-required-key? (key-test (first required-key) (second required-key))))
    (lambda (thing)
      (find-if has-required-key? (keys thing)))))

(defun required-keys-test (required-keys)
  "Checks if THING contains all required keys from KEYS-REQUIRED."
  (let ((required-key-tests (mapcar #'required-key-test required-keys)))
    (lambda (thing)
      (every (lambda (test)
               (funcall test thing))
             required-key-tests))))

(defun keys (thing)
  (cond ((alist? thing) (mapcar #'car thing))
        ((hash-table-p thing) (hash-table-keys thing))
        (t (error "Can't get keys of ~s" thing))))

(defun valid-key-test (keywords strings)
  "Returns predicate to check if key is in keywords or strings."
  (lambda (thing)
    (or (and (keywordp thing) (member thing keywords))
        (and (stringp thing) (member thing strings :test 'equal)))))

(defun valid-keys-test (keywords strings)
  "Returns predicate to check if all keys are in keywords or strings."
  (let ((valid-key? (valid-key-test keywords strings)))
    (lambda (thing)
      (every valid-key? (keys thing)))))

(defun is-object-test (required-keys keywords strings)
  "Return predicate that checks if THING is a valid object based on the provided REQUIRED-KEYS, KEYWORDS, and STRINGS."
  (let ((has-required-keys? (required-keys-test required-keys))
        (are-keys-valid? (valid-keys-test keywords strings)))
    (lambda (thing)
      (and (or (alist? thing)
               (hash-table-p thing))
           (funcall has-required-keys? thing)
           (funcall are-keys-valid? thing)))))

(defvar *cached-content*)

(defun default-cached-content ()
  "Returns a default cached content object."
  (when (boundp '*cached-content*)
    *cached-content*))

(defvar *candidate-count*)

(defun default-candidate-count ()
  "Returns a default candidate count."
  (when (boundp '*candidate-count*)
    *candidate-count*))

(defvar *enable-advanced-civic-answers*)

(defun default-enable-advanced-civic-answers ()
  "Returns a default value for enabling advanced civic answers."
  (when (boundp '*enable-advanced-civic-answers*)
    *enable-advanced-civic-answers*))

(defvar *frequency-penalty*)

(defun default-frequency-penalty ()
  "Returns a default frequency penalty."
  (when (boundp '*frequency-penalty*)
    *frequency-penalty*))

(defvar *logprobs*)

(defun default-logprobs ()
  "Returns a default logprobs."
  (when (boundp '*logprobs*)
    *logprobs*))

(defvar *max-output-tokens*)

(defun default-max-output-tokens ()
  "Returns a default maximum output tokens."
  (when (boundp '*max-output-tokens*)
    *max-output-tokens*))

(defvar *media-resolution*)

(defun default-media-resolution ()
  "Returns a default media resolution."
  (when (boundp '*media-resolution*)
    *media-resolution*))

(defvar *presence-penalty*)

(defun default-presence-penalty ()
  "Returns a default presence penalty."
  (when (boundp '*presence-penalty*)
    *presence-penalty*))

(defvar *response-logprobs*)

(defun default-response-logprobs ()
  "Returns a default response logprobs."
  (when (boundp '*response-logprobs*)
    *response-logprobs*))

(defvar *response-mime-type*)

(defun default-response-mime-type ()
  "Returns a default response MIME type."
  (when (boundp '*response-mime-type*)
    *response-mime-type*))

(defvar *response-modalities*)

(defun default-response-modalities ()
  "Returns a default response modalities object."
  (when (boundp '*response-modalities*)
    *response-modalities*))

(defvar *response-schema*)

(defun default-response-schema ()
  "Returns a default response schema object."
  (when (boundp '*response-schema*)
    *response-schema*))

(defvar *response-json-schema*)

(defun default-response-json-schema ()
  "Returns a default response JSON schema object."
  (when (boundp '*response-json-schema*)
    *response-json-schema*))

(defvar *seed*)

(defun default-seed ()
  "Returns a default seed value."
  (when (boundp '*seed*)
    *seed*))

(defvar *language-code*)

(defun default-language-code ()
  "Returns a default language code."
  (when (boundp '*language-code*)
    *language-code*))

(defvar *multi-speaker-voice-config*)

(defun default-multi-speaker-voice-config ()
  "Returns a default multi-speaker voice configuration object."
  (when (boundp '*multi-speaker-voice-config*)
    *multi-speaker-voice-config*))

(defvar *voice-name*)

(defun default-voice-name ()
  "Returns a default voice name."
  (when (boundp '*voice-name*)
    *voice-name*))

(defvar *prebuilt-voice-config*)

(defun default-prebuilt-voice-config ()
  "Returns a default prebuilt voice configuration object."
  (if (boundp '*prebuilt-voice-config*)
      *prebuilt-voice-config*
      (let ((prebuilt-voice-config (make-hash-table :test 'equal)))
        (let ((voice-name (default-voice-name)))
          (when voice-name
            (setf (gethash "voiceName" prebuilt-voice-config) voice-name)))
        (unless (zerop (hash-table-count prebuilt-voice-config))
          prebuilt-voice-config))))

(defvar *voice-config*)

(defun default-voice-config ()
  "Returns a default voice configuration object."
  (if (boundp '*voice-config*)
      *voice-config*
      (let ((voice-config (make-hash-table :test 'equal)))
        (let ((prebuilt-voice-config (default-prebuilt-voice-config)))
          (when prebuilt-voice-config
            (setf (gethash "prebuiltVoiceConfig" voice-config)
                  prebuilt-voice-config)))
        (unless (zerop (hash-table-count voice-config))
          voice-config))))

(defvar *speech-config*)

(defun default-speech-config ()
  "Returns a default speech configuration object."
  (if (boundp '*speech-config*)
      *speech-config*
      (let ((speech-config (make-hash-table :test 'equal)))
        (let ((language-code (default-language-code)))
          (when language-code
            (setf (gethash "languageCode" speech-config) language-code)))
        (let ((multi-speaker-voice-config (default-multi-speaker-voice-config)))
          (when multi-speaker-voice-config
            (setf (gethash "multiSpeakerVoiceConfig" speech-config)
                  multi-speaker-voice-config)))
        (let ((voice-config (default-voice-config)))
          (when voice-config
            (setf (gethash "voiceConfig" speech-config) voice-config)))
        (unless (zerop (hash-table-count speech-config))
          speech-config))))

(defvar *stop-sequences*)

(defun default-stop-sequences ()
  "Returns a default stop sequences object."
  (when (boundp '*stop-sequences*)
    *stop-sequences*))

(defvar *temperature*)

(defun default-temperature ()
  "Returns a default temperature value."
  (when (boundp '*temperature*)
    *temperature*))

(defvar *include-thoughts*)

(defun default-include-thoughts ()
  "Returns a default value for including thoughts in the response."
  (when (boundp '*include-thoughts*)
    *include-thoughts*))

(defvar *thinking-budget*)

(defun default-thinking-budget ()
  "Returns a default value for thinking budget."
  (when (boundp '*thinking-budget*)
    *thinking-budget*))

(defvar *thinking-config*)

(defun default-thinking-config ()
  "Returns a default thinking configuration object."
  (if (boundp '*thinking-config*)
      *thinking-config*
      (let ((thinking-confing (make-hash-table :test 'equal)))
        (let ((include-thoughts (default-include-thoughts)))
          (when include-thoughts
            (setf (gethash "includeThoughts" thinking-confing) include-thoughts)))
        (let ((thinking-budget (default-thinking-budget)))
          (when thinking-budget
            (setf (gethash "thinkingBudget" thinking-confing) thinking-budget)))
        (unless (zerop (hash-table-count thinking-confing))
          thinking-confing))))

(defvar *top-k*)

(defun default-top-k ()
  "Returns a default top-k value."
  (when (boundp '*top-k*)
    *top-k*))

(defvar *top-p*)

(defun default-top-p ()
  "Returns a default top-p value."
  (when (boundp '*top-p*)
    *top-p*))

(defvar *tools*)

(defun default-tools ()
  "Returns a default tools object."
  (when (boundp '*tools*)
    *tools*))

(defvar *tool-config*)

(defun default-tool-config ()
  "Returns a default tool configuration object."
  (when (boundp '*tool-config*)
    *tool-config*))

;; Default generation configuration object
(defvar *generation-config*)

(defun default-generation-config ()
  "Returns a default generation configuration object."
  (if (boundp '*generation-config*)
      *generation-config*
      (let ((gen-config (make-hash-table :test 'equal)))
        (let ((candidate-count (default-candidate-count)))
          (when candidate-count
            (setf (gethash "candidateCount" gen-config) candidate-count)))
        (let ((enable-advanced-civic-answers (default-enable-advanced-civic-answers)))
          (when enable-advanced-civic-answers
            (setf (gethash "enableAdvancedCivicAnswers" gen-config) enable-advanced-civic-answers)))
        (let ((frequency-penalty (default-frequency-penalty)))
          (when frequency-penalty
            (setf (gethash "frequencyPenalty" gen-config) frequency-penalty)))
        (let ((max-output-tokens (default-max-output-tokens)))
          (when max-output-tokens
            (setf (gethash "maxOutputTokens" gen-config) max-output-tokens)))
        (let ((media-resolution (default-media-resolution)))
          (when media-resolution
            (setf (gethash "mediaResolution" gen-config) media-resolution)))
        (let ((presence-penalty (default-presence-penalty)))
          (when presence-penalty
            (setf (gethash "presencePenaly" gen-config) presence-penalty)))
        (let ((response-logprobs (default-response-logprobs)))
          (when response-logprobs
            (setf (gethash "responseLogprobs" gen-config) response-logprobs)))
        (let ((logprobs (default-logprobs)))
          (when logprobs
            (assert (gethash "responseLogprobs" gen-config)
                    () "Response logprobs must be set when logprobs is set.")
            (setf (gethash "logprobs" gen-config) logprobs)))
        (let ((response-mime-type (default-response-mime-type)))
          (when response-mime-type
            (setf (gethash "responseMimeType" gen-config) response-mime-type)))
        (let ((response-modalities (default-response-modalities)))
          (when response-modalities
            (setf (gethash "responseModalities" gen-config) response-modalities)))
        (let ((response-schema (default-response-schema)))
          (when response-schema
            (assert (gethash "responseMimeType" gen-config)
                    () "Response MIME type must be set.")
            (setf (gethash "responseSchema" gen-config) response-schema)))
        (let ((response-json-schema (default-response-json-schema)))
          (when response-json-schema
            (assert (gethash "responseMimeType" gen-config)
                    () "Response MIME type must be set.")
            (assert (not (gethash "responseSchema" gen-config))
                    () "Response schema must not be set when response JSON schema is set.")
            (setf (gethash "responseJsonSchema" gen-config) response-json-schema)))
        (let ((seed (default-seed)))
          (when seed
            (setf (gethash "seed" gen-config) seed)))
        (let ((speech-config (default-speech-config)))
          (when speech-config
            (setf (gethash "speechConfig" gen-config) speech-config)))
        (let ((stop-sequences (default-stop-sequences)))
          (when stop-sequences
            (setf (gethash "stopSequences" gen-config) stop-sequences)))
        (let ((temperature (default-temperature)))
          (when temperature
            (setf (gethash "temperature" gen-config) temperature)))
        (let ((thinking-config (default-thinking-config)))
          (when thinking-config
            (setf (gethash "thinkingConfig" gen-config) thinking-config)))
        (let ((top-k (default-top-k)))
          (when top-k
            (setf (gethash "topK" gen-config) top-k)))
        (let ((top-p (default-top-p)))
          (when top-p
            (setf (gethash "topP" gen-config) top-p)))
        (unless (zerop (hash-table-count gen-config))
          gen-config))))
            
(defvar *safety-settings*)

(defun default-safety-settings ()
  (if (boundp '*safety-settings*)
      *safety-settings*
      nil))

(defvar *system-instruction*)

(defun default-system-instruction ()
  (if (boundp '*system-instruction*)
      *system-instruction*
      nil))

(defun content (&key role (parts (error "Content must contain PARTS.")))
  "Creates a content object with the specified PARTS.
   Returns a hash table representing the content."
  (let ((content (make-hash-table :test 'equal)))
    (when role
      (setf (gethash "role" content) role))
    (setf (gethash "parts" content) parts)
    content))

(deff candidate? (is-object-test '((:content "content"))
                                 '(:content :finish-reason :index)
                                 '("content" "finishReason" "index")))
(deff list-of-candidates? (list-of-test #'candidate?))
(deff singleton-list-of-candidates? (singleton-list-of-test #'candidate?))

(deff get-candidates (object-ref-function :candidates "candidates"))
(deff get-content (object-ref-function :content "content"))
(deff get-finish-reason (object-ref-function :finish-reason "finishReason"))
(deff get-parts (object-ref-function :parts "parts"))
(deff get-role (object-ref-function :role "role"))
(deff get-text (object-ref-function :text "text"))

(deff content?
    (is-object-test '((:parts "parts"))
                    '(:parts :role)
                    '("parts" "role")))

(deff list-of-content? (list-of-test #'content?))
(deff singleton-list-of-content? (singleton-list-of-test #'content?))

(deff blob?
    (is-object-test '((:data "data")
                      (:mime-type "mimeType"))
                    '(:data :mime-type)
                    '("data" "mimeType")))

(deff function-call?
    (is-object-test '((:name "name"))
                    '(:args :id :name)
                    '("args" "id" "name")))

(deff function-response?
    (is-object-test '((:name "name")
                      (:response "response"))
                    '(:id :name :response :scheduling :will-continue)
                    '("id" "name" "response" "scheduling" "willContinue")))

(deff file-data?
    (is-object-test '((:file-uri "fileUri"))
                    '(:file-uri :mime-type)
                    '("fileUrl" "mimeType")))

(deff executable-code?
    (is-object-test '((:code "code")
                      (:language "language"))
                    '(:code :language)
                    '("code" "language")))

(deff code-execution-result?
    (is-object-test '((:outcome "outcome"))
                    '(:outcome :output)
                    '("outcome" "output")))

(defun part (data &key metadata thought thought-signature)
  "Creates a part object with the specified DATA, METADATA, THOUGHT, and THOUGHT-SIGNATURE.
   Returns a hash table representing the part."
  (let ((part (make-hash-table :test 'equal)))
    (cond ((stringp data) (setf (gethash "text" part) data))
          ((blob? data) (setf (gethash "inlineData" part) data))
          ((function-call? data) (setf (gethash "functionCall" part) data))
          ((function-response? data) (setf (gethash "functionResponse" part) data))
          ((file-data? data) (setf (gethash "fileData" part) data))
          ((executable-code? data) (setf (gethash "executableCode" part) data))
          ((code-execution-result? data) (setf (gethash "codeExecutionResult" part) data))
          (t (error "Unrecognized data: ~s" data)))
    (when metadata
      (setf (gethash "metadata" part) metadata))
    (when thought
      (setf (gethash "thought" part) thought))
    (when thought-signature
      (setf (gethash "thoughtSignature" part) thought-signature))
    part))


(deff text-object? (is-object-test '((:text "text"))
                                        '(:text)
                                        '("text")))
(deff inline-data-object? (is-object-test '((:inline-data "inlineData"))
                                          '(:inline-data)
                                          '("inlineData")))
(deff function-call-object? (is-object-test '((:function-call "functionCall"))
                                            '(:function-call)
                                            '("functionCall")))
(deff function-response-object? (is-object-test '((:function-response "functionResponse"))
                                                '(:function-response)
                                                '("functionResponse")))
(deff file-data-object? (is-object-test '((:file-data "fileData"))
                                        '(:file-data)
                                        '("fileData")))
(deff executable-code-object? (is-object-test '((:executable-code "executableCode"))
                                              '(:executable-code)
                                              '("executableCode")))
(deff code-execution-result-object? (is-object-test
                                     '((:code-execution-result "codeExecutionResult"))
                                     '(:code-execution-result)
                                     '("codeExecutionResult")))
(deff part?
      (lambda (thing)
        (or (text-object? thing)
            (inline-data-object? thing)
            (function-call-object? thing)
            (function-response-object? thing)
            (file-data-object? thing)
            (executable-code-object? thing)
            (code-execution-result-object? thing))))

(deff list-of-parts? (list-of-test #'part?))
(deff singleton-list-of-parts? (singleton-list-of-test #'part?))
(deff list-of-strings? (list-of-test #'stringp))

(defun default-process-part (part)
  (cond ((text-object? part) (get-text part))
        (t part)))

(defun default-process-content (content)
  (if (equal (get-role content) "model")
      (let ((parts (get-parts content)))
        (if (singleton-list-of-parts? parts)
            (default-process-part (first parts))
            parts))
      content))

(defun default-process-candidate (candidate)
  (unless (equal (get-finish-reason candidate) "STOP")
    (error "Invalid finish reason: ~s" candidate))
  (default-process-content (get-content candidate)))

(defun default-gemini-output-processor (response)
  (let ((candidates (get-candidates response)))
    (if (singleton-list-of-candidates? candidates)
        (default-process-candidate (first candidates))
        candidates)))

(defvar *gemini-output-processor* #'default-gemini-output-processor
  "Function to process the output of the Gemini API.
   Can be set to a custom function to handle the response differently.")

(defun invoke-gemini (model-id contents &key
                                          (cached-content (default-cached-content))
                                          (generation-config (default-generation-config))
                                          (tools (default-tools))
                                          (tool-config (default-tool-config))
                                          (safety-settings (default-safety-settings))
                                          (system-instruction (default-system-instruction)))
  "Invokes the Gemini API with the specified MODEL-ID, CONTENTS, and GENERATION-CONFIG.
   Returns the response from the API as a decoded JSON object."
  (let* ((payload (make-hash-table :test 'equal)))
    (setf (gethash "contents" payload)
          (cond ((contents? contents) (list contents))
                ((part? contents) (list (content :parts contents)))
                ((stringp contents) (list (content :parts (list (part contents)))))
                ((list-of-contents? contents) contents)
                ((list-of-parts? contents) (content :parts contents))
                ((list-of-strings? contents) (content :parts (list (mapcar #'part contents))))
                (t (error "Unrecognized contents: ~s" contents))))
    (when cached-content
      (setf (gethash "cachedContent" payload) cached-content))
    (when generation-config
      (setf (gethash "generationConfig" payload) generation-config))
    (when safety-settings
      (setf (gethash "safetySettings" payload) safety-settings))
    (when system-instruction
      (setf (gethash "systemInstruction" payload) system-instruction))
    (when tools
      (setf (gethash "tools" payload) tools))
    (when tool-config
      (setf (gethash "toolConfig" payload) tool-config))
    (funcall *gemini-output-processor* (%invoke-gemini model-id payload))))
