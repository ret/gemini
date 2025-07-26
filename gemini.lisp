;;; -*- Lisp -*-

(in-package "GEMINI")

(defun googleapis-pathname ()
  "Returns the base pathname for Google APIs configuration files,
   typically located in the user's XDG config directory."
  (uiop/configuration:xdg-config-pathname "googleapis/"))

(defun default-apikey-pathname ()
  "Returns the default pathname for the API key file within the
   Google APIs configuration directory."
  (merge-pathnames "apikey" (googleapis-pathname)))

(defun default-project-pathname ()
  "Returns the default pathname for the default project file within the
   Google APIs configuration directory."
  (merge-pathnames "default-project" (googleapis-pathname)))

(defun default-project ()
  "Reads and returns the default Google Cloud project name from its
   designated configuration file. Returns NIL if the file does not exist
   or is empty."
  (let ((pathname (default-project-pathname)))
    (when (probe-file pathname)
      (with-open-file (stream pathname :direction :input)
        (let ((line (read-line stream nil)))
          (when line
            (str:trim line)))))))

(defun project-apikey-pathname (project)
  "Constructs the pathname for the API key file specific to a given
   Google Cloud PROJECT within the Google APIs configuration directory."
  (merge-pathnames (make-pathname :directory (list :relative project)
                                  :name "apikey")
                   (googleapis-pathname)))

(defun apikey-pathname ()
  "Determines the effective pathname for the Google API key.
   It first checks for a project-specific API key (if a default project
   is set), then falls back to the default API key pathname.
   Returns the pathname if found, otherwise NIL."
  (or (let ((project (default-project)))
        (when project
          (probe-file (project-apikey-pathname project))))
      (probe-file (default-apikey-pathname))))

(defun google-api-key ()
  "Retrieves the Google API key. It first attempts to read it from
   the API key file (either project-specific or default), then falls back
   to the GOOGLE_API_KEY environment variable.
   Signals an error if no API key is found."
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
  "https://generativelanguage.googleapis.com/v1beta/models/"
  "The base URL for the Gemini API endpoints.")

(defun %invoke-gemini (model-id payload)
  "Invokes the Gemini API with the specified MODEL-ID and PAYLOAD.
   Returns the response from the API as a decoded JSON object.
   This is an internal helper function."
  (let ((response (dex:post (concatenate 'string +gemini-api-base-url+ model-id ":generateContent")
                            :headers `(("Content-Type" . "application/json")
                                       ("x-goog-api-key" . ,(google-api-key)))
                            :content (cl-json:encode-json-to-string payload))))
    (if (stringp response)
        (cl-json:decode-json-from-string response)
        (cl-json:decode-json-from-string
          (flex:octets-to-string response :external-format :utf-8)))))

(defun key? (thing)
  "Checks if THING is a valid key (either a keyword or a string)."
  (or (keywordp thing)
      (stringp thing)))

(defun alist? (thing)
  "Checks if THING is an association list (alist).
   An alist is a list where each element is a cons cell, and the car
   of each cons cell is a key (keyword or string)."
  (or (and (consp thing)
           (every (lambda (entry)
                    (and (consp entry)
                         (key? (car entry))))
                  thing))
      (null thing)))

(defun object-ref-function (keyword string)
  "Returns a function that, when called with an alist or hash table,
   retrieves the value associated with the given KEYWORD or STRING key.
   Prioritizes keyword lookup if both are present."
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
  "Returns a predicate that checks if THING is a list where all elements
   satisfy the provided PREDICATE."
  (lambda (thing)
    (and (consp thing)
         (every predicate thing))))

(defun singleton-list-of-test (predicate)
  "Returns a predicate that checks if THING is a list containing exactly
   one element, and that element satisfies the provided PREDICATE."
  (lambda (thing)
    (and (consp thing)
         (null (cdr thing))
         (funcall predicate (car thing)))))

(defun key-test (keyword string)
  "Returns a predicate function that checks if its argument is equal to
   either KEYWORD (if it's a keyword) or STRING (if it's a string)."
  (lambda (thing)
    (or (and (keywordp thing) (eq thing keyword))
        (and (stringp thing) (equal thing string)))))

(defun required-key-test (required-key)
  "Returns a predicate that checks if a given object (alist or hash table)
   contains the specified REQUIRED-KEY (provided as a list of keyword and string)."
  (let ((has-required-key? (key-test (first required-key) (second required-key))))
    (lambda (thing)
      (find-if has-required-key? (keys thing)))))

(defun required-keys-test (required-keys)
  "Returns a predicate that checks if a given object (alist or hash table)
   contains ALL the keys specified in REQUIRED-KEYS.
   REQUIRED-KEYS is a list of (keyword string) pairs."
  (let ((required-key-tests (mapcar #'required-key-test required-keys)))
    (lambda (thing)
      (every (lambda (test)
               (funcall test thing))
             required-key-tests))))

(defun keys (thing)
  "Returns a list of keys from an alist or a hash table.
   Signals an error if THING is neither an alist nor a hash table."
  (cond ((alist? thing) (mapcar #'car thing))
        ((hash-table-p thing) (hash-table-keys thing))
        (t (error "Can't get keys of ~s" thing))))

(defun valid-key-test (keywords strings)
  "Returns a predicate that checks if a key is present in either the
   KEYWORDS list (for keyword keys) or the STRINGS list (for string keys)."
  (lambda (thing)
    (or (and (keywordp thing) (member thing keywords))
        (and (stringp thing) (member thing strings :test 'equal)))))

(defun valid-keys-test (keywords strings)
  "Returns a predicate that checks if all keys in a given object (alist or hash table)
   are present in either the KEYWORDS list or the STRINGS list."
  (let ((valid-key? (valid-key-test keywords strings)))
    (lambda (thing)
      (every valid-key? (keys thing)))))

(defun is-object-test (required-keys keywords strings)
  "Returns a predicate that checks if THING is a valid object.
   A valid object is either an alist or a hash table, contains all
   REQUIRED-KEYS, and all its keys are among the provided KEYWORDS or STRINGS."
  (let ((has-required-keys? (required-keys-test required-keys))
        (are-keys-valid? (valid-keys-test keywords strings)))
    (lambda (thing)
      (and (or (alist? thing)
               (hash-table-p thing))
           (funcall has-required-keys? thing)
           (funcall are-keys-valid? thing)))))

(defvar *cached-content*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*cached-content* 'variable) "Holds a cached content object, if one is bound."))

(defun default-cached-content ()
  "Returns the value of *CACHED-CONTENT* if it is bound, otherwise NIL.
   Provides a default cached content object."
  (when (boundp '*cached-content*)
    *cached-content*))

(defvar *candidate-count*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*candidate-count* 'variable) "Holds the default number of candidates to generate."))

(defun default-candidate-count ()
  "Returns the value of *CANDIDATE-COUNT* if it is bound, otherwise NIL.
   Provides a default candidate count for generation."
  (when (boundp '*candidate-count*)
    *candidate-count*))

(defvar *enable-advanced-civic-answers*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*enable-advanced-civic-answers* 'variable) "Controls whether advanced civic answers are enabled."))

(defun default-enable-advanced-civic-answers ()
  "Returns the value of *ENABLE-ADVANCED-CIVIC-ANSWERS* if it is bound, otherwise NIL.
   Provides a default value for enabling advanced civic answers."
  (when (boundp '*enable-advanced-civic-answers*)
    *enable-advanced-civic-answers*))

(defvar *frequency-penalty*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*frequency-penalty* 'variable) "Controls the frequency penalty for generated tokens."))

(defun default-frequency-penalty ()
  "Returns the value of *FREQUENCY-PENALTY* if it is bound, otherwise NIL.
   Provides a default frequency penalty for generation."
  (when (boundp '*frequency-penalty*)
    *frequency-penalty*))

(defvar *logprobs*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*logprobs* 'variable) "Controls whether log probabilities are returned for generated tokens."))

(defun default-logprobs ()
  "Returns the value of *LOGPROBS* if it is bound, otherwise NIL.
   Provides a default logprobs setting for generation."
  (when (boundp '*logprobs*)
    *logprobs*))

(defvar *max-output-tokens*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*max-output-tokens* 'variable) "Sets the maximum number of output tokens to generate."))

(defun default-max-output-tokens ()
  "Returns the value of *MAX-OUTPUT-TOKENS* if it is bound, otherwise NIL.
   Provides a default maximum output tokens for generation."
  (when (boundp '*max-output-tokens*)
    *max-output-tokens*))

(defvar *media-resolution*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*media-resolution* 'variable) "Sets the desired resolution for media in the response."))

(defun default-media-resolution ()
  "Returns the value of *MEDIA-RESOLUTION* if it is bound, otherwise NIL.
   Provides a default media resolution for generation."
  (when (boundp '*media-resolution*)
    *media-resolution*))

(defvar *presence-penalty*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*presence-penalty* 'variable) "Controls the presence penalty for generated tokens."))

(defun default-presence-penalty ()
  "Returns the value of *PRESENCE-PENALTY* if it is bound, otherwise NIL.
   Provides a default presence penalty for generation."
  (when (boundp '*presence-penalty*)
    *presence-penalty*))

(defvar *response-logprobs*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*response-logprobs* 'variable) "Controls whether response log probabilities are returned."))

(defun default-response-logprobs ()
  "Returns the value of *RESPONSE-LOGPROBS* if it is bound, otherwise NIL.
   Provides a default response logprobs setting for generation."
  (when (boundp '*response-logprobs*)
    *response-logprobs*))

(defvar *response-mime-type*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*response-mime-type* 'variable) "Sets the desired MIME type for the response (e.g., 'application/json')."))

(defun default-response-mime-type ()
  "Returns the value of *RESPONSE-MIME-TYPE* if it is bound, otherwise NIL.
   Provides a default response MIME type for generation."
  (when (boundp '*response-mime-type*)
    *response-mime-type*))

(defvar *response-modalities*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*response-modalities* 'variable) "Sets the desired modalities for the response."))

(defun default-response-modalities ()
  "Returns the value of *RESPONSE-MODALITIES* if it is bound, otherwise NIL.
   Provides a default response modalities object for generation."
  (when (boundp '*response-modalities*)
    *response-modalities*))

(defvar *response-schema*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*response-schema* 'variable) "Sets the desired schema for the response content."))

(defun default-response-schema ()
  "Returns the value of *RESPONSE-SCHEMA* if it is bound, otherwise NIL.
   Provides a default response schema object for generation."
  (when (boundp '*response-schema*)
    *response-schema*))

(defvar *response-json-schema*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*response-json-schema* 'variable) "Sets the desired JSON schema for the response content."))

(defun default-response-json-schema ()
  "Returns the value of *RESPONSE-JSON-SCHEMA* if it is bound, otherwise NIL.
   Provides a default response JSON schema object for generation."
  (when (boundp '*response-json-schema*)
    *response-json-schema*))

(defvar *seed*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*seed* 'variable) "Sets the random seed for deterministic generation."))

(defun default-seed ()
  "Returns the value of *SEED* if it is bound, otherwise NIL.
   Provides a default seed value for generation."
  (when (boundp '*seed*)
    *seed*))

(defvar *language-code*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*language-code* 'variable) "Sets the language code for speech synthesis (e.g., 'en-US')."))

(defun default-language-code ()
  "Returns the value of *LANGUAGE-CODE* if it is bound, otherwise NIL.
   Provides a default language code for speech configuration."
  (when (boundp '*language-code*)
    *language-code*))

(defvar *multi-speaker-voice-config*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*multi-speaker-voice-config* 'variable) "Sets the multi-speaker voice configuration for speech synthesis."))

(defun default-multi-speaker-voice-config ()
  "Returns the value of *MULTI-SPEAKER-VOICE-CONFIG* if it is bound, otherwise NIL.
   Provides a default multi-speaker voice configuration object."
  (when (boundp '*multi-speaker-voice-config*)
    *multi-speaker-voice-config*))

(defvar *voice-name*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*voice-name* 'variable) "Sets the name of the voice to use for speech synthesis."))

(defun default-voice-name ()
  "Returns the value of *VOICE-NAME* if it is bound, otherwise NIL.
   Provides a default voice name for speech configuration."
  (when (boundp '*voice-name*)
    *voice-name*))

(defvar *prebuilt-voice-config*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*prebuilt-voice-config* 'variable) "Holds a prebuilt voice configuration object for speech synthesis."))

(defun default-prebuilt-voice-config ()
  "Returns a default prebuilt voice configuration object.
   It constructs a hash table with 'voiceName' if *VOICE-NAME* is set."
  (if (boundp '*prebuilt-voice-config*)
      *prebuilt-voice-config*
      (let ((prebuilt-voice-config (make-hash-table :test 'equal)))
        (let ((voice-name (default-voice-name)))
          (when voice-name
            (setf (gethash "voiceName" prebuilt-voice-config) voice-name)))
        (unless (zerop (hash-table-count prebuilt-voice-config))
          prebuilt-voice-config))))

(defvar *voice-config*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*voice-config* 'variable) "Holds the overall voice configuration for speech synthesis."))

(defun default-voice-config ()
  "Returns a default voice configuration object.
   It constructs a hash table with 'prebuiltVoiceConfig' if
   DEFAULT-PREBUILT-VOICE-CONFIG returns a value."
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*speech-config* 'variable) "Holds the overall speech configuration for generation."))

(defun default-speech-config ()
  "Returns a default speech configuration object.
   It constructs a hash table with 'languageCode', 'multiSpeakerVoiceConfig',
   and 'voiceConfig' based on their respective default functions."
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*stop-sequences* 'variable) "A list of sequences that will cause the generation to stop."))

(defun default-stop-sequences ()
  "Returns the value of *STOP-SEQUENCES* if it is bound, otherwise NIL.
   Provides a default list of stop sequences for generation."
  (when (boundp '*stop-sequences*)
    *stop-sequences*))

(defvar *temperature*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*temperature* 'variable) "Controls the randomness of the output. Higher values mean more random."))

(defun default-temperature ()
  "Returns the value of *TEMPERATURE* if it is bound, otherwise NIL.
   Provides a default temperature value for generation."
  (when (boundp '*temperature*)
    *temperature*))

(defvar *include-thoughts*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*include-thoughts* 'variable) "Controls whether internal thoughts of the model are included in the response."))

(defun default-include-thoughts ()
  "Returns the value of *INCLUDE-THOUGHTS* if it is bound, otherwise NIL.
   Provides a default value for including thoughts in the response."
  (when (boundp '*include-thoughts*)
    *include-thoughts*))

(defvar *thinking-budget*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*thinking-budget* 'variable) "Sets the budget for the model's 'thinking' process."))

(defun default-thinking-budget ()
  "Returns the value of *THINKING-BUDGET* if it is bound, otherwise NIL.
   Provides a default value for the thinking budget."
  (when (boundp '*thinking-budget*)
    *thinking-budget*))

(defvar *thinking-config*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*thinking-config* 'variable) "Holds the configuration for the model's thinking process."))

(defun default-thinking-config ()
  "Returns a default thinking configuration object.
   It constructs a hash table with 'includeThoughts' and 'thinkingBudget'
   based on their respective default functions."
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*top-k* 'variable) "Controls the number of top-k tokens to consider for sampling."))

(defun default-top-k ()
  "Returns the value of *TOP-K* if it is bound, otherwise NIL.
   Provides a default top-k value for generation."
  (when (boundp '*top-k*)
    *top-k*))

(defvar *top-p*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*top-p* 'variable) "Controls the cumulative probability threshold for sampling tokens."))

(defun default-top-p ()
  "Returns the value of *TOP-P* if it is bound, otherwise NIL.
   Provides a default top-p value for generation."
  (when (boundp '*top-p*)
    *top-p*))

(defvar *tools*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*tools* 'variable) "A list of tools available to the model for function calling."))

(defun default-tools ()
  "Returns the value of *TOOLS* if it is bound, otherwise NIL.
   Provides a default tools object for generation."
  (when (boundp '*tools*)
    *tools*))

(defvar *tool-config*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*tool-config* 'variable) "Configuration for how the model uses tools."))

(defun default-tool-config ()
  "Returns the value of *TOOL-CONFIG* if it is bound, otherwise NIL.
   Provides a default tool configuration object for generation."
  (when (boundp '*tool-config*)
    *tool-config*))

;; Default generation configuration object
(defvar *generation-config*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*generation-config* 'variable) "Holds the overall generation configuration for the model."))

(defun default-generation-config ()
  "Returns a default generation configuration object.
   It constructs a hash table by combining various default settings
   related to candidate generation, safety, and response formatting."
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*safety-settings* 'variable) "Holds safety settings to control content generation."))

(defun default-safety-settings ()
  "Returns the value of *SAFETY-SETTINGS* if it is bound, otherwise NIL.
   Provides default safety settings for generation."
  (if (boundp '*safety-settings*)
      *safety-settings*
      nil))

(defvar *system-instruction*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*system-instruction* 'variable) "Holds a system instruction for the model to follow."))

(defun default-system-instruction ()
  "Returns the value of *SYSTEM-INSTRUCTION* if it is bound, otherwise NIL.
   Provides a default system instruction for generation."
  (if (boundp '*system-instruction*)
      *system-instruction*
      nil))

(defun content (&key role (parts (error "Content must contain PARTS.")))
  "Creates a content object with the specified ROLE and PARTS.
   Returns a hash table representing the content structure for API requests."
  (let ((content (make-hash-table :test 'equal)))
    (when role
      (setf (gethash "role" content) role))
    (setf (gethash "parts" content) parts)
    content))

(deff content?
  (is-object-test '((:content "content"))
                  '(:content :finish-reason :index)
                  '("content" "finishReason" "index"))
  "Predicate to check if a thing is a valid content object from the API response.")

(deff list-of-candidates? (list-of-test #'candidate?)
  "Predicate to check if a thing is a list of candidate objects.")

(deff singleton-list-of-candidates? (singleton-list-of-test #'candidate?)
  "Predicate to check if a thing is a singleton list of a candidate object.")

(deff get-candidates (object-ref-function :candidates "candidates")
  "Retrieves the 'candidates' field from an object.")

(deff get-content (object-ref-function :content "content")
  "Retrieves the 'content' field from an object.")

(deff get-finish-reason (object-ref-function :finish-reason "finishReason")
  "Retrieves the 'finishReason' field from an object.")

(deff get-parts (object-ref-function :parts "parts")
  "Retrieves the 'parts' field from an object.")

(deff get-role (object-ref-function :role "role")
  "Retrieves the 'role' field from an object.")

(deff get-text (object-ref-function :text "text")
  "Retrieves the 'text' field from an object.")

(deff content?
  (is-object-test '((:parts "parts"))
                  '(:parts :role)
                  '("parts" "role"))
  "Predicate to check if a thing is a valid content object.")

(deff list-of-content? (list-of-test #'content?)
  "Predicate to check if a thing is a list of content objects.")

(deff singleton-list-of-content? (singleton-list-of-test #'content?)
    "Predicate to check if a thing is a singleton list of a content object.")

(deff contents?
    (is-object-test '((:content "content"))
                    '(:content :finish-reason :index)
                    '("content" "finishReason" "index")))

(deff blob?
  (is-object-test '((:data "data")
                    (:mime-type "mimeType"))
                  '(:data :mime-type)
                  '("data" "mimeType"))
  "Predicate to check if a thing is a valid blob object (inline data).")

(deff function-call?
  (is-object-test '((:name "name"))
                  '(:args :id :name)
                  '("args" "id" "name"))
  "Predicate to check if a thing is a valid function call object.")

(deff function-response?
  (is-object-test '((:name "name")
                    (:response "response"))
                  '(:id :name :response :scheduling :will-continue)
                  '("id" "name" "response" "scheduling" "willContinue"))
  "Predicate to check if a thing is a valid function response object.")

(deff file-data?
  (is-object-test '((:file-uri "fileUri"))
                  '(:file-uri :mime-type)
                  '("fileUrl" "mimeType"))
  "Predicate to check if a thing is a valid file data object.")

(deff executable-code?
  (is-object-test '((:code "code")
                    (:language "language"))
                  '(:code :language)
                  '("code" "language"))
  "Predicate to check if a thing is a valid executable code object.")

(deff code-execution-result?
  (is-object-test '((:outcome "outcome"))
                  '(:outcome :output)
                  '("outcome" "output"))
  "Predicate to check if a thing is a valid code execution result object.")

(defun part (data &key metadata thought thought-signature)
  "Creates a part object for content. The DATA can be a string (text),
   a blob, a function call, a function response, file data, executable code,
   or a code execution result. Optional METADATA, THOUGHT, and THOUGHT-SIGNATURE
   can be included. Returns a hash table representing the part."
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

(deff text-object?
  (is-object-test '((:text "text"))
                  '(:text)
                  '("text"))
  "Predicate to check if a thing is a text part object.")

(deff inline-data-object?
  (is-object-test '((:inline-data "inlineData"))
                  '(:inline-data)
                  '("inlineData"))
  "Predicate to check if a thing is an inline data part object.")

(deff function-call-object?
  (is-object-test '((:function-call "functionCall"))
                  '(:function-call)
                  '("functionCall"))
  "Predicate to check if a thing is a function call part object.")

(deff function-response-object?
  (is-object-test '((:function-response "functionResponse"))
                  '(:function-response)
                  '("functionResponse"))
  "Predicate to check if a thing is a function response part object.")

(deff file-data-object?
  (is-object-test '((:file-data "fileData"))
                  '(:file-data)
                  '("fileData"))
  "Predicate to check if a thing is a file data part object.")

(deff executable-code-object?
  (is-object-test '((:executable-code "executableCode"))
                  '(:executable-code)
                  '("executableCode"))
  "Predicate to check if a thing is an executable code part object.")

(deff code-execution-result-object?
  (is-object-test '((:code-execution-result "codeExecutionResult"))
                  '(:code-execution-result)
                  '("codeExecutionResult"))
  "Predicate to check if a thing is a code execution result part object.")

(deff part?
  (lambda (thing)
    (or (text-object? thing)
        (inline-data-object? thing)
        (function-call-object? thing)
        (function-response-object? thing)
        (file-data-object? thing)
        (executable-code-object? thing)
        (code-execution-result-object? thing)))
  "Predicate to check if a thing is any valid part object.")

(deff list-of-parts? (list-of-test #'part?)
  "Predicate to check if a thing is a list of part objects.")

(deff singleton-list-of-parts? (singleton-list-of-test #'part?)
  "Predicate to check if a thing is a singleton list of a part object.")

(deff list-of-strings? (list-of-test #'stringp)
    "Predicate to check if a thing is a list of strings.")

(deff gemini-response? (is-object-test '((:candidates "candidates"))
                                       '(:candidates :model-version :response-id :usage-metadata)
                                       '("candidates" "modelVersion" "responseId" "usageMetadata"))
    "Predicate to check if thing is a gemini response.")

(defun default-process-part (part)
  "Processes a single part object. If it's a text object, it extracts
   and returns the text. Otherwise, it returns the part as is."
  (cond ((text-object? part) (get-text part))
        (t part)))

(defun default-process-content (content)
  "Processes a content object. If the role is 'model' and it contains
   a single part, it processes that part. Otherwise, it returns the
   content as is."
  (if (equal (get-role content) "model")
      (let ((parts (get-parts content)))
        (if (singleton-list-of-parts? parts)
            (default-process-part (first parts))
            parts))
      content))

(defun default-process-candidate (candidate)
  "Processes a candidate object from the API response.
   Asserts that the 'finishReason' is 'STOP'.
   Then processes the content of the candidate."
  (unless (equal (get-finish-reason candidate) "STOP")
    (error "Invalid finish reason: ~s" candidate))
  (default-process-content (get-content candidate)))

(defun default-process-response (response)
  "Processes an API response object.
   If the response contains one candidate, process that candidate.
   Otherwise return the list of candidates."
  (if (gemini-response? response)
      (let ((candidates (get-candidates response)))
        (if (singleton-list-of-candidates? candidates)
            (default-process-candidate (car candidates))
            candidates))
      (error "Unrecognized Gemini response ~s" response)))

(defvar *gemini-output-processor* #'default-process-result
  "Function to process the output of the Gemini API.
   Can be set to a custom function to handle the response differently.
   Defaults to DEFAULT-PROCESS-result.")

(defun invoke-gemini (model-id contents &key
                      (cached-content (default-cached-content))
                      (generation-config (default-generation-config))
                      (tools (default-tools))
                      (tool-config (default-tool-config))
                      (safety-settings (default-safety-settings))
                      (system-instruction (default-system-instruction)))
  "Invokes the Gemini API with the specified MODEL-ID and CONTENTS.
   Optional arguments allow for custom CACHED-CONTENT, GENERATION-CONFIG,
   TOOLS, TOOL-CONFIG, SAFETY-SETTINGS, and SYSTEM-INSTRUCTION.
   The CONTENTS argument can be a content object, a part object, a string,
   a list of content objects, a list of part objects, or a list of strings.
   Returns the processed response from the API, as determined by
   *GEMINI-OUTPUT-PROCESSOR*."
  (let* ((payload (make-hash-table :test 'equal)))
    (setf (gethash "contents" payload)
          (cond ((content? contents) (list contents))
                ((part? contents) (list (content :parts contents)))
                ((stringp contents) (list (content :parts (list (part contents)))))
                ((list-of-content? contents) contents)
                ((list-of-parts? contents) (list (content :parts contents))) ; Corrected to wrap parts list in a single content object
                ((list-of-strings? contents) (list (content :parts (mapcar #'part contents)))) ; Corrected to mapcar part over strings and wrap in content
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
