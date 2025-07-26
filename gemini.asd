;;;; gemini.asd

(defsystem "gemini"
  :description "API to Google's Gemini LLM"
  :author "Joe Marshall"
  :license "MIT"
  :depends-on ("alexandria" "cl-json" "dexador" "fold" "function" "named-let" "uiop")
  :components ((:file "gemini" :depends-on ("package"))
               (:file "package")))
