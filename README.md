# Common Lisp library to access Google Gemini LLM APIs

This library provides a Common Lisp interface to Google's Gemini Large Language Models.

## Setting your GOOGLE_API_KEY API key

You can obtain an API key from the [Google AI Studio](https://aistudio.google.com/app/apikey).

Put your default project in `~/.config/googleapis/default-project` and put your
API key in `~/.config/googleapis/{default-project}/apikey`.

or

Put your API key in `~/.config/googleapis/default-api-key`

or

Define the `GOOGLE_API_KEY` environment variable with the value of your API key. 

## Dependencies

This library depends on:
- `alexandria`
- `cl-json`
- `dexador`
- `function`
- `fold`
- `named-let`
- `series`
- `uiop`

Ensure these have been quickloaded or are available in your Quicklisp local-projects or via ASDF.

## Usage

Load the library using Quicklisp or ASDF:
```common-lisp
(ql:quickload "gemini")
;; or
(asdf:load-system "gemini")
```

### Basic use

To generate text from a prompt:
```common-lisp
(gemini:invoke-gemini "gemini-2.5-flash" "In one sentence, explain how AI works to a child.")
;; => "AI is like a super smart computer brain that learns from information to answer questions and do tasks."
```
You can use other models like `"gemini-2.5-pro"` as the first argument.

