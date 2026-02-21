# IDEA

The idea here is to use nodejs to glue an LLM REPL with a swank connection to a running common lisp image.

Let's abstract the AI request/response as an abstract async function that will be implemented (at last to gemini to use ATM).

The interaction of the LLM to the common lisp image will be done via swank. We can use this dependency or others if it helps.
https://github.com/neil-lindquist/swank-client

We can also provide the simplest sandbox common lisp module/image for sbcl to run with swank server, as described here: https://lispcookbook.github.io/cl-cookbook/debugging.html#remote-debugging

Let's focus first on the more experimental and challenging parts: setting up the JsToSwank bridge, and then the smallest LispSandbox to start from. The llmReqResp should be trivial to setup if/when the rest works.


# DIAGRAM

```
┌──────────────┐    ┌─────────────┐    ┌───────────────┐                         
│              ├───►│             ├───►│               │                         
│  LlmReqResp  │    │  JsToSwank  │    │  LispSandbox  │                         
│              │◄───┤             │◄───┤               │                         
└──────────────┘    └─────────────┘    └───────────────┘                         
                                       (has swank server running)                
                                       (may load quicklisp dependencies at start)
```

# TOOLS

- you do have sbcl installed, along with quicklisp
- you do have nodejs installed
