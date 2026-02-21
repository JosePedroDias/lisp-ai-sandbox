# AI Lisp REPL

A little experiment. Here be dragons (the sandbox doesn't protect much)

An AI-powered Common Lisp REPL that connects a large language model (Gemini) to a running SBCL Lisp instance via Swank.
Most code so far was done by Augment with Claude Opus 4.5.

## Prerequisites

- [SBCL](http://www.sbcl.org/) (Steel Bank Common Lisp)
- [Quicklisp](https://www.quicklisp.org/) (installed and configured)
- [Node.js](https://nodejs.org/) (v18 or later recommended)

Only tested on ARM MacbookPro/OSX, but nothing should be tying the solution to this architecture/OS.


## Installation

### 1. Install Node.js dependencies

```bash
cd node-repl
npm install
```

### 2. Configure environment

```bash
cd node-repl
cp .env.example .env
```

Edit `.env` and add your Gemini API key:

```
GEMINI_API_KEY=your-api-key-here
```

Get your API key at: https://aistudio.google.com/app/apikey

## Running

### 1. Start the Lisp sandbox

In one terminal:

```bash
cd lisp-sandbox
./start-sandbox.sh

...
(quit)

ps aux|grep sbcl
killall sbcl
```

This starts SBCL with a Swank server listening on port 4005.

### 2. Start the AI REPL

In another terminal:

```bash
cd node-repl
npm start
```

## Example usage

````
🔌 Connecting to Swank server...
   Host: localhost:4006
Connected to Swank server at localhost:4006
🧠 Initializing Gemini LLM...
   Model: gemini-3-flash-preview

🚀 AI Lisp REPL Started
Type your questions or requests in natural language.
Type "quit" or "exit" to stop.

You: create a function that return a random integer between 1 and 20

🤖 Assistant:
You can use the built-in `random` function. Since `(random 20)` returns an integer from 0 to 19, you simply add 1 to the result to get a range from 1 to 20.

```lisp
(defun random-1-to-20 ()
  "Returns a random integer between 1 and 20 inclusive."
  (1+ (random 20)))
```

To test it, you can call it like this:

```lisp
(random-1-to-20)
```

📝 Executing Lisp code...

> (defun random-1-to-20 ()
  "Returns a random integer between 1 and 20 inclusive."
  (1+ (random 20)))
Ignoring command :new-features
=> RANDOM-1-TO-20
> (random-1-to-20)
=> 15

You: Ignoring command :indentation-update
run that function again

🤖 Assistant:
```lisp
(random-1-to-20)
```

📝 Executing Lisp code...

> (random-1-to-20)
=> 13

You: run the greet function with argument "Jose"

🤖 Assistant:
```lisp
(sandbox:greet "Jose")
```

[Code executed successfully]

📝 Executing Lisp code...

> (sandbox:greet "Jose")
=> Hello, Jose! Welcome to the Lisp sandbox.

You: quit
Goodbye!
Disconnected from Swank server
````

## Configuration

All configuration is done via environment variables in `node-repl/.env`:

| Variable | Default | Description |
|----------|---------|-------------|
| `GEMINI_API_KEY` | (required) | Your Gemini API key |
| `GEMINI_MODEL` | `gemini-2.0-flash` | Gemini model to use |
| `SWANK_HOST` | `localhost` | Swank server host |
| `SWANK_PORT` | `4005` | Swank server port |
| `LISP_PACKAGE` | `COMMON-LISP-USER` | Default Lisp package |

## Project Structure

```
ai-sandbox/
├── lisp-sandbox/           # Common Lisp sandbox
│   ├── start-swank.lisp    # Swank server setup
│   └── start-sandbox.sh    # Startup script
│
└── node-repl/              # Node.js AI REPL
    ├── src/
    │   ├── index.ts        # Main entry point
    │   ├── llm.ts          # Abstract LLM interface
    │   ├── gemini.ts       # Gemini implementation
    │   └── swank.ts        # Swank client wrapper
    ├── .env.example        # Example configuration
    └── package.json
```

## License

MIT
