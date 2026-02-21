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

### Command Line Options

| Flag | Default | Description |
|------|---------|-------------|
| `--show-context` | off | Display the conversation history being sent to the LLM |
| `--max-iterations=N` | 5 | Maximum iterations for the agentic loop before prompting user |

Examples:

```bash
# Show what context is being sent to the LLM
npm start -- --show-context

# Allow up to 10 iterations before prompting
npm start -- --max-iterations=10

# Combine flags
npm start -- --show-context --max-iterations=8
```

## Example usage

````
🔌 Connecting to Swank server...
   Host: localhost:4006
Connected to Swank server at localhost:4006
🧠 Initializing Gemini LLM...
   Model: gemini-2.5-flash
   Max iterations: 5

🚀 AI Lisp REPL Started
Type your questions or requests in natural language.
Type "quit" or "exit" to stop.

You: create a function that returns a random integer between 1 and 20, then test it

🤖 Assistant:
```lisp
(defun random-1-to-20 ()
  "Returns a random integer between 1 and 20 inclusive."
  (1+ (random 20)))
```

📝 Executing Lisp code...

> (defun random-1-to-20 ()
  "Returns a random integer between 1 and 20 inclusive."
  (1+ (random 20)))
=> RANDOM-1-TO-20

🔄 Agent continuing (iteration 2/5)...

🤖 Assistant:
The function is defined. Let me test it:

```lisp
(random-1-to-20)
```

📝 Executing Lisp code...

> (random-1-to-20)
=> 15

🔄 Agent continuing (iteration 3/5)...

🤖 Assistant:
The function works. It returned 15, which is within the expected range of 1 to 20.

You: run the greet function with argument "Jose"

🤖 Assistant:
```lisp
(sandbox:greet "Jose")
```

📝 Executing Lisp code...

> (sandbox:greet "Jose")
=> Hello, Jose! Welcome to the Lisp sandbox.

🔄 Agent continuing (iteration 2/5)...

🤖 Assistant:
The greeting was successful!

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
├── lisp-sandbox/              # Common Lisp sandbox
│   ├── start-swank.lisp       # Main entry: package definition, loads modules
│   ├── demo.lisp              # Demo functions: greet, factorial, fibonacci
│   ├── tools.lisp             # File operations: list-files, read-file, write-file
│   └── start-sandbox.sh       # Startup script
│
└── node-repl/                 # Node.js AI REPL
    ├── src/
    │   ├── index.ts           # Main entry point & agentic loop
    │   ├── llm.ts             # Abstract LLM interface & prompt building
    │   ├── gemini.ts          # Gemini implementation
    │   ├── swank.ts           # Swank client wrapper
    │   ├── system-prompt.txt  # LLM system prompt template
    │   ├── llm.test.ts        # Unit tests for LLM utilities
    │   ├── gemini.test.ts     # Integration tests for Gemini
    │   └── swank.test.ts      # Integration tests for Swank
    ├── .env.example           # Example configuration
    └── package.json
```

## Testing

```bash
cd node-repl

# Run unit tests only (no external services required)
node --experimental-strip-types --test src/llm.test.ts

# Run all tests (requires Swank server and Gemini API key)
npm test
```

## License

MIT
