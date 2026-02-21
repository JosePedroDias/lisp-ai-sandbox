# AI Lisp REPL

An AI-powered Common Lisp REPL that connects a large language model (Gemini) to a running SBCL instance via Swank.

## Prerequisites

- [SBCL](http://www.sbcl.org/) (Steel Bank Common Lisp)
- [Quicklisp](https://www.quicklisp.org/) (installed and configured)
- [Node.js](https://nodejs.org/) (v18 or later recommended)

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
```

This starts SBCL with a Swank server listening on port 4005.

### 2. Start the AI REPL

In another terminal:

```bash
cd node-repl
npm start
```

## Usage

Once running, type natural language requests and the AI will generate and execute Lisp code:

```
You: Calculate the factorial of 10

🤖 Assistant:
I'll calculate the factorial of 10 for you.

(sandbox:factorial 10)

📝 Executing Lisp code...

> (sandbox:factorial 10)
=> 3628800
```

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