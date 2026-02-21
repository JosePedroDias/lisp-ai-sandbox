# Agent Context

Guidelines and context for AI agents working on this project.

## Project Overview

AI-powered Common Lisp REPL that connects Gemini LLM to a running SBCL instance via Swank protocol.

## Architecture

```
User Input → Node.js App → Gemini LLM
                ↓
           Swank Client
                ↓ (TCP port 4006)
           SBCL + Swank Server
```

## Key Technical Decisions

### Node.js Native TypeScript (no compilation)
- Using Node.js v25+ with `--experimental-strip-types`
- Local imports must use `.ts` extension
- Type-only imports must use `import type { ... }` syntax
- No ts-node or tsx needed

### Swank Client Usage
- **Do NOT use `client.initialize()`** - it hangs trying to load optional SWANK modules
- Use `client.rex()` directly with `SWANK:EVAL-AND-GRAB-OUTPUT`
- Wrap expressions in `handler-case` to catch errors without triggering SBCL debugger
- The `rex()` result is a paredit AST, not a plain value

### Port Configuration
- Default Swank port is **4006** (not 4005) to avoid conflicts with SLIME
- Both Node.js app and Lisp sandbox read from `node-repl/.env`

## Running

```bash
# Terminal 1: Start Lisp sandbox
./lisp-sandbox/start-sandbox.sh

# Terminal 2: Start REPL
cd node-repl && npm start

# Run tests
cd node-repl && npm test
```

## Testing

- Uses Node.js built-in test runner (`node:test`)
- Test files: `src/*.test.ts`
- Swank tests require the sandbox to be running on port 4006

## File Structure

```
├── AGENT.md                 # This file
├── README.md                # User documentation
├── PLAN.md                  # Original project plan/idea
├── lisp-sandbox/
│   ├── start-sandbox.sh     # Reads .env, starts SBCL
│   └── start-swank.lisp     # Swank server + SANDBOX package
└── node-repl/
    ├── .env                 # Configuration (gitignored)
    ├── .env.example         # Template
    ├── package.json
    ├── tsconfig.json
    └── src/
        ├── index.ts         # Main REPL loop
        ├── llm.ts           # Abstract LLM interface
        ├── gemini.ts        # Gemini implementation
        ├── gemini.test.ts
        ├── swank.ts         # Swank client wrapper
        └── swank.test.ts
```

## Common Issues

### Swank connection hangs
- Check if port 4006 is in use: `lsof -i :4006`
- Kill stale SBCL processes: `pkill sbcl`
- Restart sandbox

### Division by zero / errors hang
- Fixed by wrapping eval in `handler-case` in swank.ts
- Never call Lisp code that triggers debugger without error handling

### Import errors with TypeScript
- Use `.ts` extension for local imports
- Use `import type` for interfaces/types
- Ensure `allowImportingTsExtensions: true` in tsconfig.json

## Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `GEMINI_API_KEY` | required | Gemini API key |
| `GEMINI_MODEL` | `gemini-2.5-flash` | Model name |
| `SWANK_HOST` | `localhost` | Swank server host |
| `SWANK_PORT` | `4006` | Swank server port |
| `LISP_PACKAGE` | `COMMON-LISP-USER` | Default Lisp package |

