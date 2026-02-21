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

# With options
npm start -- --show-context          # Show LLM conversation context
npm start -- --max-iterations=10     # Set agentic loop limit (default: 5)

# Run tests
cd node-repl && npm test
```

## Agentic Loop

The LLM can execute multiple iterations autonomously:
1. User sends input → LLM responds with code
2. Code is executed → results fed back to LLM
3. LLM continues (or stops if no code blocks)
4. Loop until max iterations or LLM finishes

The system prompt informs the LLM of its iteration budget.

## System Prompt Hydration

`system-prompt.txt` is a template with placeholders:
- `{{SANDBOX_FUNCTIONS}}` - Auto-populated from `demo.lisp` and `tools.lisp`
- `{{MAX_ITERATIONS}}` - Set from `--max-iterations` flag
- `{{SANDBOX_FILES}}` - Files in `sandbox-files/` (not loaded yet)

## Testing

- Uses Node.js built-in test runner (`node:test`)
- Test files: `src/*.test.ts`
- Unit tests (`llm.test.ts`): No external services required
- Integration tests: Require Swank server and/or API key

## File Structure

```
├── AGENT.md                 # This file
├── README.md                # User documentation
├── PLAN.md                  # Original project plan/idea
├── lisp-sandbox/
│   ├── start-sandbox.sh     # Reads .env, starts SBCL
│   ├── start-swank.lisp     # Main entry: package definition, loads modules
│   ├── demo.lisp            # Demo functions: greet, factorial, fibonacci
│   ├── tools.lisp           # File operations: list-files, read-file, write-file, load-file
│   └── sandbox-files/       # User files created by the LLM (not loaded by default)
└── node-repl/
    ├── .env                 # Configuration (gitignored)
    ├── .env.example         # Template
    ├── package.json
    ├── tsconfig.json
    └── src/
        ├── index.ts         # Main REPL loop with agentic behavior
        ├── llm.ts           # Abstract LLM interface + prompt building utilities
        ├── gemini.ts        # Gemini implementation with error handling
        ├── swank.ts         # Swank client wrapper
        ├── system-prompt.txt # LLM system prompt template (hydrated at runtime)
        ├── llm.test.ts      # Unit tests for LLM utilities
        ├── gemini.test.ts   # Integration tests for Gemini
        └── swank.test.ts    # Integration tests for Swank
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

### Gemini API errors (500, etc.)
- Preview models (`-preview` suffix) can be unstable
- Enhanced error messages show model, timestamp, and suggestions
- 500 errors are server-side; retry after a moment

## Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `GEMINI_API_KEY` | required | Gemini API key |
| `GEMINI_MODEL` | `gemini-2.5-flash` | Model name |
| `SWANK_HOST` | `localhost` | Swank server host |
| `SWANK_PORT` | `4006` | Swank server port |
| `LISP_PACKAGE` | `COMMON-LISP-USER` | Default Lisp package |

