/**
 * AI-powered Common Lisp REPL
 *
 * Connects an LLM (Gemini) to a running Swank server, allowing natural
 * language interaction with a live Common Lisp environment.
 */

import 'dotenv/config';
import * as readline from 'readline';
import { SwankConnection } from './swank.ts';
import { GeminiProvider } from './gemini.ts';
import type { ChatMessage, LLMProvider } from './llm.ts';

// ANSI color codes
const colors = {
  dim: '\x1b[2m',
  cyan: '\x1b[36m',
  yellow: '\x1b[33m',
  magenta: '\x1b[35m',
  green: '\x1b[32m',
  reset: '\x1b[0m'
};

// Parse command line arguments
const args = process.argv.slice(2);
const showContext = args.includes('--show-context');

// Default max iterations for agentic loop
const DEFAULT_MAX_ITERATIONS = 5;
const maxIterationsArg = args.find(a => a.startsWith('--max-iterations='));
const maxIterations = maxIterationsArg
  ? parseInt(maxIterationsArg.split('=')[1], 10)
  : DEFAULT_MAX_ITERATIONS;

// Configuration from environment
const config = {
  swank: {
    host: process.env.SWANK_HOST ?? 'localhost',
    port: parseInt(process.env.SWANK_PORT ?? '4005', 10),
    package: process.env.LISP_PACKAGE ?? 'COMMON-LISP-USER'
  },
  gemini: {
    apiKey: process.env.GEMINI_API_KEY,
    model: process.env.GEMINI_MODEL ?? 'gemini-2.0-flash'
  }
};

/**
 * Extract Lisp code blocks from LLM response
 */
function extractLispCode(text: string): string[] {
  const codeBlockRegex = /```lisp\n([\s\S]*?)```/g;
  const matches: string[] = [];
  let match;
  while ((match = codeBlockRegex.exec(text)) !== null) {
    matches.push(match[1].trim());
  }
  return matches;
}

/**
 * Colorize Lisp code blocks in text
 */
function colorizeLispBlocks(text: string): string {
  return text.replace(
    /```lisp\n([\s\S]*?)```/g,
    `${colors.green}\`\`\`lisp\n$1\`\`\`${colors.reset}`
  );
}

/**
 * Log the context being sent to the LLM
 */
function logContext(history: ChatMessage[]): void {
  console.log(`\n${colors.dim}╭─── Context being sent to LLM ───${colors.reset}`);
  for (const msg of history) {
    const roleColor = msg.role === 'user' ? colors.cyan :
                      msg.role === 'assistant' ? colors.magenta : colors.yellow;
    const roleLabel = msg.role.toUpperCase();
    const content = msg.content.length > 200
      ? msg.content.slice(0, 200) + '...'
      : msg.content;
    console.log(`${colors.dim}│ ${roleColor}[${roleLabel}]${colors.reset}${colors.dim}`);
    for (const line of content.split('\n')) {
      console.log(`│   ${line}`);
    }
    console.log(`│${colors.reset}`);
  }
  console.log(`${colors.dim}╰──────────────────────────────────${colors.reset}\n`);
}

/**
 * Main AI REPL class
 */
class AILispRepl {
  private swank: SwankConnection;
  private llm: LLMProvider;
  private history: ChatMessage[] = [];
  private rl: readline.Interface;
  private showContext: boolean;
  private maxIterations: number;

  constructor(
    swank: SwankConnection,
    llm: LLMProvider,
    options: { showContext?: boolean; maxIterations?: number } = {}
  ) {
    this.swank = swank;
    this.llm = llm;
    this.showContext = options.showContext ?? false;
    this.maxIterations = options.maxIterations ?? DEFAULT_MAX_ITERATIONS;
    this.rl = readline.createInterface({
      input: process.stdin,
      output: process.stdout
    });
  }

  /**
   * Execute code blocks and return formatted results
   */
  private async executeCodeBlocks(codeBlocks: string[]): Promise<string[]> {
    console.log('\n📝 Executing Lisp code...\n');
    const executionResults: string[] = [];

    for (const code of codeBlocks) {
      console.log(`> ${code}`);
      const result = await this.swank.eval(code);

      if (result.success) {
        console.log(`=> ${result.output}`);
        executionResults.push(`> ${code}\n=> ${result.output}`);
      } else {
        console.log(`❌ Error: ${result.error}`);
        executionResults.push(`> ${code}\n❌ Error: ${result.error}`);
      }
    }

    return executionResults;
  }

  /**
   * Process user input and generate response (agentic loop)
   */
  async processInput(input: string): Promise<void> {
    // Add user message to history
    this.history.push({ role: 'user', content: input });

    let iteration = 0;
    let hasCodeToExecute = true;

    try {
      while (hasCodeToExecute && iteration < this.maxIterations) {
        iteration++;

        // Show context if enabled
        if (this.showContext) {
          logContext(this.history);
        }

        // Get LLM response
        const response = await this.llm.chat(this.history);

        if (iteration > 1) {
          console.log(`\n🔄 Agent continuing (iteration ${iteration}/${this.maxIterations})...`);
        }
        console.log('\n🤖 Assistant:\n' + colorizeLispBlocks(response.content));

        // Extract and execute any Lisp code
        const codeBlocks = extractLispCode(response.content);

        if (codeBlocks.length > 0) {
          const executionResults = await this.executeCodeBlocks(codeBlocks);

          // Add assistant response to history
          this.history.push({
            role: 'assistant',
            content: response.content
          });

          // Add execution results as a separate user message so LLM sees them
          this.history.push({
            role: 'user',
            content: `[Execution results]\n${executionResults.join('\n\n')}`
          });

          // Continue the loop - LLM might want to do more
          hasCodeToExecute = true;
        } else {
          // No code blocks - LLM is done, exit loop
          this.history.push({ role: 'assistant', content: response.content });
          hasCodeToExecute = false;
        }
      }

      if (iteration >= this.maxIterations && hasCodeToExecute) {
        console.log(`\n⚠️  Reached max iterations (${this.maxIterations}). Stopping agent loop.`);
      }
    } catch (error) {
      console.error('Error:', error instanceof Error ? error.message : error);
    }
  }

  /**
   * Start the REPL loop
   */
  async start(): Promise<void> {
    console.log('\n🚀 AI Lisp REPL Started');
    console.log('Type your questions or requests in natural language.');
    console.log('Type "quit" or "exit" to stop.\n');

    const prompt = (): void => {
      this.rl.question('You: ', async (input) => {
        const trimmed = input.trim();
        
        if (trimmed.toLowerCase() === 'quit' || trimmed.toLowerCase() === 'exit') {
          console.log('Goodbye!');
          this.rl.close();
          this.swank.disconnect();
          process.exit(0);
        }

        if (trimmed) {
          await this.processInput(trimmed);
        }
        
        console.log('');
        prompt();
      });
    };

    prompt();
  }
}

/**
 * Main entry point
 */
async function main(): Promise<void> {
  console.log('🔌 Connecting to Swank server...');
  console.log(`   Host: ${config.swank.host}:${config.swank.port}`);

  const swank = new SwankConnection({
    host: config.swank.host,
    port: config.swank.port,
    package: config.swank.package
  });

  try {
    await swank.connect();
  } catch (error) {
    console.error('Failed to connect to Swank server.');
    console.error('Make sure the Lisp sandbox is running:');
    console.error('  cd lisp-sandbox && ./start-sandbox.sh');
    process.exit(1);
  }

  console.log('🧠 Initializing Gemini LLM...');
  console.log(`   Model: ${config.gemini.model}`);
  console.log(`   Max iterations: ${maxIterations}`);
  if (showContext) {
    console.log('   Context logging: enabled');
  }

  const llm = new GeminiProvider({
    apiKey: config.gemini.apiKey,
    modelName: config.gemini.model,
    maxIterations
  });

  const repl = new AILispRepl(swank, llm, { showContext, maxIterations });
  await repl.start();
}

main().catch(console.error);

