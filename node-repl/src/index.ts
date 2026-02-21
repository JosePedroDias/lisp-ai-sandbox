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
 * Main AI REPL class
 */
class AILispRepl {
  private swank: SwankConnection;
  private llm: LLMProvider;
  private history: ChatMessage[] = [];
  private rl: readline.Interface;

  constructor(swank: SwankConnection, llm: LLMProvider) {
    this.swank = swank;
    this.llm = llm;
    this.rl = readline.createInterface({
      input: process.stdin,
      output: process.stdout
    });
  }

  /**
   * Process user input and generate response
   */
  async processInput(input: string): Promise<void> {
    // Add user message to history
    this.history.push({ role: 'user', content: input });

    try {
      // Get LLM response
      const response = await this.llm.chat(this.history);
      console.log('\n🤖 Assistant:\n' + response.content);

      // Extract and execute any Lisp code
      const codeBlocks = extractLispCode(response.content);
      
      if (codeBlocks.length > 0) {
        console.log('\n📝 Executing Lisp code...\n');
        
        for (const code of codeBlocks) {
          console.log(`> ${code}`);
          const result = await this.swank.eval(code);
          
          if (result.success) {
            console.log(`=> ${result.output}`);
          } else {
            console.log(`❌ Error: ${result.error}`);
          }
        }

        this.history.push({
          role: 'assistant',
          content: response.content + '\n\n[Code executed successfully]'
        });
      } else {
        this.history.push({ role: 'assistant', content: response.content });
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

  const llm = new GeminiProvider(config.gemini.apiKey, config.gemini.model);

  const repl = new AILispRepl(swank, llm);
  await repl.start();
}

main().catch(console.error);

