/**
 * Abstract LLM interface for AI request/response
 */

import * as fs from 'node:fs';
import * as path from 'node:path';
import { fileURLToPath } from 'node:url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

export interface ChatMessage {
  role: 'user' | 'assistant' | 'system';
  content: string;
}

export interface LLMResponse {
  content: string;
  raw?: unknown;
}

/**
 * Token usage metadata from an LLM interaction
 */
export interface TokenUsage {
  promptTokenCount: number;
  candidatesTokenCount: number;
  totalTokenCount: number;
  cachedContentTokenCount?: number;
  timestamp: Date;
}

export interface SandboxFunction {
  name: string;
  args: string;
  docstring: string;
}

/**
 * Parse functions from a single Lisp file
 */
function parseLispFile(filePath: string, skipFunctions: string[] = []): SandboxFunction[] {
  const content = fs.readFileSync(filePath, 'utf-8');
  const functions: SandboxFunction[] = [];

  // Match (defun name (args) "docstring" ...)
  // Captures: name, args, docstring
  const defunRegex = /\(defun\s+([a-z-]+)\s+\(([^)]*)\)\s*\n\s*"([^"]+)"/g;

  let match;
  while ((match = defunRegex.exec(content)) !== null) {
    const [, name, args, docstring] = match;
    if (skipFunctions.includes(name)) {
      continue;
    }
    functions.push({
      name,
      args: args.trim(),
      docstring: docstring.split('\n')[0].trim() // First line only
    });
  }

  return functions;
}

/**
 * Parse sandbox functions from all Lisp sandbox files
 */
export function parseSandboxFunctions(): SandboxFunction[] {
  const lispDir = path.resolve(__dirname, '../../lisp-sandbox');
  const files = ['demo.lisp', 'tools.lisp'];
  const skipFunctions = ['ensure-sandbox-dir', 'sandbox-filepath', 'start-swank-server'];

  const allFunctions: SandboxFunction[] = [];

  for (const file of files) {
    const filePath = path.join(lispDir, file);
    if (fs.existsSync(filePath)) {
      allFunctions.push(...parseLispFile(filePath, skipFunctions));
    }
  }

  return allFunctions;
}

/**
 * Format sandbox functions for the system prompt
 */
export function formatSandboxFunctions(functions: SandboxFunction[]): string {
  return functions.map(fn => {
    const args = fn.args ? ` ${fn.args}` : '';
    return `- (sandbox:${fn.name}${args}) - ${fn.docstring}`;
  }).join('\n');
}

/**
 * Build the system prompt from template
 */
export function buildSystemPrompt(options: { maxIterations?: number } = {}): string {
  const templatePath = path.resolve(__dirname, 'system-prompt.txt');
  const template = fs.readFileSync(templatePath, 'utf-8');

  const functions = parseSandboxFunctions();
  const formattedFunctions = formatSandboxFunctions(functions);
  const maxIterations = options.maxIterations ?? 5;

  return template
    .replace('{{SANDBOX_FUNCTIONS}}', formattedFunctions)
    .replace('{{MAX_ITERATIONS}}', String(maxIterations));
}

/**
 * Abstract interface for LLM providers
 */
export interface LLMProvider {
  /**
   * Send a chat message and get a response
   */
  chat(messages: ChatMessage[]): Promise<LLMResponse>;

  /**
   * Simple single-turn completion
   */
  complete(prompt: string): Promise<string>;

  /**
   * Get the history of token usage from all interactions
   */
  getTokenUsageHistory?(): TokenUsage[];

  /**
   * Get the most recent token usage
   */
  getLastTokenUsage?(): TokenUsage | undefined;
}

export interface LLMProviderOptions {
  systemPrompt?: string;
  maxIterations?: number;
}

/**
 * Abstract base class for LLM providers with common functionality
 */
export abstract class BaseLLMProvider implements LLMProvider {
  protected systemPrompt: string;
  protected maxIterations: number;

  constructor(options: LLMProviderOptions = {}) {
    this.maxIterations = options.maxIterations ?? 5;
    this.systemPrompt = options.systemPrompt ?? this.getDefaultSystemPrompt();
  }

  protected getDefaultSystemPrompt(): string {
    return buildSystemPrompt({ maxIterations: this.maxIterations });
  }

  abstract chat(messages: ChatMessage[]): Promise<LLMResponse>;

  async complete(prompt: string): Promise<string> {
    const response = await this.chat([
      { role: 'user', content: prompt }
    ]);
    return response.content;
  }
}

