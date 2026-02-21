/**
 * Abstract LLM interface for AI request/response
 */

export interface ChatMessage {
  role: 'user' | 'assistant' | 'system';
  content: string;
}

export interface LLMResponse {
  content: string;
  raw?: unknown;
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
}

/**
 * Abstract base class for LLM providers with common functionality
 */
export abstract class BaseLLMProvider implements LLMProvider {
  protected systemPrompt: string;

  constructor(systemPrompt?: string) {
    this.systemPrompt = systemPrompt ?? this.getDefaultSystemPrompt();
  }

  protected getDefaultSystemPrompt(): string {
    return `You are a helpful Common Lisp programming assistant. 
You help users write and understand Lisp code.
When asked to write code, respond with valid Common Lisp S-expressions.
Wrap any code you want executed in \`\`\`lisp code blocks.
Be concise and helpful.`;
  }

  abstract chat(messages: ChatMessage[]): Promise<LLMResponse>;
  
  async complete(prompt: string): Promise<string> {
    const response = await this.chat([
      { role: 'user', content: prompt }
    ]);
    return response.content;
  }
}

