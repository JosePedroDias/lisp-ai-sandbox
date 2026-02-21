/**
 * Gemini LLM provider implementation
 */

import { GoogleGenerativeAI, type Content } from '@google/generative-ai';
import { BaseLLMProvider, type ChatMessage, type LLMResponse } from './llm.ts';

export class GeminiProvider extends BaseLLMProvider {
  private genAI: GoogleGenerativeAI;
  private model: any;
  private modelName: string;

  constructor(apiKey?: string, modelName: string = 'gemini-2.5-flash') {
    super();
    const key = apiKey ?? process.env.GEMINI_API_KEY;
    if (!key) {
      throw new Error('GEMINI_API_KEY environment variable is required');
    }
    this.genAI = new GoogleGenerativeAI(key);
    this.modelName = modelName;
    this.model = this.genAI.getGenerativeModel({ 
      model: this.modelName,
      systemInstruction: this.systemPrompt
    });
  }

  protected getDefaultSystemPrompt(): string {
    return `You are a helpful Common Lisp programming assistant connected to a live SBCL REPL via Swank.

Your role is to help users write and understand Lisp code. When asked to perform tasks:
1. Write valid Common Lisp S-expressions
2. Wrap executable code in \`\`\`lisp code blocks
3. Be concise and helpful
4. Explain what the code does when appropriate

The user's code will be evaluated in a live Lisp environment with access to:
- Standard Common Lisp functions
- The SANDBOX package with: greet, add-numbers, factorial, fibonacci

When showing results or explaining errors, be clear and educational.`;
  }

  /**
   * Convert our message format to Gemini's format
   */
  private toGeminiMessages(messages: ChatMessage[]): Content[] {
    return messages.map(msg => ({
      role: msg.role === 'assistant' ? 'model' : 'user',
      parts: [{ text: msg.content }]
    }));
  }

  async chat(messages: ChatMessage[]): Promise<LLMResponse> {
    const geminiMessages = this.toGeminiMessages(messages);
    
    // Start a chat session
    const chat = this.model.startChat({
      history: geminiMessages.slice(0, -1)
    });

    // Send the last message
    const lastMessage = messages[messages.length - 1];
    const result = await chat.sendMessage(lastMessage.content);
    const response = await result.response;
    const text = response.text();

    return {
      content: text,
      raw: response
    };
  }

  async complete(prompt: string): Promise<string> {
    const result = await this.model.generateContent(prompt);
    const response = await result.response;
    return response.text();
  }
}

