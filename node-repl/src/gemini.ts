/**
 * Gemini LLM provider implementation
 */

import { GoogleGenerativeAI, type Content } from '@google/generative-ai';
import { BaseLLMProvider, type ChatMessage, type LLMResponse } from './llm.ts';

export interface GeminiProviderOptions {
  apiKey?: string;
  modelName?: string;
  maxIterations?: number;
}

export class GeminiProvider extends BaseLLMProvider {
  private genAI: GoogleGenerativeAI;
  private model: any;
  private modelName: string;

  constructor(options: GeminiProviderOptions = {}) {
    super({ maxIterations: options.maxIterations });
    const key = options.apiKey ?? process.env.GEMINI_API_KEY;
    if (!key) {
      throw new Error('GEMINI_API_KEY environment variable is required');
    }
    this.genAI = new GoogleGenerativeAI(key);
    this.modelName = options.modelName ?? 'gemini-2.5-flash';
    this.model = this.genAI.getGenerativeModel({
      model: this.modelName,
      systemInstruction: this.systemPrompt
    });
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

