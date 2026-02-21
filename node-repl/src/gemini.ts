/**
 * Gemini LLM provider implementation
 */

import { GoogleGenerativeAI, type Content, type UsageMetadata } from '@google/generative-ai';
import { BaseLLMProvider, type ChatMessage, type LLMResponse, type TokenUsage } from './llm.ts';

export interface GeminiProviderOptions {
  apiKey?: string;
  modelName?: string;
  maxIterations?: number;
}

export class GeminiProvider extends BaseLLMProvider {
  private genAI: GoogleGenerativeAI;
  private model: any;
  private modelName: string;
  private tokenUsageHistory: TokenUsage[] = [];

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

    try {
      // Start a chat session
      const chat = this.model.startChat({
        history: geminiMessages.slice(0, -1)
      });

      // Send the last message
      const lastMessage = messages[messages.length - 1];
      const result = await chat.sendMessage(lastMessage.content);
      const response = await result.response;
      const text = response.text();

      // Store token usage if available
      this.storeTokenUsage(response.usageMetadata);

      return {
        content: text,
        raw: response
      };
    } catch (error) {
      throw this.enhanceError(error, 'chat');
    }
  }

  async complete(prompt: string): Promise<string> {
    try {
      const result = await this.model.generateContent(prompt);
      const response = await result.response;
      return response.text();
    } catch (error) {
      throw this.enhanceError(error, 'complete');
    }
  }

  /**
   * Enhance error with more details for debugging
   */
  private enhanceError(error: unknown, operation: string): Error {
    const details: string[] = [];
    details.push(`\n❌ Gemini API Error`);
    details.push(`   Operation: ${operation}`);
    details.push(`   Model: ${this.modelName}`);
    details.push(`   Time: ${new Date().toISOString()}`);

    if (error instanceof Error) {
      details.push(`   Message: ${error.message}`);

      // Check for HTTP status in error
      const anyError = error as any;
      if (anyError.status) {
        details.push(`   HTTP Status: ${anyError.status}`);
      }
      if (anyError.statusText) {
        details.push(`   Status Text: ${anyError.statusText}`);
      }
      if (anyError.errorDetails) {
        details.push(`   Details: ${JSON.stringify(anyError.errorDetails, null, 2)}`);
      }

      // Google API specific error properties
      if (anyError.response?.status) {
        details.push(`   Response Status: ${anyError.response.status}`);
      }
      if (anyError.code) {
        details.push(`   Error Code: ${anyError.code}`);
      }

      // Add suggestions based on error type
      const suggestion = this.getSuggestion(error.message);
      if (suggestion) {
        details.push(`\n   💡 ${suggestion}`);
      }

      const enhancedError = new Error(details.join('\n'));
      enhancedError.cause = error;
      return enhancedError;
    }

    details.push(`   Raw error: ${String(error)}`);
    return new Error(details.join('\n'));
  }

  /**
   * Get suggestion based on error message
   */
  private getSuggestion(message: string): string | null {
    if (message.includes('500') || message.includes('Internal')) {
      return 'Server error on Gemini side. Try again in a few moments.';
    }
    if (message.includes('502') || message.includes('503') || message.includes('504')) {
      return 'Gemini service temporarily unavailable. Try again shortly.';
    }
    if (message.includes('429') || message.includes('quota') || message.includes('rate')) {
      return 'Rate limit hit. Wait a moment or check your API quota at https://aistudio.google.com/';
    }
    if (message.includes('401') || message.includes('403') || message.includes('API key')) {
      return 'Authentication error. Check your GEMINI_API_KEY is valid.';
    }
    if (message.includes('404')) {
      return `Model "${this.modelName}" may not exist. Check GEMINI_MODEL setting.`;
    }
    if (message.includes('timeout') || message.includes('ETIMEDOUT')) {
      return 'Request timed out. Check your network connection.';
    }
    return null;
  }

  /**
   * Store token usage from a response
   */
  private storeTokenUsage(usageMetadata: UsageMetadata | undefined): void {
    if (usageMetadata) {
      this.tokenUsageHistory.push({
        promptTokenCount: usageMetadata.promptTokenCount,
        candidatesTokenCount: usageMetadata.candidatesTokenCount,
        totalTokenCount: usageMetadata.totalTokenCount,
        cachedContentTokenCount: usageMetadata.cachedContentTokenCount,
        timestamp: new Date()
      });
    }
  }

  /**
   * Get the history of token usage from all interactions
   */
  getTokenUsageHistory(): TokenUsage[] {
    return [...this.tokenUsageHistory];
  }

  /**
   * Get the most recent token usage
   */
  getLastTokenUsage(): TokenUsage | undefined {
    return this.tokenUsageHistory[this.tokenUsageHistory.length - 1];
  }
}

