/**
 * Gemini provider tests using Node.js built-in test runner
 */

import 'dotenv/config';
import { describe, it, before } from 'node:test';
import assert from 'node:assert';
import { GeminiProvider } from './gemini.ts';

describe('GeminiProvider', () => {
  let provider: GeminiProvider;
  let apiKey: string;
  let modelName: string;

  before(() => {
    apiKey = process.env.GEMINI_API_KEY!;
    assert.ok(apiKey, 'GEMINI_API_KEY must be set in .env');

    modelName = process.env.GEMINI_MODEL || 'gemini-2.5-flash';
    provider = new GeminiProvider({ apiKey, modelName });
  });

  it('should accept maxIterations option', () => {
    const customProvider = new GeminiProvider({
      apiKey,
      modelName,
      maxIterations: 10
    });
    // Provider should be created without error
    assert.ok(customProvider, 'Should create provider with custom maxIterations');
  });

  it('should complete a simple prompt', async () => {
    const response = await provider.complete('Reply with exactly: HELLO');
    assert.ok(response.includes('HELLO'), `Response should contain HELLO, got: "${response}"`);
  });

  it('should remember context in chat history', async () => {
    const chatResponse = await provider.chat([
      { role: 'user', content: 'My name is TestUser' },
      { role: 'assistant', content: 'Nice to meet you, TestUser!' },
      { role: 'user', content: 'What is my name?' }
    ]);
    assert.ok(
      chatResponse.content.includes('TestUser'),
      `Should remember the name, got: "${chatResponse.content}"`
    );
  });

  it('should generate Lisp code', async () => {
    const response = await provider.complete(
      'Write a simple Lisp expression that adds 2 and 3. Only output the code, nothing else.'
    );
    assert.ok(
      response.includes('+'),
      `Should generate Lisp addition code, got: "${response}"`
    );
  });

  it('should track token usage after chat', async () => {
    const freshProvider = new GeminiProvider({ apiKey, modelName });

    // Initially no token usage
    assert.strictEqual(freshProvider.getTokenUsageHistory().length, 0, 'Should start with empty history');
    assert.strictEqual(freshProvider.getLastTokenUsage(), undefined, 'Should have no last usage initially');

    // Make a chat request
    await freshProvider.chat([{ role: 'user', content: 'Say hi' }]);

    // Should now have token usage
    const history = freshProvider.getTokenUsageHistory();
    assert.strictEqual(history.length, 1, 'Should have one usage entry after one chat');

    const usage = freshProvider.getLastTokenUsage();
    assert.ok(usage, 'Should have last token usage');
    assert.ok(usage!.promptTokenCount > 0, 'Should have prompt tokens');
    assert.ok(usage!.candidatesTokenCount > 0, 'Should have response tokens');
    assert.ok(usage!.totalTokenCount > 0, 'Should have total tokens');
    assert.ok(usage!.timestamp instanceof Date, 'Should have timestamp');
  });
});

