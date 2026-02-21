/**
 * LLM utility function tests using Node.js built-in test runner
 */

import { describe, it } from 'node:test';
import assert from 'node:assert';
import {
  parseSandboxFunctions,
  formatSandboxFunctions,
  buildSystemPrompt,
  type SandboxFunction
} from './llm.ts';

describe('parseSandboxFunctions', () => {
  it('should parse functions from lisp files', () => {
    const functions = parseSandboxFunctions();

    assert.ok(Array.isArray(functions), 'Should return an array');
    assert.ok(functions.length > 0, 'Should find at least one function');

    // Check that each function has required properties
    for (const fn of functions) {
      assert.ok(fn.name, 'Function should have a name');
      assert.ok(typeof fn.args === 'string', 'Function should have args string');
      assert.ok(fn.docstring, 'Function should have a docstring');
    }
  });

  it('should find expected demo functions', () => {
    const functions = parseSandboxFunctions();
    const names = functions.map(fn => fn.name);

    assert.ok(names.includes('greet'), 'Should find greet function');
    assert.ok(names.includes('factorial'), 'Should find factorial function');
    assert.ok(names.includes('fibonacci'), 'Should find fibonacci function');
    assert.ok(names.includes('add-numbers'), 'Should find add-numbers function');
  });

  it('should find expected tool functions', () => {
    const functions = parseSandboxFunctions();
    const names = functions.map(fn => fn.name);

    assert.ok(names.includes('list-files'), 'Should find list-files function');
    assert.ok(names.includes('read-file'), 'Should find read-file function');
    assert.ok(names.includes('write-file'), 'Should find write-file function');
    assert.ok(names.includes('load-file'), 'Should find load-file function');
  });

  it('should skip internal helper functions', () => {
    const functions = parseSandboxFunctions();
    const names = functions.map(fn => fn.name);

    assert.ok(!names.includes('ensure-sandbox-dir'), 'Should skip ensure-sandbox-dir');
    assert.ok(!names.includes('sandbox-filepath'), 'Should skip sandbox-filepath');
  });
});

describe('formatSandboxFunctions', () => {
  it('should format functions with sandbox prefix', () => {
    const functions: SandboxFunction[] = [
      { name: 'test-fn', args: 'a b', docstring: 'A test function' }
    ];

    const formatted = formatSandboxFunctions(functions);

    assert.ok(formatted.includes('sandbox:test-fn'), 'Should include sandbox prefix');
    assert.ok(formatted.includes('a b'), 'Should include args');
    assert.ok(formatted.includes('A test function'), 'Should include docstring');
  });

  it('should handle functions with no args', () => {
    const functions: SandboxFunction[] = [
      { name: 'no-args-fn', args: '', docstring: 'No arguments' }
    ];

    const formatted = formatSandboxFunctions(functions);

    assert.ok(formatted.includes('(sandbox:no-args-fn)'), 'Should format without extra space');
  });

  it('should format multiple functions as list', () => {
    const functions: SandboxFunction[] = [
      { name: 'fn1', args: 'x', docstring: 'First' },
      { name: 'fn2', args: 'y', docstring: 'Second' }
    ];

    const formatted = formatSandboxFunctions(functions);
    const lines = formatted.split('\n');

    assert.strictEqual(lines.length, 2, 'Should have two lines');
    assert.ok(lines[0].startsWith('- '), 'Each line should start with bullet');
    assert.ok(lines[1].startsWith('- '), 'Each line should start with bullet');
  });
});

describe('buildSystemPrompt', () => {
  it('should build a complete system prompt', () => {
    const prompt = buildSystemPrompt();

    assert.ok(prompt.includes('Common Lisp'), 'Should mention Common Lisp');
    assert.ok(prompt.includes('SBCL'), 'Should mention SBCL');
    assert.ok(prompt.includes('sandbox:'), 'Should include sandbox functions');
  });

  it('should include max iterations with default value', () => {
    const prompt = buildSystemPrompt();

    assert.ok(prompt.includes('5 iterations'), 'Should include default max iterations (5)');
  });

  it('should include custom max iterations value', () => {
    const prompt = buildSystemPrompt({ maxIterations: 10 });

    assert.ok(prompt.includes('10 iterations'), 'Should include custom max iterations');
    assert.ok(!prompt.includes('5 iterations'), 'Should not include default value');
  });

  it('should include agentic execution section', () => {
    const prompt = buildSystemPrompt();

    assert.ok(prompt.includes('Agentic Execution'), 'Should include agentic section');
    assert.ok(prompt.includes('Fix errors'), 'Should mention error fixing');
    assert.ok(prompt.includes('without any ```lisp code blocks'), 'Should explain how to stop');
  });

  it('should include all parsed sandbox functions', () => {
    const prompt = buildSystemPrompt();
    const functions = parseSandboxFunctions();

    for (const fn of functions) {
      assert.ok(
        prompt.includes(`sandbox:${fn.name}`),
        `Should include function ${fn.name}`
      );
    }
  });
});

