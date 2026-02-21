/**
 * Swank connection tests using Node.js built-in test runner
 *
 * IMPORTANT: These tests require a running Swank server.
 * Start the Lisp sandbox first:
 *   cd lisp-sandbox && ./start-sandbox.sh
 */

import 'dotenv/config';
import { describe, it, before, after } from 'node:test';
import assert from 'node:assert';
import { SwankConnection } from './swank.ts';

describe('SwankConnection', () => {
  let swank: SwankConnection;

  before(async () => {
    const host = process.env.SWANK_HOST || 'localhost';
    const port = parseInt(process.env.SWANK_PORT || '4006', 10);

    console.log('');
    console.log('========================================');
    console.log('  Swank Integration Tests');
    console.log('========================================');
    console.log('');
    console.log('NOTE: These tests require a running Swank server.');
    console.log('If connection fails, start the Lisp sandbox:');
    console.log('  cd lisp-sandbox && ./start-sandbox.sh');
    console.log('');
    console.log(`Connecting to Swank at ${host}:${port}...`);

    swank = new SwankConnection({ host, port });

    try {
      await swank.connect();
      console.log('Connected!\n');
    } catch (error) {
      console.error('\n❌ Failed to connect to Swank server!');
      console.error('Make sure the Lisp sandbox is running:');
      console.error('  cd lisp-sandbox && ./start-sandbox.sh\n');
      throw error;
    }
  });

  after(() => {
    if (swank) {
      swank.disconnect();
    }
  });

  it('should be connected', () => {
    assert.ok(swank.isConnected(), 'Should be connected to Swank server');
  });

  it('should evaluate simple arithmetic', async () => {
    const result = await swank.eval('(+ 1 2 3)');
    console.log('Eval result:', result);
    assert.ok(result.success, `Eval should succeed: ${result.error}`);
    assert.ok(result.output.includes('6'), `Result should be 6, got: ${result.output}`);
  });

  it('should evaluate a defun and call it', async () => {
    const defResult = await swank.eval('(defun test-add (a b) (+ a b))');
    console.log('Defun result:', defResult);
    assert.ok(defResult.success, `Defun should succeed: ${defResult.error}`);

    const callResult = await swank.eval('(test-add 10 20)');
    console.log('Call result:', callResult);
    assert.ok(callResult.success, `Call should succeed: ${callResult.error}`);
    assert.ok(callResult.output.includes('30'), `Result should be 30, got: ${callResult.output}`);
  });

  it('should handle undefined symbol gracefully', async () => {
    const result = await swank.eval('(this-function-does-not-exist)');
    console.log('Error result:', result);
    // Undefined function should fail
    assert.ok(!result.success, 'Should fail for undefined function');
  });

  it('should call sandbox:greet function', async () => {
    const result = await swank.eval('(sandbox:greet "World")');
    console.log('Greet result:', result);
    assert.ok(result.success, `Greet should succeed: ${result.error}`);
    assert.ok(result.output.includes('World'), `Should greet World, got: ${result.output}`);
  });

  it('should call sandbox:list-files function', async () => {
    const result = await swank.eval('(sandbox:list-files)');
    console.log('List files result:', result);
    assert.ok(result.success, `List files should succeed: ${result.error}`);
    // Result should be a list (possibly empty)
    assert.ok(result.output.includes('NIL') || result.output.includes('('),
      `Should return a list, got: ${result.output}`);
  });

  it('should call sandbox:factorial function', async () => {
    const result = await swank.eval('(sandbox:factorial 5)');
    console.log('Factorial result:', result);
    assert.ok(result.success, `Factorial should succeed: ${result.error}`);
    assert.ok(result.output.includes('120'), `Factorial of 5 should be 120, got: ${result.output}`);
  });
});

