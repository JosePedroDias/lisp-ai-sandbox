/**
 * Swank connection tests using Node.js built-in test runner
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

    console.log(`Connecting to Swank at ${host}:${port}...`);
    swank = new SwankConnection({ host, port });

    await swank.connect();
    console.log('Connected!');
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
});

