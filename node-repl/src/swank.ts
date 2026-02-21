/**
 * Swank client wrapper for communicating with Common Lisp
 */

// @ts-expect-error - swank-client has no type definitions
import swankClient from 'swank-client';

const { Client } = swankClient;

// Filter out noisy "Ignoring command" logs from swank-client
const originalConsoleLog = console.log;
console.log = (...args: unknown[]) => {
  const msg = args[0];
  if (typeof msg === 'string' && msg.startsWith('Ignoring command')) {
    return; // Suppress these messages
  }
  originalConsoleLog.apply(console, args);
};

export interface SwankEvalResult {
  success: boolean;
  output: string;
  error?: string;
}

export interface SwankClientOptions {
  host?: string;
  port?: number;
  package?: string;
}

/**
 * Wrapper around swank-client with Promise-based API
 */
export class SwankConnection {
  private client: any;
  private host: string;
  private port: number;
  private package: string;
  private outputBuffer: string[] = [];
  private connected: boolean = false;

  constructor(options: SwankClientOptions = {}) {
    this.host = options.host ?? 'localhost';
    this.port = options.port ?? 4006;
    this.package = options.package ?? 'COMMON-LISP-USER';

    this.client = new Client(this.host, this.port);

    // Capture print output from Lisp
    this.client.on('print_string', (msg: string) => {
      this.outputBuffer.push(msg);
    });
  }

  /**
   * Connect to the Swank server
   * Note: We skip client.initialize() as it tries to load optional SWANK modules
   * that may hang. Instead we use SWANK:EVAL-AND-GRAB-OUTPUT directly.
   */
  async connect(): Promise<void> {
    await this.client.connect();
    // Skip initialize() - it hangs trying to load optional modules
    // We'll use rex() with SWANK:EVAL-AND-GRAB-OUTPUT instead
    this.connected = true;
    console.log(`Connected to Swank server at ${this.host}:${this.port}`);
  }

  /**
   * Disconnect from the Swank server
   */
  disconnect(): void {
    if (this.connected) {
      this.client.disconnect();
      this.connected = false;
      console.log('Disconnected from Swank server');
    }
  }

  /**
   * Check if connected
   */
  isConnected(): boolean {
    return this.connected && this.client.is_connected();
  }

  /**
   * Evaluate a Lisp expression using SWANK:EVAL-AND-GRAB-OUTPUT
   */
  async eval(expression: string): Promise<SwankEvalResult> {
    if (!this.isConnected()) {
      return {
        success: false,
        output: '',
        error: 'Not connected to Swank server'
      };
    }

    // Clear the output buffer
    this.outputBuffer = [];

    try {
      // Wrap expression in handler-case to catch errors without triggering debugger
      const escapedExpr = expression.replace(/\\/g, '\\\\').replace(/"/g, '\\"');
      const wrappedExpr = `
        (handler-case
          (let ((result (eval (read-from-string "${escapedExpr}"))))
            (list :ok (princ-to-string result)))
          (error (e)
            (list :error (princ-to-string e))))
      `;
      const cmd = `(SWANK:EVAL-AND-GRAB-OUTPUT "${wrappedExpr.replace(/\\/g, '\\\\').replace(/"/g, '\\"')}")`;
      const result = await this.client.rex(cmd, this.package, 'T');

      // Parse the result - it's a paredit AST
      // Result format: ("printed output" "return value")
      // The return value is either (:ok "result") or (:error "message")
      const printedOutput = this.extractString(result.children?.[0]);
      const returnValue = this.extractString(result.children?.[1]);

      // Parse the inner result to check for :ok or :error
      if (returnValue.startsWith('(:ERROR') || returnValue.startsWith('(:error')) {
        // Extract error message
        const errorMatch = returnValue.match(/:ERROR\s+"?([^")]+)"?\)/i)
                        || returnValue.match(/:ERROR\s+([^)]+)\)/i);
        return {
          success: false,
          output: printedOutput || '',
          error: errorMatch ? errorMatch[1] : returnValue
        };
      }

      // Extract the actual value from (:OK "value")
      const valueMatch = returnValue.match(/:OK\s+"?([^")]*)"?\)/i)
                      || returnValue.match(/:OK\s+([^)]*)\)/i);
      const output = valueMatch ? valueMatch[1] : returnValue;

      return {
        success: true,
        output: printedOutput ? `${printedOutput}\n${output}` : (output || 'NIL')
      };
    } catch (error) {
      return {
        success: false,
        output: '',
        error: error instanceof Error ? error.message : String(error)
      };
    }
  }

  /**
   * Extract string value from paredit AST node
   */
  private extractString(node: any): string {
    if (!node) return '';
    if (node.type === 'string' && node.source) {
      // Remove surrounding quotes and unescape
      return node.source
        .slice(1, -1)
        .replace(/\\"/g, '"')
        .replace(/\\\\/g, '\\');
    }
    if (node.source) return node.source;
    return '';
  }

  /**
   * Set the current package
   */
  setPackage(pkg: string): void {
    this.package = pkg;
  }

  /**
   * Get autocomplete suggestions
   */
  async autocomplete(prefix: string): Promise<string[]> {
    if (!this.isConnected()) return [];
    try {
      return await this.client.autocomplete(prefix, this.package);
    } catch {
      return [];
    }
  }
}

