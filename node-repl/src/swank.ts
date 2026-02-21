/**
 * Swank client wrapper for communicating with Common Lisp
 */

// @ts-expect-error - swank-client has no type definitions
import swankClient from 'swank-client';

const { Client } = swankClient;

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
    this.port = options.port ?? 4005;
    this.package = options.package ?? 'COMMON-LISP-USER';
    
    this.client = new Client(this.host, this.port);
    
    // Capture print output from Lisp
    this.client.on('print_string', (msg: string) => {
      this.outputBuffer.push(msg);
    });
  }

  /**
   * Connect to the Swank server
   */
  async connect(): Promise<void> {
    await this.client.connect();
    await this.client.initialize();
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
   * Evaluate a Lisp expression
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
      const result = await this.client.eval(expression, this.package);
      
      // Give a small delay for output to arrive
      await new Promise(resolve => setTimeout(resolve, 100));
      
      const output = this.outputBuffer.join('');
      
      return {
        success: true,
        output: output || this.formatResult(result)
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
   * Format the raw result from swank
   */
  private formatResult(result: any): string {
    if (!result) return 'NIL';
    if (result.source) return result.source;
    return String(result);
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

