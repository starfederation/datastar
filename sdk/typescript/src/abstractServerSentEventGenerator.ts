import {
  DatastarEventOptions,
  DefaultMapping,
  EventType,
  PatchElementsOptions,
  PatchSignalsOptions,
  Jsonifiable,
  ElementPatchMode,
} from "./types.ts";

import {
  DatastarDatalineElements,
  DatastarDatalinePatchMode,
  DatastarDatalineSelector,
  DatastarDatalineSignals,
  DefaultSseRetryDurationMs,
  ElementPatchModes,
} from "./consts.ts";

/**
 * Abstract ServerSentEventGenerator class, responsible for initializing and handling
 * server-sent events (SSE) as well as reading signals sent by the client.
 *
 * The concrete implementation must override the send and constructor methods as well
 * as implement readSignals and stream static methods.
 */
export abstract class ServerSentEventGenerator {
  protected constructor() {}

  /**
   * Validates that the provided mode is a valid ElementPatchMode.
   * @param mode - The mode to validate
   * @throws {Error} If the mode is invalid
   */
  private validateElementPatchMode(mode: string): asserts mode is ElementPatchMode {
    if (!ElementPatchModes.includes(mode as ElementPatchMode)) {
      throw new Error(`Invalid ElementPatchMode: "${mode}". Valid modes are: ${ElementPatchModes.join(', ')}`);
    }
  }

  /**
   * Validates that the provided string is valid JSON.
   * @param jsonString - The JSON string to validate
   * @throws {Error} If the JSON is invalid
   */
  private validateJSON(jsonString: string): void {
    try {
      JSON.parse(jsonString);
    } catch (error) {
      throw new Error(`Invalid JSON provided: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
  }

  /**
   * Validates required parameters are not empty or undefined.
   * @param value - The value to validate
   * @param paramName - The parameter name for error messages
   * @throws {Error} If the value is empty or undefined
   */
  private validateRequired(value: string | undefined, paramName: string): asserts value is string {
    if (!value || value.trim() === '') {
      throw new Error(`${paramName} is required and cannot be empty`);
    }
  }

  /**
   * Sends a server-sent event (SSE) to the client.
   *
   * Runtimes should override this method by calling the parent function
   *  with `super.send(event, dataLines, options)`. That will return all the
   * datalines as an array of strings that should be streamed to the client.
   *
   * @param eventType - The type of the event.
   * @param dataLines - Lines of data to send.
   * @param [sendOptions] - Additional options for sending events.
   */
  protected send(
    event: EventType,
    dataLines: string[],
    options: DatastarEventOptions,
  ): string[] {
    const { eventId, retryDuration } = options || {};

    const typeLine = [`event: ${event}\n`];
    const idLine = eventId ? [`id: ${eventId}\n`] : [];
    const retryLine = !retryDuration || retryDuration === 1000 ? [] : [
      `retry: ${retryDuration ?? DefaultSseRetryDurationMs}\n`,
    ];

    return typeLine.concat(
      idLine,
      retryLine,
      dataLines.map((data) => {
        return `data: ${data}\n`;
      }),
      ["\n\n"],
    );
  }

  private eachNewlineIsADataLine(prefix: string, data: string) {
    return data.split("\n").map((line) => {
      return `${prefix} ${line}`;
    });
  }

  private eachOptionIsADataLine(
    options: Record<string, Jsonifiable>,
  ): string[] {
    return Object.keys(options).filter((key) => {
      return !this.hasDefaultValue(key, options[key as keyof typeof options]);
    }).flatMap((key) => {
      return this.eachNewlineIsADataLine(
        key,
        options[key as keyof typeof options]!.toString(),
      );
    });
  }

  private hasDefaultValue(key: string, val: unknown): boolean {
    if (key in DefaultMapping) {
      return val === (DefaultMapping as Record<string, unknown>)[key];
    }

    return false;
  }

  /**
   * Patches HTML elements into the DOM.
   *
   * @param elements - HTML elements that will be patched.
   * @param [options] - Additional options for patching.
   * @throws {Error} If validation fails
   */
  public PatchElements(
    elements: string,
    options?: PatchElementsOptions,
  ): ReturnType<typeof this.send> {
    // Validate required parameters
    this.validateRequired(elements, 'elements');

    const { eventId, retryDuration, ...renderOptions } = options ||
      {} as Partial<PatchElementsOptions>;

    // Validate patch mode if provided
    const patchMode = (renderOptions as Record<string, unknown>)[DatastarDatalinePatchMode] as string;
    if (patchMode) {
      this.validateElementPatchMode(patchMode);
    }

    // Per spec: If no selector specified, elements must have IDs (this validation would be complex
    // and is better handled client-side, but we ensure elements is not empty)
    const selector = (renderOptions as Record<string, unknown>)[DatastarDatalineSelector] as string;
    if (!selector && patchMode === 'remove') {
      // For remove mode, elements parameter may be omitted when selector is supplied
      // but since we have no selector, we need elements with IDs
      if (!elements || elements.trim() === '') {
        throw new Error('For remove mode without selector, elements parameter with IDs is required');
      }
    }

    const dataLines = this.eachOptionIsADataLine(renderOptions)
      .concat(this.eachNewlineIsADataLine(DatastarDatalineElements, elements));

    return this.send("datastar-patch-elements", dataLines, {
      eventId,
      retryDuration,
    });
  }

  /**
   * Patches signals into the signal store.
   *
   * @param signals - JSON string containing signal data to patch.
   * @param [options] - Additional options for patching.
   * @throws {Error} If validation fails
   */
  public PatchSignals(
    signals: string,
    options?: PatchSignalsOptions,
  ): ReturnType<typeof this.send> {
    // Validate required parameters
    this.validateRequired(signals, 'signals');
    
    // Validate JSON format
    this.validateJSON(signals);

    const { eventId, retryDuration, ...eventOptions } = options ||
      {} as Partial<PatchSignalsOptions>;

    const dataLines = this.eachOptionIsADataLine(eventOptions)
      .concat(this.eachNewlineIsADataLine(DatastarDatalineSignals, signals));

    return this.send("datastar-patch-signals", dataLines, {
      eventId,
      retryDuration,
    });
  }
}
