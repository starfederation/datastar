import {
  DatastarEventOptions,
  DefaultMapping,
  EventType,
  ExecuteScriptOptions,
  ElementOptions,
  PatchElementsOptions,
  PatchSignalsOptions,
  Jsonifiable,
} from "./types.ts";

import {
  DatastarDatalineElements,
  DatastarDatalinePatchMode,
  DatastarDatalineSelector,
  DatastarDatalineSignals,
  DatastarDatalineOnlyIfMissing,
  DatastarDatalineUseViewTransition,
  DefaultExecuteScriptAttributes,
  DefaultSseRetryDurationMs,
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
      return `${prefix}${line}`;
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
      return val === DefaultMapping[key as keyof typeof DefaultMapping];
    }

    return false;
  }

  /**
   * Patches HTML elements into the DOM.
   *
   * @param elements - HTML elements that will be patched.
   * @param [options] - Additional options for patching.
   */
  public PatchElements(
    elements: string,
    options?: PatchElementsOptions,
  ): ReturnType<typeof this.send> {
    const { eventId, retryDuration, ...renderOptions } = options ||
      {} as Partial<PatchElementsOptions>;

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
   */
  public PatchSignals(
    signals: string,
    options?: PatchSignalsOptions,
  ): ReturnType<typeof this.send> {
    const { eventId, retryDuration, ...eventOptions } = options ||
      {} as Partial<PatchSignalsOptions>;

    const dataLines = this.eachOptionIsADataLine(eventOptions)
      .concat(this.eachNewlineIsADataLine(DatastarDatalineSignals, signals));

    return this.send("datastar-patch-signals", dataLines, {
      eventId,
      retryDuration,
    });
  }

  /**
   * Executes a script on the client-side by creating a script element via PatchElements.
   *
   * @param script - Script code to execute.
   * @param [options] - Additional options for execution.
   */
  public ExecuteScript(
    script: string,
    options?: ExecuteScriptOptions,
  ): ReturnType<typeof this.send> {
    const {
      eventId,
      retryDuration,
      attributes,
      autoRemove,
      ...eventOptions
    } = options || {} as Partial<ExecuteScriptOptions>;

    // Build script tag
    let scriptTag = '<script';
    
    // Add attributes if provided
    if (attributes) {
      if (Array.isArray(attributes)) {
        for (const attr of attributes) {
          scriptTag += ` ${attr}`;
        }
      } else {
        for (const [key, value] of Object.entries(attributes)) {
          scriptTag += ` ${key}="${value}"`;
        }
      }
    }

    // Add auto-remove attribute if needed (default is false per spec)
    if (autoRemove === true) {
      scriptTag += ' data-on-load="el.remove()"';
    }

    scriptTag += `>${script}</script>`;

    // Use PatchElements with body selector and append mode
    const patchOptions: PatchElementsOptions = {
      [DatastarDatalineSelector]: 'body',
      [DatastarDatalinePatchMode]: 'append',
      eventId,
      retryDuration,
    };

    return this.PatchElements(scriptTag, patchOptions);
  }
}
