import {
  DatastarEventOptions,
  DefaultMapping,
  EventType,
  ExecuteScriptOptions,
  FragmentOptions,
  MergeFragmentsOptions,
  MergeSignalsOptions,
} from "./types.ts";

import {
  DefaultExecuteScriptAttributes,
  DefaultSseRetryDurationMs,
} from "./consts.ts";

import type { Jsonifiable } from "npm:type-fest";

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
      return val === DefaultMapping[key as keyof typeof DefaultMapping];
    }

    return false;
  }

  /**
   * Sends a merge fragments event.
   *
   * @param fragments - HTML fragments that will be merged.
   * @param [options] - Additional options for merging.
   */
  public mergeFragments(
    data: string,
    options?: MergeFragmentsOptions,
  ): ReturnType<typeof this.send> {
    const { eventId, retryDuration, ...renderOptions } = options ||
      {} as Partial<MergeFragmentsOptions>;

    const dataLines = this.eachOptionIsADataLine(renderOptions)
      .concat(this.eachNewlineIsADataLine("fragments", data));

    return this.send("datastar-merge-fragments", dataLines, {
      eventId,
      retryDuration,
    });
  }

  /**
   * Sends a remove fragments event.
   *
   * @param selector - CSS selector of fragments to remove.
   * @param [options] - Additional options for removing.
   */
  public removeFragments(selector: string, options?: FragmentOptions) {
    const { eventId, retryDuration, ...eventOptions } = options ||
      {} as Partial<FragmentOptions>;
    const dataLines = this.eachOptionIsADataLine(eventOptions)
      .concat(this.eachNewlineIsADataLine("selector", selector));

    return this.send("datastar-remove-fragments", dataLines, {
      eventId,
      retryDuration,
    });
  }

  /**
   * Sends a merge signals event.
   *
   * @param data - Data object that will be merged into the client's signals.
   * @param options - Additional options for merging.
   */
  public mergeSignals(
    data: Record<string, Jsonifiable> | string,
    options?: MergeSignalsOptions,
  ): ReturnType<typeof this.send> {
    const { eventId, retryDuration, ...eventOptions } = options ||
      {} as Partial<MergeSignalsOptions>;

    const signals = typeof data === "string" ? data : JSON.stringify(data);
    const dataLines = this.eachOptionIsADataLine(eventOptions)
      .concat(this.eachNewlineIsADataLine("signals", signals));

    return this.send("datastar-merge-signals", dataLines, {
      eventId,
      retryDuration,
    });
  }

  /**
   * Sends a remove signals event.
   *
   * @param paths - Array of paths to remove from the client's signals
   * @param options - Additional options for removing signals.
   */
  public removeSignals(
    paths: string[] | string,
    options?: DatastarEventOptions,
  ): ReturnType<typeof this.send> {
    const eventOptions = options || {} as DatastarEventOptions;
    const pathsArray = typeof paths === "string"
      ? paths.split(" ")
      : paths.flatMap((path) => path.split(" "));

    const dataLines = pathsArray.map((path) => `paths ${path}`);

    return this.send("datastar-remove-signals", dataLines, eventOptions);
  }

  /**
   * Executes a script on the client-side.
   *
   * @param script - Script code to execute.
   * @param options - Additional options for execution.
   */
  public executeScript(
    script: string,
    options?: ExecuteScriptOptions,
  ): ReturnType<typeof this.send> {
    const {
      eventId,
      retryDuration,
      attributes,
      ...eventOptions
    } = options || {} as Partial<ExecuteScriptOptions>;
    const attributesArray = attributes instanceof Array
      ? attributes
      : this.eachOptionIsADataLine(attributes ?? {});

    const attributesDataLines = attributesArray.filter((line) => {
      const parts = line.split(" ");
      const defaultParts = DefaultExecuteScriptAttributes.split(" ");
      if (parts[0] === defaultParts[0] && parts[1]) {
        return parts[1] !== defaultParts[1];
      }
      return true;
    }).map((line) => `attributes ${line}`);

    const dataLines = attributesDataLines.concat(
      this.eachOptionIsADataLine(eventOptions),
      this.eachNewlineIsADataLine("script", script),
    );

    return this.send("datastar-execute-script", dataLines, {
      eventId,
      retryDuration,
    });
  }
}
