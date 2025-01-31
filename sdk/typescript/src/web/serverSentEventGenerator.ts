import { DatastarEventOptions, EventType, sseHeaders } from "../types.ts";
import { ServerSentEventGenerator as AbstractSSEGenerator } from "../abstractServerSentEventGenerator.ts";

import type { Jsonifiable } from "npm:type-fest";
import { deepmerge } from "npm:deepmerge-ts";

function isRecord(obj: unknown): obj is Record<string, Jsonifiable> {
  return typeof obj === "object" && obj !== null;
}

/**
 * ServerSentEventGenerator class, responsible for initializing and handling
 * server-sent events (SSE) as well as reading signals sent by the client.
 * Cannot be instantiated directly, you must use the stream static method.
 */
export class ServerSentEventGenerator extends AbstractSSEGenerator {
  protected controller: ReadableStreamDefaultController;

  protected constructor(controller: ReadableStreamDefaultController) {
    super();
    this.controller = controller;
  }

  /**
   * Closes the ReadableStream
   */
  public close() {
    this.controller.close();
  }

  /**
   * Initializes the server-sent event generator and executes the streamFunc function.
   *
   * @param onStart - A function that will be passed the initialized ServerSentEventGenerator class as it's first parameter.
   * @param options? - An object that can contain options for the Response constructor as well as onError and onCancel callbacks.
   * The onAbort callback will be called whenever the request is aborted or the stream is cancelled
   * The onError callback keeps the stream open after an exception  to report the error. If it is not provided it will
   * cancel the stream and throw the error.
   *
   * @returns an HTTP Response
   */
  static stream(
    onStart: (stream: ServerSentEventGenerator) => Promise<void> | void,
    options?: Partial<{
      onError: (
        stream: ServerSentEventGenerator,
        error: unknown,
      ) => Promise<void> | void;
      onAbort: (reason: string) => Promise<void> | void;
      init: ResponseInit;
    }>,
  ): Response {
    const readableStream = new ReadableStream({
      async start(controller) {
        const generator = new ServerSentEventGenerator(controller);

        try {
          const startedStream = onStart(generator);
          if (startedStream instanceof Promise) await startedStream;
        } catch (error) {
          const abortResult = options && options.onAbort
            ? options.onAbort(
              error instanceof Error
                ? error.message
                : "onStart callback threw an error",
            )
            : null;

          if (abortResult instanceof Promise) await abortResult;
          if (options && options.onError) {
            const errorStream = options.onError(generator, error);
            if (errorStream instanceof Promise) await errorStream;
          } else {
            controller.close();
            throw error;
          }
        }
      },
      async cancel(reason) {
        const abortResult = options && options.onAbort
          ? options.onAbort(reason)
          : null;
        if (abortResult instanceof Promise) await abortResult;
      },
    });

    return new Response(
      readableStream,
      deepmerge({
        headers: sseHeaders,
      }, options?.init ?? {}),
    );
  }

  protected override send(
    event: EventType,
    dataLines: string[],
    options: DatastarEventOptions,
  ): string[] {
    const eventLines = super.send(event, dataLines, options);

    eventLines.forEach((line) => {
      this.controller?.enqueue(new TextEncoder().encode(line));
    });

    return eventLines;
  }

  /**
   * Reads client sent signals based on HTTP methods
   *
   * @params request - The HTTP Request object.
   *
   * @returns An object containing a success boolean and either the client's signals or an error message.
   */
  static async readSignals(request: Request): Promise<
    | { success: true; signals: Record<string, Jsonifiable> }
    | { success: false; error: string }
  > {
    try {
      if (request.method === "GET") {
        const url = new URL(request.url);
        const params = url.searchParams;
        if (params.has("datastar")) {
          const signals = JSON.parse(params.get("datastar")!);

          if (isRecord(signals)) {
            return { success: true, signals };
          } else throw new Error("Datastar param is not a record");
        } else throw new Error("No datastar object in request");
      }

      const signals = await request.json();

      if (isRecord(signals)) {
        return { success: true, signals: signals };
      }

      throw new Error("Parsed JSON body is not of type record");
    } catch (e: unknown) {
      if (isRecord(e) && "message" in e && typeof e.message === "string") {
        return { success: false, error: e.message };
      }

      return { success: false, error: "unknown error when parsing request" };
    }
  }
}
