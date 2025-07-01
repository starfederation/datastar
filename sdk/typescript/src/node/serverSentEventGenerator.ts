import { DatastarEventOptions, EventType, sseHeaders, StreamOptions, Jsonifiable } from "../types.ts";

import { ServerSentEventGenerator as AbstractSSEGenerator } from "../abstractServerSentEventGenerator.ts";

import { IncomingMessage, ServerResponse } from "node:http";
import process from "node:process";

function isRecord(obj: unknown): obj is Record<string, Jsonifiable> {
  return typeof obj === "object" && obj !== null;
}

/**
 * ServerSentEventGenerator class, responsible for initializing and handling
 * server-sent events (SSE) as well as reading signals sent by the client.
 * Cannot be instantiated directly, you must use the stream static method.
 */
export class ServerSentEventGenerator extends AbstractSSEGenerator {
  protected req: IncomingMessage;
  protected res: ServerResponse;

  protected constructor(req: IncomingMessage, res: ServerResponse) {
    super();
    this.req = req;
    this.res = res;

    this.res.writeHead(200, sseHeaders);
    // Per spec: "Should flush response immediately to prevent timeouts"
    this.res.flushHeaders();
  }

  /**
   * Initializes the server-sent event generator and executes the onStart callback.
   *
   * @param req - The NodeJS request object.
   * @param res - The NodeJS response object.
   * @param onStart - A function that will be passed the initialized ServerSentEventGenerator class as it's first parameter.
   * @param options? - An object that can contain onError and onCancel callbacks as well as a keepalive boolean.
   * The onAbort callback will be called whenever the request is aborted
   *
   * The onError callback will be called whenever an error is met. If provided, the onAbort callback will also be executed.
   * If an onError callback is not provided, then the stream will be ended and the error will be thrown up.
   *
   * The stream is always closed after the onStart callback ends.
   * If onStart is non blocking, but you still need the stream to stay open after it is called,
   * then the keepalive option will maintain it open until the request is aborted by the client.
   */
  static async stream(
    req: IncomingMessage,
    res: ServerResponse,
    onStart: (stream: ServerSentEventGenerator) => Promise<void> | void,
    options?: StreamOptions,
  ): Promise<void> {
    const generator = new ServerSentEventGenerator(req, res);

    req.on("close", async () => {
      const onAbort = options?.onAbort ? options.onAbort() : null;
      if (onAbort instanceof Promise) await onAbort;

      res.end();
    });

    try {
      const stream = onStart(generator);
      if (stream instanceof Promise) await stream;
      if (!options?.keepalive) {
        res.end();
      }
    } catch (error: unknown) {
      const onAbort = options?.onAbort ? options.onAbort() : null;
      if (onAbort instanceof Promise) await onAbort;

      if (options?.onError) {
        const onError = options.onError(error);
        if (onError instanceof Promise) await onError;
        res.end();
      } else {
        res.end();
        throw error;
      }
    }
  }

  protected override send(
    event: EventType,
    dataLines: string[],
    options: DatastarEventOptions,
  ): string[] {
    const eventLines = super.send(event, dataLines, options);

    eventLines.forEach((line) => {
      this.res.write(line);
    });

    return eventLines;
  }

  /**
   * Reads client sent signals based on HTTP methods
   *
   * @params request - The NodeJS Request object.
   *
   * @returns An object containing a success boolean and either the client's signals or an error message.
   */
  static async ReadSignals(request: IncomingMessage): Promise<
    | { success: true; signals: Record<string, Jsonifiable> }
    | { success: false; error: string }
  > {
    if (request.method === "GET") {
      const url = new URL(
        `http://${process.env.HOST ?? "localhost"}${request.url}`,
      );
      const params = url.searchParams;

      try {
        if (params.has("datastar")) {
          const signals = JSON.parse(params.get("datastar")!);
          if (isRecord(signals)) {
            return { success: true, signals };
          } else throw new Error("Datastar param is not a record");
        } else throw new Error("No datastar object in request");
      } catch (e: unknown) {
        if (isRecord(e) && "message" in e && typeof e.message === "string") {
          return { success: false, error: e.message };
        } else {return {
            success: false,
            error: "unknown error when parsing request",
          };}
      }
    }
    const body = await new Promise((resolve, _) => {
      let chunks = "";
      request.on("data", (chunk) => {
        chunks += chunk;
      });
      request.on("end", () => {
        resolve(chunks);
      });
    });
    
    try {
      if (typeof body !== "string") throw Error("body was not a string");
      const parsedBody = JSON.parse(body);
      
      if (isRecord(parsedBody)) {
        return { success: true, signals: parsedBody };
      } else {
        throw new Error("Parsed JSON body is not of type record");
      }
    } catch (e: unknown) {
      if (isRecord(e) && "message" in e && typeof e.message === "string") {
        return { success: false, error: e.message };
      } else {
        return {
          success: false,
          error: "unknown error when parsing request",
        };
      }
    }
  }
}
