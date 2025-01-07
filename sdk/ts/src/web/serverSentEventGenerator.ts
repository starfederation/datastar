import {
    EventType,
    DatastarEventOptions,
    sseHeaders,
} from "../types";

import { ServerSentEventGenerator as AbstractSSEGenerator } from "../abstractServerSentEventGenerator";

function isRecord(obj: unknown): obj is Record<string, unknown> {
    return typeof obj === 'object' && obj !== null;
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
   * Initializes the server-sent event generator and executes the streamFunc function.
   *
   * @param streamFunc - A function that will be passed the initialized ServerSentEventGenerator class as it's first parameter.
   * @returns an HTTP Response
   */
    static stream(streamFunc: (stream: ServerSentEventGenerator) => Promise<void>): Response {
        const stream = new ReadableStream({
            async start(controller) {
                 await streamFunc(new ServerSentEventGenerator(controller));
                 controller.close();
            }
        });

        return new Response(stream, {
            headers: sseHeaders,
        });
    }

    protected send(
         event: EventType,
         dataLines: string[],
         options: DatastarEventOptions
    ): string[] {
        const eventLines = super.send(event, dataLines, options);

        eventLines.forEach((line) => {
            this.controller?.enqueue(new TextEncoder().encode(line))
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
        { success: true, signals: Record<string, unknown> }
        | { success: false, error: string }
    > {
        if (request.method === "GET") {
            const url = new URL(request.url);
            const params = url.searchParams;
             try {
                 if (params.has('datastar')) {
                     const signals = JSON.parse(params.get('datastar')!);
                     return { success: true, signals };
                 } else throw new Error("No datastar object in request");
             } catch(e: unknown) {
                 if (isRecord(e) && 'message' in e && typeof e.message === 'string') {
                      return { success: false, error: e.message }
                 }
                 else return { success: false, error: "unknown error when parsing requestuest" }
            }
        }

        return { success: true, signals: await request.json() };
    }
}