import {
    EventType,
    DatastarEventOptions,
    sseHeaders,
} from "../types";

import { ServerSentEventGenerator as AbstractSSEGenerator } from "../abstractServerSentEventGenerator";

function isRecord(obj: unknown): obj is Record<string, unknown> {
    return typeof obj === 'object' && obj !== null;
}

export class ServerSentEventGenerator extends AbstractSSEGenerator {
    protected controller: ReadableStreamDefaultController;

    protected constructor(controller: ReadableStreamDefaultController) {
            super();
            this.controller = controller;
    }

    static stream(streamFunc: (stream: ServerSentEventGenerator) => void): Response {
        const stream = new ReadableStream({
            start(controller) {
                 streamFunc(new ServerSentEventGenerator(controller));
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