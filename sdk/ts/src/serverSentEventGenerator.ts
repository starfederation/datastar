import {
    EventType,
    DatastarEventOptions,
    sseHeaders,
} from "./types.ts";

import { ServerSentEventGenerator as AbstractSSEGenerator } from "./abstractServerSentEventGenerator.ts";

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
}
