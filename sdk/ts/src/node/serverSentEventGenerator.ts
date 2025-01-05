import {
    EventType,
    DatastarEventOptions,
    sseHeaders,
} from "../types";

import { ServerSentEventGenerator as AbstractSSEGenerator } from "../abstractServerSentEventGenerator";

import { IncomingMessage, ServerResponse } from "node:http";

function isRecord(obj: unknown): obj is Record<string, unknown> {
    return typeof obj === 'object' && obj !== null;
}

export class ServerSentEventGenerator extends AbstractSSEGenerator {
    protected req: IncomingMessage;
    protected res: ServerResponse;

    protected constructor(req: IncomingMessage, res: ServerResponse) {
            super();
            this.req = req;
            this.res = res;

	   this.res.writeHead(200, sseHeaders);
	   req.on('close', () => {
                 res.end();
            });
    }

    static stream(req: IncomingMessage, res: ServerResponse, streamFunc: (stream: ServerSentEventGenerator) => void): void {
        streamFunc(new ServerSentEventGenerator(req, res));
    }

    protected send(
         event: EventType,
         dataLines: string[],
         options: DatastarEventOptions
    ): string[] {
        const eventLines = super.send(event, dataLines, options);

        eventLines.forEach((line) => {
            this.res.write(line);
        });

        return eventLines;
    }

    static async readSignals(request: IncomingMessage) : Promise<
        { success: true, signals: Record<string, unknown> }
        | { success: false, error: string }
    > {
        if (request.method === "GET") {
            const url = new URL(`http://${process.env.HOST ?? 'localhost'}${request.url}`);
            const params = url.searchParams;

             try {
                 if (params.has('datastar')) {
                     const signals = JSON.parse(params.get('datastar')!);
                     return { success: true, signals };
                 } else throw new Error("No datastar object in requestuest");
             } catch(e: unknown) {
                 if (isRecord(e) && 'message' in e && typeof e.message === 'string') {
                      return { success: false, error: e.message }
                 }
                 else return { success: false, error: "unknown error when parsing request" }
            }
        }

        return { success: true, signals: await request.json() };
    }
}
