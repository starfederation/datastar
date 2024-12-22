import {
    EventType,
    DatastarEventOptions,
    sseHeaders,
} from "../types";

import { ServerSentEventGenerator as AbstractSSEGenerator } from "../abstractServerSentEventGenerator";

import { IncomingMessage, ServerResponse } from "node:http";

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
}
