import {
    EventType,
    DatastarEventOptions,
    sseHeaders,

} from "./types.ts";

import { ServerSentEventGenerator as AbstractSSEGenerator } from "./abstractServerSentEventGenerator.ts";

function isRecord(obj: unknown): obj is Record<string, unknown> {
    return typeof obj === 'object' && obj !== null;
}

export class ServerSentEventGenerator extends AbstractSSEGenerator {
    protected req: Request;
    protected res: Response;

    protected constructor(req: Request, res: Response) {
        super();
        this.req = req;
        this.res = res;

        this.res.writeHead(200, sseHeaders);

        // When client closes connection, stop sending events
        this.req.on('close', () => {
            this.res.end();
        });
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

    public async readSignals(request?: Request) : Promise<
        { success: true, signals: Record<string, unknown> }
        | { success: false, error: string }
    > {
        const req = request ?? this.req;

        if (req.method === "GET") {
            const url = new URL(req.url);
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
                 else return { success: false, error: "unknown error when parsing request" }
            }
        }

        const body = await req.json();

        if ('datastar' in body && isRecord(body.datastar)) {
            return { success: true, signals: body.datastar };
        } else return { success: false, error: "No datastar object in request" };
    }
}
