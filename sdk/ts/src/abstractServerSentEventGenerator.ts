import {
    EventType,
    DatastarEventOptions,
    FragmentOptions,
    MergeFragmentsOptions,
    MergeSignalsOptions,
    ExecuteScriptOptions,
} from "./types";

import {
    DefaultSseRetryDurationMs
} from "./consts";

export abstract class ServerSentEventGenerator {
    // runtimes should override this method to create an sse stream
    protected constructor() {}

    // runtimes should override this method and use it's output to send an event
    protected send(
         event: EventType,
         dataLines: string[],
         options: DatastarEventOptions
    ): string[] {
        const { eventId, retryDuration } = options || {};

        const typeLine = [`event: ${event}\n`];
        const idLine = eventId ? [`id: ${eventId}\n`] : [];
        const retryLine = [`retry: ${retryDuration ?? DefaultSseRetryDurationMs}\n`];

        return typeLine.concat(
            idLine,
            retryLine,
            dataLines.map((data) => {
                return `data: ${data}\n`;
            }),
            ['\n\n']
        );
    }

    private eachNewlineIsADataLine(prefix: string, data: string) {
       return data.split('\n').map((line) => {
           return `${prefix} ${line}`;
       });
    }

    private eachOptionIsADataLine(options: Record<string, any>): string[] {
        return Object.keys(options).flatMap((key) => {
            return this.eachNewlineIsADataLine(key, options[key as keyof typeof options]!.toString());
        });
    }

    public mergeFragments(data: string, options?: MergeFragmentsOptions): ReturnType<typeof this.send> {
        const { eventId, retryDuration, ...renderOptions } = options || {} as Partial<MergeFragmentsOptions>;
        const dataLines = this.eachOptionIsADataLine(renderOptions)
            .concat(this.eachNewlineIsADataLine('fragments', data));

        return this.send('datastar-merge-fragments', dataLines, { eventId, retryDuration });
    }

    public removeFragments(selector: string, options?: FragmentOptions): ReturnType<typeof this.send> {
        const { eventId, retryDuration, ...eventOptions } = options || {} as Partial<FragmentOptions>;
        const dataLines = this.eachOptionIsADataLine(eventOptions)
            .concat(this.eachNewlineIsADataLine("selector", selector));

        return this.send('datastar-remove-fragments', dataLines, { eventId, retryDuration });
    }

    public mergeSignals(data: Record<string, any>, options?: MergeSignalsOptions): ReturnType<typeof this.send> {
        const { eventId, retryDuration, ...eventOptions } = options || {} as Partial<MergeSignalsOptions>;
        const dataLines = this.eachOptionIsADataLine(eventOptions)
            .concat(this.eachNewlineIsADataLine('signals', JSON.stringify(data)));

        return this.send('datastar-merge-signals', dataLines, { eventId, retryDuration });
    }

    public removeSignals(paths: string[], options?: DatastarEventOptions): ReturnType<typeof this.send> {
        const eventOptions = options || {} as DatastarEventOptions;
        const dataLines = paths.flatMap((path) => path.split(' ')).map((path) => `paths ${path}`);

        return this.send('datastar-remove-signals', dataLines, eventOptions);
    }

    public executeScript(script: string, options?: ExecuteScriptOptions): ReturnType<typeof this.send> {
        const {
            eventId,
            retryDuration,
            attributes,
            ...eventOptions
        } = options || {} as Partial<ExecuteScriptOptions>;

        const attributesDataLines = this.eachOptionIsADataLine(attributes ?? {})
            .map((line) => `attributes ${line}`);

        const dataLines = attributesDataLines.concat(
            this.eachOptionIsADataLine(eventOptions),
            this.eachNewlineIsADataLine('script', script));

        return this.send('datastar-execute-script', dataLines, { eventId, retryDuration });
    }

    abstract readSignals(request: any): Promise<
        { success: true, signals: Record<string, unknown> }
        | { success: false, error: string }
    >
}
