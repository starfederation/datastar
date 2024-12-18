import {
    EventType,
    DatastarEventOptions,
    MergeSignalsOptions,
    MergeFragmentsOptions,
    FragmentOptions,
    ExecuteScriptOptions,
} from "./types.ts";

export abstract class ServerSentEventGenerator {
    // runtimes should override this method to create an sse stream
    protected constructor() {

    }

    // runtimes should override this method and use it's output to send an event
    protected send(
         event: EventType,
         dataLines: string[],
         options: DatastarEventOptions
    ): string[] {
        const { id, retryDuration } = options || {};

        const typeLine = [`event: ${event}\n`];
        const idLine = id ? [`id: ${id}\n`] : [];
        const retryLine = retryDuration ? [`retry: ${retryDuration}\n`] : [];

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
        const [ head, ...tail] = data.split('\n');

        return [ `${prefix} ${head}`].concat(tail);
    }

    private eachOptionIsADataLine(options: Record<string, any>): string[] {
        return Object.keys(options).map((key) => {
            return `${key} ${options}`;
        });
    }

    public mergeFragments(data: string, options?: MergeFragmentsOptions): ReturnType<typeof this.send> {
        const { id, retryDuration, ...renderOptions } = options || {} as Partial<MergeFragmentsOptions>;
        const dataLines = this.eachOptionIsADataLine(renderOptions)
            .concat(this.eachNewlineIsADataLine('fragments', data));

        return this.send('datastar-merge-fragments', dataLines, { id, retryDuration });
    }

    public removeFragments(selector: string, options?: FragmentOptions): ReturnType<typeof this.send> {
        const { id, retryDuration, ...eventOptions } = options || {} as Partial<FragmentOptions>;
        const dataLines = this.eachOptionIsADataLine(eventOptions)
            .concat([`selector ${selector}`]);

        return this.send('datastar-remove-fragments', dataLines, { id, retryDuration });
    }

    public mergeSignals(data: Record<string, any>, options?: MergeSignalsOptions): ReturnType<typeof this.send> {
        const { id, retryDuration, ...eventOptions } = options || {} as Partial<MergeSignalsOptions>;
        const dataLines = this.eachOptionIsADataLine(eventOptions)
            .concat(this.eachNewlineIsADataLine('signals', JSON.stringify(data)));

        return this.send('datastar-merge-signals', dataLines, { id, retryDuration });
    }

    public removeSignals(paths: string[], options?: DatastarEventOptions): ReturnType<typeof this.send> {
        const eventOptions = options || {} as DatastarEventOptions;
        const dataLines = [`paths ${paths.join(' ')}`];

        return this.send('datastar-remove-signals', dataLines, eventOptions);
    }

    public executeScript(script: string, options?: ExecuteScriptOptions): ReturnType<typeof this.send> {
        const { id, retryDuration, ...eventOptions } = options || {} as Partial<ExecuteScriptOptions>;
        const dataLines = this.eachOptionIsADataLine(eventOptions)
            .concat(this.eachNewlineIsADataLine('script', script));

        return this.send('datastar-execute-script', dataLines, { id, retryDuration });
    }

    abstract readSignals(request: any): Record<string, unknown>;
}
