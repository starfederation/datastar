import { expect, test } from 'vitest'
import fc from 'fast-check';
import {
    EventType,
    DatastarEventOptions,
} from "./types.ts";
import {
    FragmentMergeModes
} from "./consts.ts";
import { ServerSentEventGenerator }  from "./abstractServerSentEventGenerator.ts";

class MockedServerSentEventGenerator extends ServerSentEventGenerator {
    public constructor() {
        super();
        // no setup needed
    }

    protected send(eventType: EventType, dataLines: string[], options: DatastarEventOptions): string[] {
        return super.send(eventType, dataLines, options);
    }

    public readSignals(request: string) {
        return JSON.parse(request);
    }
}

test('ServerSentEventGenerator can be instantiated', () => {
    const sseGenerator = new MockedServerSentEventGenerator();

    expect(sseGenerator).toBeInstanceOf(MockedServerSentEventGenerator);
});

expect.extend({
  anyItemStartsWith(actual: string[], required: string) {
      const pass = actual.some((line) => {
          return line.startsWith(required);
      });
      const message = () => `Expected an item of [${actual}] to start with ${required}`;

      return { pass, message };
  }
});

function containsAllKeys(
    eventType: EventType,
    event: Record<string, any>,
    res: string[]
) {
    const { id, retryDuration, ...eventOptions } = event;

    expect(res).anyItemStartsWith(`event: ${eventType}`);

    if (id) {
        expect(res).anyItemStartsWith(`id: ${id}`);
    }

    if (retryDuration) {
        expect(res).anyItemStartsWith(`retry: ${retryDuration}`);
    }

    Object.keys(eventOptions).forEach((key) => {
        expect(res).anyItemStartsWith(`data: ${key}`);
    });
}

const mergeFragmentsRecord = fc.record({
    id: fc.string(),
    retryDuration: fc.nat(),
    fragments: fc.string(),
    selector: fc.string(),
    useViewTransition: fc.boolean(),
    settleDuration: fc.nat(),
    mergeMode: fc.constantFrom(...FragmentMergeModes)
}, {
    requiredKeys: ['fragments']
});

test('mergeFragments has valid line endings', () => fc.assert(
    fc.property(fc.array(mergeFragmentsRecord), (data) => {
        const sseGenerator = new MockedServerSentEventGenerator();
        data.forEach((event) => {
            const { fragments, ...options } = event;

            const res = sseGenerator.mergeFragments(fragments, options);

            res.forEach((line) => {
                expect(line).toMatch(/\n$/);
            });

            expect(res.at(-1)).toEqual("\n\n");
        });
    })
));

test('mergeFragments contains all provided keys', () => fc.assert(
    fc.property(fc.array(mergeFragmentsRecord), (data) => {
        const sseGenerator = new MockedServerSentEventGenerator();
        data.forEach((event) => {
            const { fragments, ...options } = event;

            const res = sseGenerator.mergeFragments(fragments, options);

            containsAllKeys('datastar-merge-fragments', event, res);
        });
    })
));

const removeFragmentsRecord = fc.record({
    id: fc.string(),
    retryDuration: fc.nat(),
    selector: fc.string(),
    useViewTransition: fc.boolean(),
    settleDuration: fc.nat()
}, {
    requiredKeys: ['selector']
});

test('removeFragments has valid line endings', () => fc.assert(
    fc.property(fc.array(removeFragmentsRecord), (data) => {
        const sseGenerator = new MockedServerSentEventGenerator();
        data.forEach((event) => {
            const { selector, ...options } = event;

            const res = sseGenerator.removeFragments(selector, options);

            res.forEach((line) => {
                expect(line).toMatch(/\n$/);
            });

            expect(res.at(-1)).toEqual("\n\n");
        });
    })
));

test('removeFragments contains all provided keys', () => fc.assert(
    fc.property(fc.array(removeFragmentsRecord), (data) => {
        const sseGenerator = new MockedServerSentEventGenerator();
        data.forEach((event) => {
            const { selector, ...options } = event;

            const res = sseGenerator.removeFragments(selector, options);

            containsAllKeys('datastar-remove-fragments', event, res);
        });
    })
));

const mergeSignalsRecord = fc.record({
    id: fc.string(),
    retryDuration: fc.nat(),
    data: fc.object(),
    onlyIfMissing: fc.boolean(),
}, {
    requiredKeys: ['data']
});

test('mergeSignals has valid line endings', () => fc.assert(
    fc.property(fc.array(mergeSignalsRecord), (data) => {
        const sseGenerator = new MockedServerSentEventGenerator();
        data.forEach((event) => {
            const { data, ...options } = event;

            const res = sseGenerator.mergeSignals(data, options);

            res.forEach((line) => {
                expect(line).toMatch(/\n$/);
            });

            expect(res.at(-1)).toEqual("\n\n");
        });
    })
));

test('mergeSignals contains all provided keys', () => fc.assert(
    fc.property(fc.array(mergeSignalsRecord), (data) => {
        const sseGenerator = new MockedServerSentEventGenerator();
        data.forEach((event) => {
            const { data, ...options } = event;

            const res = sseGenerator.mergeSignals(data, options);

            containsAllKeys('datastar-merge-signals', { signal: data, ...options }, res);
        });
    })
));

const removeSignalsRecord = fc.record({
    id: fc.string(),
    retryDuration: fc.nat(),
    paths: fc.array(fc.string()),
}, {
    requiredKeys: ['paths']
});

test('removeSignals has valid line endings', () => fc.assert(
    fc.property(fc.array(removeSignalsRecord), (data) => {
        const sseGenerator = new MockedServerSentEventGenerator();
        data.forEach((event) => {
            const { paths, ...options } = event;

            const res = sseGenerator.removeSignals(paths, options);

            res.forEach((line) => {
                expect(line).toMatch(/\n$/);
            });

            expect(res.at(-1)).toEqual("\n\n");
        });
    })
));

test('removeSignals contains all provided keys', () => fc.assert(
    fc.property(fc.array(removeSignalsRecord), (data) => {
        const sseGenerator = new MockedServerSentEventGenerator();
        data.forEach((event) => {
            const { paths, ...options } = event;

            const res = sseGenerator.removeSignals(paths, options);

            containsAllKeys('datastar-remove-signals', event, res);
        });
    })
));

const executeScriptRecord = fc.record({
    id: fc.string(),
    retryDuration: fc.nat(),
    script: fc.string(),
    attributes: fc.string(),
    autoRemove: fc.boolean(),
}, {
    requiredKeys: ['script']
});

test('executeScript has valid line endings', () => fc.assert(
    fc.property(fc.array(executeScriptRecord), (data) => {
        const sseGenerator = new MockedServerSentEventGenerator();
        data.forEach((event) => {
            const { script, ...options } = event;

            const res = sseGenerator.executeScript(script, options);

            res.forEach((line) => {
                expect(line).toMatch(/\n$/);
            });

            expect(res.at(-1)).toEqual("\n\n");
        });
    })
));

test('executeScript contains all provided keys', () => fc.assert(
    fc.property(fc.array(executeScriptRecord), (data) => {
        const sseGenerator = new MockedServerSentEventGenerator();
        data.forEach((event) => {
            const { script, ...options } = event;

            const res = sseGenerator.executeScript(script, options);

            containsAllKeys('datastar-execute-script', { script, ...options }, res);
        });
    })
));
