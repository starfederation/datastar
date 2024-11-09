import { expect, describe, test } from 'vitest'
import fc from 'fast-check';

import * as Event from "./console-sugar.ts";

const doesNotChangeMessage = (func) => {
    return fc.property(fc.array(fc.string({minLength: 1})), (data) => {
        data.forEach((msg) => {
            const event = func(msg);
            expect(event.message).toEqual(msg);
        });
    })
}

describe.each([
    Event.consoleError,
    Event.consoleWarn,
    Event.consoleInfo,
    Event.consoleLog,
    Event.consoleDebug,
    Event.consoleGroup,
])('console events with messages', (func) => {
    test('do not change message', () => fc.assert(
        doesNotChangeMessage(func)
    ));
});

test('consoleGroupEnd does not carry a message', () => {
    fc.assert(
      fc.property(fc.array(fc.integer()), (nbTimes) => {
        nbTimes.forEach((_) => {
            const event = Event.consoleGroupEnd();
            expect(event.message).toEqual("");
        });
      })
    );
});
