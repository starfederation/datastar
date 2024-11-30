// Authors: Delaney Gillilan
// Icon: tabler:typography
// Slug: Set the text content of an element
// Description: This attribute sets the text content of an element to the result of the expression.

import { AttributePlugin } from "../../../../engine";
import { PluginType } from "../../../../engine/enums";
import { ERR_BAD_ARGS } from "../../../../engine/errors";

export const Text: AttributePlugin = {
    pluginType: PluginType.Attribute,
    name: "text",
    mustHaveEmptyKey: true,
    onLoad: (ctx) => {
        const { el, expressionFn } = ctx;
        if (!(el instanceof HTMLElement)) {
            // NON_HTML_EL – The `data-{plugin}` attribute was placed on an invalid element. It can only be placed on HTML elements.
            throw ERR_BAD_ARGS;
        }
        return ctx.reactivity.effect(() => {
            const res = expressionFn(ctx);
            el.textContent = `${res}`;
        });
    },
};
