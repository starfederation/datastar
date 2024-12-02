// Authors: Delaney Gillilan
// Icon: ic:baseline-format-paint
// Slug: Add or remove classes from an element reactively
// Description: This action adds or removes classes from an element reactively based on the expression provided. The expression should be an object where the keys are the class names and the values are booleans. If the value is true, the class is added. If the value is false, the class is removed.

import { AttributePlugin, PluginType } from "../../../../engine/types";
import { kebabize } from "../../../../utils/text";

export const Class: AttributePlugin = {
    type: PluginType.Attribute,
    name: "class",
    onLoad: ({ key, el, rx, reactivity: { effect } }) => {
        const cl = el.classList;
        return effect(() => {
            if (key === "") {
                const classes: Object = rx<Record<string, boolean>>();
                for (const [k, v] of Object.entries(classes)) {
                    const classNames = k.split(" ");
                    if (v) {
                        cl.add(...classNames);
                    } else {
                        cl.remove(...classNames);
                    }
                }
            } else {
                const shouldInclude = rx<boolean>();
                const cls = kebabize(key);
                if (shouldInclude) {
                    cl.add(cls);
                } else {
                    cl.remove(cls);
                }
            }
        });
    },
};
