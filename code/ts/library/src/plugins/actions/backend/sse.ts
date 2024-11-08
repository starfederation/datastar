import { DATASTAR } from "library/src/engine/const";
import {
    ActionMethod,
    ActionPlugin,
    AttributeContext,
    ExpressionFunction,
} from "library/src/engine/types";
import {
    INDICATOR_CLASS,
    INDICATOR_LOADING_CLASS,
} from "library/src/plugins/attributes/backend/fetch-indicator";
import {
    remoteSignals,
    storeFromPossibleContents,
} from "library/src/utils/signals";
import {
    docWithViewTransitionAPI,
    supportsViewTransitions,
} from "library/src/utils/view-transitions";
import {
    fetchEventSource,
    FetchEventSourceInit,
} from "library/src/vendored/fetch-event-source";
import { idiomorph } from "library/src/vendored/idiomorph";
import { Signal } from "library/src/vendored/preact-core";

const DEFAULT_MERGE: FragmentMergeOption = "morph";
const DEFAULT_SETTLE_TIME = 500;
const DEFAULT_USE_VIEW_TRANSITION = false;

const FragmentMergeOptions = {
    MorphElement: "morph",
    InnerElement: "inner",
    OuterElement: "outer",
    PrependElement: "prepend",
    AppendElement: "append",
    BeforeElement: "before",
    AfterElement: "after",
    UpsertAttributes: "upsert_attributes",
} as const;
export type FragmentMergeOption =
    (typeof FragmentMergeOptions)[keyof typeof FragmentMergeOptions];

export const GetSSEActionPlugin: ActionPlugin = {
    pluginType: "action",
    name: "get",
    method: fetcherActionMethod("GET"),
};

export const PostSSEActionPlugin: ActionPlugin = {
    pluginType: "action",
    name: "post",
    method: fetcherActionMethod("POST"),
};

export const PutSSEActionPlugin: ActionPlugin = {
    pluginType: "action",
    name: "put",
    method: fetcherActionMethod("PUT"),
};

export const PatchSSEActionPlugin: ActionPlugin = {
    pluginType: "action",
    name: "patch",
    method: fetcherActionMethod("PATCH"),
};

export const DeleteSSEActionPlugin: ActionPlugin = {
    pluginType: "action",
    name: "delete",
    method: fetcherActionMethod("DELETE"),
};

export function fetcherActionMethod(method: string): ActionMethod {
    return (ctx, urlExpression, onlyRemoteRaw) => {
        const onlyRemotes = ["true", true, undefined].includes(onlyRemoteRaw);
        fetcher(method, urlExpression, ctx, onlyRemotes);
    };
}

type IndicatorReference = { el: HTMLElement; count: number };

async function fetcher(
    method: string,
    urlExpression: string,
    ctx: AttributeContext,
    onlyRemote = true,
) {
    const store = ctx.store();

    if (!urlExpression) {
        throw new Error(`No signal for ${method} on ${urlExpression}`);
    }

    let storeValue = { ...store.value };
    if (onlyRemote) storeValue = remoteSignals(storeValue);
    const storeJSON = JSON.stringify(storeValue);

    const loadingTarget = ctx.el as HTMLElement;

    ctx.sendDatastarEvent(
        "plugin",
        "backend",
        "fetch_start",
        loadingTarget,
        JSON.stringify({ method, urlExpression, onlyRemote, storeJSON }),
    );
    const indicatorElements: HTMLElement[] =
        store?._dsPlugins?.fetch?.indicatorElements
            ? store._dsPlugins.fetch.indicatorElements[loadingTarget.id]
                ?.value || []
            : [];
    const indicatorsVisible: Signal<IndicatorReference[]> | undefined = store
        ?._dsPlugins.fetch?.indicatorsVisible;
    if (!!indicatorElements?.forEach) {
        indicatorElements.forEach((indicator) => {
            if (!indicator || !indicatorsVisible) return;
            const indicatorVisibleIndex = indicatorsVisible.value.findIndex(
                (indicatorVisible) => {
                    if (!indicatorVisible) return false;
                    return indicator.isSameNode(indicatorVisible.el);
                },
            );
            if (indicatorVisibleIndex > -1) {
                const indicatorVisible =
                    indicatorsVisible.value[indicatorVisibleIndex];
                const indicatorsVisibleNew = [...indicatorsVisible.value];
                delete indicatorsVisibleNew[indicatorVisibleIndex];
                indicatorsVisible.value = [
                    ...indicatorsVisibleNew.filter((ind) => {
                        return !!ind;
                    }),
                    { el: indicator, count: indicatorVisible.count + 1 },
                ];
            } else {
                indicator.classList.remove(INDICATOR_CLASS);
                indicator.classList.add(INDICATOR_LOADING_CLASS);
                indicatorsVisible.value = [
                    ...indicatorsVisible.value,
                    {
                        el: indicator,
                        count: 1,
                    },
                ];
            }
        });
    }

    const isWrongContent = (err: any) =>
        `${err}`.includes(
            `Expected content-type to be text/event-stream`,
        );

    const url = new URL(urlExpression, window.location.origin);
    method = method.toUpperCase();
    const req: FetchEventSourceInit = {
        method,
        headers: {
            ["Content-Type"]: "application/json",
            [`${DATASTAR}-request`]: "true",
        },
        onmessage: (evt) => {
            if (!evt.event) return;
            else if (!evt.event.startsWith(DATASTAR)) {
                console.log(`Unknown event: ${evt.event}`);
                debugger;
            }

            switch (evt.event) {
                case `${DATASTAR}-fragment`:
                    const lines = evt.data.trim().split("\n");
                    const knownEventTypes = [
                        "selector",
                        "merge",
                        "settle",
                        "fragment",
                        "vt",
                    ];

                    let fragment = "",
                        merge = DEFAULT_MERGE,
                        settleTime = DEFAULT_SETTLE_TIME,
                        useViewTransition = DEFAULT_USE_VIEW_TRANSITION,
                        exists = false,
                        selector = "",
                        currentDatatype = "";

                    for (let i = 0; i < lines.length; i++) {
                        let line = lines[i];
                        if (!line?.length) continue;

                        const firstWord = line.split(" ", 1)[0];
                        const isDatatype = knownEventTypes.includes(firstWord);
                        const isNewDatatype = isDatatype &&
                            firstWord !== currentDatatype;
                        if (isNewDatatype) {
                            currentDatatype = firstWord;
                            line = line.slice(firstWord.length + 1);

                            switch (currentDatatype) {
                                case "selector":
                                    selector = line;
                                    break;
                                case "merge":
                                    merge = line as FragmentMergeOption;
                                    exists = Object.values(FragmentMergeOptions)
                                        .includes(merge);
                                    if (!exists) {
                                        throw new Error(
                                            `Unknown merge option: ${merge}`,
                                        );
                                    }
                                    break;
                                case "settle":
                                    settleTime = parseInt(line);
                                    break;
                                case "fragment":
                                    break;
                                case "vt":
                                    useViewTransition = line === "true";
                                    break;
                                default:
                                    throw new Error(`Unknown data type`);
                            }
                        }

                        if (currentDatatype === "fragment") {
                            fragment += line + "\n";
                        }
                    }

                    if (!fragment?.length) fragment = "<div></div>";
                    mergeHTMLFragment(
                        ctx,
                        selector,
                        merge,
                        fragment,
                        settleTime,
                        useViewTransition,
                    );
                    ctx.sendDatastarEvent(
                        "plugin",
                        "backend",
                        "merge",
                        selector,
                        JSON.stringify({
                            fragment,
                            settleTime,
                            useViewTransition,
                        }),
                    );

                    break;

                case `${DATASTAR}-signal`:
                    let onlyIfMissing = false;
                    let storeToMerge = "";

                    const signalLines = evt.data.trim().split("\n");
                    for (let i = 0; i < signalLines.length; i++) {
                        const line = signalLines[i];
                        const [signalType, ...signalRest] = line.split(" ");
                        const signalLine = signalRest.join(" ");
                        switch (signalType) {
                            case "onlyIfMissing":
                                onlyIfMissing = signalLine.trim() === "true";
                                break;
                            case "store":
                                storeToMerge += `${signalLine}\n`;
                                break;
                            default:
                                throw new Error(
                                    `Unknown signal type: ${signalType}`,
                                );
                        }
                    }

                    const fnContents =
                        ` return Object.assign({...ctx.store()}, ${storeToMerge})`;
                    try {
                        const fn = new Function(
                            "ctx",
                            fnContents,
                        ) as ExpressionFunction;
                        const possibleMergeStore = fn(ctx);
                        const actualMergeStore = storeFromPossibleContents(
                            ctx.store(),
                            possibleMergeStore,
                            onlyIfMissing,
                        );
                        ctx.mergeStore(actualMergeStore);
                        ctx.applyPlugins(document.body);
                    } catch (e) {
                        console.log(fnContents);
                        console.error(e);
                        debugger;
                    }
                    break;

                case `${DATASTAR}-remove`:
                    const [removePrefix, ...removeRest] = evt.data.trim().split(
                        " ",
                    );

                    switch (removePrefix) {
                        case "selector":
                            const removeSelector = removeRest.join(" ");
                            const removeTargets = document.querySelectorAll(
                                removeSelector,
                            );
                            removeTargets.forEach((target) => target.remove());
                            break;
                        case "paths":
                            const paths = removeRest.join(" ").split(" ");
                            ctx.removeFromStore(...paths);
                            break;
                        default:
                            throw new Error(
                                `Unknown delete prefix: ${removePrefix}`,
                            );
                    }
                    break;

                case `${DATASTAR}-redirect`:
                    const [redirectSelector, ...redirectRest] = evt.data.trim()
                        .split(" ");
                    if (redirectSelector !== "url") {
                        throw new Error(
                            `Unknown redirect selector: ${redirectSelector}`,
                        );
                    }
                    const redirectTarget = redirectRest.join(" ");
                    ctx.sendDatastarEvent(
                        "plugin",
                        "backend",
                        "redirect",
                        "WINDOW",
                        redirectTarget,
                    );
                    window.location.href = redirectTarget;
                    break;

                case `${DATASTAR}-console`:
                    const [consoleMode, ...consoleRest] = evt.data.trim().split(
                        " ",
                    );
                    const consoleMessage = consoleRest.join(" ");
                    switch (consoleMode) {
                        case "assert":
                        case "clear":
                        case "count":
                        case "countReset":
                        case "debug":
                        case "dir":
                        case "dirxml":
                        case "error":
                        case "group":
                        case "groupCollapsed":
                        case "groupEnd":
                        case "info":
                        case "log":
                        case "table":
                        case "time":
                        case "timeEnd":
                        case "timeLog":
                        case "trace":
                        case "warn":
                            const fn = console[consoleMode] as Function;
                            fn(consoleMessage);
                            break;
                        default:
                            throw new Error(
                                `Unknown console mode: '${consoleMode}', message: '${consoleMessage}'`,
                            );
                    }
            }
        },
        onerror: (err) => {
            if (isWrongContent(err)) {
                // don't retry if the content-type is wrong
                throw err;
            }

            // do nothing and it will retry
        },
        onclose: () => {
            try {
                const store = ctx.store();
                const indicatorsVisible: Signal<IndicatorReference[]> =
                    store?._dsPlugins?.fetch?.indicatorsVisible || [];
                const indicatorElements: HTMLElement[] =
                    store?._dsPlugins?.fetch?.indicatorElements
                        ? store._dsPlugins.fetch
                            .indicatorElements[loadingTarget.id]?.value || []
                        : [];
                const indicatorCleanupPromises: Promise<() => void>[] = [];
                if (indicatorElements?.forEach) {
                    indicatorElements.forEach((indicator) => {
                        if (!indicator || !indicatorsVisible) return;
                        const indicatorsVisibleNew = indicatorsVisible.value;
                        const indicatorVisibleIndex = indicatorsVisibleNew
                            .findIndex((indicatorVisible) => {
                                if (!indicatorVisible) return false;
                                return indicator.isSameNode(
                                    indicatorVisible.el,
                                );
                            });
                        const indicatorVisible =
                            indicatorsVisibleNew[indicatorVisibleIndex];
                        if (!indicatorVisible) return;
                        if (indicatorVisible.count < 2) {
                            indicatorCleanupPromises.push(
                                new Promise(() =>
                                    setTimeout(() => {
                                        indicator.classList.remove(
                                            INDICATOR_LOADING_CLASS,
                                        );
                                        indicator.classList.add(
                                            INDICATOR_CLASS,
                                        );
                                    }, 300)
                                ),
                            );
                            delete indicatorsVisibleNew[indicatorVisibleIndex];
                        } else if (indicatorVisibleIndex > -1) {
                            indicatorsVisibleNew[indicatorVisibleIndex].count =
                                indicatorsVisibleNew[indicatorVisibleIndex]
                                    .count - 1;
                        }
                        indicatorsVisible.value = indicatorsVisibleNew.filter(
                            (ind) => {
                                return !!ind;
                            },
                        );
                    });
                }

                Promise.all(indicatorCleanupPromises);
            } catch (e) {
                console.error(e);
                debugger;
            } finally {
                ctx.sendDatastarEvent(
                    "plugin",
                    "backend",
                    "fetch_end",
                    loadingTarget,
                    JSON.stringify({ method, urlExpression }),
                );
            }
        },
    };

    if (method === "GET") {
        const queryParams = new URLSearchParams(url.search);
        queryParams.append("datastar", storeJSON);
        url.search = queryParams.toString();
    } else {
        req.body = storeJSON;
    }

    const headers = store?._dsPlugins?.fetch?.headers || {};
    if (req.headers) {
        for (const [key, value] of Object.entries(headers)) {
            if (key.startsWith("_")) continue;
            req.headers[key] = `${value}`;
        }
    }

    try {
        await fetchEventSource(url.toString(), req);
    } catch (err) {
        if (!isWrongContent(err)) {
            throw err;
        }

        // exit gracefully and do nothing if the content-type is wrong
        // this can happen if the client is sending a request
        // where no response is expected and they haven't
        // set the content-type to text/event-stream
    }
}

const SETTLING_CLASS = `${DATASTAR}-settling`;
const SWAPPING_CLASS = `${DATASTAR}-swapping`;

const fragContainer = document.createElement("template");
export function mergeHTMLFragment(
    ctx: AttributeContext,
    selector: string,
    merge: FragmentMergeOption,
    fragmentsRaw: string,
    settleTime: number,
    useViewTransition: boolean,
) {
    const { el } = ctx;

    fragContainer.innerHTML = fragmentsRaw.trim();
    const frags = [...fragContainer.content.children];
    frags.forEach((frag) => {
        if (!(frag instanceof Element)) {
            throw new Error(`No fragment found`);
        }
        const applyToTargets = (capturedTargets: Element[]) => {
            for (const initialTarget of capturedTargets) {
                initialTarget.classList.add(SWAPPING_CLASS);
                const originalHTML = initialTarget.outerHTML;
                let modifiedTarget = initialTarget;
                switch (merge) {
                    case FragmentMergeOptions.MorphElement:
                        const result = idiomorph(modifiedTarget, frag, {
                            callbacks: {
                                beforeNodeRemoved: (
                                    oldNode: Element,
                                    _: Element,
                                ) => {
                                    ctx.cleanupElementRemovals(oldNode);
                                    return true;
                                },
                            },
                        });
                        if (!result?.length) {
                            throw new Error(`No morph result `);
                        }
                        const first = result[0] as Element;
                        modifiedTarget = first;
                        break;
                    case FragmentMergeOptions.InnerElement:
                        // Replace the contents of the target element with the response
                        modifiedTarget.innerHTML = frag.innerHTML;
                        break;
                    case FragmentMergeOptions.OuterElement:
                        // Replace the entire target element with the response
                        modifiedTarget.replaceWith(frag);
                        break;
                    case FragmentMergeOptions.PrependElement:
                        modifiedTarget.prepend(frag); //  Insert the response before the first child of the target element
                        break;
                    case FragmentMergeOptions.AppendElement:
                        modifiedTarget.append(frag); //  Insert the response after the last child of the target element
                        break;
                    case FragmentMergeOptions.BeforeElement:
                        modifiedTarget.before(frag); //  Insert the response before the target element
                        break;
                    case FragmentMergeOptions.AfterElement:
                        modifiedTarget.after(frag); //  Insert the response after the target element
                        break;
                    case FragmentMergeOptions.UpsertAttributes:
                        //  Upsert the attributes of the target element
                        frag.getAttributeNames().forEach((attrName) => {
                            const value = frag.getAttribute(attrName)!;
                            modifiedTarget.setAttribute(attrName, value);
                        });
                        break;
                    default:
                        throw new Error(`Unknown merge type: ${merge}`);
                }
                ctx.cleanupElementRemovals(modifiedTarget);
                modifiedTarget.classList.add(SWAPPING_CLASS);

                ctx.applyPlugins(document.body);

                setTimeout(() => {
                    initialTarget.classList.remove(SWAPPING_CLASS);
                    modifiedTarget.classList.remove(SWAPPING_CLASS);
                }, settleTime);

                const revisedHTML = modifiedTarget.outerHTML;

                if (originalHTML !== revisedHTML) {
                    modifiedTarget.classList.add(SETTLING_CLASS);
                    setTimeout(() => {
                        modifiedTarget.classList.remove(SETTLING_CLASS);
                    }, settleTime);
                }
            }
        };

        const useElAsTarget = selector === "self";

        let targets: Iterable<Element>;
        if (useElAsTarget) {
            targets = [el];
        } else {
            const selectorOrID = selector || `#${frag.getAttribute("id")}`;
            targets = document.querySelectorAll(selectorOrID) || [];
            if (!!!targets) {
                throw new Error(`No targets found for ${selectorOrID}`);
            }
        }
        const allTargets = [...targets];
        if (!allTargets.length) {
            throw new Error(`No targets found for ${selector}`);
        }

        if (supportsViewTransitions && useViewTransition) {
            docWithViewTransitionAPI.startViewTransition(() =>
                applyToTargets(allTargets)
            );
        } else {
            applyToTargets(allTargets);
        }
    });
}
