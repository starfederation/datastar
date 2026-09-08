import languageDataJson from './language-data.json'

type Requirement = 'allowed' | 'must' | 'denied' | 'exclusive'
type SignalKind = 'signal' | 'property' | 'computed' | 'reference'
type CompletionKind = 'signal' | 'property' | 'modifier' | 'action'

type Reference = {
    name: string
    url: string
}

type AttributeMetadata = {
    name: string
    description: string
    requirement: { key: Requirement; value: Requirement }
    modifiers: Array<{ name: string; description?: string }>
    signals?: 'key' | 'key-or-value' | 'key-or-object'
    valueKind: 'expression' | 'signal-name' | 'string'
    keys?: string[]
    element?: string
    highlight: boolean
    pro: boolean
}

type CompletionMetadata = {
    name: string
    pluginName: string
    insertText: string
    description: string
    references: Reference[]
}

type LanguageData = {
    attributes: AttributeMetadata[]
    completions: CompletionMetadata[]
    nativeEvents: string[]
    actions: ActionMetadata[]
}

type ActionMetadata = {
    name: string
    description: string
    signature: string
    parameters: Array<{ label: string }>
    pro: boolean
}

type AttributeRule = AttributeMetadata['requirement'] & {
    element?: string
    keys?: string[]
}

type Tag = {
    name: string
    start: number
    attributesStart: number
    attributesEnd: number
    closed: boolean
    end: number
}

type ParsedAttribute = {
    pluginName: string
    key?: string
    modifiers: string[]
    name: string
    start: number
    nameEnd: number
    end: number
    hasValue: boolean
    value: string
    valueStart?: number
    valueEnd?: number
    tagName: string
}

export type LanguageDiagnostic = {
    start: number
    end: number
    message: string
    code: string
}

export type LanguageCompletion = {
    label: string
    insertText: string
    description: string
    references: Reference[]
    kind?: CompletionKind
    snippet?: boolean
    detail?: string
    start: number
    end: number
}

export type LanguageSignature = {
    label: string
    description: string
    parameters: Array<{ label: string }>
    activeParameter: number
    pro: boolean
}

export type SignalDeclaration = {
    name: string
    kind: SignalKind
    start: number
    end: number
}

export type LanguageHover = {
    start: number
    end: number
    name: string
    description: string
    requirements: string[]
    references: Reference[]
}

const languageData = languageDataJson as LanguageData

const snippetEntries = languageData.completions;
const nativeEvents = languageData.nativeEvents;
const actionData = languageData.actions;
const attributeMetadata = new Map<string, AttributeMetadata>(languageData.attributes.map(attribute => [attribute.name, attribute]));
const knownPlugins = new Set(attributeMetadata.keys());
const ATTRIBUTE_RULES: Record<string, AttributeRule> = Object.fromEntries(languageData.attributes.map(attribute => [
    attribute.name,
    {
        ...attribute.requirement,
        element: attribute.element,
        keys: attribute.keys,
    },
]));
const KEY_SIGNAL_PLUGINS = new Set(languageData.attributes
    .filter(attribute => attribute.signals !== undefined
        && ['key', 'key-or-value', 'key-or-object'].includes(attribute.signals))
    .map(attribute => attribute.name));
const VALUE_SIGNAL_PLUGINS = new Set(languageData.attributes
    .filter(attribute => attribute.signals === 'key-or-value')
    .map(attribute => attribute.name));
const OBJECT_SIGNAL_PLUGINS = new Set(languageData.attributes
    .filter(attribute => attribute.signals === 'key-or-object')
    .map(attribute => attribute.name));
const NON_EXPRESSION_VALUE_PLUGINS = new Set(languageData.attributes
    .filter(attribute => attribute.valueKind !== 'expression')
    .map(attribute => attribute.name));

function getPluginName(attributeName: string): string {
    return attributeName.slice('data-'.length).split(':', 1)[0].split('__', 1)[0];
}

function parseAttributeName(attributeName: string): Pick<ParsedAttribute, 'pluginName' | 'key' | 'modifiers'> {
    const withoutPrefix = attributeName.slice('data-'.length);
    const [nameAndKey, ...modifiers] = withoutPrefix.split('__');
    const colonIndex = nameAndKey.indexOf(':');

    return {
        pluginName: colonIndex === -1 ? nameAndKey : nameAndKey.slice(0, colonIndex),
        key: colonIndex === -1 ? undefined : nameAndKey.slice(colonIndex + 1),
        modifiers,
    };
}

function findTags(text: string): Tag[] {
    const tags: Tag[] = [];
    let cursor = 0;

    while (cursor < text.length) {
        const start = text.indexOf('<', cursor);
        if (start === -1) break;

        if (text.startsWith('<!--', start)) {
            const commentEnd = text.indexOf('-->', start + 4);
            cursor = commentEnd === -1 ? text.length : commentEnd + 3;
            continue;
        }

        const tagMatch = text.slice(start).match(/^<\s*([A-Za-z][\w:-]*)/);
        if (!tagMatch) {
            cursor = start + 1;
            continue;
        }

        let quote: string | undefined;
        let end = start + tagMatch[0].length;
        for (; end < text.length; end++) {
            const char = text[end];
            if (quote) {
                if (char === quote && text[end - 1] !== '\\') quote = undefined;
            } else if (char === '"' || char === "'") {
                quote = char;
            } else if (char === '>') {
                break;
            }
        }

        const closed = end < text.length;
        const attributesEnd = closed ? end : text.length;
        tags.push({
            name: tagMatch[1].toLowerCase(),
            start,
            attributesStart: start + tagMatch[0].length,
            attributesEnd,
            closed,
            end: closed ? end + 1 : text.length,
        });

        if (closed && ['script', 'style'].includes(tagMatch[1].toLowerCase())) {
            const closingTag = text.toLowerCase().indexOf(`</${tagMatch[1].toLowerCase()}`, end + 1);
            cursor = closingTag === -1 ? text.length : closingTag;
        } else {
            cursor = closed ? end + 1 : text.length;
        }
    }

    return tags;
}

function findAttributes(text: string, tag: Tag): ParsedAttribute[] {
    const source = text.slice(tag.attributesStart, tag.attributesEnd);
    const attributePattern = /\b(data-[a-z][a-z0-9-]*(?::[a-zA-Z0-9_$.*-]+)?(?:__[a-z][a-z0-9-]*(?:\.[a-z0-9-]+)*)*)\s*(?:=\s*(?:(["'])([\s\S]*?)\2|([^\s>]+)))?/gi;
    const attributes: ParsedAttribute[] = [];
    let match: RegExpExecArray | null;

    while ((match = attributePattern.exec(source))) {
        const start = tag.attributesStart + match.index;
        const name = match[1];
        const parsed = parseAttributeName(name);
        const equalsIndex = match[0].indexOf('=');
        let value = match[3] ?? match[4] ?? '';
        let valueStart: number | undefined;
        let valueEnd: number | undefined;

        if (equalsIndex !== -1) {
            const afterEquals = match[0].slice(equalsIndex + 1);
            const whitespaceLength = afterEquals.length - afterEquals.trimStart().length;
            const tokenStart = start + equalsIndex + 1 + whitespaceLength;
            if (match[2]) {
                valueStart = tokenStart + 1;
            } else if (value.startsWith('"') || value.startsWith("'")) {
                value = value.slice(1);
                valueStart = tokenStart + 1;
            } else {
                valueStart = tokenStart;
            }
            valueEnd = valueStart + value.length;
        }

        attributes.push({
            ...parsed,
            name,
            start,
            nameEnd: start + name.length,
            end: start + match[0].length,
            hasValue: equalsIndex !== -1,
            value,
            valueStart,
            valueEnd,
            tagName: tag.name,
        });
    }

    return attributes;
}

export function parseDocument(text: string): ParsedAttribute[] {
    return findTags(text).flatMap(tag => findAttributes(text, tag));
}

function createDiagnostic(attribute: ParsedAttribute, message: string, code: string): LanguageDiagnostic {
    return {
        start: attribute.start,
        end: attribute.nameEnd,
        message,
        code,
    };
}

export function getDiagnostics(text: string, customAttributes: string[] = []): LanguageDiagnostic[] {
    const customPlugins = new Set(customAttributes);
    const diagnostics: LanguageDiagnostic[] = [];

    for (const attribute of parseDocument(text)) {
        if (!knownPlugins.has(attribute.pluginName) || customPlugins.has(attribute.pluginName)) {
            continue;
        }

        const rule = ATTRIBUTE_RULES[attribute.pluginName];
        if (!rule) continue;

        const keyProvided = attribute.key !== undefined && attribute.key !== '';
        const valueProvided = attribute.hasValue && attribute.value !== '';

        if (rule.element && attribute.tagName !== rule.element) {
            diagnostics.push(createDiagnostic(
                attribute,
                `${attribute.name} is only valid on the <${rule.element}> element.`,
                'invalid-element',
            ));
        }

        if (rule.key === 'must' && !keyProvided) {
            diagnostics.push(createDiagnostic(attribute, `${attribute.name} requires a key after ":".`, 'key-required'));
        } else if (rule.key === 'denied' && keyProvided) {
            diagnostics.push(createDiagnostic(attribute, `${attribute.pluginName} does not accept a key.`, 'key-not-allowed'));
        } else if (rule.keys && keyProvided && !rule.keys.includes(attribute.key!)) {
            diagnostics.push(createDiagnostic(
                attribute,
                `${attribute.pluginName} only accepts the following keys: ${rule.keys.join(', ')}.`,
                'key-not-allowed',
            ));
        }

        if (rule.key === 'exclusive') {
            if (keyProvided === valueProvided) {
                diagnostics.push(createDiagnostic(
                    attribute,
                    `${attribute.pluginName} requires either a key or a value, but not both.`,
                    'exclusive-key-value',
                ));
            }
        } else if (rule.value === 'must' && !valueProvided) {
            diagnostics.push(createDiagnostic(attribute, `${attribute.name} requires a value.`, 'value-required'));
        } else if (rule.value === 'denied' && valueProvided) {
            diagnostics.push(createDiagnostic(attribute, `${attribute.pluginName} does not accept a value.`, 'value-not-allowed'));
        }
    }

    return diagnostics;
}

function isInsideQuotedValue(text: string, tag: Tag, offset: number): boolean {
    let quote: string | undefined;
    for (let index = tag.attributesStart; index < offset; index++) {
        const char = text[index];
        if (quote) {
            if (char === quote && text[index - 1] !== '\\') quote = undefined;
        } else if (char === '"' || char === "'") {
            quote = char;
        }
    }
    return quote !== undefined;
}

function applySignalCase(name: string, modifiers: string[]): string {
    const caseModifier = modifiers.find(modifier => modifier.startsWith('case.'));
    const selectedCase = caseModifier?.slice('case.'.length) || 'camel';

    if (selectedCase === 'snake') return name.replace(/-/g, '_');
    if (selectedCase === 'pascal') {
        const camelName = name.replace(/-[a-z]/g, match => match[1].toUpperCase());
        return camelName[0]?.toUpperCase() + camelName.slice(1);
    }
    if (selectedCase === 'kebab') return name;
    return name.replace(/-[a-z]/g, match => match[1].toUpperCase());
}

function skipQuoted(source: string, start: number): number {
    const quote = source[start];
    let index = start + 1;
    while (index < source.length) {
        if (source[index] === '\\') {
            index += 2;
        } else if (source[index] === quote) {
            return index + 1;
        } else {
            index++;
        }
    }
    return index;
}

function skipWhitespace(source: string, start: number): number {
    let index = start;
    while (/\s/.test(source[index] || '')) index++;
    return index;
}

function skipExpression(source: string, start: number): number {
    const pairs: Record<string, string> = { '(': ')', '[': ']', '{': '}' };
    const closing: string[] = [];
    let index = start;

    while (index < source.length) {
        const char = source[index];
        if (char === '"' || char === "'" || char === '`') {
            index = skipQuoted(source, index);
            continue;
        }
        if (pairs[char]) {
            closing.push(pairs[char]);
        } else if (closing[closing.length - 1] === char) {
            closing.pop();
        } else if (closing.length === 0 && (char === ',' || char === '}')) {
            break;
        }
        index++;
    }

    return index;
}

function collectObjectSignals(
    source: string,
    sourceOffset: number,
    start = 0,
    prefix: string[] = [],
    signals: SignalDeclaration[] = [],
): { index: number; signals: SignalDeclaration[] } {
    let index = skipWhitespace(source, start);
    if (source[index] !== '{') return { index, signals };
    index++;

    while (index < source.length) {
        index = skipWhitespace(source, index);
        while (source[index] === ',') index = skipWhitespace(source, index + 1);
        if (source[index] === '}') return { index: index + 1, signals };
        if (source.startsWith('...', index)) {
            index = skipExpression(source, index + 3);
            continue;
        }

        const keyStart = index;
        let key: string;
        if (source[index] === '"' || source[index] === "'") {
            const keyEnd = skipQuoted(source, index);
            key = source.slice(index + 1, Math.max(index + 1, keyEnd - 1));
            index = keyEnd;
        } else {
            const keyMatch = source.slice(index).match(/^[A-Za-z_$][\w$-]*/);
            if (!keyMatch) {
                index = skipExpression(source, index);
                if (source[index] === ',') index++;
                continue;
            }
            key = keyMatch[0];
            index += key.length;
        }

        index = skipWhitespace(source, index);
        if (source[index] !== ':') {
            index = skipExpression(source, index);
            if (source[index] === ',') index++;
            continue;
        }

        const path = [...prefix, key].join('.');
        signals.push({
            name: path,
            kind: prefix.length === 0 ? 'signal' : 'property',
            start: sourceOffset + keyStart,
            end: sourceOffset + keyStart + key.length,
        });

        index = skipWhitespace(source, index + 1);
        if (source[index] === '{') {
            index = collectObjectSignals(source, sourceOffset, index, [...prefix, key], signals).index;
        } else {
            index = skipExpression(source, index);
        }
        if (source[index] === ',') index++;
    }

    return { index, signals };
}

export function collectSignalDeclarations(text: string): SignalDeclaration[] {
    const declarations = new Map<string, SignalDeclaration>();
    const add = (declaration: SignalDeclaration) => {
        if (declaration.name && !declarations.has(declaration.name)) {
            declarations.set(declaration.name, declaration);
        }
    };

    for (const attribute of parseDocument(text)) {
        if (attribute.key && KEY_SIGNAL_PLUGINS.has(attribute.pluginName)) {
            const name = applySignalCase(attribute.key, attribute.modifiers);
            const keyStart = attribute.start + `data-${attribute.pluginName}:`.length;
            add({
                name,
                kind: attribute.pluginName === 'computed' ? 'computed' : 'signal',
                start: keyStart,
                end: keyStart + attribute.key.length,
            });
        }

        if (VALUE_SIGNAL_PLUGINS.has(attribute.pluginName) && !attribute.key && attribute.value.trim()) {
            const leadingWhitespace = attribute.value.length - attribute.value.trimStart().length;
            const rawName = attribute.value.trim();
            add({
                name: applySignalCase(rawName, attribute.modifiers),
                kind: attribute.pluginName === 'ref' ? 'reference' : 'signal',
                start: attribute.valueStart! + leadingWhitespace,
                end: attribute.valueStart! + leadingWhitespace + rawName.length,
            });
        }

        if (OBJECT_SIGNAL_PLUGINS.has(attribute.pluginName) && !attribute.key && attribute.valueStart !== undefined) {
            for (const declaration of collectObjectSignals(attribute.value, attribute.valueStart).signals) {
                add({
                    ...declaration,
                    kind: attribute.pluginName === 'computed' ? 'computed' : declaration.kind,
                });
            }
        }
    }

    return [...declarations.values()];
}

function expressionAttributeAt(text: string, offset: number): ParsedAttribute | undefined {
    return parseDocument(text).find(candidate => (
        candidate.valueStart !== undefined
        && candidate.valueEnd !== undefined
        && candidate.valueStart <= offset
        && offset <= candidate.valueEnd
    ));
}

function getSignalCompletions(
    text: string,
    offset: number,
    attribute: ParsedAttribute,
): LanguageCompletion[] | undefined {
    if (!attribute || NON_EXPRESSION_VALUE_PLUGINS.has(attribute.pluginName)) return undefined;

    const beforeCursor = text.slice(attribute.valueStart, offset);
    const referenceMatch = beforeCursor.match(/\$[A-Za-z0-9_-]*(?:\.[A-Za-z0-9_-]*)*$/);
    if (!referenceMatch) return [];

    const declarations = collectSignalDeclarations(text);
    const reference = referenceMatch[0].slice(1);
    const lastDot = reference.lastIndexOf('.');
    const parent = lastDot === -1 ? '' : reference.slice(0, lastDot);
    const typedPart = lastDot === -1 ? reference : reference.slice(lastDot + 1);
    const candidates = new Map<string, SignalDeclaration>();

    for (const declaration of declarations) {
        const parts = declaration.name.split('.');
        if (!parent) {
            const root = parts[0];
            if (!candidates.has(root)) candidates.set(root, declaration);
        } else if (declaration.name.startsWith(`${parent}.`)) {
            const remainder = declaration.name.slice(parent.length + 1);
            const property = remainder.split('.')[0];
            if (!candidates.has(property)) candidates.set(property, declaration);
        }
    }

    const start = offset - typedPart.length - (lastDot === -1 ? 1 : 0);
    return [...candidates].map(([name, declaration]) => ({
        label: lastDot === -1 ? `$${name}` : name,
        insertText: lastDot === -1 ? `$${name}` : name,
        description: `${declaration.kind === 'computed' ? 'Computed signal' : 'Signal'} declared in this document.`,
        references: [],
        kind: lastDot === -1 ? 'signal' : 'property',
        start,
        end: offset,
    }));
}

function getActionCompletions(
    text: string,
    offset: number,
    attribute: ParsedAttribute,
): LanguageCompletion[] | undefined {
    if (NON_EXPRESSION_VALUE_PLUGINS.has(attribute.pluginName)) return undefined;

    const beforeCursor = text.slice(attribute.valueStart, offset);
    const match = beforeCursor.match(/@[A-Za-z_$][\w$]*$/) || beforeCursor.match(/@$/);
    if (!match) return undefined;

    return actionData.map(action => ({
        label: `@${action.name}`,
        insertText: `@${action.name}(`,
        description: action.description,
        references: [],
        kind: 'action',
        detail: action.pro ? 'Datastar Pro action' : 'Datastar action',
        start: offset - match[0].length,
        end: offset,
    }));
}

export function getSignatureHelp(text: string, offset: number): LanguageSignature | undefined {
    const attribute = expressionAttributeAt(text, offset);
    if (!attribute || NON_EXPRESSION_VALUE_PLUGINS.has(attribute.pluginName)) return undefined;

    const source = text.slice(attribute.valueStart, offset);
    const stack: Array<{ close: string; action?: string; argument: number }> = [];
    const pairs: Record<string, string> = { '(': ')', '[': ']', '{': '}' };

    for (let index = 0; index < source.length; index++) {
        const char = source[index];
        if (char === '"' || char === "'" || char === '`') {
            index = skipQuoted(source, index) - 1;
            continue;
        }

        if (char === '@') {
            const match = source.slice(index).match(/^@([A-Za-z_$][\w$]*)\s*\(/);
            if (match) {
                stack.push({ close: ')', action: match[1], argument: 0 });
                index += match[0].length - 1;
                continue;
            }
        }

        if (pairs[char]) {
            stack.push({ close: pairs[char], argument: 0 });
        } else if (stack.at(-1)?.close === char) {
            stack.pop();
        } else if (char === ',' && stack.at(-1)?.action) {
            stack.at(-1)!.argument++;
        }
    }

    const call = [...stack].reverse().find(frame => frame.action);
    if (!call) return undefined;
    const action = actionData.find(candidate => candidate.name === call.action);
    if (!action) return undefined;

    return {
        label: action.signature,
        description: action.description,
        parameters: action.parameters,
        activeParameter: Math.min(call.argument, Math.max(0, action.parameters.length - 1)),
        pro: action.pro,
    };
}

function getModifierCompletions(prefix: string, offset: number): LanguageCompletion[] | undefined {
    const separator = prefix.lastIndexOf('__');
    if (separator === -1) return undefined;

    const attributeName = prefix.slice(0, separator);
    const current = prefix.slice(separator + 2);
    if (!/^[a-zA-Z0-9.-]*$/.test(current)) return [];

    const parsed = parseAttributeName(attributeName);
    const metadata = attributeMetadata.get(parsed.pluginName);
    if (!metadata) return [];

    const appliedModifiers = new Set(parsed.modifiers.map(modifier => modifier.split('.')[0]));
    const completion = snippetEntries.find(entry => entry.pluginName === parsed.pluginName);
    const parts = current.split('.');
    const modifierName = parts[0];
    const completingTag = parts.length > 1;
    const typedPart = completingTag ? parts.at(-1)! : current;
    const appliedTags = new Set(parts.slice(1, -1));

    return metadata.modifiers
        .filter(modifier => {
            const [name, tag] = modifier.name.split('.', 2);
            if (appliedModifiers.has(name)) return false;
            if (!completingTag) return tag === undefined;
            return name === modifierName && tag !== undefined && !appliedTags.has(tag);
        })
        .map(modifier => {
            const insertText = completingTag
                ? modifier.name.slice(modifier.name.indexOf('.') + 1)
                : modifier.name;
            return {
                label: insertText,
                insertText,
                description: modifier.description || 'Datastar attribute modifier.',
                references: completion?.references || [],
                kind: 'modifier' as const,
                detail: metadata.pro ? 'Datastar Pro attribute modifier' : 'Datastar attribute modifier',
                start: offset - typedPart.length,
                end: offset,
            };
        });
}

export function getCompletions(
    text: string,
    offset: number,
    customAttributes: string[] = [],
): LanguageCompletion[] {
    const expressionAttribute = expressionAttributeAt(text, offset);
    if (expressionAttribute) {
        const actionCompletions = getActionCompletions(text, offset, expressionAttribute);
        if (actionCompletions !== undefined) return actionCompletions;

        const signalCompletions = getSignalCompletions(text, offset, expressionAttribute);
        if (signalCompletions !== undefined) return signalCompletions;
    }

    const tag = findTags(text).find(candidate => (
        candidate.start <= offset
        && (candidate.closed ? offset < candidate.end : offset <= candidate.end)
    ));
    if (!tag || isInsideQuotedValue(text, tag, offset)) return [];

    const prefixMatch = text.slice(tag.attributesStart, offset).match(/data-[a-zA-Z0-9:_*.-]*$/);
    if (!prefixMatch) return [];

    const modifierCompletions = getModifierCompletions(prefixMatch[0], offset);
    if (modifierCompletions !== undefined) return modifierCompletions;

    if (/^data-on:[a-zA-Z0-9-]*$/.test(prefixMatch[0])) {
        const genericEntry = snippetEntries.find(entry => entry.pluginName === 'on')!;
        return nativeEvents.map(eventName => {
            const name = `data-on:${eventName}`;
            const explicitEntry = snippetEntries.find(entry => entry.name === name);
            return {
                label: name,
                insertText: explicitEntry?.insertText ?? `${name}="\${1:expression}"`,
                description: explicitEntry?.description
                    ?? `Runs an expression whenever the \`${eventName}\` event is triggered.`,
                references: explicitEntry?.references ?? genericEntry.references,
                start: offset - prefixMatch[0].length,
                end: offset,
            };
        });
    }

    const entries = snippetEntries
        .filter(entry => entry.pluginName !== 'nonce' || tag.name === 'html')
        .map(entry => ({
            label: entry.name,
            insertText: entry.insertText,
            description: entry.description,
            references: entry.references || [],
            detail: attributeMetadata.get(entry.pluginName)?.pro
                ? 'Datastar Pro attribute'
                : 'Datastar attribute',
            start: offset - prefixMatch[0].length,
            end: offset,
        }));

    for (const pluginName of customAttributes) {
        entries.push({
            label: `data-${pluginName}`,
            insertText: `data-${pluginName}="\${1:expression}"`,
            description: 'Custom Datastar attribute.',
            references: [],
            detail: 'Custom Datastar attribute',
            start: offset - prefixMatch[0].length,
            end: offset,
        });
    }

    return entries;
}

export function getHover(text: string, offset: number): LanguageHover | undefined {
    const attribute = parseDocument(text).find(candidate => candidate.start <= offset && offset <= candidate.nameEnd);
    if (!attribute) return undefined;

    const entry = snippetEntries.find(candidate => candidate.name === attribute.name)
        || snippetEntries.find(candidate => candidate.pluginName === attribute.pluginName);
    if (!entry) return undefined;

    const rule = ATTRIBUTE_RULES[attribute.pluginName];
    const requirements: string[] = [];
    if (rule?.element) requirements.push(`Element: \`<${rule.element}>\` only`);
    if (rule?.key === 'must') requirements.push('Key: required');
    if (rule?.key === 'denied') requirements.push('Key: not allowed');
    if (rule?.value === 'must') requirements.push('Value: required');
    if (rule?.value === 'denied') requirements.push('Value: not allowed');

    return {
        start: attribute.start,
        end: attribute.nameEnd,
        name: attribute.name,
        description: entry.description,
        requirements,
        references: entry.references,
    };
}
