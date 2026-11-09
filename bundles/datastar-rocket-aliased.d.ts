export type JSONPatch = Record<string, any> & {
	length?: never;
};
export type Paths = [
	string,
	any
][];
export type WatcherArgsValue = string | Element | DocumentFragment | undefined;
export type WatcherArgs = Record<string, WatcherArgsValue>;
export type ErrorFn = (name: string, ctx?: Record<string, any>) => Error;
export type ActionContext = {
	el: HTMLOrSVG;
	evt?: Event;
	error: ErrorFn;
	cleanups: Map<string, () => void>;
};
export type RequirementType = "allowed" | "must" | "denied" | "exclusive";
export type Requirement = RequirementType | {
	key: Exclude<RequirementType, "exclusive">;
	value?: Exclude<RequirementType, "exclusive">;
} | {
	key?: Exclude<RequirementType, "exclusive">;
	value: Exclude<RequirementType, "exclusive">;
};
export type Rx<B extends boolean> = (...args: any[]) => B extends true ? unknown : void;
export type ReqField<R, K extends "key" | "value", Return> = R extends "must" | {
	[P in K]: "must";
} ? Return : R extends "denied" | {
	[P in K]: "denied";
} ? undefined : R extends "allowed" | {
	[P in K]: "allowed";
} | (K extends keyof R ? never : R) ? Return | undefined : never;
export type ReqFields<R extends Requirement, B extends boolean> = R extends "exclusive" ? {
	key: string;
	value: undefined;
	rx: undefined;
} | {
	key: undefined;
	value: string;
	rx: Rx<B>;
} : {
	key: ReqField<R, "key", string>;
	value: ReqField<R, "value", string>;
	rx: ReqField<R, "value", Rx<B>>;
};
export type AttributeContext<R extends Requirement = Requirement, RxReturn extends boolean = boolean> = {
	el: HTMLOrSVG;
	mods: Modifiers;
	rawKey: string;
	evt?: Event;
	error: ErrorFn;
	loadedPluginNames: {
		actions: Readonly<Set<string>>;
		attributes: Readonly<Set<string>>;
	};
} & ReqFields<R, RxReturn>;
export type AttributePlugin<R extends Requirement = Requirement, RxReturn extends boolean = boolean> = {
	name: string;
	apply: (ctx: AttributeContext<R, RxReturn>) => void | (() => void);
	requirement?: R;
	returnsValue?: RxReturn;
	argNames?: string[];
};
export type WatcherContext = {
	error: ErrorFn;
};
export type WatcherPlugin = {
	name: string;
	apply: (ctx: WatcherContext, args: WatcherArgs) => void;
};
export type ActionPlugin<T = any> = {
	name: string;
	apply: (ctx: ActionContext, ...args: any[]) => T;
};
export type MergePatchArgs = {
	ifMissing?: boolean;
};
export type HTMLOrSVG = HTMLElement | SVGElement | MathMLElement;
export type Modifiers = Map<string, Set<string>>;
export type SignalFilterOptions = {
	include?: RegExp | string;
	exclude?: RegExp | string;
};
export type Signal<T> = {
	(): T;
	(value: T): boolean;
};
export type Computed<T> = () => T;
export type Effect = () => void;
export declare const actions: Record<string, (ctx: ActionContext, ...args: any[]) => any>;
export declare const attribute: <R extends Requirement, B extends boolean>(plugin: AttributePlugin<R, B>) => void;
export declare const action: <T>(plugin: ActionPlugin<T>) => void;
export declare const watcher: (plugin: WatcherPlugin) => void;
export interface ReactiveNode {
	deps_?: Link;
	depsTail_?: Link;
	subs_?: Link;
	subsTail_?: Link;
	flags_: ReactiveFlags;
}
export interface Link {
	version_: number;
	dep_: ReactiveNode;
	sub_: ReactiveNode;
	prevSub_?: Link;
	nextSub_?: Link;
	prevDep_?: Link;
	nextDep_?: Link;
}
declare enum ReactiveFlags {
	None = 0,
	Mutable = 1,
	Watching = 2,
	RecursedCheck = 4,
	Recursed = 8,
	Dirty = 16,
	Pending = 32
}
export declare const beginBatch: () => void;
export declare const endBatch: () => void;
export declare const startPeeking: (sub?: ReactiveNode) => void;
export declare const stopPeeking: () => void;
export declare const signal: <T>(initialValue?: T) => Signal<T>;
export declare const computed: <T>(getter: (previousValue?: T) => T) => Computed<T>;
export declare const effect: (fn: () => void) => Effect;
export declare const getPath: <T = any>(path: string) => T | undefined;
export declare const mergePatch: (patch: JSONPatch, { ifMissing }?: MergePatchArgs) => void;
export declare const mergePaths: (paths: Paths, options?: MergePatchArgs) => void;
/**
 * Filters the root store based on an include and exclude RegExp
 *
 * @returns The filtered object
 */
export declare const filtered: ({ include, exclude }?: SignalFilterOptions, obj?: JSONPatch) => Record<string, any>;
export declare const root: Record<string, any>;
export type DefaultValue<T> = T | (() => T);
export type Codec<T> = {
	decode(value: unknown): T;
	encode(value: T): string;
};
export type CodecDocs = {
	description?: string;
	label?: string;
	control?: "auto" | "text" | "textarea" | "number" | "boolean" | "select";
	placeholder?: string;
};
export type CodecManifest = {
	type: "string" | "number" | "boolean" | "date" | "json" | "js" | "binary" | "array" | "tuple" | "object" | "oneOf" | "custom";
	values?: readonly unknown[];
	docs?: CodecDocs;
};
export type RuntimeCodec<T> = Codec<T> & {
	decode(value: unknown): T;
	encode(value: T): string;
	default(value: DefaultValue<T>): RuntimeCodec<T>;
	docs(meta: CodecDocs): RuntimeCodec<T>;
	readonly manifestMeta?: CodecManifest;
};
export type InferCodec<T> = T extends RuntimeCodec<infer Value> ? Value : never;
export type StringCodec = RuntimeCodec<string> & {
	readonly trim: StringCodec;
	readonly upper: StringCodec;
	readonly lower: StringCodec;
	readonly kebab: StringCodec;
	readonly camel: StringCodec;
	readonly snake: StringCodec;
	readonly pascal: StringCodec;
	readonly title: StringCodec;
	prefix(value: string): StringCodec;
	suffix(value: string): StringCodec;
	maxLength(length: number): StringCodec;
	default(value: DefaultValue<string>): StringCodec;
};
export type NumberCodec = RuntimeCodec<number> & {
	min(value: number): NumberCodec;
	max(value: number): NumberCodec;
	clamp(minValue: number, maxValue: number): NumberCodec;
	step(stepValue: number, base?: number): NumberCodec;
	readonly round: NumberCodec;
	ceil(decimals?: number): NumberCodec;
	floor(decimals?: number): NumberCodec;
	fit(inMin: number, inMax: number, outMin: number, outMax: number, clamped?: boolean, rounded?: boolean): NumberCodec;
	default(value: DefaultValue<number>): NumberCodec;
};
export type BoolCodec = RuntimeCodec<boolean> & {
	default(value: DefaultValue<boolean>): BoolCodec;
};
export type DateCodec = RuntimeCodec<Date> & {
	default(value: DefaultValue<Date>): DateCodec;
};
export type JsonCodec<T = any> = RuntimeCodec<T> & {
	default(value: DefaultValue<T>): JsonCodec<T>;
};
export type JsCodec<T = any> = RuntimeCodec<T> & {
	default(value: DefaultValue<T>): JsCodec<T>;
};
export type BinCodec = RuntimeCodec<Uint8Array> & {
	default(value: DefaultValue<Uint8Array>): BinCodec;
};
export type ArrayCodec<T> = RuntimeCodec<T[]> & {
	default(value: DefaultValue<T[]>): ArrayCodec<T>;
};
export type TupleCodec<T extends readonly unknown[]> = RuntimeCodec<T> & {
	default(value: DefaultValue<T>): TupleCodec<T>;
};
export type ObjectCodec<T extends Record<string, any>> = RuntimeCodec<T> & {
	default(value: DefaultValue<T>): ObjectCodec<T>;
};
export type OneOfCodec<T> = RuntimeCodec<T> & {
	default(value: DefaultValue<T>): OneOfCodec<T>;
};
export type CodecRegistry = {
	string: StringCodec;
	number: NumberCodec;
	bool: BoolCodec;
	date: DateCodec;
	json: JsonCodec;
	js: JsCodec;
	bin: BinCodec;
	array<T>(codec: RuntimeCodec<T>): ArrayCodec<T>;
	array<T extends readonly RuntimeCodec<any>[]>(...codecs: T): TupleCodec<{
		[K in keyof T]: InferCodec<T[K]>;
	}>;
	object<T extends Record<string, RuntimeCodec<any>>>(shape: T): ObjectCodec<{
		[K in keyof T]: InferCodec<T[K]>;
	}>;
	oneOf<const T extends readonly unknown[]>(...values: T): OneOfCodec<T[number]>;
	oneOf<T extends readonly RuntimeCodec<any>[]>(...codecs: T): OneOfCodec<InferCodec<T[number]>>;
};
export type PropDefs = Record<string, RuntimeCodec<any>>;
export type InferProps<T extends PropDefs> = {
	[K in keyof T]: InferCodec<T[K]>;
};
export declare const createCodec: <T>(handlers: Codec<T>) => RuntimeCodec<T>;
export type TaggedLiteral = (strings: TemplateStringsArray, ...values: unknown[]) => DocumentFragment;
export type AnyRecord = {
	[key: string]: any;
};
export type SetupSignal = (<T>(name: string, initialValue: T) => T) & AnyRecord;
export type RefCtors = Record<string, abstract new (...args: any[]) => Element>;
export type InstancesOf<C extends RefCtors> = {
	[K in keyof C]?: InstanceType<C[K]>;
};
export type StateRecord = AnyRecord;
export type PropOverrideGetter<Props extends Record<string, any>, Name extends keyof Props & string> = (getDefault: () => Props[Name]) => any;
export type PropOverrideSetter<Props extends Record<string, any>, Name extends keyof Props & string> = (value: any, setDefault: (value: Props[Name]) => void) => void;
export type HostPropDescriptor = Omit<PropertyDescriptor, "configurable">;
export type RocketHost = HTMLElement & {
	dispatchRocketAction(name: string, el: Element | null, evt: Event | undefined, cleanups: Map<string, () => void>, ...args: any[]): any;
};
export type RocketHostWithProps<Props extends Record<string, any>> = RocketHost & Props;
export type SetupEmit = {
	(type: string): void;
	(...types: [
		string,
		...string[]
	]): void;
	<Detail>(type: string, detail: Detail, options?: Omit<CustomEventInit<Detail>, "detail">): void;
};
export type SetupEmitCancellable = {
	(type: string): boolean;
	<Detail>(type: string, detail: Detail, options?: Omit<CustomEventInit<Detail>, "detail" | "cancelable">): boolean;
};
export type PropObserver<Props extends Record<string, any>> = (() => void) | ((props: Props, changes: Partial<Props>) => void);
export type SetupContext<Props extends Record<string, any>> = {
	props: Props;
	readonly internals: ElementInternals;
	$: Record<string, any>;
	$$: SetupSignal;
	effect(fn: () => void): () => void;
	apply(root: HTMLOrSVG | ShadowRoot, merge?: boolean): void;
	adoptStyles(host: HTMLElement, ...styles: string[]): void;
	cleanup(fn: () => void): void;
	emit: SetupEmit;
	emitCancellable: SetupEmitCancellable;
	actions: Record<string, (...args: any[]) => any>;
	action(name: string, fn: RocketAction<Props>): void;
	observeProps(fn: PropObserver<Props>, ...propNames: Array<keyof Props & string>): () => void;
	overrideProp<Name extends keyof Props & string>(name: Name, getter?: PropOverrideGetter<Props, Name>, setter?: PropOverrideSetter<Props, Name>): void;
	defineHostProp(name: string, descriptor: HostPropDescriptor): void;
	render: SetupRender<Props>;
	host: RocketHostWithProps<Props>;
};
export type FirstUpdateContext<Props extends Record<string, any>, Refs extends RefCtors = RefCtors> = SetupContext<Props> & {
	refs: InstancesOf<Refs>;
};
export type RenderContext<Props extends Record<string, any>> = {
	html: TaggedLiteral;
	svg: TaggedLiteral;
	props: Props;
	host: RocketHostWithProps<Props>;
};
export type RenderContextOverrides<Props extends Record<string, any>> = Partial<RenderContext<Props>>;
export type RocketPrimitiveRenderValue = string | number | boolean | bigint | Date | null | undefined;
export type RocketComposedRenderValue = RocketPrimitiveRenderValue | Node | Iterable<RocketComposedRenderValue>;
export type RocketRenderValue = DocumentFragment | RocketPrimitiveRenderValue | Iterable<RocketComposedRenderValue>;
export type RocketRender<Props extends Record<string, any>> = (context: RenderContext<Props>, ...args: any[]) => RocketRenderValue;
export type SetupRender<Props extends Record<string, any>> = (context: RenderContextOverrides<Props>, ...args: any[]) => void;
export type RocketAction<Props extends Record<string, any>> = (context: {
	host: RocketHostWithProps<Props>;
	props: Props;
	state: StateRecord;
	el: Element | null;
	evt: Event | undefined;
}, ...args: any[]) => any;
export type RocketDefinition<Defs extends PropDefs = PropDefs, Refs extends RefCtors = RefCtors> = {
	refs?: Refs;
	props?: (codecs: CodecRegistry) => Defs;
	manifest?: RocketManifestMeta;
	setup?: (context: SetupContext<InferProps<Defs>>) => void;
	onFirstRender?: (context: FirstUpdateContext<InferProps<Defs>, Refs>) => void;
	render?: RocketRender<InferProps<Defs>>;
	mode?: "open" | "closed" | "light";
	ignoredByMorph?: boolean | "children";
	formAssociated?: boolean;
	delegatesFocus?: boolean;
	formValue?: false | ((context: SetupContext<InferProps<Defs>>) => string | File | FormData | null);
	onFormAssociated?: (context: SetupContext<InferProps<Defs>>, form: HTMLFormElement | null) => void;
	onFormReset?: (context: SetupContext<InferProps<Defs>>) => void;
	onFormDisabled?: (context: SetupContext<InferProps<Defs>>, disabled: boolean) => void;
	onFormStateRestore?: (context: SetupContext<InferProps<Defs>>, state: string | File | FormData | null, mode: "restore" | "autocomplete") => void;
	renderOnPropChange?: boolean | ((context: {
		host: RocketHostWithProps<InferProps<Defs>>;
		props: InferProps<Defs>;
		changes: Partial<InferProps<Defs>>;
	}) => boolean);
};
export type RocketManifestMeta = {
	slots?: RocketManifestSlot[];
	events?: RocketManifestEvent[];
};
export type RocketManifestSlot = {
	name: string;
	description?: string;
};
export type RocketManifestEvent = {
	name: string;
	kind?: "event" | "custom-event";
	bubbles?: boolean;
	composed?: boolean;
	description?: string;
};
export type RocketPublishOptions = {
	endpoint: string;
	headers?: Record<string, string>;
};
export declare const publishRocketManifests: ({ endpoint, headers, }: RocketPublishOptions) => Promise<Response>;
export declare function rocket<Refs extends RefCtors = RefCtors, Defs extends PropDefs = PropDefs>(tag: string, options?: RocketDefinition<Defs, Refs>): CustomElementConstructor | undefined;

export {};
