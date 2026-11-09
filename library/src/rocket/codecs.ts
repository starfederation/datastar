import { jsStrToObject } from '@utils/text'

type DefaultValue<T> = T | (() => T)

export type Codec<T> = {
  decode(value: unknown): T
  encode(value: T): string
}

export type CodecDocs = {
  description?: string
  label?: string
  control?: 'auto' | 'text' | 'textarea' | 'number' | 'boolean' | 'select'
  placeholder?: string
}

export type CodecManifest = {
  type:
    | 'string'
    | 'number'
    | 'boolean'
    | 'date'
    | 'json'
    | 'js'
    | 'binary'
    | 'array'
    | 'tuple'
    | 'object'
    | 'oneOf'
    | 'custom'
  values?: readonly unknown[]
  docs?: CodecDocs
}

type RuntimeCodec<T> = Codec<T> & {
  decode(value: unknown): T
  encode(value: T): string
  default(value: DefaultValue<T>): RuntimeCodec<T>
  docs(meta: CodecDocs): RuntimeCodec<T>
  readonly manifestMeta?: CodecManifest
}

type DefaultableCodec<T> = RuntimeCodec<T> & {
  readonly defaultFactory?: () => T
  readonly manifestMeta?: CodecManifest
}

export type InferCodec<T> = T extends RuntimeCodec<infer Value> ? Value : never

export type StringCodec = RuntimeCodec<string> & {
  readonly trim: StringCodec
  readonly upper: StringCodec
  readonly lower: StringCodec
  readonly kebab: StringCodec
  readonly camel: StringCodec
  readonly snake: StringCodec
  readonly pascal: StringCodec
  readonly title: StringCodec
  prefix(value: string): StringCodec
  suffix(value: string): StringCodec
  maxLength(length: number): StringCodec
  default(value: DefaultValue<string>): StringCodec
}

export type NumberCodec = RuntimeCodec<number> & {
  min(value: number): NumberCodec
  max(value: number): NumberCodec
  clamp(minValue: number, maxValue: number): NumberCodec
  step(stepValue: number, base?: number): NumberCodec
  readonly round: NumberCodec
  ceil(decimals?: number): NumberCodec
  floor(decimals?: number): NumberCodec
  fit(
    inMin: number,
    inMax: number,
    outMin: number,
    outMax: number,
    clamped?: boolean,
    rounded?: boolean,
  ): NumberCodec
  default(value: DefaultValue<number>): NumberCodec
}

export type BoolCodec = RuntimeCodec<boolean> & {
  default(value: DefaultValue<boolean>): BoolCodec
}

export type DateCodec = RuntimeCodec<Date> & {
  default(value: DefaultValue<Date>): DateCodec
}

export type JsonCodec<T = any> = RuntimeCodec<T> & {
  default(value: DefaultValue<T>): JsonCodec<T>
}

export type JsCodec<T = any> = RuntimeCodec<T> & {
  default(value: DefaultValue<T>): JsCodec<T>
}

export type BinCodec = RuntimeCodec<Uint8Array> & {
  default(value: DefaultValue<Uint8Array>): BinCodec
}

export type ArrayCodec<T> = RuntimeCodec<T[]> & {
  default(value: DefaultValue<T[]>): ArrayCodec<T>
}

export type TupleCodec<T extends readonly unknown[]> = RuntimeCodec<T> & {
  default(value: DefaultValue<T>): TupleCodec<T>
}

export type ObjectCodec<T extends Record<string, any>> = RuntimeCodec<T> & {
  default(value: DefaultValue<T>): ObjectCodec<T>
}

export type OneOfCodec<T> = RuntimeCodec<T> & {
  default(value: DefaultValue<T>): OneOfCodec<T>
}

export type CodecRegistry = {
  string: StringCodec
  number: NumberCodec
  bool: BoolCodec
  date: DateCodec
  json: JsonCodec
  js: JsCodec
  bin: BinCodec
  array<T>(codec: RuntimeCodec<T>): ArrayCodec<T>
  array<T extends readonly RuntimeCodec<any>[]>(
    ...codecs: T
  ): TupleCodec<{ [K in keyof T]: InferCodec<T[K]> }>
  object<T extends Record<string, RuntimeCodec<any>>>(
    shape: T,
  ): ObjectCodec<{ [K in keyof T]: InferCodec<T[K]> }>
  oneOf<const T extends readonly unknown[]>(...values: T): OneOfCodec<T[number]>
  oneOf<T extends readonly RuntimeCodec<any>[]>(
    ...codecs: T
  ): OneOfCodec<InferCodec<T[number]>>
}

export type PropDefs = Record<string, RuntimeCodec<any>>
export type InferProps<T extends PropDefs> = {
  [K in keyof T]: InferCodec<T[K]>
}

// Read a codec default without requiring callers to know whether it stores an explicit default factory.
export const getCodecDefault = <T>(codec: RuntimeCodec<T>) =>
  'defaultFactory' in codec && typeof codec.defaultFactory === 'function'
    ? codec.defaultFactory()
    : codec.decode(undefined)

// Fall back to the codec default when malformed prop input would otherwise break component setup/render.
export const decodeCodec = <T>(codec: RuntimeCodec<T>, value: unknown): T => {
  try {
    return codec.decode(value)
  } catch (error) {
    console.warn('Rocket codec decode failed', { value, error })
    return getCodecDefault(codec)
  }
}

type PropertyCodec<T> = {
  normalize(value: unknown): T
  encode(value: T): string
}
const propertyCodecs = new WeakMap<Codec<any>, PropertyCodec<any>>()
const registerPropertyCodec = <C extends RuntimeCodec<any>>(
  codec: C,
  encode: (value: InferCodec<C>) => string,
  normalize: (value: unknown) => InferCodec<C> = (value) =>
    decodeCodec(codec, value),
): C => {
  propertyCodecs.set(codec, { normalize, encode })
  return codec
}
const encodeNormalized = <T>(codec: RuntimeCodec<T>, value: T): string =>
  (propertyCodecs.get(codec)?.encode ?? codec.encode)(value)

// Normalize imperative input once, then serialize the normalized value without
// running transforms such as number.fit a second time. Custom codecs retain
// their encode/decode boundary for already-typed property values.
export const normalizeProperty = <T>(codec: RuntimeCodec<T>, input: T) => {
  if (input === undefined) {
    const value = getCodecDefault(codec)
    return { value, encoded: encodeNormalized(codec, value) }
  }
  const rules = propertyCodecs.get(codec)
  if (rules) {
    const value = rules.normalize(input) as T
    return { value, encoded: rules.encode(value) }
  }
  const encoded = codec.encode(input)
  return { value: decodeCodec(codec, encoded), encoded }
}

// Clone object defaults so component instances do not share mutable state through one codec.
const cloneValue = <T>(value: T): T => {
  if (value == null || typeof value !== 'object') return value
  try {
    return structuredClone(value)
  } catch {
    return value
  }
}

// Normalize static and factory defaults into one cloning factory shape for the rest of the codec pipeline.
const toDefaultFactory = <T>(value: DefaultValue<T>): (() => T) => {
  if (typeof value === 'function') {
    return () => cloneValue((value as () => T)())
  }
  return () => cloneValue(value)
}

// Attach `.default(...)` to a codec factory so derived codecs preserve defaults while continuing to compose.
const withDefault = <T, C extends RuntimeCodec<T>>(
  factory: (defaultFactory?: () => T, manifestMeta?: CodecManifest) => C,
  defaultFactory?: () => T,
  manifestMeta?: CodecManifest,
) =>
  ({
    default(value: DefaultValue<T>) {
      return factory(toDefaultFactory(value), manifestMeta)
    },
    docs(meta: CodecDocs) {
      return factory(defaultFactory, {
        ...(manifestMeta ?? { type: 'custom' }),
        docs: meta,
      })
    },
    defaultFactory,
    manifestMeta,
  }) as Pick<DefaultableCodec<T>, 'default' | 'defaultFactory'> &
    Pick<DefaultableCodec<T>, 'docs' | 'manifestMeta'>

// Lift plain decode/encode handlers into Rocket's runtime codec shape with default propagation.
const createCodecWithDefault = <T>(
  { decode, encode }: Codec<T>,
  defaultFactory?: () => T,
  manifestMeta: CodecManifest = { type: 'custom' },
): RuntimeCodec<T> =>
  ({
    decode,
    encode,
    ...withDefault<T, RuntimeCodec<T>>(
      (nextDefaultFactory, nextManifestMeta) =>
        createCodecWithDefault(
          { decode, encode },
          nextDefaultFactory,
          nextManifestMeta ?? manifestMeta,
        ),
      defaultFactory,
      manifestMeta,
    ),
  }) as RuntimeCodec<T>

export const createCodec = <T>(handlers: Codec<T>) =>
  // Give custom codecs the same default and decode-fallback contract as built-ins.
  createCodecWithDefault(handlers)

// Let `number.fit(...)` declaratively normalize external numeric input from one range into another.
const fitNumber = (
  value: number,
  inMin: number,
  inMax: number,
  outMin: number,
  outMax: number,
  clamped = true,
  rounded = false,
) => {
  if (inMax === inMin) return rounded ? Math.round(outMin) : outMin
  const ratio = (value - inMin) / (inMax - inMin)
  const normalized = clamped ? Math.max(0, Math.min(1, ratio)) : ratio
  const result = outMin + (outMax - outMin) * normalized
  return rounded ? Math.round(result) : result
}

// Parse JSON only for strings so codec combinators can reuse values that are already decoded.
const parseJSON = (value: unknown) => {
  if (typeof value === 'string') {
    try {
      return JSON.parse(value)
    } catch {
      return undefined
    }
  }
  return value
}

// Build composable string cleanup and case-shaping rules for Rocket props.
const createStringCodec = (
  decode: (value: unknown) => string = (value) => String(value ?? ''),
  encode: (value: string) => string = (value) => String(value ?? ''),
  defaultFactory?: () => string,
  manifestMeta: CodecManifest = { type: 'string' },
): StringCodec => {
  const mapString = (fn: (value: string) => string) =>
    createStringCodec(
      (value) => fn(decode(value)),
      (value) => fn(encode(value)),
      defaultFactory,
      manifestMeta,
    )

  const codec = {
    decode,
    encode,
    ...withDefault<string, StringCodec>(
      createStringCodec.bind(null, decode, encode),
      defaultFactory,
      manifestMeta,
    ),
    prefix(value: string) {
      return mapString((text) => (text.startsWith(value) ? text : value + text))
    },
    suffix(value: string) {
      return mapString((text) => (text.endsWith(value) ? text : text + value))
    },
    maxLength(length: number) {
      return mapString((value) => value.slice(0, length))
    },
  } as StringCodec

  Object.defineProperties(codec, {
    trim: { get: () => mapString((value) => value.trim()) },
    upper: { get: () => mapString((value) => value.toUpperCase()) },
    lower: { get: () => mapString((value) => value.toLowerCase()) },
    kebab: {
      get: () =>
        mapString((value) =>
          value
            .trim()
            .replace(/([a-z0-9])([A-Z])/g, '$1-$2')
            .replace(/[\s_]+/g, '-')
            .toLowerCase(),
        ),
    },
    camel: {
      get: () =>
        mapString((value) =>
          value
            .trim()
            .replace(/[-_\s]+(.)?/g, (_, char) =>
              char ? char.toUpperCase() : '',
            )
            .replace(/^(.)/, (_, char) => char.toLowerCase()),
        ),
    },
    snake: {
      get: () =>
        mapString((value) =>
          value
            .trim()
            .replace(/([a-z0-9])([A-Z])/g, '$1_$2')
            .replace(/[-\s]+/g, '_')
            .toLowerCase(),
        ),
    },
    pascal: {
      get: () =>
        mapString((value) =>
          value
            .trim()
            .replace(/(^|[-_\s]+)(.)/g, (_, __, char) => char.toUpperCase()),
        ),
    },
    title: {
      get: () =>
        mapString((value) =>
          value.replace(/\w\S*/g, (part) => {
            return part.charAt(0).toUpperCase() + part.slice(1).toLowerCase()
          }),
        ),
    },
  })

  return registerPropertyCodec(codec, (value) => String(value))
}

// Build composable clamp, round, and range-mapping rules for numeric props.
const createNumberCodec = (
  decode: (value: unknown) => number = (value) => {
    const number = Number(value)
    return Number.isFinite(number) ? number : 0
  },
  encode: (value: number) => string = (value) => `${value}`,
  defaultFactory?: () => number,
  manifestMeta: CodecManifest = { type: 'number' },
): NumberCodec => {
  const mapNumber = (fn: (value: number) => number) =>
    createNumberCodec(
      (value) => fn(decode(value)),
      (value) => `${fn(Number(value))}`,
      defaultFactory,
      manifestMeta,
    )

  const codec = {
    decode,
    encode,
    ...withDefault<number, NumberCodec>(
      createNumberCodec.bind(null, decode, encode),
      defaultFactory,
      manifestMeta,
    ),
    min(value: number) {
      return mapNumber((input) => Math.max(input, value))
    },
    max(value: number) {
      return mapNumber((input) => Math.min(input, value))
    },
    clamp(minValue: number, maxValue: number) {
      return mapNumber((input) => Math.max(minValue, Math.min(maxValue, input)))
    },
    step(stepValue: number, base = 0) {
      return mapNumber(
        (input) => base + Math.round((input - base) / stepValue) * stepValue,
      )
    },
    ceil(decimals = 0) {
      return mapNumber(
        (input) => Math.ceil(input * 10 ** decimals) / 10 ** decimals,
      )
    },
    floor(decimals = 0) {
      return mapNumber(
        (input) => Math.floor(input * 10 ** decimals) / 10 ** decimals,
      )
    },
    fit(
      inMin: number,
      inMax: number,
      outMin: number,
      outMax: number,
      clamped = true,
      rounded = false,
    ) {
      return mapNumber((input) =>
        fitNumber(input, inMin, inMax, outMin, outMax, clamped, rounded),
      )
    },
  } as NumberCodec

  Object.defineProperties(codec, {
    round: {
      get: () => mapNumber((value) => Math.round(value)),
    },
  })

  return registerPropertyCodec(codec, (value) => `${value}`)
}

// Decode boolean values for structured codecs. Direct HTML boolean props use
// attribute presence in the runtime rather than parsing their attribute text.
const createBoolCodec = (
  decode: (value: unknown) => boolean = (value) =>
    value === true ||
    value === '' ||
    value === 'true' ||
    value === 1 ||
    value === '1',
  encode: (value: boolean) => string = (value) => (value ? 'true' : 'false'),
  defaultFactory?: () => boolean,
  manifestMeta: CodecManifest = { type: 'boolean' },
): BoolCodec =>
  registerPropertyCodec(
    {
      decode,
      encode,
      ...withDefault<boolean, BoolCodec>(
        createBoolCodec.bind(null, decode, encode),
        defaultFactory,
        manifestMeta,
      ),
    },
    encode,
  )

// Normalize date input and output through `Date` instances across attribute and imperative values.
const createDateCodec = (
  decode: (value: unknown) => Date = (value) => {
    if (value instanceof Date)
      return Number.isNaN(value.getTime()) ? new Date() : value
    const date = new Date(value as any)
    return Number.isNaN(date.getTime()) ? new Date() : date
  },
  encode: (value: Date) => string = (value) => {
    const date = value instanceof Date ? value : new Date(value)
    return Number.isNaN(date.getTime())
      ? new Date().toISOString()
      : date.toISOString()
  },
  defaultFactory?: () => Date,
  manifestMeta: CodecManifest = { type: 'date' },
): DateCodec =>
  registerPropertyCodec(
    {
      decode,
      encode,
      ...withDefault<Date, DateCodec>(
        (nextDefaultFactory, nextManifestMeta) =>
          createDateCodec(
            decode,
            encode,
            nextDefaultFactory,
            nextManifestMeta ?? manifestMeta,
          ),
        defaultFactory,
        manifestMeta,
      ),
    },
    encode,
  )

// Clone decoded JSON values so component instances cannot mutate one another's structured props.
const createJsonCodec = <T = any>(
  decode: (value: unknown) => T = (value) => {
    const parsed = parseJSON(value)
    if (parsed == null) return {} as T
    return cloneValue(parsed as T)
  },
  encode: (value: T) => string = (value) => JSON.stringify(value),
  defaultFactory?: () => T,
  manifestMeta: CodecManifest = { type: 'json' },
): JsonCodec<T> =>
  registerPropertyCodec(
    {
      decode,
      encode,
      ...withDefault<T, JsonCodec<T>>(
        (nextDefaultFactory, nextManifestMeta) =>
          createJsonCodec(
            decode,
            encode,
            nextDefaultFactory,
            nextManifestMeta ?? manifestMeta,
          ),
        defaultFactory,
        manifestMeta,
      ),
    },
    encode,
  )

// Revive functions in JS-like serialized objects for props that intentionally allow richer data than JSON.
const createJsCodec = <T = any>(
  decode: (value: unknown) => T = (value) => {
    if (typeof value === 'string') {
      try {
        return jsStrToObject(value, { reviveFunctionStrings: true }) as T
      } catch {
        return {} as T
      }
    }
    if (value == null) return {} as T
    return cloneValue(value as T)
  },
  encode: (value: T) => string = (value) => JSON.stringify(value),
  defaultFactory?: () => T,
  manifestMeta: CodecManifest = { type: 'js' },
): JsCodec<T> =>
  registerPropertyCodec(
    {
      decode,
      encode,
      ...withDefault<T, JsCodec<T>>(
        (nextDefaultFactory, nextManifestMeta) =>
          createJsCodec(
            decode,
            encode,
            nextDefaultFactory,
            nextManifestMeta ?? manifestMeta,
          ),
        defaultFactory,
        manifestMeta,
      ),
    },
    encode,
  )

// Encode binary props as base64 so byte arrays cross attribute boundaries without losing fidelity.
const createBinCodec = (
  decode: (value: unknown) => Uint8Array = (value) => {
    if (value instanceof Uint8Array) return value
    const text = atob(String(value ?? ''))
    const bytes = new Uint8Array(text.length)
    for (let i = 0; i < text.length; i += 1) bytes[i] = text.charCodeAt(i)
    return bytes
  },
  encode: (value: Uint8Array) => string = (value) =>
    btoa(Array.from(value, (byte) => String.fromCharCode(byte)).join('')),
  defaultFactory?: () => Uint8Array,
  manifestMeta: CodecManifest = { type: 'binary' },
): BinCodec =>
  registerPropertyCodec(
    {
      decode,
      encode,
      ...withDefault<Uint8Array, BinCodec>(
        (nextDefaultFactory, nextManifestMeta) =>
          createBinCodec(
            decode,
            encode,
            nextDefaultFactory,
            nextManifestMeta ?? manifestMeta,
          ),
        defaultFactory,
        manifestMeta,
      ),
    },
    encode,
  )

// Reuse one item codec's decode/default semantics across every value in a list prop.
const createArrayCodec = <T>(
  codec: RuntimeCodec<T>,
  defaultFactory?: () => T[],
  manifestMeta: CodecManifest = { type: 'array' },
): ArrayCodec<T> =>
  registerPropertyCodec(
    {
      decode(value: unknown) {
        const parsed = parseJSON(value)
        const array = Array.isArray(parsed) ? parsed : []
        return array.map((item) => decodeCodec(codec, item))
      },
      encode(value: T[]) {
        return JSON.stringify(
          value.map(
            (item) => parseJSON(codec.encode(item)) ?? codec.encode(item),
          ),
        )
      },
      ...withDefault<T[], ArrayCodec<T>>(
        (nextDefaultFactory, nextManifestMeta) =>
          createArrayCodec(
            codec,
            nextDefaultFactory,
            nextManifestMeta ?? manifestMeta,
          ),
        defaultFactory,
        manifestMeta,
      ),
    },
    (value) =>
      JSON.stringify(
        value.map((item) => {
          const encoded = encodeNormalized(codec, item)
          return parseJSON(encoded) ?? encoded
        }),
      ),
    (value) => {
      const parsed = parseJSON(value)
      return (Array.isArray(parsed) ? parsed : []).map(
        (item) => normalizeProperty(codec, item).value,
      )
    },
  )

// Preserve per-position codecs and defaults in heterogeneous tuple props.
const createTupleCodec = <T extends readonly RuntimeCodec<any>[]>(
  codecs: T,
  defaultFactory?: () => { [K in keyof T]: InferCodec<T[K]> },
  manifestMeta: CodecManifest = { type: 'tuple' },
): TupleCodec<{ [K in keyof T]: InferCodec<T[K]> }> =>
  registerPropertyCodec(
    {
      decode(value: unknown) {
        const parsed = parseJSON(value)
        const array = Array.isArray(parsed) ? parsed : []
        return codecs.map((codec, index) => {
          if (index < array.length) return decodeCodec(codec, array[index])
          return getCodecDefault(codec)
        }) as { [K in keyof T]: InferCodec<T[K]> }
      },
      encode(value: { [K in keyof T]: InferCodec<T[K]> }) {
        return JSON.stringify(
          value.map((item, index) => {
            const encoded = codecs[index]!.encode(item)
            return parseJSON(encoded) ?? encoded
          }),
        )
      },
      ...withDefault<
        { [K in keyof T]: InferCodec<T[K]> },
        TupleCodec<{ [K in keyof T]: InferCodec<T[K]> }>
      >(
        (nextDefaultFactory, nextManifestMeta) =>
          createTupleCodec(
            codecs,
            nextDefaultFactory,
            nextManifestMeta ?? manifestMeta,
          ),
        defaultFactory,
        manifestMeta,
      ),
    },
    (value) =>
      JSON.stringify(
        value.map((item, index) => {
          const encoded = encodeNormalized(codecs[index]!, item)
          return parseJSON(encoded) ?? encoded
        }),
      ),
    (value) => {
      const parsed = parseJSON(value)
      const array = Array.isArray(parsed) ? parsed : []
      return codecs.map((codec, index) =>
        index < array.length
          ? normalizeProperty(codec, array[index]).value
          : getCodecDefault(codec),
      ) as { [K in keyof T]: InferCodec<T[K]> }
    },
  )

// Decode object props field by field through a fixed shape of child codecs and defaults.
const createObjectCodec = <T extends Record<string, RuntimeCodec<any>>>(
  shape: T,
  defaultFactory?: () => { [K in keyof T]: InferCodec<T[K]> },
  manifestMeta: CodecManifest = { type: 'object' },
): ObjectCodec<{ [K in keyof T]: InferCodec<T[K]> }> =>
  registerPropertyCodec(
    {
      decode(value: unknown) {
        const parsed = parseJSON(value)
        const source = (
          parsed && typeof parsed === 'object' && !Array.isArray(parsed)
            ? parsed
            : {}
        ) as Record<string, unknown>
        const next = {} as { [K in keyof T]: InferCodec<T[K]> }
        for (const [key, codec] of Object.entries(shape)) {
          next[key as keyof T] =
            key in source
              ? decodeCodec(codec, source[key])
              : getCodecDefault(codec)
        }
        return next
      },
      encode(value: { [K in keyof T]: InferCodec<T[K]> }) {
        const next: Record<string, unknown> = {}
        for (const [key, codec] of Object.entries(shape)) {
          const encoded = codec.encode(value[key as keyof T])
          next[key] = parseJSON(encoded) ?? encoded
        }
        return JSON.stringify(next)
      },
      ...withDefault<
        { [K in keyof T]: InferCodec<T[K]> },
        ObjectCodec<{ [K in keyof T]: InferCodec<T[K]> }>
      >(
        (nextDefaultFactory, nextManifestMeta) =>
          createObjectCodec(
            shape,
            nextDefaultFactory,
            nextManifestMeta ?? manifestMeta,
          ),
        defaultFactory,
        manifestMeta,
      ),
    },
    (value) => {
      const next: Record<string, unknown> = {}
      for (const [key, codec] of Object.entries(shape)) {
        const encoded = encodeNormalized(codec, value[key as keyof T])
        next[key] = parseJSON(encoded) ?? encoded
      }
      return JSON.stringify(next)
    },
    (value) => {
      const parsed = parseJSON(value)
      const source =
        parsed && typeof parsed === 'object' && !Array.isArray(parsed)
          ? parsed
          : {}
      const next = {} as { [K in keyof T]: InferCodec<T[K]> }
      for (const [key, codec] of Object.entries(shape)) {
        next[key as keyof T] =
          key in source
            ? normalizeProperty(codec, source[key]).value
            : getCodecDefault(codec)
      }
      return next
    },
  )

// Distinguish literal `oneOf(...)` entries from codec entries so one API can support both union styles.
const isCodec = (value: unknown): value is RuntimeCodec<any> =>
  !!value && typeof value === 'object' && 'decode' in value && 'encode' in value

const oneOfDefaultFactory = <T>(
  values: readonly unknown[],
  defaultFactory?: () => T,
) => {
  if (defaultFactory) return defaultFactory
  const entry = values[0]
  if (isCodec(entry)) return () => getCodecDefault(entry) as T
  return () => entry as T
}

const codecValueKind = (value: unknown) =>
  value instanceof Date
    ? Date
    : value instanceof Uint8Array
      ? Uint8Array
      : Array.isArray(value)
        ? Array
        : value === null
          ? null
          : typeof value

const codecMatchesValue = (
  codec: RuntimeCodec<any>,
  value: unknown,
): boolean => {
  // A successful round trip identifies the runtime union member without rejecting codecs that intentionally normalize values while encoding.
  try {
    return (
      codecValueKind(value) ===
      codecValueKind(codec.decode(codec.encode(value)))
    )
  } catch {
    return false
  }
}

const codecMatchesInput = (codec: RuntimeCodec<any>, value: unknown) => {
  const parsed = parseJSON(value)
  switch (getCodecManifest(codec).type) {
    case 'string':
      return (
        typeof parsed === 'string' ||
        (typeof value === 'string' && parsed === undefined)
      )
    case 'number':
      return typeof parsed === 'number' && Number.isFinite(parsed)
    case 'boolean':
      return (
        typeof parsed === 'boolean' ||
        value === '' ||
        value === 1 ||
        value === '1'
      )
    case 'date':
      return (
        value instanceof Date ||
        (typeof value === 'string' &&
          value !== '' &&
          !Number.isNaN(Date.parse(value)))
      )
    case 'binary':
      return value instanceof Uint8Array || typeof value === 'string'
    case 'array':
    case 'tuple':
      return Array.isArray(parsed)
    case 'object':
      return (
        parsed !== null && typeof parsed === 'object' && !Array.isArray(parsed)
      )
    case 'json':
    case 'js':
      return parsed !== undefined
    default:
      try {
        codec.decode(value)
        return true
      } catch {
        return false
      }
  }
}

// Build a union over literals and child codecs so props can declare a constrained set of values.
const createOneOfCodec = <T>(
  values: readonly unknown[],
  defaultFactory?: () => T,
  manifestMeta: CodecManifest = {
    type: 'oneOf',
    values: values.map((entry) =>
      isCodec(entry) ? getCodecManifest(entry) : entry,
    ),
  },
): OneOfCodec<T> =>
  registerPropertyCodec(
    {
      decode(value: unknown) {
        let hadDecodeError = false
        const parsedValue = parseJSON(value)
        for (const entry of values) {
          if (isCodec(entry) && codecMatchesInput(entry, value)) {
            try {
              return entry.decode(value) as T
            } catch {
              hadDecodeError = true
            }
          }
          if (
            entry === value ||
            entry === parsedValue ||
            `${entry}` === `${value}`
          ) {
            return entry as T
          }
        }
        if (hadDecodeError) {
          console.warn('Rocket codec decode failed', { value })
        }
        if (defaultFactory) return defaultFactory()
        return oneOfDefaultFactory<T>(values)()
      },
      encode(value: T) {
        for (const entry of values) {
          if (isCodec(entry) && codecMatchesValue(entry, value)) {
            return entry.encode(value)
          }
          if (Object.is(entry, value)) return `${value}`
        }
        return `${value}`
      },
      ...withDefault<T, OneOfCodec<T>>(
        (nextDefaultFactory, nextManifestMeta) =>
          createOneOfCodec(
            values,
            nextDefaultFactory,
            nextManifestMeta ?? manifestMeta,
          ),
        oneOfDefaultFactory(values, defaultFactory),
        manifestMeta,
      ),
    },
    (value) => {
      for (const entry of values) {
        if (isCodec(entry) && codecMatchesValue(entry, value))
          return encodeNormalized(entry, value)
        if (Object.is(entry, value)) return `${value}`
      }
      return `${value}`
    },
    (value) => {
      for (const entry of values) {
        if (isCodec(entry) && codecMatchesValue(entry, value))
          return normalizeProperty(entry, value).value as T
        if (Object.is(entry, value)) return entry as T
      }
      return oneOfDefaultFactory<T>(values, defaultFactory)()
    },
  )

function oneOf<const T extends readonly unknown[]>(
  ...values: T
): OneOfCodec<T[number]>
function oneOf<T extends readonly RuntimeCodec<any>[]>(
  ...codecs: T
): OneOfCodec<InferCodec<T[number]>>
function oneOf(...values: readonly unknown[]) {
  return createOneOfCodec(values)
}

export const getCodecManifest = <T>(codec: RuntimeCodec<T>) =>
  'manifestMeta' in codec && codec.manifestMeta
    ? codec.manifestMeta
    : ({ type: 'custom' } as CodecManifest)

export const codecRegistry: CodecRegistry = {
  // Share one registry so every component composes the same prop codec semantics.
  string: createStringCodec(),
  number: createNumberCodec(),
  bool: createBoolCodec(),
  date: createDateCodec(),
  json: createJsonCodec(),
  js: createJsCodec(),
  bin: createBinCodec(),
  array: ((...codecs: readonly RuntimeCodec<any>[]) => {
    if (codecs.length === 1) return createArrayCodec(codecs[0]!)
    return createTupleCodec(codecs)
  }) as CodecRegistry['array'],
  object: createObjectCodec,
  oneOf,
}
