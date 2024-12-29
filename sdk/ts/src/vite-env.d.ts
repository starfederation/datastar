/// <reference types="vite/client" />
import 'vitest'

interface CustomMatchers<R = unknown> {
  anyItemStartsWith: (required: string) => R
}

declare module 'vitest' {
  interface Assertion<T = any> extends CustomMatchers<T> {}
  interface AsymmetricMatchersContaining extends CustomMatchers {}
}