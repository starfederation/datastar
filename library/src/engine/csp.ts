import { DOCUMENT } from '@engine/consts'

type ExpressionFn = (...args: any[]) => any

const nonceAttribute = 'data-nonce'
const root = DOCUMENT.documentElement
const pageNonce = root.getAttribute(nonceAttribute)

const cspEnabled = pageNonce !== null

let policy: any
if (cspEnabled) {
  if (!pageNonce) {
    throw new Error('Datastar CSP requires a nonempty html data-nonce.')
  }
  root.removeAttribute(nonceAttribute)
  policy = (window as any).trustedTypes?.createPolicy('datastar', {
    createHTML: (html: string) => html,
    createScript: (script: string) => script,
  })
}

export const prepareScript = (
  script: HTMLScriptElement,
  content: string,
): void => {
  if (cspEnabled) script.nonce = pageNonce!
  script.text = policy ? policy.createScript(content) : content
}

export const createHTML = (html: string): any =>
  policy ? policy.createHTML(html) : html

const compiledExpressions = new Map<string, ExpressionFn>()

export const compileExpression = (
  argNames: string[],
  expression: string,
): ExpressionFn => {
  if (!cspEnabled) return Function(...argNames, expression) as ExpressionFn

  const source = `function(${argNames.join(',')}){${expression}\n}`
  const cached = compiledExpressions.get(source)
  if (cached) return cached

  const script = DOCUMENT.createElement('script')
  prepareScript(script, `document.currentScript.x=${source}`)
  DOCUMENT.head.appendChild(script)
  script.remove()

  const compiled = (script as any).x as ExpressionFn | undefined
  if (!compiled) {
    throw new Error('CSP blocked Datastar expression compilation.')
  }
  compiledExpressions.set(source, compiled)
  return compiled
}
