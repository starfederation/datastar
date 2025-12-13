// Icon: material-symbols:cloud-download
// Slug: Patches elements into the DOM.
// Description: Patches elements into the DOM.

import { watcher } from '@engine'
import { morph } from '@engine/morph'
import type { WatcherContext } from '@engine/types'
import { supportsViewTransitions } from '@utils/view-transitions'

type PatchElementsMode =
  | 'remove'
  | 'outer'
  | 'inner'
  | 'replace'
  | 'prepend'
  | 'append'
  | 'before'
  | 'after'

type PatchElementsArgs = {
  elements: string
  mode: PatchElementsMode
  selector: string
  useViewTransition: boolean
  namespace?: string
}

const NAMESPACE_WRAPPER: Record<string, string | null> = {
  'http://www.w3.org/1999/xhtml': null,
  'http://www.w3.org/2000/svg': 'svg',
  'http://www.w3.org/1998/Math/MathML': 'math',
}

watcher({
  name: 'datastar-patch-elements',
  apply(
    ctx,
    { elements = '', selector = '', mode = 'outer', useViewTransition, namespace },
  ) {
    switch (mode) {
      case 'remove':
      case 'outer':
      case 'inner':
      case 'replace':
      case 'prepend':
      case 'append':
      case 'before':
      case 'after':
        break
      default:
        throw ctx.error('PatchElementsInvalidMode', { mode })
    }

    if (!selector && mode !== 'outer' && mode !== 'replace') {
      throw ctx.error('PatchElementsExpectedSelector')
    }

    if (namespace && !(namespace in NAMESPACE_WRAPPER)) {
      throw ctx.error('PatchElementsUnsupportedNamespace', { namespace })
    }

    const args2: PatchElementsArgs = {
      mode,
      selector,
      elements,
      useViewTransition: useViewTransition?.trim() === 'true',
      namespace,
    }

    if (supportsViewTransitions && useViewTransition) {
      document.startViewTransition(() => onPatchElements(ctx, args2))
    } else {
      onPatchElements(ctx, args2)
    }
  },
})

const onPatchElements = (
  { error }: WatcherContext,
  { elements, selector, mode, namespace }: PatchElementsArgs,
) => {
  const elementsWithSvgsRemoved = elements.replace(
    /<svg(\s[^>]*>|>)([\s\S]*?)<\/svg>/gim,
    '',
  )
  const hasHtml = /<\/html>/.test(elementsWithSvgsRemoved)
  const hasHead = /<\/head>/.test(elementsWithSvgsRemoved)
  const hasBody = /<\/body>/.test(elementsWithSvgsRemoved)

  const wrapper = namespace ? NAMESPACE_WRAPPER[namespace] : null

  const newDocument = new DOMParser().parseFromString(
    hasHtml || hasHead || hasBody
      ? elements
      : wrapper
        ? `<body><template><${wrapper} xmlns="${namespace}">${elements}</${wrapper}></template></body>`
        : `<body><template>${elements}</template></body>`,
    'text/html',
  )

  let newContent = document.createDocumentFragment()
  if (hasHtml) {
    newContent.appendChild(newDocument.documentElement)
  } else if (hasHead && hasBody) {
    newContent.appendChild(newDocument.head)
    newContent.appendChild(newDocument.body)
  } else if (hasHead) {
    newContent.appendChild(newDocument.head)
  } else if (hasBody) {
    newContent.appendChild(newDocument.body)
  } else if (wrapper) {
    const wrapperEl = newDocument
      .querySelector('template')!
      .content.querySelector(wrapper)!
    for (const child of wrapperEl.childNodes) {
      newContent.appendChild(child)
    }
  } else {
    newContent = newDocument.querySelector('template')!.content
  }

  if (!selector && (mode === 'outer' || mode === 'replace')) {
    for (const child of newContent.children) {
      let target: Element
      if (child instanceof HTMLHtmlElement) {
        target = document.documentElement
      } else if (child instanceof HTMLBodyElement) {
        target = document.body
      } else if (child instanceof HTMLHeadElement) {
        target = document.head
      } else {
        target = document.getElementById(child.id)!
        if (!target) {
          console.warn(error('PatchElementsNoTargetsFound'), {
            element: { id: child.id },
          })
          continue
        }
      }

      applyToTargets(mode as PatchElementsMode, child, [target])
    }
  } else {
    const targets = document.querySelectorAll(selector)
    if (!targets.length) {
      console.warn(error('PatchElementsNoTargetsFound'), { selector })
      return
    }

    applyToTargets(mode as PatchElementsMode, newContent, targets)
  }
}

const scripts = new WeakSet<HTMLScriptElement>()
for (const script of document.querySelectorAll('script')) {
  scripts.add(script)
}

const execute = (target: Element): void => {
  const elScripts =
    target instanceof HTMLScriptElement
      ? [target]
      : target.querySelectorAll('script')
  for (const old of elScripts) {
    if (!scripts.has(old)) {
      const script = document.createElement('script')
      for (const { name, value } of old.attributes) {
        script.setAttribute(name, value)
      }
      script.text = old.text
      old.replaceWith(script)
      scripts.add(script)
    }
  }
}

const applyPatchMode = (
  targets: Iterable<Element>,
  element: DocumentFragment | Element,
  action: string,
) => {
  for (const target of targets) {
    const cloned = element.cloneNode(true) as Element
    execute(cloned)
    // @ts-expect-error
    target[action](cloned)
  }
}

const applyToTargets = (
  mode: PatchElementsMode,
  element: DocumentFragment | Element,
  targets: Iterable<Element>,
) => {
  switch (mode) {
    case 'remove':
      for (const target of targets) {
        target.remove()
      }
      break
    case 'outer':
    case 'inner':
      for (const target of targets) {
        morph(target, element.cloneNode(true) as Element, mode)
        execute(target)
      }
      break
    case 'replace':
      applyPatchMode(targets, element, 'replaceWith')
      break
    case 'prepend':
    case 'append':
    case 'before':
    case 'after':
      applyPatchMode(targets, element, mode)
  }
}
