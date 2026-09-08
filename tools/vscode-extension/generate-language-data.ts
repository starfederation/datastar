import fs from 'node:fs'
import https from 'node:https'
import path from 'node:path'
import ts from 'typescript'

type Requirement = 'allowed' | 'must' | 'denied' | 'exclusive'
type SignalStrategy = 'key' | 'key-or-value' | 'key-or-object'

type Modifier = {
  name: string
  description?: string
}

type Action = {
  name: string
  description: string
  signature: string
  parameters: Array<{ label: string }>
  pro: boolean
}

type AttributeSemantics = {
  requirement: {
    key: Requirement
    value: Requirement
  }
  signals?: SignalStrategy
  valueKind?: 'expression' | 'signal-name' | 'string'
  keys?: string[]
  element?: string
  highlight?: boolean
}

type Snippet = {
  body: string
  description: string
  references?: Array<{ name: string; url: string }>
}

type AttributeDefinition = AttributeSemantics & {
  completions: Record<string, Snippet>
}

const extensionRoot = __dirname
const snippetsPath = path.join(extensionRoot, 'src', 'data-attributes.json')
const modifiersPath = path.join(extensionRoot, 'src', 'modifier-metadata.json')
const actionsPath = path.join(extensionRoot, 'src', 'action-metadata.json')
const proAttributesPath = path.join(extensionRoot, 'src', 'pro-attribute-metadata.json')
const outputPath = path.join(extensionRoot, 'src', 'language-data.json')
const grammarPath = path.join(
  extensionRoot,
  'src',
  'datastar.injection.tmLanguage.json',
)
const checkOnly = process.argv.includes('--check')
const docsUrl = 'https://data-star.dev/docs.md'
let docsPromise: Promise<string> | undefined

const fetchText = (url: string, redirects = 0): Promise<string> =>
  new Promise((resolve, reject) => {
    https
      .get(url, { headers: { 'User-Agent': 'datastar-vscode-generator' } }, (response) => {
        const status = response.statusCode || 0
        const location = response.headers.location
        if (status >= 300 && status < 400 && location) {
          response.resume()
          if (redirects >= 5) {
            reject(new Error(`Too many redirects while fetching ${docsUrl}.`))
            return
          }
          resolve(fetchText(new URL(location, url).toString(), redirects + 1))
          return
        }
        if (status !== 200) {
          response.resume()
          reject(new Error(`Failed to fetch ${url}: HTTP ${status}.`))
          return
        }

        response.setEncoding('utf8')
        let body = ''
        response.on('data', (chunk: string) => {
          body += chunk
        })
        response.on('end', () => resolve(body))
      })
      .on('error', reject)
  })

const fetchDocs = (): Promise<string> => docsPromise ||= fetchText(docsUrl)

const modifiersFromDocs = (markdown: string): Record<string, Modifier[]> => {
  const result = new Map<string, Map<string, Modifier>>()
  let attributeName: string | undefined
  let modifierName: string | undefined
  let inModifiers = false

  for (const line of markdown.split(/\r?\n/)) {
    const attributeHeading = line.match(/^### `data-([^`]+)`/)
    if (attributeHeading) {
      attributeName = pluginName(`data-${attributeHeading[1]}`)
      modifierName = undefined
      inModifiers = false
      continue
    }
    if (/^### /.test(line)) {
      attributeName = undefined
      modifierName = undefined
      inModifiers = false
      continue
    }
    if (line === '#### Modifiers') {
      inModifiers = attributeName !== undefined
      continue
    }
    if (/^#### /.test(line)) {
      modifierName = undefined
      inModifiers = false
      continue
    }
    if (!inModifiers || !attributeName) continue

    const modifier = line.match(
      /^- `__([a-z][a-z0-9-]*)`(?:\s+\*+)?\s+[–-]\s+(.+)$/,
    )
    if (modifier) {
      modifierName = modifier[1]
      const modifiers = result.get(attributeName) || new Map<string, Modifier>()
      modifiers.set(modifierName, {
        name: modifierName,
        description: modifier[2],
      })
      result.set(attributeName, modifiers)
      continue
    }

    const tag = line.match(/^\s+- `\.([^`]+)`\s+[–-]\s+(.+)$/)
    if (tag && modifierName) {
      const name = `${modifierName}.${tag[1]}`
      result.get(attributeName)!.set(name, {
        name,
        description: tag[2],
      })
    }
  }

  const modifiers = Object.fromEntries(
    [...result].map(([name, entries]) => [name, [...entries.values()]]),
  )
  if (!modifiers.on?.some((modifier) => modifier.name === 'document')) {
    throw new Error(`Could not parse data-on modifiers from ${docsUrl}.`)
  }
  return modifiers
}

const loadModifiers = async (): Promise<Record<string, Modifier[]>> => {
  if (checkOnly) {
    if (!fs.existsSync(modifiersPath)) {
      throw new Error(`Missing ${path.relative(extensionRoot, modifiersPath)}. Run npm run generate.`)
    }
    return JSON.parse(fs.readFileSync(modifiersPath, 'utf8')) as Record<
      string,
      Modifier[]
    >
  }

  const modifiers = modifiersFromDocs(await fetchDocs())
  fs.writeFileSync(modifiersPath, `${JSON.stringify(modifiers, null, 2)}\n`)
  return modifiers
}

const splitParameters = (source: string): string[] => {
  const parameters: string[] = []
  const closing: string[] = []
  const pairs: Record<string, string> = { '(': ')', '[': ']', '{': '}', '<': '>' }
  let quote: string | undefined
  let start = 0

  for (let index = 0; index < source.length; index++) {
    const char = source[index]
    if (quote) {
      if (char === quote && source[index - 1] !== '\\') quote = undefined
    } else if (char === '"' || char === "'" || char === '`') {
      quote = char
    } else if (pairs[char]) {
      closing.push(pairs[char])
    } else if (closing.at(-1) === char) {
      closing.pop()
    } else if (char === ',' && closing.length === 0) {
      parameters.push(source.slice(start, index).trim())
      start = index + 1
    }
  }

  const last = source.slice(start).trim()
  if (last) parameters.push(last)
  return parameters
}

const actionsFromDocs = (markdown: string): Action[] => {
  const lines = markdown.split(/\r?\n/)
  const actions: Action[] = []
  let inActions = false
  let pro = false

  for (let index = 0; index < lines.length; index++) {
    const line = lines[index]
    if (line === '# Actions') {
      inActions = true
      continue
    }
    if (inActions && /^# [^#]/.test(line)) break
    if (!inActions) continue
    if (/^## Pro Actions\s*$/.test(line)) {
      pro = true
      continue
    }

    const heading = line.match(/^### `@([A-Za-z_$][\w$]*)\(\)`/)
    if (!heading) continue

    let signature: string | undefined
    let description: string | undefined
    for (index++; index < lines.length; index++) {
      const detail = lines[index]
      if (/^#{1,3} /.test(detail)) {
        index--
        break
      }
      const signatureMatch = detail.match(/^> `(@[^`]+)`$/)
      if (signatureMatch) {
        signature = signatureMatch[1]
      } else if (signature && detail && !detail.startsWith('>') && !detail.startsWith('```')) {
        description = detail
        break
      }
    }

    if (!signature || !description) {
      throw new Error(`Could not parse @${heading[1]} action metadata from ${docsUrl}.`)
    }
    const open = signature.indexOf('(')
    const close = signature.lastIndexOf(')')
    if (open === -1 || close < open) {
      throw new Error(`Could not parse @${heading[1]} signature from ${docsUrl}.`)
    }

    actions.push({
      name: heading[1],
      description,
      signature,
      parameters: splitParameters(signature.slice(open + 1, close)).map(label => ({ label })),
      pro,
    })
  }

  if (!actions.some(action => action.name === 'peek') || !actions.some(action => action.name === 'get')) {
    throw new Error(`Could not parse Datastar actions from ${docsUrl}.`)
  }
  return actions
}

const loadActions = async (): Promise<Action[]> => {
  if (checkOnly) {
    if (!fs.existsSync(actionsPath)) {
      throw new Error(`Missing ${path.relative(extensionRoot, actionsPath)}. Run npm run generate.`)
    }
    return JSON.parse(fs.readFileSync(actionsPath, 'utf8')) as Action[]
  }

  const actions = actionsFromDocs(await fetchDocs())
  fs.writeFileSync(actionsPath, `${JSON.stringify(actions, null, 2)}\n`)
  return actions
}

const proAttributesFromDocs = (markdown: string): string[] => {
  const names: string[] = []
  let inProAttributes = false

  for (const line of markdown.split(/\r?\n/)) {
    if (/^## Pro Attributes\s*$/.test(line)) {
      inProAttributes = true
      continue
    }
    if (inProAttributes && /^##? [^#]/.test(line)) break
    if (!inProAttributes) continue

    const heading = line.match(/^### `data-([^`]+)`/)
    if (heading) names.push(pluginName(`data-${heading[1]}`))
  }

  if (names.length === 0) {
    throw new Error(`Could not parse Datastar Pro attributes from ${docsUrl}.`)
  }
  return names
}

const loadProAttributes = async (): Promise<string[]> => {
  if (checkOnly) {
    if (!fs.existsSync(proAttributesPath)) {
      throw new Error(`Missing ${path.relative(extensionRoot, proAttributesPath)}. Run npm run generate.`)
    }
    return JSON.parse(fs.readFileSync(proAttributesPath, 'utf8')) as string[]
  }

  const names = proAttributesFromDocs(await fetchDocs())
  fs.writeFileSync(proAttributesPath, `${JSON.stringify(names, null, 2)}\n`)
  return names
}

const nativeEventNames = (): string[] => {
  const libDomPath = path.join(
    path.dirname(require.resolve('typescript')),
    'lib.dom.d.ts',
  )
  const program = ts.createProgram([libDomPath], {
    noLib: true,
    skipLibCheck: true,
  })
  const source = program.getSourceFile(libDomPath)
  const eventMap = source?.statements.find(
    (statement): statement is ts.InterfaceDeclaration =>
      ts.isInterfaceDeclaration(statement) &&
      statement.name.text === 'HTMLElementEventMap',
  )
  if (!eventMap) {
    throw new Error('Could not find HTMLElementEventMap in TypeScript DOM types.')
  }

  return program
    .getTypeChecker()
    .getPropertiesOfType(program.getTypeChecker().getTypeAtLocation(eventMap))
    .map((event) => event.name)
    .sort()
}

const attributeDefinitions = JSON.parse(
  fs.readFileSync(snippetsPath, 'utf8'),
) as Record<string, AttributeDefinition>

const pluginName = (attributeName: string): string =>
  attributeName.slice('data-'.length).split(':', 1)[0].split('__', 1)[0]

const generateLanguageData = (
  modifiers: Record<string, Modifier[]>,
  actions: Action[],
  proAttributes: string[],
) => {
  const completions = Object.entries(attributeDefinitions).flatMap(
    ([declaredPluginName, definition]) =>
      Object.entries(definition.completions).map(([name, snippet]) => {
        const completionPluginName = pluginName(name)
        if (completionPluginName !== declaredPluginName) {
          throw new Error(
            `Completion ${name} belongs to ${completionPluginName}, not ${declaredPluginName}.`,
          )
        }
        return {
          name,
          pluginName: declaredPluginName,
          insertText: snippet.body,
          description: snippet.description,
          references: snippet.references || [],
        }
      }),
  )
  const attributeNames = Object.keys(attributeDefinitions)
  const missingCompletions = attributeNames.filter(
    (name) => attributeDefinitions[name].completions === undefined
      || Object.keys(attributeDefinitions[name].completions).length === 0,
  )
  if (missingCompletions.length) {
    throw new Error(
      `Attribute metadata is missing completions: ${missingCompletions.join(', ')}.`,
    )
  }

  const attributes = attributeNames.sort().map((name) => {
    const completion = completions.find(
      (candidate) => candidate.pluginName === name,
    )!
    const definition = attributeDefinitions[name]
    return {
      name,
      description: completion.description,
      requirement: definition.requirement,
      modifiers: modifiers[name] || [],
      signals: definition.signals,
      valueKind: definition.valueKind || 'expression',
      keys: definition.keys,
      element: definition.element,
      highlight: definition.highlight ?? true,
      pro: proAttributes.includes(name),
    }
  })

  return { version: 1, attributes, completions, nativeEvents: nativeEventNames(), actions }
}

const grammarWithAttributes = (data: ReturnType<typeof generateLanguageData>) => {
  const grammar = JSON.parse(fs.readFileSync(grammarPath, 'utf8')) as {
    repository: Record<
      string,
      { begin: string; patterns: Array<{ match: string }> }
    >
  }
  const names = data.attributes
    .filter((attribute) => attribute.highlight)
    .map((attribute) => attribute.name)
  const attrList = names.join('|')
  grammar.repository['datastar-attribute'].begin =
    `\\b(data-)(${attrList})(?=__|:|[\\s>=])`
  grammar.repository['datastar-attribute'].patterns[0].match =
    `(:)(data-(?:${attrList}))(?=__|:|[\\s>=])`
  return `${JSON.stringify(grammar, null, 2)}\n`
}

const main = async () => {
  const [modifiers, actions, proAttributes] = await Promise.all([
    loadModifiers(),
    loadActions(),
    loadProAttributes(),
  ])
  const data = generateLanguageData(modifiers, actions, proAttributes)
  const languageDataContent = `${JSON.stringify(data, null, 2)}\n`
  const grammarContent = grammarWithAttributes(data)

  if (checkOnly) {
    const failures: string[] = []
    if (
      !fs.existsSync(outputPath) ||
      fs.readFileSync(outputPath, 'utf8') !== languageDataContent
    ) {
      failures.push(path.relative(extensionRoot, outputPath))
    }
    if (fs.readFileSync(grammarPath, 'utf8') !== grammarContent) {
      failures.push(path.relative(extensionRoot, grammarPath))
    }
    if (failures.length) {
      console.error(`Generated files are out of date: ${failures.join(', ')}`)
      console.error('Run npm run generate.')
      process.exitCode = 1
    }
  } else {
    fs.writeFileSync(outputPath, languageDataContent)
    fs.writeFileSync(grammarPath, grammarContent)
    console.log(
      `Generated ${data.attributes.length} attributes, ${data.completions.length} completions, and ${data.actions.length} actions from ${docsUrl}.`,
    )
  }
}

void main().catch((error: unknown) => {
  console.error(error instanceof Error ? error.message : error)
  process.exitCode = 1
})
