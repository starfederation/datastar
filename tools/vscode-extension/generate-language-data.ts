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

type AttributeDocumentation = {
  name: string
  description: string
  modifiers: Modifier[]
  pro: boolean
  reference: string
}

type Action = {
  name: string
  description: string
  signature?: string
  parameters?: ActionParameter[]
  pro: boolean
}

type ActionParameter = {
  label: string
}

type ActionOption = {
  name: string
  description: string
}

type ActionData = {
  actions: Action[]
  backendActionNames: string[]
  backendActionParameters: ActionParameter[]
  backendActionOptions: ActionOption[]
}

type DocsMetadata = {
  attributes: AttributeDocumentation[]
  actions: ActionData
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
}

type AttributeDefinition = AttributeSemantics & {
  completions: Record<string, string>
}

const extensionRoot = __dirname
const editorMetadataPath = path.join(extensionRoot, 'src', 'data-attributes.json')
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

const actionsFromDocs = (markdown: string): ActionData => {
  const lines = markdown.split(/\r?\n/)
  const actions: Action[] = []
  const backendActionNames = new Set<string>()
  let inActions = false
  let inBackendActions = false
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
      inBackendActions = false
      pro = true
      continue
    }
    if (/^## Backend Actions\s*$/.test(line)) {
      inBackendActions = true
      continue
    }
    if (/^## [^#]/.test(line)) {
      inBackendActions = false
    }

    const heading = line.match(/^### `@([A-Za-z_$][\w$]*)\(\)`/)
    if (!heading) continue
    if (inBackendActions) backendActionNames.add(heading[1])

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

  const options: ActionOption[] = []
  const actionsStart = lines.findIndex(line => /^# Actions\s*$/.test(line))
  const backendActionsStart = lines.findIndex((line, index) => (
    index > actionsStart && /^## Backend Actions\s*$/.test(line)
  ))
  const optionsStart = lines.findIndex((line, index) => (
    index > backendActionsStart && /^### Options\s*$/.test(line)
  ))
  if (optionsStart !== -1) {
    for (let index = optionsStart + 1; index < lines.length; index++) {
      const line = lines[index]
      if (/^### /.test(line)) break
      const option = line.match(/^- `([A-Za-z_$][\w$]*)`\s+[–-]\s+(.+)$/)
      if (option) options.push({ name: option[1], description: option[2] })
    }
  }
  if (!options.some(option => option.name === 'contentType')
    || !options.some(option => option.name === 'requestCancellation')) {
    throw new Error(`Could not parse backend action options from ${docsUrl}.`)
  }

  const backendActions = actions.filter(action => backendActionNames.has(action.name))
  const backendActionParameters = backendActions[0]?.parameters
  if (!backendActionParameters || !backendActions.every(action =>
    JSON.stringify(action.parameters) === JSON.stringify(backendActionParameters)
  )) {
    throw new Error(`Backend action parameters differ in ${docsUrl}.`)
  }

  return {
    actions: actions.map(action => backendActionNames.has(action.name)
      ? { name: action.name, description: action.description, pro: action.pro }
      : action),
    backendActionNames: [...backendActionNames],
    backendActionParameters,
    backendActionOptions: options,
  }
}

const attributesFromDocs = (markdown: string): AttributeDocumentation[] => {
  const lines = markdown.split(/\r?\n/)
  const modifiers = modifiersFromDocs(markdown)
  const attributes: AttributeDocumentation[] = []
  let inAttributes = false
  let pro = false

  for (let index = 0; index < lines.length; index++) {
    const line = lines[index]
    if (/^# Attributes\s*$/.test(line)) {
      inAttributes = true
      continue
    }
    if (inAttributes && /^# [^#]/.test(line)) break
    if (!inAttributes) continue
    if (/^## Pro Attributes\s*$/.test(line)) {
      pro = true
      continue
    }
    if (/^## [^#]/.test(line)) break

    const heading = line.match(/^### `data-([^`]+)`/)
    if (!heading) continue
    const name = pluginName(`data-${heading[1]}`)
    const descriptionLines: string[] = []
    for (index++; index < lines.length; index++) {
      const detail = lines[index]
      if (!detail.trim()) {
        if (descriptionLines.length) break
        continue
      }
      if (/^#{1,4} |^```|^> /.test(detail)) break
      descriptionLines.push(detail.trim())
    }
    index--
    if (!descriptionLines.length) {
      throw new Error(`Could not parse data-${name} description from ${docsUrl}.`)
    }
    attributes.push({
      name,
      description: descriptionLines.join(' '),
      modifiers: modifiers[name] || [],
      pro,
      reference: `https://data-star.dev/reference/attributes#data-${name}`,
    })
  }

  const nonceDescription = markdown.match(
    /To enable CSP mode, add a `data-nonce` attribute to the `html` element\.[^\n]+/,
  )?.[0]
  if (!nonceDescription) {
    throw new Error(`Could not parse data-nonce description from ${docsUrl}.`)
  }
  attributes.push({
    name: 'nonce',
    description: nonceDescription,
    modifiers: [],
    pro: false,
    reference: 'https://data-star.dev/reference/security#content-security-policy',
  })

  if (!attributes.some(attribute => attribute.name === 'attr')
    || !attributes.some(attribute => attribute.name === 'animate')) {
    throw new Error(`Could not parse all Datastar attributes from ${docsUrl}.`)
  }
  return attributes
}

const docsMetadataFromDocs = (markdown: string): DocsMetadata => ({
  attributes: attributesFromDocs(markdown),
  actions: actionsFromDocs(markdown),
})

const loadDocsMetadata = async (): Promise<DocsMetadata> => {
  if (checkOnly) {
    if (!fs.existsSync(outputPath)) {
      throw new Error(`Missing ${path.relative(extensionRoot, outputPath)}. Run npm run generate.`)
    }
    const cached = JSON.parse(fs.readFileSync(outputPath, 'utf8')) as {
      attributes: AttributeDocumentation[]
    } & ActionData
    return {
      attributes: cached.attributes.map(attribute => ({
        name: attribute.name,
        description: attribute.description,
        modifiers: attribute.modifiers,
        pro: attribute.pro,
        reference: attribute.reference,
      })),
      actions: {
        actions: cached.actions,
        backendActionNames: cached.backendActionNames,
        backendActionParameters: cached.backendActionParameters,
        backendActionOptions: cached.backendActionOptions,
      },
    }
  }

  return docsMetadataFromDocs(await fetchDocs())
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
  fs.readFileSync(editorMetadataPath, 'utf8'),
) as Record<string, AttributeDefinition>

const pluginName = (attributeName: string): string =>
  attributeName.slice('data-'.length).split(':', 1)[0].split('__', 1)[0]

const generateLanguageData = (
  docsMetadata: DocsMetadata,
) => {
  const attributeDocumentation = new Map(
    docsMetadata.attributes.map(attribute => [attribute.name, attribute]),
  )
  const missingDefinitions = docsMetadata.attributes
    .filter(attribute => attributeDefinitions[attribute.name] === undefined)
    .map(attribute => attribute.name)
  if (missingDefinitions.length) {
    throw new Error(
      `Editor metadata is missing for docs attributes: ${missingDefinitions.join(', ')}.`,
    )
  }
  const completions = Object.entries(attributeDefinitions).flatMap(
    ([declaredPluginName, definition]) =>
      Object.entries(definition.completions).map(([name, snippet]) => {
        const completionPluginName = pluginName(name)
        if (completionPluginName !== declaredPluginName) {
          throw new Error(
            `Completion ${name} belongs to ${completionPluginName}, not ${declaredPluginName}.`,
          )
        }
        const documentation = attributeDocumentation.get(declaredPluginName)
        if (!documentation) {
          throw new Error(`Missing docs metadata for data-${declaredPluginName}.`)
        }
        return {
          name,
          pluginName: declaredPluginName,
          insertText: snippet,
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
    const definition = attributeDefinitions[name]
    const documentation = attributeDocumentation.get(name)
    if (!documentation) {
      throw new Error(`Missing docs metadata for data-${name}.`)
    }
    return {
      name,
      description: documentation.description,
      reference: documentation.reference,
      requirement: definition.requirement,
      modifiers: documentation.modifiers,
      signals: definition.signals,
      valueKind: definition.valueKind || 'expression',
      keys: definition.keys,
      element: definition.element,
      pro: documentation.pro,
    }
  })

  return {
    version: 1,
    attributes,
    completions,
    nativeEvents: nativeEventNames(),
    ...docsMetadata.actions,
  }
}

const grammarWithAttributes = (data: ReturnType<typeof generateLanguageData>) => {
  const grammar = JSON.parse(fs.readFileSync(grammarPath, 'utf8')) as {
    repository: Record<
      string,
      { begin: string; patterns: Array<{ match: string }> }
    >
  }
  const names = data.attributes.map((attribute) => attribute.name)
  const attrList = names.join('|')
  grammar.repository['datastar-attribute'].begin =
    `\\b(data-)(${attrList})(?=__|:|[\\s>=])`
  grammar.repository['datastar-attribute'].patterns[0].match =
    `(:)(data-(?:${attrList}))(?=__|:|[\\s>=])`
  return `${JSON.stringify(grammar, null, 2)}\n`
}

const main = async () => {
  const docsMetadata = await loadDocsMetadata()
  const data = generateLanguageData(docsMetadata)
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
