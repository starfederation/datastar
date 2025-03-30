import { DSS } from '../engine/consts'
import type { SignalsRoot } from '../engine/signals'
import { trimDollarSignPrefix } from './text'

export function pathMatchesPattern(path: string, pattern: string) {
  // Do a little dance to correctly replace the double astersik
  pattern = pattern
    .replaceAll('.', '\\.')
    .replaceAll('**', DSS)
    .replaceAll('*', '[^\\.]*')
    .replaceAll(DSS, '.*')
  const regex = new RegExp(`^${pattern}$`)

  return regex.test(path)
}

export function getMatchingSignalPaths(signals: SignalsRoot, paths: string) {
  const matches: string[] = []
  let patterns = paths.split(/\s+/).filter((p) => p !== '')
  patterns = patterns.map((p) => trimDollarSignPrefix(p))

  for (const pattern of patterns) {
    signals.walk((signalPath) => {
      if (pathMatchesPattern(signalPath, pattern)) {
        matches.push(signalPath)
      }
    })
  }

  return matches
}