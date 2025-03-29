export function pathMatchesPattern(path: string, pattern: string) {
  const regex = new RegExp(
    `^${pattern.replaceAll('.', '\\.').replaceAll('*', '.*')}$`,
  )
  
  return regex.test(path)
}
