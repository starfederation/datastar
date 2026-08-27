export const tagToMs = (args: Set<string>) => {
  for (const arg of args) {
    if (arg.endsWith('ms')) {
      return +arg.slice(0, -2)
    }
    if (arg.endsWith('s')) {
      return +arg.slice(0, -1) * 1000
    }
    return Number.parseFloat(arg)
  }
  return 0
}

export const tagHas = (tags: Set<string>, tag: string) =>
  tags.has(tag.toLowerCase())

export const tagFirst = (tags?: Set<string>, defaultValue = ''): string => {
  if (tags) {
    for (const tag of tags) {
      return tag
    }
  }
  return defaultValue
}
