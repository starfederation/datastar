export const tagToMs = (args: Set<string>) => {
  if (!args || args.size <= 0) return 0
  for (const arg of args) {
    if (arg.endsWith('ms')) {
      return +arg.replace('ms', '')
    }
    if (arg.endsWith('s')) {
      return +arg.replace('s', '') * 1000
    }
    try {
      return Number.parseFloat(arg)
    } catch (_) {}
  }
  return 0
}

export const tagHas = (
  tags: Set<string>,
  tag: string,
  defaultValue = false,
) => {
  if (!tags) return defaultValue
  return tags.has(tag.toLowerCase())
}

export const tagFirst = (tags?: Set<string>, defaultValue = ''): string => {
  if (tags && tags.size > 0) {
    for (const tag of tags) {
      return tag
    }
  }
  return defaultValue
}
