export class LruCache<T> {
  private entries: Map<string, T> = new Map<string, T>()
  constructor(private maxEntries = 64) {}

  public get(key: string) {
    const entry = this.entries.get(key)
    if (entry) {
      this.entries.delete(key)
      this.entries.set(key, entry)
    }
    return entry
  }

  public set(key: string, value: T) {
    if (this.entries.size >= this.maxEntries) {
      const keyToDelete = this.entries.keys().next().value
      if (!keyToDelete) return
      this.entries.delete(keyToDelete)
    }
    this.entries.set(key, value)
  }
}
