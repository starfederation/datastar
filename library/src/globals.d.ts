declare const ALIAS: string | null

interface Element {
  startViewTransition(callback: () => void | Promise<void>): ViewTransition
}
