// const url = 'https://data-star.dev/errors'
const url = 'http://localhost:8080/errors'

export const hasValNonExpr = /([\w0-9.]+)\.value/gm

export const dsErr = (code: string, args?: any) => {
  const e = new Error()
  // code = code.charAt(0).toUpperCase() + code.slice(1)
  e.name = code
  const fullURL = `${url}/${code}?${new URLSearchParams(args)}`
  e.message = `for more info see ${fullURL}`
  return e
}
