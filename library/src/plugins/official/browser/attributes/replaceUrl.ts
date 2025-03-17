// Authors: Delaney Gillilan
// Icon: carbon:url
// Slug: Replace the current URL with a new URL
// Description: This plugin allows you to replace the current URL with a new URL.  Once you add this attribute the current URL will be replaced with the new URL.

import {
  type AttributePlugin,
  PluginType,
  Requirement,
} from '../../../../engine/types'

export const ReplaceUrl: AttributePlugin = {
  type: PluginType.Attribute,
  name: 'replaceUrl',
  keyReq: Requirement.Denied,
  valReq: Requirement.Must,
  onLoad: ({ effect, genRX }) => {
    const { deps, rxFn } = genRX()
    return effect(deps, () => {
      const url = rxFn<string>()
      const baseUrl = window.location.href
      const fullUrl = new URL(url, baseUrl).toString()
      window.history.replaceState({}, '', fullUrl)
    })
  },
}
