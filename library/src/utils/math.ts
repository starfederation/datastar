export function clamp(value: number, min: number, max: number) {
  return Math.max(min, Math.min(max, value))
}

export function lerp(min: number, max: number, t: number, clamped = true) {
  const v = min + (max - min) * t
  return clamped ? clamp(v, min, max) : v
}

export function inverseLerp(
  min: number,
  max: number,
  value: number,
  clamped = true,
) {
  if (value < min) return 0
  if (value > max) return 1
  const v = (value - min) / (max - min)
  return clamped ? clamp(v, min, max) : v
}
