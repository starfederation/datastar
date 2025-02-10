import { LitElement, html } from 'lit'
import { customElement, property } from 'lit/decorators.js'
import { createRef, ref } from 'lit/directives/ref.js'

// Originally from https://codepen.io/Vicente-Alcazar/pen/NWZbmqP
const TAU = Math.PI * 2
const rand = Math.random

@customElement('ds-starfield')
export class DatastarStarfield extends LitElement {
  @property({ type: Number })
  numStars = 5000

  @property({ type: Number, attribute: 'center-x' })
  centerXPercentage = 25

  @property({ type: Number, attribute: 'center-y' })
  centerYPercentage = 50

  @property({ type: Number })
  size = 2.5

  @property({ type: Number })
  speed = 10

  @property({ type: Number })
  streak = 50

  stars: { x: number; y: number; z: number }[] = []

  canvasRef = createRef<HTMLCanvasElement>()
  ctx: CanvasRenderingContext2D | null = null

  firstUpdated(): void {
    const canvas = this.canvasRef.value
    if (!canvas) {
      throw new Error('Canvas not found')
    }

    canvas.width = canvas.parentElement?.clientWidth || window.innerWidth
    canvas.height = canvas.parentElement?.clientHeight || window.innerHeight

    this.ctx = canvas.getContext('2d')
    this.#resetStars()
    this.#animate()
  }

  #center() {
    if (!this.canvasRef.value) throw new Error('Canvas not found')
    const { width, height } = this.canvasRef.value
    const centerX = (width * this.centerXPercentage) / 100
    const centerY = (height * this.centerYPercentage) / 100
    return { centerX, centerY }
  }

  #resetStars() {
    if (!this.canvasRef.value) throw new Error('Canvas not found')

    const { width, height } = this.canvasRef.value
    const { centerX, centerY } = this.#center()

    this.stars = []
    for (let i = 0; i < this.numStars; i++) {
      this.stars.push({
        x: rand() * width - centerX,
        y: rand() * height - centerY,
        z: rand() * width,
      })
    }
  }

  #animate() {
    if (!this.ctx) throw new Error('Canvas not found')
    if (!this.canvasRef.value) throw new Error('Canvas not found')
    const { width, height } = this.canvasRef.value
    const { centerX, centerY } = this.#center()

    const style = getComputedStyle(this)
    const color = style.getPropertyValue('color')
    const background = style.getPropertyValue('background-color')

    this.ctx.clearRect(0, 0, width, height)
    this.ctx.fillStyle = background
    this.ctx.fillRect(0, 0, width, height)

    // Draw stars
    this.ctx.fillStyle = color
    for (const star of this.stars) {
      const x = (star.x / star.z) * width + centerX
      const y = (star.y / star.z) * height + centerY
      const size = this.size * (1 - star.z / width)

      this.ctx.beginPath()
      this.ctx.arc(x, y, size, -this.streak * TAU, this.streak * TAU)
      this.ctx.fill()

      star.z -= this.speed

      if (star.z <= 0) {
        star.z = width
        star.x = rand() * width - centerX
        star.y = rand() * height - centerY
      }
    }

    requestAnimationFrame(() => this.#animate())
  }

  render() {
    return html`
        <canvas
            ${ref(this.canvasRef)}
            style="width: 100%; height: 100%;">
        </canvas>
    `
  }
}

declare global {
  interface HTMLElementTagNameMap {
    'ds-starfield': DatastarStarfield
  }
}
