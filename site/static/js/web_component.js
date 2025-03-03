class ReverseComponent extends HTMLElement {
    static get observedAttributes() {
        return ['name'];
    }

    attributeChangedCallback(name, oldValue, newValue) {
        const value = newValue.split('').reverse().join('');
        this.dispatchEvent(new CustomEvent('reverse', {detail: {value}}));
    }
}

customElements.define('reverse-component', ReverseComponent);
