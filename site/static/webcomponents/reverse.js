class ReverseComponent extends HTMLElement {
    static get observedAttributes() {
        return ['name'];
    }

    attributeChangedCallback(name, oldValue, newValue) {
        const reversed = newValue.split('').reverse().join('');
        this.dispatchEvent(new CustomEvent('reverse', {detail: {reversed}}));
    }
}

customElements.define('reverse-component', ReverseComponent);
