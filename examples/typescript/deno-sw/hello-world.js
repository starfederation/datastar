export function getHelloWorldHtml() {
    return `<!DOCTYPE html>
<html lang="en">
<head>
    <title>Datastar SDK Demo</title>
    <script>
        if ('serviceWorker' in navigator) {
            window.addEventListener('load', async () => {
                try {
                    const registration = await navigator.serviceWorker.register('/service-worker.js');
                    console.log('ServiceWorker registration successful with scope:', registration.scope);
                } catch (err) {
                    console.error('ServiceWorker registration failed:', err);
                }
            });
        }
    </script>
    <script src="/static/tailwind.js"></script>
    <script type="module" src="/static/datastar.js"></script>
</head>
<body class="bg-white dark:bg-gray-900 text-lg max-w-xl mx-auto my-16">
    <div data-signals-delay="400" class="bg-white dark:bg-gray-800 text-gray-500 dark:text-gray-400 rounded-lg px-6 py-8 ring shadow-xl ring-gray-900/5 space-y-2">
        <div class="flex justify-between items-center">
            <h1 class="text-gray-900 dark:text-white text-3xl font-semibold">Datastar SDK Demo</h1>
            <img src="/static/rocket.png" alt="Rocket" width="64" height="64" />
        </div>
        <p class="mt-2">SSE events will be streamed from the backend to the frontend.</p>
        <div class="space-x-2">
            <label for="delay">Delay in milliseconds</label>
            <input data-bind-delay id="delay" type="number" step="100" min="0" class="w-36 rounded-md border border-gray-300 px-3 py-2 placeholder-gray-400 shadow-sm focus:border-sky-500 focus:outline focus:outline-sky-500 dark:disabled:border-gray-700 dark:disabled:bg-gray-800/20" />
        </div>
        <button data-on-click="@get('/hello-world')" class="rounded-md bg-sky-500 px-5 py-2.5 leading-5 font-semibold text-white hover:bg-sky-700 hover:text-gray-100 cursor-pointer">Start</button>
    </div>
    <div class="my-16 text-8xl font-bold text-transparent" style="background: linear-gradient(to right in oklch, red, orange, yellow, green, blue, blue, violet); background-clip: text">
        <div id="message">Hello, world!</div>
    </div>
</body>
</html>`;
}