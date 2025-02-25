# Yes, you want a build step

## In response to the HTMX essay [No Build Step](https://htmx.org/essays/no-build-step/)

This is making the counter-argument that a build step is extremely valuable when building a frontend framework. Lack of
builds might be the right choice for HTMX but in my eyes it's a self-fulfilling prophecy. The author is concurrently
wary of progress yet works on a library about extending the HTML spec to do more than it was designed for. The essay is
written by a core contributor and not the author of HTMX. In general, it's filled with so many fallacies but the real
culprit is the _false dilemma fallacy_.

> This common fallacy misleads by presenting complex issues in terms of two inherently opposed sides. Instead of
acknowledging that most (if not all) issues can be thought of on a spectrum of possibilities and stances, the false
dilemma fallacy asserts that there are only two mutually exclusive outcomes.
> This fallacy is particularly problematic because it can lend false credence to extreme stances, ignoring opportunities
for compromise or chances to re-frame the issue in a new way.

The whole discussion is framed as a false dilemma. Almost every point he makes is demonstrably false by the counter
example of Datastar. Its smaller, faster, supports more features, isn't written in JavaScript, targets multiple browser
versions, and has a build step. If he is trying to prove his point it needs to be done with more rigor. In general I
find the HTMX essays to be a beacon for well articulated arguments; this one is not and does a disservice to the
community exploring hypermedia.

## TED Talk

In the first paragraph he says both:

> A recurring question from some htmx contributors is why htmx isn’t written in TypeScript, or, for that matter, why
htmx **_lacks any build step at all_**. The full htmx source is a single 3,500-line JavaScript file; if you want to
contribute to htmx, you do so by modifying the htmx.js file, the same file that gets sent to browsers in production,
**_give or take minification and compression_**.

So before the essay starts he's given up the ghost. Even in as close to pure JavaScript as you can get you still need a
build step. Thank you, this has been my TED talk.

## Write once, run forever

> The best reason to write a library in plain JavaScript is that it lasts forever.

This is just not the world we live in. ES5 is dead, IE11 is dead, ECMAScript doesn't look anything like it did a decade
ago, let alone 20 years ago. ECMAScript has been around for 25 years and has changed a lot in that time, let alone the
APIs you are interfacing with. The web is a moving target and you need to keep up with it. If you don't you will be left
behind.

## Reduction in labor

> Maintenance is a cost paid for with labor, and open-source code bases are the projects that can least afford to pay
it. Opting not to use a build step drastically minimizes the labor required to keep htmx up-to-date.

No, just no. Much like CI/CD pipelines you set it up and largely forget about it. You can even automate the updates. The
build step is a one time cost that pays dividends for the life of the project. If you don't have a build step you are
going to be spending a lot of time manually updating your codebase. This is a false dichotomy. Datastar supports IIFE,
UMD, & Modules and took all of adding the options to the build step. I've seen the crazy wrapper that HTMX uses to
support all the different module types. It's a lot of code and a lot of maintenance. I'm not saying that HTMX is doing
it wrong, just that it's a lot of work to support all the different module types. I'm not sure how much time it saves in
the long run.

> But htmx is written in JavaScript, with no dependencies, so it will run unmodified for as long as web browsers remain
relevant. Let the browser vendors do the hard work for you.

Every Datastar build or CDN version is a single JavaScript file with no dependencies. The FUD here doesn't match reality
at sets up a narrative of purity that is just not true. If fact oddly **because** of our build step we have full source
mapping supported so you can debug your the production minified version in the browser looking at the original source
and stack. This is a huge win for developers. I'm not sure why you would want to give that up.

## Source Maps and Hot Module Reloading

> Build step advocates point out that TypeScript can generate source maps, which tell your browser what TypeScript
corresponds to what JavaScript, and that’s true! But now you have another thing to keep track of—the TypeScript you
wrote, the JavaScript it generated, and the source map that connects these two. The hot-reloading development server
you’re now dependent on will keep these up to date for you on localhost

Yes you have **_the same build folder_** with more static files that reference each other. This is not a big deal, a
weird straw man, non-issue. You have to run a dev server to test your code anyway for things like CORS to work. This is
just a weird argument, especially in light of things like hot module reloading for both code and styles. Modern tooling
is now in the _sub milliseconds_ for rebuilds. Want to run your tests in parallel, well now you are just missing out on
the real DX that modern tooling provides.

> ...but what about on your staging server? What about in production? Bugs that appear in these environments will be
harder to track down, because you’ve lost a lot of information about where they come from.

Github actions for `bun build` or `vite build` are dead easy, and with weird arguments that are just not an issue
nowadays. Like complaining about Makefiles when you have Visual Studio.

## The DX

> The htmx DX is very simple—your browser loads a single file, which in every environment is the exact same file you
wrote. The tradeoffs required to maintain that experience are real, but they’re tradeoffs that make sense for this
project.

The DX is why I personally moved on. The core monkey patches the DOM and that's just not a good DX. The core is not very
extendable and you have to keep all 3000 LOC in your head to understand the ramification of your changes let alone the
changes that other people make. You are giving up a lot of the DX that you get with a build step.

> Requiring that htmx remain in a single file (again, around 3,500 LOC) enforces a degree of intention on the library;
there is a real pressure when working on the htmx source to justify the addition of new code, a pressure which maintains
an equilibrium of relative simplicity.

Or... make everything an extension and allow them to rely on each other at there discretion. Making it a single file
doesn't do anything but make harder to maintain.

## Amalgamation is a build step

> the lack of places for functionality to hide makes working on htmx a lot more approachable. Far, far more complex
projects use aspects of this approach as well: SQLite3 compiles from a single-file source amalgamation (though they use
separate files for development, they’re not crazy) which makes hacking on it significantly easier. You could never build
the linux kernel this way—but htmx is not the linux kernel.

This is one of the worst takes in the whole essay. SQLite is **developed** as a set of files and **deployed** as an
amalgamation. Just like Datastar, Alpine, HTMX, Zod, etc. You are conflating distribution with development and even
making examples against you're own position.

> The htmx code does have to be written in JavaScript, though, because browsers run JavaScript. And as long as
JavaScript is dynamically typed, the tradeoffs required to get true static typing in the htmx source are not worth it
(htmx users can still take advantage of typed APIs, declared with .d.ts files).

The declaration (.d.ts) he point do are not accurate and aren't maintained last I checked. Also they are filled with
`any` escape hatches which don't say anything. Separating definition into documentation seems to always cause this to
happen. Conflating a build target with a language target. Like saying you have write in Assembly because x86 run
assembly. JavaScript can be a runtime target just like C can compile to assembly. You can write in Typescript and
compile to JavaScript. Another fallacy.

## Legacy targets stop progress

> Because htmx maintains support for Internet Explorer 11, and because it does not have a build step, every line of htmx
has to be written in IE11-compatible JavaScript, which means no ES6.

This is certainly a choice and have seems as a hot button debate in the HTMX discord channel. If you had a build step,
let alone used Typescript, you could target both **with the same codebase**. The false dilemma fallacy.

> This point is obvious, but it's worth re-stating: the htmx source would be a lot tidier if it could be split it into
modules. There are other factors that affect code quality besides tidiness, but to the extent that the htmx source is
high-quality, it is not because it is tidy.

This is just a weird argument. You can split it into modules and still have tidiness. You can still split up into
extensions, I know because I did it. To be clear, this isn't a declaration of minimal code sizes, but it _should_ be
broken up into at least a per module/extension basis.

> Once you no longer have an enormous codebase of frontend JavaScript, there is far less pressure to adopt JavaScript on
the backend. You can write backends in Python, Go, even NodeJS, and it doesn’t matter to htmx—every mainstream language
has mature solutions for formatting HTML. This is the principle of Hypermedia On Whatever you’d Like (HOWL).

This essay is about HTMX build step, not the backend. It appears to me that not only is it clear you are throwing out
the baby with the bathwater but demolishing the whole house. Just because SPA's are not the right tool for every job
doesn't mean that you should throw out the whole tool box. We are talking about the best way to build a frontend
library, not the backend.

## Conclusion

Even HTMX has a build step, and if they embraced it they could support multiple targets while still keeping their single
un-tree shakeable amalgamation that they enjoy developing with.