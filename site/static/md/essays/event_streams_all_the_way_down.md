# Streams All the Way Down

[SSE](https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events) is a really great idea. With a single request you can create a server driven stream of events. Using this for driving Datastar's [HTMX](https://htmx.org/) like fragments plugin is a natural fit. The only problem is that SSE doesn't support anything but the `GET` method. This means that you can't use SSE to send data to the server. This is a problem for HTMX, because HTMX uses the `POST` method to send data to the server. So, what to do?

## The Solution

`text/event-stream` MIME type is the underlying protocol for [Server Sent Events](https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events). It is a simple protocol that is easy to implement. It is also a simple protocol to extend. So, we can extend it to support `POST`,`PUT`,`PATCH`,`DELETE` requests. The majority of the code is in how your read and buffer results from [fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API) calls. The rest is just a matter of parsing the request and sending the appropriate response. The great part is the server side still just uses the same libraries or helpers that it would use for SSE.

This means every data fragment is an OOB in HTMX terms.

```html
event: datastar-merge-fragments
id: 92680296792588350
data: mergeMode morph
data: settleDuration 0
data: fragment
<div id="lazy_tabs">
  <div class="tabs tabs-bordered">
    <button
      id="tab_0"
      class="tab tab-active"
      data-on-click="@sse('/examples/lazy_tabs/data?tabId=0')"
    >
      Tab 0</button
    ><button
      id="tab_1"
      class="tab"
      data-on-click="@sse('/examples/lazy_tabs/data?tabId=1')"
    >
      Tab 1</button
    ><button
      id="tab_2"
      class="tab"
      data-on-click="@sse('/examples/lazy_tabs/data?tabId=2')"
    >
      Tab 2</button
    ><button
      id="tab_3"
      class="tab"
      data-on-click="@sse('/examples/lazy_tabs/data?tabId=3')"
    >
      Tab 3</button
    ><button
      id="tab_4"
      class="tab"
      data-on-click="@sse('/examples/lazy_tabs/data?tabId=4')"
    >
      Tab 4</button
    ><button
      id="tab_5"
      class="tab"
      data-on-click="@sse('/examples/lazy_tabs/data?tabId=5')"
    >
      Tab 5</button
    ><button
      id="tab_6"
      class="tab"
      data-on-click="@sse('/examples/lazy_tabs/data?tabId=6')"
    >
      Tab 6</button
    ><button
      id="tab_7"
      class="tab"
      data-on-click="@sse('/examples/lazy_tabs/data?tabId=7')"
    >
      Tab 7
    </button>
  </div>
  <div id="tab_content" class="p-4 shadow-lg bg-base-200">
    <p>
      Voluptas minima totam qui est ut. Fuga accusantium sint voluptatem nihil
      asperiores. Saepe illo eveniet consequatur voluptatibus maxime. Voluptates
      aut illo est recusandae omnis. Sit iure sunt et provident ut. Velit
      quibusdam repellendus sed fugiat et.
    </p>
    <p>Possimus molestiae quaerat incidunt sapiente ipsam.</p>
    <p>
      Autem quis quia libero expedita accusantium. Tempora quia qui voluptatem
      inventore repellendus. Sit officia aliquam laudantium similique delectus.
      Non eos rerum quisquam voluptas ullam. Placeat dolores facere laborum non
      fugit. Est neque earum eum aut non. Quia voluptas rerum quia perspiciatis
      harum. Voluptatem et mollitia repellendus sed dicta.
    </p>
    <p>
      Eos rerum harum hic sunt omnis. Est veniam est aut ex consequuntur. Est
      qui inventore assumenda enim perferendis. Libero tenetur nisi quasi
      recusandae rerum. Consectetur iste distinctio id accusamus quo. Enim
      voluptatum rerum voluptas rem harum.
    </p>
    <p>
      Consectetur eveniet ex quaerat velit et. Sint nesciunt temporibus minus
      deserunt perferendis. Molestiae modi nulla ratione ea sapiente. Laboriosam
      sed voluptatibus quo tenetur repudiandae. Accusamus culpa ipsa ab nihil
      suscipit. Mollitia veritatis id consequatur dolorem rerum. Ratione atque
      quia illum sunt repudiandae. Voluptate qui temporibus laborum nobis
      officia. Voluptate unde sed culpa dolorem minus.
    </p>
  </div>
</div>
```

This is easy to debug, easy to create, and still makes use of middlewares like compression. It's just HTML fragments but wrapped in a protocol that allows for streaming and server driven events.
