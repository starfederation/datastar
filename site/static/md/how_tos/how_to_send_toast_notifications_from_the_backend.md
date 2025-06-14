# How to send Toast Notifications from the backend

## Intro

A common usecase in modern web applications is to display toast notifications somewhere on the page to alert the user that an action they did worked,
or not. With Datastar, we can easily push the notifcation directly to our application without having to return some status code and then letting the
application react to those. Since we pilot our application from the backend, we can then notify the user using that same backend.

## Goal

Send a toast notification from the backend and have it appear on the page.

## Demo

<button data-on-click="@post('/how_tos/toast/data')" class="btn btn-primary font-bold">Cook!</button>
<ul id="toaster" style="position: absolute; right: 1em; bottom: 1em;"></ul>

## Steps

## Conclusion
