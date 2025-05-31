# Datastar Django

ASGI and WSGI examples of Datastar and Django in action.


To take full advantage of Datastar, we recommend (but do not require) that you use ASGI, especially on green field projects to take advantage of the full range of Datastar's capabilities. That said, Datastar and Django will work perfectly well over WSGI.

## ASGI (default)

Install the dependencies, start runserver as usual, and go to http://127.0.0.1:8000/ to see the demo.

💡️ Note: Because Django's `runserver` is WSGI only, we use [Daphne's](https://github.com/django/daphne) built-in ASGI `runserver`.

## WSGI

While, Datastar works great in a WSGI context, we recommend you give ASGI a try if possible (see top of this page for more details).

To use WSGI make sure runserver is stopped and make the following changes to the settings file in the `datastar` project directory:

1. Uncomment the `WSGI_APP` setting, and comment out the `ASGI_APP` setting.

2. In `INSTALLED_APPS` comment out `"daphne"`,

3. Start runserver and go to http://127.0.0.1:8000/wsgi/ and watch the demo.

   ```
   uv run manage.py runserver
   ```
