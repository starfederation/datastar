"""
WSGI config for datastar project.

It exposes the WSGI callable as a module-level variable named ``application``.

For more information on this file, see
https://docs.djangoproject.com/en/4.2/howto/deployment/wsgi/
"""


import os
import wsgiref.handlers

from django.conf import settings
from django.core.wsgi import get_wsgi_application

os.environ.setdefault("DJANGO_SETTINGS_MODULE", "datastar.settings")


if settings.DEBUG:
    original_is_hop_by_hop = wsgiref.handlers.is_hop_by_hop


    def custom_is_hop_by_hop(header_name):
        """Permit the "connection" header over WSGI

        Datastar by default, sets the connection header to "keep-alive".
        Per the WSGI spec (see below), wsgiref does not permit setting
        "hop-by-hop" headers like connection. This function works around
        that limitation.

        https://peps.python.org/pep-0333/#other-http-features
        """
        if header_name.casefold() == "connection":            
            return False

        return original_is_hop_by_hop(header_name)


    wsgiref.handlers.is_hop_by_hop = custom_is_hop_by_hop


application = get_wsgi_application()
