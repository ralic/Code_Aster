# coding=utf-8

"""
Cata package
------------

This package give access to the catalogs of commands.

It works as a switch between the legacy supervisor and the next generation
of the commands language (already used by AsterStudy).

"""

try:
    from common.session import AsterStudySession
    HAVE_ASTERSTUDY = AsterStudySession.use_cata()
except ImportError:
    HAVE_ASTERSTUDY = False


if not HAVE_ASTERSTUDY:
    # use legacy supervisor of commands
    from .Legacy import cata
