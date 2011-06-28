#@ MODIF N_info Noyau  DATE 28/06/2011   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
# THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY  
# IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY  
# THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR     
# (AT YOUR OPTION) ANY LATER VERSION.                                                  
#                                                                       
# THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT   
# WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF            
# MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU      
# GENERAL PUBLIC LICENSE FOR MORE DETAILS.                              
#                                                                       
# YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE     
# ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,         
#    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.        
# ======================================================================
# RESPONSABLE COURTOIS M.COURTOIS

"""Module to manage information printing : debug, info, error.
Should replace 'print' and 'UTMESS' calls at least in the supervisor
modules.
Only used for debug right now.
"""

import os
import os.path as osp
import re
import traceback
from functools import partial
from subprocess import Popen, PIPE

from N_utils import Enum

def default_print(text):
    """Basic print function."""
    print text

LEVEL = Enum(
    'DEBUG',
    'INFO',
    'WARN',
    'ERROR'
)

class Category(object):
    """Define a category of message for different parts of the code.
    This allows to store different parameters for each category of message."""
    def __init__(self):
        self._level = LEVEL.INFO
        self._fmt = "%-8s"
        self._header = {
            LEVEL.DEBUG : "DEBUG",
            LEVEL.INFO : None,
            LEVEL.WARN : "WARNING",
            LEVEL.ERROR : "ERROR",
        }

    def set_level(self, level):
        """Set the current level."""
        self._level = level

    def get_level(self):
        """Return the current level."""
        return self._level

    def set_header(self, level, header):
        """Set the header of ``level`` messages."""
        self._header[level] = header

    def get_header(self, level):
        """Return the header at this ``level``."""
        header = self._header.get(level, "")
        if header:
            header = self._fmt % header
        return header

    def active(self, level):
        """Tell if a message should be print at this ``level``."""
        return self._level <= level


ALL = Category()
SUPERV = Category()
SUPERV.set_header(LEVEL.ERROR, None)
MISS = Category()

REGEXP_ORIG = re.compile('File [\'\"]*(.*?)[\'\"]*, *line ([0-9]+), *in (.*)')

# slighty different and very simplier than logger objects
# from the logging module.
class InfoLevel(object):
    """Store informations level."""
    def __init__(self, level):
        """Initialization"""
        self._parts = []
        for part in self._parts:
            part.level = level
        self.reset_print_function()
        self._msg_callback = []
        #self.extend_message(ALL, stack_header_callback)
        self.extend_message(ALL, insert_header)

    def add(self, category):
        """Add a category of message."""
        self._parts.append(category)

    def set_level(self, category, level):
        """Set the current level for ``category``."""
        assert category in self._parts, "unknown category : %s" % category
        assert LEVEL.exists(level), "unknown level : %s" % level
        category.set_level(level)
        if category == ALL:
            for part in self._parts:
                part.set_level(level)

    def set_debug(self):
        """Set debug level for all categories."""
        self.set_level(ALL, LEVEL.DEBUG)

    def set_header(self, category, level, header):
        """Set the header of ``level`` messages."""
        category.set_header(level, header)

    def register_print_function(self, print_function):
        """Define the `print_function` to use."""
        self._print = print_function

    def reset_print_function(self):
        """Register the default 'print function'."""
        self._print = default_print

    def extend_message(self, category, callback):
        """Allow to extend the message calling an external function."""
        self._msg_callback.append((category, callback))

    def _message(self, category, level, msg, args, kwargs):
        """Print the message if the level is reached."""
        if category.active(level):
            if kwargs.get('utmess'):
                func = self._message_utmess
            else:
                func = self._message_print
            func = self._message_print
            apply(func, (category, level, msg, args, kwargs))

    def _message_print(self, category, level, msg, args, kwargs):
        """Print the message if the level is reached."""
        for cat, cbk in self._msg_callback:
            if cat in (ALL, category):
                msg, args = cbk(category, level, msg, args, kwargs)
        if len(args) > 0:
            try:
                msg = msg % args
            except Exception, err:
                msg = repr((msg, args, err))
        self._print(msg)

    def _message_utmess(self, category, level, msg, args, kwargs):
        """Print the message if the level is reached."""
        # how to use callbacks ? valk ?
        from Utilitai.Utmess import MessageLog
        code = {
            LEVEL.DEBUG : 'I',
            LEVEL.INFO : 'I',
            LEVEL.WARN : 'A',
            LEVEL.ERROR : 'F',
        }
        valk = kwargs.get('valk', ())
        vali = kwargs.get('vali', ())
        valr = kwargs.get('valr', ())
        msg = MessageLog.GetText(code[level], msg, valk, vali, valr)
        for cat, cbk in self._msg_callback:
            if cat in (ALL, category):
                msg, args = cbk(category, level, msg, args, kwargs)
        self._print(msg)

    def debug(self, category, msg, *args, **kwargs):
        """Print a debug message."""
        self._message(category or ALL, LEVEL.DEBUG, msg, args, kwargs)

    def info(self, category, msg, *args, **kwargs):
        """Print an information message."""
        self._message(category or ALL, LEVEL.INFO, msg, args, kwargs)

    def warn(self, category, msg, *args, **kwargs):
        """Print a warning message."""
        self._message(category or ALL, LEVEL.WARN, msg, args, kwargs)

    def error(self, category, msg, *args, **kwargs):
        """Print an error message."""
        self._message(category or ALL, LEVEL.ERROR, msg, args, kwargs)

    critical = error

    def add_memory_info(self, category):
        """Shortcut to add memory informations."""
        self.extend_message(category, mem_msg_callback)


# defined extensions
def insert_header(category, level, msg, args, kwargs):
    """Insert the header."""
    header = category.get_header(level)
    if header:
        msg = header + msg
    return msg, args

def stack_header_callback(category, level, msg, args, kwargs):
    """To insert the origin."""
    if level <= LEVEL.DEBUG:
        stack_id = -5 + kwargs.get('stack_id', 0)
        stack = traceback.format_stack(limit=10)[stack_id]
        mat = REGEXP_ORIG.search(stack)
        origin = '[%s:%s in %s] ' % (osp.basename(mat.group(1)), mat.group(2), mat.group(3))
        msg = origin + msg
    return msg, args


# objet singleton
message = InfoLevel(LEVEL.INFO)
message.add(ALL)
message.add(SUPERV)
message.add(MISS)

# callback to add memory information
_pid = os.getpid()

RE_VMPEAK = re.compile('VmPeak:\s*([0-9]+)\s*([kMGBo]+)', re.M | re.I)

def memory_used(pid):
    """Return the current VmPeak value."""
    p = Popen(['cat', '/proc/%s/status' % pid], stdout=PIPE)
    output = p.communicate()[0]
    mat = RE_VMPEAK.search(output)
    return int(mat.group(1)) / 1024.

current_memory_used = partial(memory_used, _pid)

def mem_msg_callback(category, level, msg, args, kwargs):
    """Callback to add memory infos to message."""
    if level <= LEVEL.DEBUG:
        msg = msg + " - VmPeak : %.2f Mo"
        args = tuple(list(args) + [current_memory_used(), ])
    return msg, args


if __name__ == "__main__":
    message.set_level(SUPERV, LEVEL.WARN)
    message.set_level(MISS, LEVEL.DEBUG)
    message.debug(None, "debug message")
    message.info(ALL, "information message")
    message.warn(None, "warning message")
    message.error(ALL, "error message")
    message.add_memory_info()
    message.debug(MISS, "debug supervisor message")
    message.info(SUPERV, "information supervisor message")
    message.warn(SUPERV, "warning supervisor message")
    message.error(SUPERV, "error supervisor message")
    message.critical(MISS, "test the critical alias")


