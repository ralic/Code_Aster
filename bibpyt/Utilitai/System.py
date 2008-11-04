#@ MODIF System Utilitai  DATE 03/11/2008   AUTEUR PELLET J.PELLET 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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

"""Ce module définit la classe `SYSTEM` et la fonction `ExecCommand`
qui est présente uniquement pour commodité pour les Macros.

La classe SYSTEM est semblable à celle utilisée dans ASTK_SERV.
"""

__all__ = ["SYSTEM", "ExecCommand"]

import sys
import os
import time
import popen2
import re
from sets import Set
from types import FileType

try:
   import threading as _threading
except ImportError:
   import dummy_threading as _threading

# ----- differ messages translation
def _(mesg):
   return mesg

#-------------------------------------------------------------------------------
class NonBlockingReader(_threading.Thread):
   """Classe pour lire l'output/error d'un process fils sans bloquer."""
   def __init__(self, process, file, bufsize=1000, sleep=0.1):
      _threading.Thread.__init__(self)
      self.process = process
      self.file    = file
      self.bufsize = bufsize
      self.lock    = _threading.Lock()
      self.content = []
      self.buffer  = []
      self.sleeptime = sleep
      self.ended   = False

   def fill_buffer(self, bufsize=-1):
      #add = os.read(self.file.fileno(), self.bufsize)
      add = self.file.read(bufsize)
      if add:
         self.lock.acquire()
         self.buffer.append(add)
         self.lock.release()

   def run(self):
      while self.process.poll() == -1:
         self.fill_buffer(self.bufsize)
         time.sleep(self.sleeptime)
      self.fill_buffer()
      self.ended = True

   def flush(self):
      if len(self.buffer) > 0:
         self.content.extend(self.buffer)
         self.buffer = []

   def getcurrent(self):
      time.sleep(self.sleeptime)
      if self.ended:
         self.fill_buffer()
      self.lock.acquire()
      txt = ''.join(self.buffer)
      self.flush()
      self.lock.release()
      return txt

   def read(self):
      self.fill_buffer()
      self.lock.acquire()
      self.flush()
      txt = ''.join(self.content)
      self.lock.release()
      return txt

#-------------------------------------------------------------------------------
def _exitcode(status, default=0):
   """Extrait le code retour du status. Retourne `default` si le process
   n'a pas fini par exit.
   """
   if os.WIFEXITED(status):
      iret = os.WEXITSTATUS(status)
   elif os.WIFSIGNALED(status):
      iret = os.WTERMSIG(status)
   elif os.WIFSTOPPED(status):
      iret = os.WSTOPSIG(status)
   else:
      iret = default
   return iret

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
class SYSTEM:
   """Class to encapsultate "system" commands (this a simplified version of
   ASTER_SYSTEM class defined in ASTK_SERV part).
   """
   # this value should be set during installation step.
   MaxCmdLen = 1024
   # line length -9
   _LineLen = 80-9
   
#-------------------------------------------------------------------------------
   def __init__(self, **kargs):
      """Initialization.
      Optionnal arguments : silent, verbose, debug, cc_files, maxcmdlen.
      """
      self.verbose   = kargs.get('verbose', True)
      self.debug     = kargs.get('debug', False)
      self.cc_files  = kargs.get('cc_files', None)
      if kargs.has_key('maxcmdlen'):
         self.MaxCmdLen = kargs['maxcmdlen']

#-------------------------------------------------------------------------------
   def _mess(self, msg, cod=''):
      """Just print a message
      """
      self._print('%-18s %s' % (cod, msg))

#-------------------------------------------------------------------------------
   def _print(self, *args, **kargs):
      """print replacement.
      Optionnal argument :
         term  : line terminator (default to os.linesep).
      """
      term = kargs.get('term', os.linesep)
      files = Set([sys.stdout])
      if self.cc_files:
         files.add(self.cc_files)
      for f in files:
         if type(f) is FileType:
            txt = ' '.join(['%s'%a for a in args])
            f.write(txt.replace(os.linesep+' ', os.linesep)+term)
            f.flush()
         else:
            print _('FileType object expected : %s / %s') % (type(f), repr(f))

#-------------------------------------------------------------------------------
   def VerbStart(self, cmd, verbose=None):
      """Start message in verbose mode
      """
      Lm = self._LineLen
      if verbose == None:
         verbose = self.verbose
      if verbose:
         pcmd = cmd
         if len(cmd) > Lm-2 or cmd.count('\n') > 0:
            pcmd = pcmd+'\n'+' '*Lm
         self._print(('%-'+str(Lm)+'s') % (pcmd,), term='')

#-------------------------------------------------------------------------------
   def VerbEnd(self, iret, output='', verbose=None):
      """Ends message in verbose mode
      """
      if verbose == None:
         verbose = self.verbose
      if verbose:
         if iret == 0:
            self._print('[  OK  ]')
         else:
            self._print(_('[FAILED]'))
            self._print(_('Exit code : %d') % iret)
         if (iret != 0 or self.debug) and output:
            self._print(output)

#-------------------------------------------------------------------------------
   def VerbIgnore(self, verbose=None):
      """Ends message in verbose mode
      """
      if verbose == None:
         verbose = self.verbose
      if verbose:
         self._print(_('[ SKIP ]'))

#-------------------------------------------------------------------------------
   def Shell(self, cmd, bg=False, verbose=None, follow_output=False,
             alt_comment=None, interact=False,
             capturestderr=True, separated_stderr=False):
      """Execute a command shell
         cmd      : command
         bg       : put command in background if True
         verbose  : print status messages during execution if True
         follow_output : follow interactively output of command
         alt_comment : print this "alternative comment" instead of "cmd"
         interact : allow the user to interact with the process
            (don't close stdin). bg=True implies interact=False.
      Return :
         iret     : exit code if bg = False,
                    process id if bg = True
         output   : output lines (as string)
      """
      if not alt_comment:
         alt_comment = cmd
      if not capturestderr:
         separated_stderr = False
      if verbose == None:
         verbose = self.verbose
      if bg:
         interact = False
      if len(cmd) > self.MaxCmdLen:
         self._mess((_('length of command shell greater '\
               'than %d characters.') % self.MaxCmdLen), _('<A>_ALARM'))
      if self.debug:
         self._print('<DBG> <local_shell>', cmd)
         self._print('<DBG> <local_shell> background mode : ', bg)
      # exec
      self.VerbStart(alt_comment, verbose=verbose)
      if follow_output and verbose:
         self._print(_('\nCommand output :'))
      # run interactive command
      if interact:
         iret = os.system(cmd)
         return _exitcode(iret), ''
      # use popen to manipulate stdout/stderr
      output = ''
      error  = ''
      if separated_stderr or not capturestderr:
         p = popen2.Popen3(cmd, capturestderr=capturestderr)
      else:
         p = popen2.Popen4(cmd)
      p.tochild.close()
      if not bg:
         th_out = NonBlockingReader(p, p.fromchild)
         th_out.start()
         if separated_stderr:
            th_err = NonBlockingReader(p, p.childerr)
            th_err.start()
         if follow_output:
            while p.poll() == -1:
               new = th_out.getcurrent()
               if new:
                  # \n already here...
                  self._print(new, term='')
            # to be sure to empty the buffer
            new = th_out.getcurrent()
            self._print(new)
         th_out.join()
         if separated_stderr:
            th_err.join()
         # store all output/error
         output = th_out.read()
         if separated_stderr:
            error  = th_err.read()
         try:
            iret = _exitcode(p.wait())
         except OSError, e:
            self._print("OSError = %s" % str(e))
            iret = 4
      else:
         iret = 0
      p.fromchild.close()
      if separated_stderr:
         p.childerr.close()

      # repeat header message
      if follow_output:
         self.VerbStart(alt_comment, verbose=verbose)
      mat = re.search('EXIT_CODE=([0-9]+)', output)
      if mat:
         iret = int(mat.group(1))
      self.VerbEnd(iret, output, verbose=verbose)
      if bg:
         iret = p.pid
         if verbose:
            self._print(_('Process ID : '), iret)
      if separated_stderr:
         return iret, output, error
      return iret, output

#-------------------------------------------------------------------------------
# Juste par commodité.
system = SYSTEM()
ExecCommand = system.Shell


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
if __name__ == '__main__':
   iret, output = ExecCommand('ls', alt_comment='Lancement de la commande...')

