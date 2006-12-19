#@ MODIF utprin Messages  DATE 18/12/2006   AUTEUR COURTOIS M.COURTOIS 
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

import os
import sys
import traceback
import imp
import re

def _(s):
   return s

# -----------------------------------------------------------------------------
contacter_assistance = """
Il y a probablement une erreur dans la programmation.
Veuillez contacter votre assistance technique."""

# -----------------------------------------------------------------------------
def utprin(typmess,unite,idmess,valk,vali,valr):
   """
      Cette methode permet d'imprimer un message venu d'U2MESG
   """
   import aster
   from Utilitai.Utmess import UTMESS

   # en cas d'erreur, si on lève une exception au lieu de s'arreter
   # on n'affiche pas le type de l'erreur pour ne pas fausser le diagnostic
   typmess = typmess.strip()
   if aster.onFatalError() == 'EXCEPTION':
      if typmess in ('E', 'F'):
         typmess = 'EXCEPTION'
   # dans tous les cas, pour S et Z (exception), on affiche EXCEPTION.
   if typmess in ('Z', 'S'):
      typmess = 'EXCEPTION'
   
   unite   = unite.strip()
   idmess  = idmess.strip()
   valk    = [k.strip() for k in valk]

   # on décode idmess => catamess, numess :
   x = idmess.split("_")
   assert len(x) == 2, idmess
   catamess=x[0].lower()
   numess = int(x[1])
   assert numess > 0 and numess < 100, idmess

   # on importe catamess => cata_msg :
   pkg = 'Messages'
   try:
      argp = imp.find_module(pkg)
      pack = imp.load_module(pkg, *argp)
      args = imp.find_module(catamess, pack.__path__)
      mod  = imp.load_module(catamess, *args)
      cata_msg = mod.cata_msg
   except ImportError, msg:
      UTMESS('F', 'utprin', """Impossible d'importer %(catamess)s dans Messages.
Le fichier %(catamess)s.py n'existe pas dans le répertoire 'Messages'.""" \
            % { 'catamess' : catamess })
   else:
      args[0].close()

   # on prépare le dictionnaire des arguments (dicarg) :
   dicarg = {}
   # initialisation des 10 premiers
   for i in range(1,11):
      dicarg['i%d' % i] = 99999999
      dicarg['r%d' % i] = 9.9999E99
      dicarg['k%d' % i] = 'xxxxxx'
   # arguments
   for i in range(1,len(valk)+1):
      dicarg['k%d' % i] = valk[i-1]
   for i in range(1,len(vali)+1):
      dicarg['i%d' % i] = vali[i-1]
   for i in range(1,len(valr)+1):
      dicarg['r%d' % i] = valr[i-1]
   # valeur spéciale : ktout
   dicarg['ktout'] = ' '.join(valk)

   # on imprime le message :
   try:
      dictmess = {
         'type_message'  : typmess,
         'id_message'    : '<%s>' % idmess,
         'corps_message' : cata_msg[numess] % dicarg,
      }
      if typmess == 'I':
         dictmess['id_message'] = ''
   except Exception, msg:
      dictmess = {
         'type_message'  : typmess,
         'id_message'    : '',
         'corps_message' : """Erreur de programmation.
Le message %s n'a pas pu etre formatté correctement.
--------------------------------------------------------------------------
%s
--------------------------------------------------------------------------

%s""" \
   % (idmess, ''.join(traceback.format_tb(sys.exc_traceback)), contacter_assistance),
      }
   
   txt = clean_string(format_message(dictmess))
   aster.affiche(unite, txt)

   return None


# -----------------------------------------------------------------------------
def format_message(dictmess):
   """Formate le message décrit dans un dico :
      'type_message'  : A, E, S, F, I
      'id_message'    : identification du message
      'corps_message' : texte
   """
   charh = '-'
   charv = '!'
   charc = '!'
   dcomm = {
      'A' : _("""Ceci est une alarme. Si vous ne comprenez pas le sens de cette
alarme, vous pouvez obtenir des résultats inattendus !"""),
      'E' : _("""Cette erreur sera suivie d'une erreur fatale."""),
      'S' : _("""Cette erreur est fatale. Le code s'arrete. Toutes les étapes
du calcul ont été sauvées dans la base jusqu'au moment de l'arret."""),
      'F' : _("""Cette erreur est fatale. Le code s'arrete."""),
   }
   
   # format complet
   format_general = {
      'decal'  : '   ',
      'header' : """<%(type_message)s> %(id_message)s""",
      'ligne'  : '%(charv)s %%-%(maxlen)ds %(charv)s',
      'corps'  : """%(header)s

%(corps_message)s

%(commentaire)s
""",
      'final'  : """
%(separateur)s
%(corps)s
%(separateur)s

""",
   }
   # format light pour les infos
   format_light = {
      'decal'  : '',
      'header' : """<%(type_message)s> """,
      'ligne'  : '%%s',
      'corps'  : """%(corps_message)s
""",
      'final'  : """%(corps)s""",
   }
   dmsg = dictmess.copy()
   
   # format utilisé
   format = format_general
   if dmsg['type_message'] == 'I':
      format = format_light
   
   dmsg['header']      = format['header'] % dmsg
   dmsg['commentaire'] = dcomm.get(dmsg['type_message'], '')
   if re.search('^DVP', dmsg['id_message']) != None:
      dmsg['commentaire'] += contacter_assistance
   
   dmsg['corps']       = format['corps'] % dmsg
   
   # longueur de la ligne la plus longue
   l_line = dmsg['corps'].splitlines()
   maxlen = max([len(line) for line in l_line])
   
   # format des lignes sur maxlen caractères
   dlin = {
      'charh'  : charh,
      'charv'  : charv,
      'charc'  : charc,
      'maxlen' : maxlen
   }
   fmt_line = format['ligne'] % dlin
   
   # on formate toutes les lignes
   txt = [fmt_line % line for line in l_line]
   dmsg['corps'] = os.linesep.join(txt)
   dmsg['separateur'] = charc + charh * (maxlen + 2) + charc
   
   # ligne haut et bas
   newtxt = format['final'] % dmsg
   # on décale
   l_txt = [format['decal'] + line for line in newtxt.splitlines()]
   
   return os.linesep.join(l_txt)


# -----------------------------------------------------------------------------
def clean_string(chaine):
   """Supprime tous les caractères non imprimables.
   """
   invalid = '?'
   txt = []
   for c in chaine:
      if ord(c) != 0:
         txt.append(c)
      else:
         txt.append(invalid)
   return ''.join(txt)


# -----------------------------------------------------------------------------
if __name__ == '__main__':
   dtest = {
      'type_message'  : 'A',
      'id_message'    : 'DVP_00',
      'corps_message' : """
Ceci est le texte d'un message bidon.
Ligne 2...
""",
   }
   print format_message(dtest)
