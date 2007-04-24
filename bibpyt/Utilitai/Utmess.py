#@ MODIF Utmess Utilitai  DATE 24/04/2007   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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

import os
import sys
import traceback
import imp
import re

from Messages.context_info import message_context_concept

try:
   import aster
except:
   pass

def _(s):
   return s

# -----------------------------------------------------------------------------
contacter_assistance = """
Il y a probablement une erreur dans la programmation.
Veuillez contacter votre assistance technique."""

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
class MESSAGE_LOGGER:
   """Classe gérant l'impression de messages.
   On ne crée qu'une instance de ce type.
   Cette instance est accessible via le module E_Global pour astermodule.c
   """
   def __init__(self):
      """Initialisation
      """
      self.init_buffer()
      
      # est-ce qu'une erreur <E> s'est produite
      self.erreur_E = False
      
      # compteur des alarmes émises { 'id_alarm' : count }
      self.count_alarm = {}
      
      # on prépare le dictionnaire des valeurs par défaut des arguments (dicarg) :
      self.default_args = {}
      # initialisation des 10 premiers
      for i in range(1,11):
         self.default_args['i%d' % i] = 99999999
         self.default_args['r%d' % i] = 9.9999E99
         self.default_args['k%d' % i] = 'xxxxxx'

# -----------------------------------------------------------------------------
   def __call__(self, *args, **kwargs):
      """Raccourci pour simplifier l'appel depuis astermodule.c et UTMESS.
      """
      self.print_message(*args, **kwargs)

# -----------------------------------------------------------------------------
   def print_message(self, code, idmess, valk=(), vali=(), valr=()):
      """Appelé par la routine fortran U2MESG ou à la fonction python UTMESS
      pour afficher un message.
      L'impression de ce message est différée si le `code` est suivi d'un "+".
         code  : 'A', 'E', 'S', 'F', 'I'
         idmess : identificateur du message
         valk, vali, valr : liste des chaines, entiers ou réels.
      """
      # récupération du texte du message
      dictmess = self.get_message(code, idmess, valk, vali, valr)
      
      # on le met dans le buffer
      self.add_to_buffer(dictmess)
      
      # si on n'attend pas une suite, ...
      if len(code) < 2 or code[1] != '+':
         # mise à jour des compteurs
         self.update_counter()
         
         # on imprime le message en attente
         self.print_buffer_content()

      return None

# -----------------------------------------------------------------------------
   def build_dict_args(self, valk, vali, valr):
      """Construit le dictionnaire de formattage du message.
      """
      # homogénéisation : uniquement des tuples + strip des chaines de caractères
      valk, vali, valr = map(force_enum, (valk, vali, valr))
      valk    = [k.strip() for k in valk]
      
      # variables passées au message
      dicarg = self.default_args.copy()
      for i in range(1,len(valk)+1):
         dicarg['k%d' % i] = valk[i-1]
      for i in range(1,len(vali)+1):
         dicarg['i%d' % i] = vali[i-1]
      for i in range(1,len(valr)+1):
         dicarg['r%d' % i] = valr[i-1]
      # valeur spéciale : ktout = concaténation de toutes les chaines
      dicarg['ktout'] = ' '.join(valk)
   
      return dicarg

# -----------------------------------------------------------------------------
   def get_message(self, code, idmess, valk=(), vali=(), valr=()):
      """Retourne le texte du message dans un dictionnaire dont les clés sont :
         'code', 'id_message', 'corps_message'
      """
      dicarg = self.build_dict_args(valk, vali, valr)
   
      # décodage : idmess => (catamess, numess)
      idmess  = idmess.strip()
      x = idmess.split("_")
      assert len(x) > 1, idmess
      catamess='_'.join(x[0:-1]).lower()
      numess = int(x[-1])
      assert numess > 0 and numess < 100, idmess
   
      # import catamess => cata_msg
      try:
         mod = __import__('Messages.%s' % catamess, globals(), locals(), [catamess])
         # si le dictionnaire n'existe pas, on alertera au moment du formatage.
         cata_msg = getattr(mod, 'cata_msg', {})
      except Exception, msg:
         # doit permettre d'éviter la récursivité
         if catamess != 'supervis':
            self.print_message('A', 'SUPERVIS_57', valk=(catamess, str(msg)))
         cata_msg = {}
      
      # corps du message
      try:
         # cata_msg[num] = 'format'
         #              ou {'message' : 'format', 'context' : 'éléments de contexte'}
         if type(cata_msg[numess]) == dict:
            fmt_msg  = cata_msg[numess]['message']
            ctxt_msg = cata_msg[numess]['context']
         else:
            fmt_msg  = cata_msg[numess]
            ctxt_msg = None
         
         dictmess = {
            'code'          : code,
            'id_message'    : idmess,
            'corps_message' : fmt_msg % dicarg,
            'context_info'  : self.get_context(ctxt_msg, idmess, dicarg),
         }
         if code == 'I':
            dictmess['id_message'] = ''
      except Exception, msg:
         dictmess = {
            'code'          : code,
            'id_message'    : '',
            'corps_message' : """Erreur de programmation.
Le message %s n'a pas pu etre formaté correctement.
--------------------------------------------------------------------------
%s
--------------------------------------------------------------------------

%s""" \
      % (idmess,
         ''.join(traceback.format_tb(sys.exc_traceback)), contacter_assistance),
            'context_info'  : '',
         }
      # limite la longueur des ligness
      dictmess['corps_message'] = cut_long_lines(dictmess['corps_message'], 80)
      return dictmess

# -----------------------------------------------------------------------------
   def init_buffer(self):
      """Initialise le buffer.
      """
      self._buffer = []

# -----------------------------------------------------------------------------
   def add_to_buffer(self, dictmess):
      """Ajoute le message décrit dans le buffer en vue d'une impression
      ultérieure.
      """
      self._buffer.append(dictmess)

# -----------------------------------------------------------------------------
   def get_current_code(self):
      """Retourne le code du message du buffer = code du message le plus grave
      (cf. dgrav)
      """
      dgrav = { '?' : -9, 'I' : 0, 'A' : 1, 'S' : 4, 'Z' : 4, 'E' : 6, 'F' : 10 }
      
      current = '?'
      for dictmess in self._buffer:
         code = dictmess['code'][0]
         if dgrav.get(code, -9) > dgrav.get(current, -9):
            current = code
      
      return current

# -----------------------------------------------------------------------------
   def get_current_id(self):
      """Retourne l'id du message du buffer = id du premier message
      """
      return self._buffer[0]['id_message']

# -----------------------------------------------------------------------------
   def print_buffer_content(self):
      """Extrait l'ensemble des messages du buffer dans un dictionnaire unique,
      imprime le message, et vide le buffer pour le message suivant.
         - code : celui du message le plus grave (cf. dgrav)
         - id   : celui du premier message qui est affiché
         - corps : concaténation de tous les messages.
      """
      if len(self._buffer) < 1:
         return None
      
      # construction du dictionnaire du message global
      dglob = {
         'code'          : self.get_current_code(),
         'id_message'    : self.get_current_id(),
         'liste_message' : [],
         'liste_context' : [],
      }
      for dictmess in self._buffer:
         dglob['liste_message'].append(dictmess['corps_message'])
         dglob['liste_context'].append(dictmess['context_info'])
      dglob['corps_message'] = ''.join(dglob['liste_message'])
      dglob['context_info'] = ''.join(dglob['liste_context'])
      
      # liste des unités d'impression en fonction du type de message
      l_unit = self.list_unit(dglob['code'])
      
      # texte final et impression
      txt = self.format_message(dglob)
      for unite in l_unit:
         aster.affiche(unite, txt)
      
      self.init_buffer()

# -----------------------------------------------------------------------------
   def update_counter(self):
      """Mise à jour des compteurs et réaction si besoin.
      Retourne un flag permettant de savoir si on doit afficher le nouveau
      message (skip_next=False) demandé ou bien l'ignorer (skip_next=True).
      """
      nmax_alarm = 5
      code = self.get_current_code()
      if code == 'E':
         self.erreur_E = True
      elif code == 'A':
         idmess = self.get_current_id()
         # nombre d'occurence de cette alarme
         self.count_alarm[idmess] = self.count_alarm.get(idmess, 0) + 1
         if self.count_alarm[idmess] == nmax_alarm:
            # Pour mettre en relief le message SUPERVIS_41, on le sépare
            # de la dernière alarme
            self.print_buffer_content()
            dictmess = self.get_message(code, 'SUPERVIS_41',
                                        valk=idmess, vali=nmax_alarm)
            self.add_to_buffer(dictmess)
         elif self.count_alarm[idmess] > nmax_alarm:
            # count_alarm > 5, on vide le buffer
            self.init_buffer()

# -----------------------------------------------------------------------------
   def check_counter(self):
      """Méthode "jusqu'ici tout va bien" !
      Si des erreurs <E> se sont produites, on arrete le code en <F>.
      Appelée par FIN ou directement au cours de l'exécution d'une commande.
      Retourne un entier : 0 si tout est ok (toujours pour le moment)
      """
      iret = 0
      if self.erreur_E:
         self.print_message('F', 'SUPERVIS_6')
      return iret

# -----------------------------------------------------------------------------
   def format_message(self, dictmess):
      """Formate le message décrit dans un dico :
         'code'          : A, E, S, F, I
         'id_message'    : identification du message
         'corps_message' : texte
      """
      charh = '-'    # horizontal
      charv = '!'    # vertical
      charc = '!'    # coin
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
         'header' : """<%(type_message)s> %(str_id_message)s""",
         'ligne'  : '%(charv)s %%-%(maxlen)ds %(charv)s',
         'corps'  : """%(header)s

%(corps_message)s
%(context_info)s

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
%(context_info)s""",
         'final'  : """%(corps)s""",
      }
      dmsg = dictmess.copy()
      dmsg['type_message'] = self.get_type_message(dictmess['code'])
      if dmsg['id_message'] != '':
         dmsg['str_id_message'] = '<%s>' % dmsg['id_message']
      else:
         dmsg['str_id_message'] = ''
      
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
      
      return clean_string(os.linesep.join(l_txt))

# -----------------------------------------------------------------------------
   def list_unit(self, code):
      """Retourne la liste des noms de fichiers (logiques) sur lesquels doit
      etre imprimé le message.
      """
      #IDF  = INDEX('EFIDASXZ', ...)
      #'Z' (IDF=8) = LEVEE D'EXCEPTION
      d = {
         'E' : ('ERREUR', 'MESSAGE', 'RESULTAT'),
         'I' : ('MESSAGE',),
         'A' : ('MESSAGE', 'RESULTAT'),
      }
      d['F'] = d['S'] = d['Z'] = d['E']
      d['X'] = d['A']
      return d.get(code, d['F'])

# -----------------------------------------------------------------------------
   def get_type_message(self, code):
      """Retourne le type du message affiché.
      En cas d'erreur, si on lève une exception au lieu de s'arreter,
      on n'affiche pas le type de l'erreur pour ne pas fausser le diagnostic
      """
      typmess = code.strip()
      if aster.onFatalError() == 'EXCEPTION':
         if typmess in ('E', 'F'):
            typmess = 'EXCEPTION'
      # dans tous les cas, pour S et Z (exception), on affiche EXCEPTION.
      if typmess in ('Z', 'S'):
         typmess = 'EXCEPTION'
      return typmess

# -----------------------------------------------------------------------------
   def get_context(self, ctxt_msg, idmess, dicarg):
      """Prise en compte du context du message pour donner d'autres infos
      à l'utilisateur.
      ctxt_msg est un dictionnaire. Les clés traitées sont :
         - CONCEPT
      """
      if not ctxt_msg:
         return ''
      msg = []
      # tout dans un try/except car c'est du bonus, il ne faudrait pas planter !
      try:
         if ctxt_msg.has_key('CONCEPT'):
            l_co =  [dicarg[arg] for arg in force_enum(ctxt_msg['CONCEPT'])]
            for co in l_co:
               msg.append(message_context_concept(co))
      except:
         pass
      return os.linesep.join(msg)


# -----------------------------------------------------------------------------
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
def force_enum(obj):
   """Retourne `obj` si c'est une liste ou un tuple,
   sinon retourne [obj,]
   """
   if type(obj) not in (list, tuple):
      obj = [obj,]
   return obj

# -----------------------------------------------------------------------------
def maximize_lines(l_fields, maxlen, sep):
   """Construit des lignes dont la longueur est au plus de `maxlen` caractères.
   Les champs sont assemblés avec le séparateur `sep`.
   """
   newlines = []
   while len(l_fields) > 0:
      cur = []
      while len(l_fields) > 0 and len(sep.join(cur + [l_fields[0],])) <= maxlen:
         cur.append(l_fields.pop(0))
      newlines.append(sep.join(cur))
   newlines = [l for l in newlines if l != '']
   return newlines

def cut_long_lines(txt, maxlen, sep=os.linesep,
                   l_separ=(' ', ',', ';', '.', ':')):
   """Coupe les morceaux de `txt` (isolés avec `sep`) de plus de `maxlen`
   caractères.
   On utilise successivement les séparateurs de `l_separ`.
   """
   l_lines = txt.split(sep)
   newlines = []
   for line in l_lines:
      if len(line) > maxlen:
         l_sep = list(l_separ)
         line = cut_long_lines(line, maxlen, l_sep[0], l_sep[1:])
         line = maximize_lines(line, maxlen, l_sep[0])
         newlines.extend(line)
      else:
         newlines.append(line)
   # au plus haut niveau, on assemble le texte
   if sep == os.linesep:
      newlines = os.linesep.join(newlines)
   return newlines

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# unique instance du MESSAGE_LOGGER
MessageLog = MESSAGE_LOGGER()


# -----------------------------------------------------------------------------
def U2MESS(code, idmess, valk=(), vali=(), valr=()):
   """Utilitaire analogue à la routine fortran U2MESS/U2MESG avec les arguments
   optionnels.
      code   : 'A', 'E', 'S', 'F', 'I'
      idmess : identificateur du message
      valk, vali, valr : liste des chaines, entiers ou réels.
   
   Appel sans valeurs :                avec valeurs :
      U2MESS('A', 'SUPERVIS_55')          U2MESS('A', 'SUPERVIS_55', vali=[1, 2])
   
   Remarques :
      - Nommer les arguments permet de ne pas tous les passer.
      - Meme fonctionnement que U2MESG :
         + appel à MessageLog
         + puis exception ou abort en fonction du niveau d'erreur.
   """
   MessageLog(code, idmess, valk, vali, valr)
   
   reason = ' <EXCEPTION LEVEE> %s' % idmess
   if code == 'S':
      raise aster.error, reason
   elif code == 'F':
      raise aster.FatalError, reason

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
#
#       !!! UTMESS sera remplacé par U2MESS !!!
#       !!!     Ne plus utilisé UTMESS      !!!
#
def UTMESS(code, sprg, texte):
   """Conserver le temps de réalisé le basculement
   """
   fmt='\n <%s> <%s> %s\n\n'
   sanscode='\n <%s> %s\n\n'
   UL=[
      'MESSAGE',
      'RESULTAT',
      #'ERREUR',
   ]
#
   # Comme l'UTMESS fortran, on supprime le code si on ne fait pas l'abort
   if aster.onFatalError()=='EXCEPTION':
      reason=sanscode % (sprg, texte)
   else:
      reason=fmt % (code, sprg, texte)
   
   for nom in UL:
      # écriture du message
      aster.affiche(nom,reason)

   if code=='S':
      raise aster.error, reason
   elif code=='F':
      raise aster.FatalError, reason

