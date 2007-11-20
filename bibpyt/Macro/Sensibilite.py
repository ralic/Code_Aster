#@ MODIF Sensibilite Macro  DATE 19/11/2007   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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

"""
Module traite de la gestion des concepts sensibles :
   - classe permettant de stocker les noms des concepts et leurs dérivées
   - corps de la macro MEMO_NOM_SENSI
"""

# protection pour eficas
try:
   import aster
   from Utilitai.Utmess import  UTMESS
except:
   pass

_VIDE_ = '????????'

#-------------------------------------------------------------------------------
def _force_list(obj):
   """Impose le type list."""
   if obj is not None and not type(obj) in (list, tuple):
      obj = (obj,)
   return obj


#-------------------------------------------------------------------------------
class MEMORISATION_SENSIBILITE:
   """Classe pour la mémorisation des concepts sensibles et leurs dérivées.
   """
   def __init__(self, debug=False):
      """Initialisation de la structure
      """
      self._debug = debug
      # dictionnaire de correspondance : ('nom concept', 'nom parasensi') : 'nom compose'
      self._corr = {}
      # idem que _corr mais, pour les commandes principales, on stocke sd elle-meme
      self._cosd = {}
      self._nsnp = {}   # l'inverse
      # dictionnaire donnant les 3 tuples de mots-clés, valeurs, mots-clés facteurs
      self._mcle = {}


   def key(self, nosimp, nopase):
      """Retourne la clé d'accès aux dictionnaires
      """
      if not type(nosimp) in (str, unicode) or not type(nopase) in (str, unicode) or \
         len(nosimp.strip()) > 8 or len(nopase.strip()) > 8:
            UTMESS('F', 'SENSIBILITE_57', valk=(nosimp, nopase))
      return nosimp.strip(), nopase.strip()
   
   
   def set(self, nosimp, nopase, nocomp=None, limocl=None, livale=None, limofa=None,
           verbose=False):
      """On renseigne la structure de mémorisation : CHOIX='E'
      """
      key = self.key(nosimp, nopase)
      if self._corr.has_key(key):
         UTMESS('F', 'SENSIBILITE_90', valk=(nosimp, nopase))
      limocl = _force_list(limocl) or ()
      livale = _force_list(livale) or ()
      limofa = _force_list(limofa) or ()
      if not (len(limocl) == len(livale) == len(limofa)):
         UTMESS('F', 'SENSIBILITE_97')
      
      if nocomp is None:
         nocomp = aster.get_nom_concept_unique('S')
      # on ne conserve que le nom (au moins pour le moment)
      if type(nocomp) != str:
         sd = nocomp
         nocomp = nocomp.nom
      else:
         sd = None
      self._corr[key] = nocomp
      self._cosd[nocomp] = sd
      self._nsnp[nocomp] = key
      self._mcle[key] = (limocl, livale, limofa)
      
      if verbose or self._debug:
         UTMESS('I', 'SENSIBILITE_58', valk=(nosimp, nopase, nocomp))
         if len(limocl) != 0:
            UTMESS('I', 'SENSIBILITE_59', valk=(str(limocl), str(livale), str(limofa)))


   def get_nocomp(self, nosimp, nopase):
      """On récupère le nom composé associé à un nom simple.
      """
      key = self.key(nosimp, nopase)
      nocomp = self._corr.get(key, _VIDE_)
      if self._debug:
         print '<DBG> memo_sensi.get_nocomp'
         print '      nosimp, nopase = "%s", "%s"' % (nosimp, nopase)
         print '      nom composé    = "%s"' % nocomp
      return nocomp


   def get_nsnp(self, nocomp):
      """On récupère le nom du concept et le parasensi associé au nom composé fourni.
      """
      key = self._nsnp.get(nocomp, (_VIDE_, _VIDE_))
      if self._debug:
         print '<DBG> memo_sensi.get_nsnp'
         print '      nom composé    = "%s"' % nocomp
         print '      nosimp, nopase = "%s", "%s"' % key
      return nocomp


   def get_mcle(self, nosimp, nopase):
      """On récupère les mots-clés associés à un couple ('nom concept', 'nom parasensi')
      """
      key = self.key(nosimp, nopase)
      tup3 = self._mcle.get(key, ((), (), ()) )
      if self._debug:
         print tup3
         print '<DBG> memo_sensi.get_mcle      nbmocl =',len(tup3[0])
         print '      nosimp, nopase = "%s", "%s"' % (nosimp, nopase)
         print '      mots-clés      = ', tup3
      return tup3


   def psinfo(self, nosimp):
      """Pendant de l'ex-routine psnosd : retour selon le type de `nosimp`.
      """
      # est-ce une structure dérivée ?
      if nosimp in self._nsnp.keys():
         t_couples = self._nsnp[nosimp]
         ideriv = 1
      else:
         t_couples = self.get_deriv(nosimp)
         ideriv = 0
      # on met le tuple des couples à plat : un tuple de longueur double
      l_res = []
      for coupl in t_couples:
         l_res.extend(coupl)
      t_res = tuple(l_res)
      if self._debug:
         print '<DBG> memo_sensi.psinfo'
         print '      nosimp = "%s"' % nosimp
         print '      ideriv = ', ideriv
         print '      result = ', t_res
      return ideriv, t_res


   def get_deriv(self, nosimp):
      """On récupère la liste des couples ('nom composé', 'nom parasensi') associé à
      un nom simple.
      """
      res = []
      # liste des clés d'accès concernant `nosimp`.
      l_key = [(ns, np) for ns, np in self._corr.keys() if ns == nosimp.strip()]
      for ns, np in l_key:
         res.append((self._corr[ns, np], np))
      return tuple(res)


   def get_nom_sd_princ(self):
      """Retourne la liste des noms des sd dérivées produites par les commandes principales.
      """
      return tuple(self._cosd.keys())


   def delete(self, nosimp, nopase):
      """On récupère les mots-clés associés à un couple ('nom concept', 'nom parasensi')
      """
      key = self.key(nosimp, nopase)
      nocomp = self.get_nocomp(nosimp, nopase)
      if nocomp != _VIDE_:
         del self._corr[key]
         del self._cosd[nocomp]
         del self._mcle[key]
         del self._nsnp[nocomp]


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def memo_nom_sensi_ops(self, NOM, **args):
   """Macro MEMO_NOM_SENSI.
   Transitoire : l'appel aux méthodes de MEMORISATION_SENSIBILITE devraient
      à terme etre fait directement sans introduire de commandes MEMO_NOM_SENSI.
   
   Faut-il traiter les mots-clés NOM_ZERO et NOM_UN ?
   Il me semble que NOM_COMPOSE est toujours présent : obligatoire
   """
   import aster
   
   ier=0
   # La macro compte pour 1 dans la numerotation des commandes
   self.set_icmd(1)
   
   dNOM = NOM[0].cree_dict_valeurs(NOM[0].mc_liste)
   
   nosimp = dNOM['NOM_SD']
   nopase = dNOM['PARA_SENSI'].nom
   nocomp = dNOM['NOM_COMPOSE']
   
   self.jdc.memo_sensi.set(nosimp, nopase, nocomp,
                           dNOM['MOT_CLE'], dNOM['VALEUR'], dNOM['MOT_FACT'],
                           verbose=True)

   # s'il faut déclarer le concept dérivé dans le jdc
   if dNOM['TYPE_SD_DERIV']:
      self.DeclareOut('nocomp', dNOM['NOM_COMPOSE'])
      # enregistrement dans le tableau des concepts jeveux
      icmdt = aster.co_register_jev(dNOM['NOM_COMPOSE'].nom, dNOM['TYPE_SD_DERIV'].upper(), 'MEMO_NOM_SENSI')
   
   return ier
 
 
 
