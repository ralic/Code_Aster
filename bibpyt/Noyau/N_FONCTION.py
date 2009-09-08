#@ MODIF N_FONCTION Noyau  DATE 07/09/2009   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
# RESPONSABLE COURTOIS M.COURTOIS
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
#                                                                       
#                                                                       
# ======================================================================

# attention ! cet import permet d'avoir, dans les formules, le comportement
# de la division réelle pour les entiers, et non la division entière
# 1/2=0.5 (et non 0)
from __future__ import division

from N_ASSD import ASSD
from asojb import AsBase

class FONCTION(ASSD):pass

class formule(ASSD,AsBase):
   def __init__(self,**args):
      ASSD.__init__(self,**args)
      self.nompar    =None
      self.expression=None

   def __call__(self,*val):
      if hasattr(self.parent,'contexte_fichier_init'):
                        context=self.parent.contexte_fichier_init
      else            : context={}
      i=0
      for param in self.nompar : 
         context[param]=val[i]
         i=i+1
      try :
       res=eval(self.expression,self.jdc.const_context, context)
      except :
       print 75*'!'
       print '! ' + '%-72s' % ('Erreur evaluation formule '+self.nom) + '!'
       print 75*'!'
       raise
      return res

   def setFormule(self,nom_para,texte):
      """
         Cette methode sert a initialiser les attributs
         nompar, expression et code qui sont utilisés
         dans l'évaluation de la formule
      """
      self.nompar     = nom_para
      self.expression = texte
      try :
        self.code=compile(texte,texte,'eval')
      except SyntaxError :
        print 75*'!'
        print '! ' + '%-72s' % ('Erreur evaluation formule '+self.nom) + '!'
        print 75*'!'
        raise

   def __setstate__(self,state):
      """
         Cette methode sert a restaurer l'attribut code
         lors d'un unpickle
      """
      self.__dict__.update(state)                  # update attributes
      self.setFormule(self.nompar,self.expression) # restore code attribute
      
   def __getstate__(self):
      """
         Pour les formules, il faut enlever l'attribut code
         qui n'est pas picklable
      """
      d=ASSD.__getstate__(self)
      del d['code']
      return d

   def Parametres(self):
      """Equivalent de fonction.Parametres pour pouvoir utiliser des formules
      à la place de fonctions dans certaines macro-commandes.
      """
      from SD.sd_fonction  import sd_formule
      from Utilitai.Utmess import UTMESS
      if self.accessible():
        TypeProl={'E':'EXCLU', 'L':'LINEAIRE', 'C':'CONSTANT', 'I':'INTERPRE' }
        sd = sd_formule(self.get_name())
        prol = sd.PROL.get()
        nova = sd.NOVA.get()
        if prol is None or nova is None:
           UTMESS('F', 'SDVERI_2', valk=[objev])
        dico={
         'INTERPOL'    : ['LIN','LIN'],
         'NOM_PARA'    : [s.strip() for s in nova],
         'NOM_RESU'    : prol[3][0:16].strip(),
         'PROL_DROITE' : TypeProl['E'],
         'PROL_GAUCHE' : TypeProl['E'],
        }
      else:
         raise Accas.AsException("Erreur dans fonction.Parametres en PAR_LOT='OUI'")
      return dico


class formule_c(formule):
   pass



