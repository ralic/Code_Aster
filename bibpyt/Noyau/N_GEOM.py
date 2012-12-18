#@ MODIF N_GEOM Noyau  DATE 18/12/2012   AUTEUR SELLENET N.SELLENET 
# -*- coding: iso-8859-1 -*-
# RESPONSABLE COURTOIS M.COURTOIS
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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


"""

"""
from N_ASSD import ASSD

class GEOM(ASSD):
   """
      Cette classe sert à définir les types de concepts
      géométriques comme GROUP_NO, GROUP_MA,NOEUD et MAILLE

   """
   def __init__(self,nom,etape=None,sd=None,reg='oui'):
      """
      """
      self.etape=etape
      self.sd=sd
      if etape:
        self.parent=etape.parent
      else:
        self.parent=CONTEXT.get_current_step()
      if self.parent :
         self.jdc = self.parent.get_jdc_root()
      else:
         self.jdc = None

      if not self.parent:
        self.id=None
      elif reg == 'oui' :
        self.id = self.parent.reg_sd(self)
      self.nom=nom

   def get_name(self):
      return self.nom

   def __convert__(cls,valeur):
      if isinstance(valeur, (str,unicode)) and len(valeur.strip()) <= 8:
         return valeur.strip()
      raise ValueError(_(u'On attend une chaine de caractères (de longueur <= 8).'))
   __convert__=classmethod(__convert__)

class geom(GEOM):pass

