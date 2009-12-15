#@ MODIF co_resultat SD  DATE 14/12/2009   AUTEUR ANDRIAM H.ANDRIAMBOLOLONA 
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

import Accas
from SD import *
from sd_resultat import sd_resultat

# -----------------------------------------------------------------------------
class resultat_sdaster(ASSD, sd_resultat):
   def LIST_CHAMPS (self) :
      if not self.accessible():
         raise Accas.AsException("Erreur dans resultat.LIST_CHAMPS en PAR_LOT='OUI'")
      return aster.GetResu(self.get_name(), "CHAMPS")

   def LIST_NOM_CMP (self) :
      if not self.accessible():
         raise Accas.AsException("Erreur dans resultat.LIST_NOM_CMP en PAR_LOT='OUI'")
      return aster.GetResu(self.get_name(), "COMPOSANTES")

   def LIST_VARI_ACCES (self) :
      if not self.accessible():
         raise Accas.AsException("Erreur dans resultat.LIST_VARI_ACCES en PAR_LOT='OUI'")
#      return aster.GetResu(self.get_name(), "VARI_ACCES")
      return aster.GetResu(self.get_name().ljust(19), "VARI_ACCES")

   def LIST_PARA (self) :
      if not self.accessible():
         raise Accas.AsException("Erreur dans resultat.LIST_PARA en PAR_LOT='OUI'")
      return aster.GetResu(self.get_name(), "PARAMETRES")

# -----------------------------------------------------------------------------
class resultat_jeveux(resultat_sdaster):
   """Classe permettant d'accéder à un resultat jeveux qui n'a pas d'ASSD associée,
   c'est le cas des concepts résultats (table, evol_xxxx) dérivés."""
   def __init__(self,nom_jeveux):
      self.nom=nom_jeveux
      AsBase.__init__(self, nomj=self.nom)
