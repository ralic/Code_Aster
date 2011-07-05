#@ MODIF co_maillage SD  DATE 05/07/2011   AUTEUR COURTOIS M.COURTOIS 
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

import Accas
from Accas import ASSD

class maillage_sdaster(ASSD):
   cata_sdj = "SD.sd_maillage.sd_maillage"

   def LIST_GROUP_NO(self) :
      """ retourne la liste des groupes de noeuds sous la forme :
        [ (gno1, nb noeuds  gno1), ...] """
      if not self.accessible():
         raise Accas.AsException("Erreur dans maillage.LIST_GROUP_NO en PAR_LOT='OUI'")
      dic_gpno = self.sdj.GROUPENO.get()
      return [(gpno.strip(),len(dic_gpno[gpno])) for gpno in dic_gpno]

   def LIST_GROUP_MA(self) :
      """ retourne la liste des groupes de mailles sous la forme :
        [ (gma1, nb mailles gma1, dime max des mailles gma1), ...] """
      if not self.accessible():
         raise Accas.AsException("Erreur dans maillage.LIST_GROUP_MA en PAR_LOT='OUI'")
      import aster
      ngpma = []
      ltyma = aster.getvectjev("&CATA.TM.NOMTM")
      catama = aster.getcolljev("&CATA.TM.TMDIM")
      dic_gpma = self.sdj.GROUPEMA.get()
      if dic_gpma is None:
          return ngpma
      dimama = [catama[ltyma[ma-1]][0] for ma in self.sdj.TYPMAIL.get()]
      for grp in dic_gpma.keys():
         dim = max([dimama[ma-1] for ma in dic_gpma[grp]])
         ngpma.append((grp.strip(), len(dic_gpma[grp]),dim))
      return ngpma

