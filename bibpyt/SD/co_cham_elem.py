#@ MODIF co_cham_elem SD  DATE 28/06/2011   AUTEUR COURTOIS M.COURTOIS 
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

import numpy

import Accas
from co_champ import cham_gd_sdaster

# post-traitement :
class post_comp_cham_el :
  def __init__(self, valeurs, maille=None, point=None, sous_point=None) :
    self.valeurs = valeurs
    self.maille = maille
    self.point = point
    self.sous_point = sous_point

class cham_elem(cham_gd_sdaster):
   cata_sdj = "SD.sd_champ.sd_cham_elem_class"

   def EXTR_COMP(self,comp,lgma,topo=0) :
      """ retourne les valeurs de la composante comp du champ sur la liste
        de groupes de mailles lgma avec eventuellement l'info de la
        topologie si topo>0. Si lgma est une liste vide, c'est equivalent
        a un TOUT='OUI' dans les commandes aster
        Attributs retourne
          - self.valeurs : numpy.array contenant les valeurs
        Si on a demande la topo  :
          - self.maille  : numero de mailles
          - self.point   : numero du point dans la maille
          - self.sous_point : numero du sous point dans la maille """
      import aster
      if not self.accessible() :
         raise Accas.AsException("Erreur dans cham_elem.EXTR_COMP en PAR_LOT='OUI'")

      ncham=self.get_name()
      ncham=ncham+(8-len(ncham))*' '
      nchams=ncham[0:7]+'S'
      ncmp=comp+(8-len(comp))*' '

      aster.prepcompcham(ncham,nchams,ncmp,"EL      ",topo,lgma)

      valeurs=numpy.array(aster.getvectjev(nchams+(19-len(ncham))*' '+'.V'))

      if (topo>0) :
         maille=(aster.getvectjev(nchams+(19-len(ncham))*' '+'.M'))
         point=(aster.getvectjev(nchams+(19-len(ncham))*' '+'.P'))
         sous_point=(aster.getvectjev(nchams+(19-len(ncham))*' '+'.SP'))
      else :
         maille=None
         point=None
         sous_point=None

      aster.prepcompcham("__DETR__",nchams,ncmp,"EL      ",topo,lgma)

      return post_comp_cham_el(valeurs,maille,point,sous_point)

