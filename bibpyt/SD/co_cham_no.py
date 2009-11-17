#@ MODIF co_cham_no SD  DATE 16/11/2009   AUTEUR COURTOIS M.COURTOIS 
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
from sd_cham_no import sd_cham_no
from co_champ   import cham_gd_sdaster

import Numeric

# -----------------------------------------------------------------------------
# post-traitement :
class post_comp_cham_no :
  def __init__(self,valeurs,noeud=None) :
    self.valeurs=valeurs
    self.noeud=noeud

# -----------------------------------------------------------------------------
class cham_no_sdaster(cham_gd_sdaster, sd_cham_no):
   def EXTR_COMP(self,comp,lgno,topo=0) :
      """ retourne les valeurs de la composante comp du champ sur la liste
        de groupes de noeuds lgno avec eventuellement l'info de la
        topologie si topo>0. Si lgno est une liste vide, c'est equivalent
        a un TOUT='OUI' dans les commandes aster
        Attributs retourne
          - self.valeurs : Numeric.array contenant les valeurs
        Si on a demande la topo (i.e. self.topo = 1) :
          - self.noeud  : numero de noeud """
      if not self.accessible() :
         raise Accas.AsException("Erreur dans cham_no.EXTR_COMP en PAR_LOT='OUI'")

      ncham=self.get_name()
      ncham=ncham+(8-len(ncham))*' '
      nchams=ncham[0:7]+'S'
      ncmp=comp+(8-len(comp))*' '

      aster.prepcompcham(ncham,nchams,ncmp,"NO      ",topo,lgno)

      valeurs=Numeric.array(aster.getvectjev(nchams+(19-len(ncham))*' '+'.V'))

      if (topo>0) :
         noeud=(aster.getvectjev(nchams+(19-len(ncham))*' '+'.N'))
      else :
         noeud=None

      aster.prepcompcham("__DETR__",nchams,ncmp,"NO      ",topo,lgno)

      return post_comp_cham_no(valeurs,noeud)
      
   def __add__(self, other):
      from SD.sd_nume_equa import sd_nume_equa
      from SD.sd_maillage import sd_maillage
      from SD.co_maillage import maillage_sdaster
      from Cata.cata import CREA_CHAMP,_F
      from Noyau.nommage import GetNomConceptResultat
      # on recupere le type
      __nume_ddl=sd_nume_equa(self.REFE.get()[1])
      __gd=__nume_ddl.REFN.get()[1].strip()
      __type='NOEU_'+__gd
      # on recupere le nom du maillage
      __nomMaillage=self.REFE.get()[0].strip()
      # on recupere l'objet du maillage
      __maillage=CONTEXT.get_current_step().get_concept(__nomMaillage)
      # on recupere le nom a gauche du signe "="
      toto=GetNomConceptResultat(self)
      __CHAM = CREA_CHAMP(OPERATION='ASSE',
                          MAILLAGE=__maillage,
                          TYPE_CHAM=__type,
                          INFO=1,
                          ASSE=(_F(CHAM_GD=self,
                                   TOUT='OUI',
                                   CUMUL='OUI',
                                   COEF_R=1.),
                                _F(CHAM_GD=other,
                                   TOUT='OUI',
                                   CUMUL='OUI',
                                   COEF_R=1.),  
                               ))
      return __CHAM






