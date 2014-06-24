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
# ======================================================================
# person_in_charge: samuel.geniaut at edf.fr

# determination du type de sd produite par la commande
def raff_xfem_prod(self,TYPE,**args):
  if TYPE == 'DISTANCE' :
     return cham_no_sdaster
  elif TYPE == 'ZONE' :
     return carte_sdaster
  else :
     raise AsException("type de concept non prevu")


RAFF_XFEM=MACRO(nom="RAFF_XFEM",
                op=OPS('Macro.raff_xfem_ops.raff_xfem_ops'),
                sd_prod=raff_xfem_prod,
                fr=tr("Calcul d'un indicateur pour le raffinement"),
                reentrant='n',
                UIinfo={"groupes":("Résultats et champs","Rupture",)},

                TYPE   =SIMP(statut='f',typ='TXM',into=('DISTANCE','ZONE'),defaut='DISTANCE'),
                FISSURE=SIMP(statut='o',typ=fiss_xfem,min=1,max='**',),

                b_zone =BLOC(condition = "TYPE == 'ZONE' ",fr=tr("Paramètres de la zone"),
                   RAYON =SIMP(statut='o',typ='R',val_min=0.),
                            ),
                
                )  ;
