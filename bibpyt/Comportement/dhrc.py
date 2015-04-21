# coding=utf-8
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
# person_in_charge: sebastien.fayolle@edf.fr

from cata_comportement import LoiComportement

loi = LoiComportement(
    nom='DHRC',
    doc="""Ce modèle homogénéisé permet de représenter l'endommagement et le glissement interne périodique d'une plaque en béton armé pour des sollicitations modérées.
   La loi de comportement s'écrit directement en terme de contraintes et de déformations généralisées.
   La modélisation jusqu'à la rupture n'est pas recommandée, puisque les phénomènes de plastification des aciers et de propagation de fissures ne sont pas
   pris en compte. L'identification des paramètres nécessaires à cette loi de comportement se fait via une procédure préalable d'homogénéisation.
   Pour les précisions sur la formulation du modèle voir [R7.01.36]""",
    num_lc=9999,
    nb_vari=9,
    nom_vari=('ENDOSUP', 'ENDOINF', 'GLISXSUP', 'GLISYSUP',
              'GLISXINF', 'GLISYINF', 'DISSENDO', 'DISSGLIS', 'DISSIP',),
    mc_mater = ('DHRC'),
    modelisation = ('DKTG'),
    deformation = ('PETIT', 'GROT_GDEP'),
    nom_varc = ('TEMP'),
    algo_inte = ('NEWTON',),
    type_matr_tang = (),
    proprietes = None,
)
