# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
# person_in_charge: renaud.bargellini at edf.fr

from cata_comportement import LoiComportement

loi = LoiComportement(
    nom='ROUSSELIER',
    doc="""Relation de comportement élasto-plastique de G.Rousselier en grandes déformations.
   Elle permet de rendre compte de la croissance des cavités et de décrire la rupture ductile.
   Pour faciliter l'intégration de ce modèle, il est conseillé d'utiliser systématiquement le redécoupage global du pas de temps (SUBD_PAS).""",
    num_lc=36,
    nb_vari=9,
    nom_vari=('EPSPEQ', 'POROSITE', 'INDIPLAS',
              'EPSEXX', 'EPSEYY', 'EPSEZZ', 'EPSEXY', 'EPSEXZ', 'EPSEYZ'),
    mc_mater = ('ELAS', 'ROUSSELIER'),
    modelisation = ('3D', 'AXIS', 'D_PLAN'),
    deformation = ('SIMO_MIEHE'),
    nom_varc = ('TEMP'),
    algo_inte = ('NEWTON_1D',),
    type_matr_tang = ('PERTURBATION', 'VERIFICATION'),
    proprietes = None,
)
