# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
# person_in_charge: josselin.delmas at edf.fr

cata_msg = {


   10 : _(u"""
   Les champs que l'on cherche à combiner doivent tous être des champs aux noeuds.
"""),

   11 : _(u"""
   Les champs que l'on cherche à combiner doivent tous avoir la même grandeur (DEPL_R, ...).
   Ce doit être la même que celle donnée dans TYPE_CHAM.
"""),

   12 : _(u"""
   Les champs que l'on cherche à combiner doivent tous avoir la même numérotation.
"""),

   13 : _(u"""
   Les champs que l'on cherche à combiner doivent tous s'appuyer sur le même maillage.
"""),

   14 : _(u"""
   On impose la même numérotation sur le champ de sortie (mots-clefs NUME_DDL ou CHAM_NO) que sur les champs d'entrée pour économiser de la mémoire.
   Mais cette opération fait perdre l'information sur les degrés de liberté correspondant aux conditions limites dualisées (AFFE_CHAR_MECA ou AFFE_CHAR_THER).
   Si vous ne voulez pas perdre cette information, il ne faut pas utiliser NUME_DDL ou CHAM_NO.
"""),


}
