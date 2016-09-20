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

    1 : _(u"""
  -> Les groupes '%(k1)s' et '%(k2)s'
     ont les mêmes vingt quatre premiers caractères, leur nom
     court est donc '%(k3)s'.
     Comme il n'est pas légitime dans le cas général que deux
     groupes aux noms initialement différents soient fusionnés,
     le calcul s'arrête.
  -> Conseil :
     Dans le cas où certains des noms de groupes de votre maillage
     sont trop longs, modifiez les pour qu'ils ne dépassent pas
     les 8 caractères.
"""),

    2 : _(u"""
     '%(k1)s'
"""),

    3 : _(u"""
     Fichier MED introuvable.
"""),

    4 : _(u"""
Le champ '%(k1)s' est déjà présent
dans le fichier MED pour l'instant %(r1)G.
  -> Conseil :
     Vous pouvez soit imprimer le champ dans un autre fichier, soit
     nommer le champ différemment.
"""),

    5 : _(u"""
Le champ '%(k1)s' dont vous avez demandé l'impression au format MED
est défini sur des éléments utilisant la famille de points de Gauss
'%(k2)s'. Or l'impression de cette famille n'est pas possible au
format MED.
  -> Conseil :
     Restreignez l'impression demandée aux éléments ne contenant pas
     la famille de point de Gauss incriminée.
"""),

    6 : _(u"""
    Les mots-clés %(k1)s et %(k2)s sont incompatibles.
"""),

    7 : _(u"""
  Il n'a pas été possible d'imprimer le champ %(k1)s en utilisant IMPR_NOM_VARI='OUI'. 
  Cela est dû au fait que certains comportements  dans votre modèle ne sont pas imprimables avec cette option:
  - Le comportement a été défini avec MFRONT ou UMAT en mode prototype
  - Le comportement contient a été défini sur une poutre multifibres
  - Le comportement ne contient que des comportements élastiques

  -> Conseils :
     - N'utilisez pas IMPR_NOM_VARI='OUI' pour imprimer ce champ
"""),


    9 : _(u"""
  Vous demandez l'impression du champ %(k1)s issu de la commande
  PROJ_CHAMP utilisant la méthode 'SOUS_POINT'.

  Cette impression n'est pas possible au format MED.
"""),


    11 : _(u"""
  Votre champ repose sur un modèle comportant des éléments joints
  ou interfaces non encore imprimables au format MED.

  Conseil : Pour que cette impression soit possible, il vous faut
            faire une demande d'évolution.
"""),

}
