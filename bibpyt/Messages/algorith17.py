#@ MODIF algorith17 Messages  DATE 27/08/2012   AUTEUR ALARCON A.ALARCON 
# -*- coding: iso-8859-1 -*-
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
# ======================================================================
# RESPONSABLE DELMAS J.DELMAS

cata_msg={
1: _(u"""
 Il y a moins de sous-domaines (%(i1)d) que de processeurs participant au calcul (%(i2)d).

 Conseils :
   - augmentez le nombre de sous-domaines de la partition du mot-clé PARTITION
   - diminuez le nombre de processeurs du calcul
"""),

2: _(u"""
         Comportement %(k1)s non implanté pour l'élément d'interface
"""),

4: _(u"""
        La formulation n'est ni en contrainte nette ni en Bishop
"""),

5 : _(u"""
  Le champ post-traité est un CHAM_ELEM, le calcul de moyenne ne fonctionne que
 sur les CHAM_NO. Pour les CHAM_ELEM utiliser POST_ELEM mot-clé INTEGRALE.
"""),

6 : _(u"""
  Le calcul de la racine numéro %(i1)d par la méthode de la matrice compagnon a échoué.
"""),

7 : _(u"""
  Il n'y a qu'un seul MODE_MECA en entrée de DEFI_BASE_MODALE. La numérotation
  de référence prise est celle associée a celui-ci. Le mot-clé NUME_REF
  n'est pas pris en compte
"""),

8 : _(u"""
  Il manque le nume_ddl pour le résultat. Propositions :
   - renseigner le mot-clé NUME_REF dans DEFI_BASE_MODALE,
   - utiliser les mots-clés MATR_A et MATR_B dans CREA_RESU.
"""),

10 : _(u"""
  La loi de comportement mécanique %(k1)s n'est pas compatible avec les
  éléments de joint avec couplage hydro-mécanique.
"""),
11 : _(u"""
  La fermeture du joint sort des bornes [0,fermeture maximale] sur la maille %(k1)s.
  fermeture du joint CLO = %(r1)f
  fermeture maximale UMC = %(r2)f
  Vérifier la cohérence chargement mécanique, fermeture asymptotique et ouverture
  initiale.
"""),
12 : _(u"""
  La température de référence (exprimée en Kelvin) doit toujours être strictement supérieure à zéro.
"""),
13 : _(u"""
  La pression de gaz de référence doit toujours être différente de zéro.
"""),

14 : _(u"""
  Les mots clés PRES_FLU et PRES_CLAV sont incompatibles avec les modélisations xxx_JOINT_HYME
"""),

15 : _(u"""
  Les données matériau RHO_F, VISC_F et OUV_MIN sont obligatoires avec les modélisations xxx_JOINT_HYME
"""),


16 : _(u"""
  Les données matériau RHO_F, VISC_F et OUV_MIN sont incompatibles avec les modélisations xxx_JOINT
"""),

17 : _(u"""
  La partition %(k1)s que vous utilisez pour partitionner le modèle %(k2)s en sous-domaines a été construite sur un autre modèle (%(k3)s).

  Conseil : vérifiez la cohérence des modèles.
"""),

18 : _(u"""
  La base de modes associée au résultat généralisé sous le mot-clé
  EXCIT_RESU %(i1)d n'est pas la même que celle utilisée pour la
  fabrication des matrices généralisées.
"""),

19 : _(u"""
  La projection d'un resultat non réel sur une base de mode (de type
  résultat harmonique) n'est pas possible. Vous pouvez demander
  l'évolution.
"""),

20 : _(u"""
  La prise en compte d'un amortissement équivalent a un amortissement modal par le mot-clé AMOR_MODAL nécessite 
  une base de modes pré calculée sur laquelle est décomposé l'amortissement. 
  Conseil: vérifiez qu'un base de modes est bien renseignée sous le mot-clé MODE_MECA.
"""),
21 : _(u"""
  Aucune valeur d'amortissement modal n'a été trouvée sous le mot-clé AMOR_MODAL. 
  Cette information est nécessaire pour la prise en compte d'un amortissement de type modal.

"""),

25 : _(u"""
  Lors de la reprise du calcul, la liste des champs calculés (DEPL, VITE, ACCE) doit être la même 
  pour le concept entrant et sortant.
"""),
26 : _(u"""
  La structure de données resultat est corrompue. Elle ne contient pas d'objet avec la liste des numéros d'ordre.
"""),
27 : _(u"""
  La structure de données resultat est corrompue. La liste des numéros d'ordres ne correspond pas
  à la liste des discrétisations temporelles ou fréquentielles.
"""),
28 : _(u"""
  La structure de données en entrée ne contient aucun des champs requis pour la restitution temporelle.
  Conseil: vérifiez la liste des champs renseignée sous NOM_CHAM, ou bien testez l'option TOUT_CHAM='OUI'.
"""),
29 : _(u"""
  Erreur dans l'allocation de la structure de données dynamique. La liste des champs à allouer n'est pas valide.
"""),
}
