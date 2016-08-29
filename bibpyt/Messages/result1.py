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
  Lors de la construction ou de la lecture d'un résultat non-linéaire, le champ des variables internes lu ou créé n'est pas cohérent avec le comportement donné par le mot-clef COMPORTEMENT.
"""),

    2 : _(u"""
Le paramètre <%(k2)s> n'est pas le même sur tous les numéros d'ordre dans la structure de données résultat de nom <%(k1)s>.
Ce n'est pas possible dans cet opérateur.
"""),

    3 : _(u"""
On ne trouve aucun numéro d'ordre dans la structure de données résultat de nom <%(k1)s>
"""),

    40: _(u"""
Le chargement fourni par l'utilisateur est différent de celui présent dans la 
structure de données Résultat. On poursuit les calculs avec le chargement fourni par l'utilisateur.

Risque & Conseil : Vérifiez si le chargement fourni dans la commande est bien celui que vous souhaitez. 
Si oui vous allez poursuivre les calculs post-traitement avec un chargement différent de celui utilisé 
pour calculer les déplacements, températures,...
"""),

    41: _(u"""
Les fonctions multiplicatrices du chargement (mot clé: FONC_MULT) fournies par l'utilisateur sont 
différentes de celles présentes dans la structure de données Résultat. On poursuit les calculs avec 
les fonctions multiplicatrices fournies par l'utilisateur.

Risque & Conseil : Vérifiez si les fonctions fournies dans la commande sont bien celles que vous souhaitez. 
Si oui vous allez poursuivre les calculs de post-traitement avec des fonctions différentes de celles 
utilisées pour calculer les déplacements, températures,...
"""),


    65: _(u"""
Vous avez fourni %(i1)d charges alors qu'il n'y a %(i2)d dans la structure de données Résultat.

Risque & Conseil :
   Vous pouvez obtenir des résultats faux si les charges sont différentes.
   Vérifiez que vous n'avez pas oublié de charge ou que vous n'en avez pas ajouté.
"""),

    66: _(u"""
Le couple (charge, fonction) fourni par l'utilisateur n'est pas présent dans la structure de données résultat.
On poursuit le calcul avec le chargement fourni par l'utilisateur.
   Charge   (utilisateur) : %(k1)s
   Fonction (utilisateur) : %(k2)s
   Charge   (résultat)    : %(k3)s
   Fonction (résultat)    : %(k4)s
"""),

}
