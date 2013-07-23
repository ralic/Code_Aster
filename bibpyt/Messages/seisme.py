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
# person_in_charge: josselin.delmas at edf.fr

cata_msg={

 1: _(u"""
 le noeud %(k1)s n'appartient pas au maillage: %(k2)s
"""),

 2: _(u"""
 le groupe de noeuds %(k1)s n'appartient pas au maillage: %(k2)s
"""),

 3: _(u"""
 le noeud %(k1)s n'est pas un noeud support.
"""),

 4: _(u"""
 le vecteur directeur du spectre est nul.
"""),

 5: _(u"""
 cas du MONO_APPUI: vous avez déjà donné un spectre pour cette direction.
"""),

 6: _(u"""
 erreur(s) rencontrée(s) lors de la lecture des supports.
"""),

 7: _(u"""
 vous avez déjà donné un spectre pour le support %(k1)s
"""),

 8: _(u"""
 on ne peut pas traiter du MONO_APPUI et du MULTI_APPUI simultanément.
"""),





10: _(u"""
 correction statique non prise en compte pour l'option: %(k1)s
"""),

11: _(u"""
 trop d'amortissements modaux
   nombre d'amortissement: %(i1)d
   nombre de mode        : %(i2)d
"""),

12: _(u"""
 amortissement non diagonal, on ne sait pas traiter.
"""),

13: _(u"""
 il manque des amortissements modaux
   nombre d'amortissements: %(i1)d
   nombre de modes        : %(i2)d
"""),

14: _(u"""
 on ne peut pas demander de réponse secondaire sans la réponse primaire
"""),

15: _(u"""
 analyse spectrale :
   la base modale utilisée est               : %(k1)s
   le nombre de vecteurs de base est         : %(i1)d
   la règle de combinaison modale est        : %(k2)s
   les options de calcul demandées sont      : %(k3)s """
   ),

16: _(u"""
                                               %(k1)s """
   ),

17: _(u"""
   la nature de l'excitation est             : %(k1)s """
   ),

18: _(u"""
   la règle de combinaison des réponses
   directionnelles est                       : %(k1)s """
   ),

19: _(u"""
   la règle de combinaison des contributions
   de chaque mouvement d'appui est           : %(k1)s """
   ),

20: _(u"""
 erreur dans les données
   la masse de la structure n'existe pas dans la table: %(k1)s
"""),

21: _(u"""
 il faut au moins 2 occurrences de DEPL_MULT_APPUI pour la combinaison des appuis.
"""),

22: _(u"""
 COMB_DEPL_APPUI: il faut au moins définir 2 cas derrière le mot clé LIST_CAS.
"""),

23: _(u"""
 données incompatibles
   pour la direction   : %(k1)s
   nombre de blocage   : %(i1)d
   nombre d'excitations: %(i2)d
"""),

24: _(u"""
 données incompatibles
   pour les modes mécaniques : %(k1)s
   il manque l'option        : %(k2)s
"""),

25: _(u"""
  problème stockage
    option de calcul: %(k1)s
    occurrence       : %(i1)d
    nom du champ    : %(k3)s
"""),

26: _(u"""
  problème stockage
    option de calcul: %(k1)s
    direction       : %(k2)s
    nom du champ    : %(k3)s
"""),

27: _(u"""
  La base modale utilisé %(k1)s ne contient pas tous les paramètres modaux
  nécessaires au calcul.
  Il faut que le concept soit issu d'un calcul sur coordonnées physiques et
  non pas généralisées.
"""),

28: _(u"""
  Dans le cas d'excitations décorrélées,
  le mot-clé COMB_MULT_APPUI n'est pas pris en compte.
"""),

29: _(u"""
  La définition du groupe d'appuis n'est pas correcte dans le cas décorrélé:
  au moins une excitation appartient à plusieurs groupes d'appuis.
  Les groupes d'appuis doivent être disjoints.
"""),

30: _(u"""
  La définition du groupe d'appuis n'est pas correcte dans le cas décorrélé.
  Un seul groupe d'appuis a été constitué contenant tous les appuis.
  Relancez le calcul avec le mot-clé MULTI_APPUI=CORRELE.
"""),

31: _(u"""
 Attention,
 il n'y a pas de déplacements différentiels pris en compte dans votre calcul
 spectral multiappui.
"""),

32: _(u"""
 Il n'est pas possible d'utiliser la combinaison GUPTA en MULTI_APPUI.
"""),

33: _(u"""
 Dans le cadre de l'utilisation de la combinaison de type GUPTA il faut que F1 < F2.
"""),

34: _(u"""
 Il n'y a pas de points d'appui sur la structure.
  -> Conseil : Vérifiez dans ASSEMBLAGE que les conditions aux limites sont présentes.
"""),

35: _(u"""
 Tolérance sur l'ajustement du spectre au tirage %(i1)d:
 L'erreur %(k1)s vaut %(r1).2f %% ce qui est supérieur à la borne de %(r2).2f %% demandée. 
"""),

36: _(u"""
 ATTENTION, FREQ_FOND <0 à l'instant t=  %(r2).2f s. 

"""),

}
