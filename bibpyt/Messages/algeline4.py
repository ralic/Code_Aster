#@ MODIF algeline4 Messages  DATE 19/02/2008   AUTEUR COURTOIS M.COURTOIS 
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

def _(x) : return x

cata_msg = {

1 : _("""
 Seules les méthodes de résolution LDLT et MULT_FRONT sont autorisées.
"""),

3 : _("""
 Erreur lors de la résolution d'un système linéaire (GCPC) :
 Non convergence  avec le nombre d'iterations autorisé :  %(i1)d
   norme du residu (absolu)  :  %(r1)f
   norme du residu (relatif) :  %(r2)f

 Conseils :
  * Vous pouvez augmenter le nombre d'itérations autorisées (SOLVEUR/NMAX_ITER).
  * Vous pouvez aussi augmenter le niveau de remplissage pour la factorisation
    incomplète (SOLVEUR/NIVE_REMPLISSAGE).
  * Dans une commande non-linéaire (STAT_NON_LINE par exemple) vous pouvez aussi essayer de
    diminuer la précision demandée pour la convergence (SOLVEUR/RESI_RELA), mais c'est plus
    risqué car cela peut empecher la convergence de l'algorithme non-linéaire.
"""),

4 : _("""
  Manque de mémoire :
     Mémoire disponible = %(i1)d
     Mémoire nécessaire = %(i2)d
"""),

5 : _("""
 Erreur données : noeud déjà existant :  %(k1)s
"""),

7 : _("""
 Erreur données : maille déjà existante :  %(k1)s
"""),

9 : _("""
 Erreur données GROUP_MA déjà existant :  %(k1)s
"""),

11 : _("""
 erreur données GROUP_NO déjà existant :  %(k1)s
"""),

19 : _("""
 Matrice masse non définie, il faudrait essayer l'autre algorithme de résolution.
"""),

21 : _("""
 manque de place memoire longueur de bloc insuffisante:  %(i1)d
 le super-noeud  %(i2)d
  neccessite un bloc de  %(i3)d
"""),

24 : _("""
 %(k1)s   pour le mot cle :  %(k2)s    noeud :  %(k3)s composante :  %(k4)s
"""),

25 : _("""
 combinaison non prevue   type resultat :  %(k1)s    type matrice  :  %(k2)s
    type constante:  %(k3)s
"""),

27 : _("""
 combinaison non prevue
 type résultat :  %(k1)s
 type matrice  :  %(k2)s
"""),

31 : _("""
 combinaison non prevue
 type résultat :  %(k1)s
"""),

33 : _("""
 la normalisation doit se faire en place
 il est impossible d'avoir comme concept produit  %(k1)s et %(k2)s comme concept d'entrée.
"""),

36 : _("""
 l'option de normalisation  %(k1)s  n'est pas implantée. %(i1)d
"""),

37 : _("""
 problème(s) rencontré(s) lors de la factorisation de la matrice : %(k1)s
"""),

38 : _("""
 appel erroné :
 code retour de rsexch : %(i1)d
 pb CHAM_NO %(k1)s
"""),

42 : _("""
 pas de produit car les valeurs de la MATRICE sont  %(k1)s
 et celles du CHAM_NO sont  %(k2)s
"""),

43 : _("""
 la maille de nom  %(k1)s  existe déjà %(k2)s
"""),

55 : _("""
 pas d'extraction pour  %(k1)s
 pour le numéro d'ordre  %(i1)d
"""),

56 : _("""
 pas de mode extrait pour  %(k1)s
"""),

57 : _("""
 NUME_MODE identique pour le %(i1)d
 mode d'ordre  %(i2)d
"""),

58 : _("""
  problème dans le préconditionnement de la matrice MATAS par LDLT imcomplet
  pivot nul à la ligne :  %(i1)d
"""),

60 : _("""
  incoherence n2 NBDDL sans lagranges %(i1)d NBDDL reconstitués %(i2)d
"""),

61 : _("""
 pas de mode statique pour le noeud :  %(k1)s  et sa composante :  %(k2)s
"""),

62 : _("""
 pour les modes statiques, on attend un :  %(k1)s
 noeud :  %(k2)s
 cmp   :  %(k3)s
"""),

63 : _("""
 champ inexistant.
 champ    :  %(k1)s
 noeud    :  %(k2)s
 cmp      :  %(k3)s
"""),

64 : _("""
 détection d'un terme nul sur la sur diagonale
 valeur de BETA   %(r1)f
 valeur de ALPHA  %(r2)f
"""),

65 : _("""
 on a la  %(i1)d -ème fréquence du système réduit  est complexe =  %(r1)f
  et partie_imaginaire/réelle =  %(r2)f
"""),

66 : _("""
 la valeur propre est :   %(r1)f
"""),

74 : _("""
 calcul d'erreur modale :
 une valeur propre réelle est detectee %(k1)s à partir du couple (fréquence, amortissement réduit)
 on ne peut plus la reconstruire %(k2)s
 par convention l'erreur modale est fixée à : %(r1)f
"""),

75 : _("""
 problème généralisé complexe
 amortissement (reduit) de décalage supérieur en valeur absolue à  %(r1)f
 on le ramène à la valeur :  %(r2)f
"""),

76 : _("""
 la réorthogonalisation diverge après  %(i1)d  itération(s)   %(i2)d
"""),

77 : _("""
 l'option de normalisation  %(k1)s  n'est pas implantée.
"""),

79 : _("""
 champ inexistant  %(k1)s impossible de récupérer NEQ %(k2)s
"""),

80 : _("""
 type de valeurs inconnu   %(k1)s
"""),

81 : _("""
 champ inexistant  %(k1)s
"""),

82 : _("""
 incohérence de certains paramètres modaux propres à ARPACK
 numéro d'erreur  %(i1)d
"""),

83 : _("""
 nombre de valeurs propres convergées  %(i1)d < nombre de fréquences demandées  %(i2)d
 erreur ARPACK numéro :  %(i3)d
 --> le calcul continue, la prochaine fois %(i4)d
 -->   augmenter DIM_SOUS_ESPACE =  %(i5)d
 -->   ou NMAX_ITER_SOREN =  %(i6)d
 -->   ou PREC_SOREN =  %(r1)f
### idem algeline4_98 ?
"""),

85 : _("""
 appel erroné mode numéro %(i1)d position modale %(i2)d
 code retour de RSEXCH : %(i3)d
 pb CHAM_NO %(k1)s
"""),

86 : _("""
 la réorthogonalisation diverge après  %(i1)d  itération(s) %(i2)d
       vecteur traité :  %(i3)d
       vecteur testé  :  %(i4)d
 arret de la réorthogonalisation %(k1)s
"""),

87 : _("""
 pour le probleme réduit
 valeur(s) propre(s) réelle(s)                  :  %(i1)d
 valeur(s) propre(s) complexe(s) avec conjuguée :  %(i2)d
 valeur(s) propre(s) complexe(s) sans conjuguée :  %(i3)d
"""),

88 : _("""
 votre problème est fortement amorti.
 valeur(s) propre(s) réelle(s)                  :  %(i1)d
 valeur(s) propre(s) complexe(s) avec conjuguée :  %(i2)d
 valeur(s) propre(s) complexe(s) sans conjuguée :  %(i3)d
"""),

94 : _("""
 problème quadratique complexe
 amortissement (réduit) de décalage supérieur en valeur absolue à  %(r1)f
 on le ramène à la valeur :  %(r2)f
"""),

95 : _("""
 problème quadratique
 amortissement (réduit) de décalage supérieur en valeur absolue à  %(r1)f
 on le ramène à la valeur :  %(r2)f
"""),

98 : _("""
 nombre de valeurs propres convergées  %(i1)d < nombre de fréquences demandées  %(i2)d
 erreur ARPACK numéro :  %(i3)d
 --> le calcul continue, la prochaine fois
 -->   augmenter DIM_SOUS_ESPACE =  %(i4)d
 -->   ou NMAX_ITER_SOREN =  %(i5)d
 -->   ou PREC_SOREN =  %(r1)f
 si votre problème est fortement amorti, il est possible que
 des modes propres non calculés soient sur-amortis
 --> diminuez le nombre de fréquences demandées
"""),

}
