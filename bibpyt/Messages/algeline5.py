#@ MODIF algeline5 Messages  DATE 06/08/2007   AUTEUR LEBOUVIER F.LEBOUVIER 
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

cata_msg={





















4: _("""
 erreur lapack (ou blas) au niveau de la routine  %(k1)s
  le parametre numero  %(i1)d
  n'a pas une valeur coherente %(i2)d
"""),

5: _("""
 !! Attention, vous utilisez l'option de test FETI de l'interface.
 On va donc simuler la résolution d'un système diagonal canonique,
 pour provoquer un test d'ensemble de l'algorithme qui doit trouver
 la solution U=1 sur tous les noeuds.
 Vos résultats sont donc articiellement faussés pour les besoins de
 ce test. Pour réaliser effectivement votre calcul, désactiver cette
 option (INFO_FETI(12:12)='F' au lieu de 'T') !!
"""),

6: _("""
 probleme dans le traitement des resultats de amdbar:
"""),

7: _("""

"""),

8: _("""
 probleme dans le traitement des resultats de amdbar:
"""),

9: _("""

"""),

10: _("""
 ! le nb de noeuds de la structure:  %(i1)d
 ! la base utilisee est           :  %(k1)s
 ! les caracteristiques elemtaires:  %(k2)s
 ! diametre de la structure       :  %(r1)f
 ! type de pas                    :  %(i2)d
 ----------------------------------------------
"""),

11: _("""
 ! le profil de vitesse de la zone:  %(k1)s
 !   type de reseau de la zone    :  %(i1)d
 ----------------------------------------------
"""),

12: _("""

"""),

13: _("""
  ! le noeud d application         :  %(k1)s
 ! la base utilisee est           :  %(k2)s
 ! les caracteristiques elemtaires:  %(k3)s
 ! diametre de la structure       :  %(r1)f
 ! type de configuration          :  %(k4)s
 ! le coefficient de masse ajoutee:  %(r2)f
 ! le profil de masse volumique   :  %(r3)f
 ----------------------------------------------

"""),

14: _("""
    pas de couplage pris en compte
 ----------------------------------------------
"""),

15: _("""
   pour le concept  %(k1)s  le mode numero  %(i1)d
"""),

16: _("""
  de frequence  %(r1)f
"""),

17: _("""
  de charge critique  %(r1)f
"""),

18: _("""
  a une norme d'erreur de  %(r1)f  superieure au seuil admis  %(r2)f
"""),

19: _("""
   pour le concept  %(k1)s  le mode numero  %(i1)d
"""),

20: _("""
  de frequence  %(r1)f
  est en dehors de l'intervalle de recherche : %(r2)f
  ,  %(r3)f
"""),

21: _("""
  de charge critique  %(r1)f
  est en dehors de l'intervalle de recherche : %(r2)f
  ,  %(r3)f
"""),

22: _("""

"""),

23: _("""
   pour le concept  %(k1)s
"""),

24: _("""
  dans l'intervalle  ( %(r1)f  ,  %(r2)f )   il y a theoriquement  %(i1)d
  frequence(s) et l'on en a calcule  %(i2)d
"""),

25: _("""
  dans l'intervalle  ( %(r1)f  ,  %(r2)f )   il y a theoriquement  %(i1)d
  charge(s) critique(s) et l'on en a calcule  %(i2)d
"""),

26: _("""

"""),

27: _("""
 la valeur du shift %(r1)f  est une frequence propre
"""),

28: _("""
 les nombres de termes des matrices rigi et masse different
 celui de la matrice masse vaut :  %(i1)d
 celui de la matrice rigi vaut :  %(i2)d

"""),

29: _("""
 le nombre d'amortissements reduits est trop grand
 le nombre de modes propres vaut  %(i1)d
 et le nombre de coefficients :   %(i2)d
 on ne garde donc que les %(i3)d premiers coefficients

"""),

30: _("""
 le nombre d'amortissements reduits est insuffisant, il en manque :  %(i1)d,
 car le nombre de modes vaut :  %(i2)d
 on rajoute  %(i3)d amortissements réduits avec la valeur du dernier mode propre.
"""),

31: _("""
  incoherence dans deeq i =  %(i1)d deeq(2*i-1) =  %(i2)d deeq(2*i) =  %(i3)d

"""),

32: _("""
  erreur de type delg(iddl) diff de -1 ou -2  %(i1)d
"""),

33: _("""
 un ddl bloque a au moins 2 lambda1 ou 2 lambda2, le ddl bloque est  %(i1)d

"""),

34: _("""
 incoherence des lagrangesddl %(i1)d lambda1 %(i2)d lambda1 %(i3)d
"""),

35: _("""
 erreur programmeur le lambda2  %(i1)d a moins de 2 voisins
 il faut le lambda1 et au moins un ddl

"""),

36: _("""
 Probleme dans le calcul des ddl: num devrait etre egal a n1 : num %(i1)d , n1 %(i2)d
 impression des lagranges
"""),

37: _("""
 NUME_DDL incoherence des lagranges
  ddl     %(i1)d
  lambda1 %(i2)d
  lambda1 %(i3)d
"""),

38: _("""
 nbre de relations lineaires %(i1)d
"""),

39: _("""
 lambda1 de r lineaire : %(i1)d
 lambda2 de r lineaire : %(i2)d
"""),

40: _("""
 Données erronées
"""),


41: _("""
 pas de mode statique pour  le noeud :  %(k1)s  et sa composante :  %(k2)s

"""),

42: _("""
 pour les modes statiques.on attend un :  %(k1)s    noeud :  %(k2)s
      cmp :  %(k3)s

"""),

43: _("""
 champ inexistant.pb champ :  %(k1)s    noeud :  %(k2)s      cmp :  %(k3)s

"""),

44: _("""
 incoherence de certains parametres modaux propres a arpack
  numero d'erreur  %(i1)d

"""),

45: _("""
 nombre de valeurs propres convergees  %(i1)d
 < nombre de frequences demandees  %(i2)d
 erreur arpack numero :  %(i3)d
 --> le calcul continue, la prochaine fois
 -->   augmenter dim_sous_espace =  %(i4)d
 -->   ou nmax_iter_soren =  %(i5)d
 -->   ou prec_soren =  %(r1)f

"""),

46: _("""
 incoherence de certains parametres modaux propres a arpack
  numero d'erreur  %(i1)d

"""),

47: _("""
 la valeur propre numero  %(i1)d a une partie imaginaire non nulle
 re(vp) = %(r1)f
 im(vp) = %(r2)f
 --> ce phenomene numerique est frequent
 --> sur les premieres valeurs propres
 --> lorsque le spectre recherche est
 --> tres etendu (en pulsation)

"""),

48: _("""
 incoherence de certains parametres modaux propres a arpack
  numero d'erreur  %(i1)d

"""),

49: _("""
 nombre de valeurs propres convergees  %(i1)d
 < nombre de frequences demandees  %(i2)d
 erreur arpack numero :  %(i3)d
 --> le calcul continue, la prochaine fois
 -->   augmenter dim_sous_espace =  %(i4)d
 -->   ou nmax_iter_soren =  %(i5)d
 -->   ou prec_soren =  %(r1)f

"""),

50: _("""
 incoherence de certains parametres modaux propres a arpack
  numero d'erreur  %(i1)d

"""),

51: _("""
 la valeur propre numero  %(i1)d a une partie imaginaire non nulle
 re(vp) = %(r1)f
 im(vp) = %(r2)f
 --> ce phenomene numerique est frequent
 --> sur les premieres valeurs propres
 --> lorsque le spectre recherche est
 --> tres etendu (en pulsation)

"""),

52: _("""
 LAIGLE: Erreur
   - Non convergence a l'itération maxi : %(i1)d
   - Convergence irrégulière & erreur >   %(r1)f
   - Diminuer la taille d'incrément.
"""),

53: _("""
 Erreur de programmation MULT_FRONT (NUME_DDL / PREML0) :
   * Sur-connexion des Lagranges Lambda1
"""),

54: _("""
     ==== Type de maille Aster / Type de maille GMSH ====
"""),

55: _("""
    %(i1)d  éléments %(k1)s découpés en %(i2)d  éléments %(k2)s a %(i3)d noeuds
"""),


}
