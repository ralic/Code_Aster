#@ MODIF rupture1 Messages  DATE 22/01/2008   AUTEUR REZETTE C.REZETTE 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
1: _("""
L'option de lissage 'LAGRANGE_REGU' n'a pas été développée lorsque
le nombre de noeuds d'un fond de fissure fermé est pair.
-> Risque et Conseil :
Veuillez utiliser une autre option de lissage
(par exemple, le lissage 'LAGRANGE' pour le champ theta)
"""),

2: _("""
%(k1)s n'est pas un GROUP_NO ou un GROUP_MA.
"""),

6: _("""
Le rayon R_SUP (ou R_SUP_FO) doit obligatoirement etre supérieur au rayon
R_INF (resp. R_INF_FO).
-> Risque et Conseil :
Veuillez revoir votre mise en données.
"""),

7: _("""
Problème dans RINF et RSUP.
-> Risque et Conseil :
Veuillez revoir les valeurs fournies aux mots-clés R_INF ou R_INF_FO 
ou R_SUP ou R_SUP_FO.
"""),

8: _("""
Le groupe %(k1)s n'appartient pas au maillage : %(k2)s
"""),

9: _("""
Le fond de fissure n'est pas complet.
-> Risque et Conseil :
Veuillez revoir la mise en données de l'opérateur DEFI_FOND_FISS.
"""),

10: _("""
Le fond de fissure ne doit etre défini que par un noeud.
-> Risque et Conseil :
Veuillez revoir le contenu du mot-clé GROUP_NO ou NOEUD ou FOND_FISS.
"""),

11: _("""
Il faut un mot clé parmis FOND_FISS ou FISSURE pour l'option %(k1)s
Veuillez le renseigner.
"""),

12: _("""
Aucun champ initial trouvé.
"""),

13: _("""
%(k1)s : champ initial impossible avec cette option.
"""),

14: _("""
Nombre de bornes erroné.
-> Risque et Conseil :
On doit en avoir autant que de numéros d'ordre.
"""),

15: _("""
Le résultat n'est pas un EVOL_NOLI.
"""),

16: _("""
Le champ %(k1)s n'a pas été trouvé.
"""),

17: _("""
L'association: lissage du champ THETA par les polynomes de Lagrange
               lissage de G autre que par des polynomes de Lagrange
n'est pas possible.
-> Risque et Conseil :
Veuillez consulter la documentation U4.82.03 pour déterminer une
association satisfaisante.
"""),

19: _("""
Il faut définir la normale au fond de fissure.
-> Risque et Conseil :
Veuillez revoir la mise en données de l'opérateur DEFI_FOND_FISS
(mot-clé NORMALE).
"""),

20: _("""
Une déformation initiale est présente dans la charge. Ceci est incompatible 
avec la contrainte initiale sigma_init.
-> Risque et Conseil :
On ne peut pas faire de calcul en introduisant simultanément une contrainte
initiale ET une déformation initiale. Veuillez revoir les données.
"""),

21: _("""
Seule une loi de comportement élastique isotrope est valide pour
le calcul de DG.
"""),

22: _("""
Le calcul de DG n'a pas été étendu à la plasticité !
"""),

23: _("""
CALC_G - option CALC_G : détection de chargements non nuls sur l'axe, 
le calcul est impossible.
-> Risque et Conseil :
En 2D-axi, le calcul de G n'est pas possible pour les éléments de l'axe de
symétrie si un chargement est imposé sur ceux-ci.
Modifier les couronnes R_INF et R_SUP pour qu'elles soient toutes les deux
plus petites que le rayon du fond de fissure. 
"""),

24: _("""
L'option CALC_K_G est incompatible avec les comportements incrémentaux,
avec les comportements non linéaires et avec la déformation GREEN.
"""),

25: _("""
Il faut affecter les éléments de bord (E et NU) pour le calcul des fic.
-> Risque et Conseil :
Veuillez revoir la mise en données des opérateurs DEFI_MATERIAU
et AFFE_MATERIAU.
"""),

26: _("""
La masse volumique RHO n'a pas été définie.
-> Risque et Conseil :
Pour l'option K_G_MODA, il est indispensable de fournir la masse volumique
du matériau considéré. Veuillez revoir la mise en données de l'opérateur
DEFI_MATERIAU.
"""),

27: _("""
L'option est incompatible avec les comportements incrémentaux ainsi
qu'avec la déformation Green.
"""),

28: _("""
Le champ de nom symbolique %(k1)s existe déjà dans la SD RESULTAT  %(k1)s.
"""),

29: _("""
Arret sur erreur(s) 'utilisateur': deux mailles du fond de fissure sont
non consécutives dans la numérotation des noeuds.
-> Risque et Conseil :
Veuillez revoir l'ordre des mailles fournies au mot-clé MAILLE.
"""),

30: _("""
Il faut donner 3 composantes de la direction.
-> Risque et Conseil :
Si vous utilisez CALC_THETA/THETA_2D ou CALG_G/THETA en 2d, veuillez fournir
une valeur nulle pour la 3eme composante.
"""),

31: _("""
Option non opérationnelle:
Seule l'option COURONNE est à utiliser dans le cas ou
on emploie le mot clé THETA_3D ou THETA_2D.
"""),

32: _("""
Option inexistante:
Seule l'option BANDE est à utiliser dans le cas ou on emploie
le mot clé THETA_BANDE .
"""),

}
