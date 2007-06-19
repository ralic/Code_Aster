#@ MODIF algeline5 Messages  DATE 19/06/2007   AUTEUR LEBOUVIER F.LEBOUVIER 
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

}
