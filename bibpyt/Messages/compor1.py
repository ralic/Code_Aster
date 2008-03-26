#@ MODIF compor1 Messages  DATE 25/03/2008   AUTEUR PROIX J-M.PROIX 
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

1: _("""
 HUJPLA :: nombre de variables internes incorrect:
           NVI > NVIMAX
"""),

2: _("""
 HUJDDD :: on ne calcule pas DPSIDS pour K=4.
           - vérifiez la programmation -
"""),

3: _("""
 CAM_CLAY :: le coefficient de poisson et ou le module d'Young ne sont pas corrects
             dans la maille %(k1)s
             
             *** vérifiez la cohérence des données mécaniques suivantes :
                 E, nu, eO (indice des vides), kapa
                 (contrainte volumique initiale)

                 il faut notamment vérifier ceci:
               
                 0. <NU = (3*(1+e0)*P- - 2*kapa*mu)/
                          (6*(1+e0)*P- + 2*kapa*mu) <= 0.5 
                          
                          et E >0 ***
"""),

4: _("""
 HUJEUX :: les modélisations autorisées sont 3D D_PLAN ou AXIS
"""),

5: _("""
 HUJEUX :: K différent de NBMECA pour le mécanisme isotrope
           - vérifiez la programmation -
"""),

6: _("""
 HUJEUX :: erreur inversion par pivot de Gauss
"""),

7: _("""
 HUJCRI :: EPSI_VP est trop grand:
           l'exponentielle explose
"""),

8: _("""
 HUJEUX :: mécanisme indéterminé
           - vérifiez la programmation -
"""),

9 : _("""
Arret suite à l'échec de l'intégration de la loi de comportement.
"""),

10: _("""
 HUJKSI :: mot-clé inconnu
"""),

11: _("""
 HUJNVI :: modélisation inconnue
"""),

12: _("""
 HUJCI1 :: l'incrément de déformation est nul:
           on ne peut pas trouver le zéro de la fonction.
"""),

13: _("""
 HUJCI1 :: le critère d'existence du zéro de la fonction est violé:
           on recommande soit de changer les données matériaux,
           soit de raffiner le pas de temps.
           
           Ce critère est :
           
           2 x P- < P0 * (P0 /K0 /TRACE(DEPS_ELA) /N)**(1-N)
"""),

14: _("""
 HUJTID :: erreur dans le calcul de la matrice tangente
"""),

15: _("""
 NMCOMP :: la loi élastique n'est plus disponible directement avec SIMO_MIEHE : utilisez 
VMIS_ISOT_LINE avec un SY grand
"""),

16 : _("""
Arret suite à l'échec de l'intégration de la loi de comportement.

Erreur numérique (overflow) : la plasticité cumulée devient très grande.
"""),

17 : _("""
  HUJCI1 :: Soit le zéro n'existe pas, soit il se trouve hors des
            bornes admissibles.
"""),

18 : _("""
  HUJCI1 :: Cas de traction à l'instant moins.
"""),

19 : _("""
  MONOCRISTAL :: écrouissage cinematique non trouvé.
"""),

20 : _("""
  MONOCRISTAL :: écoulement non trouvé.
"""),

21 : _("""
  MONOCRISTAL :: écrouissage isotrope non trouvé.
"""),

22 : _("""
  MONOCRISTAL :: nombre de matrice d'interaction trop grand.
"""),

23 : _("""
  MONOCRISTAL :: la matrice d'interaction est définie avec 
  4 coefficients. Ceci n'est applicable qu'avec 24 systèmes de
  glissement (famille BCC24).
"""),

24 : _("""
  MONOCRISTAL :: la matrice d'interaction est définie avec 
  6 coefficients. Ceci n'est applicable qu'avec 12 systèmes de
  glissement.
"""),

25 : _("""
  MONOCRISTAL :: la matrice d'interaction est définie avec 
  un nombre de coefficients incorrect :: il en faut 1, ou 4, ou 6.
"""),


26: _("""
 lklmat :: paramètres de la loi LETK non cohérents 
"""),
27: _("""
 lkcomp :: Réduire le pas de temps peut dans certains cas remédier au problème.
 Le critère visqueux max en ce point de charge n'est 
 pas défini. Le calcul de la distance au critère n'est pas fait.
"""),
28: _("""
 lkcomp :: Réduire le pas de temps peut dans certains cas remédier au problème.
 Le critère visqueux en ce point de charge n'est pas défini. 
 Le calcul de la distance au critère n'est pas fait.
"""),

29: _("""
 lkdhds :: division par zéro - entrée rapide en plasticité avec un déviateur nul. 
 réduire le pas de temps.
"""),

30: _("""
 lkds2h :: division par zéro - entrée rapide en plasticité avec un déviateur nul. 
 réduire le pas de temps.
"""),

31: _("""
 lkcaln :: division par zéro - entrée rapide en plasticité avec un déviateur nul. 
 réduire le pas de temps.
"""),

32: _("""
 VISC_CINx_CHAB :: pour la viscosité, renseigner le mot-clé LEMAITRE dans DEFI_MATERIAU. 
 Si vous voulez seulement de l'élastoplasticité, il faut utiliser VMIS_CINx_CHAB.
"""),

}
