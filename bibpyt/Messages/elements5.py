#@ MODIF elements5 Messages  DATE 19/06/2007   AUTEUR LEBOUVIER F.LEBOUVIER 
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







2: _("""
 tuyau : le nombre de couches est limite a  %(i1)d 
"""),

3: _("""
 tuyau : le nombre de secteurs est limite a  %(i1)d 
"""),





























8: _("""
 Vous voulez utiliser l'indicateur de convergence RESI_REFE_RELA mais vous n'avez pas
 renseigné le mot-clé %(k1)s .
"""),

9: _("""
 Employez la modélisation spécifique aux grandes déformations XX_INCO_GD
"""), 

10: _("""
 La modélisation GRAD_VARI n'est plus disponible en grandes déformations. Pour Rousselier
 version SIMO_MIEHE, vous pouvez faire du non-local en utilisant la modélisation XX_INCO_GD
 et en définissant C_GONF<>0 sous l'opérande NON_LOCAL de DEFI_MATERIAU
"""), 

11: _("""
 Le rayon R_SUP (ou R_SUP_FO) doit obligatoirement etre supérieur au rayon R_INF 
 (resp. R_INF_FO).
"""), 

12: _("""
 Le noeud %(k1)s du fond de fissure n est rattaché à aucune maille surfacique 
 de la lèvre supérieure : vérifier les groupes de mailles.
"""), 

13: _("""
 Le noeud %(k1)s du fond de fissure n est rattaché à aucune maille surfacique 
 de la lèvre inférieure : vérifier les groupes de mailles.
"""), 

14: _("""
 Les noeuds %(k1)s de FOND_INF et %(k2)s de FOND_SUP ne sont pas en vis à vis. 
"""), 

15: _("""
 FONFIS - occurence %(i1)s : les objets précédemment évoqués sont inexistants
 ou de type incompatible.
"""), 

16: _("""
 FONFIS - occurence %(i1)s : les mailles spécifiées ne permettent pas de définir 
 une ligne continue.
 Conseil (si opérateur DEFI_FOND_FISS) : vérifier le groupe de maille du fond de fissure.
"""), 

17: _("""
 FONFIS - Trop de noeuds dans le groupe de noeuds %(k1)s.
 --> Noeud utilisé : %(k2)s
"""), 

18: _("""
 FONFIS - Trop de mailles dans le groupe de mailles GROUP_MA_ORIG.
 --> Maille utilisée : %(k1)s
"""), 

19: _("""
 FONFIS - Occurence %(i1)s : maille %(k1)s inexistante.
"""), 

20: _("""
 FONFIS - Occurence %(i1)s : maille %(k1)s non linéique.
"""), 

21: _("""
 FONFIS - Occurence %(i1)s : mélange de SEG2 et de SEG3 (maille %(k1)s).
"""), 

22: _("""
   Erreur, le nombre de noeuds d'un element de joint 3D n'est pas correct   
"""),


23: _("""
   Erreur, le nombre de points de Gauss d'un element de joint 3D n'est pas correct   
"""),

24: _("""
  le nombre de mailles du modele %(i1)d est différent de la somme des mailles des sous-domaines %(i2)d 
"""),
25: _("""
  le sous-domaine n %(i1)d n'est pas renseigné ou vide dans DEFI_PART_OPS
"""),








28: _("""
  le modele comporte %(i1)d mailles de plus que l'ensemble des sous-domaines 
"""),
29: _("""
  le modele comporte %(i1)d mailles de moins que l'ensemble des sous-domaines 
"""),

30: _("""
 jacobien negatif ou nul : jacobien =  %(r1)f 
"""),

31: _("""
 jacobien negatif ou nul : jacobien =  %(r1)f 
"""),

32: _("""
  Toute méthode de contact autre que la méthode continue est proscrite avec
  FETI! En effet cette dernière méthode est basée sur un vision maille/calcul
  élémentaire et non pas sur une approche globale discrète dont le flot de
  données est plus difficilement dissociable par sous-domaine.
  Merci, d'activer donc toutes les zones de contact avec ladite méthode. 
"""),

33: _("""
  Avec FETI, on ne peut mélanger dans une seul AFFE_CHAR_MECA, du contact
  avec des chargements à LIGREL tardif (Dirichlet, Force Nodale...).
  Merci, de dissocier les types de chargement par AFFE_CHAR_MECA.
"""),
34: _("""
  Contact méthode continue avec FETI: la maille %(i1)d de la zone %(i2)d
  du chargement %(i3)d , semble etre à cheval entre les sous-domaines
  %(i4)d et %(i5)d !
  Solution paliative: il faut forcer le partitionnement à ne pas couper
  cette zone de contact ou essayer de la dédoubler en deux zones.
"""),

35: _("""
  Contact méthode continue avec FETI: la surface %(i1)d de la zone %(i2)d
  du chargement %(i3)d n'est portée par aucun sous-domaine !
"""),

36: _("""
  Contact méthode continue avec FETI: le noeud %(i1)d est présent plusieurs
  fois dans la zone de contact %(i2)d . Cela ne devrait pas etre un problème
  pour l'algorithme, mais ce n'est pas une modélisation du contact très
  orthodoxe !
"""),

37: _("""
  Contact méthode continue avec FETI: le noeud %(i1)d est a l'intersection de
  plusieurs zones de contact. Cela va probablement générer un problème dans
  l'algorithme de contact (pivot nul) !
"""),

38: _("""
  Contact méthode continue avec FETI: le noeud %(i1)d de la zone de contact
  %(i2)d est aussi sur l'interface FETI ! Pour l'instant ce cas de figure
  est proscrit. Essayer de l'enlevez de la zone de contact ou de reconfigurer
  vos sous-domaines.
"""),
}
