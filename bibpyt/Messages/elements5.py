#@ MODIF elements5 Messages  DATE 04/04/2007   AUTEUR ABBAS M.ABBAS 
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
 la contrainte equivalente est nulle pour la maille  %(k1)s 
"""),

2: _("""
 tuyau : le nombre de couches est limite a  %(i1)d 
"""),

3: _("""
 tuyau : le nombre de secteurs est limite a  %(i1)d 
"""),

4: _("""
 tuyau : le nombre de couches est limite a  %(i1)d 
"""),

5: _("""
 tuyau : le nombre de secteurs est limite a  %(i1)d 
"""),

6: _("""
 tuyau : le nombre de couches est limite a  %(i1)d 
"""),

7: _("""
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
26: _("""
  le LIAISON_** du chargement %(k1)s impliquant les noeuds %(k2)s et %(k3)s traverse l'interface 
"""),
27: _("""
  le LIAISON_** du chargement %(k1)s impliquant le noeud %(k2)s touche l'interface 
"""),
28: _("""
  le modele comporte %(i1)d mailles de plus que l'ensemble des sous-domaines 
"""),
29: _("""
  le modele comporte %(i1)d mailles de moins que l'ensemble des sous-domaines 
"""),
}
