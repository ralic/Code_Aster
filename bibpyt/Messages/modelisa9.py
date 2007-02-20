#@ MODIF modelisa9 Messages  DATE 20/02/2007   AUTEUR LEBOUVIER F.LEBOUVIER 

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
 il manque le parametre  %(k1)s dans la table %(k2)s 
 .sa presence est indispensable a la  creation d'un champ elementaire. %(k3)s 
"""),

2: _("""
 le parametre  %(k1)s de la table %(k2)s 
 est incompatible a la  creation d'un  champ elementaire constant. %(k3)s 
"""),

3: _("""
 il manque le parametre  %(k1)s dans la table %(k2)s 
 .sa presence est indispensable a la  creation d'un champ  %(k3)s 
"""),

4: _("""
 le parametre  %(k1)s de la table %(k2)s 
 n'est valable que pour la  creation d'un champ  %(k3)s 
"""),

5: _("""
 incoherence entre maille et point dans la table %(k1)s maille : %(k2)s 
 point  : %(i1)d 
 nombre de points de la maille: %(i2)d 
"""),

6: _("""
 plusieurs affectations  pour le meme point d'une maille
  dans la table %(k1)s 
 maille: %(k2)s 
 point : %(i1)d 
"""),

7: _("""
 plusieurs affectations  pour le meme sous-point dans la table %(k1)s 
 maille: %(k2)s 
 point : %(i1)d 
 sous-point : %(i2)d 
"""),

8: _("""
 plusieurs affectations  pour la meme maille dans la table %(k1)s 
 maille: %(k2)s 
"""),

9: _("""
 trop de noeuds dans le group_no  noeud utilise:  %(k1)s 
"""),

10: _("""
 trop de noeuds dans le group_no  noeud utilise:  %(k1)s 
"""),

11: _("""
 mocle facteur non traite :mclf %(k1)s 
"""),

}
