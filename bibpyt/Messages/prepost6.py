#@ MODIF prepost6 Messages  DATE 06/08/2007   AUTEUR LEBOUVIER F.LEBOUVIER 
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
 
"""),

2: _("""
 fichier med :  %(k1)s erreur efferm numero  %(i1)d 
"""),

3: _("""
 le volume differe du volume use mais le nombre d'iteration
  est superieur a  %(i1)d 
      volume use:  %(r1)f 
  volume calcule:  %(r2)f 
"""),

4: _("""
 verifier les parametres d'usure pour le secteur  %(i1)d 
"""),

5: _("""
 verifier les parametres d'usure pour le secteur  %(i1)d 
"""),

6: _("""
 composante %(k1)s / point  %(i1)d 
"""),

7: _("""
   nombre de valeurs        =  %(i1)d    
     %(r1)f, %(r2)f, ... 
"""),

8: _("""
   nombre de pics extraits   =  %(i1)d
     %(r1)f, %(r2)f, ... 
"""),

9: _("""
   nombre de cycles detectes =  %(i1)d 
"""),

10: _("""
   %(i1)d  /  %(r1)f   %(r2)f 
"""),

11: _("""
   dommage en ce point/cmp  =  %(r1)f 
"""),

12: _("""
 fichier med :  %(k1)s champ :  %(k2)s erreur effoco numero  %(i1)d 
"""),

13: _("""
 le fichier n'a pas ete construit avec la meme version de med.
 erreur efveco numero  %(i1)d 
"""),

14: _("""
 version de la bibliotheque med utilisee par code_aster: %(i1)d %(i2)d %(i3)d 
"""),

15: _("""
 version de la bibliotheque med qui a cree le fichier   : < 2.1.5
"""),

16: _("""
 version de la bibliotheque med pour creer le fichier  : %(i1)d %(i2)d %(i3)d
"""),

17: _("""
 un utilitaire vous permet peut-etre de convertir votre fichier (medimport)
"""),

18: _("""
 
"""),

19: _("""
 fichier med :  %(k1)s champ :  %(k2)s erreur efouvr numero  %(i1)d 
"""),

20: _("""
 fichier med :  %(k1)s champ :  %(k2)s instant voulu :  %(r1)f 
 typent :  %(i1)d 
 typgeo :  %(i2)d 
 
"""),

21: _("""
 champ a lire :  %(k1)s typent :  %(i1)d typgeo :  %(i2)d 
 instant voulu :  %(r1)f 
 --> numero d ordre :  %(i3)d 
 --> numero de pas de temps :  %(i4)d 
 
"""),

22: _("""
 fichier med :  %(k1)s champ :  %(k2)s 
"""),

23: _("""
 instant voulu :  %(r1)f 
"""),

24: _("""
 numero d ordre :  %(i1)d numero de pas de temps :  %(i2)d 
"""),

25: _("""
 
"""),

26: _("""
 fichier med :  %(k1)s champ :  %(k2)s erreur efferm numero  %(i1)d 
"""),

27: _("""
 parametres de calcul du dommagenombre de numeros d'ordre  =  %(i1)d 
 nombre de points de calcul =  %(i2)d 
"""),

28: _("""
 calcul     du      dommage en %(k1)s points  de   calcul  du    dommage %(k2)s 
 composante(s) grandeur equivalente %(k3)s 
 methode  d'extraction  des    pics %(k4)s 
 methode  de  comptage  des  cycles %(k5)s 
 methode  de  calcul    du  dommage %(k6)s 
 
"""),

29: _("""
 maille:  %(k1)s 
"""),

30: _("""
 des mailles de peau ne s'appuient sur aucune maille support
    maille:  %(k1)s 
"""),

31: _("""

     ===== GROUP_MA ASTER / PHYSICAL GMSH =====

"""),

32: _("""

  Le GROUP_MA GMSH GM10000 contient %(i1)d éléments :
"""),

33: _("""
       %(i1)d éléments de type %(k1)s
"""),

}
