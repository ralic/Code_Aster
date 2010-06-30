#@ MODIF arlequindebg2 Messages  DATE 30/06/2010   AUTEUR DELMAS J.DELMAS 

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
# RESPONSABLE DELMAS J.DELMAS

def _(x) : return x

cata_msg={


1: _("""
 <ARLEQUIN> *** CREATION DU PSEUDO-MAILLAGE...
"""),

2: _("""
 <ARLEQUIN> ... PSEUDO-MAILLAGE - NBR ELEMENTS A PRIORI          : %(i1)s
"""),

3: _("""
 <ARLEQUIN> ... PSEUDO-MAILLAGE - LONGUEUR CONNECTIVITES A PRIORI: %(i1)s
"""),

4: _("""
 <ARLEQUIN> ... PSEUDO-MAILLAGE - NBR NOEUDS   A PRIORI          : %(i1)s
"""),

5: _("""
 <ARLEQUIN> ... PSEUDO-MAILLAGE - SURESTIMATION NBRE MAILLES 
 <ARLEQUIN> ...... PREVU  : %(i1)s MAILLES
 <ARLEQUIN> ...... OBTENU : %(i2)s MAILLES
 <ARLEQUIN> ......... %(i3)s MAILLES 2 SUR 1
 <ARLEQUIN> ......... %(i4)s MAILLES 1 SUR 2
 <ARLEQUIN> ......... %(i5)s MAILLES DECOUPEES
"""),

6: _("""
 <ARLEQUIN> ... PSEUDO-MAILLAGE - SURESTIMATION NBRE NOEUDS
 <ARLEQUIN> ...... PREVU :  %(i1)s
 <ARLEQUIN> ...... OBTENU:  %(i2)s
 
"""),

7: _("""
 <ARLEQUIN> ... PSEUDO-MAILLAGE - SURESTIMATION LONGUEUR CUMULEE CONNECTIVITE 
 <ARLEQUIN> ...... PREVU :  %(i1)s
 <ARLEQUIN> ...... OBTENU:  %(i2)s
"""),

8: _("""
 <ARLEQUIN> ... CREATION DES GROUPES
"""),

9: _("""
 <ARLEQUIN> ... NBRE GROUPES: %(i1)s
"""),

10: _("""
 <ARLEQUIN> ... GROUPE INTEG. SUR M1 <INCLU1> - %(i1)s MAILLES
"""),

11: _("""
 <ARLEQUIN> ... GROUPE INTEG. SUR M2 <INCLU2> - %(i1)s MAILLES
"""),

12: _("""
 <ARLEQUIN> ... GROUPE INTEG. PAR SOUS-MAILLES <SOUS> - %(i1)s MAILLES
"""),

13: _("""
 <ARLEQUIN> ... IMPRESSION DU PSEUDO-MAILLAGE
"""),

14: _("""
 <ARLEQUIN> *** FIN DE CREATION DU PSEUDO-MAILLAGE
"""),

15: _("""
 <ARLEQUIN> CALCUL DES NORMALES POUR LES COQUES...
"""),

16: _("""
 <ARLEQUIN><ECH> *** ECHANTILLONNAGE DE LA FRONTIERE DE LA MAILLE %(k1)s
 <ARLEQUIN><ECH> ... TYPE MAILLE         : %(k2)s
 <ARLEQUIN><ECH> ... NBRE ECHANTILLONS   : %(i1)s
 <ARLEQUIN><ECH> ... DIMENSION  MAILLE   : %(i2)s
 <ARLEQUIN><ECH> ... NBRE SOMMETS MAILLE : %(i3)s
 <ARLEQUIN><ECH> ... NBRE ARETES  MAILLE : %(i4)s
"""),

17: _("""
 <ARLEQUIN><ECH> ... NBRE PANS    MAILLE : %(i1)s
"""),

18: _("""
 <ARLEQUIN><ECH> ... NBRE DE POINTS SUPPL. SUR ARETES  : %(i1)s
"""),

19: _("""
 <ARLEQUIN><ECH> ... LISTE DES POINTS 
"""),

20: _("""
 <ARLEQUIN><ECH> ...... POINT <%(i1)s> : %(r1)g %(r2)g
"""),

21: _("""
 <ARLEQUIN><ECH> ...... POINT <%(i1)s> : %(r1)g %(r2)g %(r3)g
"""),

22: _("""
 <ARLEQUIN><ECH> *** FIN ECHANTILLONNAGE 
"""),

23: _("""
 <ARLEQUIN><INT> *** CALCUL INTERSECTION DE DEUX POLYGONES 
"""),

24: _("""
 <ARLEQUIN><INT> ... NOUVEAUX SOMMETS ?
"""),

25: _("""
 <ARLEQUIN><INT> ...... PAS D'INTERSECTIONS DETECTEES
"""),

26: _("""
 <ARLEQUIN><INT> ...... %(i1)s INTERSECTIONS DETECTEES
"""),

27: _("""
 <ARLEQUIN><INT> ... CREATION NOUVELLES ARETES
"""),

28: _("""
 <ARLEQUIN><INT> ... CREATION GRAPHE SOMMET -> ARETES
"""),

29: _("""
  <ARLEQUIN><INT> ... NBRE COMPOSANTES CONNEXES: %(i1)s
  <ARLEQUIN><INT> *** FIN CALCUL INTERSECTION DE DEUX POLYGONES 
"""),

30: _("""
 <ARLEQUIN><INT> ...... Ajout du point <%(i1)s> : %(r1)g %(r2)g
"""),

31: _("""
 <ARLEQUIN><INT> ...... Ajout du point <%(i1)s> : %(r1)g %(r2)g %(r3)g
"""),

32: _("""
 <ARLEQUIN><APP> ... Le point %(i1)s est dans la boite de la maille %(k1)s
"""),

33: _("""
 <ARLEQUIN><APP> ... Le barycentre des points est dans la boite de la maille %(k1)s
"""),


}
