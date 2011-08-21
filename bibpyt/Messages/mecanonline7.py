#@ MODIF mecanonline7 Messages  DATE 22/08/2011   AUTEUR ABBAS M.ABBAS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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


# Attention a ne pas faire de retour à la ligne !

def _(x) : return x

cata_msg = {

1 : _("""
  Temps CPU consommé dans ce pas de temps: %(k1)s.
"""),

2 : _("""    * Temps moyen par itération de Newton : %(k1)s ( %(i1)d itérations     )"""),

3 : _("""    * Temps total factorisation matrice   : %(k1)s ( %(i1)d factorisations )"""),

4 : _("""    * Temps total intégration LDC         : %(k1)s ( %(i1)d intégrations   )"""),

5 : _("""    * Temps total résolution K.U=F        : %(k1)s ( %(i1)d résolutions    )"""),

6 : _("""    * Temps autres opérations             : %(k1)s"""),

7 : _("""    * Temps post-traitement (flambement)  : %(k1)s"""),

8 : _("""    * Nombre d'itérations de recherche linéaire   : %(i1)d"""),

9 : _("""    * Nombre d'itérations du solveur FETI         : %(i1)d"""),

10 : _("""    * Temps résolution contact            : %(k1)s ( %(i1)d itérations     )"""),

11 : _("""    * Temps appariement contact           : %(k1)s ( %(i1)d appariements   )"""),

12 : _("""    * Temps construction second membre    : %(k1)s"""),

13 : _("""    * Temps assemblage matrice            : %(k1)s"""),

20 : _("""    * Temps construction matrices contact : %(k1)s ( %(i1)d constructions  )"""),

22 : _("""    * Temps frottement                    : %(k1)s ( %(i1)d boucles        )"""),

23 : _("""    * Temps contact                       : %(k1)s ( %(i1)d boucles        )"""),

24 : _("""    * Temps préparation données contact   : %(k1)s ( %(i1)d préparations   )"""),

30 :_("""
  Statistiques du contact dans ce pas de temps.
"""),

31 : _("""    * Nombre de liaisons de contact       : %(i1)d"""),

32 : _("""    * Nombre de liaisons de frottement    : %(i1)d"""),

}
