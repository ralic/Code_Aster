# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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

cata_msg = {

1 : _(u"""
 On utilise l'opérateur en enrichissant les résultats (REUSE).
 Mais on ne définit pas d'état initial: on prend un état initial nul.
"""),

3 : _(u"""
 L'instant spécifié sous ETAT_INIT/INST n'est pas trouvé dans la structure de données
 résultat de nom <%(k1)s>.
"""),

4 : _(u"""
 Il y a plusieurs instants dans la structure de données résultat de nom <%(k1)s> qui
 correspondent à celui spécifié sous ETAT_INIT/INIT.
"""),

5 : _(u"""
 A l'instant initial, tous les termes du bilan d'énergie sont nuls bien qu'un état
 initial non vierge soit renseigné. Le bilan d'énergie indique la variation des différents
 termes d'énergie entre deux instants de calcul consécutifs ainsi que leur variation
 totale entre l'instant courant et l'instant initial.
"""),

10 : _(u"""
 Lecture de l'état initial
"""),

20 : _(u"""
 Il n'y a pas d'état initial défini. On prend un état initial nul.
"""),

30 : _(u"""
  Le champ %(k1)s n'est pas trouvé dans ETAT_INIT et on ne sait pas l'initialiser à zéro.
"""),

31 : _(u"""  Le champ <%(k1)s> est initialisé a zéro"""),

32 : _(u"""  Le champ <%(k1)s> est lu dans ETAT_INIT dans la structure de données
résultats de nom <%(k2)s>"""),

33 : _(u"""  Le champ <%(k1)s> est lu dans ETAT_INIT, par un champ donné explicitement"""),

34 : _(u"""  Le champ de température initiale est calculé par un état stationnaire"""),

35 : _(u"""  Le champ de température initiale est donné par une valeur qui vaut %(r1)19.12e"""),

50 : _(u"""
  Le champ <%(k1)s> est d'un type inconnu.
"""),

51 : _(u"""  Le champ <%(k1)s> est de type <%(k2)s> mais on attend un champ de type <%(k3)s>.
On le convertit automatiquement"""),

52 : _(u"""
  Le champ <%(k1)s> est de type <%(k2)s> mais on attend un champ de type <%(k3)s>.
  On ne sait pas le convertir automatiquement
"""),

}
