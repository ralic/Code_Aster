#@ MODIF mecanonline4 Messages  DATE 21/02/2011   AUTEUR ABBAS M.ABBAS 
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
# RESPONSABLE DELMAS J.DELMAS

def _(x) : return x

cata_msg = {

3 : _("""
 Calcul des valeurs propres en grandes déformations
"""),

5 : _("""
Le fichier pour le SUIVI_DDL doit etre défini dans la première occurrence
"""),

6 : _("""
Le fichier pour le SUIVI_DDL a été donné sans unité logique
"""),

14 : _("""
 Vous utilisez la méthode CONTINUE pour le traitement du contact et faites une reprise de calcul (mot-clé REUSE). L'état initial de contact sera
 non contactant sauf si vous avez utilisé le mot-clé CONTACT_INIT.
 Cela peut entraîner des difficultés de convergence en présence de fortes non-linéarités. En présence de frottement, la solution peut bifurquer
 différemment.
 Conseils :
   Si vous le pouvez, faites votre calcul en une seule fois.
"""),

15 : _("""
 Vous utilisez la méthode CONTINUE pour le traitement du contact et définissez un état initial via le mot-clé ETAT_INIT. L'état initial de contact
 sera non-contactant sauf si vous avez utilisé le mot-clé CONTACT_INIT.
"""),

22 : _("""
 L'etat initial n'appartient pas à un EVOL_NOLI :
 on suppose qu'on part d'un état a vitesses nulles
"""),

41 : _("""
 Le champ des déplacements (ou sa dérivée pour la sensibilité) n'a pas été trouvé 
 dans le concept EVOL_NOLI de nom <%(k1)s>
"""),

42 : _("""
 Le champ des contraintes (ou sa dérivée pour la sensibilité) n'a pas été trouvé 
 dans le concept EVOL_NOLI de nom <%(k1)s>
"""),

43 : _("""
 Le champ des vitesses (ou sa dérivée pour la sensibilité) n'a pas été trouvé 
 dans le concept EVOL_NOLI de nom <%(k1)s>
 On suppose qu'on part d'un champ de vitesses nulles.
"""),

44 : _("""
 Le champ des accélérations (ou sa dérivée pour la sensibilité) n'a pas été trouvé 
 dans le concept EVOL_NOLI de nom <%(k1)s>
 On calcule un champ d'accélérations initiales, ce qui est possible puisque les vitesses initiales sont nulles
"""),

45 : _("""
 Le champ des accélérations (ou sa dérivée pour la sensibilité) n'a pas été trouvé 
 dans le concept EVOL_NOLI de nom <%(k1)s>
 On ne peut pas calculer un champ d'accélérations initiales, car les vitesses initiales ne sont pas nulles
"""),

46 : _("""
 Le champ des variables internes (ou sa dérivée pour la sensibilité) n'a pas été trouvé 
 dans le concept EVOL_NOLI de nom <%(k1)s>
"""),

47 : _("""
 Vous faites une reprise de calcul avec PILOTAGE en longueur d'arc et avec l'option ANGL_INCR_DEPL mais il n'y pas assez d'informations dans
 la structure de données résultat. Il vous faut en effet au moins les deux derniers champs déplacements solutions.
 Changer l'option de PILOTAGE (utilisez NORM_INCR_DEPL) ou refaites le premier calcul pour enrichir la structure de données résultat (modifiez vos options d'ARCHIVAGE).
"""),

}
