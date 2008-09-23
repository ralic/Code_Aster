#@ MODIF mecanonline4 Messages  DATE 23/09/2008   AUTEUR ABBAS M.ABBAS 
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

cata_msg = {

3 : _("""
Il y a trop de colonnes de SUIVI_DDL (limité à quatre)
"""),

5 : _("""
Le fichier pour le SUIVI_DDL doit etre défini dans la première occurrence
"""),

6 : _("""
Le fichier pour le SUIVI_DDL a été donné sans unité logique
"""),

21 : _("""
Le format est trop grand pour la largeur max. d'une colonne (16)
"""),


35 : _("""
 On utilise MECA_NON_LINE en enrichissant les résultats (REUSE).
 Mais on ne définit pas d'état initial: on prend un état initial nul.
"""),

37 : _("""
 On ne trouve aucun numéro d'ordre pour le concept EVOl_NOLI de nom <%(k1)s> 
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
 Le concept EVOL_NOLI de nom <%(k1)s> n'est pas issu précédemment d'un calcul
 de type 'PROJ_MODAL'.
 On  suppose qu'on part d'un champ de déplacements/vitesses/accélérations généralisées nulles.
"""),

}
