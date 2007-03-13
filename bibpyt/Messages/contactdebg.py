#@ MODIF contactdebg Messages  DATE 13/03/2007   AUTEUR ABBAS M.ABBAS 
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
# RESPONSABLE ABBAS M.ABBAS

def _(x) : return x

cata_msg={


1: _("""
 <CONTACT_2> Reactualisation de l'appariement.  
"""),

2: _("""
 <CONTACT_2> Reactualisation de la geometrie.  
"""),

3: _("""
 <CONTACT_2> Appariement des surfaces.  
"""),

4: _("""
 <CONTACT_2> Pas de reactualisation de l'appariement.  
"""),

5: _("""
 <CONTACT_2> Sauvegarde des anciennes donnees d'appariement.  
"""),

6: _("""
 <CONTACT_2> Calcul des normales sur tous les noeuds (maitres et esclaves).  
"""),

7: _("""
 <CONTACT_2> Zone de contact <%(i1)s> - Pas de reactualisation de l'appariement sur cette zone.
 <CONTACT_2>   Nombre de fois ou l'appariement est reste fixe sur cette zone: <%(i2)s>     
"""),

8: _("""
 <CONTACT_2> Zone de contact <%(i1)s> - Reactualisation de l'appariement sur cette zone.
"""),

9: _("""
 <CONTACT_2> Restitution des anciennes donnees d'appariement.  
"""),

10: _("""
 <CONTACT_2> Zone de contact <%(i1)s> - Appariement de type nodal.
"""),

11: _("""
 <CONTACT_2> Le noeud <%(k1)s> n'est pas apparie car il aurait ete projete hors de la zone de tolerance de la maille <%(k2)s> qui etait la plus proche. 
 <CONTACT_2> Vous pouvez eventuellement modifier TOLE_PROJ_EXT ou revoir la definition de vos zones esclaves et maitres. 
"""),

12: _("""
 <CONTACT_2> Le noeud <%(k1)s> n'est pas apparie car il risque de provoquer des pivots nuls (appariement MAIT_ESCL_SYME).  
"""),

13: _("""
 <CONTACT_2> Le noeud <%(k1)s> n'est pas apparie car il appartient a SANS_NOEUD ou SANS_GROUP_NO.  
"""),

14: _("""
 <CONTACT_2> Zone de contact <%(i1)s> - Appariement de type maitre/esclave.
"""),

15: _("""
 <CONTACT_2> Appariement de type maitre/esclave - Recherche du noeud maitre le plus proche du noeud esclave. 
"""),

16: _("""
 <CONTACT_2> Appariement de type maitre/esclave - Recherche de la maille maitre la plus proche du noeud esclave. 
"""),

17: _("""
 <CONTACT_2> Appariement de type maitre/esclave - Projection. 
"""),

18: _("""
 <CONTACT_2> Debut du traitement des conditions de contact. 
"""),

19: _("""
 <CONTACT_2> Fin du traitement des conditions de contact. 
"""),

20: _("""
 <CONTACT_2> Temps CPU passe dans la geometrie : %(r1)g secondes. 
"""),

21: _("""
 <CONTACT_2> Temps CPU passe dans l'algorithme : %(r1)g secondes. 
"""),

22: _("""
 <CONTACT_2> Creation de la structure de donnees pour le contact. 
"""),

23: _("""
 <CONTACT_2> Appariement de type maitre/esclave - Creation de l'objet pour appariement symetrique. 
"""),

98: _("""
 <CONTACT> Erreur fatale <%(i1)s> dans <%(k1)s>: contacter les developpeurs
"""),

99: _("""
 <CONTACT_2> Message inconnu lors du debuggage : contacter les developpeurs
"""),


}
