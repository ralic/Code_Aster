#@ MODIF xfem2 Messages  DATE 16/10/2007   AUTEUR REZETTE C.REZETTE 
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

1 : _("""
     Point : %(r1)f %(r2)f
  """),

2 : _("""
  -> Seules les modélisations C_PLAN/D_PLAN sont disponibles pour XFEM.
  -> Risques et conseils:
     Veuillez considérer l'une des deux modélisations dans AFFE_MODELE.
"""),

3 : _("""
  -> Pour le traitement du contact avec X-FEM, le solveur mumps est vivement
     recommandé.
  -> Risque & Conseil:
     Pour XFEM, la matrice produite est parfois non définie positive. 
     Seul un solveur qui pivote peut alors résoudre le problème.
     Il est donc préférable d'utiliser MUMPS (mot-clef SOLVEUR) pour ne pas finir
     en erreur du type "PIVOT NUL" avec le solveur MULT_FRONT ou LDLT.
"""),

4 : _("""
  -> Le type de formulation du contact (DISCRET/CONTINUE/XFEM) doit etre le meme pour
     toutes les zones de contact.
  -> Risque & Conseil:
     Veuillez revoir la mise en données de AFFE_CHAR_MECA/CONTACT.
"""),

5 : _("""
  -> Le vecteur TAU1 correspondant à la première direction du frottement dans
     l'élément XFEM est nul. Ceci signifie que les gradients des level sets
     sont surement colinéaires en ce point.
"""),

6 : _("""
     Multifissuration interdite avec l'opérateur PROPA_XFEM.
"""),

7 : _("""
  -> Le contact a été activé dans XFEM (CONTACT_XFEM='OUI' dans MODI_MODELE_XFEM)
  -> Risque & Conseil:
     Vous devez également l'activer dans AFFE_CHAR_MECA/CONTACT_XFEM
"""),

8 : _("""
  -> Le modèle %(k1)s transmis dans AFFE_CHAR_MECA/CONTACT n'est pas un modèle
     XFEM. 
  -> Risque & Conseil:
     Veuillez utiliser la commande MODI_MODELE_XFEM pour fournir à 
     AFFE_CHAR_MECA/CONTACT un modèle XFEM.
"""),

9 : _("""
  -> Le modèle %(k1)s transmis dans AFFE_CHAR_MECA/CONTACT n'est pas un modèle
     XFEM avec contact.
  -> Risque & Conseil:
     Veuillez activer CONTACT='OUI' dans MODI_MODELE_XFEM.
"""),

10 : _("""
  -> Le modèle %(k1)s transmis dans MECA_NON_LINE n'est pas un modèle
     XFEM. 
  -> Risque & Conseil:
     Veuillez utiliser la commande MODI_MODELE_XFEM pour fournir à 
     MECA_NON_LINE un modèle XFEM.
"""),

11 : _("""
  -> Le modèle %(k1)s transmis dans AFFE_CHAR_MECA/CONTACT_XFEM n'est pas 
     le modèle XFEM utilisé dans le AFFE_CHAR_MECA/CONTACT nommé %(k2)s.
  -> Risque & Conseil:
     Risques de résultats faux.
"""),

12 : _("""
  -> Le modèle %(k1)s transmis dans AFFE_CHAR_MECA/CONTACT_XFEM n'est pas un modèle
     XFEM. 
  -> Risque & Conseil:
     Veuillez utiliser la commande MODI_MODELE_XFEM pour fournir à 
     AFFE_CHAR_MECA/CONTACT_XFEM un modèle XFEM.
"""),

13 : _("""
     Point : %(r1)f %(r2)f %(r3)f
  """),

14 : _("""
  -> La discrétisation du fond de fissure est grossière par rapport à la 
     courbure du fond de fissure.
  -> Risque & Conseil:
     - possibilité de résultats faux
     - il faudrait raffiner le maillage autour du fond de fissure.
"""),

15 : _("""
  -> Point de FOND_FISS sans maille de surface rattachée.
  -> Risque & Conseil:
     Veuillez revoir la définition des level sets.
"""),

16 : _("""
  -> Problème dans l'orientation des normales a fond_fiss.
  -> Risque & Conseil: 
     Veuillez vérifier la continuité des mailles de FOND_FISS
"""),

17 : _("""
  -> Segment de fond_fiss sans maille de surface rattachée
  -> Risque & Conseil:
     Veuillez revoir la définition des level sets.
"""),

18 : _("""
  -> Lors de l'enregistrement du champ d'archivage du contact, il s'est avéré
     que les valeurs de contact au noeud %(k1)s différents selon la maille sur
     laquelle se trouve ce noeud.
  -> Risque & Conseil :
     Ce message est normal si le contact est activé sur la fissure.
"""),

19 : _("""
  -> Lors de l'orientation des points du fond de fissure, le point du fond de
     fissure initial (PFON_INI) est trop loin du fond de fissure.
  -> Risque & Conseil :
     Le point initial qui en résulte amène surement à une orientation du fond
     de fissure erronée.
     Veuillez redéfinir le point du fond de fissure initial (mot clé PFON_INI).
"""),

20 : _("""
  -> PFON_INI = PT_ORIGINE
  -> Risque & Conseil :
     Veuillez définir deux points différents pour PFON_INI et PT_ORIGINE.
"""),

21 : _("""
  -> Problème dans l'orientation du fond de fissure : PT_ORIGIN mal choisi.
  -> Risque & Conseil : 
     Veuillez redéfinir PT_ORIGIN.
"""),

22 : _("""
  -> Tous les points du fond de fissure sont des points de bord.
  -> Risque & Conseil : 
     Assurez-vous du bon choix des paramètres d'orientation de fissure
     et de PFON_INI.
"""),

23 : _("""
  -> PFON_INI semble etre un point de fin de fissure selon l'orientation choisie.
  -> Risque & Conseil : 
     Veuillez vous assurez du bon choix de PFON_INI
"""),

24 : _("""
  -> La méthode "UPWIND" est en cours d'implémentation.
  -> Risque & Conseil :
     Veuillez choisir une autre méthode.
"""),

25 : _("""
  -> La norme du vecteur VECT_ORIE est nulle.
  -> Risque & Conseil :
     Veuillez redéfinir VECT_ORIE.
"""),

51 : _("""
  -> Il n'y a aucune maille enrichie.
  -> Risque & Conseil:
     Veuillez vérifier les définitions des level sets.
  """),

}
