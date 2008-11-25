#@ MODIF xfem2 Messages  DATE 24/11/2008   AUTEUR LAVERNE J.LAVERNE 
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



2 : _("""
  -> Seules les modélisations C_PLAN/D_PLAN sont disponibles pour XFEM.
  -> Risques et conseils:
     Veuillez considérer l'une des deux modélisations dans AFFE_MODELE.
"""),

4 : _("""
  -> Le type de formulation du contact (DISCRET/CONTINUE/XFEM) doit etre le meme pour
     toutes les zones de contact.
  -> Risque & Conseil:
     Veuillez revoir la mise en données de AFFE_CHAR_MECA/CONTACT.
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

17 : _("""
  -> Segment de fond_fiss sans maille de surface rattachée
  -> Risque & Conseil:
     Veuillez revoir la définition des level sets.
"""),

20 : _("""
  -> PFON_INI = POINT_ORIG
  -> Risque & Conseil :
     Veuillez définir deux points différents pour PFON_INI et POINT_ORIG.
"""),

21 : _("""
  -> Problème dans l'orientation du fond de fissure : POINT_ORIG mal choisi.
  -> Risque & Conseil : 
     Veuillez redéfinir POINT_ORIG.
"""),

22 : _("""
  -> Tous les points du fond de fissure sont des points de bord.
  -> Risque & Conseil : 
     Assurez-vous du bon choix des paramètres d'orientation de fissure.
"""),

23 : _("""
  -> PFON_INI semble etre un point mal choisi, on le modifie automatiquement.
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


39 : _("""
  -> Deux points du fond de fissure sont très proches ou coincident.
  -> Risque & Conseil :
     Vérifier les définitions des level sets et la liste des points du fond
     de fissure trouvés. Si c'est normal, contactez votre correspondant.
"""),

51 : _("""
  -> Il n'y a aucune maille enrichie.
  -> Risque & Conseil:
     Veuillez vérifier les définitions des level sets.
  """),

}
