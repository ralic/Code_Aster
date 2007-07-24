#@ MODIF xfem2 Messages  DATE 23/07/2007   AUTEUR ABBAS M.ABBAS 
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
Erreur (i1)i dvt dans la récupération de la topologie des sous-éléments pour la visu.
"""),

5: _("""
Le vecteur TAU1 correspondant la première direction du frottement dans l'élément XFEM est nul.
Ceci signifie que les gradients des level sets sont surement colinéaires en ce point.
"""),

6: _("""
Multifissuration interdite avec l'opérateur PROPA_XFEM.
"""),

7: _("""
Le contact a été activé dans XFEM (CONTACT_XFEM='OUI' dans MODI_MODELE_XFEM)
Mais vous ne l'avez pas activé par AFFE_CHAR_MECA/CONTACT_XFEM.

"""),

8: _("""
Le modele %(k1)s transmis dans AFFE_CHAR_MECA/CONTACT n'est pas un modele
XFEM. Ce n'est pas un concept résultant de MODI_MODELE_XFEM.
"""),

9: _("""
Le modele %(k1)s transmis dans AFFE_CHAR_MECA/CONTACT n'est pas un modele
XFEM avec contact. Il faut CONTACT='OUI' dans MODI_MODELE_XFEM.
"""),

10: _("""
Le modele %(k1)s transmis dans MECA_NON_LINE n'est pas un modele
XFEM. Ce n'est pas un concept résultant de MODI_MODELE_XFEM.
"""),

11: _("""
Le modele %(k1)s transmis dans AFFE_CHAR_MECA/CONTACT_XFEM n'est le modele
XFEM utilise dans le AFFE_CHAR_MECA/CONTACT nommé %(k2)s.
"""),

12: _("""
Le modele %(k1)s transmis dans AFFE_CHAR_MECA/CONTACT_XFEM n'est pas un modele
XFEM. Ce n'est pas un concept résultant de MODI_MODELE_XFEM.
"""),

44: _("""
Le champ de nom %(k1)s n'a pas été créé car aucun TYPE_ELEM du LIGREL de nom %(k2)s
ne connait le paramètre de l'option %(k3)s.
Contactez les développeurs.
"""),

51: _("""
Il n'y a aucune maille enrichie.
"""),


}
