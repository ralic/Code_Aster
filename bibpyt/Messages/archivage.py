#@ MODIF archivage Messages  DATE 08/10/2012   AUTEUR DESOZA T.DESOZA 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
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
# RESPONSABLE DELMAS J.DELMAS

# Attention a ne pas faire de retour � la ligne !

cata_msg = {

1 : _(u"""
 Vous risquez d'�craser des donn�es d�j� stock�es dans la structure de donn�es r�sultat.
 Dernier instant stock� dans la structure de donn�es r�sultat: %(r1)19.12e
 Premier instant du calcul: %(r2)19.12e
"""),

4 : _(u"""
 Archivage de l'�tat initial"""),

5 : _(u"""
  Archivage des champs
"""),

6 : _(u"""    Champ stock� <%(k1)s> � l'instant %(r1)19.12e pour le num�ro d'ordre %(i1)d"""),


}
