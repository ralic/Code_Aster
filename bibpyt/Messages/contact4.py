# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
# person_in_charge: josselin.delmas at edf.fr

cata_msg = {

    1 : _(u"""
Contact LAC
Seuls les algorithmes en Newton sont utilisables (ALGO_RESO_GEOM et ALGO_RESO_CONT)
"""),

    2 : _(u"""
Contact LAC
    Le maillage %(k1)s ne contient pas les objets spécifiques à la méthode ALGO_CONT='LAC'.
Conseil:
    Il faut faire CREA_MAILLAGE/DECOUPE_LAC avant DEFI_CONTACT
"""),

    4 : _(u"""
Contact LAC
        ALGO_CONT='LAC' ne fonctionne pas avec le frottement. 
"""),

    5 : _(u"""
Contact LAC
         On ne détecte pas le bon nombre de mailles esclaves. 
         Conseil :
             Cette erreur est probablement dû au fait que vous avez inversé les rôles maîtres et esclaves. 
             Vérifiez que votre GROUP_MA_ESCL est bien celui utilisé par DECOUPE_LAC de CREA_MAILLAGE.  
"""),

    6 : _(u"""
Contact LAC
         Le frottement n'est pas autorisé (COULOMB=0.0).   
"""),

}
