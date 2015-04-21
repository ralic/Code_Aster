# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
# person_in_charge: jean-luc.flejou at edf.fr
#
def ValeurCara(cara, Lcara, Lvale, valdefaut=None):
    if ( cara in Lcara ):
        return Lvale[Lcara.index(cara)]
    else:
        if valdefaut != None :
            return valdefaut
        else:
            raise AsException("Erreur construction")


def affe_cara_elem_ops(self, MODELE, INFO, VERIF,
    POUTRE, BARRE, COQUE, CABLE,
    DISCRET, DISCRET_2D, ORIENTATION, DEFI_ARC, MASSIF, POUTRE_FLUI, GRILLE, MEMBRANE,
    RIGI_PARASOL, RIGI_MISS_3D, MASS_AJOU,
    GEOM_FIBRE, MULTIFIBRE, **args):
    """
       Ecriture de la macro AFFE_CARA_ELEM
    """
    #
    ier = 0
    # On importe les définitions des commandes à utiliser dans la macro
    # Le nom de la variable doit être obligatoirement le nom de la commande
    from Macro.cara_elem import CARA_ELEM
    # La macro compte pour ?? dans la numérotation des commandes
    self.set_icmd(1)
    #
    # Le concept sortant est nommé 'nomres' dans le contexte de la macro
    self.DeclareOut('nomres', self.sd)
    #
    motclef_cara_elem = {}
    if ( MODELE ):  motclef_cara_elem['MODELE']   = MODELE
    if ( INFO ):    motclef_cara_elem['INFO']     = INFO
    if ( VERIF ):   motclef_cara_elem['VERIF']    = VERIF
    #
    if POUTRE != None:
        motclef_cara_elem['POUTRE']         = POUTRE.List_F()
    #
    if BARRE != None:
        motclef_cara_elem['BARRE']          = BARRE.List_F()
    #
    if COQUE != None:
        motclef_cara_elem['COQUE']          = COQUE.List_F()
    #
    if CABLE != None:
        motclef_cara_elem['CABLE']          = CABLE.List_F()
    #
    if DISCRET != None:
        motclef_cara_elem['DISCRET']        = DISCRET.List_F()
    #
    if DISCRET_2D != None:
        motclef_cara_elem['DISCRET_2D']     = DISCRET_2D.List_F()
    #
    if ORIENTATION != None:
        motclef_cara_elem['ORIENTATION']    = ORIENTATION.List_F()
    #
    if DEFI_ARC != None:
        motclef_cara_elem['DEFI_ARC']       = DEFI_ARC.List_F()
    #
    if MASSIF != None:
        motclef_cara_elem['MASSIF']         = MASSIF.List_F()
    #
    if POUTRE_FLUI != None:
        motclef_cara_elem['POUTRE_FLUI']    = POUTRE_FLUI.List_F()
    #
    if GRILLE != None:
        motclef_cara_elem['GRILLE']         = GRILLE.List_F()
    #
    if MEMBRANE != None:
        motclef_cara_elem['MEMBRANE']       = MEMBRANE.List_F()
    #
    if RIGI_PARASOL != None:
        motclef_cara_elem['RIGI_PARASOL']   = RIGI_PARASOL.List_F()
    #
    if RIGI_MISS_3D != None:
        motclef_cara_elem['RIGI_MISS_3D']   = RIGI_MISS_3D.List_F()
    #
    if MASS_AJOU != None:
        motclef_cara_elem['MASS_AJOU']      = MASS_AJOU.List_F()
    #
    if GEOM_FIBRE != None:
        motclef_cara_elem['GEOM_FIBRE']     = GEOM_FIBRE
    #
    if MULTIFIBRE != None:
        motclef_cara_elem['MULTIFIBRE']     = MULTIFIBRE.List_F()
    #
    nomres = CARA_ELEM( **motclef_cara_elem )
    #
    return ier
