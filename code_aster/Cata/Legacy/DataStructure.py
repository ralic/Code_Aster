# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
# person_in_charge: mathieu.courtois at edf.fr

"""
Module cata
-----------

All the objects needed by the legacy supervisor.
"""


from .DS.co_cham_gd_sdaster import (cham_gd_sdaster, carte_sdaster, cham_elem,
    cham_no_sdaster, post_comp_cham_no, post_comp_cham_el)
from .DS.co_char_meca import char_meca
from .DS.co_mater_sdaster import mater_sdaster
from .DS.co_matr_asse import (matr_asse, matr_asse_gd, matr_asse_depl_c,
                           matr_asse_depl_r, matr_asse_pres_c, matr_asse_pres_r,
                           matr_asse_temp_c, matr_asse_temp_r)
from .DS.co_spectre_sdaster import spectre_sdaster
from .DS.co_fonction_class import (fonction_class, fonction_sdaster, fonction_c,
                                nappe_sdaster)
from .DS.co_nume_ddl_gene import nume_ddl_gene
from .DS.co_macr_elem_dyna import macr_elem_dyna
from .DS.co_matr_asse_gene import matr_asse_gene, matr_asse_gene_r, matr_asse_gene_c
from .DS.co_char_acou import char_acou
from .DS.co_maillage_sdaster import maillage_sdaster, grille_sdaster, squelette
from .DS.co_listis_sdaster import listis_sdaster
from .DS.co_type_flui_stru import type_flui_stru
from .DS.co_matr_elem import (matr_elem, matr_elem_depl_c, matr_elem_depl_r,
                           matr_elem_pres_c, matr_elem_temp_r)
from .DS.co_char_contact import char_contact
from .DS.co_macr_elem_stat import macr_elem_stat
from .DS.co_interspectre import interspectre
from .DS.co_fond_fiss import fond_fiss
from .DS.co_char_cine_meca import char_cine_meca
from .DS.co_resultat_sdaster import (resultat_sdaster, comb_fourier, fourier_elas,
                                  fourier_ther, mult_elas, mode_empi,
                                  evol_sdaster, evol_char, evol_elas,
                                  evol_noli, evol_ther, evol_varc)
from .DS.co_listr8_sdaster import listr8_sdaster
from .DS.co_cara_elem import cara_elem
from .DS.co_entier import entier
from .DS.co_modele_sdaster import modele_sdaster
from .DS.co_sd_dyna import (dyna_gene, dyna_phys, harm_gene, tran_gene, acou_harmo,
                         dyna_harmo, dyna_trans, mode_acou, mode_flamb, mode_meca,
                         mode_meca_c, mode_gene)
from .DS.co_melasflu_sdaster import melasflu_sdaster
from .DS.co_modele_gene import modele_gene
from .DS.co_vect_elem import vect_elem, vect_elem_depl_r, vect_elem_pres_c, vect_elem_temp_r
from .DS.co_cabl_precont import cabl_precont
from .DS.co_char_cine_ther import char_cine_ther
from .DS.co_nume_ddl_sdaster import nume_ddl_sdaster
from .DS.co_cham_mater import cham_mater
from .DS.co_char_cine_acou import char_cine_acou
from .DS.co_gfibre_sdaster import gfibre_sdaster
from .DS.co_list_inst import list_inst
from .DS.co_mode_cycl import mode_cycl
from .DS.co_corresp_2_mailla import corresp_2_mailla
from .DS.co_fiss_xfem import fiss_xfem
from .DS.co_table_sdaster import table_sdaster, table_fonction, table_fonction, table_container
from .DS.co_interf_dyna_clas import interf_dyna_clas
from .DS.co_char_ther import char_ther
from .DS.co_compor_sdaster import compor_sdaster
from .DS.co_vect_asse_gene import vect_asse_gene
from .DS.co_reel import reel


from Accas import GEOM, formule, formule_c

# Types géométriques
class no(GEOM):
    """
    Classe servant à définir le nom d'un noeud dans le fichier de commande
    En clair : un chaine de longueur 8.
    """
    pass

class grno(GEOM):
    """
    Classe servant à définir le nom d'un groupe de noeuds dans le fichier de commande
    En clair : un chaine de longueur 24.
    """
    def __convert__(cls,valeur):
        """
        Fonction de verification de la longueur de la chaine
        """
        if isinstance(valeur, (str,unicode)) and len(valeur.strip()) <= 24:
            return valeur.strip()
        raise ValueError(_(u'On attend une chaine de caractères (de longueur <= 24).'))
    __convert__ = classmethod(__convert__)

class ma(GEOM):
    """
    Classe servant à définir le nom d'une maille dans le fichier de commande
    En clair : un chaine de longueur 8.
    """
    pass

class grma(GEOM):
    """
    Classe servant à définir le nom d'un groupe de mailles dans le fichier de commande
    En clair : un chaine de longueur 24.
    """
    def __convert__(cls,valeur):
        """
        Fonction de verification de la longueur de la chaine
        """
        if isinstance(valeur, (str,unicode)) and len(valeur.strip()) <= 24:
            return valeur.strip()
        raise ValueError(_(u'On attend une chaine de caractères (de longueur <= 24).'))
    __convert__ = classmethod(__convert__)


# Ce type doit être associé à tous les mots-clés devant recevoir un numéro
# d'unité logique fortran. De base, il s'agit d'un simple entier.
def UnitType(filter=None):
    """Emulated type for *UNITE* keywords.

    Arguments:
        filter (str): Can be used to pass a filter or an expected filetype.
    """
    return "I"
