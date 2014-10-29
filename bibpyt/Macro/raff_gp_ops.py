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
#
#
# FORMULE 2D PERMETTANT LA DEFINITION ET LE CALCUL DES COPEAUX DANS LE CAS SS_COPEAU
#           (Pour plus de renseignement, voir CR-AMA-12-272)
#


def SEUIL(X, Y, X0, Y0, R, lc, Nume_cop, ccos, ssin):
    f1 = 0
    f2 = 0
    f3 = 0
    # DY1,DY2,DX3,DX2
    if (-(X - X0) * ccos <= (Y - Y0) * ssin)\
        and((X - X0 - Nume_cop * lc * ccos) * ccos
            <= -ssin * (Y - Y0 - Nume_cop * lc * ssin))\
        and((Y - Y0 + R * ccos) * ccos >= (X - X0 - R * ssin) * ssin)\
            and((Y - Y0 - R * ccos) * ccos <= (X - X0 + R * ssin) * ssin):
        f1 = 1
    # C2,DY2
    if ((X - X0 - Nume_cop * lc * ccos) ** 2 + (Y - Y0 - Nume_cop * lc * ssin) ** 2
       <= R ** 2)\
       and ((X - X0 - Nume_cop * lc * ccos) * ccos
            >= -ssin * (Y - Y0 - Nume_cop * lc * ssin)):
        f2 = 1
    # C1,DY1
    if ((X - X0) ** 2 + (Y - Y0) ** 2 <= R ** 2) and (-(X - X0) * ccos <= (Y - Y0) * ssin):
        f3 = 1
    f = f1 + f2 - f3
    return f

#--


#
# DEBUT DE LA MACRO PROPREMENT DITE
#
def raff_gp_ops(self, **args):
    """Corps de RAFF_GP"""
    from numpy import *
    import aster
    from Accas import _F
    from Utilitai.Utmess import UTMESS, MasquerAlarme, RetablirAlarme
    MasquerAlarme('CALCCHAMP_1')
#    MasquerAlarme('HOMARD0_9')
#
# PREPARATION DES SORTIES
#
    self.DeclareOut('MAOUT', self.sd)

    ier = 0
    # La macro compte pour 1 dans la numerotation des commandes
    self.set_icmd(1)
#
# IMPORTATION DES COMMANDES ET MACRO UTILISEES
#
    CO = self.get_cmd('CO')
    CREA_CHAMP = self.get_cmd('CREA_CHAMP')
    FORMULE = self.get_cmd('FORMULE')
    CALC_CHAMP = self.get_cmd('CALC_CHAMP')
    MACR_ADAP_MAIL = self.get_cmd('MACR_ADAP_MAIL')
    AFFE_MODELE = self.get_cmd('AFFE_MODELE')
    DETRUIRE = self.get_cmd('DETRUIRE')
    COPIER = self .get_cmd('COPIER')
#

#
# RECUPERATION DU MAILLAGE ET DES DONNEES UTILISATEUR
#

    __MA0 = self['MAILLAGE_N']

    nb_calc = self['NB_RAFF']
# Manipulation obligatoire pour pouvoir se servir des grandeurs dans les
# formules
    TRANCHE_2D = self['TRANCHE_2D']
    nbcop = TRANCHE_2D['NB_ZONE']
    theta = TRANCHE_2D['ANGLE']
    taille = TRANCHE_2D['TAILLE']
    self.update_const_context({'origine': TRANCHE_2D['CENTRE']})
    self.update_const_context({'rayon': TRANCHE_2D['RAYON']})
    self.update_const_context({'taille': TRANCHE_2D['TAILLE']})
    self.update_const_context({'theta': TRANCHE_2D['ANGLE']})
    self.update_const_context({'nbcop': TRANCHE_2D['NB_ZONE']})

#
# INITIALISATIONS
#
    __MA = [None] * (nb_calc + 1)
    __MA[0] = __MA0
    self.update_const_context({'SEUIL': SEUIL})
    self.update_const_context({'ccos': cos(theta * pi / 180.)})
    self.update_const_context({'ssin': sin(theta * pi / 180.)})

#
# C EST PARTI
#
    for num_calc in range(0, nb_calc):
        if num_calc % 3 == 0:
            __seuil = FORMULE(
                VALE='''SEUIL(X,Y,origine[0]-3.*rayon*ccos,origine[1]-3*rayon*ssin,3*rayon,taille,nbcop+4,ccos,ssin)''',
                NOM_PARA=('X', 'Y'),)
        elif num_calc % 3 == 1:
            __seuil = FORMULE(
                VALE='''SEUIL(X,Y,origine[0]-2.*rayon*ccos,origine[1]-2.*rayon*ssin,2.*rayon,taille,nbcop+2,ccos,ssin)''',
                NOM_PARA=('X', 'Y'),)
        elif num_calc % 3 == 2:
            __seuil = FORMULE(
                VALE='''SEUIL(X,Y,origine[0]-1.2*rayon*ccos,origine[1]-1.2*rayon*ssin,1.2*rayon,taille,nbcop,ccos,ssin)''',
                NOM_PARA=('X', 'Y'),)
        __MO = AFFE_MODELE(MAILLAGE=__MA[num_calc],
                           AFFE=_F(TOUT='OUI',
                                   PHENOMENE='MECANIQUE',
                                   MODELISATION='D_PLAN'),
                           )
# champ de geometrie et de points de gauss (coordonnees des points de gauss)
        __CHXN = CREA_CHAMP(OPERATION='EXTR',
                            TYPE_CHAM='NOEU_GEOM_R',
                            NOM_CHAM='GEOMETRIE',
                            MAILLAGE=__MA[num_calc])
        __CHXG = CREA_CHAMP(OPERATION='DISC',
                            TYPE_CHAM='ELGA_GEOM_R',
                            MODELE=__MO,
                            CHAM_GD=__CHXN)
        __f_seuil = CREA_CHAMP(TYPE_CHAM='ELGA_NEUT_F',
                               MODELE=__MO,
                               OPERATION='AFFE',
                               PROL_ZERO='OUI',
                               AFFE=_F(TOUT='OUI',
                                       NOM_CMP='X1',
                                       VALE_F=__seuil,),)
        __COPEAUX = CREA_CHAMP(TYPE_CHAM='ELGA_NEUT_R',
                               OPERATION='EVAL',
                               CHAM_F=__f_seuil,
                               CHAM_PARA=(__CHXG),)
        __MA[num_calc + 1] = CO('__MA_%d' % (num_calc + 1))
        MACR_ADAP_MAIL(ADAPTATION='RAFFINEMENT',
                       CHAM_GD=__COPEAUX,
                       CRIT_RAFF_ABS=0.01,
                       MAILLAGE_N=__MA[num_calc],
                       MAILLAGE_NP1=__MA[num_calc + 1])
        DETRUIRE(CONCEPT=(_F(NOM=__COPEAUX,),
                          _F(NOM=__MO,),
                 _F(NOM=__CHXN),
            _F(NOM=__CHXG),
            _F(NOM=__f_seuil),
            _F(NOM=__seuil),
        ),
        )

    MAOUT = COPIER(CONCEPT=__MA[nb_calc])
    RetablirAlarme('CALCCHAMP_1')

    return ier
