# ======================================================================
# COPYRIGHT (C) 1991 - 2014  EDF R&D                  WWW.CODE-ASTER.ORG
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
# person_in_charge: serguei.potapov at edf.fr

LIRE_EUROPLEXUS = MACRO(nom="LIRE_EUROPLEXUS",
                        op=OPS('Macro.lire_europlexus_ops.lire_europlexus_ops'),
                        sd_prod=evol_noli,
                        reentrant='n',
                        UIinfo={"groupes":("Outils-métier","Dynamique",)},
                        fr="Chainage Code_Aster-Europlexus",

        UNITE_MED = SIMP(statut='o', typ='I',),
        MODELE      = SIMP(statut='o',typ=modele_sdaster),
        CARA_ELEM   = SIMP(statut='f',typ=cara_elem),
        CHAM_MATER  = SIMP(statut='f',typ=cham_mater),
        COMPORTEMENT  =C_COMPORTEMENT('CALC_EUROPLEXUS'),
        EXCIT       = FACT(statut='f',max='**',
           CHARGE         = SIMP(statut='o',typ=(char_meca,)),
           FONC_MULT      = SIMP(statut='f',typ=(fonction_sdaster,)),
          ),
        INFO        = SIMP(statut='f',typ='I',defaut=1,into=( 1, 2 ) ),
        ) ;
