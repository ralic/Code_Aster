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
# person_in_charge: nicolas.sellenet at edf.fr

TEST_FONCTION=MACRO(nom="TEST_FONCTION",
                    op=OPS('Macro.test_fonction_ops.test_fonction_ops'),
                    sd_prod=None,
            fr=tr("Extraction d'une valeur numérique ou d'un attribut de fonction pour comparaison à une valeur de référence"),
            UIinfo={"groupes":("Fonctions","Utilitaires",)},
         TEST_NOOK       =SIMP(statut='f',typ='TXM',defaut="NON",into=("OUI","NON") ),
         VALEUR          =FACT(statut='f',max='**',
                               fr=tr("Tester la valeur d une fonction ou d une nappe"),
           regles=(UN_PARMI('VALE_PARA','INTERVALLE'),),
           FONCTION        =SIMP(statut='o',typ=(fonction_sdaster,fonction_c,nappe_sdaster,formule) ),
           NOM_PARA        =SIMP(statut='f',typ='TXM',validators=NoRepeat(),max=2),
           VALE_PARA       =SIMP(statut='f',typ='R'  ,validators=NoRepeat(),max=2),
           INTERVALLE      =SIMP(statut='f',typ='R'  ,validators=NoRepeat(),min=2,max=2),
           **C_TEST_REFERENCE('FONCTION', max='**')
         ),
         ATTRIBUT        =FACT(statut='f',max='**',
                               fr=tr("Tester la valeur d un attribut d une fonction ou d''une nappe"),
           FONCTION        =SIMP(statut='o',typ=(fonction_sdaster,fonction_c,nappe_sdaster,formule) ),
           PARA            =SIMP(statut='f',typ='R' ),
           CRIT_PARA       =SIMP(statut='f',typ='TXM',defaut="RELATIF",into=("RELATIF","ABSOLU") ),
           PREC_PARA       =SIMP(statut='f',typ='R',defaut= 1.E-3 ),
           ATTR            =SIMP(statut='o',typ='TXM',
                                 into=("NOM_PARA","NOM_RESU","PROL_DROITE","PROL_GAUCHE","INTERPOL",
                                       "PROL_GAUCHE_FONC","PROL_DROITE_FONC","INTERPOL_FONC","NOM_PARA_FONC") ),
           ATTR_REFE       =SIMP(statut='o',typ='TXM' ),
           REFERENCE       =SIMP(statut='f',typ='TXM',
                                 into=("ANALYTIQUE","SOURCE_EXTERNE","AUTRE_ASTER") ),
         ),
)
