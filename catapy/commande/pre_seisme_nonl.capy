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

def pre_seisme_nonl_sdprod(self, RESULTAT, PARAMETRE, **args):
   if RESULTAT[0]['MODELE'] :
       self.type_sdprod(RESULTAT[0]['MODELE'], modele_sdaster)
   if RESULTAT[0]['MAILLAGE'] :
       self.type_sdprod(RESULTAT[0]['MAILLAGE'], maillage_sdaster)
   if RESULTAT[0]['CHAM_MATER'] :
       self.type_sdprod(RESULTAT[0]['CHAM_MATER'], cham_mater)
   if RESULTAT[0]['CARA_ELEM'] :
       self.type_sdprod(RESULTAT[0]['CARA_ELEM'], cara_elem)
   if RESULTAT[0]['BASE_MODALE'] :
       if PARAMETRE[0]['PRE_CALC_MISS']:
           self.type_sdprod(RESULTAT[0]['BASE_MODALE'], mode_meca)
       else:
           raise AsException("Le mot-clé PRE_CALC_MISS est obligatoire pour créer un concept de type BASE_MODALE")
   if RESULTAT[0]['MACR_ELEM_DYNA'] :
       if PARAMETRE[0]['PRE_CALC_MISS']:
           self.type_sdprod(RESULTAT[0]['MACR_ELEM_DYNA'], macr_elem_dyna)
       else:
           raise AsException("Le mot-clé PRE_CALC_MISS est obligatoire pour créer un concept de type MACR_ELEM_DYNA")
   if RESULTAT[0]['CHARGE'] :
       for mcfact in RESULTAT[0]['CHARGE']:
           if mcfact['OPTION'] == 'LAPL_TEMPS' and PARAMETRE[0]['PRE_CALC_MISS']:
               raise AsException("Le mot-clé POST_CALC_MISS est obligatoire pour créer une charge de type LAPL_TEMPS")
           self.type_sdprod(mcfact['NOM'], char_meca)

def affe_char_meca_regles(**args):
       # ONLY if PRE_CALC_MISS is not None
       AFFE_CHAR_MECA.entites['MODELE'].statut = 'f'   
       return AFFE_CHAR_MECA.regles

def affe_cara_elem_regles(**args):
       # ONLY if PRE_CALC_MISS is not None
       AFFE_CARA_ELEM.entites['MODELE'].statut = 'f'
       return AFFE_CARA_ELEM.regles

PRE_SEISME_NONL = MACRO(nom="PRE_SEISME_NONL",
                 op=OPS('Macro.pre_seisme_nonl_ops.pre_seisme_nonl_ops'),
                 sd_prod=pre_seisme_nonl_sdprod,
                 fr=tr("description"),
                 reentrant='n',
                 UIinfo={"groupes":("Fonctions",)},
                 AFFE_MODELE = FACT(statut='d',
                     regles=AFFE_MODELE.regles,
                     **AFFE_MODELE.entites
                                    ),
                 AFFE_MATERIAU = FACT(statut='d',
                     regles=AFFE_MATERIAU.regles,
                     **AFFE_MATERIAU.entites
                                    ),

                 AFFE_CARA_ELEM = FACT(statut='d',
                     regles=affe_cara_elem_regles(),
                     **AFFE_CARA_ELEM.entites
                                    ),
                 AFFE_CHAR_MECA = FACT(statut='d',
                     regles=affe_char_meca_regles(),
                     **AFFE_CHAR_MECA.entites
                                    ),
                 PARAMETRE = FACT( statut = 'o', min = 1, max = 1,
                                   regles = UN_PARMI('PRE_CALC_MISS','POST_CALC_MISS'),
                                   PRE_CALC_MISS = FACT(statut = 'f', max = 1,
                                                         REDUC_DYNA_ISS = SIMP(statut='f', typ='TXM', into=('OUI','NON',), defaut='NON'),
                                                         REDUC_DYNA_IFS = SIMP(statut='f', typ='TXM', into=('OUI','NON',), defaut='NON'),
                                                         NMAX_MODE_ISS = SIMP(statut='o', typ='I'),
                                                         b_ISFS = BLOC(condition = " CALC_MISS_OPTION == 'ISFS' ",
                                                                       NMAX_MODE_IFS = SIMP(statut='o', typ='I'),),
                                                         GROUP_NO_CENT = SIMP(statut='f', typ=grno, max='**'),
                                                         CALC_MISS_OPTION = SIMP(statut='o',typ='TXM', into=('ISS','ISFS')),
                                                         GROUP_NO_INTERF = SIMP(statut='o', typ=grno, max='**'),
                                                         ),
                                    POST_CALC_MISS = FACT(statut = 'f', max = 1,
                                                          MACR_ELEM_DYNA  = SIMP(statut='o', typ=macr_elem_dyna,
                                                                           fr=tr("Macro élément produit en amont")),
                                                          UNITE_RESU_RIGI = SIMP(statut='f', typ='I'),
                                                          UNITE_RESU_MASS = SIMP(statut='f', typ='I'),
                                                          UNITE_RESU_AMOR = SIMP(statut='f', typ='I'),
                                                         ),
                                  ),
                 RESULTAT = FACT(statut='o', min = 1, max = 1,
                                     MODELE = SIMP(statut='f',typ=CO),
                                     MAILLAGE = SIMP(statut='f',typ=CO),
                                     CHAM_MATER = SIMP(statut='f',typ=CO),
                                     CARA_ELEM = SIMP(statut='f',typ=CO),
                                     # ONLY when PRE_CALC_MISS is not None
                                     BASE_MODALE = SIMP(statut='f',typ=CO),
                                     # ONLY when PRE_CALC_MISS is not None
                                     MACR_ELEM_DYNA = SIMP(statut='f',typ=CO),
                                     CHARGE = FACT( statut='f', max = '**',
                                                   OPTION = SIMP(statut='o',typ='TXM',
                                                            into=('COND_LIM', 'LAPL_TEMPS') ),
                                                   NOM = SIMP(statut='o',typ=CO),
                                                  ),
                                    ),

                   INFO = SIMP( statut='f', typ='I', defaut= 1 ,into=(1,2) ),

)
