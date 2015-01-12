# ======================================================================
# COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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

def dyna_visco_prod(self,TYPE_RESU,TYPE_MODE,**args):
    if (TYPE_RESU not in ['HARM','MODE']):
        # return a fictional type so that the dectection of syntax problem happens when reading the user commands file
        return ASSD

    if TYPE_RESU == 'HARM':
        if args.has_key('MODE_MECA'):
            MODE_MECA = args['MODE_MECA']
            # in the case 'HARM', the type can be only mode_meca (not mode_meca_c)
            self.type_sdprod(MODE_MECA,mode_meca)
        return dyna_harmo
    elif TYPE_RESU == 'MODE':
        if TYPE_MODE == 'COMPLEXE':
            return mode_meca_c
        else:
            return mode_meca

    raise AsException("type de concept résultat non prevu")


DYNA_VISCO=MACRO(nom="DYNA_VISCO",
                 op=OPS('Macro.dyna_visco_ops.dyna_visco_ops'),
                 sd_prod=dyna_visco_prod,
                 reentrant='n',
                 UIinfo={"groupes":("Résolution","Dynamique",)},
                 fr="Calcul par projection sur modes réels des FRF avec dependance en fréquence de la matrice K",
                  
                 # INPUT GENERAL DATA
                 MODELE          =SIMP(statut='o',typ=modele_sdaster),
                 CARA_ELEM       =SIMP(statut='f',typ=cara_elem),
                 
                 # ELASTIC MATERIALS WITH CONSTANT PROPERTIES
                 MATER_ELAS      =FACT(statut='f',max='**',
                         regles=(AU_MOINS_UN('MATER','E','AMOR_HYST','RHO','NU'),
                                 PRESENT_ABSENT('MATER','E','AMOR_HYST','RHO','NU'),
                                 ENSEMBLE('E','AMOR_HYST','RHO','NU')),
                         MATER        =SIMP(statut='f',typ=mater_sdaster),
                         E            =SIMP(statut='f',typ='R'),
                         AMOR_HYST    =SIMP(statut='f',typ='R'),
                         RHO          =SIMP(statut='f',typ='R'),
                         NU           =SIMP(statut='f',typ='R'),
                         GROUP_MA     =SIMP(statut='o',typ=grma,validators=NoRepeat(),max='**'),
                                      ),

                 # VISCOELASTIC MATERIALS WITH FREQUENCY DEPENDANT PROPERTIES
                 MATER_ELAS_FO   =FACT(statut='o',max='**',
                         E            =SIMP(statut='o',typ=(fonction_sdaster,formule)),
                         AMOR_HYST    =SIMP(statut='o',typ=(fonction_sdaster,formule)),
                         RHO          =SIMP(statut='o',typ='R'),
                         NU           =SIMP(statut='o',typ='R'),
                         GROUP_MA     =SIMP(statut='o',typ=grma,validators=NoRepeat(),max='**'),
                                      ),
                 
                 TYPE_RESU       =SIMP(statut='d',typ='TXM',defaut='HARM',into=('HARM','MODE') ),
 
                 regles=(UN_PARMI('FREQ','LIST_FREQ')),
                 FREQ            =SIMP(statut='f',typ='R',validators=AndVal((OrdList('croissant'), NoRepeat())), min=2,max='**'),
                 LIST_FREQ       =SIMP(statut='f',typ=listr8_sdaster),

                 # BOUNDARY CONDITIONS AND POSSIBLE EXTERNAL FORCES
                 EXCIT           =FACT(statut='o',max='**',
                         CHARGE       =SIMP(statut='o',typ=char_meca,validators=NoRepeat(),max='**'),
                                      ),

#               KEYWORDS FOR THE MODES COMPUTATION
            b_type_mode_mode=BLOC(condition="TYPE_RESU=='MODE'",
                 TYPE_MODE        =SIMP(statut='d',typ='TXM',defaut='REEL',into=('REEL','BETA_REEL','COMPLEXE') ),
                                  ),
            b_type_mode_harm=BLOC(condition="TYPE_RESU!='MODE'",
                 TYPE_MODE        =SIMP(statut='d',typ='TXM',defaut='REEL',into=('REEL','BETA_REEL') ),
                                  ),
                 RESI_RELA       =SIMP(statut='d',typ='R',defaut=1.e-3),


#               KEYWORDS FOR THE FREQUENCY RESPONSE COMPUTATION
            b_harm          =BLOC(condition="TYPE_RESU=='HARM'", fr="calcul harmonique",
                 COEF_FREQ_MAX   =SIMP(statut='o',typ='R',val_min=1.5),
                 SOLVEUR         =C_SOLVEUR('DYNA_LINE_HARM','GENE'),
                 NOM_CHAM        =SIMP(statut='d',typ='TXM',defaut='DEPL',into=('DEPL','VITE','ACCE'),max=3,validators=NoRepeat() ),
                 MODE_MECA       =SIMP(statut='f',typ= CO),
                                  ),
                 
                 INFO            =SIMP(statut='f',typ='I',defaut=1,into=(1,2)),
);
