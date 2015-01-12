# coding=utf-8
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

def dyna_visco_harm(self, EXCIT, list_FREQ, modes,
                          MATER_ELAS_FO, __asseKg, __asseKgr, __asseMg,
                          __listKv, e0, eta0, __num, **args):

    """
       Macro-command DYNA_VISCO,
       function to compute the harmonic response of the structure
    """

    from Accas import _F
    import aster
    from Utilitai.Utmess import UTMESS
    from numpy import array

    DEFI_BASE_MODALE= self.get_cmd('DEFI_BASE_MODALE')
    MODE_STATIQUE   = self.get_cmd('MODE_STATIQUE')
    NUME_DDL_GENE   = self.get_cmd('NUME_DDL_GENE')
    CALC_VECT_ELEM  = self.get_cmd('CALC_VECT_ELEM')
    ASSE_VECTEUR    = self.get_cmd('ASSE_VECTEUR')
    PROJ_MATR_BASE  = self.get_cmd('PROJ_MATR_BASE')
    PROJ_VECT_BASE  = self.get_cmd('PROJ_VECT_BASE')
    DYNA_VIBRA      = self.get_cmd('DYNA_VIBRA')
    REST_GENE_PHYS  = self.get_cmd('REST_GENE_PHYS')
    COMB_MATR_ASSE  = self.get_cmd('COMB_MATR_ASSE')
    CREA_CHAMP      = self.get_cmd('CREA_CHAMP')
    CREA_RESU       = self.get_cmd('CREA_RESU')
    DETRUIRE        = self.get_cmd('DETRUIRE')


    self.DeclareOut('dyna_harm',self.sd)


    if args['NOM_CHAM']!=None:
        NOM_CHAM = args['NOM_CHAM']
    if isinstance(NOM_CHAM,str):
        NOM_CHAM = (NOM_CHAM,) # convert the string into a tuple


    # get the characteristics of the excitation
    l_force_nodale = False  # logical which indicates the presence of nodal forces in the excitation
    charge=[]
    for l_char in EXCIT:
        for l_char2 in l_char['CHARGE']:
            charge.append(l_char2)

    mc_force_nodale=[]
    for i in range(0,len(charge)):
        ddl = charge[i].sdj.CHME.FORNO.VALE.get()

        if ddl!=None: # if no nodal force is found, one does not achieve the following treatment
            l_force_nodale = True
            no_force = charge[i].sdj.CHME.LIGRE.NEMA.get()

            for j in no_force.keys():
                if not ( len(no_force[j])==2 and no_force[j][1]==1 ): # in the dictionary no_force, an imposed nodal force presents as 
                                                                      # an entry (a key) having this shape: (node_number, 1)
                                                                      # so one deletes all entries having a different shape
                    no_force.pop(j)
    
            iret,ibid,nom_mail = aster.dismoi('NOM_MAILLA',__num.nom,'NUME_DDL','F')
            maillage = self.get_concept(nom_mail)
    
            direction = array( [tuple(array(ddl[i*12:(i+1)*12]).nonzero()[0]) for i in range(0, len(ddl)/12)] )
            # array which contains, for each node having an imposed force, the list of the imposed directions
            # (rq : "/12" because each node has 12 DoF)
            # the values are 0 if FX imposed, 1 if FY, and 2 if FZ
            #                3 if MX imposed, 4 if MY, and 5 if MZ
            ddl_phys = [None]*len(no_force)
            for i_nf in range(0, len(no_force)):
                ddl_phys[i_nf]=[]
                for j in range(0,len(direction[i_nf])):
                    if direction[i_nf][j]==0:
                        ddl_phys[i_nf].append('DX')
                    elif direction[i_nf][j]==1:
                        ddl_phys[i_nf].append('DY')
                    elif direction[i_nf][j]==2:
                        ddl_phys[i_nf].append('DZ')
                    if direction[i_nf][j]==3:
                        ddl_phys[i_nf].append('DRX')
                    elif direction[i_nf][j]==4:
                        ddl_phys[i_nf].append('DRY')
                    elif direction[i_nf][j]==5:
                        ddl_phys[i_nf].append('DRZ')


            no_mail = maillage.sdj.NOMNOE.get()  # name of the nodes presents in the mesh

            for i in range(1, len(no_force)+1):
                mc_composante={}
                mc_composante['AVEC_CMP'] = tuple( ddl_phys[i-1] )
                
                mc_force_nodale.append( _F(NOEUD=(no_mail[(no_force[i][0]-1)].strip()),
                                           **mc_composante)
                                       )

    
    if l_force_nodale==False:
        UTMESS('F', 'DYNAVISCO_8')

    __modstat=MODE_STATIQUE(MATR_RIGI=__asseKgr,
                            FORCE_NODALE=mc_force_nodale);


    #################################################################################################

    # PROJECTION OF THE MATRICES K AND M ON A BASE MADE WITH THE REAL EIGENMODES AND THE STATIC MODES

    __modrs=DEFI_BASE_MODALE(DIAG_MASS=_F(MODE_MECA=modes,
                                          MODE_STAT=__modstat,),);

    __ddlplein=NUME_DDL_GENE(BASE=__modrs,
                             STOCKAGE='PLEIN',);

    __Mgproj=PROJ_MATR_BASE(BASE=__modrs,
                            NUME_DDL_GENE=__ddlplein,
                            MATR_ASSE=__asseMg,);

    __Kgproj=PROJ_MATR_BASE(BASE=__modrs,
                            NUME_DDL_GENE=__ddlplein,
                            MATR_ASSE=__asseKg,);


    # ASSEMBLY AND PROJECTION OF THE EXCITATION

    __felem=CALC_VECT_ELEM(OPTION='CHAR_MECA',
                           CHARGE=charge,)

    __assef=ASSE_VECTEUR(VECT_ELEM=__felem,
                         NUME_DDL=__num,)

    __lfor=PROJ_VECT_BASE(BASE=__modrs,
                          NUME_DDL_GENE=__ddlplein,
                          VECT_ASSE=__assef,);

    DETRUIRE(CONCEPT = _F (NOM = __felem,) , )
    DETRUIRE(CONCEPT = _F (NOM = __assef,) , )
    DETRUIRE(CONCEPT = _F (NOM = __asseKg,) , )


    # PROJECTION OF THE STIFFNESS MATRICE ISOLATED FOR EACH VISCOELASTIC PART
    __lKv={}
    ny=0
    for y in MATER_ELAS_FO:
        __lKv[ny]=PROJ_MATR_BASE(BASE=__modrs,
                               NUME_DDL_GENE=__ddlplein,
                               MATR_ASSE=__listKv[ny],);
        ny=ny+1


    ##############################################################
    # HARMONIC RESPONSE COMPUTATION
    ##############################################################

    # compute the response of the projected problem for f=fmin
    __dyngene=DYNA_VIBRA(TYPE_CALCUL='HARM',
                         BASE_CALCUL='GENE',
                         MATR_MASS=__Mgproj,
                         MATR_RIGI=__Kgproj,
                         FREQ=list_FREQ[0],
                         EXCIT=_F(VECT_ASSE_GENE=__lfor,
                                  COEF_MULT=1,),),

    dyna_harm=REST_GENE_PHYS(RESU_GENE=__dyngene,
                             MODE_MECA=__modrs,
                             NOM_CHAM=NOM_CHAM);


    # compute the response of the projected problem for f>fmin
    for num_freq in range(1,len(list_FREQ)):

        __Kwproj=__Kgproj
        ny=0
        for y in MATER_ELAS_FO:
            e=y['E'](list_FREQ[num_freq])
            eta=y['AMOR_HYST'](list_FREQ[num_freq])

            __Kwproj=COMB_MATR_ASSE(COMB_C=(_F(MATR_ASSE=__Kwproj,
                                               COEF_R=1.),
                                            _F(MATR_ASSE=__lKv[ny],
                                               COEF_C=('RI',e/e0[ny]-1,eta*e/e0[ny]-eta0[ny],),),),)

            ny=ny+1

        __dyngene=DYNA_VIBRA(TYPE_CALCUL='HARM',
                             BASE_CALCUL='GENE',
                             MATR_MASS=__Mgproj,
                             MATR_RIGI=__Kwproj,
                             FREQ=list_FREQ[num_freq],
                             EXCIT=_F(VECT_ASSE_GENE=__lfor,
                                      COEF_MULT=1.,),),

        __dynphys=REST_GENE_PHYS(RESU_GENE=__dyngene,
                                 MODE_MECA=__modrs,
                                 NOM_CHAM=NOM_CHAM);


        for champ in NOM_CHAM:

            __resveu=CREA_CHAMP(OPERATION='EXTR',
                                NOM_CHAM=champ,
                                TYPE_CHAM='NOEU_DEPL_C',
                                RESULTAT=__dynphys,
                                NUME_ORDRE=1,);
            
            dyna_harm=CREA_RESU(reuse=dyna_harm,
                                OPERATION='AFFE',
                                TYPE_RESU='DYNA_HARMO',
                                NOM_CHAM=champ,
                                AFFE=_F(CHAM_GD=__resveu,
                                        FREQ=list_FREQ[num_freq],),);


    return dyna_harm

