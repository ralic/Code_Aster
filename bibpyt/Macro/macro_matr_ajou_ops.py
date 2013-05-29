# coding=utf-8
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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


def macro_matr_ajou_ops(self,MAILLAGE,GROUP_MA_FLUIDE,GROUP_MA_INTERF,MODELISATION,MODE_MECA,DEPL_IMPO,
                             NUME_DDL_GENE,MODELE_GENE,MATR_MASS_AJOU,MATR_AMOR_AJOU,MATR_RIGI_AJOU,
                             NOEUD_DOUBLE,FLUIDE,DDL_IMPO,DIST_REFE,SOLVEUR,INFO,AVEC_MODE_STAT,
                             MODE_STAT,MONO_APPUI,
                             FORC_AJOU,ECOULEMENT,**args):
  """
     Ecriture de la macro MACRO_MATR_AJOU
  """
  from Accas import _F
  import types
  import aster
  from Utilitai.Utmess     import  UTMESS
  ier=0
  
  # On importe les definitions des commandes a utiliser dans la macro
  DEFI_MATERIAU      =self.get_cmd('DEFI_MATERIAU')
  AFFE_MATERIAU      =self.get_cmd('AFFE_MATERIAU')
  AFFE_MODELE        =self.get_cmd('AFFE_MODELE')
  AFFE_CHAR_THER     =self.get_cmd('AFFE_CHAR_THER')
  CALC_MATR_AJOU     =self.get_cmd('CALC_MATR_AJOU')
  THER_LINEAIRE      =self.get_cmd('THER_LINEAIRE')
  CALC_FORC_AJOU     =self.get_cmd('CALC_FORC_AJOU')

  # La macro compte pour 1 dans la numerotation des commandes
  self.set_icmd(1)
 
  if len(FLUIDE)==1 :
     message=         '<I> <MACRO_MATR_AJOU> tout le domaine fluide specifie dans GROUP_MA_INTERF et GROUP_MA_FLUIDE \n'
     message=message+ '                      sera affecte par la masse volumique RHO = '+str(FLUIDE['RHO'])+' \n'
     aster.affiche('MESSAGE',message)
     if FLUIDE['GROUP_MA']!=None :
       message=         '<I> <MACRO_MATR_AJOU> cas fluide simple : le group_ma dans lequel vous affectez la masse \n'
       message=message+ 'volumique RHO doit etre la reunion de GROUP_MA_INTERF et GROUP_MA_FLUIDE. \n'
       aster.affiche('MESSAGE',message)
  else :
     for flu in FLUIDE :
         if flu['GROUP_MA']==None :
            UTMESS('F','MATRICE0_1')

  IOCFLU=len(FLUIDE)

#  ---------------------------------------------------------------
#  definition du materiau fluide par caracteristique
#  thermique equivalente

# CAS FLUIDE SIMPLE
  if IOCFLU==1 :
     __NOMMAT=DEFI_MATERIAU( THER = _F( LAMBDA = 1.0,
                                        RHO_CP = FLUIDE[0]['RHO']))
     __NOMCMA=AFFE_MATERIAU( MAILLAGE = MAILLAGE,
                             AFFE     = _F( GROUP_MA = (GROUP_MA_FLUIDE,GROUP_MA_INTERF),
                                            MATER    =  __NOMMAT),    )

#  ---------------------------------------------------------------
#  cas fluides multiples
  else :
     affmat=[]
     for flu in FLUIDE:
        __NOMMAT=DEFI_MATERIAU( THER = _F( LAMBDA = 1.0,
                                           RHO_CP = flu['RHO']))
        mfact=_F(GROUP_MA=flu['GROUP_MA'],MATER=__NOMMAT)
        affmat.append(mfact)

     __NOMCMA=AFFE_MATERIAU( MAILLAGE = MAILLAGE,
                             AFFE     = affmat  )

#  ---------------------------------------------------------------
#  commande AFFE_MODELE modele fluide
  __NOMFLU=AFFE_MODELE( MAILLAGE = MAILLAGE,
                        AFFE     = _F( GROUP_MA     = (GROUP_MA_FLUIDE,GROUP_MA_INTERF),
                                       MODELISATION = MODELISATION,
                                       PHENOMENE    = 'THERMIQUE'    ), )

#  ---------------------------------------------------------------
#  commande AFFE_MODELE modele interface
  __NOMINT=AFFE_MODELE( MAILLAGE = MAILLAGE,
                        AFFE     = _F( GROUP_MA     = GROUP_MA_INTERF,
                                       MODELISATION = MODELISATION,
                                       PHENOMENE    = 'THERMIQUE'    ), )

#  ---------------------------------------------------------------
#  commande AFFE_CHAR_THER condition de pression imposee
#  en un point ou un groupe du fluide
  affimp=[]
  nflui=0
  for DDL in DDL_IMPO :
     if DDL['PRES_FLUIDE']!=None :
        nflui=nflui+1
        if DDL['NOEUD']   !=None : mfact=_F(NOEUD   =DDL['NOEUD'],   TEMP=DDL['PRES_FLUIDE'])
        if DDL['GROUP_NO']!=None : mfact=_F(GROUP_NO=DDL['GROUP_NO'],TEMP=DDL['PRES_FLUIDE'])
        affimp.append(mfact)
  if nflui==0:
     UTMESS('F','MATRICE0_2')

  __CHARGE=AFFE_CHAR_THER( MODELE    = __NOMFLU,
                           TEMP_IMPO = affimp )

#  ---------------------------------------------------------------
#  calcul des masses, rigidites et amortissements ajoutes en theorie
#  potentielle
#  commande CALC_MATR_AJOU, calcul de la masse ajoutee

  if MATR_MASS_AJOU!=None :
     self.DeclareOut('MASSAJ',MATR_MASS_AJOU)
     solveur=SOLVEUR[0].cree_dict_valeurs(SOLVEUR[0].mc_liste)
     mostcles={}
     if   NUME_DDL_GENE!=None : mostcles['NUME_DDL_GENE'] =NUME_DDL_GENE
     if   INFO         !=None : mostcles['INFO']          =INFO
     if   MODE_MECA    !=None : mostcles['MODE_MECA']     =MODE_MECA
     elif DEPL_IMPO    !=None : mostcles['CHAM_NO']       =DEPL_IMPO
     elif MODELE_GENE  !=None :
                                mostcles['MODELE_GENE']   =MODELE_GENE
                                mostcles['AVEC_MODE_STAT']=AVEC_MODE_STAT
                                mostcles['DIST_REFE']     =DIST_REFE
     if   NOEUD_DOUBLE !=None : mostcles['NOEUD_DOUBLE']  =NOEUD_DOUBLE

     MASSAJ = CALC_MATR_AJOU(MODELE_FLUIDE    = __NOMFLU,
                             MODELE_INTERFACE = __NOMINT,
                             CHARGE           = __CHARGE,
                             CHAM_MATER       = __NOMCMA,
                             OPTION           = 'MASS_AJOU',
                             SOLVEUR          = solveur,
                             **mostcles)

#  ---------------------------------------------------------------
#  calcul de l amortissement ajoute
  if (MATR_AMOR_AJOU!=None ) or (MATR_RIGI_AJOU!=None ):

#  ---------------------------------------------------------------
#  on definit un nouveau modele fluide pour calculer
#  le potentiel stationnaire - AFFE_MODELE
     grma=[GROUP_MA_FLUIDE,]
     if ECOULEMENT!=None :
        grma.append(ECOULEMENT['GROUP_MA_1'])
        grma.append(ECOULEMENT['GROUP_MA_2'])
     __NOFLUI=AFFE_MODELE( MAILLAGE = MAILLAGE,
                           AFFE     = _F( GROUP_MA     = grma,
                                          MODELISATION = MODELISATION,
                                          PHENOMENE    = 'THERMIQUE'    ), )
     affimp=[]
     for DDL in DDL_IMPO :
        if DDL['PRES_SORTIE']!=None :
           if DDL['NOEUD']   !=None : mfact=_F(NOEUD   =DDL['NOEUD'],   TEMP=DDL['PRES_SORTIE'])
           if DDL['GROUP_NO']!=None : mfact=_F(GROUP_NO=DDL['GROUP_NO'],TEMP=DDL['PRES_SORTIE'])
           affimp.append(mfact)

     affecl=[]
     for ECL in ECOULEMENT :
           mfact=_F(GROUP_MA=ECL['GROUP_MA_1'],FLUN=ECL['VNOR_1'])
           affecl.append(mfact)
           mfact=_F(GROUP_MA=ECL['GROUP_MA_2'],FLUN=ECL['VNOR_2'])
           affecl.append(mfact)
     __CHARG2=AFFE_CHAR_THER( MODELE    = __NOFLUI,
                              TEMP_IMPO = affimp ,
                              FLUX_REP  = affecl )

     __POTEN = THER_LINEAIRE( MODELE     = __NOFLUI,
                              CHAM_MATER = __NOMCMA ,
                              EXCIT      = _F( CHARGE = __CHARG2 ) )

#  ---------------------------------------------------------------
#  calcul amortissement proprement dit
  if MATR_AMOR_AJOU!=None :
     self.DeclareOut('AMORAJ',MATR_AMOR_AJOU)
     solveur=SOLVEUR[0].cree_dict_valeurs(SOLVEUR[0].mc_liste)
     mostcles={}
     if   NUME_DDL_GENE!=None : mostcles['NUME_DDL_GENE'] =NUME_DDL_GENE
     if   INFO         !=None : mostcles['INFO']          =INFO
     if   MODE_MECA    !=None : mostcles['MODE_MECA']     =MODE_MECA
     elif DEPL_IMPO    !=None : mostcles['CHAM_NO']       =DEPL_IMPO
     else :
       UTMESS('F','MATRICE0_3')

     AMORAJ = CALC_MATR_AJOU(MODELE_FLUIDE    = __NOMFLU,
                             MODELE_INTERFACE = __NOMINT,
                             CHARGE           = __CHARGE,
                             CHAM_MATER       = __NOMCMA,
                             OPTION           = 'AMOR_AJOU',
                             SOLVEUR          = solveur,
                             POTENTIEL        = __POTEN,
                             **mostcles)

#  ---------------------------------------------------------------
#  calcul de la rigidite ajoutee
  if MATR_RIGI_AJOU!=None :
     self.DeclareOut('RIGIAJ',MATR_RIGI_AJOU)
     solveur=SOLVEUR[0].cree_dict_valeurs(SOLVEUR[0].mc_liste)
     mostcles={}
     if   NUME_DDL_GENE!=None : mostcles['NUME_DDL_GENE'] =NUME_DDL_GENE
     if   INFO         !=None : mostcles['INFO']          =INFO
     if   MODE_MECA    !=None : mostcles['MODE_MECA']     =MODE_MECA
     elif DEPL_IMPO    !=None : mostcles['CHAM_NO']       =DEPL_IMPO
     else :
       UTMESS('F','MATRICE0_4')

     RIGIAJ = CALC_MATR_AJOU(MODELE_FLUIDE    = __NOMFLU,
                             MODELE_INTERFACE = __NOMINT,
                             CHARGE           = __CHARGE,
                             CHAM_MATER       = __NOMCMA,
                             OPTION           = 'RIGI_AJOU',
                             SOLVEUR          = solveur,
                             POTENTIEL        = __POTEN,
                             **mostcles)

#  ---------------------------------------------------------------
#  boucle sur le nombre de vecteurs a projeter, commande CALC_FORC_AJOU
  if FORC_AJOU!=None :
     for FORCAJ in FORC_AJOU:
       self.DeclareOut('VECTAJ',FORCAJ['VECTEUR'])
       solveur=SOLVEUR[0].cree_dict_valeurs(SOLVEUR[0].mc_liste)
       mostcles={}
       if   NUME_DDL_GENE!=None : mostcles['NUME_DDL_GENE'] =NUME_DDL_GENE
       if   MODE_MECA    !=None : mostcles['MODE_MECA']     =MODE_MECA
       elif MODELE_GENE  !=None :
                                  mostcles['MODELE_GENE']   =MODELE_GENE
                                  mostcles['AVEC_MODE_STAT']=AVEC_MODE_STAT
                                  mostcles['DIST_REFE']     =DIST_REFE
       if   NOEUD_DOUBLE !=None : mostcles['NOEUD_DOUBLE']  =NOEUD_DOUBLE
       if   MODE_STAT    !=None :
         mostcles['MODE_STAT']                                =MODE_STAT
         if FORCAJ['NOEUD']    !=None : mostcles['NOEUD']     =FORCAJ['NOEUD']
         if FORCAJ['GROUP_NO'] !=None : mostcles['GROUP_NO']  =FORCAJ['GROUP_NO']
       else                     :
                                  mostcles['MONO_APPUI']    =MONO_APPUI

       VECTAJ = CALC_FORC_AJOU(DIRECTION        = FORCAJ['DIRECTION'],
                               MODELE_FLUIDE    = __NOMFLU,
                               MODELE_INTERFACE = __NOMINT,
                               CHARGE           = __CHARGE,
                               CHAM_MATER       = __NOMCMA,
                               SOLVEUR          = solveur,
                               **mostcles)


  return ier
