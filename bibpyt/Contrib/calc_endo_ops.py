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


def calc_endo_ops(self,MODELE,CHAM_MATER,CARA_ELEM,EXCIT,
                                COMPORTEMENT,ETAT_INIT,INCREMENT,
                                CONVERGENCE,PILOTAGE,SOLVEUR,
                                ARCHIVAGE,INFO,TITRE,IMPR,**args):
                                                                    
                                  
  """
     Calcul d'endommagement a partir d'un champ d'endommagement initial
     sur une sequence de pas de pilotage
  """
  
  import math
  import aster
  from Accas import _F

  
  
  # -------------------------------------------------------------
  # 1. Initialisation
  # -------------------------------------------------------------

  # Code retour a priori
  ier=0

  # On importe les definitions des commandes a utiliser dans la macro
  DEFI_CONSTANTE   = self.get_cmd('DEFI_CONSTANTE')
  DEFI_FONCTION    = self.get_cmd('DEFI_FONCTION')
  LIRE_CHAMP       = self.get_cmd('LIRE_CHAMP')
  CREA_CHAMP       = self.get_cmd('CREA_CHAMP')
  DEFI_LIST_REEL   = self.get_cmd('DEFI_LIST_REEL')
  DEFI_LIST_INST   = self.get_cmd('DEFI_LIST_INST')
  STAT_NON_LINE    = self.get_cmd('STAT_NON_LINE')
  IMPR_RESU        = self.get_cmd('IMPR_RESU')

  # La macro compte pour 1 dans la numerotation des commandes
  self.set_icmd(1)

  # Le concept sortant (de type evol_noli) est nomme EVOL 
  self.DeclareOut('EVOL',self.sd)

  # On se prepare a manipuler le mot-cle facteur SOLVEUR
  assert len(SOLVEUR) == 1
  solveur = SOLVEUR[0].cree_dict_toutes_valeurs()
  solvsym = SOLVEUR[0].cree_dict_toutes_valeurs()
  solvsym['SYME'] = 'OUI'
  
  

  # -------------------------------------------------------------
  # 2. Read the initial damage field
  # -------------------------------------------------------------  
  
  initial = False
  formerTangent = False
  
  if ETAT_INIT <> None:
    unite = ETAT_INIT[0]['UNITE']
    nomCham = ETAT_INIT[0]['NOM_CHAM_MED']

    # Recuperation du maillage a partir du modele
    modelName   = MODELE.get_name().ljust(8)
    pointerMesh =  modelName + '.MODELE    .LGRF        '
    meshName    = aster.getvectjev(pointerMesh)[0].strip()
    MAILLAGE    = self.get_concept(meshName)

    # Lecture du champ de variables internes (avec detection si absent)
    try:    
      __VBLOC = LIRE_CHAMP(
        TYPE_CHAM    = 'ELGA_VARI_R',
        NOM_MED      =  nomCham,
        MAILLAGE     =  MAILLAGE,
        MODELE       =  MODELE,
        PROL_ZERO    = 'NON',               
        NOM_CMP_IDEM = 'OUI',
        UNITE        =  unite,
        INFO         =  1,
        )
      initial = True
      
    except aster.error as err:
      errText = err.basic_format()
      if eval(errText)[0][0] == "MED_32":   # exception MED_32 (field does not exist in MED file)
        aster.affiche("MESSAGE","")
        aster.affiche("MESSAGE","==> Initialisation avec un état nul (endommagement absent)")
        aster.affiche("MESSAGE","")
      else:
        raise err

     
    # Resize the internal variable field according to the constitutive laws
    if initial :
      asse   = Extract(COMPORTEMENT,__VBLOC)
      __PROV_VARI = CREA_CHAMP(TYPE_CHAM='ELGA_VARI_R', MODELE=MODELE, OPERATION='ASSE', ASSE=asse)
           
      
      
  # -------------------------------------------------------------
  # 3. Initial equilibrium with zero force
  # -------------------------------------------------------------
  
  if initial:
    
    # Single time step
    __INST_1 = DEFI_LIST_REEL(DEBUT=-2,INTERVALLE=_F(JUSQU_A=0,NOMBRE=2))
   
   
    # Create a loading of zero amplitude
    __ZERO = DEFI_CONSTANTE(VALE=0)
   
    loading_0 = [excit.cree_dict_toutes_valeurs() for excit in EXCIT]
    for excit in loading_0:
      try:
        if excit['TYPE_CHARGE'] == 'FIXE_PILO':
          excit['TYPE_CHARGE'] = 'FIXE_CSTE'
          excit['FONC_MULT'] = __ZERO
      except KeyError:
        pass
     

    # More accurate convergence criterion (also used for a refined path-following)
    cvg = CONVERGENCE[0].cree_dict_toutes_valeurs()
    cvg['RESI_REFE_RELA'] = min(1.E-6,cvg['RESI_REFE_RELA'])

    
    # Equilibrium recovery with zero force
    para = dict(
      ETAT_INIT    = _F(VARI=__PROV_VARI, INST=-2),
      MODELE       = MODELE,
      CHAM_MATER   = CHAM_MATER,
      EXCIT        = loading_0,
      COMPORTEMENT = [comportement.cree_dict_toutes_valeurs() for comportement in COMPORTEMENT],
      CONVERGENCE  = cvg,
      SOLVEUR      = solvsym,
      INCREMENT    = _F(LIST_INST=__INST_1, INST_FIN=-1),
      NEWTON       = _F(MATRICE='TANGENTE', PAS_MINI_ELAS=2, REAC_ITER_ELAS=0),    # secant matrix
      )

    if CARA_ELEM <> None:
      para['CARA_ELEM'] = CARA_ELEM
      
    __UNLOAD = STAT_NON_LINE(**para)
          
    __UNLOAD_DEPL = CREA_CHAMP(OPERATION='EXTR', TYPE_CHAM='NOEU_DEPL_R', RESULTAT=__UNLOAD, INST=-1, NOM_CHAM='DEPL')
    __UNLOAD_SIGM = CREA_CHAMP(OPERATION='EXTR', TYPE_CHAM='ELGA_SIEF_R', RESULTAT=__UNLOAD, INST=-1, NOM_CHAM='SIEF_ELGA')
    __UNLOAD_VARI = CREA_CHAMP(OPERATION='EXTR', TYPE_CHAM='ELGA_VARI_R', RESULTAT=__UNLOAD, INST=-1, NOM_CHAM='VARI_ELGA')
          
          
          
  # -------------------------------------------------------------
  # 4. Equilibrium phase to reach the damage threshold
  # -------------------------------------------------------------
  
          
    # 4.1. First strategy: path-following to reach the damage threshold
    
    # Accuracy around the damage threshold
    da = 1.E-8  
    pilo = PILOTAGE[0].cree_dict_toutes_valeurs()
    pilo['COEF_MULT'] = 1/da  
    
    cmprt = [comportement.cree_dict_toutes_valeurs() for comportement in COMPORTEMENT]
    for compor in cmprt:
      if compor['RELATION'] == 'ENDO_FISS_EXP':
        compor['RESI_INTE_RELA'] = da/10
        compor['ITER_INTE_MAXI'] = 1000
     
     
    # Use of the secant operator for the unilateral part of ENDO_FISS_EXP  
    asse = Extract(COMPORTEMENT, __UNLOAD_VARI, __PROV_VARI, ENDO_FISS_EXP=('EPSEXX','EPSEYY','EPSEZZ','EPSEXY','EPSEXZ','EPSEYZ'))
    __UNLOAD_VARI_MOD = CREA_CHAMP(TYPE_CHAM='ELGA_VARI_R', MODELE=MODELE, OPERATION='ASSE', ASSE=asse)


    # Minimal number of iterations
    cvg = CONVERGENCE[0].cree_dict_toutes_valeurs() 
    cvg['ITER_GLOB_ELAS'] = 6
    cvg['ARRET'] = 'NON'
    
    
    # Computation
    #TODO : ideally, elastic (secant ?) predictor, then tangent corrector (seemingly not available) --> through INDIENDO ? What about stiffness recovery ?
    
    para = dict(
      ETAT_INIT    = _F(DEPL=__UNLOAD_DEPL, SIGM=__UNLOAD_SIGM, VARI=__UNLOAD_VARI_MOD, INST=-1),
      MODELE       = MODELE,
      CHAM_MATER   = CHAM_MATER,
      EXCIT        = [excit.cree_dict_toutes_valeurs() for excit in EXCIT],
      COMPORTEMENT = cmprt,
      PILOTAGE     = pilo,
      CONVERGENCE  = cvg,
      SOLVEUR      = solvsym,
      INCREMENT    = _F(LIST_INST=__INST_1, INST_INIT=-1, INST_FIN=0),
      NEWTON       = _F(MATRICE='TANGENTE', REAC_ITER=1,PAS_MINI_ELAS=2, REAC_ITER_ELAS=0),        # secant matrix
      ARCHIVAGE    = _F(INST=0),
      )
  
    if CARA_ELEM <> None: para['CARA_ELEM'] = CARA_ELEM
    
    __INIT = STAT_NON_LINE(**para)

    iteglob = __INIT.LIST_PARA()['ITER_GLOB'][-1]
    
    # Convergence
    if iteglob <= 5:
    
      __INIT_DEPL = CREA_CHAMP(OPERATION='EXTR', TYPE_CHAM='NOEU_DEPL_R', RESULTAT=__INIT, INST=0, NOM_CHAM='DEPL')
      __INIT_SIGM = CREA_CHAMP(OPERATION='EXTR', TYPE_CHAM='ELGA_SIEF_R', RESULTAT=__INIT, INST=0, NOM_CHAM='SIEF_ELGA')
      __INIT_VARI = CREA_CHAMP(OPERATION='EXTR', TYPE_CHAM='ELGA_VARI_R', RESULTAT=__INIT, INST=0, NOM_CHAM='VARI_ELGA')
      formerTangent = True
      

    else:
        
    # 4.2. Second strategy: Estimation of the load amplitude and direct computation
    
      
      # Estimation of the load amplitude to reach the damage threshold
            
      eta = __INIT.LIST_PARA()['ETA_PILOTAGE'][-1]
    

      # Direct computation up to the amplitude eta without damaging

      __RAMPE = DEFI_FONCTION(
          NOM_PARA = 'INST',
          VALE     = (-1,0,0,eta*0.99),  # some margin in case of instability
          )

      loading_1 = [excit.cree_dict_toutes_valeurs() for excit in EXCIT]
      for excit in loading_1:
        try:
          if excit['TYPE_CHARGE'] == 'FIXE_PILO':
            excit['TYPE_CHARGE'] = 'FIXE_CSTE'
            excit['FONC_MULT'] = __RAMPE
        except KeyError:
          pass


      # substepping and rule for stopping the computation when damage starts growing
      damax = 1.0/PILOTAGE[0]['COEF_MULT']
      
      __DISCRETE = DEFI_LIST_INST(
        DEFI_LIST = _F(VALE = (-1.0, 0.0)),
        ECHEC = (
          _F(EVENEMENT='ERREUR', SUBD_PAS=4, SUBD_NIVEAU=3),
          _F(EVENEMENT='DELTA_GRANDEUR', VALE_REF=damax, NOM_CHAM='VARI_ELGA', NOM_CMP='V1', SUBD_PAS=4, SUBD_NIVEAU=5),
          ),
        )
      
      # computation
      para = dict(
        ETAT_INIT    = _F(DEPL=__UNLOAD_DEPL, SIGM=__UNLOAD_SIGM, VARI=__UNLOAD_VARI_MOD, INST=-1),
        MODELE       = MODELE,
        CHAM_MATER   = CHAM_MATER,
        EXCIT        = loading_1,
        COMPORTEMENT = cmprt,
        CONVERGENCE  = CONVERGENCE[0].cree_dict_toutes_valeurs(),
        SOLVEUR      = solveur,
        INCREMENT    = _F(LIST_INST=__DISCRETE, INST_INIT=-1, INST_FIN=0),
        NEWTON       = _F(MATRICE='TANGENTE', REAC_ITER=1),        
        ARCHIVAGE    = _F(INST=0),
        )
      if CARA_ELEM <> None: para['CARA_ELEM'] = CARA_ELEM
        

      try:
        __INIT = __UNLOAD
        STAT_NON_LINE(reuse=__INIT, **para)
        
        #TODO : take the max of the damage increment in order to know whether damage took place or not (stiffness recovery only)
        # if no (significant ?) damage, formerTangent = True, else formerTangent = false
        formerTangent = True 

      except aster.ActionError:
        formerTangent = True
      
      tfin = __INIT.LIST_VARI_ACCES()['INST'][-1]
      
      __INIT_DEPL = CREA_CHAMP(OPERATION='EXTR', TYPE_CHAM='NOEU_DEPL_R', RESULTAT=__INIT, INST=tfin, NOM_CHAM='DEPL')
      __INIT_SIGM = CREA_CHAMP(OPERATION='EXTR', TYPE_CHAM='ELGA_SIEF_R', RESULTAT=__INIT, INST=tfin, NOM_CHAM='SIEF_ELGA')
      __INIT_VARI = CREA_CHAMP(OPERATION='EXTR', TYPE_CHAM='ELGA_VARI_R', RESULTAT=__INIT, INST=tfin, NOM_CHAM='VARI_ELGA')

    
    
  # -------------------------------------------------------------
  # 5. Computation over the whole sequence
  # -------------------------------------------------------------


  # Use of the  tangent operator provided by the initial internal variable field
  if initial and formerTangent:
    
    asse = Extract(COMPORTEMENT,__INIT_VARI, __PROV_VARI, ENDO_FISS_EXP=('INDIENDO',))
    __INIT_VARI_MOD = CREA_CHAMP(TYPE_CHAM='ELGA_VARI_R', MODELE=MODELE, OPERATION='ASSE', ASSE=asse)
        

  # Time steps with increment 1 so that path following leads to damage increments of 0.1
  nbrStep = INCREMENT[0]['NOMBRE']
  __SEQU = DEFI_LIST_REEL(DEBUT=0, INTERVALLE=_F(JUSQU_A=nbrStep,NOMBRE=nbrStep))


  # Number of sub-stepping conditionned by the residual precision criterion
  prec  = CONVERGENCE[0]['RESI_REFE_RELA']/10.0
  level = int(2+math.log(0.1/prec)/math.log(4.0))
  elasInc = 4.0**(-level) * 0.5
  
  __SUB = DEFI_LIST_INST(
        DEFI_LIST = _F(LIST_INST=__SEQU),
        ECHEC     = _F(SUBD_PAS=4, SUBD_NIVEAU=level+2),
    )


  # Computation
  para = dict(
    MODELE       = MODELE,
    CHAM_MATER   = CHAM_MATER,
    EXCIT        = [excit.cree_dict_toutes_valeurs() for excit in EXCIT],
    COMPORTEMENT = [comportement.cree_dict_toutes_valeurs() for comportement in COMPORTEMENT],
    PILOTAGE     = [pilotage.cree_dict_toutes_valeurs() for pilotage in PILOTAGE],
    CONVERGENCE  = [convergence.cree_dict_toutes_valeurs() for convergence in CONVERGENCE],
    SOLVEUR      = solveur,
    INCREMENT    =_F(LIST_INST=__SUB, INST_INIT=0),
    NEWTON       =_F(MATRICE='TANGENTE', REAC_ITER=1, REAC_ITER_ELAS=0, PAS_MINI_ELAS=elasInc),
    )

  if CARA_ELEM <> None: para['CARA_ELEM'] = CARA_ELEM
      
  if initial:
    if formerTangent:
      para['ETAT_INIT'] = _F(INST=0, DEPL=__INIT_DEPL, SIGM=__INIT_SIGM, VARI=__INIT_VARI_MOD)
    else:
      para['ETAT_INIT'] = _F(INST=0, DEPL=__INIT_DEPL, SIGM=__INIT_SIGM, VARI=__INIT_VARI)
  
  EVOL = STAT_NON_LINE(**para)
  
  
  
  # -------------------------------------------------------------
  # 6. Impression des résultats au format MED
  # -------------------------------------------------------------

  if IMPR <> None:
    unite = IMPR[0]['UNITE']
    filtre = IMPR[0]['FILTRE']
    
    if filtre == 'OUI':
      
      chk3d = 'DZ' in EVOL.LIST_NOM_CMP()['DEPL']
      
      if chk3d:
        IMPR_RESU(
          UNITE = unite,
          FORMAT = 'MED',
          RESU   = (
            _F(RESULTAT = EVOL, NOM_CHAM='DEPL',      NOM_CHAM_MED='DISPLACEMENT', NOM_CMP=('DX','DY','DZ')),
            _F(RESULTAT = EVOL, NOM_CHAM='SIEF_ELGA', NOM_CHAM_MED='STRESS',       NOM_CMP=('SIXX','SIYY','SIZZ','SIXY','SIXZ','SIYZ')),
            _F(RESULTAT = EVOL, NOM_CHAM='VARI_ELGA', NOM_CHAM_MED='VARI',         ),
            ),
          )
          
      else :
        IMPR_RESU(
          UNITE = unite,
          FORMAT = 'MED',
          RESU   = (
            _F(RESULTAT = EVOL, NOM_CHAM='DEPL',      NOM_CHAM_MED='DISPLACEMENT', NOM_CMP=('DX','DY')),
            _F(RESULTAT = EVOL, NOM_CHAM='SIEF_ELGA', NOM_CHAM_MED='STRESS',       NOM_CMP=('SIXX','SIYY','SIZZ','SIXY')),
            _F(RESULTAT = EVOL, NOM_CHAM='VARI_ELGA', NOM_CHAM_MED='VARI',         ),
            ),
          )
            
    else:
      IMPR_RESU(
        UNITE = unite,
        FORMAT = 'MED',
        RESU   = (
          _F(RESULTAT = EVOL, NOM_CHAM='DEPL',      NOM_CHAM_MED='DEPL',    ),
          _F(RESULTAT = EVOL, NOM_CHAM='SIEF_ELGA', NOM_CHAM_MED='SIEF_ELGA'),
          _F(RESULTAT = EVOL, NOM_CHAM='VARI_ELGA', NOM_CHAM_MED='VARI',    ),
          ),
        )
      
  return ier
  
  
  
  
def Extract(COMPORTEMENT, baseField, cplField=None, **components):
  """
  Take some components of cplField, insert them into baseField and provide the result
  in terms of the parameters ASSE for CREA_CHAMP. Both fields are internal variable fields. 
  The list of components to replace is defined for each constitutive law. The constitutive 
  law are distributed on the mesh through the keyword Compor. If cplField is none, then 
  baseField is only resized according to the number of internal variables for each constitutive 
  law.
  """

  from Accas import _F

  
  # 1 - Access to the properties of the constitutive laws
  
  laws = []
  for compor in COMPORTEMENT:
    lawName = compor['RELATION'].lower()
    mdlName = 'Comportement.%s' % lawName                   # module name corresponding to the constitutive law
    lawMdl = __import__(mdlName,fromlist=[1])               # module corresponding to the constitutive law
    laws.append(lawMdl.loi)
  
  
  # 2 - Definition of the component lists for each constitutive law
  
  baseCmpsList = []
  cplCmpsList  = []
  
  for (compor,law) in zip(COMPORTEMENT,laws):
    nbvi    = law.get_nb_vari()                             # number of internal variables of the constitutive law
    viNames = law.get_nom_vari()
    viNbrs  = tuple(['V%d'%(i+1) for i in range(nbvi)])

    baseCmps = list(viNbrs)
    cplCmps  = []

    lawName = compor['RELATION']
    if cplField <> None and lawName in components.keys():
      for cmpName in components[lawName]:
        pos = viNames.index(cmpName)
        nbr = 'V%d' % (pos+1)
        cplCmps.append(nbr)
        baseCmps.remove(nbr)

    baseCmpsList.append(baseCmps)
    cplCmpsList.append(cplCmps)
    
    
  # 3 - Field assembly
  
  asse = []
  for (compor,baseCmps,cplCmps) in zip(COMPORTEMENT,baseCmpsList,cplCmpsList):
    #print "baseCmps = ",baseCmps
    #print "cplCmps  = ",cplCmps
    
    kwds = [ _F(CHAM_GD=baseField, NOM_CMP=baseCmps, NOM_CMP_RESU=baseCmps) ]
    if len(cplCmps) <> 0:
      kwds += [ _F(CHAM_GD=cplField, NOM_CMP=cplCmps, NOM_CMP_RESU=cplCmps) ]
    
    gma = compor['GROUP_MA']
    for kwd in kwds:
      if gma == None:
        kwd.update( _F(TOUT='OUI') )
      else:
        kwd.update( _F(GROUP_MA=gma) )
      
    asse += kwds
  #print "asse = ",asse
  return tuple(asse)
  
  
  
## Frozen damage : constitutive relation with frozen damage (should exist)
#dealtLaw = ('ENDO_FISS_EXP',)
#compor = [comportement.cree_dict_toutes_valeurs() for comportement in COMPORTEMENT]
#for occ in compor:
  #if occ['RELATION'] in dealtLaw: # for other laws, nothing is done (elasticity OK)
    #occ['ALGO_INTE'] = 'FIGE_VARI'
        
   
    
    

  
