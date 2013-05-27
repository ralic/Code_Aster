
#            CONFIGURATION MANAGEMENT OF EDF VERSION
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
#-*- coding: iso-8859-1 -*-


def calc_modal_ops(self,MODELE,CHAM_MATER,CARA_ELEM,AMORTISSEMENT,
                        CHARGE,INST,METHODE,CALC_FREQ, MODE_RIGIDE,
                        VERI_MODE,INFO,**args):
  """
     Ecriture de la macro CALC_MODAL
  """
  from Accas import _F
  ier=0
  from Utilitai.Utmess     import  UTMESS

  # On importe les definitions des commandes a utiliser dans la macro
  # Le nom de la variable doit etre obligatoirement le nom de la commande
  CALC_MATR_ELEM=self.get_cmd('CALC_MATR_ELEM')
  NUME_DDL      =self.get_cmd('NUME_DDL')
  ASSE_MATRICE  =self.get_cmd('ASSE_MATRICE')  
  MODE_ITER_SIMULT  =self.get_cmd('MODE_ITER_SIMULT')
  # La macro compte pour 1 dans la numerotation des commandes
  self.set_icmd(1)


  # on defini la liste des mots cle pour les appels aux CALC_MATR_ELEM
  motsclece={}
  if CHARGE     != None: motsclece['CHARGE']      =CHARGE
  if CHAM_MATER != None: motsclece['CHAM_MATER']  =CHAM_MATER
  if CARA_ELEM  != None: motsclece['CARA_ELEM']   =CARA_ELEM
  if INST       != None: motsclece['INST']        =INST

  #c'est avec le mot cle AMORTISSEMENT qu'on decide si on calcule la matrice C
  # d'amortissement 

  
  _a=CALC_MATR_ELEM(MODELE=MODELE, OPTION='RIGI_MECA', **motsclece)
  _b=CALC_MATR_ELEM(MODELE=MODELE, OPTION='MASS_MECA', **motsclece)
  if AMORTISSEMENT=='OUI':  
    _c=CALC_MATR_ELEM(MODELE=MODELE, OPTION='AMOR_MECA',
                       RIGI_MECA=_a, MASS_MECA=_b,**motsclece)

  #on produit en local le concept NUME_DDL, il n'est pas visible pour l'utilisateur

  _num=NUME_DDL(MATR_RIGI=_a,INFO=INFO)
  
  #assemblages des matrices 
  _rigas=ASSE_MATRICE(MATR_ELEM=_a,NUME_DDL=_num)
  _masas=ASSE_MATRICE(MATR_ELEM=_b,NUME_DDL=_num)
  if AMORTISSEMENT=='OUI':     
    _amoras=ASSE_MATRICE(MATR_ELEM=_c,NUME_DDL=_num)

  #lancement du calcul des modes propres
  # on defini la liste des mots cle pour l'appel au MODE_ITER_SIMULT

  motscit={}
  motscfa={}

  if METHODE=='TRI_DIAG':
     if args.has_key('NMAX_ITER_ORTHO'):
        motscit['NMAX_ITER_ORTHO'] =args['NMAX_ITER_ORTHO']
     if args.has_key('PREC_ORTHO'):
        motscit['PREC_ORTHO']      =args['PREC_ORTHO']
     if args.has_key('PREC_LANCZOS'):
        motscit['PREC_LANCZOS']    =args['PREC_LANCZOS']
     if args.has_key('MAX_ITER_QR'):
        motscit['NMAX_ITER_QR']    =args['NMAX_ITER_QR']
  elif METHODE=='JACOBI':
     if args.has_key('NMAX_ITER_BATHE'):
        motscit['NMAX_ITER_BATHE'] =args['NMAX_ITER_BATHE']
     if args.has_key('PREC_BATHE'):
        motscit['PREC_BATHE']      =args['PREC_BATHE']
     if args.has_key('NMAX_ITER_JACOBI'):
        motscit['NMAX_ITER_JACOBI']=args['NMAX_ITER_JACOBI']
     if args.has_key('PREC_JACOBI'):
        motscit['PREC_JACOBI']     =args['PREC_JACOBI']
  elif METHODE=='SORENSEN':
     if args.has_key('NMAX_ITER_SOREN'):
        motscit['NMAX_ITER_SOREN'] =args['NMAX_ITER_SOREN']
     if args.has_key('PARA_ORTHO_SOREN'):
        motscit['PARA_ORTHO_SOREN']=args['PARA_ORTHO_SOREN']
     if args.has_key('PREC_SOREN'):
        motscit['PREC_SOREN']      =args['PREC_SOREN']
  elif METHODE=='QZ':
     if args.has_key('TYPE_QZ'):
        motscit['TYPE_QZ'] =args['TYPE_QZ']
  
  if CALC_FREQ['DIM_SOUS_ESPACE']: motscfa['DIM_SOUS_ESPACE']=CALC_FREQ['DIM_SOUS_ESPACE']
  if CALC_FREQ['COEF_DIM_ESPACE']: motscfa['COEF_DIM_ESPACE']=CALC_FREQ['COEF_DIM_ESPACE']
  
  if CALC_FREQ['OPTION']==('PLUS_PETITE' or 'PLUS_GRANDE'):
     motscfa['NMAX_FREQ']=CALC_FREQ['NMAX_FREQ']

  if CALC_FREQ['OPTION']=='CENTRE':
     motscfa['FREQ']=CALC_FREQ['FREQ']
     if CALC_FREQ['AMOR_REDUIT']: motscfa['AMOR_REDUIT']=CALC_FREQ['AMOR_REDUIT']
     motscfa['NMAX_FREQ']=CALC_FREQ['NMAX_FREQ']
     
  if CALC_FREQ['OPTION']=='BANDE':
     motscfa['FREQ']=CALC_FREQ['FREQ']
     
  motscit['CALC_FREQ'] = _F(OPTION          =CALC_FREQ['OPTION'],
                            SEUIL_FREQ      =CALC_FREQ['SEUIL_FREQ'],
                            NMAX_ITER_SHIFT =CALC_FREQ['NMAX_ITER_SHIFT'],
                            PREC_SHIFT      =CALC_FREQ['PREC_SHIFT'],
                            APPROCHE        =CALC_FREQ['APPROCHE'],
                            **motscfa)

  motscit['VERI_MODE'] = _F(STOP_ERREUR=VERI_MODE['STOP_ERREUR'],
                            SEUIL      =VERI_MODE['SEUIL'],
                            STURM      =VERI_MODE['STURM'],
                            PREC_SHIFT =VERI_MODE['PREC_SHIFT'])

  motscit['STOP_BANDE_VIDE'] = CALC_FREQ['STOP_BANDE_VIDE']

 
  if MODE_RIGIDE=='OUI':
    mode_rigi='MODE_RIGIDE'
  elif MODE_RIGIDE=='NON':
    mode_rigi='SANS' 
 
  self.DeclareOut('modes',self.sd)
  
  if AMORTISSEMENT=='NON':
     modes=MODE_ITER_SIMULT(MATR_RIGI  =_rigas,
                            MATR_MASS  =_masas,
                            METHODE    =METHODE,
                            OPTION     =mode_rigi,
                            INFO       =INFO,
                            **motscit)
  elif AMORTISSEMENT=='OUI':
     modes=MODE_ITER_SIMULT(MATR_RIGI  =_rigas,
                            MATR_MASS  =_masas,
                            MATR_AMOR  =_amoras,
                            METHODE    =METHODE,
                            OPTION     =mode_rigi,
                            INFO       =INFO,
                            **motscit)
 
  return ier
