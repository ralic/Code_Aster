#@ MODIF macro_mode_meca_ops Macro  DATE 11/09/2002   AUTEUR VABHHTS J.PELLET 
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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

def macro_mode_meca_ops(self,MATR_A,MATR_B,INFO,METHODE,OPTION,CALC_FREQ,
                        VERI_MODE,NORM_MODE,FILTRE_MODE,IMPRESSION,**args):
  """
     Ecriture de la macro MACRO_MODE_MECA
  """
  from Accas import _F
  ier=0
    
  #  on protege le contenu du mot cle NORM_MODE pour eviter les confusions
  #  avec la commande du meme nom
  
  normode=NORM_MODE
  
  # On importe les definitions des commandes a utiliser dans la macro
  MODE_ITER_SIMULT  =self.get_cmd('MODE_ITER_SIMULT')
  NORM_MODE         =self.get_cmd('NORM_MODE')
  IMPR_RESU         =self.get_cmd('IMPR_RESU')
  EXTR_MODE         =self.get_cmd('EXTR_MODE')
  # La macro compte pour 1 dans la numerotation des commandes
  self.icmd=1

  nompro=None
  iocc=0
  if CALC_FREQ['FREQ']:
     nnfreq=len(CALC_FREQ['FREQ'])
  else:
     lborne=[]
     nnfreq= CALC_FREQ['NB_BLOC_FREQ']+1
     zlborn=(CALC_FREQ['FREQ_MAX']-CALC_FREQ['FREQ_MIN'])/(nnfreq-1)
     for i in range(0,nnfreq):
         lborne.append(CALC_FREQ['FREQ_MIN']+i*zlborn)

  motscles={}
  motscles['FILTRE_MODE']=[]
  for i in range(0,nnfreq-1):
     motscit={}
     motscfa={}
     if CALC_FREQ['DIM_SOUS_ESPACE']: motscfa['DIM_SOUS_ESPACE']=CALC_FREQ['DIM_SOUS_ESPACE']
     if CALC_FREQ['COEF_DIM_ESPACE']: motscfa['COEF_DIM_ESPACE']=CALC_FREQ['COEF_DIM_ESPACE']
     if CALC_FREQ['FREQ']:
        motscfa['FREQ']=(CALC_FREQ['FREQ'][i],CALC_FREQ['FREQ'][i+1])
     else:
        motscfa['FREQ']=(lborne[i],lborne[i+1])
     motscit['CALC_FREQ']=_F(OPTION          ='BANDE',
                             SEUIL_FREQ      =CALC_FREQ['SEUIL_FREQ'],
                             NPREC_SOLVEUR   =CALC_FREQ['NPREC_SOLVEUR'],
                             NMAX_ITER_SHIFT =CALC_FREQ['NMAX_ITER_SHIFT'],
                             PREC_SHIFT      =CALC_FREQ['PREC_SHIFT'],
                             **motscfa)
     motscit['VERI_MODE']=_F(STOP_ERREUR=VERI_MODE['STOP_ERREUR'],
                             SEUIL      =VERI_MODE['SEUIL'],
                             STURM      =VERI_MODE['STURM'],
                             PREC_SHIFT =VERI_MODE['PREC_SHIFT'])
     motscit['STOP_FREQ_VIDE']=CALC_FREQ['STOP_FREQ_VIDE']

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

     __nomre0=MODE_ITER_SIMULT(MATR_A  =MATR_A,
                                  MATR_B  =MATR_B,
                                  INFO    =INFO,
                                  METHODE =METHODE,
                                  OPTION  =OPTION,
                                  **motscit)

     __nomre0=NORM_MODE(reuse     =__nomre0,
                        MASS_INER =normode['MASS_INER'],
                        MODE      =__nomre0,
                        NORME     =normode['NORME'],
                        INFO      =normode['INFO'],)

     if IMPRESSION['TOUT_PARA']=='OUI':
        IMPR_RESU(RESU=_F(RESULTAT=__nomre0,
                          TOUT_ORDRE='OUI',
                          TOUT_CHAM ='NON',
                          TOUT_PARA ='OUI',) )

     if FILTRE_MODE :
        motscles['FILTRE_MODE'].append(_F(MODE      =__nomre0,
                                          CRIT_EXTR =FILTRE_MODE['CRIT_EXTR'],
                                          SEUIL     =FILTRE_MODE['SEUIL'], ))
     else:
        motscles['FILTRE_MODE'].append(_F(MODE      =__nomre0,
                                          TOUT_ORDRE='OUI',) )

  motscles['IMPRESSION']=_F(CUMUL    =IMPRESSION['CUMUL'],
                            CRIT_EXTR=IMPRESSION['CRIT_EXTR'],)
  self.DeclareOut('nomres',self.sd)
  nomres=EXTR_MODE(**motscles)
  return ier
