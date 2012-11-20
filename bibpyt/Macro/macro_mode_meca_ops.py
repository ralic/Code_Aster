#@ MODIF macro_mode_meca_ops Macro  DATE 19/11/2012   AUTEUR BOITEAU O.BOITEAU 
# -*- coding: iso-8859-1 -*-
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



def macro_mode_meca_ops(self,MATR_RIGI,MATR_MASS,INFO,METHODE,OPTION,CALC_FREQ,
                      SOLVEUR,VERI_MODE,NORM_MODE,FILTRE_MODE,IMPRESSION,**args):
  """
     Ecriture de la macro MACRO_MODE_MECA
  """
  from Accas import _F
  from Utilitai.Utmess import UTMESS

  ier=0
    
  #  on protege le contenu du mot cle NORM_MODE pour eviter les confusions
  #  avec la commande du meme nom
  
  normode=NORM_MODE
  
  # On importe les definitions des commandes a utiliser dans la macro
  MODE_ITER_SIMULT  =self.get_cmd('MODE_ITER_SIMULT')
  NORM_MODE         =self.get_cmd('NORM_MODE')
  IMPR_RESU         =self.get_cmd('IMPR_RESU')
  EXTR_MODE         =self.get_cmd('EXTR_MODE')
  INFO_MODE         =self.get_cmd('INFO_MODE')
  # La macro compte pour 1 dans la numerotation des commandes
  self.set_icmd(1)

  nompro=None
  iocc=0
  
  # Construction de la liste de frequences
  if CALC_FREQ['FREQ']:
     lborne=[]
     nnfreq=len(CALC_FREQ['FREQ'])
     for i in range(0,nnfreq):
         lborne.append(CALC_FREQ['FREQ'][i])
  else:
     assert(False)

  # Recuperation parametres solveur lineaire
  dSolveur=SOLVEUR[0].cree_dict_valeurs(SOLVEUR[0].mc_liste)
  for i in dSolveur.keys():
      if dSolveur[i]==None : del dSolveur[i]

  # INFO_MODE global sur la liste de frequences 
  motfaci={}
  motfaci['COMPTAGE']=_F(METHODE         ='AUTO',
                         SEUIL_FREQ      =CALC_FREQ['SEUIL_FREQ'],
                         NMAX_ITER_SHIFT =CALC_FREQ['NMAX_ITER_SHIFT'],
                         PREC_SHIFT      =CALC_FREQ['PREC_SHIFT'],
                           )
  __nbmodi=INFO_MODE(MATR_RIGI  =MATR_RIGI,
                     MATR_MASS  =MATR_MASS,
                     INFO       =INFO,
                     FREQ       =lborne,
                     SOLVEUR    =dSolveur,
                     **motfaci)      

  # Recuperation du nbre de modes total theorique
  freq_ini=1.E+99
  freq_fin=-1.E+99
  nbmodeth=0
  nbmodeef=None
  for i in range(0,nnfreq-1):
    nbmodeth = nbmodeth + __nbmodi['NB_MODE',i+1]            

  # Boucle de MODE_ITER_SIMULT(OPTION=BANDE) + NORM_MODE
  # On ne traite pas les sous-bandes vides (gain de temps et d'affichage!)   
  motscles={}
  motscles['FILTRE_MODE']=[]
  for i in range(0,nnfreq-1):
     if (__nbmodi['NB_MODE',i+1] != 0):
        motscit={}
        motscfa={}
        if CALC_FREQ['DIM_SOUS_ESPACE']:
           motscfa['DIM_SOUS_ESPACE']=CALC_FREQ['DIM_SOUS_ESPACE']
        if CALC_FREQ['COEF_DIM_ESPACE']:
           motscfa['COEF_DIM_ESPACE']=CALC_FREQ['COEF_DIM_ESPACE']
        motscfa['FREQ']=(lborne[i],lborne[i+1])        
        motscfa['TABLE_FREQ']=__nbmodi
        motscit['CALC_FREQ']=_F(OPTION          ='BANDE',
                                SEUIL_FREQ      =CALC_FREQ['SEUIL_FREQ'],
                                NMAX_ITER_SHIFT =CALC_FREQ['NMAX_ITER_SHIFT'],
                                PREC_SHIFT      =CALC_FREQ['PREC_SHIFT'],
                                **motscfa)

        if VERI_MODE['STURM'] == 'LOCAL':
           motveri='OUI'
        else:
           motveri='NON'
        
        motscit['VERI_MODE']=_F(STOP_ERREUR=VERI_MODE['STOP_ERREUR'],
                                SEUIL      =VERI_MODE['SEUIL'],
                                STURM      =motveri,
                                PREC_SHIFT =VERI_MODE['PREC_SHIFT'])

        motscit['STOP_BANDE_VIDE']=CALC_FREQ['STOP_BANDE_VIDE']

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
        else:
           assert(False)

        __nomre0=MODE_ITER_SIMULT(   MATR_RIGI  =MATR_RIGI,
                                     MATR_MASS  =MATR_MASS,
                                     INFO       =INFO,
                                     METHODE    =METHODE,
                                     OPTION     =OPTION,
                                     SOLVEUR    =dSolveur,
                                     **motscit)

     
        # Si VERI_MODE/STURM=GLOBAL on recupere les frequences extremes trouvees
        # au cours des nnfreq-1 calculs MODE_ITER_SIMULT
        # On le fait pour chaque bande au cas ou certaines serait vide
        if VERI_MODE['STURM'] == 'GLOBAL':
           dicomode={}
           dicomode=__nomre0.LIST_VARI_ACCES()
           if (len(dicomode['FREQ']) != 0):
              raux_ini=dicomode['FREQ'][0]
              raux_fin=dicomode['FREQ'][-1]
              if (raux_ini < freq_ini):
                freq_ini=raux_ini
              if (raux_fin > freq_fin):
                freq_fin=raux_fin            
           else:
              assert(False)
            
        __nomre0=NORM_MODE(reuse     =__nomre0,
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
     # Sous-bande vide
     else:
        if (CALC_FREQ['STOP_BANDE_VIDE'] == 'OUI'):
           UTMESS('F', 'MODAL_6',vali=(i+1,))
        elif (CALC_FREQ['STOP_BANDE_VIDE'] == 'NON'):
           print 72*"-"
           UTMESS('I', 'MODAL_6',vali=(i+1,))
           print 72*"-"
        else:
           assert(False)

  # Si VERI_MODE/STURM=GLOBAL on fait un test de Sturm global via INFO_MODE     
  # On decale les bornes de test comme en FORTRAN (routine vpfopr.f) pour limiter
  # les decalages de shift internes a INFO_MODE
  if VERI_MODE['STURM'] == 'NON':
     print 72*"-"
     UTMESS('I', 'MODAL_2')
     print 72*"-"
  elif VERI_MODE['STURM'] == 'LOCAL':
     print 72*"-"
     UTMESS('I', 'MODAL_3')
     print 72*"-"    
  elif VERI_MODE['STURM'] == 'GLOBAL':
     # Construction des 2 bornes de la bande a tester
     if (nbmodeth != 0) :
        omecor=CALC_FREQ['SEUIL_FREQ']
        precshift=VERI_MODE['PREC_SHIFT']
        freq_ini=(1.0-precshift)*freq_ini
        freq_fin=(1.0+precshift)*freq_fin
        if (abs(freq_ini)<omecor):
           freq_ini=-omecor
        if (abs(freq_fin)<omecor):
           freq_fin=omecor
        __nbmodf=INFO_MODE(MATR_RIGI  =MATR_RIGI,
                           MATR_MASS  =MATR_MASS,
                           INFO       =INFO,
                           FREQ       = (freq_ini,freq_fin),
                           SOLVEUR    =dSolveur,
                           **motfaci)
        # Recuperation du nbre de modes effectivement calcules
        nbmodeef=__nbmodf['NB_MODE',1]
        if (nbmodeth==nbmodeef) :
           print 72*"-"
           UTMESS('I','MODAL_4',valr=(freq_ini,freq_fin),vali=(nbmodeef,nbmodeth))
           print 72*"-"
        else:
           # Message similaire a ALGELINE5_24 pour le FORTRAN
           UTMESS('F','MODAL_5',valr=(freq_ini,freq_fin),vali=(nbmodeef,nbmodeth))

     # La bande globale est vide
     else:
        print 72*"-"
        UTMESS('I', 'MODAL_7',valr=(lborne[0],lborne[nnfreq-1]))
        print 72*"-"
  else:
     assert(False)
 
 
  # EXTR_MODE final sur toutes les frequences
  if (nbmodeth != 0) :  
     motscles['IMPRESSION']=_F(CUMUL    =IMPRESSION['CUMUL'],
                            CRIT_EXTR=IMPRESSION['CRIT_EXTR'],)
     self.DeclareOut('nomres',self.sd)
     nomres=EXTR_MODE(**motscles)
  else:
     print 72*"-"
     if (CALC_FREQ['STOP_BANDE_VIDE'] == 'OUI'):
        UTMESS('F', 'MODAL_8',valr=(lborne[0],lborne[nnfreq-1]))
     elif (CALC_FREQ['STOP_BANDE_VIDE'] == 'NON'):
        UTMESS('A', 'MODAL_8',valr=(lborne[0],lborne[nnfreq-1]))
     else:
        assert(False)
     print 72*"-"
  
  return ier
