#@ MODIF macr_cabri_calc_ops Macro  DATE 08/11/2007   AUTEUR SALMONA L.SALMONA 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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




def macr_cabri_calc_ops(self,MAILLAGE,MODELE_MECA,MODELE_THER,CHAR_THER,
    CHAR_MECA,RESU_THER,RESO_INTE,
    AFFE_MATERIAU,DEFI_CHAR_THER,DEFI_CHAR_MECA,RELATION,SOLVEUR,CONVERGENCE,NEWTON, 
    INCREMENT,CHAM_MATER,**args):
   """
     Ecriture de la macro MACR_CABRI_CALC
   """
   
   #################################################################
   ########## PREPARATION MACRO
   #################################################################

   from Accas import _F

   ier =0
   
   # On met certains mots-clefs dans des variables locales pour les proteger
   affemateriau = AFFE_MATERIAU
   mail         = MAILLAGE  
   resointe     = RESO_INTE

   # On importe les definitions des commandes a utiliser dans la macro
   # Le nom de la variable doit etre obligatoirement le nom de la commande
   DEFI_GROUP       = self.get_cmd('DEFI_GROUP')
   AFFE_MATERIAU    = self.get_cmd('AFFE_MATERIAU')
   AFFE_MODELE      = self.get_cmd('AFFE_MODELE') 
   MODI_MAILLAGE    = self.get_cmd('MODI_MAILLAGE')   
   AFFE_CHAR_THER_F = self.get_cmd('AFFE_CHAR_THER_F')
   AFFE_CHAR_THER   = self.get_cmd('AFFE_CHAR_THER')  
   AFFE_CHAR_MECA_F = self.get_cmd('AFFE_CHAR_MECA_F')
   AFFE_CHAR_MECA   = self.get_cmd('AFFE_CHAR_MECA')  
   DEFI_FONCTION    = self.get_cmd('DEFI_FONCTION')
   DEFI_LIST_REEL   = self.get_cmd('DEFI_LIST_REEL')
   THER_LINEAIRE    = self.get_cmd('THER_LINEAIRE')
   STAT_NON_LINE    = self.get_cmd('STAT_NON_LINE')

   # La macro compte pour 1 dans la numerotation des commandes
   self.set_icmd(1)

   # Le concept sortant (de type evol_noli) est nomme 'resumeca' dans 
   # le contexte de la macro
   self.DeclareOut('resumeca',self.sd)
   self.DeclareOut('mail',MAILLAGE)
           
   #################################################################
   ########## PREPARATION DES MODELES
   #################################################################

   # Definition des groupes
   mail=DEFI_GROUP(reuse =mail,MAILLAGE=mail,
                        CREA_GROUP_NO=(
                                  _F(GROUP_MA='M_GOU',NOM='N_M_GOU',),
                                  _F(GROUP_MA='M_JOI',NOM='N_M_JOI',),
                                  _F(GROUP_MA='SCBJ',NOM='N_SCBJ',),
                                  _F(GROUP_MA='SCJB',NOM='N_SCJB',),
                                  _F(GROUP_MA='M_L_AA',NOM='N_M_L_AA',),
                                  _F(GROUP_MA='SCEG',NOM='N_SCEG',),
                                  _F(GROUP_MA='SCGE',NOM='N_SCGE',),),)

   # Creation du modele thermique
   if MODELE_THER != None:
      self.DeclareOut('modther',MODELE_THER)
         
   modther=AFFE_MODELE(MAILLAGE=mail,
                          AFFE=_F(GROUP_MA=('VTOT','M_GOU','M_TUB','M_JOI','SCBJ','SCJB',
                          'M_L_AA','M_INT','M_L_SA','M_EXT','SCEG','SCGE',),
                                  PHENOMENE='THERMIQUE',
                                  MODELISATION='3D_DIAG',),
                         );

   # Creation du modele mecanique
   if MODELE_MECA != None:
      self.DeclareOut('modmeca',MODELE_MECA)
         
   modmeca = AFFE_MODELE(MAILLAGE=mail,
                          AFFE=_F(GROUP_MA=('VTOT','M_GOU','M_TUB','M_JOI','SCBJ','SCJB',
                          'M_L_AA','M_L_SA','SCEG','SCGE','M_INT','M_EXT',),
                                  PHENOMENE='MECANIQUE',
                                  MODELISATION='3D',),
                         );

   # Orientation des mailles
   mail=MODI_MAILLAGE(reuse =mail,
                 MAILLAGE=mail,
                 ORIE_PEAU_3D=(_F(GROUP_MA=('M_INT','M_TUB',),),
                               _F(GROUP_MA=('M_L_AA','M_JOI','M_L_SA',),),),
                              );



   # Affectation des materiaux (thermique)
   motscles={}
   motscles['AFFE']=[]
   for mat in affemateriau:
      if mat['TOUT'] == None:
         # Creation de mots-cles pour les AFFE_CHAR_MECA
         motscles['AFFE'].append(_F(GROUP_MA=mat['GROUP_MA'],
                                    MATER = mat['MATER'],
                                    TEMP_REF = mat['TEMP_REF'],) )
      else:
         # Creation de mots-cles pour les AFFE_CHAR_MECA
         motscles['AFFE'].append(_F(TOUT='OUI',
                                    MATER = mat['MATER'],
                                    TEMP_REF = mat['TEMP_REF'],) )
      
   __cham = AFFE_MATERIAU(MAILLAGE=mail,
                    MODELE=modther,
                    AFFE=motscles['AFFE'],
                   )

   #################################################################
   ########## CONDITIONS AUX LIMITES THERMIQUES
   #################################################################   
   # Recuperation des parametres thermiques

   if DEFI_CHAR_THER != None:
      temp_ini = DEFI_CHAR_THER['TEMP_INIT']
      if DEFI_CHAR_THER['COEF_H_FLUI']!=None:
         coef_int = DEFI_CHAR_THER['COEF_H_FLUI']
      else:
         coef_int = DEFI_FONCTION(NOM_PARA='INST',
                    VALE=(0.0,0.016,
                       7200.0,0.016,),
                    PROL_DROITE='CONSTANT',
                    PROL_GAUCHE='CONSTANT',);      
      if DEFI_CHAR_THER['TEMP_EXT_FLUI']!=None:
         temp_int = DEFI_CHAR_THER['TEMP_EXT_FLUI']
      else:
         temp_int = DEFI_FONCTION(NOM_PARA='INST',
                    VALE=(0.0,temp_ini,1.0,temp_ini,11.0,60.0,
                          600.0,60.0,610.0,280.0,1800.0,280.0,7200.0,280.0,),
                    PROL_DROITE='CONSTANT',
                    PROL_GAUCHE='CONSTANT',);    
      if DEFI_CHAR_THER['COEF_H_AIR']!=None:
         coef_ext = DEFI_CHAR_THER['COEF_H_AIR']
      else:
         coef_ext = DEFI_FONCTION(NOM_PARA='INST',
                    VALE=(0.0,1e-05,7200.0,1e-05,),
                    PROL_DROITE='CONSTANT',
                    PROL_GAUCHE='CONSTANT',);
     
      if DEFI_CHAR_THER['TEMP_EXT_AIR']!=None:
         temp_ext = DEFI_CHAR_THER['TEMP_EXT_AIR']
      else:
         temp_ext = DEFI_FONCTION(NOM_PARA='INST',
                    VALE=(0.0,20.0,7200.0,20.0,),
                    PROL_DROITE='CONSTANT',
                    PROL_GAUCHE='CONSTANT',);    
      if DEFI_CHAR_THER['LIST_INST']!=None:
         transi1  = DEFI_CHAR_THER['LIST_INST']
      else:
         transi1  = DEFI_LIST_REEL(DEBUT=0.0,
                     INTERVALLE=(_F(JUSQU_A=1.0,
                                    NOMBRE=1,),
                                 _F(JUSQU_A=11.0,
                                    NOMBRE=10,),
                                 _F(JUSQU_A=600.0,
                                    NOMBRE=10,),
                                 _F(JUSQU_A=610.0,
                                    NOMBRE=30,),
                                 _F(JUSQU_A=1800.0,
                                    NOMBRE=30,),
                                 _F(JUSQU_A=7200.0,
                                    NOMBRE=10,),),);                                                        
   else:
      temp_ini = DEFI_CHAR_THER['TEMP_INIT']
      coef_int = DEFI_FONCTION(NOM_PARA='INST',
                    VALE=(0.0,0.016,
                       7200.0,0.016,),
                    PROL_DROITE='CONSTANT',
                    PROL_GAUCHE='CONSTANT',);      
      temp_int = DEFI_FONCTION(NOM_PARA='INST',
                    VALE=(0.0,temp_ini,1.0,temp_ini,11.0,60.0,
                          600.0,60.0,610.0,280.0,1800.0,280.0,7200.0,280.0,),
                    PROL_DROITE='CONSTANT',
                    PROL_GAUCHE='CONSTANT',);    
      coef_ext = DEFI_FONCTION(NOM_PARA='INST',
                    VALE=(0.0,1e-05,7200.0,1e-05,),
                    PROL_DROITE='CONSTANT',
                    PROL_GAUCHE='CONSTANT',);    
      temp_ext = DEFI_FONCTION(NOM_PARA='INST',
                    VALE=(0.0,20.0,7200.0,20.0,),
                    PROL_DROITE='CONSTANT',
                    PROL_GAUCHE='CONSTANT',);                                       
      transi1  = DEFI_LIST_REEL(DEBUT=0.0,
                     INTERVALLE=(_F(JUSQU_A=1.0,
                                    NOMBRE=1,),
                                 _F(JUSQU_A=11.0,
                                    NOMBRE=10,),
                                 _F(JUSQU_A=600.0,
                                    NOMBRE=10,),
                                 _F(JUSQU_A=610.0,
                                    NOMBRE=30,),
                                 _F(JUSQU_A=1800.0,
                                    NOMBRE=30,),
                                 _F(JUSQU_A=7200.0,
                                    NOMBRE=10,),),);     
   # Que sauver ?
   if CHAR_THER != None:
      for m in CHAR_THER:
         if m['TYPE']=="BRIDE_FLUIDE":
            self.DeclareOut('cl_th1',m['CHARGE'])
         if m['TYPE']=="BRIDE_AIR":
            self.DeclareOut('cl_th2',m['CHARGE'])
         if m['TYPE']=="ECROU_GOUJON":
            self.DeclareOut('cl_th3',m['CHARGE'])
         if m['TYPE']=="BRIDE_JOINT":
            self.DeclareOut('cl_th4',m['CHARGE'])

   # Echanges thermiques internes entre le fluide et la bride
   cl_th1=AFFE_CHAR_THER_F(MODELE=modther,
                           ECHANGE=_F(GROUP_MA = 'M_INT',
                                      COEF_H   = coef_int,
                                      TEMP_EXT = temp_int,),);

   # Echanges thermiques externes entre bride et air ambiant
   cl_th2=AFFE_CHAR_THER_F(MODELE=modther,
                           ECHANGE=_F(GROUP_MA='M_EXT',
                                   COEF_H=coef_ext,
                                   TEMP_EXT=temp_ext,),);

   # Echanges thermiques entre ecrou et goujon
   cl_th3=AFFE_CHAR_THER(MODELE=modther,
                         LIAISON_GROUP=_F(GROUP_NO_1='N_SCEG',
                                        GROUP_NO_2='N_SCGE',
                                        DDL_1='TEMP',
                                        COEF_MULT_1=1.0,
                                        DDL_2='TEMP',
                                        COEF_MULT_2=-1.0,
                                        COEF_IMPO=0.0,),);

   # Echanges thermiques entre bride et joint
   cl_th4=AFFE_CHAR_THER(MODELE=modther,
                         LIAISON_GROUP=_F(GROUP_NO_1='N_SCBJ',
                                        GROUP_NO_2='N_SCJB',
                                        DDL_1='TEMP',
                                        COEF_MULT_1=1.0,
                                        DDL_2='TEMP',
                                        COEF_MULT_2=-1.0,
                                        COEF_IMPO=0.0,),);



   #################################################################
   ########## CALCUL THERMIQUE
   #################################################################   
   if RESU_THER != None:
      self.DeclareOut('resuther',RESU_THER)   

   resuther=THER_LINEAIRE(MODELE=modther,
                  CHAM_MATER=__cham,
                  EXCIT=(_F(CHARGE=cl_th1,),
                         _F(CHARGE=cl_th2,),
                         _F(CHARGE=cl_th3,),
                         _F(CHARGE=cl_th4,),),
                  INCREMENT=_F(LIST_INST=transi1,),
                  ETAT_INIT=_F(VALE=temp_ini,),
                  TITRE='CABRI THERMIQUE &DATE &HEURE',);
 
      # Affectation des materiaux (mécanique)
   if CHAM_MATER != None:
      self.DeclareOut('_chamt',CHAM_MATER)
   motscles={}
   motscles['AFFE']=[]
   motscles['AFFE_VARC']=[]
   for mat in affemateriau:
      if mat['TOUT'] == None:
         # Creation de mots-cles pour les AFFE_CHAR_MECA
         motscles['AFFE'].append(_F(GROUP_MA=mat['GROUP_MA'],
                                    MATER = mat['MATER'],) )
         motscles['AFFE_VARC'].append(_F(NOM_VARC='TEMP',GROUP_MA=mat['GROUP_MA'],
                                         EVOL=resuther,NOM_CHAM='TEMP',
                                         VALE_REF = mat['TEMP_REF'],))
      else:
         # Creation de mots-cles pour les AFFE_CHAR_MECA
         motscles['AFFE'].append(_F(TOUT='OUI',
                                    MATER = mat['MATER'],
                                    TEMP_REF = mat['TEMP_REF'],) )
         motscles['AFFE_VARC'].append(_F(NOM_VARC='TEMP',TOUT='OUI',
                                         EVOL=resuther,NOM_CHAM='TEMP',
                                         VALE_REF = mat['TEMP_REF'],))
      
   _chamt = AFFE_MATERIAU(MAILLAGE=mail,
                    MODELE=modther,
                    AFFE=motscles['AFFE'],
                    AFFE_VARC=motscles['AFFE_VARC'],
                   )

   #################################################################
   ########## CONDITIONS AUX LIMITES MECANIQUES
   #################################################################   
   # Recuperation des parametres mecaniques
   if DEFI_CHAR_MECA != None:
     if DEFI_CHAR_MECA['PRETENS']!=None:
         f_pret = DEFI_CHAR_MECA['PRETENS']
     else:
         f_pret=DEFI_FONCTION(NOM_PARA='INST',
                     VALE=(0.0,0.0,1.0,-0.02,),
                     PROL_DROITE='CONSTANT',
                     PROL_GAUCHE='CONSTANT',);                                 
     if DEFI_CHAR_MECA['PRES_REP']!=None:
         pre_int = DEFI_CHAR_MECA['PRES_REP']
     else:
         pre_int = DEFI_FONCTION(NOM_PARA='INST',
                      VALE=(0.0,0.0,1.0,0.0,11.0,16.0,),
                      PROL_DROITE='CONSTANT',
                      PROL_GAUCHE='CONSTANT',);  
     if DEFI_CHAR_MECA['EFFE_FOND']!=None:
         eff_fond = DEFI_CHAR_MECA['EFFE_FOND']
     else:
         eff_fond=DEFI_FONCTION(NOM_PARA='INST',
                       VALE=(0.0,-0.0,1.0,-0.0,11.0,-20.607059,),
                       PROL_DROITE='CONSTANT',
                       PROL_GAUCHE='CONSTANT',);
   else:
      f_pret=DEFI_FONCTION(NOM_PARA='INST',
                     VALE=(0.0,0.0,1.0,-0.02,),
                     PROL_DROITE='CONSTANT',
                     PROL_GAUCHE='CONSTANT',);                                 

      pre_int = DEFI_FONCTION(NOM_PARA='INST',
                      VALE=(0.0,0.0,1.0,0.0,11.0,16.0,),
                      PROL_DROITE='CONSTANT',
                      PROL_GAUCHE='CONSTANT',);  

      eff_fond=DEFI_FONCTION(NOM_PARA='INST',
                       VALE=(0.0,-0.0,1.0,-0.0,11.0,-20.607059,),
                       PROL_DROITE='CONSTANT',
                       PROL_GAUCHE='CONSTANT',);     
   # Que sauver ?
   if CHAR_MECA != None:
      for m in CHAR_MECA:
         if m['TYPE']=="BLOC_BAS_GOUJ":
            self.DeclareOut('cl_me1',m['CHARGE'])
         if m['TYPE']=="BLOC_BAS_JOINT":
            self.DeclareOut('cl_me2',m['CHARGE'])
         if m['TYPE']=="BLOC_LAT_ALES":
            self.DeclareOut('cl_me3',m['CHARGE'])
         if m['TYPE']=="BLOC_LAT_NALES":
            self.DeclareOut('cl_me4',m['CHARGE'])
         if m['TYPE']=="PLAN_TUBE":
            self.DeclareOut('cl_me5',m['CHARGE'])
         if m['TYPE']=="PRES_FLU":
            self.DeclareOut('cl_me6',m['CHARGE'])
         if m['TYPE']=="EFFET_FOND":
            self.DeclareOut('cl_me7',m['CHARGE'])
         if m['TYPE']=="CONT_JOINT":
            self.DeclareOut('cl_me8',m['CHARGE'])
         if m['TYPE']=="SERR_ECROU_1":
            self.DeclareOut('cl_me10',m['CHARGE'])
         if m['TYPE']=="SERR_ECROU_2":
            self.DeclareOut('cl_me11',m['CHARGE'])            
                            

   # Blocage bas du goujon
   cl_me1=AFFE_CHAR_MECA(MODELE=modmeca,
                      DDL_IMPO=_F(GROUP_NO='N_M_GOU',
                                  DZ=0.0,),
                      INFO=2,);
   # Blocage bas du joint
   cl_me2=AFFE_CHAR_MECA(MODELE=modmeca,
                      DDL_IMPO=_F(GROUP_NO='N_M_JOI',
                                  DZ=0.0,),
                      INFO=2,);

   # Blocage lateral, face laterale avec alesage
   cl_me3=AFFE_CHAR_MECA(MODELE=modmeca,
                      DDL_IMPO=_F(GROUP_NO='N_M_L_AA',
                                  DY=0.0,),
                      INFO=2,);

   # Face laterale sans alesage
   cl_me4=AFFE_CHAR_MECA(MODELE=modmeca,
                      FACE_IMPO=_F(GROUP_MA='M_L_SA',
                                   DNOR=0.0,),
                      INFO=2,);

   # Condition de planeite de la face de coupe du tube
   cl_me5=AFFE_CHAR_MECA(MODELE=modmeca,
                      LIAISON_UNIF=_F(GROUP_MA='M_TUB',
                                      DDL='DZ',),
                      INFO=2,);
   # Pression due au fluide
   cl_me6=AFFE_CHAR_MECA_F(MODELE=modmeca,
                           PRES_REP=_F(GROUP_MA='M_INT',
                                     PRES=pre_int,),
                           INFO=2,);

   # Effet de fond
   cl_me7=AFFE_CHAR_MECA_F(MODELE=modmeca,
                           PRES_REP=_F(GROUP_MA='M_TUB',
                                     PRES=eff_fond,),
                           INFO=2,);

   # Contact zone de joint
   cl_me8=AFFE_CHAR_MECA(MODELE=modmeca,
                        CONTACT=_F(GROUP_MA_MAIT='SCBJ',
                                   GROUP_MA_ESCL='SCJB',),
                        INFO=2,);

   # Serrage ecrou/goujon (pre-tensionnement)
   cl_me10=AFFE_CHAR_MECA_F(MODELE=modmeca,
                         LIAISON_GROUP=_F(GROUP_NO_1='N_SCEG',
                                          GROUP_NO_2='N_SCGE',
                                          DDL_1='DZ',
                                          COEF_MULT_1=1.0,
                                          DDL_2='DZ',
                                          COEF_MULT_2=-1.0,
                                          COEF_IMPO=f_pret,),
                      INFO=2,);

   cl_me11=AFFE_CHAR_MECA(MODELE=modmeca,
                       LIAISON_GROUP=_F(GROUP_NO_1='N_SCEG',
                                        GROUP_NO_2='N_SCGE',
                                        DDL_1='DX',
                                        COEF_MULT_1=1.0,
                                        DDL_2='DX',
                                        COEF_MULT_2=-1.0,
                                        COEF_IMPO=0.0,),
                      INFO=2,);


   #################################################################
   ########## CALCUL MECANIQUE
   #################################################################  
   # Options de convergence        
   solveur=SOLVEUR[0].cree_dict_valeurs(SOLVEUR[0].mc_liste)
   
   # Elimination des valeurs "None"
   for i in solveur.keys():
      if solveur[i]==None : del solveur[i]


   transi2 = DEFI_LIST_REEL(DEBUT=0.0,
                     INTERVALLE=(_F(JUSQU_A=1.0,
                                    NOMBRE=2,),
                                 _F(JUSQU_A=11.0,
                                    NOMBRE=20,),
                                 _F(JUSQU_A=600.0,
                                    NOMBRE=20,),
                                 _F(JUSQU_A=610.0,
                                    NOMBRE=20,),
                                 _F(JUSQU_A=1800.0,
                                    NOMBRE=20,),
                                 _F(JUSQU_A=7200.0,
                                    NOMBRE=20,),),);   

   # Options d'incrementation  
   if INCREMENT != None:
      if INCREMENT['LIST_INST'] != None:
         listinst = INCREMENT['LIST_INST']
      else:
         listinst = transi2   
   
      increment=INCREMENT[0].cree_dict_valeurs(INCREMENT[0].mc_liste)
   
      # Elimination des valeurs "None"
      for i in increment.keys():
         if increment[i]==None : del increment[i]
      
      increment['LIST_INST'] = listinst
            
   else:
      listinst  = transi2
      increment =_F(
               LIST_INST       = listinst,
               ),       
                              
   # Options de Newton     
   newton=NEWTON[0].cree_dict_valeurs(NEWTON[0].mc_liste)
   # Elimination des valeurs "None"
   for i in newton.keys():
      if newton[i]==None : del newton[i]   

   # Options de convergence        
   convergence=CONVERGENCE[0].cree_dict_valeurs(CONVERGENCE[0].mc_liste) 
   # Elimination des valeurs "None"
   for i in convergence.keys():
      if convergence[i]==None : del convergence[i]

   # Options de comportement
   # Type incremental (=1) ou elastique (=0)
   comp_incr = 0
   if RELATION:
      relation=RELATION
      if relation == 'VMIS_ISOT_TRAC':
         comp_incr = 1
      else:
         comp_incr = 0
   else:
      relation = 'ELAS'
      comp_incr = 0

         
   # Parametres du calcul
   if comp_incr == 1:
      resumeca=STAT_NON_LINE(MODELE=modmeca,
                  CHAM_MATER=_chamt,
                  EXCIT=(_F(CHARGE=cl_me1,),
                         _F(CHARGE=cl_me2,),
                         _F(CHARGE=cl_me3,),
                         _F(CHARGE=cl_me4,),
                         _F(CHARGE=cl_me5,),
                         _F(CHARGE=cl_me6,), 
                         _F(CHARGE=cl_me7,), 
                         _F(CHARGE=cl_me8,), 
                         _F(CHARGE=cl_me10,), 
                         _F(CHARGE=cl_me11,),                            
                  ),
                  SOLVEUR        = solveur, 
                  COMP_INCR      =_F(RELATION=relation,RESO_INTE=resointe),
                  NEWTON         = newton,
                  INCREMENT      = increment,
                  CONVERGENCE    = convergence,
                  TITRE='CABRI THERMOM\xe9CANIQUE &DATE &HEURE',);
   else:
      resumeca=STAT_NON_LINE(MODELE=modmeca,
                  CHAM_MATER=_chamt,
                  EXCIT=(_F(CHARGE=cl_me1,),
                         _F(CHARGE=cl_me2,),
                         _F(CHARGE=cl_me3,),
                         _F(CHARGE=cl_me4,),
                         _F(CHARGE=cl_me5,),
                         _F(CHARGE=cl_me6,), 
                         _F(CHARGE=cl_me7,), 
                         _F(CHARGE=cl_me8,), 
                         _F(CHARGE=cl_me10,), 
                         _F(CHARGE=cl_me11,),                            
                  ),
                  SOLVEUR        = solveur, 
                  COMP_ELAS      =_F(RELATION=relation,RESO_INTE=resointe),
                  NEWTON         = newton,
                  INCREMENT      = increment,
                  CONVERGENCE    = convergence,
                  TITRE='CABRI THERMOM\xe9CANIQUE &DATE &HEURE',);                  

   return ier

