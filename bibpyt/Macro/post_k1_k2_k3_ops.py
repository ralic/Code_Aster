#@ MODIF post_k1_k2_k3_ops Macro  DATE 22/05/2006   AUTEUR REZETTE C.REZETTE 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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

def veri_tab(tab,nom,ndim) :
   from Utilitai.Utmess     import UTMESS
   macro = 'POST_K1_K2_K3'
   for label in ('DX','DY','COOR_X','COOR_Y','ABSC_CURV') :
       if label not in tab.para :
          message='le label '+label+' doit etre present dans la table : '+nom
          UTMESS('F', macro, message)
   if ndim==3 :
      if 'DZ'     not in tab.para :
          message='le label DZ doit etre present dans la table : '+nom
          UTMESS('F', macro, message)
      if 'COOR_Z' not in tab.para :
          message='le label COOR_Z doit etre present dans la table : '+nom
          UTMESS('F', macro, message)

def cross_product(a,b):
    cross = [0]*3
    cross[0] = a[1]*b[2]-a[2]*b[1]
    cross[1] = a[2]*b[0]-a[0]*b[2]
    cross[2] = a[0]*b[1]-a[1]*b[0]
    return cross
    
def post_k1_k2_k3_ops(self,MODELISATION,FOND_FISS,MATER,RESULTAT,
                   TABL_DEPL_SUP,TABL_DEPL_INF,ABSC_CURV_MAXI,PREC_VIS_A_VIS,
                   TOUT_ORDRE,NUME_ORDRE,LIST_ORDRE,INST,LIST_INST,SYME_CHAR,
                   INFO,VECT_K1,TITRE,**args):
   """
   Macro POST_K1_K2_K3
   Calcul des facteurs d'intensité de contraintes en 2D et en 3D
   par extrapolation des sauts de déplacements sur les lèvres de
   la fissure. Produit une table.
   """
   import aster
   import string
   import copy
   import math
   import Numeric
   from Numeric import array,asarray,Float,sqrt,matrixmultiply,transpose,sign,resize,dot,multiply
   from math import pi
   from types import ListType, TupleType
   from Accas import _F
   from Utilitai.Table      import Table, merge
   EnumTypes = (ListType, TupleType)

   macro = 'POST_K1_K2_K3'
   from Accas               import _F
   from Utilitai.Utmess     import UTMESS

   ier = 0
   # La macro compte pour 1 dans la numerotation des commandes
   self.set_icmd(1)

   # Le concept sortant (de type table_sdaster ou dérivé) est tab
   self.DeclareOut('tabout', self.sd)
   
   # On importe les definitions des commandes a utiliser dans la macro
   # Le nom de la variable doit etre obligatoirement le nom de la commande
   CREA_TABLE    = self.get_cmd('CREA_TABLE')
   CALC_TABLE    = self.get_cmd('CALC_TABLE')
   IMPR_TABLE    = self.get_cmd('IMPR_TABLE')
   POST_RELEVE_T    = self.get_cmd('POST_RELEVE_T')
   DETRUIRE      = self.get_cmd('DETRUIRE')
   MACR_LIGN_COUPE      = self.get_cmd('MACR_LIGN_COUPE')

#   ------------------------------------------------------------------
#                         CARACTERISTIQUES MATERIAUX
#   ------------------------------------------------------------------
   matph = aster.getvectjev( string.ljust(MATER.nom,8)+'.MATERIAU.NOMRC         ')
   phenom=None
   for cmpt in matph :
       if cmpt[:4]=='ELAS' :
          phenom=cmpt
          break
   if phenom==None : UTMESS('F', macro, 'IL FAUT DEFINIR ELAS DANS DEFI_MATERIAU')
#   --- RECHERCHE SI LE MATERIAU DEPEND DE LA TEMPERATURE:
   valk = aster.getvectjev( string.ljust(MATER.nom,8)+'.'+phenom[:10]+'.VALK')
   valk = [x.strip() for x in valk]
   valr = aster.getvectjev( string.ljust(MATER.nom,8)+'.'+phenom[:10]+'.VALR')
   dicmat=dict(zip(valk,valr))
#   --- PROPRIETES MATERIAUX DEPENDANTES DE LA TEMPERATURE
   Tempe3D = False
   if dicmat.has_key('TEMP_DEF') and FOND_FISS and RESULTAT : 
# on recupere juste le nom du resultat thermique
      ndim   = 3
      Lchar = aster.getvectjev(string.ljust(RESULTAT.nom,8)+'.0000.EXCIT.LCHA        ')
      for i in range(len(Lchar)):
         resuth = aster.getvectjev(Lchar[i][0:8]+'.CHME.TEMPE.TEMP        ')
         if resuth !=None :
            Tempe3D = True
            break
   elif dicmat.has_key('TEMP_DEF') and not Tempe3D :
      message = 'LES PROPRIETES MATERIAUX, NECESSAIRES AUX CALCULS \n'
      message = message +'DES COEFFICIENTS D INTENSITE DES CONTRAINTES, ONT ETE OBTENUES A LA\n'
      message = message +'TEMPERATURE DE REFERENCE DU MATERIAU ET NON A LA TEMPERATURE CALCULEE.'
      UTMESS('A', macro, message)
      nompar = ('TEMP',)
      valpar = (dicmat['TEMP_DEF'],)
      nomres=['E','NU']
      valres,codret = MATER.RCVALE('ELAS',nompar,valpar,nomres,'F')
      e = valres[0]
      nu = valres[1]
      

#   --- PROPRIETES MATERIAUX INDEPENDANTES DE LA TEMPERATURE
   else :
      e  = dicmat['E']
      nu = dicmat['NU']  
   
   if not Tempe3D :
      coefd3 = 0.
      coefd  = e * sqrt(2.*pi)
      unmnu2 = 1. - nu**2
      unpnu  = 1. + nu
      if MODELISATION=='3D' :
         UTMESS('I', macro, 'l operateur CALC_G -option CALC_K_G- calcule plus precisement les K1 K2 K3')
         ndim   = 3
         coefd  = coefd      / ( 8.0 * unmnu2 )
         coefd3 = e*sqrt(2*pi) / ( 8.0 * unpnu )
         coefg  = unmnu2 / e
         coefg3 = unpnu  / e
      elif MODELISATION=='AXIS' :
         ndim   = 2
         coefd  = coefd  / ( 8. * unmnu2 )
         coefg  = unmnu2 / e
         coefg3 = unpnu  / e
      elif MODELISATION=='D_PLAN' :
         UTMESS('I', macro, 'l operateur CALC_G -option CALC_K_G- calcule plus precisement les K1 K2')
         ndim   = 2
         coefd  = coefd / ( 8. * unmnu2 )
         coefg  = unmnu2 / e
         coefg3 = unpnu  / e
      elif MODELISATION=='C_PLAN' :
         UTMESS('I', macro, 'l operateur CALC_G -option CALC_K_G- calcule plus precisement les K1 K2')
         ndim   = 2
         coefd  = coefd / 8.
         coefg  = 1. / e
         coefg3 = unpnu / e
      else :
         UTMESS('F', macro, 'modélisation non implantée')


#   ------------------------------------------------------------------
#                        CAS FOND_FISS
#   ------------------------------------------------------------------
   if FOND_FISS : 
      MAILLAGE = args['MAILLAGE']
      NOEUD = args['NOEUD']
      SANS_NOEUD = args['SANS_NOEUD']
      GROUP_NO = args['GROUP_NO']
      SANS_GROUP_NO = args['SANS_GROUP_NO']
      TOUT = args['TOUT']
      TYPE_MAILLAGE = args['TYPE_MAILLAGE']
      NB_NOEUD_COUPE = args['NB_NOEUD_COUPE']
      LNOFO = aster.getvectjev(string.ljust(FOND_FISS.nom,8)+'.FOND      .NOEU        ')
      RECOL = False
# Cas double fond de fissure : par convention les noeuds sont ceux de fond_inf
      if LNOFO==None :
         RECOL = True
         LNOFO = aster.getvectjev(string.ljust(FOND_FISS.nom,8)+'.FOND_INF  .NOEU        ')
         if LNOFO==None : UTMESS('F', macro, 'PROBLEME A LA RECUPERATION DES NOEUDS DU FOND DE FISSURE')
      LNOFO = map(string.rstrip,LNOFO)
      Nbfond = len(LNOFO)

#   ----------Mots cles TOUT, NOEUD, SANS_NOEUD -------------
      Typ = aster.getvectjev(string.ljust(FOND_FISS.nom,8)+'.FOND      .TYPE        ')
      if (Typ[0]=='SEG2    ') or (Typ[0]=='SEG3    ' and TOUT=='OUI') :
         pas = 1
      elif (Typ[0]=='SEG3    ') : 
         pas = 2
      else :
         UTMESS('F', macro, 'TYPE DE MAILLES DU FOND DE FISSURE NON DEFINI')
####
      NO_SANS = []
      NO_AVEC = []
      if GROUP_NO!=None :
        collgrno=aster.getcolljev(string.ljust(MAILLAGE.nom,8)+'.GROUPENO')
        cnom = aster.getvectjev(string.ljust(MAILLAGE.nom,8)+'.NOMNOE')
        if type(GROUP_NO) not in EnumTypes : GROUP_NO = (GROUP_NO,)
        for m in range(len(GROUP_NO)) :
          ngrno=GROUP_NO[m].ljust(8).upper()
          if ngrno not in collgrno.keys() :
            UTMESS('F', macro, "LE GROUP_NO "+ngrno+" N EST PAS DANS LE MAILLAGE")
          for i in range(len(collgrno[ngrno])) : NO_AVEC.append(cnom[collgrno[ngrno][i]-1])
        NO_AVEC= map(string.rstrip,NO_AVEC)
      if NOEUD!=None : 
        if type(NOEUD) not in EnumTypes : NO_AVEC = (NOEUD,)
        else : NO_AVEC = NOEUD
      if SANS_GROUP_NO!=None :
        collgrno=aster.getcolljev(string.ljust(MAILLAGE.nom,8)+'.GROUPENO')
        cnom = aster.getvectjev(string.ljust(MAILLAGE.nom,8)+'.NOMNOE')
        if type(SANS_GROUP_NO) not in EnumTypes : SANS_GROUP_NO = (SANS_GROUP_NO,)
        for m in range(len(SANS_GROUP_NO)) :
          ngrno=SANS_GROUP_NO[m].ljust(8).upper()
          if ngrno not in collgrno.keys() :
            UTMESS('F', macro, "LE GROUP_NO "+ngrno+" N EST PAS DANS LE MAILLAGE")
          for i in range(len(collgrno[ngrno])) : NO_SANS.append(cnom[collgrno[ngrno][i]-1])
        NO_SANS= map(string.rstrip,NO_SANS)
      if SANS_NOEUD!=None : 
        if type(SANS_NOEUD) not in EnumTypes : NO_SANS = (SANS_NOEUD,)
        else : NO_SANS = SANS_NOEUD
# Creation de la liste des noeuds du fond a traiter : Lnf1
      Lnf1 = []
      Nbf1 = 0
      if len(NO_AVEC)!=0 :
        for i in range(len(NO_AVEC)) :
          if NO_AVEC[i] in LNOFO : 
            Lnf1.append(NO_AVEC[i])
            Nbf1 = Nbf1 +1
          else : 
            UTMESS('F', macro, 'LE NOEUD %s N APPARTIENT PAS AU FOND DE FISSURE'%NO_AVEC[i])
      else :
         for i in range(0,Nbfond,pas) :
            if not (LNOFO[i] in NO_SANS) :
               Lnf1.append(LNOFO[i])
               Nbf1 = Nbf1 +1

##### Cas maillage libre###########
# creation des directions normales et macr_lign_coup
      if TYPE_MAILLAGE =='LIBRE':
        if not RESULTAT : UTMESS('F', macro, 'MOT CLE RESULTAT OBLIGATOIRE POUR TYPE_MAILLAGE = LIBRE')
        Lnofon = Lnf1
        Nbnofo = Nbf1
        ListmaS = aster.getvectjev(string.ljust(FOND_FISS.nom,8)+'.LEVRESUP  .MAIL        ')
        if SYME_CHAR=='SANS':
          ListmaI = aster.getvectjev(string.ljust(FOND_FISS.nom,8)+'.LEVREINF  .MAIL        ')
        __NCOFON=POST_RELEVE_T(ACTION=_F(INTITULE='Tab pour coordonnees noeuds du fond',
                                            NOEUD=LNOFO,
                                            RESULTAT=RESULTAT,
                                            NOM_CHAM='DEPL',NUME_ORDRE=1,NOM_CMP=('DX',),
                                            OPERATION='EXTRACTION',),);
        tcoorf=__NCOFON.EXTR_TABLE()
        DETRUIRE(CONCEPT=_F(NOM=__NCOFON),INFO=1) 
        nbt = len(tcoorf['NOEUD'].values()['NOEUD'])
        xs=array(tcoorf['COOR_X'].values()['COOR_X'][:nbt],Float)
        ys=array(tcoorf['COOR_Y'].values()['COOR_Y'][:nbt],Float)
        if ndim==2 : zs=Numeric.zeros(nbval,Float)
        elif ndim==3 : zs=array(tcoorf['COOR_Z'].values()['COOR_Z'][:nbt],Float)
        ns = tcoorf['NOEUD'].values()['NOEUD'][:nbt]
        ns = map(string.rstrip,ns)
        l_coorf =  [[ns[i],xs[i],ys[i],zs[i]] for i in range(0,nbt)]
        l_coorf = [(i[0],i[1:]) for i in l_coorf]
        d_coorf = dict(l_coorf) 
# Calcul des normales a chaque noeud du fond
        v1 =  array(VECT_K1)
        VN = [None]*Nbfond
        absfon = [0,]
        DTANOR = aster.getvectjev(string.ljust(FOND_FISS.nom,8)+'.DTAN_ORIGINE')
        if DTANOR != None :
          VN[0] = array(DTANOR)
        else :
          Pfon2 = array([d_coorf[LNOFO[0]][0],d_coorf[LNOFO[0]][1],d_coorf[LNOFO[0]][2]])
          Pfon3 = array([d_coorf[LNOFO[1]][0],d_coorf[LNOFO[1]][1],d_coorf[LNOFO[1]][2]])
          VT = (Pfon3 - Pfon2)/sqrt(dot(transpose(Pfon3-Pfon2),Pfon3-Pfon2))
          VN[i] = array(cross_product(VT,v1))
        for i in range(1,Nbfond-1):
          Pfon1 = array([d_coorf[LNOFO[i-1]][0],d_coorf[LNOFO[i-1]][1],d_coorf[LNOFO[i-1]][2]])
          Pfon2 = array([d_coorf[LNOFO[i]][0],d_coorf[LNOFO[i]][1],d_coorf[LNOFO[i]][2]])
          Pfon3 = array([d_coorf[LNOFO[i+1]][0],d_coorf[LNOFO[i+1]][1],d_coorf[LNOFO[i+1]][2]])
          absf = sqrt(dot(transpose(Pfon1-Pfon2),Pfon1-Pfon2)) + absfon[i-1]
          absfon.append(absf)
          VT = (Pfon3 - Pfon2)/sqrt(dot(transpose(Pfon3-Pfon2),Pfon3-Pfon2))
          VT = VT+(Pfon2 - Pfon1)/sqrt(dot(transpose(Pfon2-Pfon1),Pfon2-Pfon1))
          VN[i] = array(cross_product(VT,v1)) 
          VN[i] = VN[i]/sqrt(dot(transpose(VN[i]),VN[i]))
        i = Nbfond-1
        Pfon1 = array([d_coorf[LNOFO[i-1]][0],d_coorf[LNOFO[i-1]][1],d_coorf[LNOFO[i-1]][2]])
        Pfon2 = array([d_coorf[LNOFO[i]][0],d_coorf[LNOFO[i]][1],d_coorf[LNOFO[i]][2]])
        absf = sqrt(dot(transpose(Pfon1-Pfon2),Pfon1-Pfon2)) + absfon[i-1]
        absfon.append(absf)
        DTANEX = aster.getvectjev(string.ljust(FOND_FISS.nom,8)+'.DTAN_EXTREMITE')
        if DTANEX != None :
          VN[i] = array(DTANEX)
        else :
          VT = (Pfon2 - Pfon1)/sqrt(dot(transpose(Pfon2-Pfon1),Pfon2-Pfon1))
          VN[i] = array(cross_product(VT,v1))
        dicoF = dict([(LNOFO[i],absfon[i]) for i in range(Nbfond)])  
        dicVN = dict([(LNOFO[i],VN[i]) for i in range(Nbfond)])
#Extraction dep sup/inf sur les normales          
        TlibS = [None]*Nbf1
        TlibI = [None]*Nbf1
        if NB_NOEUD_COUPE < 3 : 
          message = 'LE NOMBRE DE NOEUDS NB_NOEUD_COUPE DOIT ETRE SUPERIEUR A 3 : ON PREND LA VALEUR PAR DEFAUT'
          UTMESS('A', macro, message)
          NB_NOEUD_COUPE = 5
        MOD = aster.getvectjev(string.ljust(RESULTAT.nom,19)+'.MODL        ')
        if MOD==None : UTMESS('F', macro, 'PROBLEME A LA RECUPERATION DU MODELE DANS LA SD RESULTAT FOURNIE')
        MOD = map(string.rstrip,MOD)
        MODEL = self.jdc.sds_dict[MOD[0]]
        for i in range(Nbf1):
          Porig = array(d_coorf[Lnf1[i]] )
          Pextr = Porig - ABSC_CURV_MAXI*dicVN[Lnf1[i]]
          TlibS[i] = MACR_LIGN_COUPE(RESULTAT=RESULTAT,
                NOM_CHAM='DEPL',MODELE=MODEL, MAILLE = ListmaS,
                LIGN_COUPE=_F(NB_POINTS=NB_NOEUD_COUPE,COOR_ORIG=(Porig[0],Porig[1],Porig[2],),
                                          COOR_EXTR=(Pextr[0],Pextr[1],Pextr[2]),),);
          if SYME_CHAR=='SANS':
            TlibI[i] = MACR_LIGN_COUPE(RESULTAT=RESULTAT,
                  NOM_CHAM='DEPL',MODELE=MODEL, MAILLE = ListmaI,
                LIGN_COUPE=_F(NB_POINTS=NB_NOEUD_COUPE,COOR_ORIG=(Porig[0],Porig[1],Porig[2],),
                                          COOR_EXTR=(Pextr[0],Pextr[1],Pextr[2]),),);


##### Cas maillage regle###########
      else:
#   ---------- Dictionnaires des levres  -------------  
        NnormS = aster.getvectjev(string.ljust(FOND_FISS.nom,8)+'.SUPNORM   .NOEU        ')
        if NnormS==None : UTMESS('F', macro, 'PROBLEME A LA RECUPERATION DES NOEUDS DES LEVRES')
        NnormS = map(string.rstrip,NnormS)
        if LNOFO[0]==LNOFO[-1] : Nbfond=Nbfond-1  # Cas fond de fissure ferme
        NnormS = [[LNOFO[i],NnormS[i*20:(i+1)*20]] for i in range(0,Nbfond)]
        NnormS = [(i[0],i[1][0:]) for i in NnormS]
        dicoS = dict(NnormS)
        if SYME_CHAR=='SANS':
           NnormI = aster.getvectjev(string.ljust(FOND_FISS.nom,8)+'.INFNORM   .NOEU        ')
           NnormI = map(string.rstrip,NnormI)
           NnormI = [[LNOFO[i],NnormI[i*20:(i+1)*20]] for i in range(0,Nbfond)]
           NnormI = [(i[0],i[1][0:]) for i in NnormI]
           dicoI = dict(NnormI)

#   ---------- Dictionnaire des coordonnees  -------------  
        if RESULTAT :
          Ltot = LNOFO
          for i in range(Nbf1) :
            for k in range(0,20) :
              if dicoS[Lnf1[i]][k] !='': Ltot.append(dicoS[Lnf1[i]][k])
          if SYME_CHAR=='SANS':
            for i in range(Nbf1) :
              for k in range(0,20) :
                if dicoI[Lnf1[i]][k] !='': Ltot.append(dicoI[Lnf1[i]][k])
          Ltot=dict([(i,0) for i in Ltot]).keys()
          __NCOOR=POST_RELEVE_T(ACTION=_F(INTITULE='Tab pour coordonnees noeuds des levres',
                                            NOEUD=Ltot,
                                            RESULTAT=RESULTAT,
                                            NOM_CHAM='DEPL',NUME_ORDRE=1,NOM_CMP=('DX',),
                                            OPERATION='EXTRACTION',),);
          tcoor=__NCOOR.EXTR_TABLE()
          DETRUIRE(CONCEPT=_F(NOM=__NCOOR),INFO=1)  
        else :  
          if SYME_CHAR=='SANS':
            __NCOOR=CALC_TABLE(TABLE=TABL_DEPL_SUP,
                        ACTION=_F(OPERATION = 'COMB',NOM_PARA='NOEUD',TABLE=TABL_DEPL_INF,))
            tcoor=__NCOOR.EXTR_TABLE()
            DETRUIRE(CONCEPT=_F(NOM=__NCOOR),INFO=1)  
          else :
            tcoor=TABL_DEPL_SUP.EXTR_TABLE()
        nbt = len(tcoor['NOEUD'].values()['NOEUD'])
        xs=array(tcoor['COOR_X'].values()['COOR_X'][:nbt],Float)
        ys=array(tcoor['COOR_Y'].values()['COOR_Y'][:nbt],Float)
        if ndim==2 : zs=Numeric.zeros(nbval,Float)
        elif ndim==3 : zs=array(tcoor['COOR_Z'].values()['COOR_Z'][:nbt],Float)
        ns = tcoor['NOEUD'].values()['NOEUD'][:nbt]
        ns = map(string.rstrip,ns)
        l_coor =  [[ns[i],xs[i],ys[i],zs[i]] for i in range(0,nbt)]
        l_coor = [(i[0],i[1:]) for i in l_coor]
        d_coor = dict(l_coor)

#   ---------- Abscisse curviligne du fond  -------------  
        absfon = [0,]
        for i in range(Nbfond-1) :
          Pfon1 = array([d_coor[LNOFO[i]][0],d_coor[LNOFO[i]][1],d_coor[LNOFO[i]][2]])
          Pfon2 = array([d_coor[LNOFO[i+1]][0],d_coor[LNOFO[i+1]][1],d_coor[LNOFO[i+1]][2]])
          absf = sqrt(dot(transpose(Pfon1-Pfon2),Pfon1-Pfon2)) + absfon[i]
          absfon.append(absf)
        dicoF = dict([(LNOFO[i],absfon[i]) for i in range(Nbfond)])

     
# ---Noeuds LEVRE_SUP et LEVRE_INF: ABSC_CURV_MAXI et PREC_VIS_A_VIS-----
   
        NBTRLS = 0
        NBTRLI = 0
        Lnosup = [None]*Nbf1
        Lnoinf = [None]*Nbf1
        Nbnofo = 0
        Lnofon = []
        precv = PREC_VIS_A_VIS
        if ABSC_CURV_MAXI!=None : rmax = ABSC_CURV_MAXI
        else                   : rmax = 100
        precn = precv * rmax
        rmprec= rmax*(1.+precv/10.)
        for i in range(0,Nbf1) :
           Pfon = array([d_coor[Lnf1[i]][0],d_coor[Lnf1[i]][1],d_coor[Lnf1[i]][2]])
           Tmpsup = []
           Tmpinf = []
           itots = 0
           itoti = 0
           NBTRLS = 0
           NBTRLI = 0
           for k in range(0,20) :
              if dicoS[Lnf1[i]][k] !='':
                 itots = itots +1
                 Nsup =  dicoS[Lnf1[i]][k]
                 Psup = array([d_coor[Nsup][0],d_coor[Nsup][1],d_coor[Nsup][2]])
                 abss = sqrt(dot(transpose(Pfon-Psup),Pfon-Psup))
                 if abss<rmprec :
                    NBTRLS = NBTRLS +1
                    Tmpsup.append(dicoS[Lnf1[i]][k])
              if SYME_CHAR=='SANS':
                 if dicoI[Lnf1[i]][k] !='':
                    itoti = itoti +1
                    Ninf =  dicoI[Lnf1[i]][k]
                    Pinf = array([d_coor[Ninf][0],d_coor[Ninf][1],d_coor[Ninf][2]])
                    absi = sqrt(dot(transpose(Pfon-Pinf),Pfon-Pinf))
# On verifie que les noeuds sont en vis a vis
                    if abss<rmprec :
                      dist = sqrt(dot(transpose(Psup-Pinf),Psup-Pinf))
                      if dist>precn : 
                        message= 'LES NOEUDS NE SONT PAS EN VIS-A-VIS \n'
                        message=message+'DANS LE PLAN PERPENDICULAIRE AU NOEUD %s \n'%Lnf1[i]
                        UTMESS('A', macro, message)
                      else :
                        NBTRLI = NBTRLI +1
                        Tmpinf.append(dicoI[Lnf1[i]][k])
# On verifie qu il y a assez de noeuds
           if NBTRLS < 3 : 
              message= 'IL MANQUE DES POINTS DANS LE PLAN DEFINI PAR LA LEVRE \n'
              message=message+'SUPERIEURE ET PERPENDICULAIRE AU FOND %s :\n'%Lnf1[i]
              if itots<3 : message=message+' Augmenter PREC_NORM dans DEFI_FOND_FISS \n'
              else : message=message+' Augmenter ABSC_CURV_MAXI'
              if Lnf1[i]==LNOFO[0] or Lnf1[i]==LNOFO[-1]: message=message+' OU VERIFIER LES TANGENTES EXTREMITES'
              UTMESS('A',macro, message)
           elif (SYME_CHAR=='SANS') and (NBTRLI < 3) :
              message= 'IL MANQUE DES POINTS DANS LE PLAN DEFINI PAR LA LEVRE \n'
              message=message+'INFERIEURE ET PERPENDICULAIRE AU FOND %s :\n'%Lnf1[i]
              if itoti<3 : message=message+' Augmenter PREC_NORM dans DEFI_FOND_FISS \n'
              else : message=message+' Augmenter ABSC_CURV_MAXI'
              if Lnf1[i]==LNOFO[0] or Lnf1[i]==LNOFO[-1]: message=message+' OU VERIFIER LES TANGENTES EXTREMITES'
              UTMESS('A',macro, message)
           else :
              Lnosup[Nbnofo] = Tmpsup
              if SYME_CHAR=='SANS' : Lnoinf[Nbnofo] = Tmpinf
              Lnofon.append(Lnf1[i])
              Nbnofo = Nbnofo+1
              
   else :
     Nbnofo = 1
  
#   ----------Recuperation de la temperature au fond -------------  
   if Tempe3D :
      resuth = map(string.rstrip,resuth)
      Rth = self.jdc.sds_dict[resuth[0]]
      __TEMP=POST_RELEVE_T(ACTION=_F(INTITULE='Temperature fond de fissure',
                                       NOEUD=Lnofon,TOUT_CMP='OUI',
                                       RESULTAT=Rth,NOM_CHAM='TEMP',TOUT_ORDRE='OUI',
                                       OPERATION='EXTRACTION',),);
      tabtemp=__TEMP.EXTR_TABLE()
      DETRUIRE(CONCEPT=_F(NOM=__TEMP),INFO=1) 
   

#   ------------------------------------------------------------------
#                         BOUCLE SUR NOEUDS DU FOND
#   ------------------------------------------------------------------
   for ino in range(0,Nbnofo) :
      if FOND_FISS and INFO==2 :
            texte="\n\n--> TRAITEMENT DU NOEUD DU FOND DE FISSURE: %s"%Lnofon[ino]
            aster.affiche('MESSAGE',texte)
#   ------------------------------------------------------------------
#                         TABLE 'DEPSUP'
#   ------------------------------------------------------------------
      if FOND_FISS : 
         if TYPE_MAILLAGE =='LIBRE':
            tabsup=TlibS[ino].EXTR_TABLE()
            DETRUIRE(CONCEPT=_F(NOM=TlibS[ino]),INFO=1)
         elif RESULTAT :
            __TSUP=POST_RELEVE_T(ACTION=_F(INTITULE='Deplacement SUP',
                                          NOEUD=Lnosup[ino],
                                          RESULTAT=RESULTAT,
                                          NOM_CHAM='DEPL',
                                          TOUT_ORDRE='OUI',
                                          NOM_CMP=('DX','DY','DZ',),
                                          OPERATION='EXTRACTION',),);
            tabsup=__TSUP.EXTR_TABLE()
            DETRUIRE(CONCEPT=_F(NOM=__TSUP),INFO=1)      
         else :
            tabsup=TABL_DEPL_SUP.EXTR_TABLE()
            veri_tab(tabsup,TABL_DEPL_SUP.nom,ndim)
            Ls = [string.ljust(Lnosup[ino][i],8) for i in range(len(Lnosup[ino]))]
            tabsup=tabsup.NOEUD==Ls
      else :
         tabsup=TABL_DEPL_SUP.EXTR_TABLE()
         veri_tab(tabsup,TABL_DEPL_SUP.nom,ndim)

#   ------------------------------------------------------------------
#                          TABLE 'DEPINF'
#   ------------------------------------------------------------------
      if SYME_CHAR=='SANS': 
         if FOND_FISS : 
            if TYPE_MAILLAGE =='LIBRE':
               tabinf=TlibI[ino].EXTR_TABLE()
               DETRUIRE(CONCEPT=_F(NOM=TlibI[ino]),INFO=1)
            elif RESULTAT :
               __TINF=POST_RELEVE_T(ACTION=_F(INTITULE='Deplacement INF',
                                          NOEUD=Lnoinf[ino],
                                          RESULTAT=RESULTAT,
                                          NOM_CHAM='DEPL',
                                          TOUT_ORDRE='OUI',
                                          NOM_CMP=('DX','DY','DZ',),
                                          OPERATION='EXTRACTION',),);
               tabinf=__TINF.EXTR_TABLE()   
               DETRUIRE(CONCEPT=_F(NOM=__TINF),INFO=1)                 
            else :
               tabinf=TABL_DEPL_INF.EXTR_TABLE()
               if TABL_DEPL_INF==None : UTMESS('F', macro, 'TABL_DEPL_SUP et TABL_DEPL_INF sont obligatoires si SYME_CHAR=SANS')
               veri_tab(tabinf,TABL_DEPL_INF.nom,ndim)
               Li = [string.ljust(Lnoinf[ino][i],8) for i in range(len(Lnoinf[ino]))]
               tabinf=tabinf.NOEUD==Li
         else :
            if TABL_DEPL_INF==None : UTMESS('F', macro, 'TABL_DEPL_SUP et TABL_DEPL_INF sont obligatoires si SYME_CHAR=SANS')
            tabinf=TABL_DEPL_INF.EXTR_TABLE()
            veri_tab(tabinf,TABL_DEPL_INF.nom,ndim)


#   ------------------------------------------------------------------
#               LES INSTANTS DE POST-TRAITEMENT
#   ------------------------------------------------------------------
      if 'INST' in tabsup.para : 
         l_inst=None
         l_inst_tab=tabsup['INST'].values()['INST']
         l_inst_tab=dict([(i,0) for i in l_inst_tab]).keys() #elimine les doublons
         if LIST_ORDRE !=None or NUME_ORDRE !=None :
           l_ord_tab = tabsup['NUME_ORDRE'].values()['NUME_ORDRE']
           l_ord_tab=dict([(i,0) for i in l_ord_tab]).keys() 
           d_ord_tab= [[l_ord_tab[i],l_inst_tab[i]] for i in range(0,len(l_ord_tab))]
           d_ord_tab= [(i[0],i[1]) for i in d_ord_tab]
           d_ord_tab = dict(d_ord_tab)
           if NUME_ORDRE !=None : 
             if type(NUME_ORDRE) not in EnumTypes : NUME_ORDRE=(NUME_ORDRE,)
             l_ord=list(NUME_ORDRE)
           elif LIST_ORDRE !=None : 
              l_ord= aster.getvectjev(string.ljust(LIST_ORDRE.nom,19)+'.VALE') 
           l_inst = []
           for ord in l_ord :
             if ord in l_ord_tab : l_inst.append(d_ord_tab[ord])
             else :  
               message ='LE NUMERO D ORDRE %i N A PAS ETE ETE TROUVE DANS LA TABLE\n'%ord 
               UTMESS('F', macro, message)
         if INST !=None or LIST_INST !=None :
            CRITERE = args['CRITERE']
            PRECISION = args['PRECISION']
         else :
            l_inst=l_inst_tab
            PRECISION = 1.E-6
            CRITERE='ABSOLU'
         if        INST !=None : 
            if type(INST) not in EnumTypes : INST=(INST,)
            l_inst=list(INST)
         elif LIST_INST !=None : l_inst=LIST_INST.Valeurs()
         if      l_inst !=None :
           for inst in l_inst  :
               if CRITERE=='RELATIF' and inst!=0.: match=[x for x in l_inst_tab if abs((inst-x)/inst)<PRECISION]
               else                              : match=[x for x in l_inst_tab if abs(inst-x)<PRECISION]
               if len(match)==0 : 
                 message = 'PAS D INSTANT TROUVE DANS LA TABLE POUR L INSTANT %f\n'%inst
                 UTMESS('F', macro, message)
               if len(match)>=2 :
                 message = 'PLUSIEURS INSTANTS TROUVES DANS LA TABLE POUR L INSTANT %f\n'%inst 
                 UTMESS('F', macro, message)
         else                  :
           l_inst  = l_inst_tab
      else :
         l_inst  = [None,]
   
#   ------------------------------------------------------------------
#                         BOUCLE SUR LES INSTANTS
#   ------------------------------------------------------------------
      for iord in range(len(l_inst)) :
        inst=l_inst[iord]
        if INFO==2 and inst!=None:
            texte="==> INSTANT: %f"%inst
            aster.affiche('MESSAGE',texte)
        if inst!=None:
           if PRECISION == None : PRECISION = 1.E-6
           if CRITERE == None : CRITERE='ABSOLU'
           if inst==0. :
             tabsupi=tabsup.INST.__eq__(VALE=inst,CRITERE='ABSOLU',PRECISION=PRECISION)
             if SYME_CHAR=='SANS': tabinfi=tabinf.INST.__eq__(VALE=inst,CRITERE='ABSOLU',PRECISION=PRECISION)
           else :
             tabsupi=tabsup.INST.__eq__(VALE=inst,CRITERE=CRITERE,PRECISION=PRECISION)
             if SYME_CHAR=='SANS': tabinfi=tabinf.INST.__eq__(VALE=inst,CRITERE=CRITERE,PRECISION=PRECISION)
        else :
           tabsupi=tabsup
           if SYME_CHAR=='SANS': tabinfi=tabinf

#     --- LEVRE SUP :  "ABSC_CURV" CROISSANTES, < RMAX ET DEP ---
        abscs = getattr(tabsupi,'ABSC_CURV').values()
        if not FOND_FISS :
          refs=copy.copy(abscs)
          refs.sort()
          if refs!=abscs : UTMESS('F', macro, 'ABSC_CURV NON CROISSANTS POUR TABL_DEPL_INF')
          if ABSC_CURV_MAXI!=None : rmax = ABSC_CURV_MAXI
          else                    : rmax = abscs[-1]
          precv = PREC_VIS_A_VIS
          rmprec= rmax*(1.+precv/10.)
          refsc=[x for x in refs if x<rmprec]
          nbval = len(refsc)
        else :
          nbval=len(abscs)
        abscs=array(abscs[:nbval])
        coxs=array(tabsupi['COOR_X'].values()['COOR_X'][:nbval],Float)
        coys=array(tabsupi['COOR_Y'].values()['COOR_Y'][:nbval],Float)
        if ndim==2 :  cozs=Numeric.zeros(nbval,Float)
        elif ndim==3 :  cozs=array(tabsupi['COOR_Z'].values()['COOR_Z'][:nbval],Float)

        if FOND_FISS and not RESULTAT : #tri des noeuds avec abscisse
          Pfon = array([d_coor[Lnofon[ino]][0],d_coor[Lnofon[ino]][1],d_coor[Lnofon[ino]][2]])
          abscs = sqrt((coxs-Pfon[0])**2+(coys-Pfon[1])**2+(cozs-Pfon[2])**2)
          tabsupi['Abs_fo'] = abscs
          tabsupi.sort('Abs_fo')
          abscs = getattr(tabsupi,'Abs_fo').values()
          abscs=array(abscs[:nbval])
          coxs=array(tabsupi['COOR_X'].values()['COOR_X'][:nbval],Float)
          coys=array(tabsupi['COOR_Y'].values()['COOR_Y'][:nbval],Float)
          if ndim==2 :  cozs=Numeric.zeros(nbval,Float)
          elif ndim==3 :  cozs=array(tabsupi['COOR_Z'].values()['COOR_Z'][:nbval],Float)
          
        if FOND_FISS and INFO==2 and iord==0 and not TYPE_MAILLAGE =='LIBRE':
          for ks in range(0,nbval) :
            texte="NOEUD RETENU POUR LA LEVRE SUP: %s  %f"%(Lnosup[ino][ks],abscs[ks])
            aster.affiche('MESSAGE',texte)
        dxs=array(tabsupi['DX'].values()['DX'][:nbval],Float)
        dys=array(tabsupi['DY'].values()['DY'][:nbval],Float)
        if ndim==2 : dzs=Numeric.zeros(nbval,Float)
        elif ndim==3 : dzs=array(tabsupi['DZ'].values()['DZ'][:nbval],Float)
        
#     --- LEVRE INF :  "ABSC_CURV" CROISSANTES et < RMAX ---
        if SYME_CHAR=='SANS': 
          absci = getattr(tabinfi,'ABSC_CURV').values()
          if not FOND_FISS :
            refi=copy.copy(absci)
            refi.sort()
            if refi!=absci : UTMESS('F', macro, 'ABSC_CURV NON CROISSANTS POUR TABL_DEPL_SUP')
            refic=[x for x in refi if x<rmprec]
            nbvali=len(refic)
          else :
            nbvali=len(absci)
          if nbvali!=nbval :
             message= 'DIFFERENCE DE POINTS ENTRE LA LEVRE SUPERIEURE ET LA LEVRE INFERIEURE'
             if FOND_FISS : message=message+' POUR TRAITER LE NOEUD %.s \n'%Lnofon[i]
             message=message+' Nombre de points - levre superieure : %i\n'%len(refsc)
             message=message+' Nombre de points - levre inferieure : %i\n'%len(refic)
             UTMESS('A',macro, message)
          nbval=min(nbval,nbvali)
          absci=array(absci[:nbval])
          coxi=array(tabinfi['COOR_X'].values()['COOR_X'][:nbval],Float)
          coyi=array(tabinfi['COOR_Y'].values()['COOR_Y'][:nbval],Float)
          if ndim==2 : cozi=Numeric.zeros(nbval,Float)
          elif ndim==3 : cozi=array(tabinfi['COOR_Z'].values()['COOR_Z'][:nbval],Float)
#     --- ON VERIFIE QUE LES NOEUDS SONT EN VIS_A_VIS  (SYME=SANS)   ---
          if not FOND_FISS :
            precn = precv * rmax
            dist=(coxs-coxi)**2+(coys-coyi)**2+(cozs-cozi)**2
            dist=sqrt(dist)
            for d in dist :
               if d>precn : UTMESS('F', macro, 'LES NOEUDS NE SONT PAS EN VIS_A_VIS')
          
          if FOND_FISS and not RESULTAT :#tri des noeuds avec abscisse
            Pfon = array([d_coor[Lnofon[ino]][0],d_coor[Lnofon[ino]][1],d_coor[Lnofon[ino]][2]])
            absci = sqrt((coxi-Pfon[0])**2+(coyi-Pfon[1])**2+(cozi-Pfon[2])**2)
            tabinfi['Abs_fo'] = absci
            tabinfi.sort('Abs_fo')
            absci = getattr(tabinfi,'Abs_fo').values()
            absci=array(abscs[:nbval])
            coxi=array(tabinfi['COOR_X'].values()['COOR_X'][:nbval],Float)
            coyi=array(tabinfi['COOR_Y'].values()['COOR_Y'][:nbval],Float)
            if ndim==2 :  cozi=Numeric.zeros(nbval,Float)
            elif ndim==3 :  cozi=array(tabinfi['COOR_Z'].values()['COOR_Z'][:nbval],Float)

          dxi=array(tabinfi['DX'].values()['DX'][:nbval],Float)
          dyi=array(tabinfi['DY'].values()['DY'][:nbval],Float)
          if ndim==2 : dzi=Numeric.zeros(nbval,Float)
          elif ndim==3 : dzi=array(tabinfi['DZ'].values()['DZ'][:nbval],Float)
          
          if FOND_FISS and INFO==2 and iord==0 and not TYPE_MAILLAGE =='LIBRE':
            for ki in range(0,nbval) :
               texte="NOEUD RETENU POUR LA LEVRE INF: %s  %f"%(Lnoinf[ino][ki],absci[ki])
               aster.affiche('MESSAGE',texte)

#     --- TESTS NOMBRE DE NOEUDS---
        if nbval<3 :
           message= 'IL FAUT AU MOINS TROIS NOEUDS DANS LE PLAN DEFINI PAR LES LEVRES ET PERPENDICULAIRE AU FOND DE FISSURE'
           if FOND_FISS : message=message+'Noeud %.s \n'%Lnofon[ino]
           message=message+' : augmenter ABSC_CURV_MAXI\n'
           UTMESS('F',macro, message)
           
#   ---------- CALCUL PROP. MATERIAU AVEC TEMPERATURE -----------  
        if Tempe3D :
           tempeno=tabtemp.NOEUD==Lnofon[ino]
           tempeno=tempeno.INST.__eq__(VALE=inst,CRITERE='ABSOLU',PRECISION=PRECISION)
           nompar = ('TEMP',)
           valpar = (tempeno.TEMP.values()[0],)
           nomres=['E','NU']
           valres,codret = MATER.RCVALE('ELAS',nompar,valpar,nomres,'F')
           e = valres[0]
           nu = valres[1] 
           coefd  = e * sqrt(2.*pi)      / ( 8.0 * (1. - nu**2))
           coefd3 = e*sqrt(2*pi) / ( 8.0 * (1. + nu))
           coefg  = (1. - nu**2) / e
           coefg3 = (1. + nu)  / e

#     ------------------------------------------------------------------
#                           CHANGEMENT DE REPERE
#     ------------------------------------------------------------------
#
#       1 : VECTEUR NORMAL AU PLAN DE LA FISSURE
#           ORIENTE LEVRE INFERIEURE VERS LEVRE SUPERIEURE
#       2 : VECTEUR NORMAL AU FOND DE FISSURE EN M
#       3 : VECTEUR TANGENT AU FOND DE FISSURE EN M
#
        if SYME_CHAR=='SANS' :
           vo =  array([( coxs[-1]+coxi[-1] )/2.,( coys[-1]+coyi[-1] )/2.,( cozs[-1]+cozi[-1] )/2.])
           ve =  array([( coxs[0 ]+coxi[0 ] )/2.,( coys[0 ]+coyi[0 ] )/2.,( cozs[0 ]+cozi[0 ] )/2.])
        else :
           vo = array([ coxs[-1], coys[-1], cozs[-1]])
           ve = array([ coxs[0], coys[0], cozs[0]])
        v1 =  array(VECT_K1)
        v2 =  ve-vo
        v2 =  v2/sqrt(v2[0]**2+v2[1]**2+v2[2]**2)
        v1p = sum(v2*v1)
        v1  = v1-v1p*v2
        v1  = v1/sqrt(v1[0]**2+v1[1]**2+v1[2]**2)
        v3  = array([v1[1]*v2[2]-v2[1]*v1[2],v1[2]*v2[0]-v2[2]*v1[0],v1[0]*v2[1]-v2[0]*v1[1]])
        pgl  = asarray([v1,v2,v3])
        dpls = asarray([dxs,dys,dzs])
        dpls = matrixmultiply(pgl,dpls)
        if SYME_CHAR=='SANS' :
           dpli = asarray([dxi,dyi,dzi])
           dpli = matrixmultiply(pgl,dpli)
        else :
           dpli = [multiply(dpls[0],-1.),dpls[1],dpls[2]]
#     ------------------------------------------------------------------
#                           CALCUL DES K1, K2, K3
#     ------------------------------------------------------------------
        saut=(dpls-dpli)
        isig=sign(transpose(resize(saut[:,-1],(nbval-1,3))))
        isig=sign(isig+0.001)
        saut=saut*array([[coefd]*nbval,[coefd]*nbval,[coefd3]*nbval])
        saut=saut**2
        ksig = isig[:,1]
        ksig = array([ksig,ksig])
        ksig = transpose(ksig)
        kgsig=resize(ksig,(1,6))[0]
        if INFO==2 :
          mcfact=[]
          mcfact.append(_F(PARA='ABSC_CURV'  ,LISTE_R=abscs.tolist() ))
          mcfact.append(_F(PARA='DEPL_SUP_DX',LISTE_R=dpls[0].tolist() ))
          mcfact.append(_F(PARA='DEPL_INF_DX',LISTE_R=dpli[0].tolist() ))
          mcfact.append(_F(PARA='SAUT_DX'    ,LISTE_R=saut[0].tolist() ))
          mcfact.append(_F(PARA='DEPL_SUP_DY',LISTE_R=dpls[1].tolist() ))
          mcfact.append(_F(PARA='DEPL_INF_DY',LISTE_R=dpli[1].tolist() ))
          mcfact.append(_F(PARA='SAUT_DY'    ,LISTE_R=saut[1].tolist() ))
          if ndim==3 :
            mcfact.append(_F(PARA='DEPL_SUP_DZ',LISTE_R=dpls[2].tolist() ))
            mcfact.append(_F(PARA='DEPL_INF_DZ',LISTE_R=dpli[2].tolist() ))
            mcfact.append(_F(PARA='SAUT_DZ'    ,LISTE_R=saut[2].tolist() ))
          __resu0=CREA_TABLE(LISTE=mcfact,TITRE='--> SAUTS')
          aster.affiche('MESSAGE',__resu0.EXTR_TABLE().__repr__())
          DETRUIRE(CONCEPT=_F(NOM=__resu0),INFO=1)
#     ------------------------------------------------------------------
#                           --- METHODE 1 ---
#     ------------------------------------------------------------------
        x1 = abscs[1:-1]
        x2 = abscs[2:  ]
        y1 = saut[:,1:-1]/x1
        y2 = saut[:,2:  ]/x2
        k  = abs(y1-x1*(y2-y1)/(x2-x1))
        g  = coefg*(k[0]+k[1])+coefg3*k[2]
        kg1 = [max(k[0]),min(k[0]),max(k[1]),min(k[1]),max(k[2]),min(k[2])]
        kg1 = sqrt(kg1)*kgsig
        kg1=Numeric.concatenate([kg1,[max(g),min(g)]])
        vk  = sqrt(k)*isig[:,:-1]
        if INFO==2 :
          mcfact=[]
          mcfact.append(_F(PARA='ABSC_CURV_1' ,LISTE_R=x1.tolist() ))
          mcfact.append(_F(PARA='ABSC_CURV_2' ,LISTE_R=x2.tolist() ))
          mcfact.append(_F(PARA='K1'          ,LISTE_R=vk[0].tolist() ))
          mcfact.append(_F(PARA='K2'          ,LISTE_R=vk[1].tolist() ))
          if ndim==3 :
            mcfact.append(_F(PARA='K3'        ,LISTE_R=vk[2].tolist() ))
          mcfact.append(_F(PARA='G'           ,LISTE_R=g.tolist() ))
          __resu1=CREA_TABLE(LISTE=mcfact,TITRE='--> METHODE 1')
          aster.affiche('MESSAGE',__resu1.EXTR_TABLE().__repr__())
          DETRUIRE(CONCEPT=_F(NOM=__resu1),INFO=1)
#     ------------------------------------------------------------------
#                           --- METHODE 2 ---
#     ------------------------------------------------------------------
        x1 = abscs[1: ]
        y1 = saut[:,1:]
        k  = abs(y1/x1)
        g  = coefg*(k[0]+k[1])+coefg3*k[2]
        kg2= [max(k[0]),min(k[0]),max(k[1]),min(k[1]),max(k[2]),min(k[2])]
        kg2 = sqrt(kg2)*kgsig
        kg2=Numeric.concatenate([kg2,[max(g),min(g)]])
        vk = sqrt(k)*isig
        if INFO==2 :
          mcfact=[]
          mcfact.append(_F(PARA='ABSC_CURV' ,LISTE_R=x1.tolist() ))
          mcfact.append(_F(PARA='K1'        ,LISTE_R=vk[0].tolist() ))
          mcfact.append(_F(PARA='K2'        ,LISTE_R=vk[1].tolist() ))
          if ndim==3 :
            mcfact.append(_F(PARA='K3'      ,LISTE_R=vk[2].tolist() ))
          mcfact.append(_F(PARA='G'         ,LISTE_R=g.tolist() ))
          __resu2=CREA_TABLE(LISTE=mcfact,TITRE='--> METHODE 2')
          aster.affiche('MESSAGE',__resu2.EXTR_TABLE().__repr__())
          DETRUIRE(CONCEPT=_F(NOM=__resu2),INFO=1)
#     ------------------------------------------------------------------
#                           --- METHODE 3 ---
#     ------------------------------------------------------------------
        x1 = abscs[:-1]
        x2 = abscs[1: ]
        y1 = saut[:,:-1]
        y2 = saut[:,1: ]
        k  = (sqrt(y2)*sqrt(x2)+sqrt(y1)*sqrt(x1))*(x2-x1)
        k  = Numeric.sum(transpose(k))
        de = abscs[-1]
        vk = (k/de**2)*isig[:,0]
        g  = coefg*(vk[0]**2+vk[1]**2)+coefg3*vk[2]**2
        kg3=Numeric.concatenate([[vk[0]]*2,[vk[1]]*2,[vk[2]]*2,[g]*2])
        if INFO==2 :
          mcfact=[]
          mcfact.append(_F(PARA='K1'        ,LISTE_R=vk[0] ))
          mcfact.append(_F(PARA='K2'        ,LISTE_R=vk[1] ))
          if ndim==3 :
            mcfact.append(_F(PARA='K3'      ,LISTE_R=vk[2] ))
          mcfact.append(_F(PARA='G'         ,LISTE_R=g ))
          __resu3=CREA_TABLE(LISTE=mcfact,TITRE='--> METHODE 3')
          aster.affiche('MESSAGE',__resu3.EXTR_TABLE().__repr__())
          DETRUIRE(CONCEPT=_F(NOM=__resu3),INFO=1)
#     ------------------------------------------------------------------
#                           CREATION DE LA TABLE 
#     ------------------------------------------------------------------
        kg=array([kg1,kg2,kg3])
        kg=transpose(kg)
        mcfact=[]
        if TITRE != None :
          titre = TITRE
        else :
          v = aster.__version__
          titre = 'ASTER %s - CONCEPT CALCULE PAR POST_K1_K2_K3 LE &DATE A &HEURE \n'%v
        if FOND_FISS : 
          mcfact.append(_F(PARA='NOEUD_FOND',LISTE_K=[Lnofon[ino],]*3))
          mcfact.append(_F(PARA='ABSC_CURV',LISTE_R=[dicoF[Lnofon[ino]]]*3))
        mcfact.append(_F(PARA='METHODE',LISTE_I=(1,2,3)))
        mcfact.append(_F(PARA='K1_MAX' ,LISTE_R=kg[0].tolist() ))
        mcfact.append(_F(PARA='K1_MIN' ,LISTE_R=kg[1].tolist() ))
        mcfact.append(_F(PARA='K2_MAX' ,LISTE_R=kg[2].tolist() ))
        mcfact.append(_F(PARA='K2_MIN' ,LISTE_R=kg[3].tolist() ))
        if ndim==3 :
          mcfact.append(_F(PARA='K3_MAX' ,LISTE_R=kg[4].tolist() ))
          mcfact.append(_F(PARA='K3_MIN' ,LISTE_R=kg[5].tolist() ))
        mcfact.append(_F(PARA='G_MAX'  ,LISTE_R=kg[6].tolist() ))
        mcfact.append(_F(PARA='G_MIN'  ,LISTE_R=kg[7].tolist() ))
        if  (ino==0 and iord==0) and inst==None :
           tabout=CREA_TABLE(LISTE=mcfact,TITRE = titre)
        elif iord==0 and ino==0 and inst!=None :
           mcfact=[_F(PARA='INST'  ,LISTE_R=[inst,]*3      )]+mcfact
           tabout=CREA_TABLE(LISTE=mcfact,TITRE = titre)
        else :
           if inst!=None : mcfact=[_F(PARA='INST'  ,LISTE_R=[inst,]*3     )]+mcfact
           __tabi=CREA_TABLE(LISTE=mcfact,)
           npara = ['K1_MAX','METHODE']
           if inst!=None : npara.append('INST')
           if FOND_FISS : npara.append('NOEUD_FOND')
           tabout=CALC_TABLE(reuse=tabout,TABLE=tabout,TITRE = titre,
                              ACTION=_F(OPERATION = 'COMB',NOM_PARA=npara,TABLE=__tabi,))

# Tri pour conserver le meme ordre que operateur initial en fortran
   if len(l_inst)!=1 and FOND_FISS :
      tabout=CALC_TABLE(reuse=tabout,TABLE=tabout,
                      ACTION=_F(OPERATION = 'TRI',NOM_PARA=('INST','ABSC_CURV','METHODE'),ORDRE='CROISSANT'))

   return ier
 
