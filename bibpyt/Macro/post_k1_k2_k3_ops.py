#@ MODIF post_k1_k2_k3_ops Macro  DATE 10/03/2008   AUTEUR GALENNE E.GALENNE 
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
   from Utilitai.Utmess     import  UTMESS
   macro = 'POST_K1_K2_K3'
   for label in ('DX','DY','COOR_X','COOR_Y','ABSC_CURV') :
       if label not in tab.para :
          UTMESS('F','RUPTURE0_2',valk=[label,nom])
   if ndim==3 :
      if 'DZ'     not in tab.para :
          label='DZ'
          UTMESS('F','RUPTURE0_2',valk=[label,nom])
      if 'COOR_Z' not in tab.para :
          label='COOR_Z'
          UTMESS('F','RUPTURE0_2',valk=[label,nom])

def cross_product(a,b):
    cross = [0]*3
    cross[0] = a[1]*b[2]-a[2]*b[1]
    cross[1] = a[2]*b[0]-a[0]*b[2]
    cross[2] = a[0]*b[1]-a[1]*b[0]
    return cross

def complete(Tab):
    n = len(Tab)
    for i in range(n) :
      if Tab[i]==None : Tab[i] = 0.
    return Tab
 
def moy(t):
    m = 0
    for value in t :
      m += value
    return (m/len(t))
     
def post_k1_k2_k3_ops(self,MODELISATION,FOND_FISS,FISSURE,MATER,RESULTAT,
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
   from SD.sd_l_charges import sd_l_charges
   from SD.sd_mater     import sd_compor1
   EnumTypes = (ListType, TupleType)

   macro = 'POST_K1_K2_K3'
   from Accas               import _F
   from Utilitai.Utmess     import  UTMESS

   ier = 0
   # La macro compte pour 1 dans la numerotation des commandes
   self.set_icmd(1)

   # Le concept sortant (de type table_sdaster ou dérivé) est tab
   self.DeclareOut('tabout', self.sd)
   
   # On importe les definitions des commandes a utiliser dans la macro
   # Le nom de la variable doit etre obligatoirement le nom de la commande
   CREA_TABLE    = self.get_cmd('CREA_TABLE')
   CALC_TABLE    = self.get_cmd('CALC_TABLE')
   POST_RELEVE_T    = self.get_cmd('POST_RELEVE_T')
   DETRUIRE      = self.get_cmd('DETRUIRE')
   DEFI_GROUP      = self.get_cmd('DEFI_GROUP')
   MACR_LIGN_COUPE      = self.get_cmd('MACR_LIGN_COUPE')

   AFFE_MODELE      = self.get_cmd('AFFE_MODELE')
   PROJ_CHAMP      = self.get_cmd('PROJ_CHAMP')
      
#   ------------------------------------------------------------------
#                         CARACTERISTIQUES MATERIAUX
#   ------------------------------------------------------------------
   matph = MATER.NOMRC.get()  
   phenom=None
   for cmpt in matph :
       if cmpt[:4]=='ELAS' :
          phenom=cmpt
          break
   if phenom==None : UTMESS('F','RUPTURE0_5')
#   --- RECHERCHE SI LE MATERIAU DEPEND DE LA TEMPERATURE:
   compor = sd_compor1('%-8s.%s' % (MATER.nom, phenom))
   valk = [s.strip() for s in compor.VALK.get()]
   valr = compor.VALR.get()
   dicmat=dict(zip(valk,valr))
#   --- PROPRIETES MATERIAUX DEPENDANTES DE LA TEMPERATURE
   Tempe3D = False
   if FOND_FISS and args['EVOL_THER'] : 
# on recupere juste le nom du resultat thermique (la température est variable de commande)
      ndim   = 3
      Tempe3D=True
      resuth=string.ljust(args['EVOL_THER'].nom,8).rstrip()
   if dicmat.has_key('TEMP_DEF') and not args['EVOL_THER'] :
      nompar = ('TEMP',)
      valpar = (dicmat['TEMP_DEF'],)
      UTMESS('A','RUPTURE0_6',valr=valpar)
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
         coefk='K1 K2 K3'
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
         coefk='K1 K2'
         ndim   = 2
         coefd  = coefd / ( 8. * unmnu2 )
         coefg  = unmnu2 / e
         coefg3 = unpnu  / e
      elif MODELISATION=='C_PLAN' :
         coefk='K1 K2'
         ndim   = 2
         coefd  = coefd / 8.
         coefg  = 1. / e
         coefg3 = unpnu / e
      else :
         UTMESS('F','RUPTURE0_10')


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
      if NB_NOEUD_COUPE ==None : NB_NOEUD_COUPE = 5
      LNOFO = FOND_FISS.FOND_______NOEU.get()
      RECOL = False
# Cas double fond de fissure : par convention les noeuds sont ceux de fond_inf
      if LNOFO==None :
         RECOL = True
         LNOFO = FOND_FISS.FONDINF____NOEU.get()
         if LNOFO==None : UTMESS('F','RUPTURE0_11')
      LNOFO = map(string.rstrip,LNOFO)
      Nbfond = len(LNOFO)

      if MODELISATION=='3D' :
#   ----------Mots cles TOUT, NOEUD, SANS_NOEUD -------------
        Typ = FOND_FISS.FOND_______TYPE.get()
        if (Typ[0]=='SEG2    ') or (Typ[0]=='SEG3    ' and TOUT=='OUI') :
           pas = 1
        elif (Typ[0]=='SEG3    ') : 
           pas = 2
        else :
           UTMESS('F','RUPTURE0_12')
####
        NO_SANS = []
        NO_AVEC = []
        if GROUP_NO!=None :
          collgrno = MAILLAGE.GROUPENO.get()
          cnom = MAILLAGE.NOMNOE.get()
          if type(GROUP_NO) not in EnumTypes : GROUP_NO = (GROUP_NO,)
          for m in range(len(GROUP_NO)) :
            ngrno=GROUP_NO[m].ljust(8).upper()
            if ngrno not in collgrno.keys() :
              UTMESS('F','RUPTURE0_13',valk=ngrno)
            for i in range(len(collgrno[ngrno])) : NO_AVEC.append(cnom[collgrno[ngrno][i]-1])
          NO_AVEC= map(string.rstrip,NO_AVEC)
        if NOEUD!=None : 
          if type(NOEUD) not in EnumTypes : NO_AVEC = (NOEUD,)
          else : NO_AVEC = NOEUD
        if SANS_GROUP_NO!=None :
          collgrno = MAILLAGE.GROUPENO.get()
          cnom = MAILLAGE.NOMNOE.get()
          if type(SANS_GROUP_NO) not in EnumTypes : SANS_GROUP_NO = (SANS_GROUP_NO,)
          for m in range(len(SANS_GROUP_NO)) :
            ngrno=SANS_GROUP_NO[m].ljust(8).upper()
            if ngrno not in collgrno.keys() :
              UTMESS('F','RUPTURE0_13',valk=ngrno)
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
              UTMESS('F','RUPTURE0_15',valk=NO_AVEC[i])
        else :
           for i in range(0,Nbfond,pas) :
              if not (LNOFO[i] in NO_SANS) :
                 Lnf1.append(LNOFO[i])
                 Nbf1 = Nbf1 +1
      else :
        Lnf1 = LNOFO
        Nbf1 = 1
        
##### Cas maillage libre###########
# creation des directions normales et macr_lign_coup
      if TYPE_MAILLAGE =='LIBRE':
        if not RESULTAT : UTMESS('F','RUPTURE0_16')
        Lnofon = Lnf1
        Nbnofo = Nbf1
        ListmaS = FOND_FISS.LEVRESUP___MAIL.get()
        if SYME_CHAR=='SANS':
          ListmaI = FOND_FISS.LEVREINF___MAIL.get()
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
        if ndim==2 : zs=Numeric.zeros(nbt,Float)
        elif ndim==3 : zs=array(tcoorf['COOR_Z'].values()['COOR_Z'][:nbt],Float)
        ns = tcoorf['NOEUD'].values()['NOEUD'][:nbt]
        ns = map(string.rstrip,ns)
        l_coorf =  [[ns[i],xs[i],ys[i],zs[i]] for i in range(0,nbt)]
        l_coorf = [(i[0],i[1:]) for i in l_coorf]
        d_coorf = dict(l_coorf) 
# Coordonnee d un pt quelconque des levres pr determination sens de propagation
        cmail=MAILLAGE.NOMMAI.get()
        for i in range(len(cmail)) :
            if cmail[i] == ListmaS[0] : break
        colcnx=MAILLAGE.CONNEX.get()
        cnom = MAILLAGE.NOMNOE.get()
        NO_TMP = []
        for k in range(len(colcnx[i+1])) : NO_TMP.append(cnom[colcnx[i+1][k]-1])
        __NCOLEV=POST_RELEVE_T(ACTION=_F(INTITULE='Tab pour coordonnees pt levre',
                                          NOEUD = NO_TMP,
                                           RESULTAT=RESULTAT,
                                           NOM_CHAM='DEPL',NUME_ORDRE=1,NOM_CMP=('DX',),
                                           OPERATION='EXTRACTION',),);
        tcoorl=__NCOLEV.EXTR_TABLE()
        DETRUIRE(CONCEPT=_F(NOM=__NCOLEV),INFO=1) 
        nbt = len(tcoorl['NOEUD'].values()['NOEUD'])
        xl=moy(tcoorl['COOR_X'].values()['COOR_X'][:nbt])
        yl=moy(tcoorl['COOR_Y'].values()['COOR_Y'][:nbt])
        zl=moy(tcoorl['COOR_Z'].values()['COOR_Z'][:nbt])
        Plev = array([xl, yl, zl])
# Calcul des normales a chaque noeud du fond
        v1 =  array(VECT_K1)
        VN = [None]*Nbfond
        absfon = [0,]
        if MODELISATION=='3D' :
          DTANOR = FOND_FISS.DTAN_ORIGINE.get()
          Pfon2 = array([d_coorf[LNOFO[0]][0],d_coorf[LNOFO[0]][1],d_coorf[LNOFO[0]][2]])
          VLori = Pfon2 - Plev
          if DTANOR != None :
            VN[0] = array(DTANOR)
          else :
            Pfon3 = array([d_coorf[LNOFO[1]][0],d_coorf[LNOFO[1]][1],d_coorf[LNOFO[1]][2]])
            VT = (Pfon3 - Pfon2)/sqrt(dot(transpose(Pfon3-Pfon2),Pfon3-Pfon2))
            VN[0] = array(cross_product(VT,v1))
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
          VLextr = Pfon2 - Plev
          absf = sqrt(dot(transpose(Pfon1-Pfon2),Pfon1-Pfon2)) + absfon[i-1]
          absfon.append(absf)
          DTANEX = FOND_FISS.DTAN_EXTREMITE.get()
          if DTANEX != None :
            VN[i] = array(DTANEX)
          else :
            VT = (Pfon2 - Pfon1)/sqrt(dot(transpose(Pfon2-Pfon1),Pfon2-Pfon1))
            VN[i] = array(cross_product(VT,v1))
          dicoF = dict([(LNOFO[i],absfon[i]) for i in range(Nbfond)])  
          dicVN = dict([(LNOFO[i],VN[i]) for i in range(Nbfond)])
#Sens de la tangente       
          v = cross_product(VLori,VLextr)
          sens = sign(dot(transpose(v),v1))
#Cas 2D              
        if MODELISATION!='3D' :
          DTANOR = False
          DTANEX = False
          VT = array([0.,0.,1.])
          VN = array(cross_product(v1,VT))
          dicVN = dict([(LNOFO[0],VN)])
          Pfon = array([d_coorf[LNOFO[0]][0],d_coorf[LNOFO[0]][1],d_coorf[LNOFO[0]][2]])
          VLori = Pfon - Plev
          sens = sign(dot(transpose(VN),VLori))
#Extraction dep sup/inf sur les normales          
        TlibS = [None]*Nbf1
        TlibI = [None]*Nbf1
        if NB_NOEUD_COUPE < 3 : 
          UTMESS('A','RUPTURE0_17')
          NB_NOEUD_COUPE = 5
        MOD = aster.getvectjev(string.ljust(RESULTAT.nom,19)+'.MODL        ')
        if MOD==None : UTMESS('F','RUPTURE0_18')
        MODEL = self.jdc.sds_dict[MOD[0].rstrip()]
        for i in range(Nbf1):
          Porig = array(d_coorf[Lnf1[i]] )
          if Lnf1[i]==LNOFO[0] and DTANOR : Pextr = Porig - ABSC_CURV_MAXI*dicVN[Lnf1[i]]
          elif Lnf1[i]==LNOFO[Nbfond-1] and DTANEX : Pextr = Porig - ABSC_CURV_MAXI*dicVN[Lnf1[i]]
          else : Pextr = Porig - ABSC_CURV_MAXI*dicVN[Lnf1[i]]*sens
          TlibS[i] = MACR_LIGN_COUPE(RESULTAT=RESULTAT,
                NOM_CHAM='DEPL',MODELE=MODEL, VIS_A_VIS=_F(MAILLE_1 = ListmaS),
                LIGN_COUPE=_F(NB_POINTS=NB_NOEUD_COUPE,COOR_ORIG=(Porig[0],Porig[1],Porig[2],),
                               TYPE='SEGMENT', COOR_EXTR=(Pextr[0],Pextr[1],Pextr[2]),),);
          if SYME_CHAR=='SANS':
            TlibI[i] = MACR_LIGN_COUPE(RESULTAT=RESULTAT,
                  NOM_CHAM='DEPL',MODELE=MODEL, VIS_A_VIS=_F(MAILLE_1 = ListmaI),
                LIGN_COUPE=_F(NB_POINTS=NB_NOEUD_COUPE,COOR_ORIG=(Porig[0],Porig[1],Porig[2],),
                               TYPE='SEGMENT',COOR_EXTR=(Pextr[0],Pextr[1],Pextr[2]),),);


##### Cas maillage regle###########
      else:
#   ---------- Dictionnaires des levres  -------------  
        NnormS = FOND_FISS.SUPNORM____NOEU.get()
        if NnormS==None : 
          UTMESS('F','RUPTURE0_19')
        NnormS = map(string.rstrip,NnormS)
        if LNOFO[0]==LNOFO[-1] and MODELISATION=='3D' : Nbfond=Nbfond-1  # Cas fond de fissure ferme
        NnormS = [[LNOFO[i],NnormS[i*20:(i+1)*20]] for i in range(0,Nbfond)]
        NnormS = [(i[0],i[1][0:]) for i in NnormS]
        dicoS = dict(NnormS)
        if SYME_CHAR=='SANS':
           NnormI = FOND_FISS.INFNORM____NOEU.get()
           if NnormI==None : 
             UTMESS('F','RUPTURE0_20')
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
        if ndim==2 : zs=Numeric.zeros(nbt,Float)
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
                        UTMESS('A','RUPTURE0_21',valk=Lnf1[i])
                      else :
                        NBTRLI = NBTRLI +1
                        Tmpinf.append(dicoI[Lnf1[i]][k])
# On verifie qu il y a assez de noeuds
           if NBTRLS < 3 : 
              UTMESS('A+','RUPTURE0_22',valk=Lnf1[i])
              if Lnf1[i]==LNOFO[0] or Lnf1[i]==LNOFO[-1]: UTMESS('A+','RUPTURE0_23')
              if itots<3 : UTMESS('A','RUPTURE0_24')
              else : UTMESS('A','RUPTURE0_25')
           elif (SYME_CHAR=='SANS') and (NBTRLI < 3) :
              UTMESS('A+','RUPTURE0_26',valk=Lnf1[i])
              if Lnf1[i]==LNOFO[0] or Lnf1[i]==LNOFO[-1]: UTMESS('A+','RUPTURE0_23')
              if itoti<3 : UTMESS('A','RUPTURE0_24')
              else :UTMESS('A','RUPTURE0_25')
#              UTMESS('A','RUPTURE0_23')
           else :
              Lnosup[Nbnofo] = Tmpsup
              if SYME_CHAR=='SANS' : Lnoinf[Nbnofo] = Tmpinf
              Lnofon.append(Lnf1[i])
              Nbnofo = Nbnofo+1
        if Nbnofo == 0 :
          UTMESS('F','RUPTURE0_30')

#------------- Cas X-FEM ---------------------------------
   elif FISSURE :
     MAILLAGE = args['MAILLAGE']
     DTAN_ORIG = args['DTAN_ORIG']
     DTAN_EXTR = args['DTAN_EXTR']
#Projection du resultat sur le maillage lineaire initial     
     MOD = aster.getvectjev(string.ljust(RESULTAT.nom,19)+'.MODL        ')
     if MOD==None : UTMESS('F','RUPTURE0_18')
     MODEL = self.jdc.sds_dict[MOD[0].rstrip()]
     xcont = MODEL.xfem.XFEM_CONT.get()
     if xcont[0] == 0 :
       __RESX = RESULTAT
# Si XFEM + contact : il faut reprojeter sur le maillage lineaire
     if xcont[0] != 0 :
       __MODLINE=AFFE_MODELE(MAILLAGE=MAILLAGE,
                           AFFE=(_F(TOUT='OUI',
                            PHENOMENE='MECANIQUE',
                            MODELISATION=MODELISATION,),),);        
       __RESX=PROJ_CHAMP(METHODE='ELEM',TYPE_CHAM='NOEU',NOM_CHAM='DEPL',
                     RESULTAT=RESULTAT,
                     MODELE_1=MODEL,
                     MODELE_2=__MODLINE, );   
#Recuperation des coordonnees des points du fond de fissure (x,y,z,absc_curv)
     Listfo = FISSURE.FONDFISS.get()
     Nbfond = len(Listfo)/4
# Calcul de la direction de propagation en chaque point du fond
     VP = [None]*Nbfond
     VN = [None]*Nbfond
     absfon = [0,]
     Vpropa = FISSURE.BASEFOND.get()
# Cas fissure non necessairement plane     
     if VECT_K1 == None :
       i = 0
       if MODELISATION=='3D' :
         if DTAN_ORIG != None :
           VP[0] = array(DTAN_ORIG)
           VP[0] = VP[0]/sqrt(VP[0][0]**2+VP[0][1]**2+VP[0][2]**2)
           VN[0] = array([Vpropa[0],Vpropa[1],Vpropa[2]])
           verif = dot(transpose(VP[0]),VN[0]) 
           if abs(verif) > 0.01:
             UTMESS('A','RUPTURE1_33',valr=[VN[0][0],VN[0][1],VN[0][2]])
         else :
           VN[0] = array([Vpropa[0],Vpropa[1],Vpropa[2]])
           VP[0] = array([Vpropa[3+0],Vpropa[3+1],Vpropa[3+2]])
         for i in range(1,Nbfond-1):
           Pfon1 = array([Listfo[4*(i-1)],Listfo[4*(i-1)+1],Listfo[4*(i-1)+2]])
           Pfon2 = array([Listfo[4*i],Listfo[4*i+1],Listfo[4*i+2]])
           absf = sqrt(dot(transpose(Pfon1-Pfon2),Pfon1-Pfon2)) + absfon[i-1]
           absfon.append(absf)
           VN[i] = array([Vpropa[6*i],Vpropa[6*i+1],Vpropa[6*i+2]])
           VP[i] = array([Vpropa[3+6*i],Vpropa[3+6*i+1],Vpropa[3+6*i+2]])
           verif = dot(transpose(VN[i]),VN[i-1]) 
           if abs(verif) < 0.98:
             UTMESS('A','RUPTURE1_35',vali=[i-1,i])
         i = Nbfond-1
         Pfon1 = array([Listfo[4*(i-1)],Listfo[4*(i-1)+1],Listfo[4*(i-1)+2]])
         Pfon2 = array([Listfo[4*i],Listfo[4*i+1],Listfo[4*i+2]])
         absf = sqrt(dot(transpose(Pfon1-Pfon2),Pfon1-Pfon2)) + absfon[i-1]
         absfon.append(absf)
         if DTAN_EXTR != None :
           VP[i] = array(DTAN_EXTR)
           VN[i] = array([Vpropa[6*i],Vpropa[6*i+1],Vpropa[6*i+2]])
           verif = dot(transpose(VP[i]),VN[0]) 
           if abs(verif) > 0.01:
             UTMESS('A','RUPTURE1_34',valr=[VN[i][0],VN[i][1],VN[i][2]])
         else :
           VN[i] = array([Vpropa[6*i],Vpropa[6*i+1],Vpropa[6*i+2]])
           VP[i] = array([Vpropa[3+6*i],Vpropa[3+6*i+1],Vpropa[3+6*i+2]])
       else : 
         for i in range(0,Nbfond):
           VP[i] = array([Vpropa[2+4*i],Vpropa[3+4*i],0.])
           VN[i] = array([Vpropa[0+4*i],Vpropa[1+4*i],0.])
# Cas fissure plane (VECT_K1 donne)
     if VECT_K1 != None :
       v1 =  array(VECT_K1)
       v1  = v1/sqrt(v1[0]**2+v1[1]**2+v1[2]**2)
       v1 =  array(VECT_K1)
       i = 0
       if MODELISATION=='3D' :
# Sens du vecteur VECT_K1       
         v1x =array([Vpropa[0],Vpropa[1],Vpropa[2]])
         verif = dot(transpose(v1),v1x) 
         if verif < 0 : v1 = -v1
         VN = [v1]*Nbfond
         if DTAN_ORIG != None :
           VP[i] = array(DTAN_ORIG)
           VP[i] = VP[i]/sqrt(VP[i][0]**2+VP[i][1]**2+VP[i][2]**2)
           verif = dot(transpose(VP[i]),VN[0]) 
           if abs(verif) > 0.01:
             UTMESS('A','RUPTURE1_36')
         else :
           Pfon2 = array([Listfo[4*i],Listfo[4*i+1],Listfo[4*i+2]])
           Pfon3 = array([Listfo[4*(i+1)],Listfo[4*(i+1)+1],Listfo[4*(i+1)+2]])
           VT = (Pfon3 - Pfon2)/sqrt(dot(transpose(Pfon3-Pfon2),Pfon3-Pfon2))
           VP[0] = array(cross_product(VT,v1))
           VNi = array([Vpropa[3],Vpropa[4],Vpropa[5]])
           verif = dot(transpose(VP[i]),VNi) 
           if abs(verif) < 0.99:
             vv =[VNi[0],VNi[1],VNi[2],VN[i][0],VN[i][1],VN[i][2],]
             UTMESS('A','RUPTURE0_32',vali=[i],valr=vv)
         for i in range(1,Nbfond-1):
           Pfon1 = array([Listfo[4*(i-1)],Listfo[4*(i-1)+1],Listfo[4*(i-1)+2]])
           Pfon2 = array([Listfo[4*i],Listfo[4*i+1],Listfo[4*i+2]])
           Pfon3 = array([Listfo[4*(i+1)],Listfo[4*(i+1)+1],Listfo[4*(i+1)+2]])
           absf = sqrt(dot(transpose(Pfon1-Pfon2),Pfon1-Pfon2)) + absfon[i-1]
           absfon.append(absf)
           VT = (Pfon3 - Pfon2)/sqrt(dot(transpose(Pfon3-Pfon2),Pfon3-Pfon2))
           VT = VT+(Pfon2 - Pfon1)/sqrt(dot(transpose(Pfon2-Pfon1),Pfon2-Pfon1))
           VP[i] = array(cross_product(VT,v1)) 
           VP[i] = VP[i]/sqrt(dot(transpose(VP[i]),VP[i]))
           VNi = array([Vpropa[6*i],Vpropa[6*i+1],Vpropa[6*i+2]])
           verif = dot(transpose(VN[i]),VNi) 
           if abs(verif) < 0.99:
             vv =[VNi[0],VNi[1],VNi[2],VN[i][0],VN[i][1],VN[i][2],]
             UTMESS('A','RUPTURE0_32',vali=[i],valr=vv)
         i = Nbfond-1
         Pfon1 = array([Listfo[4*(i-1)],Listfo[4*(i-1)+1],Listfo[4*(i-1)+2]])
         Pfon2 = array([Listfo[4*i],Listfo[4*i+1],Listfo[4*i+2]])
         absf = sqrt(dot(transpose(Pfon1-Pfon2),Pfon1-Pfon2)) + absfon[i-1]
         absfon.append(absf)
         if DTAN_EXTR != None :
           VP[i] = array(DTAN_EXTR)
           VP[i] = VP[i]/sqrt(VP[i][0]**2+VP[i][1]**2+VP[i][2]**2)
           verif = dot(transpose(VP[i]),VN[i]) 
           if abs(verif) > 0.01:
             UTMESS('A','RUPTURE1_37')
         else :
           VT = (Pfon2 - Pfon1)/sqrt(dot(transpose(Pfon2-Pfon1),Pfon2-Pfon1))
           VP[i] = array(cross_product(VT,v1))
           VNi = array([Vpropa[6*i],Vpropa[6*i+1],Vpropa[6*i+2]])
           verif = dot(transpose(VN[i]),VNi) 
           if abs(verif) < 0.99 :
             vv =[VNi[0],VNi[1],VNi[2],VN[i][0],VN[i][1],VN[i][2],]
             UTMESS('A','RUPTURE0_32',vali=[i],valr=vv)
       else :  
         VT = array([0.,0.,1.])
         for i in range(0,Nbfond):
           VP[i] = array(cross_product(v1,VT))  
           VN[i] = v1
           VNi = array([Vpropa[0+4*i],Vpropa[1+4*i],0.])
           verif = dot(transpose(VN[i]),VNi) 
           if abs(verif) < 0.99 :
             vv =[VNi[0],VNi[1],VNi[2],VN[i][0],VN[i][1],VN[i][2],]
             UTMESS('A','RUPTURE0_32',vali=[i],valr=vv)
#Sens de la tangente   
     if MODELISATION=='3D' : i = Nbfond/2
     else : i = 0
     Po =  array([Listfo[4*i],Listfo[4*i+1],Listfo[4*i+2]])
     Porig = Po + ABSC_CURV_MAXI*VP[i]
     Pextr = Po - ABSC_CURV_MAXI*VP[i]
     __Tabg = MACR_LIGN_COUPE(RESULTAT=__RESX,NOM_CHAM='DEPL',
                   LIGN_COUPE=_F(NB_POINTS=3,COOR_ORIG=(Porig[0],Porig[1],Porig[2],),
                                  TYPE='SEGMENT',COOR_EXTR=(Pextr[0],Pextr[1],Pextr[2]),),);
     tmp=__Tabg.EXTR_TABLE()
     test = getattr(tmp,'H1X').values()
     if test==[None]*3 : 
        UTMESS('F','RUPTURE0_33')
     if test[0]!=None :
       sens = 1
     else :
       sens = -1
     DETRUIRE(CONCEPT=_F(NOM=__Tabg),INFO=1) 
# Extraction des sauts sur la fissure          
     TSaut = [None]*Nbfond    
     NB_NOEUD_COUPE = args['NB_NOEUD_COUPE']
     if NB_NOEUD_COUPE < 3 : 
       UTMESS('A','RUPTURE0_34')
       NB_NOEUD_COUPE = 5
     for i in range(Nbfond):
        Porig = array([Listfo[4*i],Listfo[4*i+1],Listfo[4*i+2]])
        if i==0 and DTAN_ORIG!=None : Pextr = Porig - ABSC_CURV_MAXI*VP[i]
        elif i==(Nbfond-1) and DTAN_EXTR!=None : Pextr = Porig - ABSC_CURV_MAXI*VP[i]
        else : Pextr = Porig + ABSC_CURV_MAXI*VP[i]*sens
        TSaut[i] = MACR_LIGN_COUPE(RESULTAT=__RESX,NOM_CHAM='DEPL',
                         LIGN_COUPE=_F(NB_POINTS=NB_NOEUD_COUPE,COOR_ORIG=(Porig[0],Porig[1],Porig[2],),
                                        TYPE='SEGMENT',COOR_EXTR=(Pextr[0],Pextr[1],Pextr[2]),),);

     Nbnofo = Nbfond
     if xcont[0] != 0 :  
       DETRUIRE(CONCEPT=_F(NOM=__MODLINE),INFO=1) 
       DETRUIRE(CONCEPT=_F(NOM=__RESX),INFO=1) 
   
     if INFO==2 :
        mcfact=[]
        mcfact.append(_F(PARA='PT_FOND',LISTE_I=range(Nbfond)))
        mcfact.append(_F(PARA='VN_X'        ,LISTE_R=[VN[i][0] for i in range(Nbfond)]))
        mcfact.append(_F(PARA='VN_Y'        ,LISTE_R=[VN[i][1] for i in range(Nbfond)]))
        mcfact.append(_F(PARA='VN_Z'        ,LISTE_R=[VN[i][2] for i in range(Nbfond)]))
        mcfact.append(_F(PARA='VP_X'        ,LISTE_R=[VP[i][0] for i in range(Nbfond)]))
        mcfact.append(_F(PARA='VP_Y'        ,LISTE_R=[VP[i][1] for i in range(Nbfond)]))
        mcfact.append(_F(PARA='VP_Z'        ,LISTE_R=[VP[i][2] for i in range(Nbfond)]))
        __resu2=CREA_TABLE(LISTE=mcfact,TITRE='             VECTEUR NORMAL A LA FISSURE    -    DIRECTION DE PROPAGATION')
        aster.affiche('MESSAGE',__resu2.EXTR_TABLE().__repr__())
        DETRUIRE(CONCEPT=_F(NOM=__resu2),INFO=1)
   
   else :
     Nbnofo = 1 
     
#   ----------Recuperation de la temperature au fond -------------  
   if Tempe3D :
      Rth = self.jdc.sds_dict[resuth]
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
      if FISSURE and INFO==2 :
            texte="\n\n--> TRAITEMENT DU POINT DU FOND DE FISSURE NUMERO %s"%(ino+1)
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
      elif FISSURE :
         tabsup = TSaut[ino].EXTR_TABLE()
         DETRUIRE(CONCEPT=_F(NOM=TSaut[ino]),INFO=1)
      else :
         tabsup=TABL_DEPL_SUP.EXTR_TABLE()
         veri_tab(tabsup,TABL_DEPL_SUP.nom,ndim)

#   ------------------------------------------------------------------
#                          TABLE 'DEPINF'
#   ------------------------------------------------------------------
      if SYME_CHAR=='SANS' and not FISSURE : 
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
               if TABL_DEPL_INF==None : UTMESS('F','RUPTURE0_35')
               veri_tab(tabinf,TABL_DEPL_INF.nom,ndim)
               Li = [string.ljust(Lnoinf[ino][i],8) for i in range(len(Lnoinf[ino]))]
               tabinf=tabinf.NOEUD==Li
         else :
            if TABL_DEPL_INF==None : UTMESS('F','RUPTURE0_35')
            tabinf=TABL_DEPL_INF.EXTR_TABLE()
            veri_tab(tabinf,TABL_DEPL_INF.nom,ndim)


#   ------------------------------------------------------------------
#               LES INSTANTS DE POST-TRAITEMENT
#   ------------------------------------------------------------------
      if 'INST' in tabsup.para : 
         l_inst=None
         l_inst_tab=tabsup['INST'].values()['INST']
         l_inst_tab=dict([(i,0) for i in l_inst_tab]).keys() #elimine les doublons
         l_inst_tab.sort()
         if LIST_ORDRE !=None or NUME_ORDRE !=None :
           l_ord_tab = tabsup['NUME_ORDRE'].values()['NUME_ORDRE']
           l_ord_tab.sort()
           l_ord_tab=dict([(i,0) for i in l_ord_tab]).keys() 
           d_ord_tab= [[l_ord_tab[i],l_inst_tab[i]] for i in range(0,len(l_ord_tab))]
           d_ord_tab= [(i[0],i[1]) for i in d_ord_tab]
           d_ord_tab = dict(d_ord_tab)
           if NUME_ORDRE !=None : 
             if type(NUME_ORDRE) not in EnumTypes : NUME_ORDRE=(NUME_ORDRE,)
             l_ord=list(NUME_ORDRE)
           elif LIST_ORDRE !=None : 
              l_ord = LIST_ORDRE.VALE.get() 
           l_inst = []
           for ord in l_ord :
             if ord in l_ord_tab : l_inst.append(d_ord_tab[ord])
             else :  
               UTMESS('F','RUPTURE0_37',vali=ord)
           PRECISION = 1.E-6
           CRITERE='ABSOLU'
         elif INST !=None or LIST_INST !=None :
            CRITERE = args['CRITERE']
            PRECISION = args['PRECISION']
            if  INST !=None : 
              if type(INST) not in EnumTypes : INST=(INST,)
              l_inst=list(INST)
            elif LIST_INST !=None : l_inst=LIST_INST.Valeurs()
            for inst in l_inst  :
               if CRITERE=='RELATIF' and inst!=0.: match=[x for x in l_inst_tab if abs((inst-x)/inst)<PRECISION]
               else                              : match=[x for x in l_inst_tab if abs(inst-x)<PRECISION]
               if len(match)==0 : 
                 UTMESS('F','RUPTURE0_38',valr=inst)
               if len(match)>=2 :
                 UTMESS('F','RUPTURE0_39',valr=inst)
         else :
            l_inst=l_inst_tab
            PRECISION = 1.E-6
            CRITERE='ABSOLU'
      else :
         l_inst  = [None,]
   
#   ------------------------------------------------------------------
#                         BOUCLE SUR LES INSTANTS
#   ------------------------------------------------------------------
      for iord in range(len(l_inst)) :
        inst=l_inst[iord]
        if INFO==2 and inst!=None:
            texte="#================================================================================\n"
            texte=texte+"==> INSTANT: %f"%inst
            aster.affiche('MESSAGE',texte)
        if inst!=None:
           if PRECISION == None : PRECISION = 1.E-6
           if CRITERE == None : CRITERE='ABSOLU'
           if inst==0. :
             tabsupi=tabsup.INST.__eq__(VALE=inst,CRITERE='ABSOLU',PRECISION=PRECISION)
             if SYME_CHAR=='SANS'and not FISSURE: tabinfi=tabinf.INST.__eq__(VALE=inst,CRITERE='ABSOLU',PRECISION=PRECISION)
           else :
             tabsupi=tabsup.INST.__eq__(VALE=inst,CRITERE=CRITERE,PRECISION=PRECISION)
             if SYME_CHAR=='SANS' and not FISSURE: tabinfi=tabinf.INST.__eq__(VALE=inst,CRITERE=CRITERE,PRECISION=PRECISION)
        else :
           tabsupi=tabsup
           if SYME_CHAR=='SANS' and not FISSURE : tabinfi=tabinf

#     --- LEVRE SUP :  "ABSC_CURV" CROISSANTES, < RMAX ET DEP ---
        abscs = getattr(tabsupi,'ABSC_CURV').values()
        if not FISSURE :
          if not FOND_FISS :
            refs=copy.copy(abscs)
            refs.sort()
            if refs!=abscs :
               mctabl='TABL_DEPL_INF' 
               UTMESS('F','RUPTURE0_40',valk=mctabl)
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
        if SYME_CHAR=='SANS' and not FISSURE : 
          absci = getattr(tabinfi,'ABSC_CURV').values()
          if not FOND_FISS :
            refi=copy.copy(absci)
            refi.sort()
            if refi!=absci :
                mctabl='TABL_DEPL_SUP' 
                UTMESS('F','RUPTURE0_40',valk=mctabl)
            refic=[x for x in refi if x<rmprec]
            nbvali=len(refic)
          else :
            nbvali=len(absci)
          if nbvali!=nbval :
             if FOND_FISS : 
                UTMESS('A+','RUPTURE0_42')
                UTMESS('A','RUPTURE0_43',valk=Lnofon[i],vali=[len(refsc),len(refic)])
             else:
                UTMESS('A','RUPTURE0_42')
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
               if d>precn : UTMESS('F','RUPTURE0_44')
          
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

#     --- CAS FISSURE X-FEM ---
        if  FISSURE : 
           H1 = getattr(tabsupi,'H1X').values()
           nbval = len(H1)
           if H1[-1]==None : 
             UTMESS('F','RUPTURE0_33')
           H1 = complete(H1)
           E1 = getattr(tabsupi,'E1X').values()
           E1 = complete(E1)
           dxs = 2*(H1 + sqrt(abscs)*E1)
           H1 = getattr(tabsupi,'H1Y').values()
           E1 = getattr(tabsupi,'E1Y').values()
           H1 = complete(H1)
           E1 = complete(E1)
           dys = 2*(H1 + sqrt(abscs)*E1)
           H1 = getattr(tabsupi,'H1Z').values()
           E1 = getattr(tabsupi,'E1Z').values()
           H1 = complete(H1)
           E1 = complete(E1)
           dzs = 2*(H1 + sqrt(abscs)*E1)
           abscs=array(abscs[:nbval])

#     --- TESTS NOMBRE DE NOEUDS---
        if nbval<3 :
           UTMESS('F+','RUPTURE0_46')
           if FOND_FISS :
               UTMESS('F+','RUPTURE0_47',valk=Lnofon[ino])
           UTMESS('F','RUPTURE0_25')
           
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
        if FISSURE :
           v2 = VP[ino]
           v1 = VN[ino]
        elif SYME_CHAR=='SANS' :
           vo =  array([( coxs[-1]+coxi[-1] )/2.,( coys[-1]+coyi[-1] )/2.,( cozs[-1]+cozi[-1] )/2.])
           ve =  array([( coxs[0 ]+coxi[0 ] )/2.,( coys[0 ]+coyi[0 ] )/2.,( cozs[0 ]+cozi[0 ] )/2.])
           v2 =  ve-vo
        else :
           vo = array([ coxs[-1], coys[-1], cozs[-1]])
           ve = array([ coxs[0], coys[0], cozs[0]])
           v2 =  ve-vo
        if not FISSURE :  v1 =  array(VECT_K1)
        v2 =  v2/sqrt(v2[0]**2+v2[1]**2+v2[2]**2)
        v1p = sum(v2*v1)
        if SYME_CHAR=='SANS' : v1  = v1-v1p*v2
        else : v2  = v2-v1p*v1 
        v1  = v1/sqrt(v1[0]**2+v1[1]**2+v1[2]**2)
        v2 =  v2/sqrt(v2[0]**2+v2[1]**2+v2[2]**2)
        v3  = array([v1[1]*v2[2]-v2[1]*v1[2],v1[2]*v2[0]-v2[2]*v1[0],v1[0]*v2[1]-v2[0]*v1[1]])
        pgl  = asarray([v1,v2,v3])
        dpls = asarray([dxs,dys,dzs])
        dpls = matrixmultiply(pgl,dpls)
        if SYME_CHAR!='SANS' and abs(dpls[0][0]) > 1.e-10 :
          UTMESS('A','RUPTURE0_49',valk=[Lnofon[ino],SYME_CHAR])
        if FISSURE :
           saut=dpls
        elif SYME_CHAR=='SANS' :
           dpli = asarray([dxi,dyi,dzi])
           dpli = matrixmultiply(pgl,dpli)
           saut=(dpls-dpli)
        else :
           dpli = [multiply(dpls[0],-1.),dpls[1],dpls[2]]
           saut=(dpls-dpli)
        if INFO==2 :
          mcfact=[]
          mcfact.append(_F(PARA='ABSC_CURV'  ,LISTE_R=abscs.tolist() ))
          if not FISSURE :
            mcfact.append(_F(PARA='DEPL_SUP_1',LISTE_R=dpls[0].tolist() ))
            mcfact.append(_F(PARA='DEPL_INF_1',LISTE_R=dpli[0].tolist() ))
          mcfact.append(_F(PARA='SAUT_1'    ,LISTE_R=saut[0].tolist() ))
          if not FISSURE :
            mcfact.append(_F(PARA='DEPL_SUP_2',LISTE_R=dpls[1].tolist() ))
            mcfact.append(_F(PARA='DEPL_INF_2',LISTE_R=dpli[1].tolist() ))
          mcfact.append(_F(PARA='SAUT_2'    ,LISTE_R=saut[1].tolist() ))
          if ndim==3 :
            if not FISSURE :
              mcfact.append(_F(PARA='DEPL_SUP_3',LISTE_R=dpls[2].tolist() ))
              mcfact.append(_F(PARA='DEPL_INF_3',LISTE_R=dpli[2].tolist() ))
            mcfact.append(_F(PARA='SAUT_3'    ,LISTE_R=saut[2].tolist() ))
          __resu0=CREA_TABLE(LISTE=mcfact,TITRE='--> SAUTS')
          aster.affiche('MESSAGE',__resu0.EXTR_TABLE().__repr__())
          DETRUIRE(CONCEPT=_F(NOM=__resu0),INFO=1)
#     ------------------------------------------------------------------
#                           CALCUL DES K1, K2, K3
#     ------------------------------------------------------------------
        isig=sign(transpose(resize(saut[:,-1],(nbval-1,3))))
        isig=sign(isig+0.001)
        saut=saut*array([[coefd]*nbval,[coefd]*nbval,[coefd3]*nbval])
        saut=saut**2
        ksig = isig[:,1]
        ksig = array([ksig,ksig])
        ksig = transpose(ksig)
        kgsig=resize(ksig,(1,6))[0]
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
        if FOND_FISS and MODELISATION=='3D': 
          mcfact.append(_F(PARA='NOEUD_FOND',LISTE_K=[Lnofon[ino],]*3))
          mcfact.append(_F(PARA='ABSC_CURV',LISTE_R=[dicoF[Lnofon[ino]]]*3))
        if FISSURE and MODELISATION=='3D': 
          mcfact.append(_F(PARA='PT_FOND',LISTE_I=[ino+1,]*3))
          mcfact.append(_F(PARA='ABSC_CURV',LISTE_R=[absfon[ino],]*3))
        if FISSURE  and MODELISATION!='3D' and Nbfond!=1 :
          mcfact.append(_F(PARA='PT_FOND',LISTE_I=[ino+1,]*3))
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
           if FOND_FISS and MODELISATION=='3D' : npara.append('NOEUD_FOND')
           tabout=CALC_TABLE(reuse=tabout,TABLE=tabout,TITRE = titre,
                              ACTION=_F(OPERATION = 'COMB',NOM_PARA=npara,TABLE=__tabi,))

# Tri de la table
   if len(l_inst)!=1 and MODELISATION=='3D':
      tabout=CALC_TABLE(reuse=tabout,TABLE=tabout,
                      ACTION=_F(OPERATION = 'TRI',NOM_PARA=('INST','ABSC_CURV','METHODE'),ORDRE='CROISSANT'))

   return ier
 
