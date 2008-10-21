#@ MODIF propa_fiss_ops Macro  DATE 20/10/2008   AUTEUR GALENNE E.GALENNE 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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


def InterpolationLineaire(x0, points) :
   """
       Interpolation Lineaire de x0 sur la fonction discrétisée yi=points(xi) i=1,..,n
   """
   # x0     = Une abscisse        (1 colonne, 1 ligne)
   # points = Tableau de n points (2 colonnes, n lignes)
   # on suppose qu'il existe au moins 2 points, 
   # et que les points sont classés selon les abscisses croissantes

   n = len(points)
   if ( x0 < points[0][0] )  :
     y0 =  points[0][1]
     return y0
   if ( x0 > points[n-1][0] ) :
     y0 =  points[n-1][1]
     return y0
   i = 1
   while x0 > points[i][0]:
      i = i+1
   y0 = (x0-points[i-1][0]) * (points[i][1]-points[i-1][1]) / (points[i][0]-points[i-1][0]) + points[i-1][1]
   return y0

def InterpolFondFiss(s0, Coorfo) :
   """
       Interpolation des points du fond de fissure
   """
   # s0     = abscisse curviligne du point considéré      (0 < s0 > 1)
   # Coorfo = Coordonnées du fond (extrait de la sd fiss_xfem)
   # xyz = Coordonnées du point

   n = len(Coorfo) / 4
   if ( s0 < Coorfo[3] )  :
     xyz =  [Coorfo[0],Coorfo[1],Coorfo[2]]
     return xyz
   if ( s0 > Coorfo[-1]  ) :
     xyz =  [Coorfo[-4],Coorfo[-3],Coorfo[-2]]
     return xyz
   i = 1
   while s0 > Coorfo[4*i+3]:
      i = i+1
   xyz = [0.]*3
   xyz[0] = (s0-Coorfo[4*(i-1)+3]) * (Coorfo[4*i+0]-Coorfo[4*(i-1)+0]) / (Coorfo[4*i+3]-Coorfo[4*(i-1)+3]) + Coorfo[4*(i-1)+0]
   xyz[1] = (s0-Coorfo[4*(i-1)+3]) * (Coorfo[4*i+1]-Coorfo[4*(i-1)+1]) / (Coorfo[4*i+3]-Coorfo[4*(i-1)+3]) + Coorfo[4*(i-1)+1]
   xyz[2] = (s0-Coorfo[4*(i-1)+3]) * (Coorfo[4*i+2]-Coorfo[4*(i-1)+2]) / (Coorfo[4*i+3]-Coorfo[4*(i-1)+3]) + Coorfo[4*(i-1)+2]
   return xyz

def InterpolBaseFiss(s0, Basefo, Coorfo) :
# Interpolation de la base locale en fond de fissure
# s0     = abscisse curviligne du point considere     
# Basefo = base locale du fond (VNx,VNy,VNz,VPx,VPy,VPz)
# Coorfo = Coordonnees et abscisses du fond (extrait de la sd fiss_xfem)
# en sortie : VPVNi = base locale au point considere (6 coordonnes)
   n = len(Coorfo) / 4
   if ( s0 < Coorfo[3] )  :
     VPVNi =  Basefo[0:6]
     return VPVNi
   if ( s0 > Coorfo[-1]  ) :
     VPVNi = [Basefo[i] for i in range(-6,0)] 
     return VPVNi
   i = 1
   while s0 > Coorfo[4*i+3]:
      i = i+1
   VPVNi = [0.]*6
   for k in range(6) :
      VPVNi[k] = (s0-Coorfo[4*(i-1)+3]) * (Basefo[6*i+k]-Basefo[6*(i-1)+k]) / (Coorfo[4*i+3]-Coorfo[4*(i-1)+3]) + Basefo[6*(i-1)+k]
   return VPVNi


def propa_fiss_ops(self,METHODE_PROPA,INFO,**args):
  """
  Macro PROPA_FISS
  Propagation de fissure pour les modeles X-FEM : propagation par la methode de HAMILTON 
  ou par projection sur un maillage
  """
  import aster
  import string
  import copy
  from Accas import _F
  from Utilitai.Utmess     import  UTMESS
  from types import ListType, TupleType
  from Utilitai.Table      import Table, merge
  from Utilitai.partition import MAIL_PY
  import Numeric
  from Numeric import array,asarray,Float,concatenate,sqrt,sign,resize,dot,zeros
  from math import atan, atan2, cos, sin


  EnumTypes = (ListType, TupleType)
  
  macro = 'PROPA_FISS'
  ier=0
#------------------------------------------------------------------
  # On importe les definitions des commandes a utiliser dans la macro
  ASSE_MAILLAGE         =self.get_cmd('ASSE_MAILLAGE'  )
  LIRE_MAILLAGE    =self.get_cmd('LIRE_MAILLAGE'  )
  CREA_TABLE    =self.get_cmd('CREA_TABLE'  )
  CALC_TABLE    =self.get_cmd('CALC_TABLE'  )
  PROPA_XFEM = self.get_cmd('PROPA_XFEM'  )
  DEFI_FISS_XFEM = self.get_cmd('DEFI_FISS_XFEM'  )
  # La macro compte pour 1 dans la numerotation des commandes
  self.set_icmd(1)
  
#------------------------------------------------------------------
# CAS 1 : METHODE_PROPA = 'HAMILTON'
#
  if METHODE_PROPA == 'HAMILTON' :
    mod =  args['MODELE']
    tabk = args['TABLE']
    meth =  args['METHODE']
    ray =  args['RAYON']
    LOI_PROPA = args['LOI_PROPA']
    if LOI_PROPA != None :
      coefc =  LOI_PROPA['C']
      coefm =  LOI_PROPA['M']
    fiss =    args['FISSURE']
    self.DeclareOut('nomfiss',fiss)
    nomfiss = PROPA_XFEM(MODELE=mod,
                         TABLE=tabk,
                         METHODE=meth,
                         LOI_PROPA=_F(LOI='PARIS',
                                      C=coefc,
                                      M=coefm),
                         RAYON=ray,
                         INFO=INFO,)

#------------------------------------------------------------------
# CAS 2 : METHODE_PROPA = 'MAILLAGE'
#
  ALPHABET=['A','B','C','D','E','F','G','H','I','J','K','L','O','P','Q','R','S','T','U','V','W','X','Y','Z'];
  
  if METHODE_PROPA == 'MAILLAGE' :
    fiss =    args['FISSURE1']
    LOI_PROPA = args['LOI_PROPA']
    MA_XFEM1 =  args['MA_XFEM1']
    if LOI_PROPA != None :
      coef_paris =  LOI_PROPA['M']
      Damax = LOI_PROPA['DA_MAX']
      coef_C =  LOI_PROPA['C']
    it = args['ITERATION']
    typmod= MA_XFEM1.DIME.get()
    typmod = typmod[5]
    
#    Verification qu on a bien un fond unique
    Fissmult = fiss.FONDMULT.get()
    Nbfiss = len(Fissmult)/2
    if Nbfiss >1 :
       UTMESS('F','RUPTURE1_48',vali=Nbfiss)

# Recuperation des K et calcul de DeltaK
    Nmeth = args['METHODE_POSTK']
    SIF = args['TABLE']
    hypo = args['HYPOTHESE']
    nbinst = 1
# A- TABLEAU ISSSU DE POST_K1_K2_K3    
    if  (Nmeth != None) :
       __TABN = CALC_TABLE(TABLE=SIF,ACTION=_F(OPERATION='FILTRE',
                                           NOM_PARA='METHODE',VALE_I=Nmeth),);
       __tabp = __TABN.EXTR_TABLE()
       if ('K1_MAX' not in __tabp.para) or ('G_MAX' not in __tabp.para):
          UTMESS('F','RUPTURE1_44')
       __tab1 = __tabp.values()
       if 'INST' in __tabp.para : 
         l_inst_tab=__tabp['INST'].values()['INST']
         l_inst_tab=dict([(i,0) for i in l_inst_tab]).keys()
         nbinst = len(l_inst_tab)
       if nbinst > 1 :
          nbfis = len(__tab1['K1_MAX']) / nbinst
          DK1 = [None]*nbfis
          DKmax = 0.
          for k in range(nbfis) :
             DK1[k]=[0.]*2
             __tmp = __tabp.PT_FOND==(k+1)
             if (typmod == 3) : DK1[k][0]=__tmp.values()['ABSC_CURV'][0]
             DK1[k][1]=max(__tmp.values()['K1_MAX'])-min(__tmp.values()['K1_MAX'])
             DKmax = max(DKmax,DK1[k][1])
       else :
          nbfis = len(__tab1['K1_MAX'])
          if hypo == 'NON_PLAN' :
             BETA = [None]*nbfis
             if (min(__tab1['G_MAX']) < 0.) :
               UTMESS('F','RUPTURE1_46')
             DKmax = max(sqrt(__tab1['G_MAX']))
             BETA = [0.]*nbfis
             absc = [0.]*nbfis
             for i in range(nbfis) :
                k1 = __tab1['K1_MAX'][i]
                k2 = __tab1['K2_MAX'][i]
                if (typmod == 3) : absc[i]=__tab1['ABSC_CURV'][i]
                BETA[i] = [absc[i] , 2*atan(0.25*(k1/k2-abs(k2)/k2*sqrt((k1/k2)**2+8)))]
             DK1 = [[absc[i],sqrt(__tab1['G_MAX'][i])] for i in range(nbfis)]
          else :
             DKmax = max(__tab1['K1_MAX'])
             if (typmod == 3) :
               DK1 = [[__tab1['ABSC_CURV'][i],__tab1['K1_MAX'][i]] for i in range(nbfis)]
             else :
               DK1 = [[0.,__tab1['K1_MAX'][i]] for i in range(nbfis)]
             if (min(__tab1['K1_MAX']) < 0.) :
               UTMESS('F','RUPTURE1_49')
# B- TABLEAU ISSSU DE CALC_G (option CALC_K_G)
    else :
       __tabp = SIF.EXTR_TABLE()
       if (typmod == 3) and (('K1_LOCAL' not in __tabp.para) or ('G_LOCAL' not in __tabp.para) or ('BETA_LOCAL' not in __tabp.para)):
          UTMESS('F','RUPTURE1_45')
       if (typmod == 2) and (('K1' not in __tabp.para) or ('G' not in __tabp.para)) :
          UTMESS('F','RUPTURE1_45')
       __tab1= __tabp.values()
       if 'INST' in __tabp.para : 
         l_inst_tab=__tabp['INST'].values()['INST']
         l_inst_tab=dict([(i,0) for i in l_inst_tab]).keys()
         nbinst = len(l_inst_tab)
       if nbinst > 1 :
          nbfiss = 1
          if (typmod == 3) : nbfis = len(__tab1['K1_LOCAL']) / nbinst
          DK1 = [None]*nbfis
          DKmax = 0.
          for k in range(nbfis) :
             DK1[k]=[None]*2
             __tmp = __tabp.NUM_PT==(k+1)
             if (typmod == 3) : 
               DK1[k][0]=__tmp.values()['ABSC_CURV'][0]
               DK1[k][1]=max(__tmp.values()['K1_LOCAL'])-min(__tmp.values()['K1_LOCAL'])
             else :
               DK1[k][0]=0.
               DK1[k][1]=max(__tmp.values()['K1'])-min(__tmp.values()['K1'])
             DKmax = max(DKmax,DK1[k][1])
       elif typmod == 3 :
          nbfis = len(__tab1['K1_LOCAL'])
          if hypo == 'NON_PLAN' :
             if (min(__tab1['G_LOCAL']) < 0.) :
               UTMESS('F','RUPTURE1_46')
             DKmax = max(sqrt(__tab1['G_LOCAL']))
             DK1 = [[__tab1['ABSC_CURV'][i],sqrt(__tab1['G_LOCAL'][i])] for i in range(nbfis)]
             BETA = [[__tab1['ABSC_CURV'][i],__tab1['BETA_LOCAL'][i]] for i in range(nbfis)]
          else :
             DKmax = max(__tab1['K1_LOCAL'])
             DK1 = [[__tab1['ABSC_CURV'][i],__tab1['K1_LOCAL'][i]] for i in range(nbfis)]
             if (min(__tab1['K1_LOCAL']) < 0.) :
               UTMESS('F','RUPTURE1_49')
       else :
          nbfis = 1
          if hypo == 'NON_PLAN' :
             if (min(__tab1['G']) < 0.) :
               UTMESS('F','RUPTURE1_46')
             DKmax = max(sqrt(__tab1['G']))
             DK1 = [[0.,DKmax],]
             k1 = __tab1['K1'][0]
             k2 = __tab1['K2'][0]
             beta = 2*atan(0.25*(k1/k2-abs(k2)/k2*sqrt((k1/k2)**2+8)))
             BETA = [[0.,beta] ]
          else :
             DKmax = max(__tab1['K1'])
             DK1 = [[0.,DKmax ] ,]
             if (min(__tab1['K1']) < 0.) :
               UTMESS('F','RUPTURE1_49')

    if hypo == 'NON_PLAN'  and nbinst > 1 :
       UTMESS('F','RUPTURE1_47')

#------------------------------------------------------------------
# CAS 2a : MODELE 3D
#
    if typmod == 3 :
      mm = MAIL_PY()
      mm.FromAster(MA_XFEM1)

# Recuperation des informations sur le maillage
      nbno = mm.dime_maillage[0]
      nbma = mm.dime_maillage[2]
      collgrma = mm.gma
      nbnofo = len(collgrma['FOND_0'])+1

# Correction de la position des noeuds (equirepartition)
      Coorfo = fiss.FONDFISS.get()
      absmax = Coorfo[-1]
      abscf = [0.]*nbnofo
      for i in range(nbnofo) :
        abscf[i] = i * absmax / (nbnofo-1)
        xyzi = InterpolFondFiss(abscf[i], Coorfo)
        mm.cn[nbno-nbnofo+i][0] = xyzi[0]
        mm.cn[nbno-nbnofo+i][1] = xyzi[1]
        mm.cn[nbno-nbnofo+i][2] = xyzi[2]

# Maillage apres correction
      coord    = mm.cn
      linomno  = list(mm.correspondance_noeuds)
      linomno = map(string.rstrip,linomno)
      l_coorf =  [[linomno[i],coord[i]] for i in range(0,nbno)]
      d_coorf = dict(l_coorf)     
      
# Boucle sur le fond : coordonnees du point propage            
      Basefo = fiss.BASEFOND.get()
      Listfo = fiss.FONDFISS.get()
      Vorig = args['DTAN_ORIG']
      Vextr = args['DTAN_EXTR']
      if (Damax ==None) :
         DKmax = 1
      if (coef_C ==None) :
         coef_C = Damax
      for ifond in range(nbnofo) :
         Xf =  d_coorf['NX%s%i' %(ALPHABET[ifond],it)][0]   
         Yf =  d_coorf['NX%s%i' %(ALPHABET[ifond],it)][1]     
         Zf =  d_coorf['NX%s%i' %(ALPHABET[ifond],it)][2]  
          
         VPVNi = InterpolBaseFiss(abscf[ifond],Basefo, Listfo)
        
         k1 = InterpolationLineaire(abscf[ifond], DK1)
         if k1<=0 :
           UTMESS('F','RUPTURE1_49')

# Correction pour reduire le risque de maille aplatie (pilotage par Damax uniquement)
         if (Damax !=None) :
            Damin = Damax /10.
            if ((k1/DKmax)**coef_paris <= Damin )  :
               k1 = Damin**(1/coef_paris)*DKmax

# Tangentes aux extremites     
         if (ifond == 0) and (Vorig != None) :
           VPVNi[3] = Vorig[0]
           VPVNi[4] = Vorig[1]
           VPVNi[5] = Vorig[2]
         if (ifond == nbnofo-1) and (Vextr != None) :
           VPVNi[3] = Vextr[0]
           VPVNi[4] = Vextr[1]
           VPVNi[5] = Vextr[2]
         
         if hypo == 'NON_PLAN' :
            beta = InterpolationLineaire(abscf[ifond], BETA)
            Xf2 = Xf + coef_C*(VPVNi[3]*cos(beta)+VPVNi[0]*sin(beta))*(k1/DKmax)**coef_paris
            Yf2 = Yf + coef_C*(VPVNi[4]*cos(beta)+VPVNi[1]*sin(beta))*(k1/DKmax)**coef_paris
            Zf2 = Zf + coef_C*(VPVNi[5]*cos(beta)+VPVNi[2]*sin(beta))*(k1/DKmax)**coef_paris
         else :
            Xf2 = Xf + coef_C*VPVNi[3]*(k1/DKmax)**coef_paris
            Yf2 = Yf + coef_C*VPVNi[4]*(k1/DKmax)**coef_paris
            Zf2 = Zf + coef_C*VPVNi[5]*(k1/DKmax)**coef_paris
         
         LesNoeudsEnPlus = array([[Xf2,Yf2,Zf2]])
         if ifond ==0 :
            Pini = (Xf2,Yf2,Zf2)
            vectorie = (VPVNi[0],VPVNi[1],VPVNi[2],)
         NomNoeudsEnPlus =     ['NX%s%i' %(ALPHABET[ifond],it+1)]
         mm.cn = concatenate((mm.cn,LesNoeudsEnPlus))
         mm.correspondance_noeuds = tuple( list(mm.correspondance_noeuds) + NomNoeudsEnPlus )
  
# Ajout Maille lèvre (quad4)      
      nbnotot = len(mm.correspondance_noeuds)
      NomMaillesEnPlus = []
      num_maille = []
      NoeudsMailles = []
      for ifond in range(nbnofo-1) :
         NomMaillesEnPlus.append( 'MX%s%i' %(ALPHABET[ifond], it+1) )
         num_maille.append( [ nbma + ifond +1 ] )
         num_maille.append( nbma +ifond + 1 )
         i1 = nbnotot - 2*nbnofo  + ifond
         i2 = nbnotot - 2*nbnofo  + ifond +1
         i3 = nbnotot - nbnofo  + ifond +1
         i4 = nbnotot - nbnofo  + ifond 
         NoeudsMailles.append( array([i1,i2,i3,i4]))

      typ_maille = mm.dic['QUAD4']
      NbMailleAjoute = nbnofo-1
      mm.tm = concatenate((mm.tm,array([typ_maille]*NbMailleAjoute)))
      mm.correspondance_mailles += tuple(NomMaillesEnPlus)
      mm.co += NoeudsMailles
      fsi = mm.gma['FISS_%i' %(it-1)]
      for ifond in range(nbnofo-1) :
        fsi = concatenate((fsi,array([nbma+ifond])))
      mm.gma['FISS_%i' %it] = fsi
     
# Ajout Maille fond (SEG2)      
      NomMaillesEnPlus = []
      num_maille = []
      NoeudsMailles = []
      for ifond in range(nbnofo-1) :
         NomMaillesEnPlus.append( 'MF%s%i' %(ALPHABET[ifond], it+1) )
         num_maille.append( [ nbma + ifond +nbnofo ] )
         num_maille.append( nbma + ifond + nbnofo )
         i3 = nbnotot - nbnofo  + ifond 
         i4 = nbnotot - nbnofo  + ifond +1
         NoeudsMailles.append( array([i3,i4]))

      typ_maille = mm.dic['SEG2']
      NbMailleAjoute = nbnofo-1
      mm.tm = concatenate((mm.tm,array([typ_maille]*NbMailleAjoute)))
      mm.correspondance_mailles += tuple(NomMaillesEnPlus)
      mm.co += NoeudsMailles
      fsi = []
      for ifond in range(nbnofo-1) :
        fsi = concatenate((fsi,array([nbma+ifond+nbnofo-1])))
      mm.gma['FOND_%i' %it] = fsi
     
#------------------------------------------------------------------
# CAS 2b : MODELE 2D
#
    if typmod == 2 :
      mm = MAIL_PY()
      mm.FromAster(MA_XFEM1)
      
      (nno,ndim) = mm.cn.shape

# Recuperation des informations sur le maillage
      nbno = mm.dime_maillage[0]
      nbma = mm.dime_maillage[2]
      coord    = mm.cn
      linomno  = list(mm.correspondance_noeuds)
      linomno = map(string.rstrip,linomno)
      l_coorf =  [[linomno[i],coord[i]] for i in range(0,nbno)]
      d_coorf = dict(l_coorf) 
      
# Coordonnees du point propage       
      Xf =  d_coorf['NXA%i' %(it)][0]    
      Yf =  d_coorf['NXA%i' %(it)][1]
      if (Damax ==None) :
         DKmax = 1
      if (coef_C ==None) :
         coef_C = Damax

      VPVNi = fiss.BASEFOND.get()
      k1 = DK1[0][1]
      if hypo == 'NON_PLAN' :
         beta = BETA[0][1]
         Xf2 = Xf + coef_C*(VPVNi[2]*cos(beta)+VPVNi[0]*sin(beta))*(k1/DKmax)**coef_paris
         Yf2 = Yf + coef_C*(VPVNi[3]*cos(beta)+VPVNi[1]*sin(beta))*(k1/DKmax)**coef_paris
      else :
         Xf2 = Xf + coef_C*VPVNi[2]*(k1/DKmax)**coef_paris
         Yf2 = Yf + coef_C*VPVNi[3]*(k1/DKmax)**coef_paris
      
      LesNoeudsEnPlus = array([[Xf2,Yf2]])
      NomNoeudsEnPlus =     ['NXA%i' %(it+1)]
      mm.cn = concatenate((mm.cn,LesNoeudsEnPlus))
      mm.correspondance_noeuds = tuple(linomno + NomNoeudsEnPlus )
      
# Ajout Maille lèvre (SEG2)
      NomMaillesEnPlus =     ['MX%s%i' %(ALPHABET[0], it+1)]
      num_maille = [ nbma + 1 ]
      num_maille.append( nbma + 1 )
      NoeudsMailles = [array([nbno-1,nbno])]
      typ_maille = mm.dic['SEG2']
      NbMailleAjoute = 1
      mm.tm = concatenate((mm.tm,array([typ_maille]*NbMailleAjoute)))
      mm.correspondance_mailles += tuple(NomMaillesEnPlus)
      mm.co += NoeudsMailles
      fsi = mm.gma['FISS_%i' %(it-1)]
      fsi = concatenate((fsi,array([nbma])))
      mm.gma['FISS_%i' %it] = fsi

# Ajout Maille fond (POI1)
      NomMaillesEnPlus =     ['MF%s%i' %(ALPHABET[0], it+1)]
      num_maille = [ nbma + 2 ]
      NoeudsMailles = [array([nbno])]
      typ_maille = mm.dic['POI1']
      mm.tm = concatenate((mm.tm,array([typ_maille]*1)))
      mm.correspondance_mailles += tuple(NomMaillesEnPlus)
      mm.co += NoeudsMailles
      mm.gma['FOND_%i' %it] = array(nbma+1)

# Fin du 2D      
    if INFO==2 :
      texte="Maillage produit par l operateur PROPA_FISS"
      aster.affiche('MESSAGE',texte)
      print mm
           
# Sauvegarde (maillage xfem et maillage concatene)
    MA_XFEM2 = args['MA_XFEM2']
    if MA_XFEM2 != None : self.DeclareOut('ma_xfem2',MA_XFEM2)
    __MA = mm.ToAster(unite=39)
    self.DeclareOut('ma_xfem2',MA_XFEM2)
    ma_xfem2=LIRE_MAILLAGE(UNITE=39);

    MA_TOT2 = args['MA_TOT2']
    if MA_TOT2 != None : self.DeclareOut('ma_tot',MA_TOT2)
    MA_STRUC = args['MA_STRUC']
    ma_tot = ASSE_MAILLAGE(MAILLAGE_1 = MA_STRUC,
                      MAILLAGE_2 = ma_xfem2,
                      OPERATION='SUPERPOSE')

#------------------------------------------------------------------
# CAS 3 : METHODE_PROPA = 'INITIALISATION'
#
  if METHODE_PROPA == 'INITIALISATION' :
    form = args['FORM_FISS']
    
# 3-a : demi-droite    
    if form == 'DEMI_DROITE' :
      PF = args['PFON']
      DTAN = args['DTAN']
      PI = array([[PF[0]-DTAN[0],PF[1]-DTAN[1]],])

      ndim = 2
      mm = MAIL_PY()
      mm.__init__()
     
# Ajout des noeuds 
      LesNoeudsEnPlus = concatenate((PI,array([PF[0:2]])))
      NomNoeudsEnPlus =     ['NXA0','NXA1']
      mm.cn = LesNoeudsEnPlus
      mm.correspondance_noeuds = tuple( NomNoeudsEnPlus )

# Ajout Maille lèvre (SEG2)
      it = 1
      nbma = 0
      nbno = 0
      NomMaillesEnPlus =     ['MX%s%i' %(ALPHABET[0], it)]
      num_maille = [ nbma + 1 ]
      num_maille.append( nbma + 1 )
      NoeudsMailles = [array([nbno,nbno+1])]
      typ_maille = mm.dic['SEG2']
      NbMailleAjoute = 1
      mm.tm = concatenate((mm.tm,array([typ_maille]*NbMailleAjoute)))
      mm.correspondance_mailles += tuple(NomMaillesEnPlus)
      mm.co += NoeudsMailles
      mm.gma['FISS_0'] = array(nbma)

# Ajout Maille fond (POI1)
      NomMaillesEnPlus =     ['MF%s%i' %(ALPHABET[0], it)]
      num_maille = [ nbma + 2 ]
      NoeudsMailles = [array([nbno+1])]
      typ_maille = mm.dic['POI1']
      mm.tm = concatenate((mm.tm,array([typ_maille]*1)))
      mm.correspondance_mailles += tuple(NomMaillesEnPlus)
      mm.co += NoeudsMailles
      mm.gma['FOND_0'] = array(nbma+1)


# 3-b : demi-plan    
    if form == 'DEMI_PLAN' :
      P0 = args['POINT_ORIG']
      P1 = args['POINT_EXTR']
      dpropa = args['DTAN']
      nbpt = args['NB_POINT_FOND']
      Q0 = array([[P0[0]-dpropa[0],P0[1]-dpropa[1],P0[2]-dpropa[2]]])
      
      mm = MAIL_PY()
      mm.__init__()
      x=[None]*nbpt
      y=[None]*nbpt
      z=[None]*nbpt
      xx=[None]*nbpt
      yy=[None]*nbpt
      zz=[None]*nbpt
      LesNoeudsEnPlus = Q0
      NomNoeudsEnPlus =     ['NXA0']
      mm.cn = LesNoeudsEnPlus
      mm.correspondance_noeuds = tuple( NomNoeudsEnPlus )

      for i in range(1,nbpt) :
        x[i] = P0[0]+i*(P1[0]-P0[0])/(nbpt-1)
        y[i] = P0[1]+i*(P1[1]-P0[1])/(nbpt-1)
        z[i] = P0[2]+i*(P1[2]-P0[2])/(nbpt-1)
        xx[i] = x[i] - dpropa[0]
        yy[i] = y[i] - dpropa[1]
        zz[i] = z[i] - dpropa[2]
        LesNoeudsEnPlus = array([[xx[i],yy[i],zz[i]]])
        NomNoeudsEnPlus =     ['NX%s0' %(ALPHABET[i]) ]
        mm.cn = concatenate((mm.cn,LesNoeudsEnPlus))
        mm.correspondance_noeuds = tuple(list(mm.correspondance_noeuds)  +NomNoeudsEnPlus )
      LesNoeudsEnPlus = array([P0])
      NomNoeudsEnPlus =     ['NXA1']
      mm.cn = concatenate((mm.cn,LesNoeudsEnPlus))
      mm.correspondance_noeuds = tuple(list(mm.correspondance_noeuds)  + NomNoeudsEnPlus )
      for i in range(1,nbpt) :
        LesNoeudsEnPlus = array([[x[i],y[i],z[i]]])
        NomNoeudsEnPlus =     ['NX%s1' %(ALPHABET[i]) ]
        mm.cn = concatenate((mm.cn,LesNoeudsEnPlus))
        mm.correspondance_noeuds = tuple(list(mm.correspondance_noeuds)  +NomNoeudsEnPlus )

# Ajout Maille lèvre (quad4)      
      NomMaillesEnPlus = []
      num_maille = []
      NoeudsMailles = []
      for ifond in range(nbpt-1) :
         NomMaillesEnPlus.append( 'MX%s1' %(ALPHABET[ifond]) )
         num_maille.append( [ ifond +1 ] )
         num_maille.append( ifond + 1 )
         i1 = ifond
         i2 = ifond+1
         i3 = nbpt+ifond
         i4 = nbpt+ifond+1
         NoeudsMailles.append( array([i1,i2,i4,i3]))

      typ_maille = mm.dic['QUAD4']
      NbMailleAjoute = nbpt-1
      mm.tm = concatenate((mm.tm,array([typ_maille]*NbMailleAjoute)))
      mm.correspondance_mailles += tuple(NomMaillesEnPlus)
      mm.co += NoeudsMailles
      fsi =[]
      for ifond in range(nbpt-1) :
        fsi = concatenate((fsi,array([ifond])))
      mm.gma['FISS_0' ] = fsi
    
# Ajout Maille fond (SEG2)      
      NomMaillesEnPlus = []
      num_maille = []
      NoeudsMailles = []
      for ifond in range(nbpt-1) :
         NomMaillesEnPlus.append( 'MF%s1' %(ALPHABET[ifond]) )
         num_maille.append( [ ifond +nbpt ] )
         num_maille.append( ifond + nbpt )
         i3 = nbpt+ifond
         i4 = nbpt+ifond+1
         NoeudsMailles.append( array([i3,i4]))

      typ_maille = mm.dic['SEG2']
      NbMailleAjoute = nbpt-1
      mm.tm = concatenate((mm.tm,array([typ_maille]*NbMailleAjoute)))
      mm.correspondance_mailles += tuple(NomMaillesEnPlus)
      mm.co += NoeudsMailles
      fsi = []
      for ifond in range(nbpt-1) :
        fsi = concatenate((fsi,array([ifond+nbpt-1])))
      mm.gma['FOND_0'] = fsi
        
    
    if INFO==2 :
      texte="Maillage produit par l operateur PROPA_FISS"
      aster.affiche('MESSAGE',texte)
      print mm
           
# Sauvegarde (maillage xfem et maillage concatene)
    MA_XFEM2 = args['MA_XFEM2']
    if MA_XFEM2 != None : self.DeclareOut('ma_xfem2',MA_XFEM2)
    __MA = mm.ToAster(unite=39)
    self.DeclareOut('ma_xfem2',MA_XFEM2)
    ma_xfem2=LIRE_MAILLAGE(UNITE=39);

    MA_TOT2 = args['MA_TOT2']
    if MA_TOT2 != None : self.DeclareOut('ma_tot',MA_TOT2)
    MA_STRUC = args['MA_STRUC']
    ma_tot = ASSE_MAILLAGE(MAILLAGE_1 = MA_STRUC,
                      MAILLAGE_2 = ma_xfem2,
                      OPERATION='SUPERPOSE')
                      
  return                    
