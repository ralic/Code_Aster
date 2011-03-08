#@ MODIF propa_fiss_ops Macro  DATE 08/03/2011   AUTEUR MASSIN P.MASSIN 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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

from math import atan, atan2, cos, sin, log

import numpy as NP
from Accas import _F
from types import ListType, TupleType


def InterpolationLineaire(x0, points) :
   """
       Interpolation Lineaire de x0 sur la fonction discretisee yi=points(xi) i=1,..,n
   """
   # x0     = Une abscisse        (1 colonne, 1 ligne)
   # points = Tableau de n points (2 colonnes, n lignes)
   # on suppose qu'il existe au moins 2 points, 
   # et que les points sont classes selon les abscisses croissantes

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
   # s0     = abscisse curviligne du point considere      (0 < s0 > 1)
   # Coorfo = Coordonnees du fond (extrait de la sd fiss_xfem)
   # xyz = Coordonnees du point

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


def dadN(C,N,M,DK,R) :
# Calcul de la vitesse de propagation
   v = C/((1-R)**N)*abs(DK)**M
   return v
   
def betaf(k1,k2) :
  if k2 == 0:
     beta = 0.
  else :
     beta = 2*atan(0.25*(k1/k2-abs(k2)/k2*NP.sqrt((k1/k2)**2+8)))
  return beta

def recup_Elas(LOI):
      from SD.sd_mater     import sd_compor1
      if LOI == None :
         UTMESS('F','RUPTURE1_50') 
      dLoi=LOI[0].cree_dict_valeurs(LOI[0].mc_liste)
      mat = dLoi['MATER']
      matph = mat.NOMRC.get()  
      phenom=None
      for cmpt in matph :
         if cmpt[:4]=='ELAS' :
            phenom=cmpt
            break
      if phenom==None : UTMESS('F','RUPTURE0_5')
      compor = sd_compor1('%-8s.%s' % (mat.nom, phenom))
      valk = [s.strip() for s in compor.VALK.get()]
      valr = compor.VALR.get()
      dicmat=dict(zip(valk,valr))
      if dicmat.has_key('TEMP_DEF')  :
        nompar = ('TEMP',)
        valpar = (dicmat['TEMP_DEF'],)
        UTMESS('A','XFEM2_85',valr=valpar)
        nomres=['E','NU']
        valres,codret = MATER.RCVALE('ELAS',nompar,valpar,nomres,'F')
        e = valres[0]
        nu = valres[1]
      else :
        e  = dicmat['E']
        nu = dicmat['NU'] 
      return e,nu,dLoi

def nom_points_fonds(n_taille):
   """
   Construction des noms des points en fond de fissure
   """
   alphabet            = ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z'];
   if n_taille <= 26:
       return alphabet[:n_taille]
   else:
       tab_alphabet        = alphabet  
       taille_tab_alphabet = int(log(n_taille,26))
       for l_let1 in range(1,taille_tab_alphabet):
               for l_let2 in range(26):
                   for l_let3 in range(26):
                       tab_alphabet =  tab_alphabet + [tab_alphabet[(l_let1-1)*26+l_let2]+alphabet[l_let3]]       
       reste1 = int(n_taille-len(tab_alphabet))/26
       for l_let2 in range(reste1):
                  for l_let3 in range(26):
                      tab_alphabet =  tab_alphabet + [tab_alphabet[(taille_tab_alphabet-1)*26+l_let2]+alphabet[l_let3]]
       reste2 = int(n_taille-len(tab_alphabet))
       for l_let3 in range(reste2):
                   tab_alphabet =  tab_alphabet + [tab_alphabet[(taille_tab_alphabet-1)*26+reste1]+alphabet[l_let3]]
       return tab_alphabet  
    
#def propa_fiss_ops(self,METHODE_PROPA,TEST_MAIL,INFO,**args):
def propa_fiss_ops(self,METHODE_PROPA,INFO,**args):
  """
  Macro PROPA_FISS
  Propagation de fissure pour les modeles X-FEM : propagation par la methode de HAMILTON 
  ou par projection sur un maillage
  """

  import aster
  import string
  from Accas import _F
  from Utilitai.Utmess     import  UTMESS
  from Utilitai.partition import MAIL_PY

  EnumTypes = (ListType, TupleType)
  
  macro = 'PROPA_FISS'
  ier=0
#------------------------------------------------------------------
  # On importe les definitions des commandes a utiliser dans la macro
  ASSE_MAILLAGE         =self.get_cmd('ASSE_MAILLAGE'  )
  LIRE_MAILLAGE    =self.get_cmd('LIRE_MAILLAGE'  )
  DEFI_FICHIER = self.get_cmd('DEFI_FICHIER'  )
  DEFI_GROUP = self.get_cmd('DEFI_GROUP'  )
  CREA_TABLE    =self.get_cmd('CREA_TABLE'  )
  CALC_TABLE    =self.get_cmd('CALC_TABLE'  )
  PROPA_XFEM = self.get_cmd('PROPA_XFEM'  )
  DEFI_FISS_XFEM = self.get_cmd('DEFI_FISS_XFEM'  )
  MODI_MODELE_XFEM = self.get_cmd('MODI_MODELE_XFEM'  )
  # La macro compte pour 1 dans la numerotation des commandes
  self.set_icmd(1)

#------------------------------------------------------------------
# CAS 1 : METHODE_PROPA = 'SIMPLEXE' OU 'UPWIND'
#

  if (METHODE_PROPA == 'SIMPLEXE') or (METHODE_PROPA == 'UPWIND'):

    TEST_MAIL=args['TEST_MAIL']
    
    if (TEST_MAIL == 'NON' ) :
      LOI= args['LOI_PROPA']
      e,nu,dLoi = recup_Elas(LOI)
      
# Construction catalogue PROPA_XFEM
      dLoix = {}
      dLoix['LOI'] = 'PARIS'
      dLoix['E'] = e
      dLoix['NU'] = nu
      dLoix['C'] = dLoi['C']
      dLoix['M'] = dLoi['M']
      dLoix['N'] = dLoi['N']
     
    # Retreive all the parameters of PROPA_FISS
    mcsimp = {}
    mcsimp['MODELE'] =  args['MODELE']
    mcsimp['RAYON'] =  args['RAYON']
    mcsimp['DA_MAX'] =  args['DA_MAX']
    mcsimp['TEST_MAIL']=TEST_MAIL
    mcsimp['ZONE_MAJ']=args['ZONE_MAJ']
    if mcsimp['ZONE_MAJ'] == 'TORE' :
       if args['RAYON_TORE']!=None :
          mcsimp['RAYON_TORE']=args['RAYON_TORE']
    Fissures = args['FISSURE']

#   Build the list for the PROPA_XFEM operateur
    Table = []
    GrilleAux = []
    FissAct = []
    FissNou = []
    NbPointFond = []
    
    for Fiss in Fissures :
        FissAct.append(Fiss['FISS_ACTUELLE'])
        FissNou.append(Fiss['FISS_PROPAGEE'])
        if TEST_MAIL == 'NON':
            Table.append(Fiss['TABLE'])
            if Fiss['NB_POINT_FOND']!=None :
               if isinstance(Fiss['NB_POINT_FOND'],int) :
                  NbPointFond.append(Fiss['NB_POINT_FOND'])
               else :
                  for nbptfo in range(0,len(Fiss['NB_POINT_FOND'])) :
                      NbPointFond.append(Fiss['NB_POINT_FOND'][nbptfo])
            else :
               NbPointFond.append(-1)
        
    mcsimp['LISTE_FISS'] = FissAct
    
    if TEST_MAIL == 'NON':
       mcsimp['TABLE'] = Table
       mcsimp['NB_POINT_FOND'] = NbPointFond
       mcsimp['LOI_PROPA'      ] =dLoix
       
       COMP_LINE = args['COMP_LINE']
       if  COMP_LINE !=None : 
           dcomp=COMP_LINE[0].cree_dict_valeurs(COMP_LINE[0].mc_liste)
           mcsimp  ['COMP_LINE'      ] =dcomp

    if TEST_MAIL == 'NON' :
#      Ok. It's time for propagation! Let's call PROPA_XFEM for each
#      propagating crack.
       for NumFiss in range(0,len(FissAct)) :
           mcsimp['FISS_PROP'] = FissAct[NumFiss]
           self.DeclareOut('nomfiss',FissNou[NumFiss])
           nomfiss = PROPA_XFEM(METHODE=METHODE_PROPA,INFO=INFO,**mcsimp )
           
    else :
#      Ok. I should make several crack propagation and check for the
#      distance between each propagated front and the corresponding one
#      at the beginning of the propagation.
       UTMESS('A','XFEM2_60')
       StepTot = args['ITERATIONS']
       __Fis = [None]*(StepTot*len(FissAct))
       __Mod = [None]*StepTot
       mcsimp['TOLERANCE'] = args['TOLERANCE']
       fiss = FissAct[0]
       import aster
       iret,ibid,mod_fiss = aster.dismoi('F','NOM_MODELE',fiss.nom,'FISS_XFEM')
       mod_fiss=mod_fiss.strip()
       MOD_FISS = self.get_concept(mod_fiss)

       for NumStep in range(0,StepTot) :
         
           aster.affiche('MESSAGE',' ------------------------')
           texte=' TEST_MAIL - ITERATION %d'%(NumStep+1)
           aster.affiche('MESSAGE',texte)
           aster.affiche('MESSAGE',' ------------------------')
           
           ListeFiss = []
           mcsimp['DISTANCE'] = args['DA_MAX']*(NumStep+1)

           for NumFiss in range(0,len(FissAct)) :
              if NumStep==0 :
                 mcsimp['FISS_PROP'] = FissAct[NumFiss]
              else :
                 mcsimp['MODELE'] = __Mod[NumStep-1]
                 mcsimp['FISS_PROP'] = __Fis[(NumStep-1)*len(FissAct)+NumFiss]
              mcsimp['FISS_INITIALE'] = FissAct[NumFiss]
              if NumStep==StepTot-1 :
                 self.DeclareOut('nomfiss',FissNou[NumFiss])
                 nomfiss = PROPA_XFEM(METHODE=METHODE_PROPA,INFO=INFO,**mcsimp )
              else:
                 __Fis[NumFiss+NumStep*len(FissAct)] = PROPA_XFEM(METHODE=METHODE_PROPA,INFO=INFO,**mcsimp )
                 ListeFiss.append(__Fis[NumFiss+NumStep*len(FissAct)])
              
           if NumStep<StepTot-1 :
              aster.affiche('MESSAGE',' ------------------------')
              aster.affiche('MESSAGE',' CREATION DU MODELE FISSURE TEMPORAIRE')
              aster.affiche('MESSAGE',' ')
              __Mod[NumStep] = MODI_MODELE_XFEM(MODELE_IN=MOD_FISS,FISSURE=(ListeFiss))
              mcsimp['LISTE_FISS'] = ListeFiss
              aster.affiche('MESSAGE',' ')
              aster.affiche('MESSAGE',' ------------------------')
              aster.affiche('MESSAGE',' ')


#------------------------------------------------------------------
# CAS 2 : METHODE_PROPA = 'MAILLAGE'
#
  
  if METHODE_PROPA == 'MAILLAGE' :
    Fissures =  args['FISSURE']

    LOI_PROPA = args['LOI_PROPA']
    if LOI_PROPA != None :
      coef_paris =  LOI_PROPA['M']
      coef_M =  LOI_PROPA['M']
      coef_C =  LOI_PROPA['C']
      coef_N =  LOI_PROPA['N']
      YOUNG,NU,dLoi=recup_Elas(LOI_PROPA)
    it = args['ITERATION']
    Damax =  args['DA_MAX']
    COMP_LINE = args['COMP_LINE']

    Nbfissure=len(Fissures)
    mm = [None]*Nbfissure
    __MMX = [None]*Nbfissure
    BETA = [None]*Nbfissure
    DKeq = [None]*Nbfissure
    R = [None]*Nbfissure
    RmM = [None]*Nbfissure
    VMAX = 0.
    
    numfis=0
    print '-------------------------------------------'
    print 'NOMBRE DE FISSURES A TRAITER : ',Nbfissure
    for Fiss in Fissures :
      print 'FISSURE ',numfis+1,'  : ',Fiss['FISS_ACTUELLE'].get_name()
      numfis=numfis+1
    print '-------------------------------------------'
    
    numfis=0
    DKmax = 0
# PREMIERE BOUCLE SUR LES FISSURES : DK, DKmax    
    for Fiss in Fissures :
      fiss0 =    Fiss['FISS_ACTUELLE']
      MAIL_FISS1 =  Fiss['MAIL_ACTUEL']
      dime= MAIL_FISS1.DIME.get()
      dime = dime[5]
    
#    Verification qu on a bien un fond unique
      Fissmult = fiss0.FONDMULT.get()
      Nbfiss = len(Fissmult)/2
      if Nbfiss >1 :
         UTMESS('F','RUPTURE1_48',vali=Nbfiss)

# Recuperation des K et calcul de DeltaK
      SIF = Fiss['TABLE']
      nbinst = 1
      __tabp = SIF.EXTR_TABLE()
      if ('K1' not in __tabp.para) or ('G' not in __tabp.para):
         UTMESS('F','RUPTURE1_44')

      __tab1 = __tabp.values()
      
      if 'INST' in __tabp.para : 
        l_inst_tab=__tabp['INST'].values()['INST']
        l_inst_tab=dict([(i,0) for i in l_inst_tab]).keys()
        nbinst = len(l_inst_tab)
      nbptfon = len(__tab1['K1']) / nbinst
      RmM[numfis] = [None]*nbptfon
      DKeq[numfis] = [None]*nbptfon
      BETA[numfis] = [None]*nbptfon
      
# Lorsque le calcul porte sur plusieurs instants 
      if nbinst > 1 :
        for k in range(nbptfon) :
          if (dime == 2) : __tmp = __tabp
          if (dime == 3) :
              if __tabp.PT_FOND :
                  __tmp = __tabp.PT_FOND==(k+1)
                  indice_k = k
              else:
                  __tmp = __tabp.NUM_PT==(k+1)
                  indice_k = 0
          if ('ABSC_CURV' in __tmp.values()):
              abscisse_curv_courante = __tmp.values()['ABSC_CURV'][indice_k]
          else:
              abscisse_curv_courante = 0.
          ddkeq = NP.sqrt(YOUNG)*(NP.sqrt(max(__tmp.values()['G'])) 
                       - NP.sqrt(min(__tmp.values()['G']))) 
          rminmax = NP.sqrt(min(__tmp.values()['G'])) / NP.sqrt(max(__tmp.values()['G']))
          DKeq[numfis][k] = [abscisse_curv_courante, ddkeq ]
          RmM[numfis][k]  =   [abscisse_curv_courante, rminmax ]
          if ('BETA' in __tmp.values()):
               dbeta = max(__tmp.values()['BETA'])-min(__tmp.values()['BETA'])
               if dbeta > (5./180.*3.1415) :
                   UTMESS('F','XFEM2_72')
               BETA[numfis][k] = [abscisse_curv_courante, __tmp.values()['BETA'][0] ]
          else:
               if (dime == 2) :
                  k1 = __tmp.values()['K1'][k]
                  k2 = __tmp.values()['K2'][k]
                  BETA[numfis][k]=[0., betaf(k1,k2)]
               else:
                  k1 = __tmp.values()['K1']
                  k2 = __tmp.values()['K2']
                  betat = [0.]*nbinst
                  for jt in range(nbinst) :
                      betat[jt] = betaf(k1[jt],k2[jt])
# ANGLE BETA NE DOIT PAS TROP VARIER ENTRE LES PAS DE TEMPS
                  dbeta = max(betat) - min(betat) 
                  if dbeta > (5./180.*3.1415) :
                     UTMESS('F','XFEM2_72')
          VMAX0 = dadN(coef_C,coef_N,coef_M,DKeq[numfis][k][1],RmM[numfis][k][1]) 
          VMAX = max(VMAX,VMAX0 )
# Lorsque le calcul porte un seul instant 
      else :
        if COMP_LINE == None :
             UTMESS('A','XFEM2_76')
             CMIN = 0.
             CMAX = 1.
        else :
           CMIN = COMP_LINE['COEF_MULT_MINI']
           CMAX = COMP_LINE['COEF_MULT_MAXI']
        if (min(__tab1['G']) < 0.) :
           UTMESS('F','RUPTURE1_46')

        for k in range(nbptfon) :
          if (dime == 3) :
              if __tabp.PT_FOND :
                  indice_k = k
          else:
                  indice_k = 0
          if ('ABSC_CURV' in __tabp.para) :
              abscisse_curv_courante = __tab1['ABSC_CURV'][k]
          else:
              abscisse_curv_courante = 0.
          DKeq[numfis][k] =   [abscisse_curv_courante, NP.sqrt(YOUNG)*NP.sqrt(__tab1['G'][k]) ]
          RmM[numfis][k]  =   [abscisse_curv_courante, CMIN/CMAX ]
          if ('BETA' in __tab1.values()):
               BETA[numfis][k] = [abscisse_curv_courante, __tab1['BETA'][0] ]
          else:
              k1 = __tab1['K1'][indice_k]
              k2 = __tab1['K2'][indice_k]
              BETA[numfis][indice_k]=[abscisse_curv_courante, betaf(k1,k2)]
          VMAX0 = dadN(coef_C,coef_N,coef_M,DKeq[numfis][k][1],RmM[numfis][k][1]) 
          VMAX = max(VMAX,VMAX0 )


      numfis = numfis + 1
      
# CALCUL DU NOMBRE DE CYCLES EQUIVALENTS      
    NBCYCL = Damax / VMAX  
    print 'AVANCE MAXIMALE DU FOND DE FISSURE',Damax
    print 'NOMBRE DE CYCLES DE FATIGUE',NBCYCL
    
# DEUXIEME BOUCLE SUR LES FISSURES : PROPAGATION    
    numfis = 0
    for Fiss in Fissures :
      fiss0 =    Fiss['FISS_ACTUELLE']
      print '-------------------------------------------'
      print 'TRAITEMENT DE LA FISSURE ',fiss0.get_name()
      print '-------------------------------------------'
      MAIL_FISS1 =  Fiss['MAIL_ACTUEL']
      MFOND = Fiss['GROUP_MA_FOND']
      MFISS = Fiss['GROUP_MA_FISS']
#------------------------------------------------------------------
# CAS 2a : MODELE 3D
#
      if dime == 3 :
        mm[numfis] = MAIL_PY()
        mm[numfis].FromAster(MAIL_FISS1)
  
# Recuperation des informations sur le maillage
        nbno = mm[numfis].dime_maillage[0]
        nbma = mm[numfis].dime_maillage[2]
        collgrma = mm[numfis].gma
        gmafon = MFOND+str('_')+str(it-1)
  
# Recuperation de la liste des noeuds du fond
        connex = mm[numfis].co
        linomma  = list(mm[numfis].correspondance_mailles)
        groupma = mm[numfis].gma
        lmafo = groupma[gmafon]
        lisnofo = []
        for i in range(len(lmafo)) :
          ma_i = linomma[lmafo[i]]
          no_i = connex[lmafo[i]]
          if i == 0 :
            lisnofo.append(no_i[0])
            lisnofo.append(no_i[1])
          else :
            if lisnofo[i] != no_i[0] :
              UTMESS('F','RUPTURE1_51')
            lisnofo.append(no_i[1]) 
  
        nbnofo = len(lisnofo)
  
# Correction de la position des noeuds (equirepartition)
        Coorfo = fiss0.FONDFISS.get()
        absmax = Coorfo[-1]
        abscf = [0.]*nbnofo
        for i in range(nbnofo) :
          abscf[i] = i * absmax / (nbnofo-1)
          xyzi = InterpolFondFiss(abscf[i], Coorfo)
          mm[numfis].cn[nbno-nbnofo+i][0] = xyzi[0]
          mm[numfis].cn[nbno-nbnofo+i][1] = xyzi[1]
          mm[numfis].cn[nbno-nbnofo+i][2] = xyzi[2]
  
# Maillage apres correction
        coord    = mm[numfis].cn
        linomno  = list(mm[numfis].correspondance_noeuds)
        linomno = map(string.rstrip,linomno)
        l_coorf =  [[linomno[i],coord[i]] for i in range(0,nbno)]
        d_coorf = dict(l_coorf)     
      
# Boucle sur le fond : coordonnees du point propage            
        Basefo = fiss0.BASEFOND.get()
        Listfo = fiss0.FONDFISS.get()
        Vorig = Fiss['DTAN_ORIG']
        Vextr = Fiss['DTAN_EXTR']
        if (Damax ==None) :
           DKmax = 1
        if (coef_C ==None) :
           coef_C = Damax
        ALPHABET = nom_points_fonds(nbnofo)
        for ifond in range(nbnofo) :
           Xf =  d_coorf['NX%s%i' %(ALPHABET[ifond],it)][0]   
           Yf =  d_coorf['NX%s%i' %(ALPHABET[ifond],it)][1]     
           Zf =  d_coorf['NX%s%i' %(ALPHABET[ifond],it)][2]  
            
           VPVNi = InterpolBaseFiss(abscf[ifond],Basefo, Listfo)
           DKeqloc = InterpolationLineaire(abscf[ifond], DKeq[numfis])
           Rloc  = InterpolationLineaire(abscf[ifond], RmM[numfis])
           if DKeqloc<=0 :
             UTMESS('F','RUPTURE1_49')
   
# Tangentes aux extremites     
           if (ifond == 0) and (Vorig != None) :
             VPVNi[3] = Vorig[0]
             VPVNi[4] = Vorig[1]
             VPVNi[5] = Vorig[2]
           if (ifond == nbnofo-1) and (Vextr != None) :
             VPVNi[3] = Vextr[0]
             VPVNi[4] = Vextr[1]
             VPVNi[5] = Vextr[2]
           
           beta = InterpolationLineaire(abscf[ifond], BETA[numfis])
           Vloc = NBCYCL*dadN(coef_C,coef_N,coef_M,DKeqloc,Rloc)
           Xf2 = Xf + (VPVNi[3]*cos(beta)+VPVNi[0]*sin(beta))*Vloc
           Yf2 = Yf + (VPVNi[4]*cos(beta)+VPVNi[1]*sin(beta))*Vloc
           Zf2 = Zf + (VPVNi[5]*cos(beta)+VPVNi[2]*sin(beta))*Vloc
           
           LesNoeudsEnPlus = NP.array([[Xf2,Yf2,Zf2]])
           if ifond ==0 :
              Pini = (Xf2,Yf2,Zf2)
              vectorie = (VPVNi[0],VPVNi[1],VPVNi[2],)
           NomNoeudsEnPlus =     ['NX%s%i' %(ALPHABET[ifond],it+1)]
           mm[numfis].cn = NP.concatenate((mm[numfis].cn,LesNoeudsEnPlus))
           mm[numfis].correspondance_noeuds = tuple( list(mm[numfis].correspondance_noeuds) + NomNoeudsEnPlus )
    
  # Ajout Maille levre (quad4)      
        nbnotot = len(mm[numfis].correspondance_noeuds)
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
           NoeudsMailles.append( NP.array([i1,i2,i3,i4]))
  
        typ_maille = mm[numfis].dic['QUAD4']
        NbMailleAjoute = nbnofo-1
        mm[numfis].tm = NP.concatenate((mm[numfis].tm,NP.array([typ_maille]*NbMailleAjoute)))
        mm[numfis].correspondance_mailles += tuple(NomMaillesEnPlus)
        mm[numfis].co += NoeudsMailles
        #XXX utilise resize/arange... (MC)
        fsi = mm[numfis].gma['%s_%i' %(MFISS,it-1)]
        for ifond in range(nbnofo-1) :
          fsi = NP.concatenate((fsi,NP.array([nbma+ifond])))
        mm[numfis].gma['%s_%i' %(MFISS,it)] = fsi.astype(int)
       
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
           NoeudsMailles.append( NP.array([i3,i4]))
  
        typ_maille = mm[numfis].dic['SEG2']
        NbMailleAjoute = nbnofo-1
        mm[numfis].tm = NP.concatenate((mm[numfis].tm,NP.array([typ_maille]*NbMailleAjoute)))
        mm[numfis].correspondance_mailles += tuple(NomMaillesEnPlus)
        mm[numfis].co += NoeudsMailles
        mm[numfis].gma['%s_%i' %(MFOND,it)] = NP.arange(nbma+nbnofo-1, nbma+2*(nbnofo-1))
       
#------------------------------------------------------------------
# CAS 2b : MODELE 2D
#
      if dime == 2 :
        mm[numfis] = MAIL_PY()
        FISS_A = '%s_%i' %(MFISS,(it-1))
        DEFI_GROUP(reuse =MAIL_FISS1,
                 MAILLAGE=MAIL_FISS1,
                 CREA_GROUP_NO=_F(OPTION='NOEUD_ORDO',
                                  NOM='Nds_Plan',
                                  GROUP_MA=FISS_A,),INFO=2);
        DEFI_GROUP(reuse =MAIL_FISS1,
                 MAILLAGE=MAIL_FISS1,
                 DETR_GROUP_MA=_F(NOM='A',),
                 CREA_GROUP_MA=_F(OPTION='APPUI',
                                  NOM='A',
                                  TYPE_APPUI='TOUT',
                                  GROUP_NO='Nds_Plan',),INFO=2);
        DEFI_GROUP(reuse =MAIL_FISS1,
                 MAILLAGE=MAIL_FISS1,
                 DETR_GROUP_NO=_F(NOM='Nds_Plan',),);

        mm[numfis].FromAster(MAIL_FISS1)
        
        (nno,ndim) = mm[numfis].cn.shape
  
  # Recuperation des informations sur le maillage
        nbno = mm[numfis].dime_maillage[0]
        nbma = mm[numfis].dime_maillage[2]
        coord    = mm[numfis].cn
        linomno  = list(mm[numfis].correspondance_noeuds)
        linomno = map(string.rstrip,linomno)
        l_coorf =  [[linomno[i],coord[i]] for i in range(0,nbno)]
        d_coorf = dict(l_coorf) 
        
  # Coordonnees du point propage       
        Xf =  d_coorf['NXA%i' %(it)][0]    
        Yf =  d_coorf['NXA%i' %(it)][1]
  
        VPVNi = fiss0.BASEFOND.get()
        V = NBCYCL*dadN(coef_C,coef_N,coef_M,DKeq[numfis][0][1],RmM[numfis][0][1])
        beta = BETA[numfis][0][1]
        Xf2 = Xf +V*(VPVNi[2]*cos(beta)+VPVNi[0]*sin(beta))
        Yf2 = Yf + V*(VPVNi[3]*cos(beta)+VPVNi[1]*sin(beta))
        
        LesNoeudsEnPlus = NP.array([[Xf2,Yf2]])
        NomNoeudsEnPlus =     ['NXA%i' %(it+1)]
        mm[numfis].cn = NP.concatenate((mm[numfis].cn,LesNoeudsEnPlus))
        mm[numfis].correspondance_noeuds = tuple(linomno + NomNoeudsEnPlus )
        ALPHABET = nom_points_fonds(1)
        
  # Ajout Maille levre (SEG2)
        NomMaillesEnPlus =     ['MX%s%i' %(ALPHABET[0], it+1)]
        num_maille = [ nbma + 1 ]
        num_maille.append( nbma + 1 )
        NoeudsMailles = [NP.array([nbno-1,nbno])]
        typ_maille = mm[numfis].dic['SEG2']
        NbMailleAjoute = 1
        mm[numfis].tm = NP.concatenate((mm[numfis].tm,NP.array([typ_maille]*NbMailleAjoute)))
        mm[numfis].correspondance_mailles += tuple(NomMaillesEnPlus)
        mm[numfis].co += NoeudsMailles
        fsi = mm[numfis].gma['%s_%i' %(MFISS,it-1)]
        fsi = NP.concatenate((fsi,NP.array([nbma])))
        mm[numfis].gma['%s_%i' %(MFISS,it)] = fsi.astype(int)
 
# Ajout Maille fond (POI1)
        NomMaillesEnPlus =     ['MF%s%i' %(ALPHABET[0], it+1)]
        num_maille = [ nbma + 2 ]
        NoeudsMailles = [NP.array([nbno])]
        typ_maille = mm[numfis].dic['POI1']
        mm[numfis].tm = NP.concatenate((mm[numfis].tm,NP.array([typ_maille]*1)))
        mm[numfis].correspondance_mailles += tuple(NomMaillesEnPlus)
        mm[numfis].co += NoeudsMailles
        mm[numfis].gma['%s_%i' %(MFOND,it)] = NP.array([nbma+1], dtype=int)
# Fin du 2D      

      if INFO==2 :
        texte="Maillage produit par l operateur PROPA_FISS"
        aster.affiche('MESSAGE',texte)
        print mm[numfis]
             
# Sauvegarde maillage xfem 
      MAIL_FISS2 = Fiss['MAIL_PROPAGE']
      if MAIL_FISS2 != None : self.DeclareOut('ma_xfem2',MAIL_FISS2)
      
      unit = mm[numfis].ToAster()
      DEFI_FICHIER(UNITE=unit, ACTION="LIBERER")
      ma_xfem2=LIRE_MAILLAGE(UNITE=unit);

      if numfis == 0 :
        __MMX[0]=LIRE_MAILLAGE(UNITE=unit);
      else:
        __MMX[numfis]=ASSE_MAILLAGE(MAILLAGE_1 = __MMX[numfis-1],
                      MAILLAGE_2 = ma_xfem2,
                      OPERATION='SUPERPOSE')
      
      numfis = numfis+1
  
# Sauvegarde maillage concatene
    MAIL_TOTAL = args['MAIL_TOTAL']
    if MAIL_TOTAL != None : self.DeclareOut('ma_tot',MAIL_TOTAL)
    MAIL_STRUC = args['MAIL_STRUC']
    ma_tot = ASSE_MAILLAGE(MAILLAGE_1 = MAIL_STRUC,
                      MAILLAGE_2 = __MMX[Nbfissure-1],
                      OPERATION='SUPERPOSE',)
#------------------------------------------------------------------
# CAS 3 : METHODE_PROPA = 'INITIALISATION'
#
  if METHODE_PROPA == 'INITIALISATION' :
    form = args['FORM_FISS']
    MFOND = args['GROUP_MA_FOND']
    MFISS = args['GROUP_MA_FISS']
    
# 3-a : demi-droite    
    if form == 'DEMI_DROITE' :
      PF = args['PFON']
      DTAN = args['DTAN']
      PI = NP.array([[PF[0]-DTAN[0],PF[1]-DTAN[1]],])

      ndim = 2
      mm = MAIL_PY()
      mm.__init__()
     
# Ajout des noeuds 
      LesNoeudsEnPlus = NP.concatenate((PI,NP.array([PF[0:2]])))
      NomNoeudsEnPlus =     ['NXA0','NXA1']
      mm.cn = LesNoeudsEnPlus
      mm.correspondance_noeuds = tuple( NomNoeudsEnPlus )
      ALPHABET = nom_points_fonds(1)

# Ajout Maille levre (SEG2)
      it = 1
      nbma = 0
      nbno = 0
      NomMaillesEnPlus =     ['MX%s%i' %(ALPHABET[0], it)]
      num_maille = [ nbma + 1 ]
      num_maille.append( nbma + 1 )
      NoeudsMailles = [NP.array([nbno,nbno+1])]
      typ_maille = mm.dic['SEG2']
      NbMailleAjoute = 1
      mm.tm = NP.concatenate((mm.tm,NP.array([typ_maille]*NbMailleAjoute)))
      mm.correspondance_mailles += tuple(NomMaillesEnPlus)
      mm.co += NoeudsMailles
      mm.gma['%s_0' %(MFISS)] = NP.array([nbma], dtype=int)

# Ajout Maille fond (POI1)
      NomMaillesEnPlus =     ['MF%s%i' %(ALPHABET[0], it)]
      num_maille = [ nbma + 2 ]
      NoeudsMailles = [NP.array([nbno+1])]
      typ_maille = mm.dic['POI1']
      mm.tm = NP.concatenate((mm.tm,NP.array([typ_maille]*1)))
      mm.correspondance_mailles += tuple(NomMaillesEnPlus)
      mm.co += NoeudsMailles
      mm.gma['%s_0' %(MFOND)] = NP.array([nbma+1], dtype=int)


# 3-b : demi-plan    
    if form == 'DEMI_PLAN' :
      P0 = args['POINT_ORIG']
      P1 = args['POINT_EXTR']
      dpropa = args['DTAN']
      nbpt = args['NB_POINT_FOND']
      ALPHABET = nom_points_fonds(nbpt)
      Q0 = NP.array([[P0[0]-dpropa[0],P0[1]-dpropa[1],P0[2]-dpropa[2]]])
      
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
        LesNoeudsEnPlus = NP.array([[xx[i],yy[i],zz[i]]])
        NomNoeudsEnPlus =     ['NX%s0' %(ALPHABET[i]) ]
        mm.cn = NP.concatenate((mm.cn,LesNoeudsEnPlus))
        mm.correspondance_noeuds = tuple(list(mm.correspondance_noeuds)  +NomNoeudsEnPlus )
      LesNoeudsEnPlus = NP.array([P0])
      NomNoeudsEnPlus =     ['NXA1']
      mm.cn = NP.concatenate((mm.cn,LesNoeudsEnPlus))
      mm.correspondance_noeuds = tuple(list(mm.correspondance_noeuds)  + NomNoeudsEnPlus )
      for i in range(1,nbpt) :
        LesNoeudsEnPlus = NP.array([[x[i],y[i],z[i]]])
        NomNoeudsEnPlus =     ['NX%s1' %(ALPHABET[i]) ]
        mm.cn = NP.concatenate((mm.cn,LesNoeudsEnPlus))
        mm.correspondance_noeuds = tuple(list(mm.correspondance_noeuds)  +NomNoeudsEnPlus )

# Ajout Maille levre (quad4)      
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
         NoeudsMailles.append( NP.array([i1,i2,i4,i3]))

      typ_maille = mm.dic['QUAD4']
      NbMailleAjoute = nbpt-1
      mm.tm = NP.concatenate((mm.tm,NP.array([typ_maille]*NbMailleAjoute)))
      mm.correspondance_mailles += tuple(NomMaillesEnPlus)
      mm.co += NoeudsMailles
      mm.gma['%s_0' %(MFISS) ] = NP.arange(nbpt-1)
    
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
         NoeudsMailles.append( NP.array([i3,i4]))

      typ_maille = mm.dic['SEG2']
      NbMailleAjoute = nbpt-1
      mm.tm = NP.concatenate((mm.tm,NP.array([typ_maille]*NbMailleAjoute)))
      mm.correspondance_mailles += tuple(NomMaillesEnPlus)
      mm.co += NoeudsMailles
      mm.gma['%s_0' %(MFOND)] = NP.arange(nbpt-1, 2*(nbpt-1))
        
# 3-c : ellipse    
    if form == 'ELLIPSE' :
      P0 = args['CENTRE']
      alpha0 = args['ANGLE_ORIG']
      alpha1 = args['ANGLE_EXTR']
      vect_x = args['VECT_X']
      vect_y = args['VECT_Y']
      gdax = args['DEMI_GRAND_AXE']
      ptax = args['DEMI_PETIT_AXE']
      normale = NP.cross(vect_x,vect_y)
      verif = NP.dot(vect_x,vect_y)
      if abs(verif) > 0.01:
          UTMESS('F','RUPTURE1_52')
      nbpt = args['NB_POINT_FOND']
      ALPHABET = nom_points_fonds(nbpt)

      mm = MAIL_PY()
      mm.__init__()      
      LesNoeudsEnPlus = NP.array([[P0[0],P0[1],P0[2]]])
      NomNoeudsEnPlus =     ['NXA0']
      mm.cn = LesNoeudsEnPlus
      mm.correspondance_noeuds = tuple( NomNoeudsEnPlus )
      
# Coordonnees des noeuds  
      matr = NP.asarray([vect_x,vect_y,normale])
      matr2 = NP.transpose(matr)
      alpha0 = alpha0*NP.pi/180. 
      alpha1 = alpha1*NP.pi/180. 
      for i in range(nbpt) :
         alphai = alpha0 + i*(alpha1-alpha0) / (nbpt-1)
         coor_r1 = NP.asarray([gdax*cos(alphai), ptax*sin(alphai), 0])
         coor_r0 = NP.dot(matr2,coor_r1) + P0
         LesNoeudsEnPlus = NP.array([[coor_r0[0],coor_r0[1],coor_r0[2]]])
         NomNoeudsEnPlus =     ['NX%s1' %(ALPHABET[i]) ]
         mm.cn = NP.concatenate((mm.cn,LesNoeudsEnPlus))
         mm.correspondance_noeuds = tuple(list(mm.correspondance_noeuds)  +NomNoeudsEnPlus )

# Ajout Maille levre (TRIA3)      
      NomMaillesEnPlus = []
      num_maille = []
      NoeudsMailles = []
      typ_maille = mm.dic['TRIA3']
      for ifond in range(nbpt-1) :
         NomMaillesEnPlus.append( 'MX%s1' %(ALPHABET[ifond]) )
         num_maille.append( [ ifond +1 ] )
         num_maille.append( ifond + 1 )
         i1 = 0
         i2 = ifond + 1
         i3 = ifond + 2
         NoeudsMailles.append( NP.array([i1,i2,i3]))
      NbMailleAjoute = nbpt-1
      mm.tm = NP.concatenate((mm.tm,NP.array([typ_maille]*NbMailleAjoute)))
      mm.correspondance_mailles += tuple(NomMaillesEnPlus)
      mm.co += NoeudsMailles
      mm.gma['%s_0' %(MFISS) ] = NP.arange(NbMailleAjoute)
      
      

# Ajout Maille fond (SEG2)      
      NomMaillesEnPlus = []
      num_maille = []
      NoeudsMailles = []
      typ_maille = mm.dic['SEG2']
      for ifond in range(nbpt-1) :
         NomMaillesEnPlus.append( 'MF%s1' %(ALPHABET[ifond]) )
         num_maille.append( [ ifond +nbpt ] )
         num_maille.append( ifond + nbpt )
         i3 = ifond + 1
         i4 = ifond + 2
         NoeudsMailles.append( NP.array([i3,i4]))

      NbMailleAjoute = nbpt-1
      mm.tm = NP.concatenate((mm.tm,NP.array([typ_maille]*NbMailleAjoute)))
      mm.correspondance_mailles += tuple(NomMaillesEnPlus)
      mm.co += NoeudsMailles
      mm.gma['%s_0' %(MFOND)] = NP.arange(nbpt-1, 2*(nbpt-1))
    
    
    if INFO==2 :
      texte="Maillage produit par l operateur PROPA_FISS"
      aster.affiche('MESSAGE',texte)
      print mm
           
# Sauvegarde (maillage xfem et maillage concatene)
    MAIL_FISS2 = args['MAIL_FISS']
    if MAIL_FISS2 != None : self.DeclareOut('ma_xfem2',MAIL_FISS2)
    unit = mm.ToAster()
    DEFI_FICHIER(UNITE=unit, ACTION="LIBERER")
    self.DeclareOut('ma_xfem2',MAIL_FISS2)
    ma_xfem2=LIRE_MAILLAGE(UNITE=unit);

    MAIL_TOTAL = args['MAIL_TOTAL']
    if MAIL_TOTAL != None : self.DeclareOut('ma_tot',MAIL_TOTAL)
    MAIL_STRUC = args['MAIL_STRUC']
    ma_tot = ASSE_MAILLAGE(MAILLAGE_1 = MAIL_STRUC,
                      MAILLAGE_2 = ma_xfem2,
                      OPERATION='SUPERPOSE')
                      
  return                    
