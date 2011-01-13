#@ MODIF post_endo_fiss_ops Macro  DATE 13/01/2011   AUTEUR PELLET J.PELLET 

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
# RESPONSABLE BOTTONI M.BOTTONI
# ---------------------------------------------------------------------------
#                  POST_ENDO_FISS
# PROCEDURE PYTHON DU RECHERCHE DU TRAJET DE FISSURATION SUR UN
#  CHAMP SCALAIRE 2D



# ----------------------------
#
# FONCTIONS UTILES
#
# ----------------------------

# FIND IN A VECTOR :
#   Fonction qui trouve tous les valeurs dans "Vect"
#    egal au scalaire "a".
#    On retourne tous les indices des valeurs cherchees
#    Vect doit etre un vecteur unidimensionnel
def vfind(Vect,a) :
  import numpy as NP
  Vect0 = Vect-a
  lst0  = NP.nonzero(Vect0)[0]
  NP.put(Vect0,lst0, NP.ones(lst0.shape))
  Vect0 = Vect0-1
  lst0 = NP.nonzero(Vect0)[0]
  return lst0

# DELETE ELEMENT IN A VECTOR :
#   Fonction qui sert a effacer des elements d'un vecteur Vect
#   (element array unidimmensionnel ou matrice avec la deuxieme dimension 1)
#   a: vecteur d'indices avec le meme cahracteristiques que Vect
def delEl(Vect,a) :
  import numpy as NP
  class LengthError(Exception):
    pass

  shapeV = Vect.shape
  if type(a) == list :
    a=NP.array(a)
  shapea = a.shape

  lenErr = True
  if len(shapeV)==1 :
    lenErr = False
  if len(shapea)==1 :
    lenErr = False
  if  len(shapea)==2 and (shapea[0]==1 or shapea[1]==1) :
    lenErr = False
  if lenErr :
    raise LengthError

  Vect = Vect.tolist()
  a    = NP.ravel(a)
  a    = NP.sort(a)

  for i in range(len(a)) :
    idx = a[i]
    try :
      del Vect[idx]
    except TypeError :
      raise TypeError
    a = a-1

  Vect = NP.array(Vect)
  return Vect


# RETURN A UNIQUE VECTOR
# Fonction qui elimine les doublons dans un vecteur Vect
#   Vect doit etre un vecteur unidimensionnel
def unique(Vect):
  i = 0
  while i < len(Vect) :
    num = Vect[i]
    idxDou = vfind(Vect, num)
    if len(idxDou)>1 :
      idxDou = idxDou[1:len(idxDou)]
      Vect = delEl(Vect,idxDou)
    i = i+1
  return Vect



# CREATE A 1D-MESH :
#  Cree un maillage SEG2 en 2D
#    Coorx  : liste des coordonnees x des noeuds
#    Coory  : liste des coordonnees y des noeuds
#    Connex : connectivites entre les noeuds
#             (liste de tuples d'entiers)
#

def crea_mail_lin(XcreteTot,YcreteTot,ConnTot,dime):
  resu  = 'TITRE\n'
  titre = 'Maillage lineaire'+'\n'
  resu  = resu + titre
  resu  = resu+'FINSF\n'

  # creation des noeuds
  resu  = resu+'COOR_'+str(dime)+'D\n'
  CoorX = XcreteTot[0]
  CoorY = YcreteTot[0]
  for i in range(1,len(XcreteTot)) :
    CoorX = CoorX + XcreteTot[i]
    CoorY = CoorY + YcreteTot[i]
  nbNoeu = len(CoorX)
  for i in range(nbNoeu):
    nbno = i+1
    x = CoorX[i]
    y = CoorY[i]
    noeud = '  N'+str(nbno)+'   '+str(x)+'    '+str(y)+'\n'
    resu  = resu + noeud
  resu = resu+'FINSF\n'

  # creation des mailles
  resu = resu+'SEG2\n'
  nbmailTot = 0
  nbNoTot   = 0
  for j in range(len(ConnTot)) :
    Connex = ConnTot[j]
    nbmail = len(Connex)
    for i in range(nbmail) :
      nbma   = i+1+nbmailTot
      ma     = Connex[i]
      maille = '  M'+str(nbma)+' N'+str(ma[0]+nbNoTot)+' N'+str(ma[1]+nbNoTot)+'\n'
      resu   = resu+maille
    nbmailTot = nbmailTot + len(Connex)
    nbNoTot   = nbNoTot   + len(XcreteTot[j])
  resu = resu+'FINSF\n'

  # creation des groupes de mailles "fissure"
  nbmailTot = 0
  for j in range(len(ConnTot)):
    resu = resu+'GROUP_MA\n'
    resu = resu+'FISSURE'+str(j+1)+'\n'
    Connex = ConnTot[j]
    nbmail = len(Connex)
    for i in range(nbmail):
      nbma = i+1+nbmailTot
      resu = resu +'  M'+str(nbma)+'\n'
    resu = resu+'\n'
    resu = resu+'FINSF\n'
    nbmailTot = nbmailTot + len(Connex)

  resu = resu+'FIN\n'
  return resu











# ------------------------------------------
#
# ROUTINE POUR LA RECHERCHE DE LA CRETE
#  POST ENDO FISS
#
# ------------------------------------------

def post_endo_fiss_ops(self,
                       TABLE,
                       NOM_CMP,
                       NOM_CHAM,
                       RECHERCHE,
                       **args) :

  from Macro.macr_lign_coupe_ops import crea_noeu_lig_coup
  from Macro.macr_lign_coupe_ops import crea_mail_lig_coup
  from Utilitai.Utmess     import  UTMESS, MasquerAlarme, RetablirAlarme
  from Utilitai.UniteAster import  UniteAster
  from Accas import _F
  from math import radians
  import os
  import numpy as NP


  ier = 0
  # La macro compte pour 1 dans la numerotation des commandes
  self.set_icmd(1)

  MasquerAlarme('CALCULEL5_48')

  # --------------------------------------------------
  # IMPORTATION COMMANDES ASTER
  #
  LIRE_MAILLAGE  = self.get_cmd('LIRE_MAILLAGE')
  AFFE_MODELE    = self.get_cmd('AFFE_MODELE')
  PROJ_CHAMP     = self.get_cmd('PROJ_CHAMP')
  CREA_TABLE     = self.get_cmd('CREA_TABLE')
  CREA_RESU      = self.get_cmd('CREA_RESU')
  CREA_CHAMP     = self.get_cmd('CREA_CHAMP')


  # --------------------------------------------------
  # DECLARATION SORTIES
  #
  self.DeclareOut('__MAIFI',self.sd)
  self.DeclareOut('__tabRes',TABLE)


  # --------------------------------------------------
  # RECUPERATION ENTREES
  #
  l_dRECHERCHE = []
  for recherche in RECHERCHE :
    dRECHERCHE = recherche.cree_dict_valeurs(recherche.mc_liste)
    for i in dRECHERCHE.keys():
      if dRECHERCHE[i]==None : del dRECHERCHE[i]
    l_dRECHERCHE.append(dRECHERCHE)

  # --------------------------------------------------
  # TEST SUR LE TYPE DE CHAMP
  #
  ltyP_cham = ['NOEU_DEPL_R','ELGA_EPSI_R','ELNO_EPSI_R','NOEU_SIEF_R','ELGA_VARI_R','ELNO_VARI_R','ELNO_VARI_R','NOEU_VAR2_R']
  lnoM_cham = ['DEPL','EPSI_ELGA','EPSI_ELNO_DEPL','EPSI_NOEU','VARI_ELGA','VARI_ELNO','VARI_ELNO_ELGA','VARI_NOEU_ELGA']

  if NOM_CHAM in lnoM_cham :
    Xtype_cham = ltyP_cham[lnoM_cham.index(NOM_CHAM)]
  else :
    UTMESS('F','POST0_35')


  # --------------------------------------------------
  # RECUPERATION ENTREES, MODELE ET MAILLAGE
  #
  motscles = {}

  if args['CHAM_GD'] != None :
    build    = 'champ'
    __YBARTO = args['CHAM_GD']
    __modtot = args['MODELE']
    #n_modele = (__modtot.nom).strip()
    inst = 1.
    motscles['INST'] = inst

  else :
    build    = 'resu'
    __RESUIN = args['RESULTAT']
    nomresu  = __RESUIN.nom
    dicResu  = __RESUIN.LIST_PARA()

    if args['NUME_ORDRE'] != None :
      inst = 1.
      nume_ordre             = args['NUME_ORDRE']
      motscles['NUME_ORDRE'] = nume_ordre
    else :
      inst             = args['INST']
      motscles['INST'] = inst
      dicVarAcc        = __RESUIN.LIST_VARI_ACCES()
      nume_ordre       = dicVarAcc['INST'].index(inst) + 1

    if args['MODELE'] != None :
      __modtot = args['MODELE']
    elif dicResu['MODELE'][0] is not None:
      lst_n_modele = dicResu['MODELE']
      n_modele     = lst_n_modele[nume_ordre-1]
      n_modele = n_modele.strip()
      __modtot = self.get_concept(n_modele)
    else :
      UTMESS('F','POST0_33')


  n_mail = __modtot.MODELE.LGRF.get()
  n_mail = n_mail[0].strip()
  __mail = self.get_concept(n_mail)
  Coortot  = __mail.COORDO.VALE.get()
  Xtot     = Coortot[0:len(Coortot):3]
  Ytot     = Coortot[1:len(Coortot):3]

  dime = __mail.DIME.get()[5]
  if dime != 2 :
    UTMESS('F','POST0_34')


  if build == 'resu' :
    __YBARTO = CREA_CHAMP(TYPE_CHAM = Xtype_cham,
                      OPERATION = 'EXTR',
                      RESULTAT  = __RESUIN,
                      NOM_CHAM  = NOM_CHAM,
                      **motscles)



  # --------------------------------------------------
  # BOUCLE SUR L'OCCURRENCE DU MOT-CLEF RECHERCHE
  #
  XcreteTot    = []
  YcreteTot    = []
  ConnTot      = []
  YbarcreteTot = []
  lstFissure   = []


  for idxRech in range(len(l_dRECHERCHE)) :

    dRECHERCHE = l_dRECHERCHE[idxRech]

    # ---------------------------------
    # Recuperation valeurs d'entrees
    #
    lort     = dRECHERCHE['LONG_ORTH']
    nbPoints = dRECHERCHE['NB_POINT']
    pas      = dRECHERCHE['PAS']
    lreg     = dRECHERCHE['LONG_REG']
    seuil    = dRECHERCHE['SEUIL']
    alpha    = dRECHERCHE['ANGL_MAX']
    seuil    = dRECHERCHE['SEUIL']

    if 'GROUP_MA' in dRECHERCHE.keys() :
      groupma = dRECHERCHE['GROUP_MA']

    # --------------------------------------------------
    # Construction du concept resultat de travail
    #
    if build == 'champ' :
      if 'GROUP_MA' in dRECHERCHE.keys() :
        __YBARNO = CREA_CHAMP(OPERATION = 'ASSE',
                              TYPE_CHAM  = Xtype_cham,
                              MODELE    = __modtot,
                              ASSE      = _F(CHAM_GD  = __YBARTO,
                                            GROUP_MA = groupma,
                                            NOM_CMP  = NOM_CMP,
                                            ),)
      else :
        __YBARNO = __YBARTO


    if build == 'resu' :
      if 'GROUP_MA' in dRECHERCHE.keys() :
        __YBARNO = CREA_CHAMP(OPERATION = 'ASSE',
                              TYPE_CHAM = Xtype_cham,
                              MODELE    = __modtot,
                              ASSE      = _F(CHAM_GD  = __YBARTO,
                                             GROUP_MA = groupma,
                                             NOM_CMP  = NOM_CMP,
                                            ),)
      else :
        __YBARNO = __YBARTO


    __resu   = CREA_RESU( OPERATION  = 'AFFE',
                          TYPE_RESU  = 'EVOL_NOLI',
                          NOM_CHAM   = NOM_CHAM,
                          AFFE = (_F( CHAM_GD  = __YBARNO,
                                      #MODELE  = __modtot,
                                      INST     = inst),),)


    # --------------------------------------------------------------
    # Recuperation dans Python des valeurs du champ et du maillage
    #
    Ybarno   = __YBARNO.EXTR_COMP(NOM_CMP,[],1)
    Ybar     = Ybarno.valeurs
    Noeybar  = Ybarno.noeud
    IdxNoeud = NP.array(Noeybar)-1
    Coorx    = NP.take(Xtot,IdxNoeud)
    Coory    = NP.take(Ytot,IdxNoeud)


    # --------------------------------------------------
    # Amorcage de la procedure de recherche de la crete
    #

    # Point ou la Ybar est maximale
    #
    idxmax  = NP.argmax(Ybar)
    xmax    = Coorx[idxmax]
    ymax    = Coory[idxmax]
    ybarmax = Ybar[idxmax]

    CoxAmm  = NP.array([xmax], float)
    CoyAmm  = NP.array([ymax], float)
    YbarAmm = NP.array([ybarmax], float)

    # Creation d'un circle autour du point de max
    #  et projection sur le circle
    #
    lignes = []
    groups = []
    arcs   = []
    arcs.append(([xmax+pas,ymax],[xmax,ymax],nbPoints,180.))
    arcs.append(([xmax-pas,ymax],[xmax,ymax],nbPoints,180.))
    resu_mail0,arcgma0,angles0,nbno0 = crea_mail_lig_coup(dime,lignes,groups,arcs)


    nomFichierSortie = os.path.join(os.getcwd(),'maillage.mail')
    fproc            = open(nomFichierSortie, 'w')
    fproc.write(resu_mail0)
    fproc.close()
    UL               = UniteAster()
    uniteMail        = UL.Libre(action = 'ASSOCIER', nom = nomFichierSortie)

    __MAI = LIRE_MAILLAGE(UNITE = uniteMail)
    UL.EtatInit(uniteMail)


    __MO = AFFE_MODELE(MAILLAGE = __MAI,
                        AFFE    = _F(TOUT          = 'OUI',
                                      PHENOMENE    = 'MECANIQUE',
                                      MODELISATION = 'BARRE'),)

    __YBARPR = PROJ_CHAMP(METHODE        = 'COLLOCATION',
                            RESULTAT     = __resu,
                            MODELE_1     = __modtot,
                            MODELE_2     = __MO,
                            DISTANCE_MAX = 0.,
                            TYPE_CHAM    = 'NOEU',
                            NOM_CHAM     = NOM_CHAM,
                            NUME_ORDRE   = 1, )

    __YBARCH = CREA_CHAMP(TYPE_CHAM    = Xtype_cham,
                          OPERATION    = 'EXTR',
                          NOM_CHAM     = NOM_CHAM,
                          RESULTAT     = __YBARPR,
                          NUME_ORDRE   = 1,)

    dx0 = __YBARCH.EXTR_COMP(NOM_CMP,[],1)

    # Nonvide : liste de noeud du profil orthogonal
    #   avec des valeurs associes
    # idxpred : connections entres les 2 demi-circle
    Nonvide  = NP.array(list(dx0.noeud))
    idxpred1 = vfind(Nonvide,2*nbPoints-1)
    idxpred2 = vfind(Nonvide,nbPoints)

    Ybarort  = dx0.valeurs
    Coor0    = __MAI.COORDO.VALE.get()
    Coorxort = NP.array(Coor0[0:len(Coor0):3] , float)
    Cooryort = NP.array(Coor0[1:len(Coor0):3] , float)

    # On elimine les noeuds sans valeurs associes
    Coorxort = NP.take(Coorxort,Nonvide-1)
    Cooryort = NP.take(Cooryort,Nonvide-1)
    Coorxort = delEl(Coorxort,idxpred1)
    Coorxort = delEl(Coorxort,idxpred2)
    Cooryort = delEl(Cooryort,idxpred1)
    Cooryort = delEl(Cooryort,idxpred2)
    Ybarort  = delEl(Ybarort,idxpred1)
    Ybarort  = delEl(Ybarort,idxpred2)

    # Regularisation sur le circle
    YbarReg = NP.zeros((len(Ybarort),), float)
    X1 = NP.concatenate((Coorxort[1:len(Coorxort)], NP.array([Coorxort[0]])))
    Y1 = NP.concatenate((Cooryort[1:len(Coorxort)], NP.array([Cooryort[0]])))
    DX = X1-Coorxort
    DY = Y1-Cooryort
    DS = NP.sqrt(DX**2+DY**2)
    for l in range(len(Ybarort)):
      DSa   = DS[(l-1):len(DS)]
      DSb   = DS[0:(l-1)]
      DS1   = NP.concatenate((DSa,DSb))
      Dist  = NP.zeros((len(Ybarort),), float)
      Gauss = NP.zeros((len(Ybarort),), float)
      for k in range(len(Ybarort)/2):
        Dist[k+1]  = Dist[k]  + DS1[k]
        Dist[-k-1] = Dist[-k] + DS1[-k-1]
      for k in range(len(Ybarort)):
        Gauss[k]   = NP.e**(-(2*Dist[k]/(pas/5))**2)

      Gauss2 = NP.concatenate((Gauss[1:len(Gauss)], NP.array([Gauss[0]])))
      Den    = DS1 * ((Gauss + Gauss2)/2)

      YbarortShft = NP.concatenate((Ybarort[l:len(Ybarort)],Ybarort[0:(l)]))
      Ybargauss   = YbarortShft * Gauss
      Ybargauss2  = NP.concatenate((Ybargauss[1:len(Ybargauss)], NP.array([Ybargauss[0]])))
      Num         = DS1 * ((Ybargauss + Ybargauss2)/2)

      YbarReg[l]  = NP.sum(Num)/NP.sum(Den)

    # Deuxieme point de la crete
    idxmax   = NP.argmax(YbarReg)
    valmax   = Ybarort[idxmax]
    cox      = Coorxort[idxmax]
    coy      = Cooryort[idxmax]

    CoxAmm   = NP.concatenate((CoxAmm, NP.array([cox])))
    CoyAmm   = NP.concatenate((CoyAmm, NP.array([coy])))
    YbarAmm  = NP.concatenate((YbarAmm, NP.array([valmax])))


    # On re-calcule le premier point
    #
    CoxLast  = NP.array([ CoxAmm[1] , CoxAmm[0] ])
    CoyLast  = NP.array([ CoyAmm[1] , CoyAmm[0] ])
    VersAvan = NP.array([CoxLast[1] - CoxLast[0], CoyLast[1] - CoyLast[0]])
    VersAvan = VersAvan / (NP.sqrt((VersAvan[0])**2 + (VersAvan[1])**2))

    Ppred    = NP.array([CoxLast[0] + VersAvan[0]*pas, CoyLast[0] + VersAvan[1]*pas ])
    VersNorm = (1 / NP.sqrt((VersAvan[0])**2 + (VersAvan[1])**2)) * NP.array([ -VersAvan[1] , VersAvan[0] ])
    PPlus    = NP.array([ Ppred[0] + (lort/2)*VersNorm[0] , Ppred[1] + (lort/2)*VersNorm[1] ])
    PMoin    = NP.array([ Ppred[0] - (lort/2)*VersNorm[0] , Ppred[1] - (lort/2)*VersNorm[1] ])

    # creation du profil orthogonal
    lignes = []
    groups = []
    arcs   = []
    lignes = []
    lignes.append((PMoin.tolist(),Ppred.tolist(),nbPoints))
    lignes.append((Ppred.tolist(),PPlus.tolist(),nbPoints))
    resu_mail0,arcgma0,angles0,nbno0 = crea_mail_lig_coup(dime,lignes,groups,arcs)

    fproc            = open(nomFichierSortie, 'w')
    fproc.write(resu_mail0)
    fproc.close()
    UL               = UniteAster()
    uniteMail        = UL.Libre(action = 'ASSOCIER', nom = nomFichierSortie)

    __MAI = LIRE_MAILLAGE(UNITE = uniteMail)
    UL.EtatInit(uniteMail)

    __MO = AFFE_MODELE(MAILLAGE = __MAI,
                       AFFE     = _F(TOUT         = 'OUI',
                                     PHENOMENE    = 'MECANIQUE',
                                     MODELISATION = 'BARRE'),)

    __YBARPR = PROJ_CHAMP(METHODE      = 'COLLOCATION',
                          RESULTAT     = __resu,
                          MODELE_1     = __modtot,
                          MODELE_2     = __MO,
                          DISTANCE_MAX = 0.,
                          TYPE_CHAM    = 'NOEU',
                          NOM_CHAM     = NOM_CHAM,
                          NUME_ORDRE   = 1, )


    __YBARCH = CREA_CHAMP(TYPE_CHAM  = Xtype_cham,
                          OPERATION  = 'EXTR',
                          NOM_CHAM   = NOM_CHAM,
                          RESULTAT   = __YBARPR,
                          NUME_ORDRE = 1,)

    dx0 = __YBARCH.EXTR_COMP(NOM_CMP,[],1)


    # Pas de cas ou le point de prediction est hors de matiere!
    #  Recherche du point de prediction parmis les points projetes
    #  et elimination du double point au milieu
    Nonvide  = NP.array(list(dx0.noeud))
    idxpred  = vfind(Nonvide,nbPoints)
    Ybarort  = dx0.valeurs

    Coor0    = __MAI.COORDO.VALE.get()
    Coorxort = NP.array(Coor0[0:len(Coor0):3] , float)
    Cooryort = NP.array(Coor0[1:len(Coor0):3] , float)
    Coorxort = NP.take(Coorxort,Nonvide-1)
    Cooryort = NP.take(Cooryort,Nonvide-1)
    Coorxort = delEl(Coorxort,idxpred)
    Cooryort = delEl(Cooryort,idxpred)
    Ybarort  = delEl(Ybarort,idxpred)

    # Regularisation du profil orthogonal
    YbarReg = NP.zeros((len(Ybarort),), float)
    for l in range(len(Ybarort)):
      xcentre = Coorxort[l]
      ycentre = Cooryort[l]
      Dist    = ((Coorxort-xcentre)**2 + (Cooryort-ycentre)**2)**0.5
      Gauss = NP.zeros((len(Dist),), float)
      for m in range(len(Dist)) :
        Gauss[m] = NP.e**(-(2*Dist[m]/lreg)**2)
      Ybargauss = Ybarort * Gauss
      DeltaL = NP.absolute(Dist[0:len(Dist)-1] - Dist[1:len(Dist)])
      Num = DeltaL * (Ybargauss[0:len(Dist)-1] + Ybargauss[1:len(Dist)])/2
      Den = DeltaL * (Gauss[0:len(Dist)-1] + Gauss[1:len(Dist)])/2

      YbarReg[l] = NP.sum(Num)/NP.sum(Den)

    # Premier point de la crete
    idxmax    = NP.argmax(YbarReg)
    valmax    = Ybarort[idxmax]
    cox       = Coorxort[idxmax]
    coy       = Cooryort[idxmax]
    CoxAmm[0] = cox
    CoyAmm[0] = coy
    YbarAmm[0]= valmax



    # --------------------------------------------------
    # Recherche de la crete apres amorcage
    #


    # Definition des deux directions d'avancement possibles
    #
    VersAvn1 = NP.array([CoxAmm[1]-CoxAmm[0],CoyAmm[1]-CoyAmm[0]])
    module   = ((VersAvn1[0])**2 + (VersAvn1[1])**2)**0.5
    VersAvn1 = VersAvn1 * (1/module)
    VersAvn2 = -VersAvn1

    # Initialisation vecteurs
    #
    Coxcrete1  = NP.array([CoxAmm[1]])
    Coycrete1  = NP.array([CoyAmm[1]])
    Ybarcrete1 = NP.array([YbarAmm[1]])

    Coxcrete2  = NP.array([CoxAmm[0]])
    Coycrete2  = NP.array([CoyAmm[0]])
    Ybarcrete2 = NP.array([YbarAmm[0]])

    # Boucle sur les points de la crete
    #     Variables du boucle :
    #       dirRech   --> direction de recherche, 1,2
    #       condSort  --> condition de sortie du boucle
    dirRech = 1
    i = 0
    condSort = 1. + seuil
    while (condSort > seuil and dirRech<=2) :
      i = i+1
      # Determination du vecteur d'avancement
      if i==1:
        if dirRech == 1:
          VersAvan = VersAvn1
        else:
          VersAvan = VersAvn2
      else:
        if dirRech == 1:
          CoxLast = NP.array( [Coxcrete1[i-2] , Coxcrete1[i-1] ])
          CoyLast = NP.array([ Coycrete1[i-2] , Coycrete1[i-1] ])
        else :
          CoxLast = NP.array( [Coxcrete2[i-2] , Coxcrete2[i-1] ])
          CoyLast = NP.array([ Coycrete2[i-2] , Coycrete2[i-1] ])
        VersAvan  = NP.array([CoxLast[1]-CoxLast[0],CoyLast[1]-CoyLast[0]])
        module    = ((VersAvan[0])**2. + (VersAvan[1])**2.)**0.5
        VersAvan  = VersAvan * (1/module)

      if dirRech == 1:
        PStart = NP.array([Coxcrete1[i-1],Coycrete1[i-1]])
      else:
        PStart = NP.array([Coxcrete2[i-1],Coycrete2[i-1]])

      # point de prediction
      Ppred    = NP.array([PStart[0] + VersAvan[0]*pas, PStart[1] + VersAvan[1]*pas ])
      VersNorm = (1. / NP.sqrt((VersAvan[0])**2. + (VersAvan[1])**2.)) * NP.array([ -VersAvan[1] , VersAvan[0] ])
      PPlus    = NP.array([ Ppred[0] + (lort/2.)*VersNorm[0] , Ppred[1] + (lort/2.)*VersNorm[1] ])
      PMoin    = NP.array([ Ppred[0] - (lort/2.)*VersNorm[0] , Ppred[1] - (lort/2.)*VersNorm[1] ])

      # creation du profil orthogonal
      lignes = []
      groups = []
      arcs   = []
      lignes.append((PMoin.tolist(),Ppred.tolist(),nbPoints))
      lignes.append((Ppred.tolist(),PPlus.tolist(),nbPoints))
      resu_mail0,arcgma0,angles0,nbno0 = crea_mail_lig_coup(dime,lignes,groups,arcs)

      fproc            = open(nomFichierSortie, 'w')
      fproc.write(resu_mail0)
      fproc.close()
      UL               = UniteAster()
      uniteMail        = UL.Libre(action = 'ASSOCIER', nom = nomFichierSortie)

      __MAI = LIRE_MAILLAGE(UNITE = uniteMail)
      UL.EtatInit(uniteMail)

      __MO = AFFE_MODELE(MAILLAGE = __MAI,
                        AFFE      = _F(TOUT      = 'OUI',
                                       PHENOMENE = 'MECANIQUE',
                                       MODELISATION = 'BARRE'),)


      try:
        # on essaie de projeter, exception: il n'y a pas des points "dans la matiere"
        __YBARPR = PROJ_CHAMP(METHODE      = 'COLLOCATION',
                              RESULTAT     = __resu,
                              MODELE_1     = __modtot,
                              MODELE_2     = __MO,
                              DISTANCE_MAX = 0.,
                              TYPE_CHAM    = 'NOEU',
                              NOM_CHAM     = NOM_CHAM,
                              NUME_ORDRE   = 1,)

      except :
        print "#MC dans EXCEPT"
        # Attention!! Ici on gere seulement le cas d'aucun point dans la matiere!
        # Il faudra gerer tous les possibles erreurs de proj_champ, ou trouver nom
        # a l'erreur specifique!
        if dirRech == 1 :
          dirRech = 2
          i = 0
        else:
          condSort = seuil * 0.1

      else :
      # si la projection est possible
        __YBARCH = CREA_CHAMP(TYPE_CHAM  = Xtype_cham,
                              OPERATION  = 'EXTR',
                              NOM_CHAM   = NOM_CHAM,
                              RESULTAT   = __YBARPR,
                              NUME_ORDRE = 1,)

        dx0     = __YBARCH.EXTR_COMP(NOM_CMP,[],1)
        Nonvide = NP.array(list(dx0.noeud))

        # recherche du point de prediction parmis les points projetes
        idxpred = vfind(Nonvide,nbPoints)

        # cas ou le point de prediction est hors matiere
        if len(idxpred)==0:
          if dirRech == 1:
            dirRech = 2
            i = 0
            continue
          else:
            condSort = seuil*0.1
            break

        Ybarort = dx0.valeurs
        Coor0    = __MAI.COORDO.VALE.get()
        Coorxort = NP.array(Coor0[0:len(Coor0):3] , float)
        Cooryort = NP.array(Coor0[1:len(Coor0):3] , float)
        Coorxort = NP.take(Coorxort,Nonvide-1)
        Cooryort = NP.take(Cooryort,Nonvide-1)
        Coorxort = delEl(Coorxort,idxpred)
        Cooryort = delEl(Cooryort,idxpred)
        Ybarort  = delEl(Ybarort,idxpred)

        # Regularisation sur le profil orthogonal
        #
        YbarReg = NP.zeros((len(Ybarort),), float)
        for l in range(len(Ybarort)):
          xcentre = Coorxort[l]
          ycentre = Cooryort[l]
          Dist    = ((Coorxort-xcentre)**2 + (Cooryort-ycentre)**2)**0.5
          Gauss   = NP.zeros((len(Dist),), float)
          for m in range(len(Dist)) :
            Gauss[m] = NP.e**(-(2*Dist[m]/lreg)**2)

          Ybargauss = Ybarort * Gauss
          DeltaL = NP.absolute(Dist[0:len(Dist)-1] - Dist[1:len(Dist)])
          Num = DeltaL * (Ybargauss[0:len(Dist)-1] + Ybargauss[1:len(Dist)])/2
          Den = DeltaL * (Gauss[0:len(Dist)-1] + Gauss[1:len(Dist)])/2
          YbarReg[l] = NP.sum(Num)/NP.sum(Den)


        # Nouveau point de la crete
        #
        idxmax  = NP.argmax(YbarReg)
        valmax  = Ybarort[idxmax]
        cox     = Coorxort[idxmax]
        coy     = Cooryort[idxmax]


        # on controle que l'angle forme par le point trouve
        #   et la direction de recherche ne soit pas plus grand
        #   du seuil "ANGL_MAX"

        if round(alpha) != 180. :
          alphar = radians(alpha)
          blim   = pas * NP.tan(alphar/2.)
          btest  = ((cox-Ppred[0])**2. + (coy-Ppred[1])**2.)**0.5
          if btest > blim :
            if dirRech == 1 :
              dirRech = 2
              i = 0
              continue
            else:
              condSort = seuil*0.1
              break

        if dirRech == 1:
          Coxcrete1  = NP.concatenate((Coxcrete1,NP.array([cox])))
          Coycrete1  = NP.concatenate((Coycrete1,NP.array([coy])))
          Ybarcrete1 = NP.concatenate((Ybarcrete1,NP.array([valmax])))
        else:
          Coxcrete2  = NP.concatenate((Coxcrete2,NP.array([cox])))
          Coycrete2  = NP.concatenate((Coycrete2,NP.array([coy])))
          Ybarcrete2 = NP.concatenate((Ybarcrete2,NP.array([valmax])))

        condSort = valmax
        if condSort <= seuil and dirRech == 1 :
          dirRech = 2
          i = 0

    Coxcrete1 = Coxcrete1.tolist()
    Coxcrete2 = Coxcrete2.tolist()
    Coycrete1 = Coycrete1.tolist()
    Coycrete2 = Coycrete2.tolist()
    Ybarcrete1 = Ybarcrete1.tolist()
    Ybarcrete2 = Ybarcrete2.tolist()
    Coxcrete2.reverse()
    Coycrete2.reverse()
    Ybarcrete2.reverse()
    Coxcrete2.extend(Coxcrete1)
    Coycrete2.extend(Coycrete1)
    Ybarcrete2.extend(Ybarcrete1)


    nbNoeud = len(Coxcrete2)
    Connex  = []
    for idxNo in range(nbNoeud-1) :
      no1 = idxNo+1
      no2 = idxNo+2
      ma  = (no1,no2)
      Connex.append(ma)

    XcreteTot.append(Coxcrete2)
    YcreteTot.append(Coycrete2)
    YbarcreteTot.append(Ybarcrete2)
    ConnTot.append(Connex)
    lstFissure = lstFissure + ( ['FISS'+str(idxRech+1)]*len(Coxcrete2) )

    lstX    = []
    lstY    = []
    lstYbar = []
    for i in range(len(XcreteTot)) :
      lstX       = lstX    + XcreteTot[i]
      lstY       = lstY    + YcreteTot[i]
      lstYbar    = lstYbar + YbarcreteTot[i]



  # --------------------------------------------------
  # CREATION D'UNE TABLE POUR LE STOCKAGE DE POINTS DE
  #   LA CRETE ET DE L'OUVERTURE DE FISSURE
  #

  __tabRes = CREA_TABLE(LISTE = (
              _F(PARA = 'FISSURE'    , LISTE_K = lstFissure ),
              _F(PARA = 'COORX'      , LISTE_R = lstX ),
              _F(PARA = 'COORY'      , LISTE_R = lstY ),
              _F(PARA = 'CHAMP'      , LISTE_R = lstYbar),
                    ),)


  # --------------------------------------------------
  # CREATION D'UNE SD MAILLAGE DE LA CRETE
  #
  resu_mail0 = crea_mail_lin(XcreteTot,YcreteTot,ConnTot,dime)


  fproc            = open(nomFichierSortie, 'w')
  fproc.write(resu_mail0)
  fproc.close()
  UL               = UniteAster()
  uniteMail        = UL.Libre(action = 'ASSOCIER', nom = nomFichierSortie)
  __MAIFI = LIRE_MAILLAGE(UNITE = uniteMail)
  UL.EtatInit(uniteMail)


  # --------------------------------------------------
  # SORTIE DE LA MACRO
  #
  RetablirAlarme('CALCULEL5_48')
  return ier

