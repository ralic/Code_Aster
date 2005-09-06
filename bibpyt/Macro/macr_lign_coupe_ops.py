#@ MODIF macr_lign_coupe_ops Macro  DATE 05/09/2005   AUTEUR DURAND C.DURAND 
# -*- coding: iso-8859-1 -*-
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



########################################################################
# script PYTHON de creation d un maillage de ligne de coupe

def crea_mail_lig_coup(dimension,lignes,groups):

  import os,sys,copy
  from Utilitai.Utmess     import UTMESS

# construction du maillage au format Aster des segments de lignes de coupe

  nblig=len(lignes)
  nbngr=len(groups)

  resu='TITRE\n'
  titre='Maillage ligne de coupe'+'\n'
  resu=resu+'FINSF\n'
  resu=resu+'COOR_'+str(dimension)+'D\n'

# creation des noeuds
  nbno=0
  for i in range(nblig):
    pt1           = lignes[i][0]
    pt2           = lignes[i][1]
    nbp_lig_coupe = lignes[i][2]
    for j in range(nbp_lig_coupe):
      if dimension==2:
        x=pt1[0]+j*(pt2[0]-pt1[0])/(nbp_lig_coupe-1)
        y=pt1[1]+j*(pt2[1]-pt1[1])/(nbp_lig_coupe-1)
        nbno=nbno+1
        noeud='  N'+str(nbno)+'   '+str(x)+'    '+str(y)+'\n'
        resu=resu+noeud
      elif dimension==3:
        x=pt1[0]+j*(pt2[0]-pt1[0])/(nbp_lig_coupe-1)
        y=pt1[1]+j*(pt2[1]-pt1[1])/(nbp_lig_coupe-1)
        z=pt1[2]+j*(pt2[2]-pt1[2])/(nbp_lig_coupe-1)
        nbno=nbno+1
        noeud='  N'+str(nbno)+'   '+str(x)+'   '+str(y)+'   '+str(z)+'\n'
        resu=resu+noeud
  for i in range(nbngr):
    for pt in groups[i][1:]:
      if dimension==2:
        nbno=nbno+1
        noeud='  N'+str(nbno)+' '+str(pt[0])+'    '+str(pt[1])+'\n'
        resu=resu+noeud
      elif dimension==3:
        nbno=nbno+1
        noeud='  N'+str(nbno)+' '+str(pt[0])+'    '+str(pt[1])+'    '+str(pt[2])+'\n'
        resu=resu+noeud
  resu=resu+'FINSF\n'

# creation des mailles
  nbma=0
  for i in range(nblig):
    nbp_lig_coupe = lignes[i][2]
    resu=resu+'SEG2\n'
    for j in range(nbp_lig_coupe-1):
        nbma=nbma+1
        maille='  M'+str(nbma)+' N'+str(nbma+i)+' N'+str(nbma+1+i)+'\n'
        resu=resu+maille
    resu=resu+'FINSF\n'
  for i in range(nbngr):
    resu=resu+'SEG2\n'
    for pt in groups[i][1:-1]:
        nbma=nbma+1
        maille='  M'+str(nbma)+' N'+str(nbma+nblig+i)+' N'+str(nbma+nblig+1+i)+'\n'
        resu=resu+maille
    resu=resu+'FINSF\n'

# creation des groupes de mailles (1 par ligne de coupe)
  nbma=0
  for i in range(nblig):
    resu=resu+'GROUP_MA\n'
    resu=resu+'  LICOU'+str(i+1)
    nbp_lig_coupe = lignes[i][2]
    for j in range(nbp_lig_coupe-1):
        nbma=nbma+1
        resu=resu+'  M'+str(nbma)+'\n'
    resu=resu+'\n'
    resu=resu+'FINSF\n'
  for i in range(nbngr):
    resu=resu+'GROUP_MA\n'
    resu=resu+groups[i][0]
    nbp_lig_coupe = len(groups[i])-1
    for j in range(nbp_lig_coupe-1):
        nbma=nbma+1
        resu=resu+'  M'+str(nbma)+'\n'
    resu=resu+'\n'
    resu=resu+'FINSF\n'
  resu=resu+'FIN\n'

  return resu


########################################################################
def macr_lign_coupe_ops(self,RESULTAT,UNITE_MAILLAGE,LIGN_COUPE,NOM_CHAM,MODELE,**args):
  """
     Ecriture de la macro MACR_LIGN_COUPE
  """
  import os,string,types
  from Accas import _F
  from Noyau.N_utils import AsType
  import aster,math
  from Utilitai.UniteAster import UniteAster
  from Utilitai.Utmess import UTMESS
  ier=0

  # On importe les definitions des commandes a utiliser dans la macro
  LIRE_MAILLAGE  =self.get_cmd('LIRE_MAILLAGE')
  DEFI_GROUP     =self.get_cmd('DEFI_GROUP')
  AFFE_MODELE    =self.get_cmd('AFFE_MODELE')
  PROJ_CHAMP     =self.get_cmd('PROJ_CHAMP')
  POST_RELEVE_T  =self.get_cmd('POST_RELEVE_T')
  CREA_TABLE     =self.get_cmd('CREA_TABLE')
  MODI_REPERE    =self.get_cmd('MODI_REPERE')

  # La macro compte pour 1 dans la numerotation des commandes
  self.set_icmd(1)
  
  nomresu=RESULTAT.nom
  l_modele=aster.getvectjev(nomresu.ljust(19)+'.MODL')
  n_modele=string.strip(l_modele[0])
  if n_modele=='' :
     if MODELE==None:
       UTMESS('F', "MACR_LIGN_COUPE", "nom du modele absent dans le concept resultat "+nomresu)
     else : n_modele=MODELE.nom
  l_mailla=aster.getvectjev(n_modele.ljust(8)+'.MODELE    .NOMA')
  n_mailla=string.strip(l_mailla[0])
  dime=aster.getvectjev(n_mailla.ljust(8)+'.DIME')[5]
  collgrno=aster.getcolljev(n_mailla.ljust(8)+'.GROUPENO')

  lignes=[]
  groups=[]
  minidim=dime
  for m in LIGN_COUPE :
      if m['NB_POINTS'] !=None :
         lignes.append((m['COOR_ORIG'],m['COOR_EXTR'],m['NB_POINTS']))
         minidim=min(minidim,len(m['COOR_ORIG']),len(m['COOR_EXTR']))
      elif m['GROUP_NO']!=None :
        ngrno=m['GROUP_NO'].ljust(8).upper()
        if ngrno not in collgrno.keys() :
          UTMESS('F', "MACR_LIGN_COUPE", "le group_no "+ngrno+" n est pas dans le maillage "+n_mailla)
        grpn=collgrno[ngrno]
        l_coor_group=[ngrno,]
        for node in grpn:
          l_coor_group.append(aster.getvectjev(n_mailla.ljust(8)+'.COORDO    .VALE',3*(node-1),3))
        groups.append(l_coor_group)

  if minidim!=dime:
    UTMESS('F', "MACR_LIGN_COUPE", "dimensions de maillage et de coordonnees incoherentes")


  # Création du maillage des NB_POINTS segments entre COOR_ORIG et COOR_EXTR
  # ainsi que des segments reliant les noeuds issus des group_no demandés
  # par appel au script python crea_mail_lig_coup
  # le maillage est ensuite recopié dans l unité logique UNITE_MAILLAGE

  resu_mail=crea_mail_lig_coup(dime,lignes,groups)
  UL = UniteAster()
  nomFichierSortie = UL.Nom(UNITE_MAILLAGE)
  fproc=open(nomFichierSortie,'w')
  fproc.write(resu_mail)
  fproc.close()
  UL.EtatInit(UNITE_MAILLAGE)

  # Lecture du maillage de seg2 contenant toutes les lignes de coupe

  __macou=LIRE_MAILLAGE(UNITE=UNITE_MAILLAGE,);

  motscles={}
  iocc=1
  motscles['CREA_GROUP_NO']=[]
  for m in LIGN_COUPE :
      if m['NB_POINTS'] !=None :
        motscles['CREA_GROUP_NO'].append(_F(GROUP_MA='LICOU'+str(iocc),) )
        iocc=iocc+1
      elif m['GROUP_NO']!=None :
        motscles['CREA_GROUP_NO'].append(_F(GROUP_MA=m['GROUP_NO'].ljust(8).upper(),) )
  __macou=DEFI_GROUP( reuse =__macou , MAILLAGE=__macou , **motscles );

  if AsType(RESULTAT).__name__ in ('evol_elas','evol_noli') :
    __mocou=AFFE_MODELE(MAILLAGE=__macou,
                        AFFE=_F(TOUT='OUI',
                                PHENOMENE='MECANIQUE',
                                MODELISATION='BARRE',),);
  elif AsType(RESULTAT).__name__ in ('evol_ther',) :
    __mocou=AFFE_MODELE(MAILLAGE=__macou,
                        AFFE=_F(TOUT='OUI',
                                PHENOMENE='THERMIQUE',
                                MODELISATION='PLAN',),);

  __recou=PROJ_CHAMP(METHODE='ELEM',
                     RESULTAT=RESULTAT,
                     MODELE_1=self.jdc.current_context[n_modele],
                     MODELE_2=__mocou,
                     TYPE_CHAM='NOEU',
                     NOM_CHAM=NOM_CHAM,);

  # Expression des contraintes aux noeuds ou des déplacements dans le repere local
  __remodr=__recou
  if AsType(RESULTAT).__name__ in ('evol_elas','evol_noli') :
   for m in LIGN_COUPE :
      if m['VECT_Y'] !=None :
        epsi=0.00000001
        # --- determination des angles nautiques
        cx1=m['COOR_EXTR'][0]-m['COOR_ORIG'][0]
        cx2=m['COOR_EXTR'][1]-m['COOR_ORIG'][1]
        cx3=0.
        if dime == 3:
          cx3=m['COOR_EXTR'][2]-m['COOR_ORIG'][2]
        nvx=math.sqrt(cx1**2+cx2**2+cx3**2)
        if abs(nvx) < epsi:
            UTMESS('F', "MACR_LIGN_COUPE", "definition incorrecte de la ligne de coupe")
        cx1=cx1/nvx
        cx2=cx2/nvx
        cx3=cx3/nvx
        cy1=m['VECT_Y'][0]
        cy2=m['VECT_Y'][1]
        cy3=0.
        if dime == 3:
          cy3=m['VECT_Y'][2]
        nvy=math.sqrt(cy1**2+cy2**2+cy3**2)
        if abs(nvy) < epsi:
            UTMESS('F', "MACR_LIGN_COUPE", "valeurs incorrectes pour VECT_Y")
        cy1=cy1/nvy
        cy2=cy2/nvy
        cy3=cy3/nvy
        if ((abs(cx1-cy1)<epsi and abs(cx2-cy2)<epsi and  abs(cx3-cy3)<epsi) or \
           (abs(cx1+cy1)<epsi and abs(cx2+cy2)<epsi and  abs(cx3+cy3)<epsi)):
            UTMESS('F', "MACR_LIGN_COUPE", "valeurs incorrectes pour VECT_Y: x colineaire a y")
        if abs(cx1*cy1+cx2*cy2+cx3*cy3) > epsi  :
          cz1=cx2*cy3-cx3*cy2
          cz2=cx3*cy1-cx1*cy3
          cz3=cx1*cy2-cx2*cy1
          nvz=math.sqrt(cz1**2+cz2**2+cz3**2)
          cz1=cz1/nvz
          cz2=cz2/nvz
          cz3=cz3/nvz
          cy1=cz2*cx3-cz3*cx2
          cy2=cz3*cx1-cz1*cx3
          cy3=cz1*cx2-cz2*cx1
          nvy=math.sqrt(cy1**2+cy2**2+cy3**2)
          cy1=cy1/nvy
          cy2=cy2/nvy
          cy3=cy3/nvy
          UTMESS('A','MACR_LIGN_COUPE','LE VECTEUR Y N EST PAS ORTHOGONAL A LA LIGNE DE COUPE'
                  +'LE VECTEUR Y A ETE ORTHONORMALISE POUR VOUS')
          UTMESS('A','MACR_LIGN_COUPE','VECT_Y=('+str(cy1)+','+str(cy2)+','+str(cy3)+')')
        else:     
          cz1=cx2*cy3-cx3*cy2
          cz2=cx3*cy1-cx1*cy3
          cz3=cx1*cy2-cx2*cy1
        beta=0.
        gamma=0.
        if dime ==2:
          alpha = math.atan2(cx2,cx1)
        else:
          if cx1**2 + cx2**2 > epsi :
            alpha=math.atan2(cx2,cx1)
            beta=math.asin(cx3)
            gamma=math.atan2(cy3,cz3)
          else:
            alpha=math.atan2(cy1,cz1)
            beta=math.asin(cx3)
            gamma=0.
        alpha=alpha*180/math.pi
        beta=beta*180/math.pi
        gamma=gamma*180/math.pi

        # --- MODI_REPERE
        motscles={}
        motscles['MODI_CHAM']=[]
        motscles['DEFI_REPERE']=[]
        # MODI_CHAM
        if NOM_CHAM == 'DEPL':
           if dime == 2:
              LCMP=['DX','DY']
              TYPE_CHAM='VECT_2D'
           elif dime ==3 :
              LCMP=['DX','DY','DZ']
              TYPE_CHAM='VECT_3D'
           motscles['MODI_CHAM'].append(_F(NOM_CHAM=NOM_CHAM,NOM_CMP=LCMP,TYPE_CHAM=TYPE_CHAM),)
        elif NOM_CHAM in ('SIGM_NOEU_DEPL','SIGM_NOEU_SIEF','SIGM_NOEU_ELGA','SIGM_NOEU_COQU'):
           if dime == 2:
              LCMP=['SIXX','SIYY','SIZZ','SIXY']
              TYPE_CHAM='TENS_2D'
           elif dime ==3 :
              LCMP=['SIXX','SIYY','SIZZ','SIXY','SIXZ','SIYZ']
              TYPE_CHAM='TENS_3D'
           motscles['MODI_CHAM'].append(_F(NOM_CHAM=NOM_CHAM,NOM_CMP=LCMP,TYPE_CHAM=TYPE_CHAM),)
        # DEFI_REPERE
        ANGL_NAUT=[]
        ANGL_NAUT.append(alpha)
        if dime ==3:
           ANGL_NAUT.append(beta)
           ANGL_NAUT.append(gamma)
        motscles['DEFI_REPERE'].append(_F(REPERE='UTILISATEUR',ANGL_NAUT=ANGL_NAUT),)
        __remodr=MODI_REPERE(RESULTAT=__recou,**motscles)



  # Production d'une table pour toutes les lignes de coupe

  ioc2=0
  mcACTION=[]
  for m in LIGN_COUPE :
      if m['NB_POINTS'] !=None :
        ioc2=ioc2+1
        groupe='LICOU'+str(ioc2)
        if m['INTITULE'] !=None : intitl=m['INTITULE']
        else                    : intitl='l.coupe'+str(ioc2)
      elif m['GROUP_NO']!=None :
        groupe=m['GROUP_NO'].ljust(8).upper()
        if m['INTITULE'] !=None : intitl=m['INTITULE']
        else                    : intitl=groupe
      mcACTION.append( _F(INTITULE  = intitl,
                          RESULTAT  = __remodr,
                          GROUP_NO  = groupe,
                          NOM_CHAM  = NOM_CHAM,
                          TOUT_CMP  = 'OUI',
                          OPERATION = 'EXTRACTION', )           )

  __tabitm=POST_RELEVE_T(ACTION=mcACTION,);

  # on repasse par les tables python pour supprimer les paramètres inutiles
  # NOEUD (car il est propre au maillage de la ligne) et RESU

  self.DeclareOut('nomres',self.sd)
  dictab=__tabitm.EXTR_TABLE()
  listpara=dictab.para
  listpara.remove('NOEUD')
  listpara.remove('RESU')

  coltab=[]
  for key in listpara :
      val=dictab[key].values()[key]
      if   type(val[0])==types.IntType :
         coltab.append(_F(PARA=key,LISTE_I=val))
      elif type(val[0])==types.FloatType :
         coltab.append(_F(PARA=key,LISTE_R=val))
      elif type(val[0])==types.StringType :
         coltab.append(_F(PARA=key,LISTE_K=val,TYPE_K='K16'))
  nomres=CREA_TABLE(LISTE=coltab)

  return ier
