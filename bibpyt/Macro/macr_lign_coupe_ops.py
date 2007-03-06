#@ MODIF macr_lign_coupe_ops Macro  DATE 05/03/2007   AUTEUR GALENNE E.GALENNE 
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
# script PYTHON de creation du résultat local

def crea_resu_local(dime,MODI_REPERE,NOM_CHAM,m,resin,mail,nomgrma):

  from Utilitai.Utmess     import UTMESS
  from math import pi,sqrt,atan2,asin
  import os,string,types
  import aster
  from Accas import _F

  epsi=0.00000001
  
  if NOM_CHAM == 'DEPL':
       if dime == 2:
          LCMP=['DX','DY']
          TYPE_CHAM='VECT_2D'
       elif dime ==3 :
          LCMP=['DX','DY','DZ']
          TYPE_CHAM='VECT_3D'
  elif NOM_CHAM in ('SIGM_NOEU_DEPL','SIEF_ELNO_ELGA','SIGM_NOEU_SIEF','SIGM_NOEU_ELGA','SIGM_NOEU_COQU'):
       if dime == 2:
          LCMP=['SIXX','SIYY','SIZZ','SIXY']
          TYPE_CHAM='TENS_2D'
       elif dime ==3 :
          LCMP=['SIXX','SIYY','SIZZ','SIXY','SIXZ','SIYZ']
          TYPE_CHAM='TENS_3D'


  if m['TYPE']=='SEGMENT':
    # --- determination des angles nautiques
    cx1=m['COOR_EXTR'][0]-m['COOR_ORIG'][0]
    cx2=m['COOR_EXTR'][1]-m['COOR_ORIG'][1]
    cx3=0.
    if dime == 3:
      cx3=m['COOR_EXTR'][2]-m['COOR_ORIG'][2]
    nvx=sqrt(cx1**2+cx2**2+cx3**2)
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
    nvy=sqrt(cy1**2+cy2**2+cy3**2)
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
      nvz=sqrt(cz1**2+cz2**2+cz3**2)
      cz1=cz1/nvz
      cz2=cz2/nvz
      cz3=cz3/nvz
      cy1=cz2*cx3-cz3*cx2
      cy2=cz3*cx1-cz1*cx3
      cy3=cz1*cx2-cz2*cx1
      nvy=sqrt(cy1**2+cy2**2+cy3**2)
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
      alpha = atan2(cx2,cx1)
    else:
      if cx1**2 + cx2**2 > epsi :
        alpha=atan2(cx2,cx1)
        beta=asin(cx3)
        gamma=atan2(cy3,cz3)
      else:
#        alpha=atan2(cy1,cz1)
        alpha=atan2(-cy1,cy2)
        beta=asin(cx3)
        gamma=0.
    alpha=alpha*180/pi
    beta=beta*180/pi
    gamma=gamma*180/pi

    motscles={}
    motscles['MODI_CHAM']=[]
    motscles['DEFI_REPERE']=[]
    motscles['MODI_CHAM'].append(_F(NOM_CHAM=NOM_CHAM,NOM_CMP=LCMP,TYPE_CHAM=TYPE_CHAM),)
    ANGL_NAUT=[]
    ANGL_NAUT.append(alpha)
    if dime ==3:
       ANGL_NAUT.append(beta)
       ANGL_NAUT.append(gamma)
    motscles['DEFI_REPERE'].append(_F(REPERE='UTILISATEUR',ANGL_NAUT=ANGL_NAUT),)
    __remodr=MODI_REPERE(RESULTAT=resin,**motscles)
    
    
  if m['TYPE']=='ARC':
    motscles={}
    motscles['MODI_CHAM']=[]
    motscles['DEFI_REPERE']=[]
    motscles['MODI_CHAM'].append(_F(NOM_CHAM=NOM_CHAM,NOM_CMP=LCMP,TYPE_CHAM=TYPE_CHAM),)
    ORIGINE=[]
    ORIGINE.append(m['CENTRE'][0])
    ORIGINE.append(m['CENTRE'][1])
    if dime ==3:
      vy = m['VECT_Y']
      dn = m['DNOR']
      if vy != dn :
        message = 'AVEC TYPE=ARC, IL FAUT OBLIGATOIREMENT QUE VECT_Y SOIT EGAL A DNOR \n POUR EXPRIMER LE RESULTAT DANS LE REPERE LOCAL. \n'
        message = message +' ON POURSUIT LE CALCUL EN PRENANT VECT_Y = DNOR.'
        UTMESS('A','MACR_LIGN_COUPE',message)
      ORIGINE.append(m['CENTRE'][2])
      AXE_Z=[]
      AXE_Z.append(m['DNOR'][0])
      AXE_Z.append(m['DNOR'][1])
      AXE_Z.append(m['DNOR'][2])
      motscles['DEFI_REPERE'].append(_F(REPERE='CYLINDRIQUE',ORIGINE=ORIGINE,AXE_Z=AXE_Z),)
    elif dime ==2:
      motscles['DEFI_REPERE'].append(_F(REPERE='CYLINDRIQUE',ORIGINE=ORIGINE,),)
    __remodr=MODI_REPERE(RESULTAT=resin,**motscles)

  return __remodr

########################################################################
# script PYTHON de creation des noeuds d'une ligne de coupe 'arc'

def crea_noeu_lig_coup(dimension,pt1,pt2,anglj,dnor):
  from Utilitai.Utmess     import UTMESS
  from math import pi,sin,cos,sqrt

  a=pt1[0]-pt2[0]
  b=pt1[1]-pt2[1]
  eps=0.00000001
  anglr=anglj*pi/180.
  if dimension==2:
    r=sqrt(a**2+b**2)
    if abs(r)<eps:
      UTMESS('F', "MACR_LIGN_COUPE", "definition incorrecte de COOR_ORIG et CENTRE") 
    x=pt2[0]+a*cos(anglr)-b*sin(anglr)
    y=pt2[1]+b*cos(anglr)+a*sin(anglr)
    return x,y
  elif dimension==3:
    c=pt1[2]-pt2[2]
    r=sqrt(a**2+b**2+c**2)
    if abs(r)<eps:
      UTMESS('F', "MACR_LIGN_COUPE", "definition incorrecte de COOR_ORIG et CENTRE") 
    d1=dnor[0]
    d2=dnor[1]
    d3=dnor[2]
    d=sqrt(d1**2+d2**2+d3**2)
    if abs(r)<eps:
      UTMESS('F', "MACR_LIGN_COUPE", "definition incorrecte de DNOR") 
    x=pt2[0]+a*cos(anglr)+sin(anglr)*(c*d2-b*d3)/d
    y=pt2[1]+b*cos(anglr)+sin(anglr)*(a*d3-c*d1)/d
    z=pt2[2]+c*cos(anglr)+sin(anglr)*(b*d1-a*d2)/d
    return x,y,z

########################################################################
# script PYTHON de creation d un maillage de ligne de coupe

def crea_mail_lig_coup(dimension,lignes,groups,arcs):

  import os,sys,copy
  from Utilitai.Utmess     import UTMESS

# construction du maillage au format Aster des segments de lignes de coupe

  nblig=len(lignes)
  nbngr=len(groups)
  nbarc=len(arcs)

  resu='TITRE\n'
  titre='Maillage ligne de coupe'+'\n'
  resu=resu+'FINSF\n'
  resu=resu+'COOR_'+str(dimension)+'D\n'
  epsi=0.00000001

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
  angles=[None]*nbarc
  for i in range(nbarc):
    pt1           = arcs[i][0]
    pt2           = arcs[i][1]
    nbp_lig_coupe = arcs[i][2]
    angle         = arcs[i][3]
    if abs(angle-360.)<epsi: nbpt=nbp_lig_coupe+1
    else                   : nbpt=nbp_lig_coupe
    if dimension==3:dnor=arcs[i][4]
    angles[i] = []
    for j in range(nbp_lig_coupe):
      anglj       = j*angle/(nbpt-1)
      angles[i].append(anglj)
      if dimension==2:
        nbno=nbno+1
        x,y=crea_noeu_lig_coup(dimension,pt1,pt2,anglj,dnor=[])
        noeud='  N'+str(nbno)+'   '+str(x)+'    '+str(y)+'\n'
        resu=resu+noeud
      elif dimension==3:
        nbno=nbno+1
        x,y,z=crea_noeu_lig_coup(dimension,pt1,pt2,anglj,dnor)
        noeud='  N'+str(nbno)+'   '+str(x)+'   '+str(y)+'   '+str(z)+'\n'
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
  nprec=0

  for i in range(nbarc):
    nbp_lig_coupe = arcs[i][2]
    angle         = arcs[i][3]
    resu=resu+'SEG2\n'
    nbmai=nbma+nblig+nbngr+nprec+i+1
    for j in range(nbp_lig_coupe-1):
        nbma=nbma+1
        maille='  M'+str(nbma)+' N'+str(nbma+nblig+nbngr+nprec+i)+' N'+str(nbma+nblig+nbngr+nprec+1+i)+'\n'
        resu=resu+maille
    if abs(angle-360.)<epsi:
        nbma=nbma+1
        maille='  M'+str(nbma)+' N'+str(nbma+nblig+nbngr+nprec+i)+' N'+str(nbmai)+'\n'
        nprec=nprec-1
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
  arcgma=[]
  for i in range(nbarc):
    resu=resu+'GROUP_MA\n'
    k=nblig+i
    resu=resu+'  LICOU'+str(k+1)
    arcgma.append('LICOU'+str(k+1))
    nbp_lig_coupe = arcs[i][2]
    angle         = arcs[i][3]
    if abs(angle-360.)<epsi: nbpt=nbp_lig_coupe+1
    else                   : nbpt=nbp_lig_coupe
    for j in range(nbpt-1):
        nbma=nbma+1
        resu=resu+'  M'+str(nbma)+'\n'
    resu=resu+'\n'
    resu=resu+'FINSF\n'
  resu=resu+'FIN\n'

  return resu,arcgma,angles,nbno


########################################################################
def macr_lign_coupe_ops(self,RESULTAT,UNITE_MAILLAGE,LIGN_COUPE,NOM_CHAM,
               MODELE,GROUP_MA,MAILLE,**args):
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
  arcs=[]
  minidim=dime

  for m in LIGN_COUPE :
      if m['TYPE'] =='SEGMENT' :
         lignes.append((m['COOR_ORIG'],m['COOR_EXTR'],m['NB_POINTS']))
         minidim=min(minidim,len(m['COOR_ORIG']),len(m['COOR_EXTR']))
         if minidim!=dime:
            UTMESS('F', "MACR_LIGN_COUPE", "dimensions de maillage et de coordonnees incoherentes")
      elif m['TYPE'] =='ARC' :
         minidim=min(minidim,len(m['COOR_ORIG']),len(m['CENTRE']))
         if minidim!=dime:
            UTMESS('F', "MACR_LIGN_COUPE", "dimensions de maillage et de coordonnees incoherentes")
         if dime==2:
           arcs.append((m['COOR_ORIG'],m['CENTRE'],m['NB_POINTS'],m['ANGLE'],))
         elif dime==3:
           if str(m['DNOR'])=='None':
              UTMESS('F', "MACR_LIGN_COUPE", "le mot-clé 'DNOR' est obligatoire en 3D pour le type 'ARC'")
           arcs.append((m['COOR_ORIG'],m['CENTRE'],m['NB_POINTS'],m['ANGLE'],m['DNOR']))
      elif m['TYPE']=='GROUP_NO':
        ngrno=m['GROUP_NO'].ljust(8).upper()
        if ngrno not in collgrno.keys() :
          UTMESS('F', "MACR_LIGN_COUPE", "le group_no "+ngrno+" n est pas dans le maillage "+n_mailla)
        grpn=collgrno[ngrno]
        l_coor_group=[ngrno,]
        for node in grpn:
          l_coor_group.append(aster.getvectjev(n_mailla.ljust(8)+'.COORDO    .VALE',3*(node-1),3))
        groups.append(l_coor_group)
  
  if arcs!=[] and (lignes!=[] or groups!=[]) :
    message = 'On ne peut pas combiner des lignes de coupes de type ARC avec des lignes de coupes SEGMENT ou GROUP_NO. \n'
    UTMESS('F','MACR_LIGN_COUPE',message)


  # Création du maillage des NB_POINTS segments entre COOR_ORIG et COOR_EXTR
  # ainsi que des segments reliant les noeuds issus des group_no demandés
  # par appel au script python crea_mail_lig_coup
  # le maillage est ensuite recopié dans l unité logique UNITE_MAILLAGE

  resu_mail,arcgma,angles,nbno=crea_mail_lig_coup(dime,lignes,groups,arcs)
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
      if m['TYPE'] != 'GROUP_NO' :
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

  motscles={}
  motscles['VIS_A_VIS']=[]
  if GROUP_MA != None :
    motscles['VIS_A_VIS'].append(_F(GROUP_MA_1 = GROUP_MA,TOUT_2='OUI'),)     
  if MAILLE != None :
    motscles['VIS_A_VIS'].append(_F(MAILLE_1 = MAILLE,TOUT_2='OUI'),)     
    
  if n_modele in self.get_global_contexte().keys() : MODELE_1=self.get_global_contexte()[n_modele]
  else                                             : MODELE_1=self.jdc.current_context[n_modele]
  __recou=PROJ_CHAMP(METHODE='ELEM',
                     RESULTAT=RESULTAT,
                     MODELE_1=MODELE_1,
                     MODELE_2=__mocou,
                     TYPE_CHAM='NOEU',
                     NOM_CHAM=NOM_CHAM, **motscles);     



  # Expression des contraintes aux noeuds ou des déplacements dans le repere local
  __remodr=__recou
  icham=0
  ioc2=0
  jarc=0
  mcACTION=[]
  angtab=[]
  if AsType(RESULTAT).__name__ in ('evol_elas','evol_noli') :
   if  NOM_CHAM in ('DEPL','SIEF_ELNO_ELGA','SIGM_NOEU_DEPL','SIGM_NOEU_SIEF','SIGM_NOEU_ELGA','SIGM_NOEU_COQU'):icham=1
   for m in LIGN_COUPE :
      if m['TYPE'] =='ARC' : jarc=jarc+1
      if m['VECT_Y'] !=None and icham==1:
        if m['TYPE'] =='ARC' :  nomgrma=arcgma[jarc-1]
        else                 :  nomgrma=' '
        __remodr=crea_resu_local(dime,MODI_REPERE,NOM_CHAM,m,__recou,__macou,nomgrma)

        if m['TYPE'] != 'GROUP_NO' :
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

      else:


  # Production d'une table pour toutes les lignes de coupe
        if m['VECT_Y'] !=None:
           UTMESS('A','MACR_LIGN_COUPE','LE CHAMP '+NOM_CHAM+' N EST PAS TRAITE PAR MACR_LIGNE_COUPE EN REPERE LOCAL.'
                   +'LE CALCUL EST EFFECTUE EN REPERE GLOBAL.')

        if m['TYPE'] != 'GROUP_NO' :
           ioc2=ioc2+1
           groupe='LICOU'+str(ioc2)
           if m['INTITULE'] !=None : intitl=m['INTITULE']
           else                    : intitl='l.coupe'+str(ioc2)
        elif m['GROUP_NO']!=None :
           groupe=m['GROUP_NO'].ljust(8).upper()
           if m['INTITULE'] !=None : intitl=m['INTITULE']
           else                    : intitl=groupe
        mcACTION.append( _F(INTITULE  = intitl,
                            RESULTAT  = __recou,
                            GROUP_NO  = groupe,
                            NOM_CHAM  = NOM_CHAM,
                            TOUT_CMP  = 'OUI',
                            OPERATION = 'EXTRACTION', )           )
 
  elif AsType(RESULTAT).__name__ in ('evol_ther',) :

     for m in LIGN_COUPE :
        if m['TYPE'] != 'GROUP_NO' :
          ioc2=ioc2+1
          groupe='LICOU'+str(ioc2)
          if m['INTITULE'] !=None : intitl=m['INTITULE']
          else                    : intitl='l.coupe'+str(ioc2)
        elif m['GROUP_NO']!=None :
          groupe=m['GROUP_NO'].ljust(8).upper()
          if m['INTITULE'] !=None : intitl=m['INTITULE']
          else                    : intitl=groupe
        mcACTION.append( _F(INTITULE  = intitl,
                            RESULTAT  = __recou,
                            GROUP_NO  = groupe,
                            NOM_CHAM  = NOM_CHAM,
                            TOUT_CMP  = 'OUI',
                            OPERATION = 'EXTRACTION', )           )
 
 
  __tabitm=POST_RELEVE_T(ACTION=mcACTION,);

  # on repasse par les tables python pour supprimer les paramètres inutiles
  # NOEUD (car il est propre au maillage de la ligne) et RESU

  self.DeclareOut('nomres',self.sd)
  dictab=__tabitm.EXTR_TABLE()

### Ajout de la colonne theta
  if len(arcgma)>0:
    coltab=[]
    val =  dictab['ABSC_CURV'].values()['ABSC_CURV']
    nbi = len(val) / nbno
    nba = len(angles)
    tmp =[]
    for k in range(nba) :
      for j in range(nbi) :
        for i in range(len(angles[k])) :
          tmp.append(angles[k][i])
    dictab['ANGLE']=tmp

###
  del dictab['NOEUD']
  del dictab['RESU']
  dprod = dictab.dict_CREA_TABLE()

  nomres=CREA_TABLE(**dprod)

  return ier
