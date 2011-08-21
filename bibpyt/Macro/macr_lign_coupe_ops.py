#@ MODIF macr_lign_coupe_ops Macro  DATE 23/08/2011   AUTEUR DELMAS J.DELMAS 
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

########################################################################
# script PYTHON de creation du résultat local
########################################################################

########################################################################
# verification que les points de la ligne de coupe sont dans la matiere
def crea_grp_matiere(self,groupe,newgrp,iocc,m,__remodr,NOM_CHAM,LIGN_COUPE,__macou):

  import aster
  import os,string,types
  from Accas import _F
  from Noyau.N_utils import AsType
  from Utilitai.Utmess import  UTMESS
  import os
  POST_RELEVE_T = self.get_cmd('POST_RELEVE_T')
  DEFI_GROUP    = self.get_cmd('DEFI_GROUP')

  motscles={}
  if m['NOM_CMP']!=None:
     motscles['NOM_CMP']=m['NOM_CMP']
  else:
     motscles['TOUT_CMP']='OUI'
  motscles['OPERATION']='EXTRACTION'

  __tab=POST_RELEVE_T(ACTION=_F(  INTITULE=newgrp,
                           RESULTAT  = __remodr,
                           NOM_CHAM=NOM_CHAM,
                           GROUP_NO  = groupe,**motscles ))

  # dictb=table initiale (contenant éventuellement des noeuds hors matière)
  dictb=__tab.EXTR_TABLE()
  # listenoe_b=liste ordonnee des noeuds de la ligne de coupe (avec doublons)
  listenoe_b = dictb.NOEUD.values()
  # lno_b2=liste des noeuds de la ligne de coupe après élimination des doublons
  # (attention, on perd l'ordre des noeuds)
  lno_b2 = set(listenoe_b)

  # dictc=table (extraite de dictb) contenant uniquement des noeuds dans la matière
  if m['NOM_CMP']!=None:
     dictc=getattr(dictb,m['NOM_CMP'][0]).NON_VIDE()
     lno_c2 = set(dictc.NOEUD.values())
  else:# TOUT_CMP='OUI'
     # on garde uniquement les composantes pour conserver les noeuds où il y a des valeurs
     a_suppr = set(['INTITULE', 'RESU', 'NOM_CHAM', 'NUME_ORDRE', 'INST', 'ABSC_CURV', 'COOR_X', 'COOR_Y', 'COOR_Z'])
     new_para = set(dictb.para)
     new_para.difference_update(a_suppr)

     lno_c2 = set()
     for comp in new_para.difference(['NOEUD']):
        dictc = getattr(dictb, comp).NON_VIDE()
        lno_c2.update(dictc.NOEUD.values())

  # on réordonne la liste des noeuds de lno_c2 (selon leur position dans listenoe_b) => l_matiere
  # l_horsmat=liste des noeuds hors matière
  l_matiere = [j for j in listenoe_b if j in lno_c2]
  nderm=l_matiere.index(l_matiere[len(l_matiere)-1])
  l_horsmat = [j for j in listenoe_b if j not in lno_c2]

  # si on est en présence de noeuds hors matière,
  # on emet une alarme pour informer l'utilisateur
  nbpoin=m['NB_POINTS']
  reste=nbpoin-len(l_matiere)
  if len(l_horsmat) > 0:

       nderh=l_horsmat.index(l_horsmat[len(l_horsmat)-1])
       cnom = list(__macou.sdj.NOMNOE.get())
       l_coor = __macou.sdj.COORDO.VALE.get()
       indent=os.linesep+' '*12
       l_surlig = []
       l_horslig = []
       for j in l_matiere[:nderm+1]:
          nuno=cnom.index(j.ljust(8))
          text_coordo = '(%f, %f, %f)' % tuple(l_coor[3*nuno:3*nuno+3])
          l_surlig.append(text_coordo)
       for j in l_horsmat[:nderh+1]:
          nuno=cnom.index(j.ljust(8))
          text_coordo = '(%f, %f, %f)' % tuple(l_coor[3*nuno:3*nuno+3])
          l_horslig.append(text_coordo)
       UTMESS('A','POST0_8',valk=[indent.join(l_surlig),indent.join(l_horslig)])

  elif reste > 0:

       cnom = list(__macou.sdj.NOMNOE.get())
       l_coor = __macou.sdj.COORDO.VALE.get()
       indent=os.linesep+' '*12
       l_surlig = []
       for j in l_matiere[:nderm+1]:
          nuno=cnom.index(j.ljust(8))
          text_coordo = '(%f, %f, %f)' % tuple(l_coor[3*nuno:3*nuno+3])
          l_surlig.append(text_coordo)
       UTMESS('A','POST0_24',vali=[iocc,reste],valk=[indent.join(l_surlig)])

  __macou=DEFI_GROUP( reuse =__macou , MAILLAGE=__macou ,
                   CREA_GROUP_NO=_F(NOM=newgrp,NOEUD=l_matiere[:nderm+1]),)

  return

def crea_resu_local(self,dime,NOM_CHAM,m,resin,mail,nomgrma):

  from Utilitai.Utmess     import  UTMESS
  from math import pi,sqrt,atan2,asin
  import os,string,types
  import aster
  from Accas import _F
  MODI_REPERE = self.get_cmd('MODI_REPERE')

  epsi=0.00000001

  if NOM_CHAM == 'DEPL':
       if dime == 2:
          LCMP=['DX','DY']
          TYPE_CHAM='VECT_2D'
       elif dime ==3 :
          LCMP=['DX','DY','DZ']
          TYPE_CHAM='VECT_3D'
  elif NOM_CHAM in ('SIGM_NOEU','SIEF_ELNO','SIGM_NOEU_ELGA','SICO_NOEU','SIGM_ELNO'):
       if dime == 2:
          LCMP=['SIXX','SIYY','SIZZ','SIXY']
          TYPE_CHAM='TENS_2D'
       elif dime ==3 :
          LCMP=['SIXX','SIYY','SIZZ','SIXY','SIXZ','SIYZ']
          TYPE_CHAM='TENS_3D'


  if m['TYPE']=='SEGMENT' and m['REPERE'] != 'CYLINDRIQUE' :

    if m['REPERE'] == 'LOCAL':
      # --- determination des angles nautiques
      cx1=m['COOR_EXTR'][0]-m['COOR_ORIG'][0]
      cx2=m['COOR_EXTR'][1]-m['COOR_ORIG'][1]
      cx3=0.
      if dime == 3:
        cx3=m['COOR_EXTR'][2]-m['COOR_ORIG'][2]
      nvx=sqrt(cx1**2+cx2**2+cx3**2)
      if abs(nvx) < epsi:
         UTMESS('F','POST0_1')
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
         UTMESS('F','POST0_2')
      cy1=cy1/nvy
      cy2=cy2/nvy
      cy3=cy3/nvy
      if ((abs(cx1-cy1)<epsi and abs(cx2-cy2)<epsi and  abs(cx3-cy3)<epsi) or \
         (abs(cx1+cy1)<epsi and abs(cx2+cy2)<epsi and  abs(cx3+cy3)<epsi)):
         UTMESS('F','POST0_3')
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
        UTMESS('A','POST0_4',valr=[cy1,cy2,cy3])
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
          beta=-asin(cx3)
          gamma=atan2(cy3,cz3)
        else:
          alpha=atan2(-cy1,cy2)
          beta=-asin(cx3)
          gamma=0.
      alpha=alpha*180/pi
      beta=beta*180/pi
      gamma=gamma*180/pi

    elif m['REPERE'] == 'UTILISATEUR':
      alpha=m['ANGL_NAUT'][0]
      beta =m['ANGL_NAUT'][1]
      gamma=m['ANGL_NAUT'][2]

    motscles={}
    motscles['MODI_CHAM']=[]
    motscles['AFFE']=[]
    motscles['MODI_CHAM'].append(_F(NOM_CHAM=NOM_CHAM,NOM_CMP=LCMP,TYPE_CHAM=TYPE_CHAM),)
    ANGL_NAUT=[]
    ANGL_NAUT.append(alpha)
    if dime ==3:
       ANGL_NAUT.append(beta)
       ANGL_NAUT.append(gamma)
    motscles['AFFE'].append(_F(ANGL_NAUT=ANGL_NAUT),)
    __remodr=MODI_REPERE(RESULTAT=resin,REPERE='UTILISATEUR',**motscles)


  if m['TYPE']=='ARC':
    if m['REPERE'] == 'CYLINDRIQUE' :
      motscles={}
      motscles['MODI_CHAM']=[]
      motscles['AFFE']=[]
      motscles['MODI_CHAM'].append(_F(NOM_CHAM=NOM_CHAM,NOM_CMP=LCMP,TYPE_CHAM=TYPE_CHAM),)
      ORIGINE=[]
      ORIGINE.append(m['CENTRE'][0])
      ORIGINE.append(m['CENTRE'][1])
      if dime ==3:
        ORIGINE.append(m['CENTRE'][2])
        AXE_Z=[]
        AXE_Z.append(m['DNOR'][0])
        AXE_Z.append(m['DNOR'][1])
        AXE_Z.append(m['DNOR'][2])
        motscles['AFFE'].append(_F(ORIGINE=ORIGINE,AXE_Z=AXE_Z),)
      elif dime ==2:
        motscles['AFFE'].append(_F(ORIGINE=ORIGINE,),)
      __remodr=MODI_REPERE(RESULTAT=resin,REPERE='CYLINDRIQUE',**motscles)
    else :
      UTMESS('F','POST0_5',valk=[m['TYPE'],m['REPERE']])



  if m['TYPE'][:5]=='GROUP' or m['TYPE']=='SEGMENT':

    if m['TYPE'][:5]=='GROUP' and m['REPERE'] == 'LOCAL':
     # determination du repère local (v1,v2,v3)
     # ---------------------------------------
      noma=mail.nom
      collgrma=aster.getcolljev(noma.ljust(8)+'.GROUPEMA')
      collcnx =aster.getcolljev(noma.ljust(8)+'.CONNEX')
      coord   =aster.getvectjev(noma.ljust(8)+'.COORDO    .VALE')
      cnom    =aster.getvectjev(noma.ljust(8)+'.NOMNOE')

      numa=collgrma[nomgrma.ljust(8)]
      dictu={}
#     initialisations
      for ima in numa:
        n1=collcnx[ima][0]
        n2=collcnx[ima][1]
        dictu[n1]=[]
        dictu[n2]=[]
#     determination du vecteur tangent (v1) + normalisation
      for ima in numa:
        vectu1=[]
        vectu2=[]
        n1=collcnx[ima][0]
        n2=collcnx[ima][1]
        ux=coord[3*(n2-1)]-coord[3*(n1-1)]
        uy=coord[3*(n2-1)+1]-coord[3*(n1-1)+1]
        vectu1.append(ux)
        vectu1.append(uy)
        vectu2.append(ux)
        vectu2.append(uy)
        if dime ==3 :
          uz=coord[3*(n2-1)+2]-coord[3*(n1-1)+2]
          vectu1.append(uz)
          vectu2.append(uz)
        dictu[n1].append(vectu1)
        dictu[n2].append(vectu2)
      for i in dictu:
        if len(dictu[i])==2:
          dictu[i][0][0]=dictu[i][0][0]+dictu[i][1][0]
          dictu[i][0][1]=dictu[i][0][1]+dictu[i][1][1]
          if dime==3:dictu[i][0][2]=dictu[i][0][2]+dictu[i][1][2]
          del dictu[i][1]
      for i in dictu:
        if dime==2:
          norm=sqrt(dictu[i][0][0]**2+dictu[i][0][1]**2)
          dictu[i][0][0]=dictu[i][0][0]/norm
          dictu[i][0][1]=dictu[i][0][1]/norm
        elif dime==3:
          norm=sqrt(dictu[i][0][0]**2+dictu[i][0][1]**2+dictu[i][0][2]**2)
          dictu[i][0][0]=dictu[i][0][0]/norm
          dictu[i][0][1]=dictu[i][0][1]/norm
          dictu[i][0][2]=dictu[i][0][2]/norm
#     determination du vecteur normal (v2):
#     on projete VECT_Y sur le plan orthogonal au vecteur v1.
#     (ce vecteur normal est obtenu par 2 produits vectoriels successifs en 3D)
      if dime==3:
        norm=sqrt(m['VECT_Y'][0]**2+m['VECT_Y'][1]**2+m['VECT_Y'][2]**2)
        tmpy=[m['VECT_Y'][0]/norm,m['VECT_Y'][1]/norm,m['VECT_Y'][2]/norm]
      j=0
      __resu=[None]*(len(dictu)+1)
      __resu[0]=resin
      for i in dictu:
          j=j+1
          vecty=[]
          if dime==2:
             vecty.append(-dictu[i][0][1])
             vecty.append(dictu[i][0][0])
             dictu[i].append(vecty)
          elif dime==3:
             # v3= v1 vectoriel vect_y
             vectz=[]
             vectz.append(dictu[i][0][1]*tmpy[2]-dictu[i][0][2]*tmpy[1])
             vectz.append(dictu[i][0][2]*tmpy[0]-dictu[i][0][0]*tmpy[2])
             vectz.append(dictu[i][0][0]*tmpy[1]-dictu[i][0][1]*tmpy[0])
             normz=sqrt(vectz[0]**2+vectz[1]**2+vectz[2]**2)
             vectz[0]=vectz[0]/normz
             vectz[1]=vectz[1]/normz
             vectz[2]=vectz[2]/normz
             vecty.append(vectz[1]*dictu[i][0][2]-vectz[2]*dictu[i][0][1])
             vecty.append(vectz[2]*dictu[i][0][0]-vectz[0]*dictu[i][0][2])
             vecty.append(vectz[0]*dictu[i][0][1]-vectz[1]*dictu[i][0][0])
             normy=sqrt(vecty[0]**2+vecty[1]**2+vecty[2]**2)
             vecty[0]=vecty[0]/normy
             vecty[1]=vecty[1]/normy
             vecty[2]=vecty[2]/normy
             dictu[i].append(vecty)
             dictu[i].append(vectz)
          cx1=dictu[i][0][0]
          cx2=dictu[i][0][1]
          cy1=dictu[i][1][0]
          cy2=dictu[i][1][1]
          if dime==3:
             cx3=dictu[i][0][2]
             cy3=dictu[i][1][2]
             cz1=dictu[i][2][0]
             cz2=dictu[i][2][1]
             cz3=dictu[i][2][2]

     # determination des angles nautiques (alpha,beta,gamma)
     # ----------------------------------------------------
          beta=0.
          gamma=0.
          if dime ==2:
            alpha = atan2(cx2,cx1)
          else:
            if cx1**2 + cx2**2 > epsi :
              alpha=atan2(cx2,cx1)
              beta=-asin(cx3)
              gamma=atan2(cy3,cz3)
            else:
              alpha=atan2(-cy1,cy2)
              beta=-asin(cx3)
              gamma=0.
          alpha=alpha*180/pi
          beta=beta*180/pi
          gamma=gamma*180/pi
          motscles={}
          motscles['MODI_CHAM']=[]
          motscles['AFFE']=[]
          noeu=dictu.keys()
          motscles['MODI_CHAM'].append(_F(NOM_CHAM=NOM_CHAM,NOM_CMP=LCMP,TYPE_CHAM=TYPE_CHAM,),)
          ANGL_NAUT=[]
          ANGL_NAUT.append(alpha)
          if dime ==3:
            ANGL_NAUT.append(beta)
            ANGL_NAUT.append(gamma)
          motscles['AFFE'].append(_F(ANGL_NAUT=ANGL_NAUT,NOEUD=cnom[noeu[j-1]-1]),)
          __resu[j]=MODI_REPERE(RESULTAT=__resu[j-1],REPERE='UTILISATEUR',**motscles)
      __remodr=__resu[j]


    motscles={}
    motscles['MODI_CHAM']=[]
    motscles['AFFE']=[]
    motscles['MODI_CHAM'].append(_F(NOM_CHAM=NOM_CHAM,NOM_CMP=LCMP,TYPE_CHAM=TYPE_CHAM),)
    if m['REPERE'] == 'CYLINDRIQUE' :
      if dime ==3:
        motscles['AFFE'].append(_F(ORIGINE=m['ORIGINE'],AXE_Z=m['AXE_Z']),)
      elif dime ==2:
        motscles['AFFE'].append(_F(ORIGINE=m['ORIGINE'],),)
      __remodr=MODI_REPERE(RESULTAT=resin,REPERE='CYLINDRIQUE',**motscles)
    elif m['REPERE'] == 'UTILISATEUR':
      alpha=m['ANGL_NAUT'][0]
      beta =m['ANGL_NAUT'][1]
      gamma=m['ANGL_NAUT'][2]
      ANGL_NAUT=[]
      ANGL_NAUT.append(alpha)
      if dime ==3:
        ANGL_NAUT.append(beta)
        ANGL_NAUT.append(gamma)
      motscles['AFFE'].append(_F(ANGL_NAUT=ANGL_NAUT),)
      __remodr=MODI_REPERE(RESULTAT=resin,REPERE='UTILISATEUR',**motscles)


  return __remodr

########################################################################
# script PYTHON de creation des noeuds d'une ligne de coupe 'arc'

def crea_noeu_lig_coup(dimension,pt1,pt2,anglj,dnor):
  from Utilitai.Utmess     import  UTMESS
  from math import pi,sin,cos,sqrt

  a=pt1[0]-pt2[0]
  b=pt1[1]-pt2[1]
  eps=0.00000001
  anglr=anglj*pi/180.
  if dimension==2:
    r=sqrt(a**2+b**2)
    if abs(r)<eps:
      UTMESS('F','POST0_6')
    x=pt2[0]+a*cos(anglr)-b*sin(anglr)
    y=pt2[1]+b*cos(anglr)+a*sin(anglr)
    return x,y
  elif dimension==3:
    c=pt1[2]-pt2[2]
    r=sqrt(a**2+b**2+c**2)
    if abs(r)<eps:
      UTMESS('F','POST0_6')
    d1=dnor[0]
    d2=dnor[1]
    d3=dnor[2]
    d=sqrt(d1**2+d2**2+d3**2)
    if abs(r)<eps:
      UTMESS('F','POST0_7')
    x=pt2[0]+a*cos(anglr)+sin(anglr)*(c*d2-b*d3)/d
    y=pt2[1]+b*cos(anglr)+sin(anglr)*(a*d3-c*d1)/d
    z=pt2[2]+c*cos(anglr)+sin(anglr)*(b*d1-a*d2)/d
    return x,y,z
########################################################################
# determination de la distance min entre 2 points consécutifs de la ligne de coupe

def dist_min_deux_points(mail):
  from math import sqrt
  import aster
  nno=aster.getvectjev(mail.nom.ljust(8)+'.DIME')[0]
  l_coor1=[]
  l_coor2=[]
  for i in range(nno-1):
    l_coor1=aster.getvectjev(mail.nom.ljust(8)+'.COORDO    .VALE',3*(i),3)
    l_coor2=aster.getvectjev(mail.nom.ljust(8)+'.COORDO    .VALE',3*(i+1),3)
    d=sqrt( (l_coor1[0]-l_coor2[0])**2+(l_coor1[1]-l_coor2[1])**2+(l_coor1[2]-l_coor2[2])**2)
    if i == 0 : dist=d
    else      : dist=min(d,dist)
  return dist

########################################################################
# script PYTHON de creation d un maillage de ligne de coupe

def crea_mail_lig_coup(dimension,lignes,groups,arcs):

  import os,sys,copy
  from Utilitai.Utmess     import  UTMESS

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
def macr_lign_coupe_ops(self,RESULTAT,CHAM_GD,UNITE_MAILLAGE,LIGN_COUPE,
              NOM_CHAM,MODELE,**args):

  """
     Ecriture de la macro MACR_LIGN_COUPE
  """
  import os,string,types
  from Accas import _F
  from Noyau.N_utils import AsType
  import aster,math
  from Utilitai.UniteAster import UniteAster
  from Utilitai.Utmess import  UTMESS, MasquerAlarme, RetablirAlarme
  ier=0

  # On importe les definitions des commandes a utiliser dans la macro
  LIRE_MAILLAGE  =self.get_cmd('LIRE_MAILLAGE')
  DEFI_GROUP     =self.get_cmd('DEFI_GROUP')
  AFFE_MODELE    =self.get_cmd('AFFE_MODELE')
  PROJ_CHAMP     =self.get_cmd('PROJ_CHAMP')
  POST_RELEVE_T  =self.get_cmd('POST_RELEVE_T')
  CREA_TABLE     =self.get_cmd('CREA_TABLE')
  CREA_RESU      =self.get_cmd('CREA_RESU')
  CREA_MAILLAGE  =self.get_cmd('CREA_MAILLAGE')

  # La macro compte pour 1 dans la numerotation des commandes
  self.set_icmd(1)

  #
  MasquerAlarme('CALCULEL2_63')
  MasquerAlarme('CALCULEL2_64')
  MasquerAlarme('MODELISA5_53')

  mcORDR={}

  if RESULTAT != None:
    if args['NUME_ORDRE'] != None :
      mcORDR['NUME_ORDRE']=args['NUME_ORDRE']
    elif args['LIST_ORDRE']  != None:
      mcORDR['LIST_ORDRE']=args['LIST_ORDRE']
    elif args['INST']  != None:
      mcORDR['INST']=args['INST']
    elif args['INST']  != None:
      mcORDR['INST']=args['INST']
    elif args['LIST_INST']  != None:
      mcORDR['LIST_INST']=args['LIST_INST']
    else :
      mcORDR['TOUT_ORDRE']='OUI'

    nomresu=RESULTAT.nom
    iret,ibid,n_modele = aster.dismoi('F','MODELE',nomresu,'RESULTAT')
    n_modele=n_modele.strip()
    if n_modele=='' :
      if MODELE==None:
        UTMESS('F','POST0_9',valk=nomresu)
      else : n_modele=MODELE.nom

  elif CHAM_GD != None:
    mcORDR['TOUT_ORDRE']='OUI'
    if MODELE==None:
      UTMESS('F','POST0_10')
    else : n_modele=MODELE.nom
    # récupération de la grandeur du champ
    n_cham=CHAM_GD.nom
    catagd=aster.getvectjev("&CATA.GD.NOMGD")
    desc=aster.getvectjev(string.ljust(n_cham,19)+'.DESC')
    if desc!= None:
      nomgd=catagd[desc[0]-1]
    else:
      celd=aster.getvectjev(string.ljust(n_cham,19)+'.CELD')
      nomgd=catagd[celd[0]-1]
    # détermination du type de résultat à créer
    if   nomgd[:6] == 'TEMP_R' : TYPE_RESU='EVOL_THER'
    elif nomgd[:6] == 'DEPL_R' : TYPE_RESU='EVOL_ELAS'
    elif nomgd[:6] == 'EPSI_R' : TYPE_RESU='EVOL_ELAS'
    elif nomgd[:6] == 'VAR2_R' : TYPE_RESU='EVOL_NOLI'
    elif nomgd[:6] == 'VARI_R' : TYPE_RESU='EVOL_NOLI'
    elif nomgd[:6] == 'SIEF_R' :
       if   NOM_CHAM[:4]=='SIGM':TYPE_RESU='EVOL_ELAS'
       elif NOM_CHAM[:4]=='SIEF':TYPE_RESU='EVOL_NOLI'
    # création d'un concept résultat à partir du champ CHAM_GD
    __resuch=CREA_RESU(OPERATION='AFFE',
                       NOM_CHAM=NOM_CHAM, TYPE_RESU=TYPE_RESU,
                       AFFE=_F(CHAM_GD=CHAM_GD,INST=0.),)
    RESULTAT=__resuch
  l_mailla=aster.getvectjev(n_modele.ljust(8)+'.MODELE    .LGRF')
  n_mailla=string.strip(l_mailla[0])
  # le maillage est-il 2D ou 3D ?
  iret,dime,kbid = aster.dismoi('F','DIM_GEOM',n_mailla,'MAILLAGE')
  collgrma=aster.getcolljev(n_mailla.ljust(8)+'.GROUPEMA')
  typma=aster.getvectjev(n_mailla.ljust(8)+'.TYPMAIL')
  connex=aster.getcolljev(n_mailla.ljust(8)+'.CONNEX')
  ltyma =aster.getvectjev("&CATA.TM.NOMTM")

  lignes=[]
  groups=[]
  arcs=[]
  minidim=dime

  for m in LIGN_COUPE :
      if m['TYPE'] =='SEGMENT' :
         lignes.append((m['COOR_ORIG'],m['COOR_EXTR'],m['NB_POINTS']))
         minidim=min(minidim,len(m['COOR_ORIG']),len(m['COOR_EXTR']))
         if minidim!=dime:
           UTMESS('F','POST0_11')
      elif m['TYPE'] =='ARC' :
         minidim=min(minidim,len(m['COOR_ORIG']),len(m['CENTRE']))
         if minidim!=dime:
           UTMESS('F','POST0_11')
         if dime==2:
           arcs.append((m['COOR_ORIG'],m['CENTRE'],m['NB_POINTS'],m['ANGLE'],))
         elif dime==3:
           if str(m['DNOR'])=='None':
              UTMESS('F','POST0_12')
           arcs.append((m['COOR_ORIG'],m['CENTRE'],m['NB_POINTS'],m['ANGLE'],m['DNOR']))
      elif m['TYPE']=='GROUP_NO':
        ngrno=m['GROUP_NO'].ljust(8)
        collgrno=aster.getcolljev(n_mailla.ljust(8)+'.GROUPENO')
        if ngrno not in collgrno.keys() :
          UTMESS('F','POST0_13',valk=[ngrno,n_mailla])
        grpn=collgrno[ngrno]
        l_coor_group=[ngrno,]
        for node in grpn:
          l_coor_group.append(aster.getvectjev(n_mailla.ljust(8)+'.COORDO    .VALE',3*(node-1),3))
        groups.append(l_coor_group)
      elif m['TYPE']=='GROUP_MA':
        ngrma=m['GROUP_MA'].ljust(8)
        if ngrma not in collgrma.keys() :
          UTMESS('F','POST0_14',valk=[ngrma,n_mailla])
        grpm=collgrma[ngrma]
        for ma in grpm:
          if ltyma[typma[ma-1]-1][:3] != 'SEG' :
             nomma=aster.getvectjev(n_mailla.ljust(8)+'.NOMMAI')
             UTMESS('F','POST0_15',valk=[ngrma,nomma[ma-1]])
        __mailla=CREA_MAILLAGE(MAILLAGE= m['MAILLAGE'],COPIE=_F(),)
        __mailla=DEFI_GROUP( reuse=__mailla,MAILLAGE= __mailla,
                            CREA_GROUP_NO=_F(OPTION='NOEUD_ORDO',NOM=str(m['GROUP_MA']),GROUP_MA=m['GROUP_MA']),)
        collgrno=aster.getcolljev(__mailla.nom.ljust(8)+'.GROUPENO')
        grpn=collgrno[str(m['GROUP_MA']).ljust(8)]
        l_coor_group=[ngrma,]
        for node in grpn:
          l_coor_group.append(aster.getvectjev(n_mailla.ljust(8)+'.COORDO    .VALE',3*(node-1),3))
        groups.append(l_coor_group)


  if arcs!=[] and (lignes!=[] or groups!=[]) :
    UTMESS('F','POST0_16')

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

  # distance min entre 2 points de la ligne de coupe (utile pour PROJ_CHAMP)
  dmin=dist_min_deux_points(__macou)

  motscles={}
  iocc=1
  motscles['CREA_GROUP_NO']=[]
  for m in LIGN_COUPE :
      if m['TYPE'] in ('GROUP_NO','GROUP_MA') :
        motscles['CREA_GROUP_NO'].append(_F(GROUP_MA=m[m['TYPE']].ljust(8),) )
      else :
        motscles['CREA_GROUP_NO'].append(_F(GROUP_MA='LICOU'+str(iocc),) )
        iocc=iocc+1

  __macou=DEFI_GROUP( reuse =__macou , MAILLAGE=__macou , **motscles );

  if AsType(RESULTAT).__name__ in ('evol_elas','evol_noli') :
    __mocou=AFFE_MODELE(MAILLAGE=__macou,
                        AFFE=_F(TOUT='OUI',
                                PHENOMENE='MECANIQUE',
                                MODELISATION='BARRE',),
                                PARTITION=_F(PARALLELISME='CENTRALISE'),
                                );
  elif AsType(RESULTAT).__name__ in ('evol_ther',) :
    __mocou=AFFE_MODELE(MAILLAGE=__macou,
                        AFFE=_F(TOUT='OUI',
                                PHENOMENE='THERMIQUE',
                                MODELISATION='PLAN',),);

  motscles={}
  motscles['VIS_A_VIS']=[]
  motscles[mcORDR.keys()[0]]=mcORDR.values()[0]
  if args['VIS_A_VIS']!=None:
    for v in args['VIS_A_VIS']:
      if v['GROUP_MA_1']!=None:
         motscles['VIS_A_VIS'].append(_F(GROUP_MA_1 = v['GROUP_MA_1'],TOUT_2='OUI'),)
      elif v['MAILLE_1']!=None:
         motscles['VIS_A_VIS'].append(_F(MAILLE_1 = v['MAILLE_1'],TOUT_2='OUI'),)

  if n_modele in self.get_global_contexte().keys() : MODELE_1=self.get_global_contexte()[n_modele]
  else                                             : MODELE_1=self.jdc.current_context[n_modele]


  if NOM_CHAM[5:9]=='ELGA' : UTMESS('F','POST0_18',valk=[NOM_CHAM,])

  __recou=PROJ_CHAMP(METHODE='COLLOCATION',
                     RESULTAT=RESULTAT,
                     MODELE_1=MODELE_1,
                     DISTANCE_MAX=m['DISTANCE_MAX'],
                     MODELE_2=__mocou,
                     TYPE_CHAM='NOEU',
                     NOM_CHAM=NOM_CHAM, **motscles);

  __remodr=__recou
  icham=0
  ioc2=0
  mcACTION=[]
  angtab=[]

  if AsType(RESULTAT).__name__ in ('evol_elas','evol_noli') :

   if  NOM_CHAM in ('DEPL','SIEF_ELNO','SIGM_NOEU','SIGM_NOEU_ELGA','SICO_NOEU','SIGM_ELNO'):icham=1
   iocc=0
   for m in LIGN_COUPE :

     iocc=iocc+1
     motscles={}
     motscles['OPERATION']=m['OPERATION']
     if m['NOM_CMP']!=None:
       motscles['NOM_CMP']=m['NOM_CMP']
       if m['TRAC_NOR']!=None:
          motscles['TRAC_NOR']=m['TRAC_NOR']
       elif m['TRAC_DIR']!=None:
          motscles['TRAC_DIR']=m['TRAC_DIR']
          motscles['DIRECTION']=m['DIRECTION']
     elif m['INVARIANT']!=None:
       motscles['INVARIANT']=m['INVARIANT']
     elif m['RESULTANTE']!=None:
       motscles['RESULTANTE']=m['RESULTANTE']
     elif m['ELEM_PRINCIPAUX']!=None:
       motscles['ELEM_PRINCIPAUX']=m['ELEM_PRINCIPAUX']
     else:
       motscles['TOUT_CMP']='OUI'

     # on définit le groupe de noeud pour post_releve_t
     if m['TYPE'] in ('GROUP_NO','GROUP_MA'):
         groupe=m[m['TYPE']].ljust(8)
         nomgrma=groupe
     else:
         ioc2=ioc2+1
         groupe='LICOU'+str(ioc2)
         nomgrma=' '
         newgrp='LICOF'+str(ioc2)
         crea_grp_matiere(self,groupe,newgrp,iocc,m,__remodr,NOM_CHAM,LIGN_COUPE,__macou)
         groupe=newgrp

     # on definit l'intitulé
     if m['INTITULE'] !=None                    : intitl=m['INTITULE']
     elif  m['TYPE'] in ('GROUP_NO','GROUP_MA') : intitl=groupe
     else                                       : intitl='l.coupe'+str(ioc2)


     # Expression des contraintes aux noeuds ou des déplacements dans le repere local
     if m['REPERE'] != 'GLOBAL':

        if  icham==1:

          if m['REPERE']=='POLAIRE':
            mcACTION.append( _F(INTITULE  = intitl,
                            RESULTAT  = __remodr,
                            REPERE    = m['REPERE'],
                            GROUP_NO  = groupe,
                            NOM_CHAM  = NOM_CHAM,**motscles ),)
          else:
            __remodr=crea_resu_local(self,dime,NOM_CHAM,m,__recou,__macou,nomgrma)
            mcACTION.append( _F(INTITULE  = intitl,
                            RESULTAT  = __remodr,
                            GROUP_NO  = groupe,
                            NOM_CHAM  = NOM_CHAM,**motscles ),)

        else:
          UTMESS('A','POST0_17',valk=[NOM_CHAM,m['REPERE']])
          mcACTION.append( _F(INTITULE  = intitl,
                            RESULTAT  = __recou,
                            GROUP_NO  = groupe,
                            NOM_CHAM  = NOM_CHAM,**motscles ),)

     # Expression des contraintes aux noeuds ou des déplacements dans le repere global
     else:

          mcACTION.append( _F(INTITULE  = intitl,
                            RESULTAT  = __recou,
                            GROUP_NO  = groupe,
                            NOM_CHAM  = NOM_CHAM,**motscles ),)


  elif AsType(RESULTAT).__name__ in ('evol_ther',) :
     iocc=0
     for m in LIGN_COUPE :

        iocc=iocc+1
        motscles={}
        motscles['OPERATION']=m['OPERATION']
        if m['NOM_CMP']!=None:
          motscles['NOM_CMP']=m['NOM_CMP']
          if m['TRAC_NOR']!=None:
             motscles['TRAC_NOR']=m['TRAC_NOR']
          elif m['TRAC_DIR']!=None:
             motscles['TRAC_DIR']=m['TRAC_DIR']
             motscles['DIRECTION']=m['DIRECTION']
        elif m['INVARIANT']!=None:
          motscles['INVARIANT']=m['INVARIANT']
        elif m['RESULTANTE']!=None:
          motscles['RESULTANTE']=m['RESULTANTE']
        elif m['ELEM_PRINCIPAUX']!=None:
          motscles['ELEM_PRINCIPAUX']=m['ELEM_PRINCIPAUX']
        else:
          motscles['TOUT_CMP']='OUI'

        if m['TYPE'] not in ('GROUP_NO','GROUP_MA') :
          ioc2=ioc2+1
          groupe='LICOU'+str(ioc2)
          newgrp='LICOF'+str(ioc2)
          crea_grp_matiere(self,groupe,newgrp,iocc,m,__remodr,NOM_CHAM,LIGN_COUPE,__macou)
          groupe=newgrp
          if m['INTITULE'] !=None : intitl=m['INTITULE']
          else                    : intitl='l.coupe'+str(ioc2)
        else:
          groupe=m[m['TYPE']].ljust(8)
          if m['INTITULE'] !=None : intitl=m['INTITULE']
          else                    : intitl=groupe
        mcACTION.append( _F(INTITULE  = intitl,
                            RESULTAT  = __recou,
                            GROUP_NO  = groupe,
                            NOM_CHAM  = NOM_CHAM, **motscles ),)

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

  if 'RESU' in dictab.para:
    del dictab['RESU']
  if 'NOEUD' in dictab.para:
    del dictab['NOEUD']
  dprod = dictab.dict_CREA_TABLE()

  nomres=CREA_TABLE(**dprod)

  RetablirAlarme('CALCULEL2_63')
  RetablirAlarme('CALCULEL2_64')
  RetablirAlarme('MODELISA5_53')
  return ier
