#@ MODIF macr_lign_coupe_ops Macro  DATE 25/05/2004   AUTEUR DURAND C.DURAND 
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

def crea_mail_lig_coup(lignes):

  import os,sys,copy

  try:
# construction du maillage au format Aster des segments de lignes de coupe

    nblig=len(lignes)
    dimension=len(lignes[0][0])

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
          noeud='  N'+str(nbno)+'   '+str(x)+'    '+str(y)+'    '+str(z)+'\n'
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
    resu=resu+'FIN\n'

    return resu

  except :
    return 0

########################################################################
def macr_lign_coupe_ops(self,RESULTAT,UNITE_MAILLAGE,LIGN_COUPE,MODELE,
                        NOM_CHAM,**args):
  """
     Ecriture de la macro MACR_LIGN_COUPE
  """
  import os
  from Accas import _F
  from Noyau.N_utils import AsType
  ier=0

  # On importe les definitions des commandes a utiliser dans la macro
  LIRE_MAILLAGE  =self.get_cmd('LIRE_MAILLAGE')
  DEFI_GROUP     =self.get_cmd('DEFI_GROUP')
  AFFE_MODELE    =self.get_cmd('AFFE_MODELE')
  PROJ_CHAMP     =self.get_cmd('PROJ_CHAMP')
  POST_RELEVE_T  =self.get_cmd('POST_RELEVE_T')

  # La macro compte pour 1 dans la numerotation des commandes
  #self.icmd=1
  self.set_icmd(1)

  lignes=[]
  for m in LIGN_COUPE :
      lignes.append((m['COOR_ORIG'],m['COOR_EXTR'],m['NB_POINTS']))

  # Création du maillage des NB_POINTS segments entre COOR_ORIG et COOR_EXTR
  # par appel au script python crea_mail_lig_coup
  # le maillage est ensuite recopié dans l unité logique UNITE_MAILLAGE

  resu_mail=crea_mail_lig_coup(lignes)
  cur_dir=os.getcwd()
  nomFichierSortie   =cur_dir+'/fort.'+str(UNITE_MAILLAGE)
  fproc=open(nomFichierSortie,'w')
  fproc.write(resu_mail)
  fproc.close()

  # Lecture du maillage de seg2 contenant toutes les lignes de coupe

  __macou=LIRE_MAILLAGE(UNITE=UNITE_MAILLAGE,);

  motscles={}
  iocc=1
  motscles['CREA_GROUP_NO']=[]
  for m in LIGN_COUPE :
      motscles['CREA_GROUP_NO'].append(_F(GROUP_MA='LICOU'+str(iocc),) )
      iocc=iocc+1
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
                     MODELE_1=MODELE,
                     MODELE_2=__mocou,
                     NOM_CHAM=NOM_CHAM,);

  # Production d'une table par ligne de coupe
  # Toutes les tables sont des concepts sortant de la macro définies
  # dans chaque occurence du mcfact lign_coupe

  iocc=1
  for m in LIGN_COUPE :
      self.DeclareOut('tt',m['TABLE'])
      tt=POST_RELEVE_T(ACTION=_F(INTITULE  = 'lig.coupe'+str(iocc),
                                 RESULTAT  = __recou,
                                 GROUP_NO  = 'LICOU'+str(iocc),
                                 NOM_CHAM  = NOM_CHAM,
                                 TOUT_CMP  = 'OUI',
                                 OPERATION = 'EXTRACTION', ),);
      iocc=iocc+1

  return ier
