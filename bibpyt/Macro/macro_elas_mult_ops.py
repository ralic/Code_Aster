#@ MODIF macro_elas_mult_ops Macro  DATE 19/01/2010   AUTEUR PELLET J.PELLET 
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



def macro_elas_mult_ops(self,MODELE,CHAM_MATER,CARA_ELEM,NUME_DDL,
                        CHAR_MECA_GLOBAL,LIAISON_DISCRET,
                        CAS_CHARGE,SOLVEUR,**args):
  """
     Ecriture de la macro MACRO_ELAS_MULT
  """
  ier=0
  import types
  from Accas import _F
  from Utilitai.Utmess     import UTMESS

  # On met le mot cle NUME_DDL dans une variable locale pour le proteger
  numeddl=NUME_DDL
  # On importe les definitions des commandes a utiliser dans la macro
  CALC_MATR_ELEM  =self.get_cmd('CALC_MATR_ELEM')
  NUME_DDL        =self.get_cmd('NUME_DDL')
  ASSE_MATRICE    =self.get_cmd('ASSE_MATRICE')
  FACTORISER      =self.get_cmd('FACTORISER')
  CALC_VECT_ELEM  =self.get_cmd('CALC_VECT_ELEM')
  ASSE_VECTEUR    =self.get_cmd('ASSE_VECTEUR')
  RESOUDRE        =self.get_cmd('RESOUDRE')
  CREA_RESU       =self.get_cmd('CREA_RESU')
  CALC_ELEM       =self.get_cmd('CALC_ELEM')
  CALC_NO         =self.get_cmd('CALC_NO')
  # La macro compte pour 1 dans la numerotation des commandes
  self.set_icmd(1)

  # Le concept sortant (de type mult_elas ou fourier_elas) est nommé
  # 'nomres' dans le contexte de la macro

  self.DeclareOut('nomres',self.sd)

  ielas = 0
  ifour = 0
  for m in CAS_CHARGE:
     if m['NOM_CAS']:
        ielas=1                 # mot clé NOM_CAS      présent sous CAS_CHARGE
        tyresu = 'MULT_ELAS'
     else:
        ifour=1                 # mot clé MODE_FOURIER présent sous CAS_CHARGE
        tyresu = 'FOURIER_ELAS'
  if ielas==1 and ifour==1:
     UTMESS('F','ELASMULT0_1')

  if (numeddl in self.sdprods) or (numeddl==None):
    # Si le concept numeddl est dans self.sdprods ou n est pas nommé
    # il doit etre  produit par la macro
    # il faudra donc appeler la commande NUME_DDL
    lnume = 1
  else:
    lnume = 0

  if ielas==1 :
     motscles={}
     if   CHAR_MECA_GLOBAL: motscles['CHARGE']    =CHAR_MECA_GLOBAL
     if   CHAM_MATER      : motscles['CHAM_MATER']=CHAM_MATER
     if   CARA_ELEM       : motscles['CARA_ELEM'] =CARA_ELEM
     __nomrig=CALC_MATR_ELEM(OPTION='RIGI_MECA',MODELE=MODELE,**motscles)

     if lnume:
       # On peut passer des mots cles egaux a None. Ils sont ignores
       motscles={}
       if SOLVEUR:
          motscles['METHODE'] =SOLVEUR['METHODE']
          motscles['RENUM']   =SOLVEUR['RENUM']
       else:
          motscles['METHODE'] ='MULT_FRONT'
          motscles['RENUM']   ='METIS'
       if numeddl!=None:
          self.DeclareOut('num',numeddl)
          num=NUME_DDL(MATR_RIGI=__nomrig,**motscles)
       else:
          _num=NUME_DDL(MATR_RIGI=__nomrig,**motscles)
          num=_num
     else:
       num=numeddl

     __nomras=ASSE_MATRICE(MATR_ELEM=__nomrig,NUME_DDL=num)

     __nomraf=FACTORISER(MATR_ASSE=__nomras,NPREC=SOLVEUR['NPREC'],STOP_SINGULIER=SOLVEUR['STOP_SINGULIER'])

#####################################################################
# boucle sur les items de CAS_CHARGE

  nomchn=[]
  lcharg=[]
  iocc=0
  for m in CAS_CHARGE:
     iocc=iocc+1

     # calcul de lcharg : liste des listes de char_meca (mots clé CHAR_MECA et CHAR_MECA_GLOBAL)
     xx1=m['CHAR_MECA']
     if type(xx1) != type((1,)) : xx1=(xx1,)
     xx2=CHAR_MECA_GLOBAL
     if type(xx2) != type((1,)) : xx2=(xx2,)
     lchar1=[]
     for chargt in (xx1+xx2) :
        if chargt : lchar1.append(chargt)
     lcharg.append(lchar1)
     assert len(lchar1) > 0



     if ifour:
        motscles={}
        if   CHAR_MECA_GLOBAL: motscles['CHARGE']       =CHAR_MECA_GLOBAL
        if   CHAM_MATER      : motscles['CHAM_MATER']   =CHAM_MATER
        if   CARA_ELEM       : motscles['CARA_ELEM']    =CARA_ELEM
        motscles['MODE_FOURIER'] =m['MODE_FOURIER']
        __nomrig=CALC_MATR_ELEM(OPTION='RIGI_MECA',MODELE=MODELE,**motscles)

        if lnume:
           _num=NUME_DDL(MATR_RIGI=__nomrig,METHODE=SOLVEUR['METHODE'],RENUM=SOLVEUR['RENUM'])
           num=_num
           lnume=0

        __nomras=ASSE_MATRICE(MATR_ELEM=__nomrig,NUME_DDL=num)

        __nomraf=FACTORISER(MATR_ASSE=__nomras,NPREC=SOLVEUR['NPREC'],STOP_SINGULIER=SOLVEUR['STOP_SINGULIER'])


     if m['VECT_ASSE']==None :
        motscles={}
        if   CHAM_MATER      : motscles['CHAM_MATER']   =CHAM_MATER
        if   CARA_ELEM       : motscles['CARA_ELEM']    =CARA_ELEM
        if   ifour           : motscles['MODE_FOURIER'] =m['MODE_FOURIER']
        if   len(lchar1) > 0 : motscles['CHARGE']       =lchar1
        __nomvel=CALC_VECT_ELEM(OPTION='CHAR_MECA',**motscles)
        __nomasv=ASSE_VECTEUR(VECT_ELEM=__nomvel,NUME_DDL=num)
     else :
        __nomasv=m['VECT_ASSE']


     __nomchn=RESOUDRE(MATR=__nomraf,CHAM_NO=__nomasv,TITRE=m['SOUS_TITRE'])
     nomchn.append(__nomchn)

# fin de la boucle sur les items de CAS_CHARGE
#####################################################################

  motscles={}
  iocc=0
  motscle2={}
  if CHAM_MATER : motscle2['CHAM_MATER']=CHAM_MATER
  if CARA_ELEM  : motscle2['CARA_ELEM']=CARA_ELEM
  if ielas :
     motscles['AFFE']=[]
     for m in CAS_CHARGE:
        if len(lcharg[iocc]) > 0 :
           motscles['AFFE'].append(_F(MODELE=MODELE,
                                      CHAM_GD=nomchn[iocc],
                                      NOM_CAS=m['NOM_CAS'],
                                      CHARGE=lcharg[iocc],
                                      **motscle2) )
        else :
           motscles['AFFE'].append(_F(MODELE=MODELE,
                                      CHAM_GD=nomchn[iocc],
                                      NOM_CAS=m['NOM_CAS'],
                                      **motscle2) )
        iocc=iocc+1
  else :
     motscles['AFFE']=[]
     for m in CAS_CHARGE:
        if len(lcharg[iocc]) > 0 :
           motscles['AFFE'].append(_F(MODELE=MODELE,
                                      CHAM_GD=nomchn[iocc],
                                      NUME_MODE=m['MODE_FOURIER'],
                                      TYPE_MODE=m['TYPE_MODE'],
                                      CHARGE=lcharg[iocc],
                                      **motscle2) )
        else :
           motscles['AFFE'].append(_F(MODELE=MODELE,
                                      CHAM_GD=nomchn[iocc],
                                      NUME_MODE=m['MODE_FOURIER'],
                                      TYPE_MODE=m['TYPE_MODE'],
                                      **motscle2) )
        iocc=iocc+1


  nomres=CREA_RESU(OPERATION='AFFE',TYPE_RESU=tyresu,NOM_CHAM='DEPL',**motscles)

#####################################################################
# boucle sur les items de CAS_CHARGE pour CALC_ELEM et CALC_NO

  iocc=0
  for m in CAS_CHARGE:
     iocc=iocc+1

     if m['OPTION']:
        nbel=0
        nbno=0
        liste_el=[]
        liste_no=[]
        if type(m['OPTION'])==types.StringType:
           liste_option=(m['OPTION'],)
        else :
           liste_option=m['OPTION']
        for option in liste_option :
           if option in (     'FORC_NODA','REAC_NODA',
                              'EPSI_NOEU_DEPL','SIGM_NOEU_DEPL','EFGE_NOEU_DEPL',
                              'EQUI_NOEU_SIGM','EQUI_NOEU_EPSI',):
              nbno=nbno+1
              liste_no.append(option)
           else:
              nbel=nbel+1
              liste_el.append(option)

        if nbel:
           motscles={}
           if ielas:
              motscles['NOM_CAS']=m['NOM_CAS']
           else:
              motscles['NUME_MODE']=m['MODE_FOURIER']
           CALC_ELEM(reuse=nomres,
                     RESULTAT=nomres,
                     REPE_COQUE=_F(NIVE_COUCHE=m['NIVE_COUCHE'],
                                   NUME_COUCHE=m['NUME_COUCHE'],),
                     OPTION=tuple(liste_el),
                     **motscles)
        if nbno:
           motscles={}
           if ielas:
              motscles['NOM_CAS']=m['NOM_CAS']
           else:
              motscles['NUME_MODE']=m['MODE_FOURIER']
           CALC_NO(reuse=nomres,
                   RESULTAT=nomres,
                   OPTION=tuple(liste_no),
                     **motscles)

# fin de la boucle sur les items de CAS_CHARGE
#####################################################################
  return ier

