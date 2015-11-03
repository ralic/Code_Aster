# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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

def force_iss_vari(self,imod,MATR_GENE,NOM_CMP,ISSF,INFO,UNITE_RESU_FORC,
         UNITE_RESU_IMPE,PRECISION,INTERF,MATR_COHE,TYPE,fini,PAS,fmax):
    """Force sismique variable en ISS"""
    import os
    import numpy as NP
    from numpy import linalg
    from math import pi, ceil, sqrt, floor, log, tanh
    import aster_core
    import aster
    from Accas import _F
    from Utilitai.Table import Table
    from Utilitai.Utmess import  UTMESS
    from Cata.cata import (
     _F, DETRUIRE, LIRE_IMPE_MISS, LIRE_FORC_MISS, CREA_CHAMP, COMB_MATR_ASSE,
     DYNA_VIBRA
    )
    #--------------------------------------------------------------------------------
    NB_FREQ = 1+int((fmax-fini)/PAS)
    FREQ_INIT = fini
    from SD.sd_maillage      import sd_maillage
    from SD.sd_nume_ddl_gd   import sd_nume_ddl_gd
    from SD.sd_nume_ddl_gene import sd_nume_ddl_gene
    from SD.sd_mode_meca import sd_mode_meca
    from SD.sd_resultat import sd_resultat
    from SD.sd_cham_gene import sd_cham_gene

    # MAILLAGE NUME_DDL
    if imod==1 : 
      nom_bamo = MATR_GENE['BASE']
      resultat = MATR_GENE['BASE']
      nom_bam2 = nom_bamo.nom
      #iret,ibid,nume_ddl = aster.dismoi('NUME_DDL',nom_bamo.nom,'RESU_DYNA','F')
    elif imod>1 :
      v_refa_rigi = MATR_GENE['MATR_RIGI'].sdj.REFA.get()
      v_refa_mass = MATR_GENE['MATR_MASS'].sdj.REFA.get()
      nom_bamo = v_refa_rigi[0]
      nom_bam2 = nom_bamo
    iret, ibid, nume_ddl = aster.dismoi('NUME_DDL',nom_bam2,'RESU_DYNA','F')
    iret,ibid,nom_mail = aster.dismoi('NOM_MAILLA',nume_ddl,'NUME_DDL','F')
    maillage = sd_maillage(nom_mail)
    # MODELE, DDLGENE
    if imod==1 :
      nume_ddlgene = MATR_GENE['NUME_DDL_GENE']
    else :
      nom_ddlgene = v_refa_rigi[1]
      nume_ddlgene = self.get_concept(nom_ddlgene)
      resultat=self.get_concept(nom_bam2)
    iret,ibid,nom_modele = aster.dismoi('NOM_MODELE',nume_ddl,'NUME_DDL','F')
    nbnot, nbl, nbma, nbsm, nbsmx, dime = maillage.DIME.get()
    # coordonnees des noeuds
    l_coordo = maillage.COORDO.VALE.get()
    t_coordo = NP.array(l_coordo)
    t_coordo.shape = nbnot, 3

    del nume_ddl, nom_mail, nom_modele
       # MODES
    iret,nbmodd,kbid=aster.dismoi('NB_MODES_DYN', nom_bam2,'RESULTAT','F')
    iret,nbmods,kbid=aster.dismoi('NB_MODES_STA', nom_bam2,'RESULTAT','F')
    iret,nbmodt,kbid=aster.dismoi('NB_MODES_TOT',nom_bam2,'RESULTAT','F')

    if INFO==2:
       texte = 'NOMBRE DE MODES: '+str(nbmodt)+'   MODES DYNAMIQUES: '+str(nbmodd)+'   MODES STATIQUES: '+str(nbmods)
       aster.affiche('MESSAGE',texte)
       aster.affiche('MESSAGE','COMPOSANTE '+NOM_CMP)

    FSIST = NP.zeros((NB_FREQ,nbmodt))+0j
    if imod==2 :
      VEC = NP.zeros((NB_FREQ, nbmodt)) + 0j
    if imod==3 :
      SPEC = NP.zeros((NB_FREQ, nbmodt, nbmodt)) + 0j
    
    coll_grno = maillage.GROUPENO.get()
    GROUP_NO_INTER=INTERF['GROUP_NO_INTERF']
    group = GROUP_NO_INTER 
    l_ind = NP.array(coll_grno.get('%-24s' % group, [])) - 1
    noe_interf = NP.take(t_coordo, l_ind, axis=0)
    nbno, nbval = noe_interf.shape
    if INFO==2:
       aster.affiche('MESSAGE','NBNO INTERFACE : '+str(nbno))
     
      #  POUR TRANS, on sort le champ en déplacement: c'est équivalent du champ en deplacement si on applique un signal en ACCE

    __CHAM=CREA_CHAMP( TYPE_CHAM='NOEU_DEPL_R',
             OPERATION='EXTR',
             NUME_ORDRE=nbmodd+1,
             RESULTAT = resultat  ,
             NOM_CHAM = 'DEPL'
               );
    MCMP =__CHAM.EXTR_COMP(NOM_CMP,[GROUP_NO_INTER]).valeurs #  on recupere la composante COMP (dx,dy,dz) des modes

    NNO =__CHAM.EXTR_COMP(NOM_CMP,[GROUP_NO_INTER], topo=1).noeud     

    MCMP2=__CHAM.EXTR_COMP(' ',[GROUP_NO_INTER],0).valeurs

    NCMP2 =__CHAM.EXTR_COMP(' ',[GROUP_NO_INTER], topo=1).comp

    nddi=len(MCMP2)
    PHI=NP.zeros((nddi,nbmods))
    # ----- boucle sur les modes statiques
    for mods in range(0,nbmods):
       nmo = nbmodd+mods+1
       __CHAM=CREA_CHAMP( TYPE_CHAM='NOEU_DEPL_R',
             OPERATION='EXTR',
             NUME_ORDRE=nmo,
             RESULTAT = resultat  ,
             NOM_CHAM = 'DEPL'
               );
       MCMP =__CHAM.EXTR_COMP(NOM_CMP,[GROUP_NO_INTER]).valeurs #  on recupere la composante COMP (dx,dy,dz) des modes

       NNO =__CHAM.EXTR_COMP(NOM_CMP,[GROUP_NO_INTER], topo=1).noeud
       MCMP2=__CHAM.EXTR_COMP(' ',[GROUP_NO_INTER],0).valeurs
       PHI[:,mods]=MCMP2

    PHIT=NP.transpose(PHI)
    PPHI=NP.dot(PHIT, PHI)
     
     # MODEL fonction de cohérence
    MODEL = MATR_COHE['TYPE']
    print 'MODEL :',   MODEL

     #---------------------------------------------------------------------
       # BOUCLE SUR LES FREQUENCES
    for k in range(NB_FREQ):
      freqk=FREQ_INIT+PAS*k
      if INFO==2:
          aster.affiche('MESSAGE','FREQUENCE DE CALCUL: '+str(freqk))
     #       # Matrice de coherence
      XX=noe_interf[:,0]
      YY=noe_interf[:,1]
     #
      if MODEL=='MITA_LUCO' :
       # PARAMETRES fonction de cohérence
          VITE_ONDE = MATR_COHE['VITE_ONDE']
          alpha = MATR_COHE['PARA_ALPHA']
     # # ----MITA & LUCO
          XN=NP.repeat(XX,nbno)
          YN=NP.repeat(YY,nbno)
          XR=NP.reshape(XN,(nbno,nbno))
          YR=NP.reshape(YN,(nbno,nbno))
          XRT=NP.transpose(XR)
          YRT=NP.transpose(YR)
          DX=XR-XRT
          DY=YR-YRT
          DIST=DX**2+DY**2
          COHE=NP.exp(-(DIST*(alpha*freqk*2*pi/VITE_ONDE)**2.))

      elif MODEL=='ABRAHAMSON' :
     #----ABRAHAMSON (EPRI)
          p_a1=1.647
          p_a2=1.01
          p_a3=0.4
          p_n1=7.02
    #    p_n2=1.685
          COHE=NP.zeros((nbno,nbno))
          for no1 in range(nbno):
             for no2 in range(nbno):
               dist_xi=sqrt((XX[no1]-XX[no2])**2+(YY[no1]-YY[no2])**2)
               p_n2=5.1-0.51*log(dist_xi+10.)
               pfc=-1.886+2.221*log(4000./(dist_xi+1.)+1.5)
               term1=1.+(freqk*tanh(p_a3*dist_xi)/(p_a1*pfc))**p_n1
               term2=1.+(freqk*tanh(p_a3*dist_xi)/(p_a2*pfc))**p_n2
               COHE[no1,no2]=1./sqrt(term1* term2)
       #---------------------------------------------------------
       # On desactive temporairement les FPE qui pourraient etre generees (a tord!) par blas
      aster_core.matfpe(-1)
      eig, vec =linalg.eig(COHE)
      vec = NP.transpose(vec)   # les vecteurs sont en colonne dans numpy
      aster_core.matfpe(1)
      eig=eig.real
      vec=vec.real
      # on rearrange selon un ordre decroissant
      eig = NP.where(eig < 1.E-10, 0.0, eig)
      order = (NP.argsort(eig)[::-1])
      eig = NP.take(eig, order)
      vec = NP.take(vec, order, 0)
      #-----------------------
      # Nombre de modes POD a retenir
      etot=NP.sum(eig**2)
      ener=0.0
      nbme=0
      while nbme < nbno:
         ener= eig[nbme]**2+ener
         prec=ener/etot
         nbme=nbme+1
         if INFO==2:
            aster.affiche('MESSAGE','VALEUR PROPRE  '+str(nbme)+' : '+str(eig[nbme-1]))
         if prec > PRECISION :
            break

      if INFO==2:
         aster.affiche('MESSAGE','NOMBRE DE MODES POD RETENUS : '+str(nbme))
         aster.affiche('MESSAGE','PRECISION (ENERGIE RETENUE) : '+str(prec))

      PVEC=NP.zeros((nbme,nbno))
      for k1 in range(0,nbme):
         PVEC[k1, 0:nbno]=NP.sqrt(eig[k1])*vec[k1]

      XOe=NP.zeros(nbme)
      for k1 in range(0,nbme):
        XOe[k1]=abs(NP.sum(PVEC[k1]))/nbno
 
      # CALCUL DE FS variable-------------------------------
      XO=NP.zeros((nbme,nbmods))
      if NOM_CMP=='DX':
         COMP = 1
      elif NOM_CMP=='DY':
         COMP = 2
      elif NOM_CMP=='DZ':
         COMP = 3
       #   ------------------------MODES interface-------------------------------------
           # ----- boucle sur les modes statiques
      for mods in range(0,nbmods):
         if INTERF['MODE_INTERF'] =='QUELCONQUE':
           break
         nmo = nbmodd+mods+1
         __CHAM=CREA_CHAMP( TYPE_CHAM='NOEU_DEPL_R',
                     OPERATION='EXTR',
                     NUME_ORDRE=nmo,
                     RESULTAT = resultat  ,
                     NOM_CHAM = 'DEPL'
                           );
         MCMP =__CHAM.EXTR_COMP(NOM_CMP,[GROUP_NO_INTER]).valeurs #  on recupere la composante COMP (dx,dy,dz) des modes

         NNO =__CHAM.EXTR_COMP(NOM_CMP,[GROUP_NO_INTER], topo=1).noeud

         som=NP.sum(MCMP)
         max1=NP.max(MCMP)
         min1=NP.min(MCMP)
         maxm=NP.max([abs(max1),abs(min1)])
      #-------------CALCUL DE XO -------------
      #  on a recupere la composante COMP (dx,dy,dz) des modes et on projete
         #  CAS 1: MODES DE CORPS RIGIDE
         if INTERF['MODE_INTERF'] =='CORP_RIGI':
            for modp in range(0,nbme):
               #pour les modes de translation
               if mods+1 <=3:
                  if abs(som)<10.E-6:
                     XO[modp,mods]=0.0
                  else :
                     fact=1./som
                     XO[modp,mods]=fact*abs(NP.inner(MCMP,PVEC[modp]))
                    #modes de rotation
               else:
                  if maxm<10.E-6:
                     if som<10.E-6:
                        XO[modp,mods]=0.0
                     else :
                        UTMESS('F','ALGORITH6_86')
                  else :
                     fact = 1./(nbno)
                     XO[modp,mods]=1./(maxm**2.)*fact*abs(NP.inner(MCMP,PVEC[modp]))
         # CAS 2: MODES EF
         if INTERF['MODE_INTERF'] =='TOUT':
            for modp in range(0,nbme):
               if abs(som)<10.E-6:
                  if maxm<10.E-6:
                     XO[modp,mods]=0.0
                  else:
                     UTMESS('F','UTILITAI5_89')
               else:
                  fact=1./som
                  XO[modp,mods]=fact*abs(NP.inner(MCMP,PVEC[modp]))

         DETRUIRE(CONCEPT=_F(NOM=(__CHAM)),INFO=1)

        #----Impedances + force sismique.-----------------------------------------------------------------
      if k>0:
         DETRUIRE(CONCEPT=_F(NOM=(__impe,__fosi)),INFO=1)
         if imod>1 :
            DETRUIRE(CONCEPT=_F(NOM=(__rito)),INFO=1)

      __impe = LIRE_IMPE_MISS(BASE=resultat,
                                TYPE=TYPE,
                                NUME_DDL_GENE=nume_ddlgene,
                                UNITE_RESU_IMPE= UNITE_RESU_IMPE,
                                ISSF=ISSF,
                                FREQ_EXTR=freqk,
                                );
           #    on cree __fosi  pour  RECU_VECT_GENE_C   plus loin

      __fosi = LIRE_FORC_MISS(BASE=resultat,
                                NUME_DDL_GENE=nume_ddlgene,
                                NOM_CMP=NOM_CMP,
                                NOM_CHAM='DEPL',
                                UNITE_RESU_FORC = UNITE_RESU_FORC,
                                ISSF=ISSF,
                                FREQ_EXTR=freqk,);
                                
      if imod>1 :
        __rito = COMB_MATR_ASSE(COMB_C=(
                                _F(MATR_ASSE=__impe,
                                   COEF_C=1.0 + 0.j,),
                                _F(MATR_ASSE=MATR_GENE['MATR_RIGI'],
                                   COEF_C=1.0 + 0.j,),
                                ),
                                SANS_CMP='LAGR',
                                )

      # -------------- impedance--------------------------------
      MIMPE=__impe.EXTR_MATR_GENE()
      #  extraction de la partie modes interface
      KRS = MIMPE[nbmodd:nbmodt,nbmodd:nbmodt]

     # -------------- force sismique-------------------------------
      FSISM=__fosi.EXTR_VECT_GENE_C()

      FS0=FSISM[nbmodd:nbmodt][:]    #  extraction de la partie modes interface

      if ISSF=='OUI'  :
         if INTERF['MODE_INTERF'] =='CORP_RIGI':
            U0=NP.dot(linalg.inv(KRS), FS0)
            XOe=XO[:,COMP-1]*U0[COMP-1]
            XO[:,COMP-1]=XOe
            U0[COMP-1]=0.0+0j
            for k1 in range(0,nbme):
               U0[COMP-1]=U0[COMP-1]+XOe[k1]

      if INTERF['MODE_INTERF'] =='QUELCONQUE':
         U0=NP.dot(linalg.inv(KRS), FS0)
         XI=NP.dot(PHI, U0)
         XPI=XI
         SI0=0.0
         for k1 in range(0,nbme):
            SI0=SI0+XOe[k1]*XOe[k1]
         SI = sqrt(SI0)
         for idd in range(0,nddi):
            if NCMP2[idd][0:2] == NOM_CMP:
              XPI[idd]=SI*XI[idd]
         QPI=NP.dot(PHIT, XPI)
         U0=NP.dot(linalg.inv(PPHI), QPI)
              
      if INTERF['MODE_INTERF']=='QUELCONQUE' or ISSF=='OUI' :
         FS = NP.dot(KRS, U0)
      else:
           #   force sismique resultante: somme des mode POD
         XO_s=NP.sum(XO,0)
         FS = NP.dot(KRS,XO_s)

      FSISM[nbmodd:nbmodt][:] =FS
      if imod==3 :
         SP = NP.zeros((nbmodt, nbmodt))
      if imod>1 :
        if INTERF['MODE_INTERF']=='QUELCONQUE' or imod==2 :
            #  Calcul harmonique
          __fosi.RECU_VECT_GENE_C(FSISM)
          if MATR_GENE['MATR_AMOR'] is not None :
            __dyge = DYNA_VIBRA(TYPE_CALCUL='HARM', BASE_CALCUL='GENE',
                MATR_MASS=MATR_GENE['MATR_MASS'],
                MATR_RIGI=__rito,
                FREQ=freqk,
                MATR_AMOR=MATR_GENE['MATR_AMOR'],
                EXCIT=_F(VECT_ASSE_GENE=__fosi,
                         COEF_MULT=1.0,), 
                  )
          else :
            __dyge = DYNA_VIBRA(TYPE_CALCUL='HARM', BASE_CALCUL='GENE',
                MATR_MASS=MATR_GENE['MATR_MASS'],
                MATR_RIGI=__rito,
                FREQ=freqk,
                EXCIT=_F(VECT_ASSE_GENE=__fosi,
                         COEF_MULT=1.0,), 
                  )
         #  recuperer le vecteur modal depl calcule par dyge
          RS = NP.array(__dyge.sdj.DEPL.get())
          DETRUIRE(CONCEPT=_F(NOM=(__dyge)), INFO=1)
          if imod==2 :
            VEC[k] = RS
          elif imod==3 :
            SP = RS * NP.conj(RS[:, NP.newaxis])
            SPEC[k] = SP
            
        else :
          for k1 in range(0, nbme):
            #  calcul de la force sismique mode POD par mode POD
            FS = NP.dot(KRS, XO[k1])
            print 'TEST XO', XO[k1]
            print 'TEST mode POD', k1, 'FS', FS 
            FSISM[nbmodd:nbmodt][:] = FS
            #  Calcul harmonique
            __fosi.RECU_VECT_GENE_C(FSISM)
            if MATR_GENE['MATR_AMOR'] is not None :
              __dyge = DYNA_VIBRA(TYPE_CALCUL='HARM', BASE_CALCUL='GENE',
                MATR_MASS=MATR_GENE['MATR_MASS'],
                MATR_RIGI=__rito,
                FREQ=freqk,
                MATR_AMOR=MATR_GENE['MATR_AMOR'],
                EXCIT=_F(VECT_ASSE_GENE=__fosi,
                         COEF_MULT=1.0,), 
                  )
            else :
              __dyge = DYNA_VIBRA(TYPE_CALCUL='HARM', BASE_CALCUL='GENE',
                MATR_MASS=MATR_GENE['MATR_MASS'],
                MATR_RIGI=__rito,
                FREQ=freqk,
                EXCIT=_F(VECT_ASSE_GENE=__fosi,
                         COEF_MULT=1.0,), 
                  )
            #  recuperer le vecteur modal depl calcule par dyge
            RS = NP.array(__dyge.sdj.DEPL.get())
            DETRUIRE(CONCEPT=_F(NOM=(__dyge)), INFO=1)
            # stockage des matrices résultats: sum(s_q s_q* )
            SP = SP + RS * NP.conj(RS[:, NP.newaxis])
            SPEC[k] = SP
      FSIST[k,nbmods:nbmodt] = FSISM[0:nbmodd][:]
      FSIST[k,0:nbmods] = FS

    if imod==1 :           
      return FSIST
    if imod==2 :           
      return VEC
    if imod==3 :           
      return SPEC
