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
    from Utilitai.signal_correlation_utils import (CALC_COHE, 
                              get_group_nom_coord, calc_dist2)
    from Cata.cata import (
     _F, DETRUIRE, LIRE_IMPE_MISS, LIRE_FORC_MISS, CREA_CHAMP, COMB_MATR_ASSE,
     DYNA_LINE_HARM
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

    GROUP_NO_INTER=INTERF['GROUP_NO_INTERF']
    # MAILLAGE NUME_DDL
    nom_bamo = MATR_GENE['BASE']
    resultat = MATR_GENE['BASE']
    nom_bam2 = nom_bamo.nom
      #iret,ibid,nume_ddl = aster.dismoi('NUME_DDL',nom_bamo.nom,'RESU_DYNA','F')
    iret, ibid, nume_ddl = aster.dismoi('NUME_DDL',nom_bam2,'RESU_DYNA','F')
    iret,ibid, nom_mail = aster.dismoi('NOM_MAILLA',nume_ddl,'NUME_DDL','F')
    # MODELE, DDLGENE

    nume_ddlgene = MATR_GENE['NUME_DDL_GENE']
  #   iret,ibid,nom_modele = aster.dismoi('NOM_MODELE',nume_ddl,'NUME_DDL','F')
 #    nbnot, nbl, nbma, nbsm, nbsmx, dime = maillage.DIME.get()
    # coordonnees des noeuds
 #    l_coordo = maillage.COORDO.VALE.get()
 #    t_coordo = NP.array(l_coordo)
 #    t_coordo.shape = nbnot, 3

    l_nom, noe_interf = get_group_nom_coord(
                     GROUP_NO_INTERF, nom_mail)

 #   del nume_ddl, nom_mail, nom_modele
       # MODES
    iret,nbmodd,kbid=aster.dismoi('NB_MODES_DYN', nom_bam2,'RESULTAT','F')
    iret,nbmods,kbid=aster.dismoi('NB_MODES_STA', nom_bam2,'RESULTAT','F')
    iret,nbmodt,kbid=aster.dismoi('NB_MODES_TOT',nom_bam2,'RESULTAT','F')
    FSIST = NP.zeros((NB_FREQ,nbmodt))+0j
    nbno, nbval = noe_interf.shape


    if INFO==2:
       texte = 'NOMBRE DE MODES: '+str(nbmodt)+'   MODES DYNAMIQUES: '+str(nbmodd)+'   MODES STATIQUES: '+str(nbmods)
       aster.affiche('MESSAGE',texte)
       aster.affiche('MESSAGE','COMPOSANTE '+NOM_CMP)
       aster.affiche('MESSAGE','NBNO INTERFACE : '+str(nbno))



     
    #  POUR TRANS, on sort le champ en déplacement: c'est équivalent du champ en deplacement si on applique un signal en ACCE

    # ----- boucle sur les modes statiques
    for mods in range(0,nbmods):
       nmo = nbmodd+mods+1
       __CHAM=CREA_CHAMP( TYPE_CHAM='NOEU_DEPL_R',
             OPERATION = 'EXTR',
             NUME_ORDRE = nmo,
             RESULTAT = resultat  ,
             NOM_CHAM = 'DEPL'
               );
       MCMP2 = __CHAM.EXTR_COMP(' ',[GROUP_NO_INTER],0).valeurs
       if mods == 0:
           NCMP2 =__CHAM.EXTR_COMP(' ',[GROUP_NO_INTER], topo=1).comp
           nddi = len(MCMP2)
           PHI = NP.zeros((nddi,nbmods))
       PHI[:,mods]=MCMP2

    PHIT=NP.transpose(PHI)
    PPHI=NP.dot(PHIT, PHI)
     


     # MODEL fonction de cohérence
    MODEL = MATR_COHE['TYPE']
    print 'MODEL :',   MODEL
    Data_Cohe = {}
    Data_Cohe['TYPE'] = MODEL
    Data_Cohe['MAILLAGE'] = nom_mail
    Data_Cohe['GROUP_NO_INTERF'] = noe_interf
    Data_Cohe['DIST'] = calc_dist2(noe_interf)
    if MODEL == 'MITA_LUCO':
        Data_Cohe['VITE_ONDE'] = MATR_COHE['VITE_ONDE']
        Data_Cohe['PARA_ALPHA'] = MATR_COHE['PARA_ALPHA']
     #---------------------------------------------------------------------
       # BOUCLE SUR LES FREQUENCES
    for k in range(NB_FREQ):
      freqk=FREQ_INIT+PAS*k
      if INFO==2:
          aster.affiche('MESSAGE','FREQUENCE DE CALCUL: '+str(freqk))
      COHE = CALC_COHE(freqk*2.*pi, **Data_Cohe)


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
         PVEC[k1, 0:nbno] = NP.sqrt(eig[k1])*vec[k1]



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
      FSISM = __fosi.EXTR_VECT_GENE_C()
      FS0 = FSISM[nbmodd:nbmodt][:]    #  extraction de la partie modes interface
      U0 = NP.dot(linalg.inv(KRS), FS0)
      # projection pour obtenir UO en base physique
      XI = NP.dot(PHI, U0)   
      XPI = XI
#      # facteur de correction pour tous les modes (on somme) >> c'est faux
#      SI0 = 0.0
#      for k1 in range(0, nbme):
#          XOe = abs(NP.sum(PVEC[k1])) / nbno 
#          SI0 = SI0 + XOe**2   
#      SI = sqrt(SI0)
#      for idd in range(0,nddi):#nddi: nombre de ddl interface
#          if NCMP2[idd][0:2] == NOM_CMP:
#              XPI[idd] = SI * XI[idd]
#      # retour en base modale
#      QPI = NP.dot(PHIT, XPI)    
#      U0 = NP.dot(linalg.inv(PPHI), QPI)            
#      FS = NP.dot(KRS, U0)

#      # retour en base modale
#      QPI = NP.dot(PHIT, XI) 

# # TEST
#      XSI=[]
#      for k1 in range(0, nbme):
#          XPI = XI
#          for idd in range(0,nddi):    #nddi: nombre de ddl interface
#              if NCMP2[idd][0:2] == NOM_CMP:
#                  XPI[idd] = PVEC[k1][idd]
#          XSI.append(XPI)
#          
#      # retour en base modale
#      QPI = NP.dot(PHIT, NP.sum(XSI,axis=0)) #      QPI = NP.dot(PHIT, XI)    
#      U0 = NP.dot(linalg.inv(PPHI), QPI)            
#      FS = NP.dot(KRS, U0)
#      FS = FS + FSK

#  TEST
      FSISM[nbmodd:nbmodt][:] =FS
      if imod==3 :
         SP = NP.zeros((nbmodt, nbmodt))
      if imod>1 :
            #  Calcul harmonique
          __fosi.RECU_VECT_GENE_C(FSISM)
          if MATR_GENE['MATR_AMOR'] is not None :
            __dyge = DYNA_LINE_HARM(
                MATR_MASS=MATR_GENE['MATR_MASS'],
                MATR_RIGI=__rito,
                FREQ=freqk,
                MATR_AMOR=MATR_GENE['MATR_AMOR'],
                EXCIT=_F(VECT_ASSE_GENE=__fosi,
                         COEF_MULT=1.0,), 
                  )
          else :
            __dyge = DYNA_LINE_HARM(
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

      FSIST[k,nbmods:nbmodt] = FSISM[0:nbmodd][:]
      FSIST[k,0:nbmods] = FS

# k : frequences

    if imod==1 :           
      return FSIST
    if imod==2 :           
      return VEC
    if imod==3 :           
      return SPEC
