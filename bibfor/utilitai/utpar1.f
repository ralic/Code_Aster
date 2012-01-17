      SUBROUTINE UTPAR1(TYPSD,NBPAMX,LIPARA,NBPARA)
      IMPLICIT NONE
      CHARACTER*(*) TYPSD
      INTEGER NBPAMX,NBPARA
      CHARACTER*32 LIPARA(NBPAMX)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 16/01/2012   AUTEUR BEAURAIN J.BEAURAIN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.
C
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.
C
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE PELLET J.PELLET
C ----------------------------------------------------------------------
C BUT : CHOISIR LA LISTE DES PARAMETRES SUPPORTES PAR UN TYPE DE
C       SD_RESULTAT
C-----------------------------------------------------------------------
C IN  TYPSD  : TYPE DE LA STRUCTURE "RESULTAT"
C IN  NBPAMX : DIMENSION DE LIPARA
C OUT NBPARA : NOMBRE DE PARAMETRES
C OUT LIPARA : NOMS DES PARAMETRES AVEC LEUR TYPE ET LEUR TYPE D'ACCES.
C              TYPE=R/C/I/K8/K16/K24/K32/K80
C              ACCES= /A (ACCES) /P (PARAMETRE)
C              EXEMPLES : 'INST#A#R', 'CARAELEM#P#K8', ...
C ----------------------------------------------------------------------
      INTEGER ICO

C ----------------------------------------------------------------------
      ICO=0



      IF (TYPSD.EQ.'EVOL_THER') THEN
C     --------------------------------
        ICO=ICO+1
        LIPARA(ICO)='INST#A#R'
        ICO=ICO+1
        LIPARA(ICO)='MODELE#P#K8'
        ICO=ICO+1
        LIPARA(ICO)='CHAMPMAT#P#K8'
        ICO=ICO+1
        LIPARA(ICO)='CARAELEM#P#K8'
        ICO=ICO+1
        LIPARA(ICO)='EXCIT#P#K24'
        ICO=ICO+1
        LIPARA(ICO)='ITER_GLOB#P#I'
        ICO=ICO+1
        LIPARA(ICO)='ITER_LINE#P#I'
        ICO=ICO+1
        LIPARA(ICO)='RESI_GLOB_RELA#P#R'
        ICO=ICO+1
        LIPARA(ICO)='RESI_GLOB_MAXI#P#R'
        ICO=ICO+1
        LIPARA(ICO)='RHO#P#R'
        ICO=ICO+1
        LIPARA(ICO)='PARM_THETA#P#R'
        ICO=ICO+1
        LIPARA(ICO)='DELTAT#P#R'
        NBPARA=ICO
        CALL ASSERT(NBPARA.LE.NBPAMX)



      ELSEIF (TYPSD.EQ.'ACOU_HARMO') THEN
C     --------------------------------
        ICO=ICO+1
        LIPARA(ICO)='CARAELEM#P#K8'
        ICO=ICO+1
        LIPARA(ICO)='CHAMPMAT#P#K8'
        ICO=ICO+1
        LIPARA(ICO)='EXCIT#P#K24'
        ICO=ICO+1
        LIPARA(ICO)='FREQ#A#R'
        ICO=ICO+1
        LIPARA(ICO)='MODELE#P#K8'
        NBPARA=ICO
        CALL ASSERT(NBPARA.LE.NBPAMX)



      ELSEIF (TYPSD.EQ.'DYNA_HARMO') THEN
C     --------------------------------
        ICO=ICO+1
        LIPARA(ICO)='CARAELEM#P#K8'
        ICO=ICO+1
        LIPARA(ICO)='CHAMPMAT#P#K8'
        ICO=ICO+1
        LIPARA(ICO)='EXCIT#P#K24'
        ICO=ICO+1
        LIPARA(ICO)='FREQ#A#R'
        ICO=ICO+1
        LIPARA(ICO)='MODELE#P#K8'
        NBPARA=ICO
        CALL ASSERT(NBPARA.LE.NBPAMX)



      ELSEIF (TYPSD.EQ.'DYNA_TRANS') THEN
C     --------------------------------
        ICO=ICO+1
        LIPARA(ICO)='CARAELEM#P#K8'
        ICO=ICO+1
        LIPARA(ICO)='CHAMPMAT#P#K8'
        ICO=ICO+1
        LIPARA(ICO)='EXCIT#P#K24'
        ICO=ICO+1
        LIPARA(ICO)='INST#A#R'
        ICO=ICO+1
        LIPARA(ICO)='MODELE#P#K8'
        NBPARA=ICO
        CALL ASSERT(NBPARA.LE.NBPAMX)



      ELSEIF (TYPSD.EQ.'EVOL_ELAS') THEN
C     --------------------------------
        ICO=ICO+1
        LIPARA(ICO)='CARAELEM#P#K8'
        ICO=ICO+1
        LIPARA(ICO)='CHAMPMAT#P#K8'
        ICO=ICO+1
        LIPARA(ICO)='DEFO_D_DRY_X#P#R'
        ICO=ICO+1
        LIPARA(ICO)='DEFO_D_DRZ_X#P#R'
        ICO=ICO+1
        LIPARA(ICO)='DEFO_D_DX_X#P#R'
        ICO=ICO+1
        LIPARA(ICO)='EFFORT_D_VY_X#P#R'
        ICO=ICO+1
        LIPARA(ICO)='EFFORT_D_VZ_X#P#R'
        ICO=ICO+1
        LIPARA(ICO)='EFFORT_N#P#R'
        ICO=ICO+1
        LIPARA(ICO)='EFFORT_VY#P#R'
        ICO=ICO+1
        LIPARA(ICO)='EFFORT_VZ#P#R'
        ICO=ICO+1
        LIPARA(ICO)='ERREUR_ERRE#P#R'
        ICO=ICO+1
        LIPARA(ICO)='ERREUR_ERZ1#P#R'
        ICO=ICO+1
        LIPARA(ICO)='ERREUR_ERZ2#P#R'
        ICO=ICO+1
        LIPARA(ICO)='ERREUR_QIRE#P#R'
        ICO=ICO+1
        LIPARA(ICO)='ERREUR_QIZ1#P#R'
        ICO=ICO+1
        LIPARA(ICO)='ERREUR_QIZ2#P#R'
        ICO=ICO+1
        LIPARA(ICO)='EXCIT#P#K24'
        ICO=ICO+1
        LIPARA(ICO)='INST#A#R'
        ICO=ICO+1
        LIPARA(ICO)='ITER_GCPC#P#I'
        ICO=ICO+1
        LIPARA(ICO)='METHODE#P#K16'
        ICO=ICO+1
        LIPARA(ICO)='MODELE#P#K8'
        ICO=ICO+1
        LIPARA(ICO)='MOMENT_D_MT_X#P#R'
        ICO=ICO+1
        LIPARA(ICO)='MOMENT_MFY#P#R'
        ICO=ICO+1
        LIPARA(ICO)='MOMENT_MFZ#P#R'
        ICO=ICO+1
        LIPARA(ICO)='MOMENT_MT#P#R'
        ICO=ICO+1
        LIPARA(ICO)='RENUM#P#K16'
        ICO=ICO+1
        LIPARA(ICO)='RESI_GCPC#P#R'
        ICO=ICO+1
        LIPARA(ICO)='STOCKAGE#P#K16'
        NBPARA=ICO
        CALL ASSERT(NBPARA.LE.NBPAMX)



      ELSEIF (TYPSD.EQ.'EVOL_NOLI') THEN
C     --------------------------------
        ICO=ICO+1
        LIPARA(ICO)='INST#A#R'
        ICO=ICO+1
C       -- FREQ ET CHAR_CRIT NE PEUTVENT  PAS ETRE UNE VARIABLE D'ACCES 
C       A CAUSE DE LRIDEA.F (TEST ZZZZ165A)
        LIPARA(ICO)='FREQ#P#R'
        ICO=ICO+1
        LIPARA(ICO)='CHAR_CRIT#P#R'
C
        ICO=ICO+1
        LIPARA(ICO)='CHAR_STAB#P#R'
C
        ICO=ICO+1
        LIPARA(ICO)='CARAELEM#P#K8'
        ICO=ICO+1
        LIPARA(ICO)='CHAMPMAT#P#K8'
        ICO=ICO+1
        LIPARA(ICO)='MODELE#P#K8'
        ICO=ICO+1
        LIPARA(ICO)='PARM_THETA#P#R'
        ICO=ICO+1
        LIPARA(ICO)='EXCIT#P#K24'
C
        ICO=ICO+1
        LIPARA(ICO)='ERRE_HYD_D#P#R'
        ICO=ICO+1
        LIPARA(ICO)='ERRE_HYD_GLOB#P#R'
        ICO=ICO+1
        LIPARA(ICO)='ERRE_HYD_LOC#P#R'
        ICO=ICO+1
        LIPARA(ICO)='ERRE_HYD_S#P#R'
        ICO=ICO+1
        LIPARA(ICO)='ERRE_MEC#P#R'
        ICO=ICO+1
        LIPARA(ICO)='ERRE_MEC_GLOB#P#R'
        ICO=ICO+1
        LIPARA(ICO)='ERRE_MEC_GLOB_D#P#R'
        ICO=ICO+1
        LIPARA(ICO)='ERRE_MEC_LOC#P#R'
        ICO=ICO+1
        LIPARA(ICO)='ERRE_MEC_LOC_D#P#R'
        ICO=ICO+1
        LIPARA(ICO)='ERRE_TPS_GLOB#P#R'
        ICO=ICO+1
        LIPARA(ICO)='ERRE_TPS_LOC#P#R'
        ICO=ICO+1
        LIPARA(ICO)='ERREUR_ERRE#P#R'
        ICO=ICO+1
        LIPARA(ICO)='ERREUR_ERZ1#P#R'
        ICO=ICO+1
        LIPARA(ICO)='ERREUR_ERZ2#P#R'
        ICO=ICO+1
        LIPARA(ICO)='ERREUR_QIRE#P#R'
        ICO=ICO+1
        LIPARA(ICO)='ERREUR_QIZ1#P#R'
        ICO=ICO+1
        LIPARA(ICO)='ERREUR_QIZ2#P#R'
        ICO=ICO+1
        LIPARA(ICO)='ETA_PILOTAGE#P#R'
        ICO=ICO+1
        LIPARA(ICO)='ITER_GLOB#P#I'
        ICO=ICO+1
        LIPARA(ICO)='CHAR_MINI#P#R'       
        ICO=ICO+1
        LIPARA(ICO)='TRAN_GENE_NOLI#P#K24'
        ICO=ICO+1
        LIPARA(ICO)='COEF_MULT#P#R'                
        NBPARA=ICO
        CALL ASSERT(NBPARA.LE.NBPAMX)



      ELSEIF (TYPSD.EQ.'COMB_FOURIER') THEN
C     --------------------------------
        ICO=ICO+1
        LIPARA(ICO)='ANGL#A#R'
        ICO=ICO+1
        LIPARA(ICO)='MODELE#P#K8'
        ICO=ICO+1
        LIPARA(ICO)='CHAMPMAT#P#K8'
        ICO=ICO+1
        LIPARA(ICO)='CARAELEM#P#K8'
        ICO=ICO+1
        LIPARA(ICO)='EXCIT#P#K24'
        NBPARA=ICO
        CALL ASSERT(NBPARA.LE.NBPAMX)



      ELSEIF (TYPSD.EQ.'THETA_GEOM') THEN
C     --------------------------------
C       -- XXX : PARAMETRE BIDON POUR QU'IL Y EN AIT UN
        ICO=ICO+1
        LIPARA(ICO)='XXX#P#K8'
        NBPARA=ICO
        CALL ASSERT(NBPARA.LE.NBPAMX)



      ELSEIF (TYPSD.EQ.'MODE_FLAMB') THEN
C     --------------------------------
        ICO=ICO+1
        LIPARA(ICO)='CARAELEM#P#K8'
        ICO=ICO+1
        LIPARA(ICO)='CHAMPMAT#P#K8'
        ICO=ICO+1
        LIPARA(ICO)='CHAR_CRIT#P#R'
        ICO=ICO+1
        LIPARA(ICO)='EXCIT#P#K24'
        ICO=ICO+1
        LIPARA(ICO)='MODELE#P#K8'
        ICO=ICO+1
        LIPARA(ICO)='NORME#P#K24'
        ICO=ICO+1
        LIPARA(ICO)='NUME_MODE#A#I'
        NBPARA=ICO
        CALL ASSERT(NBPARA.LE.NBPAMX)


        
      ELSEIF (TYPSD.EQ.'MODE_STAB') THEN
C     --------------------------------
        ICO=ICO+1
        LIPARA(ICO)='CARAELEM#P#K8'
        ICO=ICO+1
        LIPARA(ICO)='CHAMPMAT#P#K8'
        ICO=ICO+1
        LIPARA(ICO)='CHAR_STAB#P#R'
        ICO=ICO+1
        LIPARA(ICO)='EXCIT#P#K24'
        ICO=ICO+1
        LIPARA(ICO)='MODELE#P#K8'
        ICO=ICO+1
        LIPARA(ICO)='NORME#P#K24'
        ICO=ICO+1
        LIPARA(ICO)='NUME_MODE#A#I'
        NBPARA=ICO
        CALL ASSERT(NBPARA.LE.NBPAMX)              
        


      ELSEIF ((TYPSD.EQ.'MODE_MECA').OR.
     &        (TYPSD.EQ.'MODE_MECA_C').OR.
     &        (TYPSD.EQ.'MODE_GENE').OR.
     &        (TYPSD.EQ.'MODE_ACOU')) THEN
C     --------------------------------
        ICO=ICO+1
        LIPARA(ICO)='AMOR_GENE#P#R'
        ICO=ICO+1
        LIPARA(ICO)='AMOR_REDUIT#P#R'
        ICO=ICO+1
        LIPARA(ICO)='CARAELEM#P#K8'
        ICO=ICO+1
        LIPARA(ICO)='CHAMPMAT#P#K8'
        ICO=ICO+1
        LIPARA(ICO)='COEF_X#P#R'
        ICO=ICO+1
        LIPARA(ICO)='COEF_Y#P#R'
        ICO=ICO+1
        LIPARA(ICO)='COEF_Z#P#R'
        ICO=ICO+1
        LIPARA(ICO)='EXCIT#P#K24'
        ICO=ICO+1
        LIPARA(ICO)='FACT_PARTICI_DX#P#R'
        ICO=ICO+1
        LIPARA(ICO)='FACT_PARTICI_DY#P#R'
        ICO=ICO+1
        LIPARA(ICO)='FACT_PARTICI_DZ#P#R'
        ICO=ICO+1
        LIPARA(ICO)='FREQ#A#R'
        ICO=ICO+1
        LIPARA(ICO)='MASS_EFFE_DX#P#R'
        ICO=ICO+1
        LIPARA(ICO)='MASS_EFFE_DY#P#R'
        ICO=ICO+1
        LIPARA(ICO)='MASS_EFFE_DZ#P#R'
        ICO=ICO+1
        LIPARA(ICO)='MASS_EFFE_UN_DX#P#R'
        ICO=ICO+1
        LIPARA(ICO)='MASS_EFFE_UN_DY#P#R'
        ICO=ICO+1
        LIPARA(ICO)='MASS_EFFE_UN_DZ#P#R'
        ICO=ICO+1
        LIPARA(ICO)='MASS_GENE#P#R'
        ICO=ICO+1
        LIPARA(ICO)='MODELE#P#K8'
        ICO=ICO+1
        LIPARA(ICO)='NORME#P#K24'
        ICO=ICO+1
        LIPARA(ICO)='NUME_DDL#P#I'
        ICO=ICO+1
        LIPARA(ICO)='NUME_MODE#A#I'
        ICO=ICO+1
        LIPARA(ICO)='NOEUD_CMP#A#K16'
        ICO=ICO+1
        LIPARA(ICO)='OMEGA2#P#R'
        ICO=ICO+1
        LIPARA(ICO)='RIGI_GENE#P#R'
        ICO=ICO+1
        LIPARA(ICO)='TYPE_DEFO#P#K16'
        ICO=ICO+1
        LIPARA(ICO)='TYPE_MODE#P#K16'
        NBPARA=ICO
        CALL ASSERT(NBPARA.LE.NBPAMX)



      ELSEIF (TYPSD.EQ.'MULT_ELAS') THEN
C     --------------------------------
        ICO=ICO+1
        LIPARA(ICO)='CARAELEM#P#K8'
        ICO=ICO+1
        LIPARA(ICO)='CHAMPMAT#P#K8'
        ICO=ICO+1
        LIPARA(ICO)='EXCIT#P#K24'
        ICO=ICO+1
        LIPARA(ICO)='MODELE#P#K8'
        ICO=ICO+1
        LIPARA(ICO)='NOM_CAS#A#K16'
        NBPARA=ICO
        CALL ASSERT(NBPARA.LE.NBPAMX)



      ELSEIF ((TYPSD.EQ.'FOURIER_ELAS').OR.
     &        (TYPSD.EQ.'FOURIER_THER')) THEN
C     -----------------------------------------
        ICO=ICO+1
        LIPARA(ICO)='NUME_MODE#A#I'
        ICO=ICO+1
        LIPARA(ICO)='TYPE_MODE#P#K8'
        ICO=ICO+1
        LIPARA(ICO)='MODELE#P#K8'
        ICO=ICO+1
        LIPARA(ICO)='CHAMPMAT#P#K8'
        ICO=ICO+1
        LIPARA(ICO)='CARAELEM#P#K8'
        ICO=ICO+1
        LIPARA(ICO)='EXCIT#P#K24'
        NBPARA=ICO
        CALL ASSERT(NBPARA.LE.NBPAMX)



      ELSEIF (TYPSD.EQ.'HARM_GENE') THEN
C     --------------------------------
        ICO=ICO+1
        LIPARA(ICO)='FREQ#A#R'
        NBPARA=ICO
        CALL ASSERT(NBPARA.LE.NBPAMX)



      ELSEIF (TYPSD.EQ.'EVOL_CHAR') THEN
C     --------------------------------
        ICO=ICO+1
        LIPARA(ICO)='INST#A#R'
        ICO=ICO+1
        LIPARA(ICO)='MODELE#P#K8'
        ICO=ICO+1
        LIPARA(ICO)='CARAELEM#P#K8'
        ICO=ICO+1
        LIPARA(ICO)='CHAMPMAT#P#K8'
        ICO=ICO+1
        LIPARA(ICO)='EXCIT#P#K24'
        NBPARA=ICO
        CALL ASSERT(NBPARA.LE.NBPAMX)



      ELSEIF (TYPSD.EQ.'EVOL_VARC') THEN
C     --------------------------------
        ICO=ICO+1
        LIPARA(ICO)='INST#A#R'
        ICO=ICO+1
        LIPARA(ICO)='MODELE#P#K8'
        ICO=ICO+1
        LIPARA(ICO)='CARAELEM#P#K8'
        ICO=ICO+1
        LIPARA(ICO)='CHAMPMAT#P#K8'
        ICO=ICO+1
        LIPARA(ICO)='EXCIT#P#K24'
        NBPARA=ICO
        CALL ASSERT(NBPARA.LE.NBPAMX)


      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF


C     -- POUR EVITER UNE LISTE VIDE :
C     -------------------------------
      IF (NBPARA.EQ.0) THEN
         NBPARA=1
         LIPARA(1)='XXX#P#R'
      ENDIF



      END
