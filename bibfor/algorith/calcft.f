        SUBROUTINE CALCFT(OPTION,MECA,THMC,THER,HYDR,IMATE,NDIM,DIMDEF,
     +                    DIMCON,NVIMEC,NVITH,YAMEC,YAP1,NBPHA1,YAP2,
     +                    NBPHA2,YATE,ADDETE,ADDEME,ADDEP1,ADDEP2,
     +                    ADCOTE,CONGEM,CONGEP,VINTM,VINTP,ADVIME,
     +                    ADVITH,DSDE,EPSV,P1,P2,T,GRAT,PHI,SATBID,PVP,
     +                    RGAZ, RHOD, CPD, BIOT, SATM, SAT,
     +                    DSATP1, PESA, PERMFH, PERMLI, DPERML, PERMGZ,
     +                    DPERMS, DPERMP, FICK, DFICKT, DFICKG, LAMBDD,
     +                    DLAMBD, RHOL, UNSURK, ALPHA, CPL, LAMBDL,
     +                    DLAMBL, VISCL, DVISCL, MAMOLG, CPG, LAMBDG,
     +                    DLAMBG,
     +                    VISCG, DVISCG, MAMOLV, CPVG, VISCVG, DVISVG,
     +                    RETCOM)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C ======================================================================
C MODIF ALGORITH  DATE 26/09/2003   AUTEUR DURAND C.DURAND 
C RESPONSABLE UFBHHLL C.CHAVANT
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C TOLE CRP_21
C ======================================================================
C ROUTINE CALC_FLUX_THERM ----------------------------------------------
C CALCULE LES CONTRAINTES GENERALISEES ET LA MATRICE TANGENTE DES FLUX -
C OUT RETCOM : RETOUR LOI DE COMPORTEMENT ------------------------------
C ======================================================================
      IMPLICIT      NONE
      INTEGER       NDIM,DIMDEF,DIMCON,NVIMEC,NVITH,IMATE
      INTEGER       YAMEC,YAP1,NBPHA1,YAP2,NBPHA2,YATE
      INTEGER       ADDETE,ADDEME,ADDEP1,ADDEP2,ADCOTE
      INTEGER       ADVIME,ADVITH,RETCOM
      REAL*8        CONGEM(1:DIMCON),CONGEP(1:DIMCON)
      REAL*8        VINTM(1:NVIMEC+NVITH),VINTP(1:NVIMEC+NVITH)
      REAL*8        DSDE(1:DIMCON,1:DIMDEF),EPSV,P1,P2,MAMOLV
      REAL*8        T,GRAT(3),PHI,SAT,DSATP1,PVP,LAMBDT(5),I,J,SATBID
      REAL*8        RGAZ, RHOD, CPD, BIOT, SATM, SATUR,DSATUR, PESA(3)
      REAL*8        PERMFH, PERMLI, DPERML, PERMGZ,DPERMS, DPERMP, FICK
      REAL*8        DFICKT, DFICKG, LAMBDD,DLAMBD, RHOL, UNSURK, ALPHA
      REAL*8        CPL, LAMBDL,DLAMBL, VISCL, DVISCL, CPG, LAMBDG
      REAL*8        DLAMBG,VISCG, DVISCG, MAMOLG, CPVG, VISCVG, DVISVG
      CHARACTER*16  OPTION,MECA,THMC,THER,HYDR
C ======================================================================
C --- SI THER_HOMO ----------------------------------------------------
C =====================================================================
      IF (THER.EQ.'THER_HOMO') THEN
         LAMBDT(1) = LAMBDD
         LAMBDT(2) = 0.0D0
         LAMBDT(3) = 0.0D0
         LAMBDT(4) = 0.0D0
         LAMBDT(5) = 0.0D0
      ELSE
C =====================================================================
C --- SI THER_POLY ----------------------------------------------------
C =====================================================================
         LAMBDT(1) = (1.D0-PHI)*LAMBDD+PHI*SAT*LAMBDL+
     +                                            PHI*(1.D0-SAT)*LAMBDG
         LAMBDT(2) = 0.D0
         LAMBDT(3) = PHI*DSATP1*LAMBDL-PHI*DSATP1*LAMBDG
         LAMBDT(4) = 0.D0
         LAMBDT(5) = (1.D0-PHI)*DLAMBD+PHI*SAT*DLAMBL+
     +                                            PHI*(1.D0-SAT)*DLAMBG
      ENDIF
C =====================================================================
C --- CALCUL DU FLUX THERMIQUE ----------------------------------------
C =====================================================================
      IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG') .OR.
     +    (OPTION(1:9).EQ.'FULL_MECA')            ) THEN
         DO 100 I=1,NDIM
            DSDE(ADCOTE+I,ADDETE+I)=DSDE(ADCOTE+I,ADDETE+I)-LAMBDT(1)
            DSDE(ADCOTE+I,ADDETE)=DSDE(ADCOTE+I,ADDETE)
     +                            - LAMBDT(5)*GRAT(I)
            IF (YAMEC.EQ.1) THEN
               DO 101 J=1,3
                  DSDE(ADCOTE+I,ADDEME+NDIM-1+J)=
     +              DSDE(ADCOTE+I,ADDEME+NDIM-1+J)-LAMBDT(2)*GRAT(I)
 101           CONTINUE    
            ENDIF
            IF (YAP1.EQ.1) THEN
               DSDE(ADCOTE+I,ADDEP1)=DSDE(ADCOTE+I,ADDEP1)
     +                            - LAMBDT(3)*GRAT(I)
               IF (YAP2.EQ.1) THEN
                  DSDE(ADCOTE+I,ADDEP2)=DSDE(ADCOTE+I,ADDEP2)
     +                               - LAMBDT(4)*GRAT(I)
               ENDIF
            ENDIF
 100     CONTINUE
      ENDIF
      IF ( (OPTION(1:9).EQ.'RAPH_MECA') .OR.
     +     (OPTION(1:9).EQ.'FULL_MECA')      ) THEN 
         DO 102 I=1,NDIM
            CONGEP(ADCOTE+I)=-LAMBDT(1)*GRAT(I)
 102     CONTINUE
      ENDIF
C =====================================================================
      END
