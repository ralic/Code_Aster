        SUBROUTINE CALCFT(OPTION,MECA,THMC,THER,HYDR,
     >                     INMECA,INTHMC,INTHER,INHYDR,
     >                       IMATE,
     &                             NDIM,DIMDEF,DIMCON,NVIMEC,NVITH,
     &                             YAMEC,YAP1,NBPHA1,YAP2,NBPHA2,YATE,
     &                             ADDETE,ADDEME,ADDEP1,ADDEP2,ADCOTE,
     &                             CONGEM,CONGEP,
     &                             VINTM,VINTP,ADVIME,ADVITH,
     &                             DSDE,
     &                             EPSV,P1,P2,T,GRAT,PHI,SAT,PVP,
     >                             RETCOM)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 02/09/2002   AUTEUR UFBHHLL C.CHAVANT 
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
C TOLE CRP_20
C TOLE CRP_21
C **********************************************************************
C ROUTINE CALC_FLUX_THERM
C CALCULE LES CONTRAINTES GENERALISEES ET LA MATRICE TANGENTE DES FLUX
C OUT RETCOM : RETOUR LOI DE COMPORTEMENT
C **********************************************************************
C
      IMPLICIT NONE
      INTEGER NDIM,DIMDEF,DIMCON,NVIMEC,NVITH,IMATE
      INTEGER YAMEC,YAP1,NBPHA1,YAP2,NBPHA2,YATE
      INTEGER ADDETE,ADDEME,ADDEP1,ADDEP2,ADCOTE
      INTEGER ADVIME,ADVITH
      CHARACTER*16 OPTION,MECA,THMC,THER,HYDR    
      INTEGER  INMECA,INTHMC,INTHER,INHYDR
C
      REAL*8 CONGEM(1:DIMCON),CONGEP(1:DIMCON)
      REAL*8 VINTM(1:NVIMEC+NVITH),VINTP(1:NVIMEC+NVITH)
      REAL*8 DSDE(1:DIMCON,1:DIMDEF)
C      
      REAL*8 EPSV,P1,P2
      REAL*8 T,GRAT(3),PHI,SAT,DSATP1,PVP
      INTEGER RETCOM
C
C
C
C
      INTEGER I,J,NSAT,NCON,NPAR,NRESMA
      INTEGER NCONU
      REAL*8 LAMBDT(5),CONDU
      PARAMETER (NSAT=2,NCON=15,NCONU=11,NPAR=4)
      PARAMETER (NRESMA=18)
      REAL*8      SATUR(NSAT),COND(NCON),VALPAR(NPAR)
      CHARACTER*8 NCRA3A(2)
      CHARACTER*2 CODRET(NRESMA)
      CHARACTER*8 NCRA1(NSAT),NCRA2(NCONU)
      CHARACTER*8 NOMPAR(NPAR)
CCCCC
      DATA NCRA1 / 'SATU_PRE','D_SATU_P' /
      DATA NCRA2 / 'PERM_IN','PERM_LIQ','D_PERM_L','PERM_GAZ',
     &             'D_PERM_S','D_PERM_P','FICK','D_FICK_T','D_FICK_G',
     &             'LAMBDA','D_LAMBDA' /
      DATA NCRA3A / 'LAMBDA','D_LAMBDA' /       
      DATA NOMPAR  / 'TEMP','SAT','PORO','PGAZ'/
C     
C
C **********************************************************************
C   RECUPERATION DE LAMBDAT ET DE SES DERIVEES 
C
      IF (((THMC.EQ.'LIQU_VAPE_GAZ').OR.(THMC.EQ.'LIQU_GAZ')).OR.
     &      (THMC.EQ.'LIQU_GAZ_ATM')) THEN
         IF (HYDR.EQ.'HYDR_UTIL') THEN
            CALL RCVALA(IMATE,'THM_DIFFU',1,'PCAP',P1,
     &            NSAT,NCRA1,SATUR,CODRET,'FM')
            SAT=SATUR(1)
            DSATP1=SATUR(2)            
         ELSE 
            CALL SATURA(HYDR,P1,SAT,DSATP1)
         ENDIF
      ENDIF
      IF (THMC.EQ.'LIQU_VAPE') THEN
         IF (HYDR.EQ.'HYDR_UTIL') THEN
            CALL RCVALA(IMATE,'THM_DIFFU',1,'PCAP',PVP-P1,
     &            NSAT,NCRA1,SATUR,CODRET,'FM')
            SAT=SATUR(1)
            DSATP1=SATUR(2)            
         ELSE 
            CALL SATURA(HYDR,PVP-P1,SAT,DSATP1)
         ENDIF
      ENDIF
      VALPAR(1)=T
      VALPAR(3)=PHI
      IF ((THMC.EQ.'GAZ').OR.(THMC.EQ.'LIQU_SATU')) THEN
         VALPAR(2)=1.D0
      ELSE
         VALPAR(2)=SAT
      ENDIF
      VALPAR(4)=P2
C
      IF (THER.EQ.'THER_HOMO') THEN
          CALL RCVALA(IMATE,'THM_DIFFU',1,'TEMP',T,
     &             1,'LAMBDA',CONDU,CODRET,'FM')
          LAMBDT(1)=CONDU
          LAMBDT(2)=0.D0
          LAMBDT(3)=0.D0
          LAMBDT(4)=0.D0
          LAMBDT(5)=0.D0
      ELSE
          CALL RCVALA(IMATE,'THM_DIFFU',NPAR,NOMPAR,VALPAR,
     &             NCONU,NCRA2,COND,CODRET,'FM')
          CALL RCVALA(IMATE,'THM_LIQU',NPAR,NOMPAR,VALPAR,
     &             2,NCRA3A,COND(12),CODRET,'FM')
          CALL RCVALA(IMATE,'THM_GAZ',NPAR,NOMPAR,VALPAR,
     &             2,NCRA3A,COND(14),CODRET,'FM')
C
C

          LAMBDT(1)=(1.D0-PHI)*COND(10)+PHI*SAT*COND(12)+
     &           PHI*(1.D0-SAT)*COND(14)         
          LAMBDT(2)=0.D0
          LAMBDT(3)=PHI*DSATP1*COND(12)-PHI*DSATP1*COND(14)
          LAMBDT(4)=0.D0
          LAMBDT(5)=(1.D0-PHI)*COND(11)+PHI*SAT*COND(13)+
     &           PHI*(1.D0-SAT)*COND(15)
      ENDIF  
C
C **********************************************************************
C CALCUL DU FLUX THERMIQUE
C

      IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &   (OPTION(1:9).EQ.'FULL_MECA')) THEN
         DO 100 I=1,NDIM
            DSDE(ADCOTE+I,ADDETE+I)=DSDE(ADCOTE+I,ADDETE+I)-LAMBDT(1)
            DSDE(ADCOTE+I,ADDETE)=DSDE(ADCOTE+I,ADDETE)
     &                            -LAMBDT(5)*GRAT(I)
            IF (YAMEC.EQ.1) THEN
               DO 101 J=1,3
                  DSDE(ADCOTE+I,ADDEME+NDIM-1+J)=
     &              DSDE(ADCOTE+I,ADDEME+NDIM-1+J)-LAMBDT(2)*GRAT(I)
 101           CONTINUE    
            ENDIF
            IF (YAP1.EQ.1) THEN
               DSDE(ADCOTE+I,ADDEP1)=DSDE(ADCOTE+I,ADDEP1)
     &                            -LAMBDT(3)*GRAT(I)
               IF (YAP2.EQ.1) THEN
                  DSDE(ADCOTE+I,ADDEP2)=DSDE(ADCOTE+I,ADDEP2)
     &                               -LAMBDT(4)*GRAT(I)
               ENDIF
            ENDIF
 100     CONTINUE
      ENDIF
C
C
      IF ((OPTION(1:9).EQ.'RAPH_MECA').OR.
     &   (OPTION(1:9).EQ.'FULL_MECA')) THEN 
         DO 102 I=1,NDIM
            CONGEP(ADCOTE+I)=-LAMBDT(1)*GRAT(I)
 102     CONTINUE
      ENDIF

C   
      END
