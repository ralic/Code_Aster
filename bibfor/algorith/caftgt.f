        SUBROUTINE CAFTGT(OPTION,THMC,HYDR,IMATE,NDIM,DIMDEF,
     +                  DIMCON,NVIMEC,NVITH,YAMEC,YAP1,YAP2,
     +                  ADDETE,ADDEME,ADDEP1,ADDEP2,ADCOTE,
     +                  CONGEP,DSDE,P1,T,GRAT,SAT)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 06/04/2004   AUTEUR DURAND C.DURAND 
C RESPONSABLE UFBHHLL C.CHAVANT
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_21
C **********************************************************************
C CLONE CE CALCFT POUR LES LOIS DU CERMES QUI N FIGURENT PLUS
C          DANS CALCFT
C **********************************************************************
C
      IMPLICIT NONE
      INTEGER NDIM,DIMDEF,DIMCON,NVIMEC,NVITH,IMATE
      INTEGER YAMEC,YAP1,YAP2
      INTEGER ADDETE,ADDEME,ADDEP1,ADDEP2,ADCOTE
      CHARACTER*16 OPTION,THMC,HYDR    
C
      REAL*8 CONGEP(1:DIMCON)
      REAL*8 DSDE(1:DIMCON,1:DIMDEF)  
      REAL*8 P1
      REAL*8 T,GRAT(3),SAT,DSATP1
      REAL*8   DTALE,DTALP1,DTALP2,DTALT,TLANV,TLANW,TLANS
      REAL*8   MAMOLG,MAMOLV,R,HFG,ATMP
C
C
      INTEGER ELA,CJS,ENLGAT,ESUGAT
C
      INTEGER I,J,NSAT,NCON,NRESMA
      REAL*8 LAMBDT(5),CONDU
      PARAMETER (NSAT=2,NCON=15)
      PARAMETER (NRESMA=18)
      REAL*8      SATUR(NSAT),COND(NCON)
      CHARACTER*2 CODRET(NRESMA)
      CHARACTER*8 NCRA1(NSAT)
CCCCC
CCCCC
      INTEGER      NDIFF,NHOM,NGAZ,NVAP,NINIG
      PARAMETER    (NDIFF=4,NHOM=4,NGAZ=3,NVAP=3,NINIG=2)
      REAL*8       VDIFF(NDIFF),HOM(NHOM),GAZ(NGAZ),VAP(NVAP)
      REAL*8       INIGAT(NINIG)
CCCCC
      DATA NCRA1 / 'SATU_PRE','D_SATU_P' /
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
C
         CALL RCVALA(IMATE,'THM_DIFFU',1,'TEMP',T,
     &             1,'LAMBDA',CONDU,CODRET,'FM')
         LAMBDT(1)=CONDU
         LAMBDT(2)=0.D0
         LAMBDT(3)=0.D0
         LAMBDT(4)=0.D0
         LAMBDT(5)=0.D0
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
