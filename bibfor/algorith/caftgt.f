        SUBROUTINE CAFTGT(OPTION,MECA,THMC,THER,HYDR,
     >                     INMECA,INTHMC,INTHER,INHYDR,
     >                       IMATE,
     &                             NDIM,DIMDEF,DIMCON,NVIMEC,NVITH,
     &                             YAMEC,YAP1,NBPHA1,YAP2,NBPHA2,YATE,
     &                             ADDETE,ADDEME,ADDEP1,ADDEP2,ADCOTE,
     &                             CONGEM,CONGEP,
     &                             VINTM,VINTP,ADVIME,ADVITH,
     &                             DSDE,
     &                             EPSV,P1,P2,T,GRAT,PHI,SAT,RV0,
     &                             G1D,G1F,G1C,J1D,J1F,J1C,J2,J3,G2,G3)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 02/09/2002   AUTEUR UFBHHLL C.CHAVANT 
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
C TOLE CRP_20
C TOLE CRP_21
C **********************************************************************
C CLONE CE CALCFT POUR LES LOIS DU CERMES QUI N FIGURENT PLUS
C          DANS CALCFT
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
      REAL*8 T,GRAT(3),PHI,SAT,DSATP1
C
      REAL*8   DTALE,DTALP1,DTALP2,DTALT,TLANV,TLANW,TLANS
      REAL*8   G1D,G1F,G1C,J1D,J1F,J1C,J2,J3,G2,G3
      REAL*8   MAMOLG,MAMOLV,R,HFG,ATMP,RV0
C
C
      INTEGER ELA,CJS,ENLGAT,ESUGAT
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
CCCCC
      INTEGER      NDIFF,NHOM,NGAZ,NVAP,NINIG
      PARAMETER    (NDIFF=4,NHOM=4,NGAZ=3,NVAP=3,NINIG=2)
      REAL*8       VDIFF(NDIFF),HOM(NHOM),GAZ(NGAZ),VAP(NVAP)
      REAL*8       INIGAT(NINIG)
      CHARACTER*8  BGCR6(NDIFF),NCRA5(NHOM),NCRA3(NGAZ),NCRA4(NVAP)
      CHARACTER*8  BGCR3(NINIG)
      DATA NCRA5 / 'R_GAZ','RHO','CP','BIOT_COE' /
      DATA BGCR6 /'SIGMA_T','D_SIGMA_T','PERM_G_INTR','CHAL_VAPO'/
      DATA NCRA3 / 'MASS_MOL','CP','VISC' /
      DATA NCRA4 / 'MASS_MOL','CP','VISC' /
      DATA BGCR3 /'DEGR_SATU','PRES_ATMO'/
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
      VALPAR(1)=T
      VALPAR(3)=PHI
      IF ((THMC.EQ.'GAZ').OR.(THMC.EQ.'LIQU_SATU')
     &          .OR.(THMC.EQ.'LIQU_SATU_GAT')) THEN
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
         IF (THMC.EQ.'LIQU_NSAT_GAT')THEN
           TLANS=COND(10)
           TLANW=COND(12)
           CALL RCVALA(IMATE,'THM_DIFFU',NPAR,NOMPAR,VALPAR,
     &             NDIFF,BGCR6,VDIFF,CODRET,'FM')
C      
           HFG= VDIFF(4)
           CALL RCVALA(IMATE,'THM_DIFFU',1,'PCAP',P1,NHOM,
     &            NCRA5,HOM,CODRET,'FM')
C      
           R = HOM(1)
           CALL RCVALA(IMATE,'THM_VAPE_GAZ',1,'TEMP',T,NVAP,NCRA4,
     &               VAP,CODRET,'FM')
           CALL RCVALA(IMATE,'THM_GAZ',1,'TEMP',T,NGAZ,NCRA3,
     &               GAZ,CODRET,'FM')
            CALL RCVALA(IMATE,'THM_INIT',0,' ',0.D0,NINIG,BGCR3,
     &               INIGAT,CODRET,'FM')
            ATMP = INIGAT(2)
           MAMOLG=GAZ(1)
           MAMOLV=VAP(1)
           CALL QCONDT(PHI,SAT,T,P2,R,MAMOLV,MAMOLG,ATMP,J1F,J2,HFG
     &               ,TLANV,TLANW,DTALE,DTALP1,DTALP2,DTALT,J1D,RV0)
C
C         
           LAMBDT(1)=(1.D0-PHI)*TLANS+PHI*SAT*TLANW+PHI*(1.D0-SAT)*TLANV
           LAMBDT(2)=(SAT*TLANW-TLANS+(1.D0-SAT)*TLANV)*J1D
     &               +PHI*(TLANW-TLANV)*G1D+PHI*(1.D0-SAT)*DTALE
           LAMBDT(3)=(SAT*TLANW-TLANS+(1.D0-SAT)*TLANV)*(J1F-J2)
     &               +PHI*(TLANW-TLANV)*(G1F-G2)+PHI*(1.D0-SAT)*DTALP1
           LAMBDT(4)=(SAT*TLANW-TLANS+(1.D0-SAT)*TLANV)*(J2-J1F)
     &               +PHI*(TLANW-TLANV)*(G2-G1F)+PHI*(1.D0-SAT)*DTALP2
           LAMBDT(5)=(SAT*TLANW-TLANS+(1.D0-SAT)*TLANV)*(J3-J1C)
     &               +PHI*(TLANW-TLANV)*(G3-G1C)+PHI*(1.D0-SAT)*DTALT
         ELSE
C

          LAMBDT(1)=(1.D0-PHI)*COND(10)+PHI*SAT*COND(12)+
     &           PHI*(1.D0-SAT)*COND(14)         
          LAMBDT(2)=0.D0
          LAMBDT(3)=PHI*DSATP1*COND(12)-PHI*DSATP1*COND(14)
          LAMBDT(4)=0.D0
          LAMBDT(5)=(1.D0-PHI)*COND(11)+PHI*SAT*COND(13)+
     &           PHI*(1.D0-SAT)*COND(15)
         ENDIF  
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
