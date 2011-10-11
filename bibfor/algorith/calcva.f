        SUBROUTINE CALCVA(  KPI,YACHAI,YAMEC, YATE, YAP1, YAP2,
     &                      DEFGEM, DEFGEP,
     +                      ADDEME, ADDEP1, ADDEP2, ADDETE, NDIM,
     +                      T0, P10, P20, DEPSV, EPSV, DEPS, T, P1, P2,
     +                      GRAT, GRAP1, GRAP2, DP1, DP2, DT, RETCOM )
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 11/10/2011   AUTEUR MEUNIER S.MEUNIER 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE GRANET S.GRANET
C ======================================================================
C TOLE CRP_21
C ======================================================================
C --- CALCUL DE VARIABLES (MECANIQUES, HYDRAULIQUES, THERMIQUES) -------
C ======================================================================
      IMPLICIT      NONE
      INTEGER       KPI,YAMEC, YATE, YAP1, YAP2
      LOGICAL       YACHAI
      INTEGER       ADDEME, ADDEP1, ADDEP2, ADDETE, NDIM, RETCOM
      REAL*8        DEFGEM(*), DEFGEP(*), T0, P10, P20
      REAL*8        DEPSV, EPSV, DEPS(6), T, P1, P2, DT, DP1, DP2
      REAL*8        GRAT(NDIM), GRAP1(NDIM), GRAP2(NDIM)
C ======================================================================
      INTEGER       I,IADZI,IAZK24,IRET1,IRET2
      CHARACTER*8   NOMAIL
      REAL*8        EPSVP,EPSVM
C ======================================================================
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C ======================================================================
C --- CALCUL DES DIFFERENTES VARIABLES QUELLE QUE SOIT L'OPTION --------
C ======================================================================
C --- VARIABLES MECANIQUES ---------------------------------------------
C ======================================================================
      DEPSV = 0.0D0
      EPSV  = 0.0D0
      CALL RCVARC(' ','DIVU','-','RIGI',KPI,1,EPSVM,IRET1)
      CALL RCVARC(' ','DIVU','+','RIGI',KPI,1,EPSVP,IRET2)
C
      YACHAI = (IRET1.EQ.0).AND.(IRET2.EQ.0)

C REMARQUE : YAMEC PEUT ETRE EGAL A 2
C            (ELEMENTS DE JOINTS HM)
      IF ((YAMEC.NE.0).AND.YACHAI) THEN
        CALL U2MESS('F','CHAINAGE_1')
      ELSEIF (IRET1.NE.IRET2) THEN
        CALL U2MESS('F','CHAINAGE_2')
      ENDIF
C
C 1ER CAS : ON A DE LA MECANIQUE ET ON EST EN TOTALEMENT COUPLE
C
      IF (YAMEC.EQ.1) THEN
         DO 100 I=1,6
            DEPS(I)=DEFGEP(ADDEME+NDIM-1+I)-DEFGEM(ADDEME+NDIM-1+I)
 100     CONTINUE
         DO 101 I=1,3
            DEPSV=DEPSV+DEFGEP(ADDEME+NDIM-1+I)-DEFGEM(ADDEME+NDIM-1+I)
 101     CONTINUE
         DO 102 I=1,3
            EPSV=EPSV+DEFGEP(ADDEME+NDIM-1+I)
 102     CONTINUE
      ENDIF
C
      IF (YACHAI) THEN
          DEPSV = EPSVP - EPSVM
          EPSV  = EPSVP
      ENDIF
C ======================================================================
C --- VARIABLES HYDRAULIQUES -------------------------------------------
C ======================================================================
      P1  = P10
      DP1 = 0.0D0
      P2  = P20
      DP2 = 0.0D0
      IF (YAP1.EQ.1) THEN
         P1=DEFGEP(ADDEP1)+P10
         DP1=DEFGEP(ADDEP1)-DEFGEM(ADDEP1)
         DO 103 I=1,NDIM
            GRAP1(I)=DEFGEP(ADDEP1+I)
 103     CONTINUE
         IF (YAP2.EQ.1) THEN
            P2=DEFGEP(ADDEP2)+P20
            DP2=DEFGEP(ADDEP2)-DEFGEM(ADDEP2)
            DO 104 I=1,NDIM
               GRAP2(I)=DEFGEP(ADDEP2+I)
 104        CONTINUE
         ENDIF
      ENDIF
C ======================================================================
C --- VARIABLES THERMIQUES ---------------------------------------------
C ======================================================================
      T  = T0
      DT = 0.0D0
      IF (YATE.EQ.1) THEN
         DT=DEFGEP(ADDETE)-DEFGEM(ADDETE)
         T=DEFGEP(ADDETE)+T0
         DO 105 I=1,NDIM
            GRAT(I)=DEFGEP(ADDETE+I)
 105     CONTINUE
         IF (T.LE.0.D0) THEN
            CALL TECAEL(IADZI,IAZK24)
            NOMAIL = ZK24(IAZK24-1+3) (1:8)
            CALL U2MESK('A','ELEMENTS5_41',1,NOMAIL)
            RETCOM = 1
         ENDIF
      ENDIF
C
      END
