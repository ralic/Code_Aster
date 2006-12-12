      SUBROUTINE CHVERI(NP1,NP2,NP3,NBM,NBMCD,
     &                  NBNL,TYPCH,NBSEG,PHII,NOECHO,
     &                  ALPHA,BETA,GAMMA,ORIG,RC,THETA,DEPG)
C
      IMPLICIT NONE
C
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 13/12/2006   AUTEUR PELLET J.PELLET 
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
C-----------------------------------------------------------------------
C VERIFICATION DE LA BONNE CONFIGURATION AUX NOEUX DE CHOC:
C  - NORMALE AU PLAN DE CHOC ET DIRECTION DU TUBE,
C  - POSITION INITIALE DU NOEUD DE CHOC, PAR RAPPORT AU JEU.
C-----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
C
C ARGUMENTS
      INTEGER      NP1, NP2, NP3
      INTEGER      NBM,NBMCD
      INTEGER      NBNL, TYPCH(*), NBSEG(*)
      REAL*8       PHII(NP2,NP1,3),DEPG(*)
      REAL*8       RC(NP3,*), THETA(NP3,*)
      REAL*8       ORIG(6,*)
      REAL*8       ALPHA(2,*), BETA(2,*), GAMMA(2,*)
      CHARACTER*8  NOECHO(NP2,*)
      CHARACTER*24 VALK(2)
C
C VARIABLES LOCALES
      INTEGER      IC, TYPOBS, NBS, INO1, JCOOR1, I, J, K, NBNO, IER
      REAL*8       XLOC(3), XGLO(3)
      REAL*8       XJEU, SINT, COST, DNORM
      REAL*8       XORIG(3),XORIV(3),SINA,COSA,SINB,COSB,SING,COSG
      REAL*8       EPSI,COOR(3,3),VECT(3),TOT
      CHARACTER*3  INUM
      CHARACTER*8  K8B, MAILLA, NOMNOE
      CHARACTER*32 JEXNOM,  JEXNUM
C
C ****************** DEBUT DU CODE EXECUTABLE ************************
C
      CALL JEMARQ()
      EPSI = 1.D-8
      XORIV(1) = 0.D0
      XORIV(2) = 0.D0
      XORIV(3) = 0.D0
C
      DO 10 I = 1,NBM
         DEPG(I) = 0.D0
  10  CONTINUE
C
C
C 1. BOUCLE SUR LES NON-LINEARITES.
C    ------------------------------
C
      DO 20 IC = 1,NBNL
C
C  1.1.  CONVERSION DDLS GENERALISES -> DDLS PHYSIQUES.
C        ----------------------------------------------
         CALL PROJMG(NP1,NP2,IC,NBMCD,PHII,DEPG,XGLO)
C
C  1.2.  PASSAGE REPERE GLOBAL -> LOCAL.
C        -------------------------------
         XORIG(1) = ORIG(1,IC)
         XORIG(2) = ORIG(2,IC)
         XORIG(3) = ORIG(3,IC)
         XGLO(1) = XGLO(1) + ORIG(4,IC)
         XGLO(2) = XGLO(2) + ORIG(5,IC)
         XGLO(3) = XGLO(3) + ORIG(6,IC)
         SINA = ALPHA(1,IC)
         COSA = ALPHA(2,IC)
         SINB = BETA(1,IC)
         COSB = BETA(2,IC)
         SING = GAMMA(1,IC)
         COSG = GAMMA(2,IC)
C
         CALL GLOLOC(XGLO,XORIG,SINA,COSA,SINB,COSB,SING,COSG,XLOC)
C
C
         TYPOBS = TYPCH(IC)
         NBS    = NBSEG(IC)
         MAILLA = NOECHO(IC,4)
         NOMNOE = NOECHO(IC,1)
C
         IF (TYPOBS.EQ.0 .OR. TYPOBS.EQ.1 .OR. TYPOBS.EQ.2) THEN
            XJEU = RC(1,IC)
         ENDIF
C        TEST DE LA POSITION INITIALES ET DE L'ORIGINE
         CALL DISBUT(NP3,IC,XLOC,TYPOBS,XJEU,RC,THETA,NBS,COST,SINT,
     &               DNORM)
         IF (DNORM .LT. 0.D0) THEN
             CALL U2MESS('A','ALGELINE_8')
             WRITE(INUM,'(I3.3)') IC
              VALK(1) = INUM
              VALK(2) = NOMNOE
              CALL U2MESK('A','ALGELINE_9', 2 ,VALK)
         ENDIF
C
C
C        TEST DE LA POSITION INITIALES ET DE L'ORIGINE DANS LES
C        DIRECTIONS NORMALES AU PLAN DE CHOC
C
      IF ( TYPOBS .EQ. 0 ) THEN
C
         IF ( ABS(XLOC(1)).GT.EPSI .OR. ABS(XLOC(3)).GT.EPSI ) THEN
             CALL U2MESS('A','ALGELINE_8')
             WRITE(INUM,'(I3.3)') IC
              VALK(1) = INUM
              VALK(2) = NOMNOE
              CALL U2MESK('A','ALGELINE_10', 2 ,VALK)
         ENDIF
C
C     --- OBSTACLE PLAN PARALLELE A ZLOCAL ---
C
      ELSEIF ( TYPOBS .EQ. 1 ) THEN
C
         IF ( ABS(XLOC(1)).GT.EPSI .OR. ABS(XLOC(2)).GT.EPSI ) THEN
             CALL U2MESS('A','ALGELINE_8')
             WRITE(INUM,'(I3.3)') IC
              VALK(1) = INUM
              VALK(2) = NOMNOE
              CALL U2MESK('A','ALGELINE_10', 2 ,VALK)
         ENDIF
C
C     --- OBSTACLE CIRCULAIRE OU DISCRETISE---
C
      ELSEIF ( TYPOBS .EQ. 2 .OR. TYPOBS .EQ. 3) THEN
C
         IF ( ABS(XLOC(1)).GT.EPSI ) THEN
             CALL U2MESS('A','ALGELINE_8')
             WRITE(INUM,'(I3.3)') IC
              VALK(1) = INUM
              VALK(2) = NOMNOE
              CALL U2MESK('A','ALGELINE_10', 2 ,VALK)
         ENDIF
C
      ENDIF
C
C
         CALL JEVEUO(MAILLA//'.COORDO    .VALE','L',JCOOR1)
         CALL JENONU(JEXNOM(MAILLA//'.NOMNOE',NOMNOE),INO1)
         CALL DISMOI('F','NB_NO_MAILLA',MAILLA,'MAILLAGE',NBNO,
     &                K8B,IER)
         IF(INO1.GT.1) THEN
            INO1 = INO1 - 1
         ENDIF
         IF(INO1.GE.(NBNO-1)) THEN
            INO1 = NBNO - 2
         ENDIF
         DO 12 K = 1,3
         DO 11 J = 1,3
            COOR(K,J) = ZR(JCOOR1+3*(INO1+K-2)+J-1)
 11      CONTINUE
 12      CONTINUE
         TOT = 0.D0
         DO 13 J = 1,3
            VECT(J) = COOR(3,J) - COOR(1,J)
            TOT = TOT + VECT(J)*VECT(J)
 13      CONTINUE
         TOT = DBLE(SQRT(TOT))
         DO 14 J = 1,3
            VECT(J) =  VECT(J) / TOT
 14      CONTINUE
C
         CALL GLOLOC(VECT,XORIV,SINA,COSA,SINB,COSB,SING,COSG,XLOC)
         IF(DBLE(ABS(XLOC(1))).LT.EPSI) THEN
            CALL U2MESS('A','ALGELINE_8')
            WRITE(INUM,'(I3.3)') IC
            CALL U2MESK('A','ALGELINE_11',1,INUM)
C
         ELSE IF(DBLE(ABS(XLOC(1))).LT.0.17364818D0) THEN
            CALL U2MESS('A','ALGELINE_8')
            WRITE(INUM,'(I3.3)') IC
            CALL U2MESK('A','ALGELINE_12',1,INUM)
C
         ELSE IF(DBLE(ABS(XLOC(1))).LT.0.70710678D0) THEN
            CALL U2MESS('A','ALGELINE_8')
            WRITE(INUM,'(I3.3)') IC
            CALL U2MESK('A','ALGELINE_13',1,INUM)
C
         ENDIF
C
C
 20   CONTINUE
C
      CALL JEDEMA()
C
      END
