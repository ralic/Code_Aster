      SUBROUTINE TE0001(OPTION,NOMTE)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16      OPTION,NOMTE
C     -----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C     CALCUL DES TERMES DE FORC_NOD_6DDL, 3DDL, 2DDL
C     -----------------------------------------------------------------
C     EN ENTREE :
C        OPTION : NOM DE L'OPTION A CALCULER
C        NOMTE  : NOM DU TYPE_ELEMENT
C     -----------------------------------------------------------------
C     COMMUNS NORMALISES  JEVEUX :
      COMMON/IVARJE/ZI(1)
      COMMON/RVARJE/ZR(1)
      COMMON/CVARJE/ZC(1)
      COMMON/LVARJE/ZL(1)
      COMMON/KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI
      REAL*8 ZR
      COMPLEX*16 ZC
      LOGICAL ZL
      CHARACTER*8  ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
C     -----------------------------------------------------------------
      REAL*8       R8DGRD,DGRD
      REAL*8       VALPAR(4),ANGL(3),MAT(3,3),VECT(6)
      CHARACTER*8  NOMPAR(4),NOMFON
      LOGICAL      LANGL
C     -----------------------------------------------------------------
      IF (NOMTE .EQ. 'FORCE_NOD_6DDL')  NDDL1 = 6
      IF (NOMTE .EQ. 'FORCE_NOD_3DDL')  NDDL1 = 3
      IF (NOMTE .EQ. 'FORCE_NOD_2DDL')  NDDL1 = 2
      IF (NOMTE .EQ. 'FORCE_NOD_COQ2D') NDDL1 = 3
      NDDL = NDDL1
      IF (NOMTE .EQ. 'FORCE_NOD_COQ2D') NDDL = 2
C
      IF (OPTION.EQ. 'CHAR_MECA_FORC_R') THEN
         CALL JEVECH ('PGEOMER', 'L', JGEOM)
         CALL JEVECH ('PFORNOR', 'L', JDIMP)
         CALL JEVECH ('PVECTUR', 'E', JVEC)
CCDIR$ IVDEP
         DO 10 I=1,NDDL1
            ZR(JVEC-1+I) = ZR(JDIMP-1+I)
 10      CONTINUE
         LANGL = ZR(JDIMP+NDDL1) .LT. 0.D0
         DO 11 I=1,3
            ANGL(I) = ZR(JDIMP+NDDL1+I)
 11      CONTINUE
      ELSE IF (OPTION.EQ. 'CHAR_MECA_FORC_F') THEN
         NBPAR = 4
         NOMPAR(1) = 'X'
         NOMPAR(2) = 'Y'
         NOMPAR(3) = 'Z'
         NOMPAR(4) = 'INST'
         CALL JEVECH ('PGEOMER', 'L', JGEOM)
         CALL JEVECH ('PTEMPSR', 'L', JTIME)
         CALL JEVECH ('PVECTUR', 'E', JVEC)
         VALPAR(1) = ZR(JGEOM-1+1)
         VALPAR(2) = ZR(JGEOM-1+2)
         VALPAR(3) = ZR(JGEOM-1+3)
         VALPAR(4) = ZR(JTIME-1+1)
         CALL JEVECH ('PFORNOF', 'L', JDIMP)
         DO 20 I=1,NDDL1
            NOMFON = ZK8(JDIMP-1+I)
            IER=0
           CALL FOINTE('FM',NOMFON,NBPAR,NOMPAR,VALPAR,ZR(JVEC-1+I),IER)
   20    CONTINUE
         LANGL = ZK8(JDIMP+NDDL1) .EQ. 'UTILISAT'
         IF ( LANGL ) THEN
            DGRD = R8DGRD()
            DO 21 I=1,3
               NOMFON = ZK8(JDIMP+NDDL1+I)
               IER=0
               CALL FOINTE('FM',NOMFON,NBPAR,NOMPAR,VALPAR,ANGL(I),IER)
               ANGL(I) = ANGL(I) * DGRD
 21         CONTINUE
         ENDIF
      ELSE
         CALL U2MESK('F','ELEMENTS2_61',1,OPTION)
      ENDIF
C
C     --- PROJECTION DANS LE REPERE ABSOLU ---
      IF ( LANGL ) THEN
         CALL MATROT ( ANGL , MAT )
         DO 101  I = 1, MIN(NDDL,3)
            VECT(I) = 0.D0
            DO 101  J = 1, MIN(NDDL,3)
               VECT(I) = VECT(I) + MAT(J,I)*ZR(JVEC-1+J)
 101     CONTINUE
         DO 102  I = 4, MIN(NDDL,6)
            VECT(I) = 0.D0
            DO 102  J = 4, MIN(NDDL,6)
               VECT(I) = VECT(I) + MAT(J-3,I-3)*ZR(JVEC-1+J)
 102     CONTINUE
         DO 103  I = 1, NDDL
            ZR(JVEC-1+I) = VECT(I)
 103     CONTINUE
      ENDIF
      END
