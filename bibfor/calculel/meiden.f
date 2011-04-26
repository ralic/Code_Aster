      LOGICAL FUNCTION MEIDEN(SCAL,NCMP,I1,I3,NEC,I2,I4)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     ARGUMENTS:
C     ----------
      CHARACTER*4 SCAL
      INTEGER NCMP,I1,I3,NEC,I2,I4
C ----------------------------------------------------------------------
C     ENTREES:
C        SCAL : R, I , C, K8, K16, K24
C        NCMP : NOMBRE DE COMPOSANTES DES GRANDEURS
C          I1 : ADRESSE DANS ZR OU ZI ... DU DEBUT DE LA 1ERE GRANDEUR
C          I3 : ADRESSE DANS ZR OU ZI ... DU DEBUT DE LA 2EME GRANDEUR
C        NEC  : NOMBRE D'ENTIERS CODES
C          I2 : ADRESSE DANS ZI DU DEBUT DU DG DE LA 1ERE GRANDEUR
C          I4 : ADRESSE DANS ZI DU DEBUT DU DG DE LA 2EME GRANDEUR
C
C     SORTIES:
C     MEIDEN : VRAI SI LES 2 GRANDEURS SONT IDENTIQUES.
C ----------------------------------------------------------------------
C
C     FONCTIONS EXTERNES:
C     -------------------
C
C     VARIABLES LOCALES:
C     ------------------
C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI
      REAL*8 ZR
      COMPLEX*16 ZC
      LOGICAL ZL
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
C
C DEB-------------------------------------------------------------------
C
      MEIDEN = .FALSE.
C
C     -- ON TESTE D'ABORD L'EGALITE DES DESCIPTEUR GRANDEUR:
      DO 1,IEC = 1,NEC
         IF (ZI(I2+IEC).NE.ZI(I4+IEC)) GO TO 9999
    1 CONTINUE
C
C     -- ON TESTE ENSUITE LES VALEURS:
      IF (SCAL(1:1).EQ.'I') THEN
         DO 2,I = 1,NCMP
            IF (ZI(I1+I).NE.ZI(I3+I)) GO TO 9999
    2    CONTINUE
      ELSE IF (SCAL(1:1).EQ.'R') THEN
         DO 3,I = 1,NCMP
            IF (ZR(I1+I).NE.ZR(I3+I)) GO TO 9999
    3    CONTINUE
      ELSE IF (SCAL(1:1).EQ.'C') THEN
         DO 4,I = 1,NCMP
            IF (ZC(I1+I).NE.ZC(I3+I)) GO TO 9999
    4    CONTINUE
      ELSE IF (SCAL(1:3).EQ.'K8 ') THEN
         DO 5,I = 1,NCMP
            IF (ZK8(I1+I).NE.ZK8(I3+I)) GO TO 9999
    5    CONTINUE
      ELSE IF (SCAL(1:3).EQ.'K16') THEN
         DO 6,I = 1,NCMP
            IF (ZK16(I1+I).NE.ZK16(I3+I)) GO TO 9999
    6    CONTINUE
      ELSE IF (SCAL(1:3).EQ.'K24') THEN
         DO 7,I = 1,NCMP
            IF (ZK24(I1+I).NE.ZK24(I3+I)) GO TO 9999
    7    CONTINUE
      ELSE
         CALL ASSERT(.FALSE.)
      END IF
      MEIDEN = .TRUE.
 9999 CONTINUE
      END
