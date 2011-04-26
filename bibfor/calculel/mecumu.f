      SUBROUTINE MECUMU(SCAL,NCMP,IAD1,IAD2,NEC,DG1,DG2)
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
C ----------------------------------------------------------------------
C     "CUMULE" LES CMPS D'1 GRANDEUR AU SENS DE:
C     '   ' + ' 2' = ' 2 '
C     ' 3 ' + ' 2' = ' 3 '
C
C ----------------------------------------------------------------------
C
C     ARGUMENTS:
C     ----------
      CHARACTER*8 SCAL
      INTEGER NCMP,IAD1,IAD2,NEC,DG1(NEC),DG2(NEC)
C ----------------------------------------------------------------------
C     ENTREES:
C       SCAL : TYPE SCALAIRE : 'R  ', 'C  ', 'I  ', 'K8 ' ,'K16 '
C       NCMP : NOMBRE DE COMPOSANTES A ACCUMULER.
C       IAD1 : ADRESSE DANS ZI,ZR,... DU SEGMENT A CUMULER
C       IAD2 : ADRESSE DANS ZI,ZR,... DU SEGMENT OU ON CUMULE
C
C       NEC  : NOMBRE D'ENTIERS CODES
C       DG1  : DESCRIPTEUR DE LA GRANDEUR A CUMULER.
C       DG2  : DESCRIPTEUR  DE LA GRANDEUR OU ON CUMULE.
C
C     SORTIES:
C       LES OBJETS SONT MODIFIES.
C ----------------------------------------------------------------------
C
C     FONCTIONS EXTERNES:
C     -------------------
      LOGICAL EXISDG
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
      ICO = 0
      IF (SCAL(1:1).EQ.'I') THEN
C
C        -- CAS D'1 SEGMENT ENTIER:
C        --------------------------
         DO 1,I = 1,NCMP
            IF (EXISDG(DG1,I)) THEN
               ICO = ICO + 1
               ZI(IAD2-1+I) = ZI(IAD1-1+ICO)
            END IF
    1    CONTINUE
      ELSE IF (SCAL(1:1).EQ.'R') THEN
C
C        -- CAS D'1 SEGMENT REEL  :
C        --------------------------
         DO 2,I = 1,NCMP
            IF (EXISDG(DG1,I)) THEN
               ICO = ICO + 1
               ZR(IAD2-1+I) = ZR(IAD1-1+ICO)
            END IF
    2    CONTINUE
C
C        -- CAS D'1 SEGMENT COMPLEX:
C        --------------------------
      ELSE IF (SCAL(1:1).EQ.'C') THEN
         DO 3,I = 1,NCMP
            IF (EXISDG(DG1,I)) THEN
               ICO = ICO + 1
               ZC(IAD2-1+I) = ZC(IAD1-1+ICO)
            END IF
    3    CONTINUE
C
C        -- CAS D'1 SEGMENT DE CARACTERES (K8):
C        ---------------------------------
      ELSE IF (SCAL(1:3).EQ.'K8 ') THEN
         DO 4,I = 1,NCMP
            IF (EXISDG(DG1,I)) THEN
               ICO = ICO + 1
               ZK8(IAD2-1+I) = ZK8(IAD1-1+ICO)
            END IF
    4    CONTINUE
C
C        -- CAS D'1 SEGMENT DE CARACTERES (K16):
C        ---------------------------------
      ELSE IF (SCAL(1:3).EQ.'K16') THEN
         DO 5,I = 1,NCMP
            IF (EXISDG(DG1,I)) THEN
               ICO = ICO + 1
               ZK16(IAD2-1+I) = ZK16(IAD1-1+ICO)
            END IF
    5    CONTINUE
C
C        -- CAS D'1 SEGMENT DE CARACTERES (K24):
C        ---------------------------------
      ELSE IF (SCAL(1:3).EQ.'K24') THEN
         DO 6,I = 1,NCMP
            IF (EXISDG(DG1,I)) THEN
               ICO = ICO + 1
               ZK24(IAD2-1+I) = ZK24(IAD1-1+ICO)
            END IF
    6    CONTINUE
      ELSE
         CALL U2MESK('F','CALCULEL3_38',1,SCAL(1:4))
      END IF
C
      DO 10,I = 1,NEC
         DG2(I) = IOR(DG2(I),DG1(I))
   10 CONTINUE
C
      END
