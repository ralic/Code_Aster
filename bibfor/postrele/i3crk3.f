      SUBROUTINE I3CRK3(TYPK,FACE,CRF,CRK)
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INTEGER TYPK,FACE
      REAL*8  CRF(*),CRK(*)
C
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 03/04/2007   AUTEUR VIVAN L.VIVAN 
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
C     ------------------------------------------------------------------
C     PASSAGE DES COORDO_REF_FACE AUX COORDO_REF_MAILLE
C     ------------------------------------------------------------------
C IN  TYPK   : I : TYPE DE LA MAILLE EI : TRETRA/PENTA/HEXA
C IN  FACE   : I : NUMERO DE LA FACE
C IN  CRF    : R : TABLE (1..2) DES COORDO_REF_FACE
C OUT CRK    : R : TABLE (1..3) DES COORDO_REF_MAILLE
C     ------------------------------------------------------------------
C
      REAL*8 ZERO,UN,UNSUR2,UNSUR4,UNSUR3,R1,R2
C
C=======================================================================
C
      ZERO   = 0.00D0
      UNSUR4 = 0.25D0
      UNSUR2 = 0.50D0
      UN     = 1.00D0
      UNSUR3 = UN/3.0D0
      R1     = CRF(1)
      R2     = CRF(2)
      IF ( TYPK .EQ. 1 ) THEN
         IF ( FACE . EQ. 0 ) THEN
            CRK(1) = UNSUR4
            CRK(2) = UNSUR4
            CRK(3) = UNSUR4
         ELSE IF ( FACE . EQ. 1 ) THEN
            CRK(1) = R2
            CRK(2) = R1
            CRK(3) = ZERO
         ELSE IF ( FACE . EQ. 2 ) THEN
            CRK(2) = R2
            CRK(3) = R1
            CRK(1) = ZERO
         ELSE IF ( FACE . EQ. 3 ) THEN
            CRK(3) = R2
            CRK(1) = R1
            CRK(2) = ZERO
         ELSE IF ( FACE . EQ. 4 ) THEN
            CRK(3) = R2
            CRK(2) = R1
            CRK(1) = UN - R1 -R2
         ELSE
            CALL U2MESK('F','INTEMAIL_6',1,'TETRA')
         ENDIF
      ELSE IF ( TYPK .EQ. 2 ) THEN
         IF ( FACE . EQ. 0 ) THEN
            CRK(1) = UNSUR3
            CRK(2) = UNSUR3
            CRK(3) = ZERO
         ELSE IF ( FACE . EQ. 1 ) THEN
            CRK(1) =  R2
            CRK(2) =  R1
            CRK(3) = -UN
         ELSE IF ( FACE . EQ. 2 ) THEN
            CRK(1) =  ZERO
            CRK(2) = (UN + R2)*UNSUR2
            CRK(3) =  R1
         ELSE IF ( FACE . EQ. 3 ) THEN
            CRK(2) =  ZERO
            CRK(1) = (UN + R1)*UNSUR2
            CRK(3) =  R2
         ELSE IF ( FACE . EQ. 4 ) THEN
            CRK(1) =  R1
            CRK(2) =  R2
            CRK(3) =  UN
         ELSE IF ( FACE . EQ. 5 ) THEN
            CRK(1) = (UN - R1)*UNSUR2
            CRK(2) = (UN + R1)*UNSUR2
            CRK(3) =  R2
         ELSE
            CALL U2MESK('F','INTEMAIL_6',1,'PENTA')
         ENDIF
      ELSE IF ( TYPK .EQ. 3 ) THEN
         IF ( FACE .EQ. 0 ) THEN
            CRK(1) = ZERO
            CRK(2) = ZERO
            CRK(3) = ZERO
         ELSE IF ( FACE . EQ. 1 ) THEN
            CRK(2) =  R1
            CRK(1) =  R2
            CRK(3) = -UN
         ELSE IF ( FACE . EQ. 2 ) THEN
            CRK(3) =  R1
            CRK(2) =  R2
            CRK(1) = -UN
         ELSE IF ( FACE . EQ. 3 ) THEN
            CRK(1) =  R1
            CRK(3) =  R2
            CRK(2) = -UN
         ELSE IF ( FACE . EQ. 4 ) THEN
            CRK(1) =  R1
            CRK(2) =  R2
            CRK(3) =  UN
         ELSE IF ( FACE . EQ. 5 ) THEN
            CRK(2) =  R1
            CRK(3) =  R2
            CRK(1) =  UN
         ELSE IF ( FACE . EQ. 6 ) THEN
            CRK(1) = -R1
            CRK(3) =  R2
            CRK(2) =  UN
         ELSE
            CALL U2MESK('F','INTEMAIL_6',1,'HEXA')
         ENDIF
      ELSE
         CALL U2MESS('F','INTEMAIL_7')
      ENDIF
      END
