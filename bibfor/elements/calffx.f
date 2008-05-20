      SUBROUTINE CALFFX(ALIAS ,KSI1  ,KSI2  ,FF    )
C      
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 19/05/2008   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      CHARACTER*8 ALIAS
      REAL*8      KSI1,KSI2,FF(9)
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
C
C CALCUL DES FONCTIONS DE FORME EN UN POINT DE L'ELEMENT DE REFERENCE
C
C ----------------------------------------------------------------------
C
C
C IN  ALIAS  : NOM D'ALIAS DE L'ELEMENT
C IN  KSI1   : POINT DE CONTACT SUIVANT KSI1 DES FONCTIONS DE FORME 
C IN  KSI2   : POINT DE CONTACT SUIVANT KSI2 DES FONCTIONS DE FORME 
C OUT FF     : FONCTIONS DE FORMES EN KSI1,KSI2
C
C ----------------------------------------------------------------------
C
      REAL*8  A,B,C,D
      REAL*8  AL31,AL32,AL33
      REAL*8  UNS4
      REAL*8  X
      INTEGER I
C
C ----------------------------------------------------------------------
C
      AL31(X) = 0.5D0*X* (X-1.D0)
      AL32(X) = - (X+1.D0)* (X-1.D0)
      AL33(X) = 0.5D0*X* (X+1.D0)
      UNS4    = 0.25D0
C
C ----------------------------------------------------------------------
C
      DO 10 I=1,9
        FF(I)     = 0.D0 
 10   CONTINUE
C 
      IF (ALIAS(1:3).EQ.'SG2') THEN
        FF(1)  = 0.5D0*(1.D0-KSI1)
        FF(2)  = 0.5D0*(1.D0+KSI1)
      ELSE IF (ALIAS(1:3).EQ.'SG3') THEN
        FF(1)  = -0.5D0* (1.D0-KSI1)*KSI1
        FF(2)  =  0.5D0* (1.D0+KSI1)*KSI1
        FF(3)  =  1.0D0* (1.D0+KSI1)*(1.D0-KSI1)
      ELSE IF (ALIAS(1:3).EQ.'TR3') THEN
        FF(1)  = 0.5D0* (1.D0+KSI2)
        FF(2)  = -0.5D0* (KSI1+KSI2)
        FF(3)  = 0.5D0* (1.D0+KSI1)
      ELSE IF (ALIAS(1:3).EQ.'TR6') THEN
        FF(1)  = 0.5D0* (1.D0+KSI2)*KSI2
        FF(2)  = 0.5D0* (KSI1+KSI2)* (KSI1+KSI2+1.D0)
        FF(3)  = 0.5D0* (1.D0+KSI1)*KSI1
        FF(4)  = - (1.D0+KSI2)* (KSI1+KSI2)
        FF(5)  = - (1.D0+KSI1)* (KSI1+KSI2)
        FF(6)  = (1.D0+KSI1)* (1.D0+KSI2)
      ELSE IF (ALIAS(1:3).EQ.'QU4') THEN
        A      = 1.D0 + KSI1
        B      = 1.D0 + KSI2
        C      = 1.D0 - KSI1
        D      = 1.D0 - KSI2
        FF(1)  = C*B*UNS4
        FF(2)  = C*D*UNS4
        FF(3)  = A*D*UNS4
        FF(4)  = A*B*UNS4
      ELSE IF (ALIAS(1:3).EQ.'QU8') THEN
        FF(1)  = (1.D0+KSI2)* (1.D0-KSI1)* (-1.D0-KSI1+KSI2)*0.25D0
        FF(2)  = (1.D0-KSI2)* (1.D0-KSI1)* (-1.D0-KSI1-KSI2)*0.25D0
        FF(3)  = (1.D0-KSI2)* (1.D0+KSI1)* (-1.D0+KSI1-KSI2)*0.25D0
        FF(4)  = (1.D0+KSI2)* (1.D0+KSI1)* (-1.D0+KSI1+KSI2)*0.25D0
        FF(5)  = (1.D0-KSI2)* (1.D0-KSI1)* (1.D0+KSI2)*0.5D0
        FF(6)  = (1.D0-KSI2)* (1.D0-KSI1)* (1.D0+KSI1)*0.5D0
        FF(7)  = (1.D0-KSI2)* (1.D0+KSI1)* (1.D0+KSI2)*0.5D0
        FF(8)  = (1.D0+KSI2)* (1.D0-KSI1)* (1.D0+KSI1)*0.5D0
      ELSE IF (ALIAS(1:3).EQ.'QU9') THEN
        FF(1)  = AL31(KSI1)*AL31(KSI2)
        FF(2)  = AL33(KSI1)*AL31(KSI2)
        FF(3)  = AL33(KSI1)*AL33(KSI2)
        FF(4)  = AL31(KSI1)*AL33(KSI2)
        FF(5)  = AL32(KSI1)*AL31(KSI2)
        FF(6)  = AL33(KSI1)*AL32(KSI2)
        FF(7)  = AL32(KSI1)*AL33(KSI2)
        FF(8)  = AL31(KSI1)*AL32(KSI2)
        FF(9)  = AL32(KSI1)*AL32(KSI2)
      ELSE
        CALL ASSERT(.FALSE.)
      END IF

      END
