      SUBROUTINE I3CRAD(K,F,A,NBA,T,R1,R2)
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INTEGER K,F,NBA,A
      INTEGER VALI(2)
      REAL*8  R1,R2,T
C
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 20/02/2007   AUTEUR LEBOUVIER F.LEBOUVIER 
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
C     COORDO REF SUR ARETE A DE FACE F AU POINT T
C     ------------------------------------------------------------------
C IN  K    : I : NUMERO GLOBAL DE LA MAILLE
C IN  F    : I : NUMERO LOCAL  DE LA FACE
C IN  A    : I : NUMERO LOCAL  DE L' ARETE
C IN  NBA  : I : NOMBRE D' A DE LA FACE
C IN  T    : I : ABSCISSE DU POINT SUR L' A 0<T<1
C OUT R1   : R : COORDO REF 1
C OUT R2   : R : COORDO REF 2
C     ------------------------------------------------------------------
C
      REAL*8 ZERO,UN,DEUX
C
C======================================================================
C
      ZERO = 0.0D0
      UN   = 1.0D0
      DEUX = 2.0D0
      IF ( NBA .EQ. 3 ) THEN
         IF ( A .EQ. 1 ) THEN
            R1 = T
            R2 = ZERO
         ELSE IF ( A .EQ. 2 ) THEN
            R1 = UN - T
            R2 = T
         ELSE
            R2 = T
            R1 = ZERO
         ENDIF
      ELSE IF ( NBA .EQ. 4 ) THEN
         IF ( A .EQ. 1 ) THEN
            R1 =  DEUX*T - 1
            R2 = -UN
         ELSE IF ( A .EQ. 2 ) THEN
            R2 =  DEUX*T - 1
            R1 =  UN
         ELSE IF ( A .EQ. 3 ) THEN
            R1 = -DEUX*T + 1
            R2 =  UN
         ELSE
            R2 = -DEUX*T + 1
            R1 = -UN
         ENDIF
      ELSE
         VALI (1) = K
         VALI (2) = F
         CALL U2MESG('F', 'POSTRELE_79',0,' ',2,VALI,0,0.D0)
      ENDIF
      END
