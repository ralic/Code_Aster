      SUBROUTINE FUN1(AREA, A1, A2, N )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER                       N
      REAL*8          AREA, A1, A2
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 04/01/95   AUTEUR G8BHHAC A.Y.PORTABILITE 
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
C         CALCUL DE L'AIRE OU DE LA CONSTANTE DE TORSION EQUIVALENTE
C    D'UNE POUTRE DROITE A SECTION VARIABLE SOUS L'HYPOTHESE DE VARIA-
C    TION LINEAIRE DES COORDONNEES
C     ------------------------------------------------------------------
C                        LISTE DES ARGUMENTS
C    TYPE  !   NOM  !  TABLEAU  !             SIGNIFICATION
C    -------------------------------------------------------------------
C IN  R8   ! A1     !     -     ! VALEUR INITIALE
C IN  R8   ! A2     !     -     ! VALEUR FINALE
C IN  IS   ! N      !     -     ! ORDRE DU POLYNOME
C OUT  R8  ! AREA   !     -     ! VALEUR EQUIVALENTE
C     ------------------------------------------------------------------
C
      REAL*8  XM,XM1,XM2
C
      IF  (A1.EQ.A2) THEN
          AREA = A1
      ELSE
         IF    ( N .LT.2 )  THEN
            AREA = (A2-A1) / (LOG(A2)-LOG(A1))
         ELSEIF( N .EQ.2 )  THEN
C           VARIATION HOMOTHETIQUE.
            AREA = SQRT (A1*A2)
         ELSEIF( N .EQ.3 )  THEN
            XM   = 2.D0/3.D0
            XM1  =  A1 ** XM
            XM2  =  A2 ** XM
            AREA =  2 * (XM1*A2 - A1*XM2 ) / (XM2-XM1)
         ELSE
            XM   = 1.D0 / N
            AREA = (N-1)*((A2**XM)-A1**XM)
            XM   = XM-1.D0
            AREA=AREA / ((A1**XM)-A2**XM)
         ENDIF
      ENDIF
      END
