      SUBROUTINE FOC2DE ( NBPTS, VAR ,FON   ,RES  )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             NBPTS
      REAL*8                     VAR(*),FON(*),RES(*)
C     ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 04/01/95   AUTEUR G8BHHAC A.Y.PORTABILITE 
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
C     DERIVATION D'UNE FONCTION PAR DIFFERENCE CENTREE (APPROXIMATION
C     PARABOLIQUE)
C     ----------------------------------------------------------------
C IN  NBPTS  : NOMBRE DE PAS DE TEMPS
C IN  VAR    : TABLEAU DE LA VARIABLE (LES INSTANTS)
C IN  FON    : TABLEAU DE LA FONCTION A DERIVER
C OUT RES    : TABLEAU DE LA FONCTION DERIVEE
C     ----------------------------------------------------------------
      REAL*8          DELTA , DELTA1, DELTA2
      REAL*8          PENTEI, PENTEF
      REAL*8          ZERO  , EPS
      REAL*8          R8GAEM
C     ------------------------------------------------------------------
      ZERO  = 0.D0
      EPS   = 1.D0 / R8GAEM()
C
      PENTEI = ZERO
      DELTA1 = ZERO
      DO 11 I = 1 , NBPTS - 1
         DELTA2 = (VAR(I+1)) - (VAR(I))
         IF ( ABS(DELTA2) .GT. EPS ) THEN
            PENTEF = (FON(I+1)-FON(I))/ DELTA2
         ELSE
            PENTEF = ZERO
         ENDIF
         DELTA = DELTA1 + DELTA2
         RES(I) = (DELTA1*PENTEI + DELTA2*PENTEF) / DELTA
         DELTA1 = DELTA2
         PENTEI = PENTEF
   11 CONTINUE
      RES(NBPTS) = PENTEF
      END
