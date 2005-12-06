      SUBROUTINE FOC2IN ( METHOD, NBPTS, VAR ,FON   ,CSTE,RES   )
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)       METHOD
      INTEGER                     NBPTS
      REAL*8                             VAR(*),FON(*),CSTE,RES(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 26/03/97   AUTEUR D6BHHBQ B.QUINNEZ 
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
C     INTEGRATION D'UNE FONCTION PAR LA METHODE DE SIMPSON.
C     ------------------------------------------------------------------
C IN  METHOD : K  : NOM DE LA METHODE D'INTEGRATION
C                       TRAPEZES   : DISPONIBLE
C                       SIMPSON    : DISPONIBLE
C IN  NBPTS  : IS : NOMBRE DE PAS DE TEMPS
C IN  VAR    : R8 : TABLEAU DE LA VARIABLE (LES INSTANTS)
C IN  FON    : R8 : TABLEAU DE LA FONCTION A INTEGRER
C IN  CSTE   : R8 : CONSTANTE D'INTEGRATION
C OUT RES    : R8 : TABLEAU DE LA FONCTION INTEGREE
C     ------------------------------------------------------------------
C
      REAL*8    FA , FM    , FB
      REAL*8    H1  , H2  , BMA, DELTAH, EPSI
      REAL*8                     CT1   , CT2 , CT3
      REAL*8    ZERO, UN, DEUX, QUATRE, SIX , EPS
C
C     COEF(1) POUR LES IMPAIRS, COEF(2) POUR LES PAIRS
      REAL*8    COEF(2)
      INTEGER   IP(2)
      DATA      IP/2,1/
C     ------------------------------------------------------------------
      ZERO   = 0.0D0
      UN     = 1.0D0
      DEUX   = 2.0D0
      QUATRE = 4.0D0
      SIX    = 6.0D0
      EPS    = 1.0D-04
C


      IF (METHOD.EQ.'TRAPEZE') THEN
C
      RES(1) = CSTE
      DO 100 I = 2, NBPTS
         RES(I) = RES(I-1) + (VAR(I)-VAR(I-1))
     &          * (FON(I)+FON(I-1)) * 0.5D0
 100  CONTINUE
C
      ELSE IF (METHOD.EQ.'SIMPSON') THEN
C
      FM      =  FON(1)
      FB      =  FON(2)
      H2      =  VAR(2) - VAR(1)
      COEF(1) = CSTE
      COEF(2) = CSTE +(FB+FM)*H2/DEUX
      RES(1)  = COEF(1)
      RES(2)  = COEF(2)
      IPERM   = 1
      DO 200 I = 3 , NBPTS
         H1  = H2
         H2  = VAR(I) - VAR(I-1)
         BMA = H1 + H2
         FA  = FM
         FM  = FB
         FB  = FON(I)
         IF ( H1 .EQ. ZERO .OR. H2 .EQ. ZERO ) THEN
            CT1 = UN
            CT2 = QUATRE
            CT3 = UN
         ELSE
            DELTAH = H2 - H1
            IF ( ABS( DELTAH / H1 ) .LE. EPS ) THEN
               CT1 = UN
               CT2 = QUATRE
               CT3 = UN
            ELSE
C              EXPRESSION "SIMPLE" DES COEFFICIENTS
C              CT1  = DEUX - H2/H1
C              CT2  = (H1+H2)*(H1+H2)/(H1*H2)
C              CT3  = DEUX - H1/H2
C
C              EXPRESSION "INFORMATIQUE" DES COEFFICIENTS
               EPSI = DELTAH / (H1*H2)
               CT1  = UN - EPSI * H2
               CT2  = QUATRE + EPSI * DELTAH
               CT3  = UN + EPSI * H1
            ENDIF
         ENDIF
         COEF(IPERM) = COEF(IPERM) + (BMA/SIX)*(CT1*FA+CT2*FM+CT3*FB)
         RES(I)      = COEF(IPERM)
         IPERM       = IP(IPERM)
  200 CONTINUE
C        
      ENDIF
C
      END
