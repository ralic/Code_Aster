      SUBROUTINE FOC2SO(NBFREQ,FREQ,NBAMOR,AMOR,NBINST,DELTAT,FON,
     +                                                         SPECTR )
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8            FREQ(*),AMOR(*),FON(*),SPECTR(NBFREQ,NBAMOR,3)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 23/01/95   AUTEUR B8BHHHH J.R.LEVESQUE 
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
C     CALCUL DU SPECTRE D'OSCILLATEUR (1 DDL)
C     METHODE D'INTEGRATION P.C. JENNINGS ET NIGAM
C     ------------------------------------------------------------------
C IN  NBFREQ : I : NOMBRE DE FREQUENCES
C IN  FREQ   : R : TABLEAU DES FREQUENCES
C IN  NBAMOR : I : NOMBRE D'AMORTISSEMENT
C IN  AMOR   : R : TABLEAU DES AMORTISSEMENT
C IN  NBINST : I : NOMBRE D'INSTANT DE L'ACCELEROGRAMME
C IN  FON    : R : VALEURS DE L'ACCELEROGRAMME
C IN  DELTAT : R : PAS DE TEMPS DES VALEURS DE L'ACCELEROGRAMME
C OUT SPECTR : R : TABLEAU DES VALEURS DU SPECTRE D'OSCILLATEUR
C     ------------------------------------------------------------------
C     RESTRICTION D'EMPLOI : ON SUPPOSE LE PAS DE TEMPS CONSTANT
C     ------------------------------------------------------------------
C
      DEUXPI = R8DEPI()
      ZERO = 0.D0
      UN   = 1.D0
      DEUX = 2.D0
      DO 100 IAMOR = 1, NBAMOR
         XSI = AMOR(IAMOR)
         XSI2 = XSI * XSI
C
         DO 200 IFREQ = 1, NBFREQ
           W = DEUXPI * FREQ(IFREQ)
           W2 = W * W
           UNSW2 = UN / W2
           WDT = W * DELTAT
           COSWDT = COS( WDT )
           SINWDT = SIN( WDT )
           A = W * SQRT( UN - XSI2 )
           B = DEUX * XSI / W
           C = ( ( DEUX * XSI2 ) - UN ) / A
           D = XSI * W
           E = W2 / A
           F = COSWDT + ( SINWDT * D / A )
           EXPXWT = EXP( -D * DELTAT )
           EXPSA = EXPXWT / A
           D1 = ZERO
           D2 = ZERO
           V2 = ZERO
           F1 = FON(1)
           DO 300 J = 1, NBINST-1
              F2 = FON(J+1)
              FSDT = ( F2 - F1 ) / DELTAT
              D3 = EXPSA*((A*COSWDT+D*SINWDT)*D2+SINWDT*V2)+UNSW2*
     +             (EXPXWT*(FSDT*(B*COSWDT+C*SINWDT)-F1*F)+F2-B*FSDT)
              V3 = EXPSA*((-W2)*SINWDT*D2+((-D)*SINWDT+A*COSWDT)*V2) +
     +             UNSW2*(EXPXWT*((-FSDT)*F+F1*E*SINWDT)+FSDT)
              D2 = D3
              V2 = V3
              D1 = MAX(D1,ABS(D3))
              F1 = F2
 300      CONTINUE
          SPECTR(IFREQ,IAMOR,1) = D1
          SPECTR(IFREQ,IAMOR,2) = W * D1
          SPECTR(IFREQ,IAMOR,3) = W2 * D1
 200    CONTINUE
 100  CONTINUE
C
      END
