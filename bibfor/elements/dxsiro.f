      SUBROUTINE DXSIRO ( NE , T2VE , CDLE , CDLC )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 23/05/2011   AUTEUR SELLENET N.SELLENET 
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
      INTEGER  NE
      REAL*8   T2VE(2,2)
      REAL*8   CDLE(*)
      REAL*8   CDLC(*)
C     ------------------------------------------------------------------
C     PASSAGE DES CONTRAINTES OU DEFORMATIONS DU REPERE INTRINSEQUE DE
C     L'ELEMENT AU REPERE LOCAL DE LA COQUE
C     ------------------------------------------------------------------
C     IN  NE    I      NOMBRE DE POINTS A TRAITER
C     IN  T2VE  R  2,2  MATRICE DE PASSAGE VARIETE - ELEMENT
C     IN  CDLE R    *   SIXX SIYY SIXY SIXZ SIYZ
C     OUT CDLC R    *   SIXX SIYY SIXY SIXZ SIYZ
C  OU IN  CDLE R    *   EPXX EPYY EPXY EPXZ EPYZ
C     OUT CDLE R    *   EPXX EPYY EPXY EPXZ EPYZ
C     ------------------------------------------------------------------
      REAL*8        SIMELE(4) , SIMELC(4)
      REAL*8        XAB(2,2) , T2EV(2,2)
C
C     TRANSPOSEE DE T2VE
      T2EV(1,1) = T2VE(1,1)
      T2EV(1,2) = T2VE(2,1)
      T2EV(2,1) = T2VE(1,2)
      T2EV(2,2) = T2VE(2,2)
C
      DO 120 I = 1 , NE
         SIMELE(1) = CDLE(1+6*(I-1))
         SIMELE(2) = CDLE(4+6*(I-1))
         SIMELE(3) = CDLE(4+6*(I-1))
         SIMELE(4) = CDLE(2+6*(I-1))
C
         CALL UTBTAB ('ZERO',2,2,SIMELE,T2EV,XAB,SIMELC)
C
         CDLC(1+6*(I-1)) = SIMELC(1)
         CDLC(2+6*(I-1)) = SIMELC(4)
         CDLC(3+6*(I-1)) = 0.D0
         CDLC(4+6*(I-1)) = SIMELC(2)
         CDLC(5+6*(I-1)) = CDLE(5+6*(I-1)) * T2EV(1,1) +
     &                     CDLE(6+6*(I-1)) * T2EV(2,1)
         CDLC(6+6*(I-1)) = CDLE(5+6*(I-1)) * T2EV(1,2) +
     &                     CDLE(6+6*(I-1)) * T2EV(2,2)
C
  120 CONTINUE
C
      END
