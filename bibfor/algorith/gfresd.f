      SUBROUTINE GFRESD ( DZ, D2Z, DT, X, FVEC, COEF1 ,FLUID, GEOM1,
     +                    CFPCD1, ITDASH, URM1 )
      IMPLICIT NONE
      INTEGER   ITDASH
      REAL*8    DZ, D2Z, DT, COEF1(*), FLUID(*), GEOM1(*), CFPCD1(*),
     +          X(*), FVEC(*), URM1
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/10/2003   AUTEUR BOYERE E.BOYERE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C
C     CALCUL DE LA VALEUR DE LA FONCTION GFRESD
C-----------------------------------------------------------------------
C
      INTEGER   SGU1
      REAL*8    GAMMA, THETA, CD0, CD1, CD2, P0, P1, P2, AA0, ROC,
     +          LAMEQ, DH, A, A1, AC, AR, A0, L0, L1, LEQ, UN
C     ------------------------------------------------------------------
C
      LAMEQ = COEF1(2)
      GAMMA = COEF1(3)
      THETA = COEF1(4)
      LEQ   = COEF1(5)
C
      ROC = FLUID(1)
      P0  = FLUID(5)
      P1  = FLUID(6)
      P2  = FLUID(7)
C      
      A   = GEOM1(4)
      A0  = GEOM1(5)
      A1  = GEOM1(6)
      AA0 = GEOM1(7)
      AC  = GEOM1(8)
      AR  = GEOM1(9)
      DH  = GEOM1(10)
      L0  = GEOM1(13)
      L1  = GEOM1(14)
C      
      CD0 = CFPCD1(1)
      CD1 = CFPCD1(2)
      CD2 = CFPCD1(3)
C
      UN = 1.0D0
C
      IF ( X(1) .GT. 0 ) THEN
         SGU1 = +1
      ELSE
         SGU1 = -1
      ENDIF
C
      IF ( ITDASH .EQ. 1 )  URM1 = X(2)
C
      FVEC(1) = (UN+SGU1*CD1)*X(1)**2 + (-UN+GAMMA+THETA
     +         -((A0/AA0)**2+CD0)*(AR/A0)**2)*X(2)**2
     +       +2*((A0/AA0)**2+CD0)*AC*AR/A0**2*DZ*X(2)
     +         -((A0/AA0)**2+CD0)*(AC/A0)**2*DZ**2
     +         -2*(P0-P1)/ROC+2*(LEQ+L0*AR/A0)*(X(2)-URM1)/DT
     +         -2*L0*AC/A0*D2Z
C
      FVEC(2)=((UN+LAMEQ*L1/DH+CD2)*(A1/A)**2-(UN+SGU1*CD1))*X(1)**2
     &        +(UN+LAMEQ*L1/DH+CD2)*(AR/A)**2*X(2)**2
     &      -2*(UN+LAMEQ*L1/DH+CD2)*AR*A1/A**2*X(1)*X(2)-2*(P1-P2)/ROC
C
      END
