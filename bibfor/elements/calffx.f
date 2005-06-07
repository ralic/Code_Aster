      SUBROUTINE CALFFX(ALIAS,XI,YI,TN)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 31/08/2004   AUTEUR JMBHH01 J.M.PROIX 
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

C.......................................................................

C BUT:   CALCUL DES FFS ET LEURS DERIVEES  AU POINT (XI,YI)

C ENTREES  ---> ALIAS       : NOM D'ALIAS DE L'ELEMENT
C          ---> XI,YI       : POINT DE CALCUL DES FFS ET DU JACOBIEN

C SORTIES  <---  TN   : FFS EN XI,YI,ZI

C.......................................................................

      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*8 ALIAS
      REAL*8 TN(9)
C     REAL*8       TAU1(3),TAU2(3)

      AL31(X) = 0.5D0*X* (X-1.D0)
      AL32(X) = - (X+1.D0)* (X-1.D0)
      AL33(X) = 0.5D0*X* (X+1.D0)
      UNS4 = 0.25D0
C  ----------------------------------------------------------

      IF (ALIAS(1:3).EQ.'SG2') THEN

C   LES FF EN XI,YI,ZI ET LEURS D

        TN(1) = 0.5D0* (1-XI)
        TN(2) = 0.5D0* (1+XI)


      ELSE IF (ALIAS(1:3).EQ.'SG3') THEN

        TN(1) = -0.5D0* (1.D0-XI)*XI
        TN(2) =  0.5D0* (1.D0+XI)*XI
        TN(3) =  1.0D0* (1.D0+XI)*(1-XI)



      ELSE IF (ALIAS(1:3).EQ.'TR3') THEN

        TN(1) = 0.5D0* (1.D0+YI)
        TN(2) = -0.5D0* (XI+YI)
        TN(3) = 0.5D0* (1.D0+XI)

      ELSE IF (ALIAS(1:3).EQ.'TR6') THEN

        TN(1) = 0.5D0* (1.D+00+YI)*YI
        TN(2) = 0.5D0* (XI+YI)* (XI+YI+1.D0)
        TN(3) = 0.5D0* (1.D+00+XI)*XI
        TN(4) = - (1.D0+YI)* (XI+YI)
        TN(5) = - (1.D0+XI)* (XI+YI)
        TN(6) = (1.D0+XI)* (1.D0+YI)

      ELSE IF (ALIAS(1:3).EQ.'QU4') THEN

        UNS4 = 0.25D0
        A = 1.D0 + XI
        B = 1.D0 + YI
        C = 1.D0 - XI
        D = 1.D0 - YI
        TN(1) = C*B*UNS4
        TN(2) = C*D*UNS4
        TN(3) = A*D*UNS4
        TN(4) = A*B*UNS4

      ELSE IF (ALIAS(1:3).EQ.'QU8') THEN

        TN(1) = (1.D0+YI)* (1.D0-XI)* (-1.D0-XI+YI)*0.25D0
        TN(2) = (1.D0-YI)* (1.D0-XI)* (-1.D0-XI-YI)*0.25D0
        TN(3) = (1.D0-YI)* (1.D0+XI)* (-1.D0+XI-YI)*0.25D0
        TN(4) = (1.D0+YI)* (1.D0+XI)* (-1.D0+XI+YI)*0.25D0
        TN(5) = (1.D0-YI)* (1.D0-XI)* (1.D0+YI)*0.5D0
        TN(6) = (1.D0-YI)* (1.D0-XI)* (1.D0+XI)*0.5D0
        TN(7) = (1.D0-YI)* (1.D0+XI)* (1.D0+YI)*0.5D0
        TN(8) = (1.D0+YI)* (1.D0-XI)* (1.D0+XI)*0.5D0


      ELSE IF (ALIAS(1:3).EQ.'QU9') THEN

        TN(1) = AL31(XI)*AL31(YI)
        TN(2) = AL33(XI)*AL31(YI)
        TN(3) = AL33(XI)*AL33(YI)
        TN(4) = AL31(XI)*AL33(YI)
        TN(5) = AL32(XI)*AL31(YI)
        TN(6) = AL33(XI)*AL32(YI)
        TN(7) = AL32(XI)*AL33(YI)
        TN(8) = AL31(XI)*AL32(YI)
        TN(9) = AL32(XI)*AL32(YI)

      ELSE
        CALL UTMESS('F','CALFFX','TYPE DE MAILLE INCONNU')
      END IF

      END
