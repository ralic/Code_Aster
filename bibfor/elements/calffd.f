      SUBROUTINE CALFFD(ALIAS,XI,YI,TN,DR,DDR)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 22/07/2003   AUTEUR LAVERNE J.LAVERNE 
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

C BUT:   CALCUL DES FONCTIONS DE FORMES ET DE LEURS DERIVEES
C        AU POINT DE COORDONNEES XI,YI,ZI

C ENTREES  ---> ALIAS : NOM D'ALIAS DE L'ELEMENT
C          ---> XI,YI : POINT DE CALCUL DES F FORMES ET LEURS DERIVEE


C SORTIES  <---  TN   : FONCTIONS DE FORMES EN XI,YI
C          <---  DR   : DERIVEES DES  FFS EN XI YI

C.......................................................................

C     IMPLICIT NONE
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*8 ALIAS
      REAL*8 TN(9),DR(2,9),DDR(3,9)

      AL31(X) = 0.5D0*X* (X-1.D0)
      AL32(X) = - (X+1.D0)* (X-1.D0)
      AL33(X) = 0.5D0*X* (X+1.D0)
      DAL31(U) = 0.5D0* (2.D0*U-1.D0)
      DAL32(U) = -2.D0*U
      DAL33(U) = 0.5D0* (2.D0*U+1.D0)

      UNS4 = 0.25D0


C  ----------------------------------------------------------

      IF (ALIAS(1:3).EQ.'SG2') THEN

C   LES FF EN XI,YI,ZI

        TN(1) = 0.5D0* (1-XI)
        TN(2) = 0.5D0* (1+XI)

C   LES DERIVEES PREMIERES

        DR(1,1) = -0.5D0
        DR(1,2) = 0.5D0

C   LES DERVIEES SECONDES

        DDR(1,1) = 0.D0
        DDR(1,2) = 0.D0

      ELSE IF (ALIAS(1:3).EQ.'SG3') THEN

C   LES FF EN XI,YI,ZI

        TN(1) = -0.5D0* (1-XI)*XI
        TN(2) = 0.5D0* (1+XI)*XI
        TN(3) = 1.D0* (1+XI)* (1-XI)

C   LES DERIVEES PREMIERES

        DR(1,1) = -0.5D0* (1-2*XI)
        DR(1,2) = 0.5D0* (1+2*XI)
        DR(1,3) = -2.D0*XI

C   LES DERVIEES SECONDES

        DDR(1,1) = -1.D0
        DDR(1,2) = 1.D0
        DDR(1,3) = -1.D0

      ELSE IF (ALIAS(1:3).EQ.'TR3') THEN

        TN(1) = 0.5D0* (1+YI)
        TN(2) = -0.5D0* (XI+YI)
        TN(3) = 0.5D0* (1+XI)

C  DERIVEES / XI
        DR(1,1) = 0.D0
        DR(1,2) = -0.5D0
        DR(1,3) = 0.5D0

C  DERIVEES / YI
        DR(2,1) = 0.5D0
        DR(2,2) = -0.5D0
        DR(2,3) = 0.D+00

C    LES DERVIEES SECONDES

        DDR(1,1) = 0.D0
        DDR(1,2) = 0.D0
        DDR(1,3) = 0.D0
        DDR(2,1) = 0.D0
        DDR(2,2) = 0.D0
        DDR(2,3) = 0.D0
        DDR(3,1) = 0.D0
        DDR(3,2) = 0.D0
        DDR(3,3) = 0.D0

C_______________________________________________________________________

      ELSE IF (ALIAS(1:3).EQ.'TR6') THEN

C LES FFS
        TN(1) = 0.5D0* (1.D+00+YI)*YI
        TN(2) = 0.5D0* (XI+YI)* (XI+YI+1)
        TN(3) = 0.5D0* (1.D+00+XI)*XI
        TN(4) = - (1.D0+YI)* (XI+YI)
        TN(5) = - (1.D0+XI)* (XI+YI)
        TN(6) = (1.D0+XI)* (1.D0+YI)

C LES DERIVEES 1ERES / XI
        DR(1,1) = 0.D0
        DR(1,2) = 0.5D0* (2.D0*XI+2.D0*YI+1.D0)
        DR(1,3) = 0.5D0* (2.D0*XI+1.D0)
        DR(1,4) = - (1.D0+YI)
        DR(1,5) = - (1.D0+YI+2.D0*XI)
        DR(1,6) = (1.D0+YI)
C ----------        / YI
        DR(2,1) = 0.5D0* (2.D0*YI+1.D0)
        DR(2,2) = 0.5D0* (2.D0*XI+2.D0*YI+1.D0)
        DR(2,3) = 0.D+00
        DR(2,4) = - (1.D0+XI+2.D0*YI)
        DR(2,5) = - (1.D0+XI)
        DR(2,6) = (1.D0+XI)

C ----------      /XIXI
        DDR(1,1) = 0.D0
        DDR(1,2) = 1.D0
        DDR(1,3) = 1.D0
        DDR(1,4) = 0.D0
        DDR(1,5) = -2.D0
        DDR(1,6) = 0.D0
C ---------      /YIYI
        DDR(2,1) = 1.D0
        DDR(2,2) = 1.D0
        DDR(2,3) = 0.D0
        DDR(2,4) = -2.D0
        DDR(2,5) = 0.D0
        DDR(2,6) = 0.D0
C ---------      /XIYI
        DDR(3,1) = 0.D0
        DDR(3,2) = 1.D0
        DDR(3,3) = 0.D0
        DDR(3,4) = -1.D0
        DDR(3,5) = -1.D0
        DDR(3,6) = 1.D0

      ELSE IF (ALIAS(1:3).EQ.'QU4') THEN

        UNS4 = 0.25D0
        A = 1.D0 + XI
        B = 1.D0 + YI
        C = 1.D0 - XI
        D = 1.D0 - YI
C     LES FFS
        TN(1) = C*B*UNS4
        TN(2) = C*D*UNS4
        TN(3) = A*D*UNS4
        TN(4) = A*B*UNS4
C     LES DD 1ER / XI
        DR(1,1) = -B*UNS4
        DR(1,2) = -D*UNS4
        DR(1,3) = D*UNS4
        DR(1,4) = B*UNS4
C     LES DD 1ER / YI
        DR(2,1) = C*UNS4
        DR(2,2) = -C*UNS4
        DR(2,3) = -A*UNS4
        DR(2,4) = A*UNS4
C     LES DD 2ER / XIXI
        DDR(1,1) = 0.D0
        DDR(1,2) = 0.D0
        DDR(1,3) = 0.D0
        DDR(1,4) = 0.D0
C     LES DD 2ER / YIYI
        DDR(2,1) = 0.D0
        DDR(2,2) = 0.D0
        DDR(2,3) = 0.D0
        DDR(2,4) = 0.D0
C                  / XIYI
        DDR(3,1) = -UNS4
        DDR(3,2) = UNS4
        DDR(3,3) = -UNS4
        DDR(3,4) = UNS4

       ELSE IF (ALIAS(1:3).EQ.'QU8') THEN

        TN(1) = (1.D0+YI)* (1.D0-XI)* (-1.D0-XI+YI)*0.25D0
        TN(2) = (1.D0-YI)* (1.D0-XI)* (-1.D0-XI-YI)*0.25D0
        TN(3) = (1.D0-YI)* (1.D0+XI)* (-1.D0+XI-YI)*0.25D0
        TN(4) = (1.D0+YI)* (1.D0+XI)* (-1.D0+XI+YI)*0.25D0
        TN(5) = (1.D0-YI)* (1.D0-XI)* (1.D0+YI)*0.5D0
        TN(6) = (1.D0-YI)* (1.D0-XI)* (1.D0+XI)*0.5D0
        TN(7) = (1.D0-YI)* (1.D0+XI)* (1.D0+YI)*0.5D0
        TN(8) = (1.D0+YI)* (1.D0-XI)* (1.D0+XI)*0.5D0

C     LES DD 1ER / XI

        DR(1,1) = 0.25D0* (1.D0+YI)* (2.D0*XI-YI)
        DR(1,2) = 0.25D0* (1.D0-YI)* (2.D0*XI+YI)
        DR(1,3) = 0.25D0* (1.D0-YI)* (2.D0*XI-YI)
        DR(1,4) = 0.25D0* (1.D0+YI)* (2.D0*XI+YI)
        DR(1,5) = -0.5D0* (1.D0-YI)* (1.D0+YI)
        DR(1,6) = - (1.D0-YI)*XI
        DR(1,7) = 0.5D0* (1.D0-YI)* (1.D0+YI)
        DR(1,8) = - (1.D0+YI)* (XI)
C     LES DD 1ER / YI
        DR(2,1) = 0.25D0* (1.D0-XI)* (2.D0*YI-XI)
        DR(2,2) = 0.25D0* (1.D0-XI)* (2.D0*YI+XI)
        DR(2,3) = 0.25D0* (1.D0+XI)* (2.D0*YI-XI)
        DR(2,4) = 0.25D0* (1.D0+XI)* (2.D0*YI+XI)
        DR(2,5) = - (1.D0-XI)*YI
        DR(2,6) = -0.5D0* (1.D0-XI)* (1.D0+XI)
        DR(2,7) = - (1.D0+XI)*YI
        DR(2,8) = (1.D0+XI)* (1.D0-XI)*0.5D0
C     LES DD 2ER / XIXI
        DDR(1,1) = 0.5D0* (1.D0+YI)
        DDR(1,2) = 0.5D0* (1.D0-YI)
        DDR(1,3) = 0.5D0* (1.D0-YI)
        DDR(1,4) = 0.5D0* (1.D0+YI)
        DDR(1,5) = 0.D0
        DDR(1,6) = - (1.D0-YI)
        DDR(1,7) = 0.D0
        DDR(1,8) = - (1.D0+YI)
C     LES DD 2ER / YIYI
        DDR(2,1) = 0.5D0* (1.D0-XI)
        DDR(2,2) = 0.5D0* (1.D0-XI)
        DDR(2,3) = 0.5D0* (1.D0+XI)
        DDR(2,4) = 0.5D0* (1.D0+XI)
        DDR(2,5) = - (1.D0-XI)
        DDR(2,6) = 0.D0
        DDR(2,7) = - (1.D0+XI)
        DDR(2,8) = 0.D0
C     LES DD 2ER / XIYI
        DDR(3,1) = 0.25D0* (-1.D0-2.D0*YI+2.D0*XI)
        DDR(3,2) = 0.25D0* (1.D0-2.D0*YI-2.D0*XI)
        DDR(3,3) = 0.25D0* (-1.D0+2.D0*YI-2.D0*XI)
        DDR(3,4) = 0.25D0* (1.D0+2.D0*YI+2.D0*XI)
        DDR(3,5) = YI
        DDR(3,6) = XI
        DDR(3,7) = -YI
        DDR(3,8) = -XI

C_______________________________________________________________________

      ELSE IF (ALIAS(1:3).EQ.'QU9') THEN

        TN(1) = AL31(XI)*AL33(YI)
        TN(2) = AL31(XI)*AL31(YI)
        TN(3) = AL33(XI)*AL31(YI)
        TN(4) = AL33(XI)*AL33(YI)
        TN(5) = AL31(XI)*AL32(YI)
        TN(6) = AL32(XI)*AL31(YI)
        TN(7) = AL33(XI)*AL32(YI)
        TN(8) = AL32(XI)*AL33(YI)
        TN(9) = AL32(XI)*AL32(YI)

        DR(1,1) = DAL31(XI)*AL33(YI)
        DR(2,1) = AL31(XI)*DAL33(YI)
        DR(1,2) = DAL31(XI)*AL31(YI)
        DR(2,2) = AL31(XI)*DAL31(YI)
        DR(1,3) = DAL33(XI)*AL31(YI)
        DR(2,3) = AL33(XI)*DAL31(YI)
        DR(1,4) = DAL33(XI)*AL33(YI)
        DR(2,4) = AL33(XI)*DAL33(YI)
        DR(1,5) = DAL31(XI)*AL32(YI)
        DR(2,5) = AL31(XI)*DAL32(YI)
        DR(1,6) = DAL32(XI)*AL31(YI)
        DR(2,6) = AL32(XI)*DAL31(YI)
        DR(1,7) = DAL33(XI)*AL32(YI)
        DR(2,7) = AL33(XI)*DAL32(YI)
        DR(1,8) = DAL32(XI)*AL33(YI)
        DR(2,8) = AL32(XI)*DAL33(YI)
        DR(1,9) = DAL32(XI)*AL32(YI)
        DR(2,9) = AL32(XI)*DAL32(YI)
C
        DDR(1,1) = 1.D0*AL33(YI)
        DDR(1,2) = 1.D0*AL31(YI)
        DDR(1,3) = 1.D0*AL31(YI)
        DDR(1,4) = 1.D0*AL33(YI)
        DDR(1,5) = 1.D0*AL32(YI)
        DDR(1,6) = -2.D0*AL31(YI)
        DDR(1,7) = 1.D0*AL32(YI)
        DDR(1,8) = -2.D0*AL33(YI)
        DDR(1,9) = -2.D0*AL32(YI)
C
        DDR(2,1) = AL31(XI)*1.D0
        DDR(2,2) = AL31(XI)*1.D0
        DDR(2,3) = AL33(XI)*1.D0
        DDR(2,4) = AL33(XI)*1.D0
        DDR(2,5) = AL31(XI)* (-2.D0)
        DDR(2,6) = AL32(XI)*1.D0
        DDR(2,7) = AL33(XI)* (-2.D0)
        DDR(2,8) = AL32(XI)*1.D0
        DDR(2,9) = AL32(XI)* (-2.D0)
C
        DDR(3,1) = DAL31(XI)*DAL33(YI)
        DDR(3,2) = DAL31(XI)*DAL31(YI)
        DDR(3,3) = DAL33(XI)*DAL31(YI)
        DDR(3,4) = DAL33(XI)*DAL33(YI)
        DDR(3,5) = DAL31(XI)*DAL32(YI)
        DDR(3,6) = DAL32(XI)*DAL31(YI)
        DDR(3,7) = DAL33(XI)*DAL32(YI)
        DDR(3,8) = DAL32(XI)*DAL33(YI)
        DDR(3,9) = DAL32(XI)*DAL32(YI)
        ELSE
       CALL UTMESS ('F','CALFFD',
     &               'TYPE DE MAILL INCONNUE')

       END IF
       END
