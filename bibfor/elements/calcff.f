      SUBROUTINE CALCFF(ALIAS,XI,YI,ZI,XIN,YIN,ZIN,TN,AJ)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 09/02/2004   AUTEUR REZETTE C.REZETTE 
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
C TOLE CRP_20
C.......................................................................
C
C BUT:   CALCUL DES FONCTIONS DE FORMES ET DE LEURS DERIVEES
C        AU POINT DE COORDONNEES XI,YI,ZI
C
C ENTREES  ---> ALIAS       : NOM D'ALIAS DE L'ELEMENT
C          ---> XI,YI,ZI    : POINT DE CALCUL DES F FORMES ET DERIVEES
C          ---> XIN,YIN,ZIN : COORDONNEES INTRINSEQUES
C
C SORTIES  <--- TN  : FONCTIONS DE FORMES EN XI,YI,ZI
C          <--- AJ  : DERIVEES DES F FORMES EN XI,YI,ZI
C.......................................................................
C
      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER*8  ALIAS
      REAL*8       TN(1),AJ(3,1),XIN(1),YIN(1),ZIN(1)
C
      AL31(X)  = 0.5D00 * X * (X-1.D00)
      AL32(X)  = -(X+1.D00) * (X-1.D00)
      AL33(X)  = 0.5D00 * X * (X+1.D00)
      DAL31(U) = 0.5D00 * (2.D00 * U -1.D00)
      DAL32(U) = -2.D00 * U
      DAL33(U) = 0.5D00 * (2.D00 * U +1.D00)
C
      ZERO   = 0.0D0
      UNDEMI = 0.5D0
      UN     = 1.0D0
      DEUX   = 2.0D0
      TROIS  = 3.0D0
      QUATRE = 4.0D0
C_______________________________________________________________________
C
      IF ( ALIAS .EQ. 'HEXA8'.OR. ALIAS .EQ. 'HEXA8_D' ) THEN
C
        UNS8 = 1.D00 / 8.D00
        UN   = 1.D00
        DO 1 I=1,8
         X0=XI*XIN(I)
         Y0=YI*YIN(I)
         Z0=ZI*ZIN(I)
           TN(I) = (UN+X0) * (UN+Y0) * (UN+Z0) * UNS8
           AJ(1,I) = XIN(I) * (UN+Y0) * (UN+Z0) * UNS8
           AJ(2,I) = YIN(I) * (UN+X0) * (UN+Z0) * UNS8
           AJ(3,I) = ZIN(I) * (UN+X0) * (UN+Y0) * UNS8
1       CONTINUE
C_______________________________________________________________________
C
      ELSE IF ( ALIAS.EQ.'HEXA20'.OR.
     >          ALIAS(1:7).EQ.'HEXA20D'.OR.
     >          ALIAS.EQ.'HEXS20' ) THEN
        UNS8 = 1.D00 / 8.D00
        UNS4 = 1.D00 / 4.D00
        UN   = 1.D00
        DE   = 2.D00
C
C   NOEUDS SOMMETS
C
      DO 2 I=1,8
         X0=XI*XIN(I)
         Y0=YI*YIN(I)
         Z0=ZI*ZIN(I)
         TN(I) = (UN+X0) * (UN+Y0) * (UN+Z0) * (X0+Y0+Z0-DE) * UNS8
         AJ(1,I) = (UN+Y0) * (UN+Z0) * (DE*X0+Y0+Z0-UN) * XIN(I) * UNS8
         AJ(2,I) = (UN+X0) * (UN+Z0) * (X0+DE*Y0+Z0-UN) * YIN(I) * UNS8
         AJ(3,I) = (UN+X0) * (UN+Y0) * (X0+Y0+DE*Z0-UN) * ZIN(I) * UNS8
2     CONTINUE
C
C   NOEUDS MILIEUX
C
      DO 3 I=1,2
      DO 3 J=1,2
         IXJ=9+(I-1)*8+(J-1)*2
         IYJ=IXJ+1
      TN(IXJ) = (UN-XI*XI) * (UN+YI*YIN(IXJ)) * (UN+ZI*ZIN(IXJ)) * UNS4
      TN(IYJ) = (UN-YI*YI) * (UN+XI*XIN(IYJ)) * (UN+ZI*ZIN(IYJ)) * UNS4
C
       AJ(1,IXJ) = -DE*XI * (UN+YI*YIN(IXJ)) * (UN+ZI*ZIN(IXJ)) * UNS4
       AJ(2,IXJ) = YIN(IXJ) * (UN-XI*XI) * (UN+ZI*ZIN(IXJ)) * UNS4
       AJ(3,IXJ) = ZIN(IXJ) * (UN-XI*XI) * (UN+YI*YIN(IXJ)) * UNS4
       AJ(1,IYJ) = XIN(IYJ) * (UN-YI*YI) * (UN+ZI*ZIN(IYJ)) * UNS4
       AJ(2,IYJ) = -DE * YI * (UN+XI*XIN(IYJ)) *(UN+ZI*ZIN(IYJ)) * UNS4
       AJ(3,IYJ) = ZIN(IYJ) * (UN+XI*XIN(IYJ)) * (UN-YI*YI) * UNS4
3     CONTINUE
C
      DO 4 I=13,16
       TN(I) = (UN-ZI*ZI) * (UN+YI*YIN(I)) * (UN+XI*XIN(I)) * UNS4
       AJ(1,I) = XIN(I) * (UN-ZI*ZI) * (UN+YI*YIN(I)) * UNS4
       AJ(2,I) = YIN(I) * (UN+XI*XIN(I)) * (UN-ZI*ZI) * UNS4
       AJ(3,I) = -DE * ZI * (UN+YI*YIN(I)) * (UN+XI*XIN(I)) * UNS4
4     CONTINUE
C_______________________________________________________________________
C
      ELSE IF ( ALIAS .EQ. 'HEXA27' ) THEN
C
        TN(1)  = AL31(XI) * AL31(YI) * AL31(ZI)
        TN(2)  = AL33(XI) * AL31(YI) * AL31(ZI)
        TN(3)  = AL33(XI) * AL33(YI) * AL31(ZI)
        TN(4)  = AL31(XI) * AL33(YI) * AL31(ZI)
        TN(5)  = AL31(XI) * AL31(YI) * AL33(ZI)
        TN(6)  = AL33(XI) * AL31(YI) * AL33(ZI)
        TN(7)  = AL33(XI) * AL33(YI) * AL33(ZI)
        TN(8)  = AL31(XI) * AL33(YI) * AL33(ZI)
        TN(9)  = AL32(XI) * AL31(YI) * AL31(ZI)
        TN(10) = AL33(XI) * AL32(YI) * AL31(ZI)
        TN(11) = AL32(XI) * AL33(YI) * AL31(ZI)
        TN(12) = AL31(XI) * AL32(YI) * AL31(ZI)
        TN(13) = AL31(XI) * AL31(YI) * AL32(ZI)
        TN(14) = AL33(XI) * AL31(YI) * AL32(ZI)
        TN(15) = AL33(XI) * AL33(YI) * AL32(ZI)
        TN(16) = AL31(XI) * AL33(YI) * AL32(ZI)
        TN(17) = AL32(XI) * AL31(YI) * AL33(ZI)
        TN(18) = AL33(XI) * AL32(YI) * AL33(ZI)
        TN(19) = AL32(XI) * AL33(YI) * AL33(ZI)
        TN(20) = AL31(XI) * AL32(YI) * AL33(ZI)
        TN(21) = AL32(XI) * AL32(YI) * AL31(ZI)
        TN(22) = AL32(XI) * AL31(YI) * AL32(ZI)
        TN(23) = AL33(XI) * AL32(YI) * AL32(ZI)
        TN(24) = AL32(XI) * AL33(YI) * AL32(ZI)
        TN(25) = AL31(XI) * AL32(YI) * AL32(ZI)
        TN(26) = AL32(XI) * AL32(YI) * AL33(ZI)
        TN(27) = AL32(XI) * AL32(YI) * AL32(ZI)
C
        AJ(1,1)  = DAL31(XI) *  AL31(YI) *  AL31(ZI)
        AJ(2,1)  =  AL31(XI) * DAL31(YI) *  AL31(ZI)
        AJ(3,1)  =  AL31(XI) *  AL31(YI) * DAL31(ZI)
        AJ(1,2)  = DAL33(XI) *  AL31(YI) *  AL31(ZI)
        AJ(2,2)  =  AL33(XI) * DAL31(YI) *  AL31(ZI)
        AJ(3,2)  =  AL33(XI) *  AL31(YI) * DAL31(ZI)
        AJ(1,3)  = DAL33(XI) *  AL33(YI) *  AL31(ZI)
        AJ(2,3)  =  AL33(XI) * DAL33(YI) *  AL31(ZI)
        AJ(3,3)  =  AL33(XI) *  AL33(YI) * DAL31(ZI)
        AJ(1,4)  = DAL31(XI) *  AL33(YI) *  AL31(ZI)
        AJ(2,4)  =  AL31(XI) * DAL33(YI) *  AL31(ZI)
        AJ(3,4)  =  AL31(XI) *  AL33(YI) * DAL31(ZI)
        AJ(1,5)  = DAL31(XI) *  AL31(YI) *  AL33(ZI)
        AJ(2,5)  =  AL31(XI) * DAL31(YI) *  AL33(ZI)
        AJ(3,5)  =  AL31(XI) *  AL31(YI) * DAL33(ZI)
        AJ(1,6)  = DAL33(XI) *  AL31(YI) *  AL33(ZI)
        AJ(2,6)  =  AL33(XI) * DAL31(YI) *  AL33(ZI)
        AJ(3,6)  =  AL33(XI) *  AL31(YI) * DAL33(ZI)
        AJ(1,7)  = DAL33(XI) *  AL33(YI) *  AL33(ZI)
        AJ(2,7)  =  AL33(XI) * DAL33(YI) *  AL33(ZI)
        AJ(3,7)  =  AL33(XI) *  AL33(YI) * DAL33(ZI)
        AJ(1,8)  = DAL31(XI) *  AL33(YI) *  AL33(ZI)
        AJ(2,8)  =  AL31(XI) * DAL33(YI) *  AL33(ZI)
        AJ(3,8)  =  AL31(XI) *  AL33(YI) * DAL33(ZI)
        AJ(1,9)  = DAL32(XI) *  AL31(YI) *  AL31(ZI)
        AJ(2,9)  =  AL32(XI) * DAL31(YI) *  AL31(ZI)
        AJ(3,9)  =  AL32(XI) *  AL31(YI) * DAL31(ZI)
        AJ(1,10) = DAL33(XI) *  AL32(YI) *  AL31(ZI)
        AJ(2,10) =  AL33(XI) * DAL32(YI) *  AL31(ZI)
        AJ(3,10) =  AL33(XI) *  AL32(YI) * DAL31(ZI)
        AJ(1,11) = DAL32(XI) *  AL33(YI) *  AL31(ZI)
        AJ(2,11) =  AL32(XI) * DAL33(YI) *  AL31(ZI)
        AJ(3,11) =  AL32(XI) *  AL33(YI) * DAL31(ZI)
        AJ(1,12) = DAL31(XI) *  AL32(YI) *  AL31(ZI)
        AJ(2,12) =  AL31(XI) * DAL32(YI) *  AL31(ZI)
        AJ(3,12) =  AL31(XI) *  AL32(YI) * DAL31(ZI)
        AJ(1,13) = DAL31(XI) *  AL31(YI) *  AL32(ZI)
        AJ(2,13) =  AL31(XI) * DAL31(YI) *  AL32(ZI)
        AJ(3,13) =  AL31(XI) *  AL31(YI) * DAL32(ZI)
        AJ(1,14) = DAL33(XI) *  AL31(YI) *  AL32(ZI)
        AJ(2,14) =  AL33(XI) * DAL31(YI) *  AL32(ZI)
        AJ(3,14) =  AL33(XI) *  AL31(YI) * DAL32(ZI)
        AJ(1,15) = DAL33(XI) *  AL33(YI) *  AL32(ZI)
        AJ(2,15) =  AL33(XI) * DAL33(YI) *  AL32(ZI)
        AJ(3,15) =  AL33(XI) *  AL33(YI) * DAL32(ZI)
        AJ(1,16) = DAL31(XI) *  AL33(YI) *  AL32(ZI)
        AJ(2,16) =  AL31(XI) * DAL33(YI) *  AL32(ZI)
        AJ(3,16) =  AL31(XI) *  AL33(YI) * DAL32(ZI)
        AJ(1,17) = DAL32(XI) *  AL31(YI) *  AL33(ZI)
        AJ(2,17) =  AL32(XI) * DAL31(YI) *  AL33(ZI)
        AJ(3,17) =  AL32(XI) *  AL31(YI) * DAL33(ZI)
        AJ(1,18) = DAL33(XI) *  AL32(YI) *  AL33(ZI)
        AJ(2,18) =  AL33(XI) * DAL32(YI) *  AL33(ZI)
        AJ(3,18) =  AL33(XI) *  AL32(YI) * DAL33(ZI)
        AJ(1,19) = DAL32(XI) *  AL33(YI) *  AL33(ZI)
        AJ(2,19) =  AL32(XI) * DAL33(YI) *  AL33(ZI)
        AJ(3,19) =  AL32(XI) *  AL33(YI) * DAL33(ZI)
        AJ(1,20) = DAL31(XI) *  AL32(YI) *  AL33(ZI)
        AJ(2,20) =  AL31(XI) * DAL32(YI) *  AL33(ZI)
        AJ(3,20) =  AL31(XI) *  AL32(YI) * DAL33(ZI)
        AJ(1,21) = DAL32(XI) *  AL32(YI) *  AL31(ZI)
        AJ(2,21) =  AL32(XI) * DAL32(YI) *  AL31(ZI)
        AJ(3,21) =  AL32(XI) *  AL32(YI) * DAL31(ZI)
        AJ(1,22) = DAL32(XI) *  AL31(YI) *  AL32(ZI)
        AJ(2,22) =  AL32(XI) * DAL31(YI) *  AL32(ZI)
        AJ(3,22) =  AL32(XI) *  AL31(YI) * DAL32(ZI)
        AJ(1,23) = DAL33(XI) *  AL32(YI) *  AL32(ZI)
        AJ(2,23) =  AL33(XI) * DAL32(YI) *  AL32(ZI)
        AJ(3,23) =  AL33(XI) *  AL32(YI) * DAL32(ZI)
        AJ(1,24) = DAL32(XI) *  AL33(YI) *  AL32(ZI)
        AJ(2,24) =  AL32(XI) * DAL33(YI) *  AL32(ZI)
        AJ(3,24) =  AL32(XI) *  AL33(YI) * DAL32(ZI)
        AJ(1,25) = DAL31(XI) *  AL32(YI) *  AL32(ZI)
        AJ(2,25) =  AL31(XI) * DAL32(YI) *  AL32(ZI)
        AJ(3,25) =  AL31(XI) *  AL32(YI) * DAL32(ZI)
        AJ(1,26) = DAL32(XI) *  AL32(YI) *  AL33(ZI)
        AJ(2,26) =  AL32(XI) * DAL32(YI) *  AL33(ZI)
        AJ(3,26) =  AL32(XI) *  AL32(YI) * DAL33(ZI)
        AJ(1,27) = DAL32(XI) *  AL32(YI) *  AL32(ZI)
        AJ(2,27) =  AL32(XI) * DAL32(YI) *  AL32(ZI)
        AJ(3,27) =  AL32(XI) *  AL32(YI) * DAL32(ZI)
C_______________________________________________________________________
C
      ELSE IF ( ALIAS(1:6) .EQ. 'PENTA6'.OR.
     >          ALIAS(1:7).EQ.'PENTA6D') THEN
C
        AL1=YI
        AL2=ZI
        AL3=(1.D00-YI-ZI)
        DO 25 I=1,2
          II=3*(I-1)
           TN(II+1)   = AL1 * (1.D00 + XI*XIN(II+1)  )*0.5D00
           TN(II+2)   = AL2 * (1.D00 + XI*XIN(II+2)  )*0.5D00
           TN(II+3)   = AL3 * (1.D00 + XI*XIN(II+3)  )*0.5D00
           AJ(1,II+1) = AL1 * XIN(II+1) * 0.5D00
           AJ(2,II+1) = (1.D00 + XI*XIN(II+1)) * 0.5D00
           AJ(3,II+1) = 0.D+00
           AJ(1,II+2) = AL2 * XIN(II+2) * 0.5D00
           AJ(2,II+2) = 0.D+00
           AJ(3,II+2) = (1.D00 + XI * XIN(II+2)) * 0.5D00
           AJ(1,II+3) = AL3 * XIN(II+3) * 0.5D00
           AJ(2,II+3) = -(1.D00 + XI * XIN(II+3)) * 0.5D00
           AJ(3,II+3) = -(1.D00 + XI * XIN(II+3)) * 0.5D00
25      CONTINUE
C_______________________________________________________________________
C
      ELSE IF ( ALIAS .EQ. 'PENTC6' ) THEN
C
        AL1=XI
        AL2=YI
        AL3=(1.D00-YI-XI)
        DO 26 I=1,2
          II=3*(I-1)
           TN(II+1)   = AL1 * (1.D00 + ZI*ZIN(II+1)  )*0.5D00
           TN(II+2)   = AL2 * (1.D00 + ZI*ZIN(II+2)  )*0.5D00
           TN(II+3)   = AL3 * (1.D00 + ZI*ZIN(II+3)  )*0.5D00
           AJ(1,II+1) = (1.D00 + ZI*ZIN(II+1)) * 0.5D00
           AJ(2,II+1) = 0.D+00
           AJ(3,II+1) = AL1 * ZIN(II+1) * 0.5D00
           AJ(1,II+2) = 0.D+00
           AJ(2,II+2) = (1.D00 + ZI * ZIN(II+2)) * 0.5D00
           AJ(3,II+2) = AL2 * ZIN(II+2) * 0.5D00
           AJ(1,II+3) = -(1.D00 + ZI * ZIN(II+3)) * 0.5D00
           AJ(2,II+3) = -(1.D00 + ZI * ZIN(II+3)) * 0.5D00
           AJ(3,II+3) = AL3 * ZIN(II+3) * 0.5D00
26      CONTINUE
C_______________________________________________________________________
C
      ELSE IF ( ALIAS .EQ. 'PENTA15'.OR.
     >          ALIAS(1:8).EQ.'PENTA15D') THEN
C
        UN = 1.D00
        DE = 2.D00
        QU = 4.D00
        AL1=YI
        AL2=ZI
        AL3=UN-YI-ZI
C
C   FONCTIONS DE FORME D'ASKA
C
      TN(1) = AL1* (DE*AL1-UN)* (UN+XI*XIN(1))/DE - AL1* (UN-XI*XI) /DE
      TN(2) = AL2* (DE*AL2-UN)* (UN+XI*XIN(2))/DE - AL2* (UN-XI*XI) /DE
      TN(3) = AL3* (DE*AL3-UN)* (UN+XI*XIN(3))/DE - AL3* (UN-XI*XI) /DE
      TN(4) = AL1* (DE*AL1-UN)* (UN+XI*XIN(4))/DE - AL1* (UN-XI*XI) /DE
      TN(5) = AL2* (DE*AL2-UN)* (UN+XI*XIN(5))/DE - AL2* (UN-XI*XI) /DE
      TN(6) = AL3* (DE*AL3-UN)* (UN+XI*XIN(6))/DE - AL3* (UN-XI*XI) /DE
      TN(7) = DE * AL1 * AL2 * (UN + XI * XIN(7))
      TN(8) = DE * AL2 * AL3 * (UN + XI * XIN(8))
      TN(9) = DE * AL1 * AL3 * (UN + XI * XIN(9))
      TN(10) = AL1 * (UN-XI*XI)
      TN(11) = AL2 * (UN-XI*XI)
      TN(12) = AL3 * (UN-XI*XI)
      TN(13) = DE * AL1 * AL2 * (UN+XI*XIN(13))
      TN(14) = DE * AL2 * AL3 * (UN+XI*XIN(14))
      TN(15) = DE * AL1 * AL3 * (UN+XI*XIN(15))
      AJ(1,1) = XIN(1) * AL1 * (DE*AL1-UN) / DE + AL1 * XI
      AJ(2,1) = (UN+XI*XIN(1)) * (QU*AL1-UN)/DE - (UN-XI*XI)/DE
      AJ(3,1) = 0.D+00
      AJ(1,2) = XIN(2) * AL2 * (DE*AL2-UN) / DE + AL2 * XI
      AJ(3,2) = (UN+XI*XIN(2)) * (QU*AL2-UN)/DE - (UN-XI*XI)/DE
      AJ(2,2) = 0.D+00
      AJ(1,3) = XIN(3) * AL3 * (DE*AL3-UN) / DE + AL3 * XI
      AJ(2,3) = -(UN+XI*XIN(3)) * (QU*AL3-UN)/DE + (UN-XI*XI)/DE
      AJ(3,3) = -(UN+XI*XIN(3)) * (QU*AL3-UN)/DE + (UN-XI*XI)/DE
      AJ(1,4) = XIN(4) * AL1 * (DE*AL1-UN) / DE + AL1 * XI
      AJ(2,4) = (UN+XI*XIN(4)) * (QU*AL1-UN)/DE - (UN-XI*XI)/DE
      AJ(3,4) = 0.D+00
      AJ(1,5) = XIN(5) * AL2 * (DE*AL2-UN) / DE + AL2 * XI
      AJ(2,5) = 0.D+00
      AJ(3,5) = (UN+XI*XIN(5)) * (QU*AL2-UN)/DE - (UN-XI*XI)/DE
        AJ(1,6) = XIN(6) * AL3 * (DE*AL3-UN) / DE + AL3 * XI
        AJ(2,6) = -(UN+XI*XIN(6)) * (QU*AL3-UN)/DE + (UN-XI*XI)/DE
        AJ(3,6) = -(UN+XI*XIN(6)) * (QU*AL3-UN)/DE + (UN-XI*XI)/DE
        AJ(1,7) = DE * AL1 * AL2 * XIN(7)
        AJ(2,7) = DE * AL2 * (UN + XI * XIN(7))
        AJ(3,7) = DE * AL1 * (UN + XI * XIN(7))
        AJ(1,8) = DE * AL3 * AL2 * XIN(8)
        AJ(2,8) = -DE * AL2 * (UN + XI * XIN(8))
        AJ(3,8) = (DE * AL3 - DE * AL2) * (UN + XI * XIN(8))
        AJ(1,9) = DE * AL1 * AL3 * XIN(9)
        AJ(2,9) = (DE * AL3 - DE * AL1) * (UN + XI * XIN(9))
        AJ(3,9) = -DE * AL1 * (UN + XI * XIN(9))
        AJ(1,10) = -DE * AL1 * XI
        AJ(2,10) = (UN - XI * XI)
        AJ(3,10) = 0.D+00
        AJ(1,11) = -DE * AL2 * XI
        AJ(2,11) = 0.D+00
        AJ(3,11) = (UN - XI * XI)
        AJ(1,12) = -DE * AL3 * XI
        AJ(2,12) = -(UN - XI * XI)
        AJ(3,12) = -(UN - XI * XI)
        AJ(1,13) = DE * AL1 * AL2 * XIN(13)
        AJ(2,13) = DE * AL2 * (UN + XI * XIN(13))
        AJ(3,13) = DE * AL1 * (UN + XI * XIN(13))
        AJ(1,14) = DE * AL3 * AL2 * XIN(14)
        AJ(2,14) = -DE * AL2 * (UN + XI * XIN(14))
        AJ(3,14) = (DE * AL3 - DE * AL2) * (UN + XI * XIN(14))
        AJ(1,15) = DE * AL1 * AL3 * XIN(15)
        AJ(2,15) = (DE * AL3 - DE * AL1) * (UN + XI * XIN(15))
        AJ(3,15) = -DE * AL1 * (UN + XI * XIN(15))
C
C   FONCTIONS DE FORME DE TOUZOT
C
C     TN(1) = AL1 * (UN-DE*AL1) * (UN-XI)*XI/DE
C     TN(2) = AL2 * (UN-DE*AL2) * (UN-XI)*XI/DE
C     TN(3) = -AL3 * (AL3-AL1-AL2) * (UN-XI)*XI/DE
C     TN(4) = -AL1 * (UN-DE*AL1) * (UN+XI)*XI/DE
C     TN(5) = -AL2 * (UN-DE*AL2) * (UN+XI)*XI/DE
C     TN(6) = AL3 * (AL3-AL1-AL2) * (UN+XI)*XI/DE
C
C     TN(7) = -QU * AL1 * AL2 * (UN - XI)*XI/DE
C     TN(8) = -QU * AL2 * AL3 * (UN - XI)*XI/DE
C     TN(9) = -QU * AL1 * AL3 * (UN - XI)*XI/DE
C
C     TN(10) = AL1 * (UN-XI*XI)
C     TN(11) = AL2 * (UN-XI*XI)
C     TN(12) = AL3 * (UN-XI*XI)
C
C     TN(13) =  QU * AL1 * AL2 * (UN + XI)*XI/DE
C     TN(14) =  QU * AL2 * AL3 * (UN + XI)*XI/DE
C     TN(15) =  QU * AL1 * AL3 * (UN + XI)*XI/DE
C
C     AJ(1,1) = AL1 * (UN-DE*AL1) * (UN-DE*XI) / DE
C     AJ(2,1) = (UN-XI) * XI * (UN-QU*AL1) /DE
C     AJ(3,1) = 0.D+00
C     AJ(1,2) = AL2 * (UN-DE*AL2) * (UN-DE*XI) / DE
C     AJ(2,2) = 0.D+00
C     AJ(3,2) = (UN-XI) * XI * (UN-QU*AL2) /DE
C     AJ(1,3) = - AL3 * (AL3-AL1-AL2) * (UN - DE*XI)/DE
C     AJ(2,3) = -(UN-XI) * XI * (-3.0D0 + QU * AL1 + QU * AL2) /DE
C     AJ(3,3) = AJ(2,3)
C     AJ(1,4) = - AL1 * (UN-DE*AL1) * (UN+DE*XI) / DE
C     AJ(2,4) = -(UN+XI) * XI * (UN-QU*AL1) /DE
C     AJ(3,4) = 0.D+00
C     AJ(1,5) = -AL2 * (UN-DE*AL2) * (UN+DE*XI) / DE
C     AJ(2,5) = 0.D+00
C     AJ(3,5) = -(UN+XI) * XI * (UN-QU*AL2) /DE
C     AJ(1,6) =   AL3 * (AL3-AL1-AL2) * (UN + DE*XI)/DE
C     AJ(2,6) =  (UN+XI) * XI * (-3.0D0 + QU * AL1 + QU * AL2) /DE
C     AJ(3,6) = AJ(2,6)
C
C       AJ(1,7) = - DE * AL1 * AL2 * (UN - DE * XI)
C       AJ(2,7) = - DE * AL2 * (UN - XI) * XI
C       AJ(3,7) = - DE * AL1 * (UN - XI) * XI
C       AJ(1,8) = - DE * AL2 * AL3 * (UN - DE * XI)
C       AJ(2,8) =   DE * AL2 * (UN - XI) * XI
C       AJ(3,8) = - DE * (AL3 - AL2) * (UN - XI) * XI
C       AJ(1,9) = - DE * AL1 * AL3 * (UN - DE * XI)
C       AJ(2,9) = - DE * (AL3 - AL1) * (UN - XI) * XI
C       AJ(3,9) =   DE * AL1 * (UN - XI) * XI
C       AJ(1,10) = -DE * AL1 * XI
C       AJ(2,10) = (UN - XI * XI)
C       AJ(3,10) = 0.D+00
C       AJ(1,11) = -DE * AL2 * XI
C       AJ(2,11) = 0.D+00
C       AJ(3,11) = (UN - XI * XI)
C       AJ(1,12) = -DE * AL3 * XI
C       AJ(2,12) = -(UN - XI * XI)
C       AJ(3,12) = -(UN - XI * XI)
C
C       AJ(1,13) =   DE * AL1 * AL2 * (UN + DE * XI)
C       AJ(2,13) =   DE * AL2 * (UN + XI) * XI
C       AJ(3,13) =   DE * AL1 * (UN + XI) * XI
C       AJ(1,14) =   DE * AL2 * AL3 * (UN + DE * XI)
C       AJ(2,14) = - DE * AL2 * (UN + XI) * XI
C       AJ(3,14) =   DE * (AL3 - AL2) * (UN + XI) * XI
C       AJ(1,15) =   DE * AL1 * AL3 * (UN + DE * XI)
C       AJ(2,15) =   DE * (AL3 - AL1) * (UN + XI) * XI
C       AJ(3,15) = - DE * AL1 * (UN + XI) * XI
C_______________________________________________________________________
C
      ELSE IF ( ALIAS(1:6) .EQ. 'TETRA4'.OR.
     >          ALIAS(1:7).EQ.'TETRA4D') THEN
C
        AL1 = XI
        AL2 = YI
        AL3 = ZI
        AL4 = 1.D00-XI-YI-ZI
        TN(1) = AL2
        TN(2) = AL3
        TN(3) = AL4
        TN(4) = AL1
        AJ(1,1) = 0.D+00
        AJ(2,1) = 1.D+00
        AJ(3,1) = 0.D+00
        AJ(1,2) = 0.D+00
        AJ(2,2) = 0.D+00
        AJ(3,2) = 1.D+00
        AJ(1,3) = -1.D+00
        AJ(2,3) = -1.D+00
        AJ(3,3) = -1.D+00
        AJ(1,4) = 1.D+00
        AJ(2,4) = 0.D+00
        AJ(3,4) = 0.D+00
C_______________________________________________________________________
C
      ELSE IF ( ALIAS .EQ. 'TETRA10'.OR.
     >          ALIAS(1:8).EQ.'TETRA10D' ) THEN
C
        AL1=XI
        AL2=YI
        AL3=ZI
        AL4=1.D+00-XI-YI-ZI
        TN(1) = (2.D+00 * AL2 - 1.D+00) * AL2
        TN(2) = (2.D+00 * AL3 - 1.D+00) * AL3
        TN(3) = (2.D+00 * AL4 - 1.D+00) * AL4
        TN(4) = (2.D+00 * AL1 - 1.D+00) * AL1
        TN(5) = 4.D+00 * AL3 * AL2
        TN(6) = 4.D+00 * AL3 * AL4
        TN(7) = 4.D+00 * AL4 * AL2
        TN(8) = 4.D+00 * AL1 * AL2
        TN(9) = 4.D+00 * AL1 * AL3
        TN(10) = 4.D+00 * AL1 * AL4
        AJ(1,1) = 0.D+00
        AJ(2,1) = 4.D+00 * AL2 - 1.D+00
        AJ(3,1) = 0.D+00
        AJ(1,2) = 0.D+00
        AJ(2,2) = 0.D+00
        AJ(3,2) = 4.D+00 * AL3 - 1.D+00
        AJ(1,3) = 1.D+00 -4.D+00 * AL4
        AJ(2,3) = 1.D+00 -4.D+00 * AL4
        AJ(3,3) = 1.D+00 -4.D+00 * AL4
        AJ(1,4) = 4.D+00 * AL1 - 1.D+00
        AJ(2,4) = 0.D+00
        AJ(3,4) = 0.D+00
        AJ(1,5) = 0.D+00
        AJ(2,5) = 4.D+00 * AL3
        AJ(3,5) = 4.D+00 * AL2
        AJ(1,6) = -4.D+00 * AL3
        AJ(2,6) = -4.D+00 * AL3
        AJ(3,6) = 4.D+00 * (AL4-AL3)
        AJ(1,7) = -4.D+00 * AL2
        AJ(2,7) = 4.D+00 * (AL4-AL2)
        AJ(3,7) = -4.D+00 * AL2
        AJ(1,8) = 4.D+00 * AL2
        AJ(2,8) = 4.D+00 * AL1
        AJ(3,8) = 0.D+00
        AJ(1,9) = 4.D+00 * AL3
        AJ(2,9) = 0.D+00
        AJ(3,9) = 4.D+00 * AL1
        AJ(1,10) = 4.D+00 * (AL4-AL1)
        AJ(2,10) = -4.D+00 * AL1
        AJ(3,10) = -4.D+00 * AL1
C_______________________________________________________________________
C
      ELSE IF ( ALIAS(1:6) .EQ. 'PYRAM5'.OR.
     >          ALIAS(1:7).EQ.'PYRAM5D') THEN
C
        ZI1 = UN - ZI
        ZI4 =(UN - ZI) * QUATRE
C
        PFACE1 = XI + YI + ZI - UN
        PFACE2 =-XI + YI + ZI - UN
        PFACE3 =-XI - YI + ZI - UN
        PFACE4 = XI - YI + ZI - UN
C
        IF (ABS(ZI-UN).LT.1.0D-6) THEN
           DO 10 I = 1, 5
              TN(I) = ZERO
              DO 11 J = 1, 2
                 AJ(J,I) = ZERO
 11           CONTINUE
 10        CONTINUE
C
           TN(5) = UN 
C
           AJ(1,1)  =  UNDEMI
           AJ(1,3)  = -UNDEMI
C
           AJ(2,2)  =  UNDEMI
           AJ(2,4)  = -UNDEMI
C
           AJ(3,1)  = -UNDEMI
           AJ(3,2)  = -UNDEMI
           AJ(3,3)  = -UNDEMI
           AJ(3,4)  = -UNDEMI
           AJ(3,5)  =  UN
C
        ELSE
C
           TN(1) = PFACE2*PFACE3/ZI4
           TN(2) = PFACE3*PFACE4/ZI4
           TN(3) = PFACE1*PFACE4/ZI4
           TN(4) = PFACE1*PFACE2/ZI4
           TN(5) = ZI
C
           AJ(1,1) = (-PFACE2-PFACE3)/ZI4
           AJ(1,2) = ( PFACE3-PFACE4)/ZI4
           AJ(1,3) = ( PFACE1+PFACE4)/ZI4
           AJ(1,4) = ( PFACE2-PFACE1)/ZI4
           AJ(1,5) =   ZERO
C
           AJ(2,1) = ( PFACE3-PFACE2)/ZI4
           AJ(2,2) = (-PFACE3-PFACE4)/ZI4
           AJ(2,3) = ( PFACE4-PFACE1)/ZI4
           AJ(2,4) = ( PFACE1+PFACE2)/ZI4
           AJ(2,5) =   ZERO
C
           AJ(3,1) = ( PFACE2+PFACE3+PFACE2*PFACE3/ZI1)/ZI4
           AJ(3,2) = ( PFACE3+PFACE4+PFACE3*PFACE4/ZI1)/ZI4
           AJ(3,3) = ( PFACE4+PFACE1+PFACE4*PFACE1/ZI1)/ZI4
           AJ(3,4) = ( PFACE1+PFACE2+PFACE1*PFACE2/ZI1)/ZI4
           AJ(3,5) =   UN
        ENDIF
C_______________________________________________________________________
C
      ELSE IF ( ALIAS .EQ. 'PYRAM13'.OR.
     >          ALIAS(1:8).EQ.'PYRAM13D' ) THEN
C
        ZI1 = UN - ZI
        ZI2 =(UN - ZI) * DEUX
C
        PFACE1 = XI + YI + ZI - UN
        PFACE2 =-XI + YI + ZI - UN
        PFACE3 =-XI - YI + ZI - UN
        PFACE4 = XI - YI + ZI - UN
C
        PMILI1 = XI - UNDEMI
        PMILI2 = YI - UNDEMI
        PMILI3 =-XI - UNDEMI
        PMILI4 =-YI - UNDEMI
C
        IF (ABS(ZI-UN).LT.1.0D-6) THEN
           DO 12 I = 1, 13
              TN(I) = ZERO
              DO 14 J = 1, 2
                 AJ(J,I) = ZERO
 14           CONTINUE
 12        CONTINUE
C
           TN(5) = UN 
C
           AJ(1,1)  = -UNDEMI
           AJ(1,3)  =  UNDEMI
           AJ(1,9)  =  DEUX
           AJ(1,11) = -DEUX
C
           AJ(2,2)  = -UNDEMI
           AJ(2,4)  =  UNDEMI
           AJ(2,10) =  DEUX
           AJ(2,12) = -DEUX
C
           AJ(3,1)   = UN/QUATRE
           AJ(3,2)   = UN/QUATRE
           AJ(3,3)   = UN/QUATRE
           AJ(3,4)   = UN/QUATRE
           AJ(3,6)   = ZERO
           AJ(3,7)   = ZERO
           AJ(3,8)   = ZERO
           AJ(3,9)   = ZERO
C
           DO 15 I = 10, 13
              AJ(3,I) = -UN
 15        CONTINUE
C
           AJ(3,5) = TROIS
C
        ELSE
C
         TN(1)  = PFACE2*PFACE3*PMILI1/ZI2
         TN(2)  = PFACE3*PFACE4*PMILI2/ZI2
         TN(3)  = PFACE4*PFACE1*PMILI3/ZI2
         TN(4)  = PFACE1*PFACE2*PMILI4/ZI2
         TN(5)  = DEUX*ZI*(ZI-UNDEMI)
         TN(6)  =-PFACE2*PFACE3*PFACE4/ZI2
         TN(7)  =-PFACE3*PFACE4*PFACE1/ZI2
         TN(8)  =-PFACE4*PFACE1*PFACE2/ZI2
         TN(9)  =-PFACE1*PFACE2*PFACE3/ZI2
         TN(10) =     ZI*PFACE2*PFACE3/ZI1
         TN(11) =     ZI*PFACE3*PFACE4/ZI1
         TN(12) =     ZI*PFACE4*PFACE1/ZI1
         TN(13) =     ZI*PFACE1*PFACE2/ZI1
C
         AJ(1,1) = ( PFACE2*PFACE3-(PFACE2+PFACE3)*PMILI1)/ZI2
         AJ(1,2) = ( PFACE3-PFACE4)*PMILI2/ZI2
         AJ(1,3) = ((PFACE1+PFACE4)*PMILI3- PFACE4*PFACE1)/ZI2
         AJ(1,4) = ( PFACE2-PFACE1)*PMILI4/ZI2
         AJ(1,5) =   ZERO
         AJ(1,6) = ( PFACE3*PFACE4 + PFACE2*PFACE4 - PFACE2*PFACE3)/ZI2
         AJ(1,7) = ( PFACE4*PFACE1 - PFACE3*PFACE1 - PFACE3*PFACE4)/ZI2
         AJ(1,8) = ( PFACE4*PFACE1 - PFACE1*PFACE2 - PFACE4*PFACE2)/ZI2
         AJ(1,9) = ( PFACE1*PFACE3 + PFACE1*PFACE2 - PFACE3*PFACE2)/ZI2
         AJ(1,10)= (-PFACE3-PFACE2)*ZI/ZI1
         AJ(1,11)= ( PFACE3-PFACE4)*ZI/ZI1
         AJ(1,12)= ( PFACE1+PFACE4)*ZI/ZI1
         AJ(1,13)= ( PFACE2-PFACE1)*ZI/ZI1
C
         AJ(2,1) = ( PFACE3-PFACE2)*PMILI1/ZI2
         AJ(2,2) = ( PFACE3*PFACE4 - (PFACE3+PFACE4)*PMILI2)/ZI2
         AJ(2,3) = ( PFACE4-PFACE1)*PMILI3/ZI2
         AJ(2,4) = ((PFACE2+PFACE1)*PMILI4 - PFACE1*PFACE2)/ZI2
         AJ(2,5) =   ZERO
         AJ(2,6) = ( PFACE2*PFACE4 + PFACE2*PFACE3 - PFACE3*PFACE4)/ZI2
         AJ(2,7) = ( PFACE4*PFACE1 + PFACE3*PFACE1 - PFACE3*PFACE4)/ZI2
         AJ(2,8) = ( PFACE1*PFACE2 - PFACE4*PFACE2 - PFACE4*PFACE1)/ZI2
         AJ(2,9) = ( PFACE1*PFACE2 - PFACE2*PFACE3 - PFACE1*PFACE3)/ZI2
         AJ(2,10)= ( PFACE3-PFACE2)*ZI/ZI1
         AJ(2,11)= (-PFACE4-PFACE3)*ZI/ZI1
         AJ(2,12)= ( PFACE4-PFACE1)*ZI/ZI1
         AJ(2,13)= ( PFACE2+PFACE1)*ZI/ZI1
C
         AJ(3,1) = ( PFACE2+PFACE3 + PFACE2*PFACE3/ZI1)*PMILI1/ZI2
         AJ(3,2) = ( PFACE3+PFACE4 + PFACE3*PFACE4/ZI1)*PMILI2/ZI2
         AJ(3,3) = ( PFACE1+PFACE4 + PFACE1*PFACE4/ZI1)*PMILI3/ZI2
         AJ(3,4) = ( PFACE2+PFACE1 + PFACE1*PFACE2/ZI1)*PMILI4/ZI2
         AJ(3,5)=   QUATRE * ZI - UN
         AJ(3,6) = -( PFACE3*PFACE4 + PFACE2*PFACE4 + PFACE2*PFACE3 +
     +                PFACE2*PFACE3*PFACE4/ZI1)/ZI2
         AJ(3,7) = -( PFACE4*PFACE1 + PFACE3*PFACE1 + PFACE3*PFACE4 +
     +                PFACE3*PFACE4*PFACE1/ZI1)/ZI2
         AJ(3,8) = -( PFACE1*PFACE2 + PFACE4*PFACE2 + PFACE4*PFACE1 +
     +                PFACE4*PFACE1*PFACE2/ZI1)/ZI2
         AJ(3,9) = -( PFACE2*PFACE3 + PFACE1*PFACE3 + PFACE1*PFACE2 +
     +                PFACE1*PFACE2*PFACE3/ZI1)/ZI2
         AJ(3,10)=   PFACE2*PFACE3/ZI1/ZI1 + (PFACE3+PFACE2)*ZI/ZI1
         AJ(3,11)=   PFACE3*PFACE4/ZI1/ZI1 + (PFACE4+PFACE3)*ZI/ZI1
         AJ(3,12)=   PFACE4*PFACE1/ZI1/ZI1 + (PFACE1+PFACE4)*ZI/ZI1
         AJ(3,13)=   PFACE1*PFACE2/ZI1/ZI1 + (PFACE1+PFACE2)*ZI/ZI1
        ENDIF
C_______________________________________________________________________
      ELSE IF ( ALIAS(1:5) .EQ. 'FACE4' ) THEN
C
        UNS4 = 0.25D00
        A = 1.D00 + XI
        B = 1.D00 + YI
        C = 1.D00 - XI
        D = 1.D00 - YI
        TN(1) = C*D*UNS4
        TN(2) = A*D*UNS4
        TN(3) = A*B*UNS4
        TN(4) = C*B*UNS4
        AJ(1,1) = -D*UNS4
        AJ(1,2) = D*UNS4
        AJ(1,3) = B*UNS4
        AJ(1,4) = -B*UNS4
        AJ(2,1) = -C*UNS4
        AJ(2,2) = -A*UNS4
        AJ(2,3) = A*UNS4
        AJ(2,4) = C*UNS4
C_______________________________________________________________________
C
      ELSE IF ( ALIAS .EQ. 'FACE8' ) THEN
C
        DO 30 I = 1 , 4
           TN(I) = (1.D00+XI*XIN(I)) * (1.D00+YI*YIN(I)) *
     &             (-1.D00+XI*XIN(I) + YI*YIN(I)) * 0.25D00
 30     CONTINUE
C
        TN(5) = (1.D00-XI*XI) * (1.D00+YI*YIN(5)) * 0.5D00
        TN(6) = (1.D00-YI*YI) * (1.D00+XI*XIN(6)) * 0.5D00
        TN(7) = (1.D00-XI*XI) * (1.D00+YI*YIN(7)) * 0.5D00
        TN(8) = (1.D00-YI*YI) * (1.D00+XI*XIN(8)) * 0.5D00
C
        DO 31 I = 1 , 4
           AJ(1,I) = XIN(I) * 0.25D00 * (1.D00+YI*YIN(I))
     &                * ( 2.D00*XI*XIN(I)+YI*YIN(I) )
           AJ(2,I) = YIN(I) * 0.25D00 * (1.D00+XI*XIN(I))
     &                * ( 2.D00*YI*YIN(I)+XI*XIN(I) )
 31     CONTINUE
C
        AJ(1,5) = -2.D00 * XI * (1.D00+YI*YIN(5)) * 0.5D00
        AJ(2,5) = (1.D00-XI*XI) * YIN(5) * 0.5D00
        AJ(1,6) = (1.D00-YI*YI) * XIN(6) * 0.5D00
        AJ(2,6) = -2.D00 * YI * (1.D00+XI*XIN(6)) * 0.5D00
        AJ(1,7) = -2.D00 * XI * (1.D00+YI*YIN(7)) * 0.5D00
        AJ(2,7) = (1.D00-XI*XI) * YIN(7) * 0.5D00
        AJ(1,8) = (1.D00-YI*YI) * XIN(8) * 0.5D00
        AJ(2,8) = -2.D00 * YI * (1.D00+XI*XIN(8)) * 0.5D00
C_______________________________________________________________________
C
      ELSE IF ( ALIAS .EQ. 'FACE9' ) THEN
C
        TN(1) = AL31(XI) * AL31(YI)
        TN(2) = AL33(XI) * AL31(YI)
        TN(3) = AL33(XI) * AL33(YI)
        TN(4) = AL31(XI) * AL33(YI)
        TN(5) = AL32(XI) * AL31(YI)
        TN(6) = AL33(XI) * AL32(YI)
        TN(7) = AL32(XI) * AL33(YI)
        TN(8) = AL31(XI) * AL32(YI)
        TN(9) = AL32(XI) * AL32(YI)
C
        AJ(1,1) = DAL31(XI) * AL31(YI)
        AJ(2,1) = AL31(XI) * DAL31(YI)
        AJ(1,2) = DAL33(XI) * AL31(YI)
        AJ(2,2) = AL33(XI) * DAL31(YI)
        AJ(1,3) = DAL33(XI) * AL33(YI)
        AJ(2,3) = AL33(XI) * DAL33(YI)
        AJ(1,4) = DAL31(XI) * AL33(YI)
        AJ(2,4) = AL31(XI) * DAL33(YI)
        AJ(1,5) = DAL32(XI) * AL31(YI)
        AJ(2,5) = AL32(XI) * DAL31(YI)
        AJ(1,6) = DAL33(XI) * AL32(YI)
        AJ(2,6) = AL33(XI) * DAL32(YI)
        AJ(1,7) = DAL32(XI) * AL33(YI)
        AJ(2,7) = AL32(XI) * DAL33(YI)
        AJ(1,8) = DAL31(XI) * AL32(YI)
        AJ(2,8) = AL31(XI) * DAL32(YI)
        AJ(1,9) = DAL32(XI) * AL32(YI)
        AJ(2,9) = AL32(XI) * DAL32(YI)
C_______________________________________________________________________
C
      ELSE IF ( ALIAS(1:5) .EQ. 'FACE3' ) THEN
C
        TN(1) = 1.D+00-XI-YI
        TN(2) = XI
        TN(3) = YI
        AJ(1,1) = -1.D+00
        AJ(1,2) = +1.D+00
        AJ(1,3) =  0.D+00
        AJ(2,1) = -1.D+00
        AJ(2,2) =  0.D+00
        AJ(2,3) = +1.D+00
C_______________________________________________________________________
C
      ELSE IF ( ALIAS .EQ. 'FACE6' ) THEN
C
        A = 1.D+00-XI-YI
        TN(1) = -A * (1.D+00 - 2.D+00*A)
        TN(2) = -XI * (1.D+00 - 2.D+00*XI)
        TN(3) = -YI * (1.D+00 - 2.D+00*YI)
        TN(4) = 4.D+00 * XI * A
        TN(5) = 4.D+00 * XI * YI
        TN(6) = 4.D+00 * YI * A
        AJ(1,1) = 1.D+00 - 4.D+00 * A
        AJ(1,2) = -1.D+00 + 4.D+00 * XI
        AJ(1,3) = 0.D+00
        AJ(1,4) = 4.D+00 * (A - XI)
        AJ(1,5) = 4.D+00 * YI
        AJ(1,6) = -4.D+00 * YI
        AJ(2,1) = 1.D+00 - 4.D+00 * A
        AJ(2,2) = 0.D+00
        AJ(2,3) = -1.D+00 + 4.D+00 * YI
        AJ(2,4) = -4.D+00 * XI
        AJ(2,5) = 4.D+00 * XI
        AJ(2,6) = 4.D+00 * (A - YI)
C
      ENDIF
      END
