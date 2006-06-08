      SUBROUTINE PIQSOU ( ALP, XC, YC, ZC, ZD, REP, RET, RIT, BET, ISO,
     &                    TYPSOU )
      IMPLICIT   NONE
      REAL*8              ALP, XC, YC, ZC, ZD, REP, RET, RIT, BET, ISO
      CHARACTER*8         TYPSOU
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SOUSTRUC  DATE 02/10/2002   AUTEUR F1BHHAJ J.ANGLES 
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
C TOLE  CRP_6
C     OPERATEUR: "DEFI_GROUP" , MOTCLE FACTEUR "EQUE_PIQUA"
C     AUTEUR Y. WADIER
C
C     CALCULE LES COORDONNEES DES POINTS DEFINISSANT LA SOUDURE
C
C IN  : ALP         : ANGLE ALPHA (CORRESPOND A L'ANGLE THETA DU
C                     CR MMN/97/136 ECRIT PAR Y. WADIER)
C IN  : ISO         : ISO = ESO POUR LES SOUDURES DE TYPE_1
C                     ISO = HSO POUR LES SOUDURES DE TYPE_2
C OUT : XC,YC,ZC,ZD
C-----------------------------------------------------------------------
C
      REAL*8  YA, ZA, GAM, EPS, FI, DEL, XD, YD, PIS2, R8PI, YI, ZI,
     &        JEU
C     ------------------------------------------------------------------
C
      PIS2 = R8PI() / 2.0D0
C
      IF ( TYPSOU .EQ. 'TYPE_1' ) THEN
        JEU = ISO - ( RET - RIT ) * TAN(BET)
C
C        XI  = ( REP + JEU ) * COS(ALP)
        YI  = ( REP + JEU ) * SIN(ALP)
        GAM = ASIN( YI / RIT )
        ZI  = RIT * COS(GAM)
C
C        XA  = REP * COS(ALP)
        YA  = REP * SIN(ALP)
        GAM = ASIN( YA / RIT )
C        ZA  = RIT * COS(GAM)
C
        EPS = SIN(ALP)*TAN(BET)
        FI  = EPS*ZI - YI
      ELSEIF ( TYPSOU .EQ. 'TYPE_2' ) THEN
        YA = REP * SIN(ALP)
        ZA = RIT * SQRT(1 - (YA/RIT)**2)
        EPS = SIN(ALP) * (ISO/(RET-RIT))
        FI  = EPS*ZA - YA
      ENDIF
      DEL = FI**2 - (1+EPS**2)*(FI**2-(RET*EPS)**2)
C
      YD = (-FI+SQRT(DEL))/(1+EPS**2)
C
      IF ( ALP .LT. 1.D-10 ) THEN
         XD = REP + ISO
      ELSEIF ( ABS(ALP-PIS2) .LT. 1.D-10 ) THEN
         XD = 0.0D0
      ELSE
         XD = YD / TAN(ALP)
      ENDIF
C
      ZD = SQRT( RET**2 - YD**2 )
C
      XC  = XD
      YC  = YD
      GAM = ASIN( YD / RIT )
      ZC  = RIT * COS(GAM)
C
      END
