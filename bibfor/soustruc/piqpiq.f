      SUBROUTINE PIQPIQ ( XP, YP, ZP, X, Y, Z, REP, RET, RIT, BET, ESO,
     +                    HSO, H2, H3, L4, L5, ZONE1, ZONE2, ZONE3,   
     +                    ZONE4, ZONE5, ZONE6, ZONE7, ZONE8, TYPSOU )
      IMPLICIT   NONE 
      REAL*8             XP, YP, ZP, X, Y, Z
      REAL*8             REP, RET, RIT, BET, HSO, ESO, H2, H3, L4, L5
      CHARACTER*8        TYPSOU
      LOGICAL            ZONE1,ZONE2,ZONE3,ZONE4,ZONE5,ZONE6,ZONE7,ZONE8
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SOUSTRUC  DATE 25/09/2002   AUTEUR GREFFET N.GREFFET 
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
C TOLE  CRP_21
C TOLE  CRP_6
C     OPERATEUR: "DEFI_GROUP" , MOTCLE FACTEUR "EQUE_PIQUA"
C     AUTEUR Y. WADIER
C
C     REALISE LA TRANSFORMATION : GEOMETRIE DE REFERENCE --> PIQUAGE
C     POUR LES DIFFERENTES ZONES : ZONE1 ... ZONE8
C IN  : XP, YP, ZP : COORD. DU POINT DANS LA GEOMETRIE DE REFERENCE
C OUT : X, Y, Z    : COORD. DU POINT DANS LA GEOMETRIE DU PIQUAGE
C
C-----------------------------------------------------------------------
C
      REAL*8  RMP, ALP, ALP0, ALP1, PI, R8PI, GAM, ETA
      REAL*8  KSI, LAN, FLA, LM0, RA, RM, RC, XS, YS, ZS, ZS0, ZM0,
     +        YP0, YP1, Z1M1, Z1M2, ZM1, ZM2, XC, YC, ZC, ZD, XC0, 
     +        YC0, ZC0, ZD0
C     ------------------------------------------------------------------
C
      PI  = R8PI()
      RMP = SQRT( XP**2 + YP**2 )
      ALP = ATAN2( YP , XP )
C
      IF ( TYPSOU .EQ. 'TYPE_1' ) THEN
        CALL PIQSOU ( ALP, XC, YC, ZC, ZD, REP, RET, RIT, BET, ESO,
     &                TYPSOU )
      ELSEIF ( TYPSOU .EQ. 'TYPE_2' ) THEN
        CALL PIQSOU ( ALP, XC, YC, ZC, ZD, REP, RET, RIT, BET, HSO,
     &                TYPSOU )
      ENDIF
C
      IF ( ZONE1 ) THEN
         X = XP
         Y = YP
         Z = ZP
         GOTO 9999
      ENDIF
C
      IF ( ZONE2 ) THEN
         X   = XP
         Y   = YP
         GAM = ASIN( YP / ZP )
         Z   = ZP*(COS(GAM)*(H2-ZP)/(H2-H3)+(ZP-H3)/(H2-H3))
         GOTO 9999
      ENDIF
C
      IF ( ZONE3 .OR. ZONE4 ) THEN
         X   = XP
         Y   = YP
         GAM = ASIN( YP / ZP )
         Z   = ZP * COS(GAM)
         GOTO 9999
      ENDIF
C
      IF ( ZONE5 ) THEN
         IF ( TYPSOU .EQ. 'TYPE_1' ) THEN
            ETA = ( RMP - REP ) / ESO
         ELSEIF ( TYPSOU .EQ. 'TYPE_2' ) THEN
            ETA = ( RMP - REP ) / HSO
         ENDIF
         RA  = REP
         RC  = SQRT( XC**2 + YC**2 )
         RM  = RA + ETA*(RC-RA)
C
         X = RM * COS(ALP)
         Y = RM * SIN(ALP)
C
         Z1M1 = RET
         IF ( TYPSOU .EQ. 'TYPE_1' ) THEN
            Z1M2 = RET + (1.D0-ETA)*HSO
         ELSEIF ( TYPSOU .EQ. 'TYPE_2' ) THEN
            Z1M2 = RET + (1.D0-ETA)*ESO
         ENDIF
         GAM  = ASIN( Y / Z1M1 )
         ZM1  = Z1M1 * COS(GAM)
         GAM  = ASIN( Y / Z1M2 )
         ZM2  = Z1M2 * COS(GAM)
         IF ( (1.D0-ETA) .GE. 1.D-14 ) THEN
            KSI = (ZP-RET) / (Z1M2-Z1M1)
            Z   = ZM1 + KSI*(ZM2-ZM1)
         ELSE
            Z = ZM1
         ENDIF
         GOTO 9999
      ENDIF
C
      IF ( ZONE6 ) THEN
         IF ( TYPSOU .EQ. 'TYPE_1' ) THEN
            ETA = ( RMP - REP ) / ESO
         ELSEIF ( TYPSOU .EQ. 'TYPE_2' ) THEN
            ETA = ( RMP - REP ) / HSO
         ENDIF
         RA  = REP
         RC  = SQRT( XC**2 + YC**2 )
         RM  = RA + ETA*(RC-RA)
C
         X = RM*COS(ALP)
         Y = RM*SIN(ALP)
C
         GAM = ASIN( Y / RIT )
         ZM1 = RIT * COS(GAM)
         GAM = ASIN( Y / RET )
         ZM2 = RET * COS(GAM)
         KSI = (ZP-RIT) / (RET-RIT)
C
         Z = ZM1 + KSI*(ZM2-ZM1)
C
         GOTO 9999
      ENDIF
C
      IF ( ZONE7 ) THEN
C
         IF ( XP .LT.L4 ) THEN
C
            ALP0 = ACOS( XP / L4 )
            ALP1 = ACOS( XP / L5 )
C
         IF ( TYPSOU .EQ. 'TYPE_1' ) THEN
            CALL PIQSOU ( ALP0, XC0, YC0, ZC0, ZD0, REP, RET, RIT, BET,
     &                    ESO, TYPSOU )
         ELSEIF ( TYPSOU .EQ. 'TYPE_2' ) THEN
            CALL PIQSOU ( ALP0, XC0, YC0, ZC0, ZD0, REP, RET, RIT, BET,
     &                    HSO, TYPSOU )
         ENDIF
            ZM0 = ( (ZP-RIT)*ZD0+(RET-ZP)*ZC0 ) / (RET-RIT)
            IF ( ZM0/ZP .GT. 1.0D0 ) ZM0 = ZP
            GAM = ACOS( ZM0 / ZP )
            LM0 = GAM*ZP
C
            YP0 = L4*SIN(ALP0)
            YP1 = L5*SIN(ALP1)
            LAN = (YP-YP0)/(YP1-YP0)
            FLA = ( 1.0D0 + COS(LAN*PI) ) / 2.0D0
C
            XS = XP
            YS = YP + FLA*(LM0-YP0)
            ZS = ZP
C
         ENDIF
C
         IF ( XP .GE. L4 ) THEN
            XS = XP
            YS = YP
            ZS = ZP
         ENDIF
C
         ZM0 = (RIT+RET) / 2.0D0
         ZS0 = ( (RMP-L4)*ZM0+(L5-RMP)*ZS ) / (L5-L4)
         GAM = YS / ZS0
C
         X = XS
         Y = ZS * SIN( GAM )
         Z = ZS * COS( GAM )
C
         GOTO 9999
      ENDIF
C
      IF ( ZONE8 ) THEN
C
         ZM0 = ( RIT + RET ) / 2.0D0
         GAM = YP / ZM0
C 
         X = XP
         Y = ZP * SIN( GAM )
         Z = ZP * COS( GAM )
C
        GOTO 9999
      ENDIF
C
 9999 CONTINUE
C
      END
