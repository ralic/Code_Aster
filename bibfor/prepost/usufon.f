      SUBROUTINE USUFON ( TYPE , PARA, D , F , DF )
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ----------------------------------------------------------------------
      REAL*8        PARA(*)
      CHARACTER*16   TYPEZ
      CHARACTER*(*)  TYPE
C     ------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      REAL*8 A ,B ,C ,C0 ,C0B ,C1 ,C1B 
      REAL*8 D ,D1 ,D2 ,DA ,DF ,DI ,DL 
      REAL*8 DR ,F ,FS ,FT ,RS ,RT ,S 
      REAL*8 S1 ,U ,U1 ,UPK ,US ,UT ,VU 
      REAL*8 X ,X1 ,XCOS1 ,XCOS2 ,XK ,XSIN1 ,XSIN2 
      REAL*8 ZERO 
C-----------------------------------------------------------------------
      ZERO = 0.D0
      F  = 9999.D0
      DF = 9999.D0
C
      IF ( TYPE(1:14) .EQ. 'GRAPPE_ALESAGE' ) THEN
         RT = PARA(1)
         RS = PARA(2)
         C0 = RS - RT + D
         FT = D * ( 2*RS + D ) / ( 2 * C0 )
         FS = D * ( 2*RT - D ) / ( 2 * C0 )
         UT = FT * ( 2*RT - FT )
         US = FS * ( 2*RS - FS )
         IF ( UT .GT. ZERO .AND. US .GT. ZERO ) THEN
            XCOS1=(RT-FT)/RT
            XSIN1=SQRT(ABS(1.D0-XCOS1*XCOS1))
            XCOS2=(RS-FS)/RS
            XSIN2=SQRT(ABS(1.D0-XCOS2*XCOS2))
            F = RT*RT*ATAN2(XSIN1,XCOS1) -
     &          RS*RS*ATAN2(XSIN2,XCOS2)
     &          - (RT-FT)*SQRT(UT) + (RS-FS)*SQRT(US)
        ENDIF
C
      ELSEIF ( TYPE(1:13) .EQ. 'GRAPPE_1_ENCO' .OR.
     &         TYPE(1:13) .EQ. 'GRAPPE_2_ENCO' ) THEN
         A  = PARA(1)
         B  = PARA(2)
         C  = PARA(3)
         VU = PARA(5)
         F  = (A * D**3) + (B * D**2) + (C * D) - VU
         DF = (3.D0 * A * D**2) + (2.D0 * B * D) + C
C
      ELSEIF ( TYPE(1:8) .EQ. 'TUBE_BAV' ) THEN
         DR = PARA(1)
         DL = PARA(2)
         DA = PARA(3)
         VU = PARA(4)
         UPK = 1.D0 + ( PARA(5) / PARA(4) )
         XK = D * UPK
         F = 8.D0 * SQRT( 2*DR ) / ( 15.D0 * DA * UPK )
         XK = D * UPK
         F = VU - F * ( ABS(XK) **2.5D0 -
     &                ( ABS(XK - DL*DA )) ** 2.5D0 )
         DF = 4.D0 * SQRT( 2*DR ) / ( 3.D0 * DA * UPK )
         DF = -DF * UPK * ( ABS(XK) **1.5D0 -
     &                    ( ABS(XK - DL*DA )) ** 1.5D0 )
C
      ELSEIF ( TYPE(1:12) .EQ. 'TUBE_ALESAGE' ) THEN
         RT = PARA(1)
         RS = PARA(2)
         DL = PARA(3)
         DA = PARA(4)
         C0 = RS - RT + D
         C1 = RS**2 - RT**2 - C0**2
         U  = RT**2 - ( C1**2 / ( 4.D0 * C0**2 ) )
         IF ( U .GT. ZERO ) THEN
            X = SQRT ( U )
            XSIN1=X/RT
            XCOS1=SQRT(ABS(1.D0-XSIN1*XSIN1))
            XSIN2=X/RS
            XCOS2=SQRT(ABS(1.D0-XSIN2*XSIN2))
            F = RT*RT*ATAN2(XSIN1,XCOS1) + X*C0 -
     &          RS*RS*ATAN2(XSIN2,XCOS2)
            F = 2.D0 * DL * F
        ENDIF
C
      ELSEIF ( TYPE(1:14) .EQ. 'TUBE_ALESAG_3A' ) THEN
         RT = PARA(1)
         RS = PARA(2)
         DL = PARA(3)
         DA = PARA(4)
         C0 = RS - RT + D
         C1 = RS**2 - RT**2 - C0**2
         U  = RT**2 - ( C1**2 / ( 4.D0 * C0**2 ) )
         IF ( U .GT. ZERO ) THEN
            X = SQRT ( U )
            XSIN1=X/RT
            XCOS1=SQRT(ABS(1.D0-XSIN1*XSIN1))
            XSIN2=X/RS
            XCOS2=SQRT(ABS(1.D0-XSIN2*XSIN2))
            F = RT*RT*ATAN2(XSIN1,XCOS1) + X*C0 -
     &          RS*RS*ATAN2(XSIN2,XCOS2)
            F = 2.D0 * D * F / ( 3.D0 * DA )
        ENDIF
C
      ELSEIF ( TYPE(1:14) .EQ. 'TUBE_ALESAG_3B' ) THEN
         RT = PARA(1)
         RS = PARA(2)
         DL = PARA(3)
         DA = PARA(4)
         C0 = RS - RT + D
         C0B = RS - RT + D - DL*DA
         C1 = RS**2 - RT**2 - C0**2
         C1B = RS**2 - RT**2 - C0B**2
         U   = RT**2 - ( C1**2  / ( 4.D0 * C0**2  ) )
         U1  = RT**2 - ( C1B**2 / ( 4.D0 * C0B**2 ) )
         IF ( U .GT. ZERO .AND. U1 .GT. ZERO ) THEN
            X  = SQRT ( U  )
            X1 = SQRT ( U1 )
            XSIN1=X/RT
            XCOS1=SQRT(ABS(1.D0-XSIN1*XSIN1))
            XSIN2=X/RS
            XCOS2=SQRT(ABS(1.D0-XSIN2*XSIN2))
            S  = RT*RT*ATAN2(XSIN1,XCOS1)  + X*C0 -
     &           RS*RS*ATAN2(XSIN2,XCOS2)
            XSIN1=X1/RT
            XCOS1=SQRT(ABS(1.D0-XSIN1*XSIN1))
            XSIN2=X1/RS
            XCOS2=SQRT(ABS(1.D0-XSIN2*XSIN2))
            S1 = RT*RT*ATAN2(XSIN1,XCOS1) + X1*C0B -
     &           RS*RS*ATAN2(XSIN2,XCOS2)
            F = 2.D0 * DL * ( S + S1 + SQRT(S*S1) ) / 3.D0
         ENDIF
C
      ELSEIF ( TYPE(1:11) .EQ. 'TUBE_3_ENCO' .OR.
     &         TYPE(1:11) .EQ. 'TUBE_4_ENCO' ) THEN
         RT = PARA(1)
         RS = PARA(2)
         DL = PARA(3)
         DA = PARA(4)
         DI = PARA(7)
         C0 = RS - RT + D
         C1 = RS**2 - RT**2 - C0**2
         D1 = TAN(DI) * D**2
         U  = RT**2 - ( C1**2 / ( 4.D0 * C0**2 ) )
         IF ( U .GT. ZERO ) THEN
            X = SQRT ( U )
            XSIN1=X/RT
            XCOS1=SQRT(ABS(1.D0-XSIN1*XSIN1))
            XSIN2=X/RS
            XCOS2=SQRT(ABS(1.D0-XSIN2*XSIN2))
            F = RT*RT*ATAN2(XSIN1,XCOS1) + X*C0 -
     &          RS*RS*ATAN2(XSIN2,XCOS2) + D1
            F = DL * F / 2.D0
        ENDIF
C
      ELSEIF ( TYPE(1:13) .EQ. 'TUBE_ENCO_2A' ) THEN
         RT = PARA(1)
         RS = PARA(2)
         DL = PARA(3)
         DA = PARA(4)
         DI = PARA(7)
         C0 = RS - RT + D
         C1 = RS**2 - RT**2 - C0**2
         D1 = TAN(DI) * D**2
         U  = RT**2 - ( C1**2 / ( 4.D0 * C0**2 ) )
         IF ( U .GT. ZERO ) THEN
            X = SQRT ( U )
            XSIN1=X/RT
            XCOS1=SQRT(ABS(1.D0-XSIN1*XSIN1))
            XSIN2=X/RS
            XCOS2=SQRT(ABS(1.D0-XSIN2*XSIN2))
            F = RT*RT*ATAN2(XSIN1,XCOS1) + X*C0 -
     &          RS*RS*ATAN2(XSIN2,XCOS2) + D1
            F = D * F / ( 6.D0 * DA )
        ENDIF
C
      ELSEIF ( TYPE(1:13) .EQ. 'TUBE_ENCO_2B' ) THEN
         RT = PARA(1)
         RS = PARA(2)
         DL = PARA(3)
         DA = PARA(4)
         DI = PARA(7)
         C0 = RS - RT + D
         C0B = RS - RT + D - DL*DA
         C1 = RS**2 - RT**2 - C0**2
         C1B = RS**2 - RT**2 - C0B**2
         D1 = TAN(DI) * D**2
         D2 = TAN(DI) * ( D - DL*DA )**2
         U   = RT**2 - ( C1**2  / ( 4.D0 * C0**2  ) )
         U1  = RT**2 - ( C1B**2 / ( 4.D0 * C0B**2 ) )
         IF ( U .GT. ZERO .AND. U1 .GT. ZERO ) THEN
            X  = SQRT ( U )
            X1 = SQRT ( U1 )
            XSIN1=X/RT
            XCOS1=SQRT(ABS(1.D0-XSIN1*XSIN1))
            XSIN2=X/RS
            XCOS2=SQRT(ABS(1.D0-XSIN2*XSIN2))
            S  = RT*RT*ATAN2(XSIN1,XCOS1)  + X*C0 -
     &           RS*RS*ATAN2(XSIN2,XCOS2)  + D1
            XSIN1=X1/RT
            XCOS1=SQRT(ABS(1.D0-XSIN1*XSIN1))
            XSIN2=X1/RS
            XCOS2=SQRT(ABS(1.D0-XSIN2*XSIN2))
            S1 = RT*RT*ATAN2(XSIN1,XCOS1) + X1*C0B -
     &           RS*RS*ATAN2(XSIN2,XCOS2) + D2
            F = DL * ( S + S1 + SQRT(S*S1) ) / 6.D0
         ENDIF
C
      ELSE
         TYPEZ = TYPE(1:16)
         CALL U2MESK('F','PREPOST4_82',1,TYPEZ)
      ENDIF
C
      END
