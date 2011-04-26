      SUBROUTINE PTMA01  (  KANL , ITYPE , M , IST , RHO ,
     &       E , A1 , A2 , XL , XIY1 , XIY2 , XIZ1 , XIZ2 ,
     &       G , ALFAY1 , ALFAY2 , ALFAZ1 , ALFAZ2 , EY , EZ )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER  KANL , IST , ITYPE
      REAL*8   E      , RHO    , A1 ,A2 ,XL ,XIY1,XIY2 , XIZ1, XIZ2, G
      REAL*8   M(*)   , ALFAY1 , ALFAY2 , ALFAZ1 , ALFAZ2 , EY , EZ
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C     CALCUL DE LA MATRICE DE MASSE DES ELEMENTS DE POUTRE
C          - DROIT A SECTION CONSTANTE
C          - DROIT A SECTION VARIABLE
C     PAR LA METHODE
C          - DES MASSES CONCENTREES
C          - DES MASSES EQUIVALENTES
C     ------------------------------------------------------------------
C IN  KANL       - TYPE DE MODELISATION DES MASSES
C IN               KANL = 0 MASSES CONCENTREES FORMULATION S.D.R.C.
C IN               KANL = 1 MASSES COHERENTE
C IN  ITYPE      - TYPE DE VARIATION SECTION DROITE
C OUT M          -(78) MATRICE DE MASSE ELEMENT
C IN  E          - MODULE D ELASTICITE MATERIAU
C IN  RHO        - MASSE VOLUMIQUE DU MATERIAU
C IN  A1         - AIRE DE LA SECTION DROITE INITIALE
C IN  A2         - AIRE DE LA SECTION DROITE FINALE
C IN  XL         - LONGUEUR DE L ELEMENT
C IN  XIY1       - MOMENT D INERTIE / Y PRINCIPAL  SECTION INITIALE
C IN  XIY2       - MOMENT D INERTIE / Y PRINCIPAL  SECTION FINALE
C IN  XIZ1       - MOMENT D INERTIE / Z PRINCIPAL  SECTION INITIALE
C IN  XIZ2       - MOMENT D INERTIE / Z PRINCIPAL  SECTION FINALE
C IN  G          - MODULE DE CISAILLEMENT DU MATERIAU
C IN  ALFAY1     - COEFFICIENT DE CISAILLEMENT AXE Y SECTION INITIALE
C IN  ALFAY2     - COEFFICIENT DE CISAILLEMENT AXE Y SECTION FINALE
C IN  ALFAZ1     - COEFFICIENT DE CISAILLEMENT AXE Z SECTION INITIALE
C IN  ALFAZ2     - COEFFICIENT DE CISAILLEMENT AXE Z SECTION FINALE
C IN  EY         - COMPOSANTE TG SUR Y PRINCIPAL
C IN  EZ         - COMPOSANTE TG SUR Z PRINCIPAL
C IN  IST        - TYPE DE STRUCTURE
C
C ======================================================================
C     SOUS - PROGRAMMES UTILISES
C
CBBL  FUN1     - AIRES ET CONSTANTE DE TORSION EQUIVALENTES
C ======================================================================
C
      REAL*8  ZAIRE1,ZAIRE2, ZINEX1,ZINEX2
      REAL*8  ZAIRE ,ZCONT, ZIAL,YIAL, XL2
      REAL*8  ASZ,ASY,PHIY,PHIZ, PHIY2,PHIZ2
      REAL*8  C, XIY,XIZ,XJX
      REAL*8 ZERO,R8GAEM
      REAL*8   C001 , C002 , C003 , C004 , C005 , C006
      REAL*8         C007 , C008 , C009 , C010 , C011
      REAL*8         C012 , C013 , C015 , C020 , C024
      REAL*8         C030 , C035 , C040 , C048 , C060
      REAL*8         C070 , C105 , C120 , C140
      REAL*8         C210 , C420
C
      INTEGER IP(12) ,I
      DATA    IP/0,1,3,6,10,15,21,28,36,45,55,66/
C
C     INITIALISATION
      ZERO =   0.0D0
      DO 10 I=1,78
        M(I) = ZERO
10    CONTINUE
C
C     -- SI G  ET E SONT NULS : ON FAIT G=1.
      IF ( ABS(G) .LT. 1.0D0/R8GAEM() ) THEN
        IF ( ABS(E) .LT. 1.0D0/R8GAEM() )  THEN
           G = 1.0D0
        ELSE
          CALL U2MESS('F','ELEMENTS2_54')
        END IF
      END IF
C
      C001 =   1.0D0
      C002 =   2.0D0
      C003 =   3.0D0
      C004 =   4.0D0
      C005 =   5.0D0
      C006 =   6.0D0
      C007 =   7.0D0
      C008 =   8.0D0
      C009 =   9.0D0
      C010 =  10.0D0
      C011 =  11.0D0
      C012 =  12.0D0
      C013 =  13.0D0
      C015 =  15.0D0
      C020 =  20.0D0
      C024 =  24.0D0
      C030 =  30.0D0
      C035 =  35.0D0
      C040 =  40.0D0
      C048 =  48.0D0
      C060 =  60.0D0
      C070 =  70.0D0
      C105 = 105.0D0
      C120 = 120.0D0
      C140 = 140.0D0
      C210 = 210.0D0
      C420 = 420.0D0
C
      IF ( KANL .EQ.0 ) THEN
C     ------------------------------------------------------------------
C               MASSES CONCENTREES FORMULATION S.D.R.C. KANL =0
C     ------------------------------------------------------------------
         IF ( ITYPE .LE. 1 ) THEN
            ZAIRE1 = RHO*A1*XL / C002
            ZAIRE2 = ZAIRE1
            ZINEX1 = RHO*XL*(XIY1+XIZ1)/C002
            ZINEX2 = ZINEX1
         ELSEIF ( ITYPE .EQ. 1 ) THEN
            ZAIRE1 = RHO*(C003*A1+A2)*XL/C008
            ZAIRE2 = RHO*(C003*A2+A1)*XL/C008
            ZINEX1 = (XIY1+XIY2+XIZ1+XIZ2)*XL*RHO / C004
            ZINEX2 = ZINEX1
         ELSE
            ZAIRE1 = RHO*(C005*A1+A2)*XL/C012
            ZAIRE2 = RHO*(C005*A2+A1)*XL/C012
            ZINEX1 = (XIY1+XIY2+XIZ1+XIZ2)*XL*RHO / C004
            ZINEX2 = ZINEX1
         ENDIF
         C = (ZAIRE1 + ZAIRE2 )*XL
         C = MIN(C*XL/C105,C/C048)
         M(IP( 1)+ 1) = ZAIRE1
         M(IP( 2)+ 2) = ZAIRE1
         M(IP( 3)+ 3) = ZAIRE1
         M(IP( 4)+ 4) = ZINEX1
         M(IP( 5)+ 5) = C + RHO*(XIY1+XIY2)*XL/C015
         M(IP( 6)+ 6) = C + RHO*(XIZ1+XIZ2)*XL/C015
         M(IP( 7)+ 7) = ZAIRE2
         M(IP( 8)+ 8) = ZAIRE2
         M(IP( 9)+ 9) = ZAIRE2
         M(IP(10)+10) = ZINEX2
         M(IP(11)+11) = M(IP( 5)+ 5)
         M(IP(12)+12) = M(IP( 6)+ 6)
      ELSE
C     ------------------------------------------------------------------
C                       MASSES COHERENTES   KANL = 1
C     ------------------------------------------------------------------
         IF ( ITYPE .EQ. 2 ) THEN
            ZAIRE = ( A1 + A2 + SQRT(A1*A2) ) / C003
         ELSE
            ZAIRE = ( A1 + A2) / C002
         ENDIF
         ZCONT = RHO * ZAIRE * XL
         M(IP(1)+ 1) = ZCONT / C003
         M(IP(7)+ 1) = M(IP(1)+ 1) / C002
         M(IP(7)+ 7) = M(IP(1)+ 1)
C
         IF ( IST .NE.2 .AND. IST .NE.5 )  THEN
            XL2 = XL * XL
            XIY = ( XIY1 + XIY2 ) / C002
            XIZ = ( XIZ1 + XIZ2 ) / C002
            XJX =  XIY + XIZ
C
C              CALCUL DES COEFFICIENTS INDUIT PAR LE CISAILLEMENT
            IF ( ALFAZ1 .NE. ZERO .AND. ALFAZ2 .NE. ZERO   .AND.
     &           ALFAY1 .NE. ZERO .AND. ALFAY2 .NE. ZERO )  THEN
C                    1/ AIRE REDUITE EN Y
               ZAIRE1 = A1 / ALFAZ1
               ZAIRE2 = A2 / ALFAZ2
               CALL FUN1 ( ASY , ZAIRE1 , ZAIRE2 , ITYPE )
C                    2/ AIRE REDUITE EN Z
               ZAIRE1 = A1 / ALFAY1
               ZAIRE2 = A2 / ALFAY2
               CALL FUN1 ( ASZ , ZAIRE1 , ZAIRE2 , ITYPE )
               PHIY  = ( C012 * E * XIZ ) / (G * ASZ * XL2 )
               PHIZ  = ( C012 * E * XIY ) / (G * ASY * XL2 )
            ELSE
C              EULER SANS INERTIE DE ROTATION
               PHIY  = ZERO
               PHIZ  = ZERO
               XIY   = ZERO
               XIZ   = ZERO
            ENDIF
            PHIY2 = PHIY * PHIY
            PHIZ2 = PHIZ * PHIZ
C
            C     = ZCONT / ( C001 + PHIY )**2
            ZIAL  = XIZ   / ( ZAIRE * XL2 )
            M(IP( 2)+ 2) = C * ( C013 / C035 + PHIY * C007 / C010  +
     &                         PHIY2 / C003 + ZIAL * C006 / C005      )
            M(IP( 6)+ 2) = C * XL *( C011 / C210  + PHIY * C011 / C120 +
     &                               PHIY2 / C024  + ZIAL / C010 -
     &                              PHIY * ZIAL / C002                )
            M(IP( 8)+ 2) = C * ( C009 / C070 + PHIY * C003 / C010 +
     &                         PHIY2 / C006 - ZIAL * C006 / C005      )
            M(IP(12)+ 2) = C * XL *( - C013 / C420 - PHIY*C003 / C040 -
     &                              PHIY2 / C024  + ZIAL / C010 -
     &                              PHIY * ZIAL / C002                )
            M(IP( 6)+ 6) = C * XL2 * ( C001 / C105 + PHIY / C060 +
     &                               PHIY2 / C120 +
     &                      ZIAL * ( C002 / C015 + PHIY / C006 +
     &                               PHIY2 / C003               )     )
            M(IP( 8)+ 6) = - M(IP(12)+ 2)
            M(IP(12)+ 6) = C * XL2 * ( - C001 / C140 - PHIY / C060 -
     &                                PHIY2 / C120 -
     &                        ZIAL * ( C001 / C030 + PHIY / C006 -
     &                                PHIY2 / C006              )     )
            M(IP( 8)+ 8) =   M(IP(2)+ 2)
            M(IP(12)+ 8) = - M(IP(6)+ 2)
            M(IP(12)+12) =   M(IP(6)+ 6)
C
            IF ( IST .NE. 3 .AND. IST .NE. 6 ) THEN
               C    = ZCONT / ( C001 + PHIZ )**2
               YIAL = XIY / ( ZAIRE * XL2 )
               M(IP( 3)+ 3) = C * ( C013 / C035 + PHIZ * C007 / C010  +
     &                           PHIZ2 / C003 + YIAL * C006 / C005    )
               M(IP( 5)+ 3) = - C * XL * ( C011 / C210  +
     &                                   PHIZ * C011 / C120 +
     &                                   PHIZ2 / C024  + YIAL / C010 -
     &                                   PHIZ * YIAL / C002           )
               M(IP( 9)+ 3) = C * ( C009 / C070 +
     &                            PHIZ * C003 / C010 +
     &                            PHIZ2 / C006 - YIAL * C006 / C005   )
               M(IP(11)+ 3) = - C * XL * ( - C013 / C420 -
     &                                   PHIZ * C003 / C040 -
     &                                   PHIZ2 / C024  + YIAL / C010 -
     &                                   PHIZ * YIAL / C002        )
               M(IP( 4)+ 4) = ZCONT * XJX / ( C003 * ZAIRE)
               M(IP(10)+ 4) = M(IP(4)+ 4) / C002
               M(IP( 5)+ 5) = C * XL2 * ( C001 / C105 + PHIZ / C060 +
     &                                  PHIZ2 / C120 +
     &                         YIAL * ( C002 / C015 + PHIZ / C006 +
     &                                  PHIZ2 / C003           )     )
               M(IP( 9)+ 5) = - M(IP(11)+ 3)
               M(IP(11)+ 5) = C * XL2 * ( - C001 / C140 - PHIZ / C060 -
     &                                  PHIZ2 / C120 -
     &                         YIAL * ( C001 / C030 + PHIZ / C006 -
     &                                  PHIZ2 / C006           )     )
               M(IP( 9)+ 9) =   M(IP(3)+ 3)
               M(IP(11)+ 9) = - M(IP(5)+ 3)
               M(IP(10)+10) =   M(IP(4)+ 4)
               M(IP(11)+11) =   M(IP(5)+ 5)
C
               IF( EZ .NE. ZERO .OR. EY .NE. ZERO ) THEN
C                 --- DANS LE REPERE LIE AU C D TORSION ---
                  M(IP( 4)+ 4) =   M(IP( 4)+ 4) +
     &                            (EZ*EZ+EY*EY)*ZCONT/C003
                  M(IP(10)+10) =   M(IP( 4)+ 4)
                  M(IP(10)+ 4) =   M(IP(10)+ 4) +
     &                            (EZ*EZ+EY*EY)*ZCONT/C006
C V TX
                  M(IP( 4)+ 2) = - EZ * ZCONT * C007/C020
                  M(IP(10)+ 8) =   M(IP( 4)+ 2)
                  M(IP(10)+ 2) = - EZ * ZCONT * C003/C020
                  M(IP( 8)+ 4) =   M(IP(10)+ 2)
C TX TZ
                  M(IP( 6)+ 4) = - EZ * ZCONT *XL/C020
                  M(IP(12)+10) = - M(IP( 6)+ 4)
                  M(IP(12)+ 4) = + EZ * ZCONT *XL/C030
                  M(IP(10)+ 6) = - M(IP(12)+ 4)
C W TX
                  M(IP( 4)+ 3) = - EY * ZCONT * C007/C020
                  M(IP(10)+ 9) =   M(IP( 4)+ 3)
                  M(IP(10)+ 3) = - EY * ZCONT * C003/C020
                  M(IP( 9)+ 4) =   M(IP(10)+ 3)
C TX TY
                  M(IP( 5)+ 4) = + EY * ZCONT *XL/C020
                  M(IP(11)+10) = - M(IP( 5)+ 4)
                  M(IP(11)+ 4) = - EY * ZCONT *XL/C030
                  M(IP(10)+ 5) = - M(IP(11)+ 4)
C
C                 --- REPASSAGE DANS LE REPERE LIE AU CDG ---
                  M(IP( 4)+ 4) =   M(IP( 4)+ 4) +
     &                              EZ * EZ * M(IP( 2)+ 2) +
     &                              EY * EY * M(IP( 3)+ 3)
     &                              -2 * EZ * M(IP( 4)+ 2)
     &                              +2 * EY * M(IP( 4)+ 3)
                  M(IP(10)+ 4) =   M(IP(10)+ 4) +
     &                              EZ * EZ * M(IP( 8)+ 2) +
     &                              EY * EY * M(IP( 9)+ 3)
     &                              - EZ * M(IP(10)+ 2)
     &                              + EY * M(IP(10)+ 3)
     &                              - EZ * M(IP( 8)+ 4)
     &                              + EY * M(IP( 9)+ 4)
                  M(IP(10)+10) =   M(IP(10)+10) +
     &                              EZ * EZ * M(IP( 8)+ 8) +
     &                              EY * EY * M(IP( 9)+ 9)
     &                              -2 * EZ * M(IP(10)+ 8)
     &                              +2 * EY * M(IP(10)+ 9)
                  M(IP( 4)+ 2) = - EZ * M(IP( 2)+ 2) + M(IP( 4)+ 2)
                  M(IP(10)+ 2) = - EZ * M(IP( 8)+ 2) + M(IP(10)+ 2)
                  M(IP( 4)+ 3) =   EY * M(IP( 3)+ 3) + M(IP( 4)+ 3)
                  M(IP(10)+ 3) =   EY * M(IP( 9)+ 3) + M(IP(10)+ 3)
                  M(IP( 5)+ 4) =   EY * M(IP( 5)+ 3) + M(IP( 5)+ 4)
                  M(IP( 6)+ 4) = - EZ * M(IP( 6)+ 2) + M(IP( 6)+ 4)
                  M(IP( 8)+ 4) = - EZ * M(IP( 8)+ 2) + M(IP( 8)+ 4)
                  M(IP( 9)+ 4) =   EY * M(IP( 9)+ 3) + M(IP( 9)+ 4)
                  M(IP(11)+ 4) =   EY * M(IP(11)+ 3) + M(IP(11)+ 4)
                  M(IP(12)+ 4) = - EZ * M(IP(12)+ 2) + M(IP(12)+ 4)
                  M(IP(10)+ 5) =   EY * M(IP( 9)+ 5) + M(IP(10)+ 5)
                  M(IP(10)+ 6) = - EZ * M(IP( 8)+ 6) + M(IP(10)+ 6)
                  M(IP(10)+ 8) = - EZ * M(IP( 8)+ 8) + M(IP(10)+ 8)
                  M(IP(10)+ 9) =   EY * M(IP( 9)+ 9) + M(IP(10)+ 9)
                  M(IP(11)+10) =   EY * M(IP(11)+ 9) + M(IP(11)+10)
                  M(IP(12)+10) = - EZ * M(IP(12)+ 8) + M(IP(12)+10)
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      END
