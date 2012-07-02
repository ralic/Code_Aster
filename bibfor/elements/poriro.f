      SUBROUTINE PORIRO  (  ITYPE , M  , RHO , OMEGA,
     &       E , A1 , A2 , XL , XIY1 , XIY2 , XIZ1 , XIZ2 ,
     &       G , ALFAY1 , ALFAY2 , ALFAZ1 , ALFAZ2  )
      IMPLICIT NONE
      INTEGER  ITYPE
      REAL*8   E      , RHO    , A1 ,A2 ,XL ,XIY1,XIY2 , XIZ1, XIZ2, G
      REAL*8   M(*)   , ALFAY1 , ALFAY2 , ALFAZ1 , ALFAZ2
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C ======================================================================
C     CALCUL DE LA MATRICE DE RAIDEUR CENTRIFUGE DES ELEMENTS DE POUTRE
C          - DROIT A SECTION CONSTANTE
C          - DROIT A SECTION VARIABLE
C     ------------------------------------------------------------------
C IN  ITYPE      - TYPE DE VARIATION SECTION DROITE
C IN  RHO        - MASSE VOLUMIQUE DU MATERIAU
C IN  OMEGA      - VECTEUR ROTATION (EN REPERE LOCAL)
C IN  E          - MODULE D ELASTICITE MATERIAU
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
C
C OUT M          -(78) MATRICE DE RAIDEUR CENTRIFUGE ELEMENTAIRE
C ======================================================================
C     SOUS - PROGRAMMES UTILISES
C
CBBL  FUN1     - AIRES ET CONSTANTE DE TORSION EQUIVALENTES
C ======================================================================
C
      REAL*8  ZAIRE1, ZAIRE2, ZAIRE , ZCONT, XL2
      REAL*8  ASZ, ASY, PHIY, PHIZ, PHIY2, PHIZ2, OMEGA(3)
      REAL*8  OMX, OMY, OMZ, OMXY, OMXZ, OMYZ, OMX2Y2, OMX2Z2, OMY2Z2
      REAL*8  C, XIY,XIZ,ZERO,R8GAEM
      REAL*8  C001 , C002 , C003 , C004 , C005 , C006 , C007 , C009
      REAL*8  C010 , C011 , C012 , C013 , C020 , C021 , C024 , C035
      REAL*8  C040 , C060 , C070 , C105 , C120 , C140 , C210 , C420
      REAL*8  C240 
C
      INTEGER IP(13) ,I
C-----------------------------------------------------------------------
      REAL*8 PHIQY ,PHIQZ ,PHIS ,PHIYZ ,QY ,QZ 
C-----------------------------------------------------------------------
      DATA    IP/0,1,3,6,10,15,21,28,36,45,55,66,78/
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
      C009 =   9.0D0
      C010 =  10.0D0
      C011 =  11.0D0
      C012 =  12.0D0
      C013 =  13.0D0
      C020 =  20.0D0
      C021 =  21.0D0
      C024 =  24.0D0
      C035 =  35.0D0
      C040 =  40.0D0
      C060 =  60.0D0
      C070 =  70.0D0
      C105 = 105.0D0
      C120 = 120.0D0
      C140 = 140.0D0
      C210 = 210.0D0
      C240 = 240.0D0
      C420 = 420.0D0
C
      IF ( ITYPE .EQ. 2 ) THEN
         ZAIRE = ( A1 + A2 + SQRT(A1*A2) ) / C003
      ELSE
         ZAIRE = ( A1 + A2) / C002
      ENDIF
      ZCONT = - RHO * ZAIRE * XL
C
      XL2 = XL * XL
      XIY = ( XIY1 + XIY2 ) / C002
      XIZ = ( XIZ1 + XIZ2 ) / C002
C
C        CALCUL DES COEFFICIENTS INDUIT PAR LE CISAILLEMENT
      IF ( ALFAZ1 .NE. ZERO .AND. ALFAZ2 .NE. ZERO   .AND.
     &     ALFAY1 .NE. ZERO .AND. ALFAY2 .NE. ZERO )  THEN
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
      OMX = OMEGA(1)
      OMY = OMEGA(2)
      OMZ = OMEGA(3)
      OMXY = OMX*OMY
      OMXZ = OMX*OMZ
      OMYZ = OMZ*OMY
      OMX2Y2 = OMX*OMX + OMY*OMY 
      OMY2Z2 = OMY*OMY + OMZ*OMZ
      OMX2Z2 = OMX*OMX + OMZ*OMZ
C
      PHIY2 = PHIY * PHIY
      PHIZ2 = PHIZ * PHIZ
C
      M(IP(1)+ 1) = ZCONT * OMY2Z2/ C003
      M(IP(7)+ 1) = M(IP(1)+ 1) / C002
      M(IP(7)+ 7) = M(IP(1)+ 1)
C
C  TERMES DEDUITS DE LA MATRICE DE MASSE :
C
      C     = ZCONT / ( C001 + PHIY )**2
      M(IP( 2)+ 2) = OMX2Z2 * C * ( C013 / C035 + PHIY * C007 / C010 +
     &               PHIY2 / C003)
      M(IP( 6)+ 2) = - OMX2Z2 * C * XL *( C011 / C210  + PHIY * C011 
     &                 / C120 + PHIY2 / C024  )
      M(IP( 8)+ 2) = OMX2Z2 * C * ( C009 / C070 + PHIY * C003 / C010 +
     &               PHIY2 / C006 )
      M(IP(12)+ 2) = - OMX2Z2 * C * XL *( - C013 / C420 - PHIY*C003 / 
     &                 C040 - PHIY2 / C024  )
      M(IP( 6)+ 6) = OMX2Z2 * C * XL * ( C001 / C105 + PHIY / C060 +
     &               PHIY2 / C120 )
      M(IP( 8)+ 6) = - M(IP(12)+ 2)
      M(IP(12)+ 6) = OMX2Z2 * C * XL2 * ( - C001 / C140 - PHIY / C060 
     &               - PHIY2 / C120 )
      M(IP( 8)+ 8) =   M(IP(2)+ 2)
      M(IP(12)+ 8) = - M(IP(6)+ 2)
      M(IP(12)+12) =   M(IP(6)+ 6)
C
      C    = ZCONT / ( C001 + PHIZ )**2
      M(IP( 3)+ 3) = OMX2Y2 * C * ( C013 / C035 + PHIZ * C007 / C010 +
     &               PHIZ2 / C003)
      M(IP( 5)+ 3) = - OMX2Y2 * C * XL * ( C011 / C210  +
     &                 PHIZ * C011 / C120 + PHIZ2 / C024  )
      M(IP( 9)+ 3) = OMX2Y2 * C * ( C009 / C070 +
     &               PHIZ * C003 / C010 + PHIZ2 / C006)
      M(IP(11)+ 3) = - OMX2Y2 * C * XL * ( - C013 / C420 -
     &                 PHIZ * C003 / C040 - PHIZ2 / C024  )
      M(IP( 5)+ 5) = OMX2Y2 * C * XL * ( C001 / C105 + PHIZ / C060 +
     &                                  PHIZ2 / C120 )
      M(IP( 9)+ 5) = - M(IP(11)+ 3)
      M(IP(11)+ 5) = OMX2Y2 * C * XL2 * ( - C001 / C140 - PHIZ / C060 
     &               - PHIZ2 / C120 )
      M(IP( 9)+ 9) =   M(IP(3)+ 3)
      M(IP(11)+ 9) = - M(IP(5)+ 3)
      M(IP(11)+11) =   M(IP(5)+ 5)
C
C  TERMES EN PLUS PAR RAPPORT A LA MATRICE DE MASSE :
C
      QY = C001/(C001 + PHIY)
      QZ = C001/(C001 + PHIZ)
      PHIQY = PHIY*QY
      PHIQZ = PHIZ*QZ
      PHIYZ = PHIY * PHIZ
      PHIS  = PHIY + PHIZ
C
      C = ZCONT
      M(IP( 2)+ 1)  =  - OMXY * C * (C009 + PHIQY)/C060
      M(IP( 3)+ 1)  =  - OMXZ * C * (C009 + PHIQZ)/C060
      M(IP( 5)+ 1)  =  - OMXZ * C * XL * (-C004 - PHIQZ)/C120
      M(IP( 6)+ 1)  =    OMXY * C * XL * (-C004 - PHIQY)/C120
      M(IP( 8)+ 1)  =  - OMXY * C * (C020 + QY)/C060
      M(IP( 9)+ 1)  =  - OMXZ * C * (C020 + QZ)/C060
      M(IP( 11)+ 1) =  - OMXZ * C * XL * (C005 + QZ)/C120
      M(IP( 12)+ 1) =    OMXY * C * XL * (C005 + QY)/C120
C
C ======================================================================
      M(IP( 3)+ 2)  =   - OMYZ * C * QY * QZ * (C013/C035 + PHIS *  
     &                 C007/C020 + PHIYZ/C003)
      M(IP( 5)+ 2)  =     OMYZ * C * XL * QY * QZ * (C011/C210   
     &                 + (PHIY * C012 + PHIZ * C010)/C240 + PHIYZ/C024)
      M(IP( 7)+ 2)  =   -  OMXY * C * (C021- PHIQY) / C060 
      M(IP( 9)+ 2)  =   -  OMYZ * C * QY * QZ * (C009/C070 + PHIS *  
     &                 C003/C020 + PHIYZ/C006 )
      M(IP( 11)+ 2) =    OMYZ * C * XL * QY * QZ * (C013/C420 +   
     &                 (PHIY * C004 + PHIZ * C005)/C120 + PHIYZ/C024)
      M(IP( 6)+ 3)  =  - OMYZ * C * XL * QY * QZ * (C011/C210 +   
     &                 (PHIY * C010 + PHIZ * C012)/C240 + PHIYZ/C024)
      M(IP( 7)+ 3)  =  -  OMXZ * C * (C021 - PHIQZ) / C060
      M(IP( 8)+ 3)  =   M(IP( 9)+ 2)
      M(IP( 12)+ 3) =   OMYZ * C * XL * QY * QZ * (C013/C420 + (PHIY *  
     &                  C005 + PHIZ * C004)/C120 + PHIYZ/C024)
      M(IP( 6)+ 5)  =   OMYZ * C * XL * QY * QZ * (C001/C105 +    
     &                 (PHIS + PHIYZ) /C120)
      M(IP( 7)+ 5)  =   OMXZ * C * XL * (C006 - PHIQZ) / C120
      M(IP( 8)+ 5)  =   M(IP( 12)+ 3)
      M(IP( 12)+ 5) =  - OMYZ * C * XL2 * QY * QZ * (C001/C140 +   
     &                 (PHIS + PHIYZ) /C120)
      M(IP( 7)+ 6)  =  - OMXY * C * XL * (C006 - PHIQY) / C120
      M(IP( 9)+ 6)  =   M(IP( 11)+ 2)
      M(IP( 11)+ 6) =   M(IP( 12)+ 5)
C
C
      M(IP( 8)+ 7)  =  - OMXY * C * (C010 - QY)/C060
      M(IP( 9)+ 7)  =  - OMXZ * C * (C010 - QZ)/C060
      M(IP( 11)+ 7) =  - OMXZ * C * XL * (C005 - QZ)/C120
      M(IP( 12)+ 7) =    OMXZ * C * XL * (C005 - QY)/C120
      M(IP( 9)+ 8)  =    M(IP( 3)+ 2)
      M(IP( 11)+ 8) =  - M(IP( 5)+ 2)
      M(IP( 12)+ 9) =  - M(IP( 6)+ 3)
      M(IP( 12)+ 11)=    M(IP( 6)+ 5)
      END
