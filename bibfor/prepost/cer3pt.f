      SUBROUTINE CER3PT ( CUPN0, CVPN0, CUPN1, CVPN1, CUPN2, CVPN2,
     &                    CUON, CVON, RAYON )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 12/09/2005   AUTEUR F1BHHAJ J.ANGLES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE F1BHHAJ J.ANGLES
      IMPLICIT   NONE
      REAL*8     CUPN0, CVPN0, CUPN1, CVPN1, CUPN2, CVPN2
      REAL*8     CUON, CVON, RAYON
C ---------------------------------------------------------------------
C BUT: DETERMINER LE POINT QUI SE TROUVE A EGALE DISTANCE DE TROIS
C      POINTS ET EN DEDUIRE LE RAYON.
C ---------------------------------------------------------------------
C ARGUMENTS:
C     CUPN0   : IN  : COMPOSANTE U DU POINT 1ER POINT.
C     CVPN0   : IN  : COMPOSANTE V DU POINT 1ER POINT.
C     CUPN1   : IN  : COMPOSANTE U DU POINT 2EME POINT.
C     CVPN1   : IN  : COMPOSANTE V DU POINT 2EME POINT.
C     CUPN2   : IN  : COMPOSANTE U DU POINT 3EME POINT.
C     CVPN2   : IN  : COMPOSANTE V DU POINT 3EME POINT.
C
C     CUON    : OUT : COMPOSANTE U DU CENTRE "On" TROUVE.
C     CVON    : OUT : COMPOSANTE V DU CENTRE "On" TROUVE.
C     RAYON   : OUT : VALEUR DU RAYON DU CERCLE CIRCONSCRIT AUX
C                     3 POINT ET DE CENTRE "On".
C     -----------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     ------------------------------------------------------------------
      INTEGER    FLAG
C
      REAL*8     CUI, CVI, DIST, DU01, DV01, DU02, DV02, DU12, DV12
      REAL*8     CUON01, CVON01, CUON02, CVON02, CUON12, CVON12
      REAL*8     CUO01P, CVO01P, A01, B01, CUO02P, CVO02P, A02, B02
      REAL*8     CUO12P, CVO12P, A12, B12
      REAL*8     CUON1, CVON1, CUON2, CVON2, CUON3, CVON3
      REAL*8     RAY0, RAY1, RAY2, RAYMIN, EPS1, EPS2
C     ------------------------------------------------------------------
C234567                                                              012

      CALL JEMARQ()

      EPS1 = 1.0D-7
      EPS2 = 1.0D-4

      CUON = 0.0D0
      CVON = 0.0D0
      RAYON = 0.0D0

C CALCUL DES POINTS MILIEUX DES TROIS SEGMENTS

      DU01 = ABS(CUPN0-CUPN1)/2.0D0
      DV01 = ABS(CVPN0-CVPN1)/2.0D0
      DU02 = ABS(CUPN0-CUPN2)/2.0D0
      DV02 = ABS(CVPN0-CVPN2)/2.0D0
      DU12 = ABS(CUPN1-CUPN2)/2.0D0
      DV12 = ABS(CVPN1-CVPN2)/2.0D0

      IF (CUPN0 .LT. CUPN1) THEN
         CUON01 = CUPN0 + DU01
      ELSE
         CUON01 = CUPN1 + DU01
      ENDIF
      IF (CVPN0 .LT. CVPN1) THEN
         CVON01 = CVPN0 + DV01
      ELSE
         CVON01 = CVPN1 + DV01
      ENDIF

      IF (CUPN0 .LT. CUPN2) THEN
         CUON02 = CUPN0 + DU02
      ELSE
         CUON02 = CUPN2 + DU02
      ENDIF
      IF (CVPN0 .LT. CVPN2) THEN
         CVON02 = CVPN0 + DV02
      ELSE
         CVON02 = CVPN2 + DV02
      ENDIF

      IF (CUPN1 .LT. CUPN2) THEN
         CUON12 = CUPN1 + DU12
      ELSE
         CUON12 = CUPN2 + DU12
      ENDIF
      IF (CVPN1 .LT. CVPN2) THEN
         CVON12 = CVPN1 + DV12
      ELSE
         CVON12 = CVPN2 + DV12
      ENDIF

C CALCUL DES NORMALES AUX TROIS SEGMENTS PASSANT PAR LEURS POINTS
C MILIEUX.

C 1/ NORMALE AU SEGMENT : PN0 PN1 PASSANT PAR ON01

      FLAG = 0
      CUO01P = CUON01 + (CVON01 - CVPN0)
      CVO01P = CVON01 + (CUPN0 - CUON01)
      IF (ABS(CUO01P - CUON01) .LT. EPS1) THEN
         CUON1 = CUON01
         CUON2 = CUON01
         CUON3 = CUON01
         A01 = 0.0D0
         B01 = 0.0D0
         FLAG = 1
      ELSE
         A01 = (CVO01P - CVON01)/(CUO01P - CUON01)
         B01 = (CUO01P*CVON01 - CUON01*CVO01P)/(CUO01P - CUON01)
      ENDIF
C 2/ NORMALE AU SEGMENT : PN0 PN2 PASSANT PAR ON02

      CUO02P = CUON02 + (CVON02 - CVPN0)
      CVO02P = CVON02 + (CUPN0 - CUON02)
      IF (ABS(CUO02P - CUON02) .LT. EPS1) THEN
         CUON1 = CUON02
         CUON2 = CUON02
         CUON3 = CUON02
         A02 = 0.0D0
         B02 = 0.0D0
         FLAG = 2
      ELSE
         A02 = (CVO02P - CVON02)/(CUO02P - CUON02)
         B02 = (CUO02P*CVON02 - CUON02*CVO02P)/(CUO02P - CUON02)
      ENDIF

C 3/ NORMALE AU SEGMENT : PN1 PN2 PASSANT PAR ON12

      CUO12P = CUON12 + (CVON12 - CVPN1)
      CVO12P = CVON12 + (CUPN1 - CUON12)
      IF (ABS(CUO12P - CUON12) .LT. EPS1) THEN
         CUON1 = CUON12
         CUON2 = CUON12
         CUON3 = CUON12
         A12 = 0.0D0
         B12 = 0.0D0
         FLAG = 3
      ELSE
         A12 = (CVO12P - CVON12)/(CUO12P - CUON12)
         B12 = (CUO12P*CVON12 - CUON12*CVO12P)/(CUO12P - CUON12)
      ENDIF

C CALCUL DU CENTRE SITUE A EGALES DISTANCES DES POINTS PN0, PN1 ET PN2.

      IF (FLAG .EQ. 0) THEN
         CUON1 = (B02 - B01)/(A01 - A02)
         CVON1 = (A01*B02 - A02*B01)/(A01 - A02)

         CUON2 = (B12 - B01)/(A01 - A12)
         CVON2 = (A01*B12 - A12*B01)/(A01 - A12)

         CUON3 = (B12 - B02)/(A02 - A12)
         CVON3 = (A02*B12 - A12*B02)/(A02 - A12)

C  FLAG = 1, 2, 3 <=> TROIS CAS PARTICULIERS :

C   ^ v                 ^ v                 ^ v
C   |                   |                   |
C   | * P2              | * P1              | * P0
C   |                   |                   |
C   |           u       |           u       |           u
C --*-------*--->     --*-------*--->     --*-------*--->
C   P0      P1          P2      P0          P1      P2
C   CVP0=CVP1           CVP2=CVP0            CVP1=CVP2
C    FLAG = 1            FLAG = 2            FLAG = 3

      ELSEIF (FLAG .EQ. 1) THEN
         CVON2 = (A02*B12 - A12*B02)/(A02 - A12)
         CVON3 = (A02*B12 - A12*B02)/(A02 - A12)
         CVON1 = (A02*B12 - A12*B02)/(A02 - A12)
      ELSEIF (FLAG .EQ. 2) THEN
         CVON1 = (A01*B12 - A12*B01)/(A01 - A12)
         CVON3 = (A01*B12 - A12*B01)/(A01 - A12)
         CVON2 = (A01*B12 - A12*B01)/(A01 - A12)
      ELSEIF (FLAG .EQ. 3) THEN
         CVON1 = (A01*B02 - A02*B01)/(A01 - A02)
         CVON2 = (A01*B02 - A02*B01)/(A01 - A02)
         CVON3 = (A01*B02 - A02*B01)/(A01 - A02)
      ENDIF

C ON CALCULE LE RAYON ET ON VERIFIE LA PRECISION DE SON CALCUL.

      RAY0 = SQRT((CUON1 - CUPN0)**2 + (CVON1 - CVPN0)**2)
      RAY1 = SQRT((CUON2 - CUPN1)**2 + (CVON2 - CVPN1)**2)
      RAY2 = SQRT((CUON3 - CUPN2)**2 + (CVON3 - CVPN2)**2)
      RAYON = MAX(RAY0,RAY1,RAY2)
      RAYMIN = MIN(RAY0,RAY1,RAY2)
C
      IF ( (((RAYON - RAYMIN)/RAYMIN) .GT. EPS2) .AND.
     &     ((RAYON - RAYMIN) .GT. EPS2) ) THEN
        CALL UTDEBM('F','CER3PT.1','LE CALCUL DU RAYON N''EST PAS '//
     &            'ASSEZ PRECIS.')
        CALL UTIMPR('L','CUPN0 = ',1,CUPN0)
        CALL UTIMPR('S',' CVPN0 = ',1,CVPN0)
        CALL UTIMPR('L','CUPN1 = ',1,CUPN1)
        CALL UTIMPR('S',' CVPN1 = ',1,CVPN1)
        CALL UTIMPR('L','CUPN2 = ',1,CUPN2)
        CALL UTIMPR('S',' CVPN2 = ',1,CVPN2)
        CALL UTIMPI('L','FLAG = ',1,FLAG)
        CALL UTIMPR('L','CUON1 = ',1,CUON1)
        CALL UTIMPR('S',' CUON2 = ',1,CUON2)
        CALL UTIMPR('L','CUON3 = ',1,CUON3)
        CALL UTIMPR('L','CVON1 = ',1,CVON1)
        CALL UTIMPR('S',' CVON2 = ',1,CVON2)
        CALL UTIMPR('L','CVON3 = ',1,CVON3)
        CALL UTIMPR('L','RAYON = ',1,RAYON)
        CALL UTIMPR('S',' RAYMIN = ',1,RAYMIN)
        CALL UTIMPR('L','(RAYON - RAYMIN) = ',1,(RAYON - RAYMIN))
        CALL UTIMPR('L','((RAYON-RAYMIN)/RAYMIN) = ',1,
     &                   ((RAYON-RAYMIN)/RAYMIN) )
C
        CALL UTFINM( )
      ELSE
        CUON = CUON1
        CVON = CVON1
      ENDIF
C
      CALL JEDEMA()
      END
