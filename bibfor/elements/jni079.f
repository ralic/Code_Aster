      SUBROUTINE JNI079 ( ELREFE, NMAXOB, LIOBJ, NBOBJ )
      IMPLICIT NONE
      INTEGER             NMAXOB, NBOBJ
      CHARACTER*8         ELREFE
      CHARACTER*24        LIOBJ(NMAXOB)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
C RESPONSABLE VABHHTS J.PELLET
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.


C ======================================================================

C ----------------------------------------------------------------------
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN DECLARATIONS NORMALISEES  JEVEUX ---------------------

      INTEGER    NBPGMX,    NBNOMX,    NBFAMX
      PARAMETER (NBPGMX=27, NBNOMX=27, NBFAMX=20)
      INTEGER       NNO, NC, IRET, NDIM, NNOS, NBFPG, NBPG(NBPGMX)
      INTEGER       LZR, LQSI, LETA, LWGT, IND, NUFL, LDESR, INDIK8
      REAL*8        X(3*NBNOMX), VOL
      CHARACTER*8   ELREF0, FAMILL, FAPG(NBFAMX)
      CHARACTER*24  DESR
C DEB ------------------------------------------------------------------

      NBOBJ = 1
      CALL ASSERT(NMAXOB.GT.NBOBJ)

      DESR = '&INEL.'//ELREFE//'.DESR'

      LIOBJ(1) = DESR

      CALL JEEXIN ( DESR , IRET )
      IF (IRET.GT.0) GO TO 10

C     ------------------------------------------------------------------
      IF (ELREFE.EQ.'MEDKTR3 ' .OR. ELREFE.EQ.'MEGRDKT ' .OR.
     &    ELREFE.EQ.'MEDSTR3 ' .OR. ELREFE.EQ.'MEDKTG3 ') THEN
         ELREF0 = 'TR3'
         FAMILL = 'FPG3'
         NC = 3
         IND = 0

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'MEDKQU4 ' .OR. ELREFE.EQ.'MEGRDKQ ' .OR.
     &         ELREFE.EQ.'MEDSQU4 ' .OR. ELREFE.EQ.'MEQ4QU4 ' .OR.
     &         ELREFE.EQ.'MEDKQG4 ' ) THEN
         ELREF0 = 'QU4'
         FAMILL = 'FPG4'
         NC = 4
         IND = 20

C     ------------------------------------------------------------------
      ELSE
        CALL U2MESK('F','ELEMENTS2_30',1,ELREFE)
      END IF

      CALL ELRACA(ELREF0,NDIM,NNO,NNOS,NBFPG,FAPG,NBPG,X,VOL)

      NUFL = INDIK8( FAPG, FAMILL, 1, NBFPG )

      LDESR = 6 + 3*NBPG(NUFL) + 2*NNO + 5*NC + 1 + IND + 17

      CALL WKVECT ( DESR, 'G V R', LDESR, LZR )


      IF (ELREFE.EQ.'MEDKTR3 ' .OR. ELREFE.EQ.'MEGRDKT ' .OR.
     &    ELREFE.EQ.'MEDSTR3 ' .OR. ELREFE.EQ.'MEDKTG3 ') THEN
C        ---------------------------------------------------------------
C        COORDONNEES ET POIDS DES POINTS D'INTEGRATION DU TRIANGLE
C        POUR LES DKT ET DST
C        --------------- PARAMETRAGE TRIANGLE --------------------------
C-DOC          PARAMETER (NPG   = 3)
C-DOC          PARAMETER (NC    = 3)
C-DOC          PARAMETER (LDETJ = 1)
C-DOC          PARAMETER (LJACO = 2)
C-DOC          PARAMETER (LTOR  = LJACO + 4)
C-DOC          PARAMETER (LQSI  = LTOR  + 1)
        LQSI = 7
C-DOC          PARAMETER (LETA  = LQSI  + NPG + NNO )
        LETA = LQSI + NBPG(1) + NNO
C-DOC          PARAMETER (LWGT  = LETA  + NPG + NNO )
        LWGT = LETA + NBPG(1) + NNO
C-DOC          PARAMETER (LXYC  = LWGT  + NPG)
C-DOC          PARAMETER (LCOTE = LXYC  + 2*NC)
C-DOC          PARAMETER (LCOS  = LCOTE + NC)
C-DOC          PARAMETER (LSIN  = LCOS  + NC)
C-DOC          PARAMETER (LAIRE = LSIN  + NC)
C-DOC          PARAMETER (LT1VE = LAIRE + 1 )
C-DOC          PARAMETER (LT2VE = LT1VE + 9 )
C-DOC          PARAMETER (LT2EV = LT2VE + 4 )
C        ---------------------------------------------------------------
C                   -------- QSI POINTS DE GAUSS
        ZR(LZR-1+LQSI)   = 0.166666666666667D0
        ZR(LZR-1+LQSI+1) = 0.666666666666667D0
        ZR(LZR-1+LQSI+2) = 0.166666666666667D0
C                   -------- QSI NOEUDS
        ZR(LZR-1+LQSI+3) = 0.D0
        ZR(LZR-1+LQSI+4) = 1.D0
        ZR(LZR-1+LQSI+5) = 0.D0
C                   -------- ETA POINTS DE GAUSS
        ZR(LZR-1+LETA)   = 0.166666666666667D0
        ZR(LZR-1+LETA+1) = 0.166666666666667D0
        ZR(LZR-1+LETA+2) = 0.666666666666667D0
C                   -------- ETA NOEUDS
        ZR(LZR-1+LETA+3) = 0.D0
        ZR(LZR-1+LETA+4) = 0.D0
        ZR(LZR-1+LETA+5) = 1.D0
C                   -------- POIDS POINTS DE GAUSS
        ZR(LZR-1+LWGT)   = 0.166666666666667D0
        ZR(LZR-1+LWGT+1) = 0.166666666666667D0
        ZR(LZR-1+LWGT+2) = 0.166666666666667D0


      ELSE IF (ELREFE.EQ.'MEDKQU4 ' .OR. ELREFE.EQ.'MEGRDKQ ' .OR.
     &         ELREFE.EQ.'MEDSQU4 ' .OR. ELREFE.EQ.'MEQ4QU4 ' .OR.
     &         ELREFE.EQ.'MEDKQG4 ' ) THEN
C        ---------------------------------------------------------------
C        COORDONNEES ET POIDS DES POINTS D'INTEGRATION DU QUADRANGLE
C        POUR LES DKQ, DSQ ET Q4GAMMA
C        --------------- PARAMETRAGE QUADRANGLE ------------------------
C-DOC          PARAMETER (NPG   = 4)
C-DOC          PARAMETER (NC    = 4)
C-DOC          PARAMETER (LDETJ = 1)
C-DOC          PARAMETER (LJACO = 2)
C-DOC          PARAMETER (LTOR  = LJACO + 4)
C-DOC          PARAMETER (LQSI  = LTOR  + 1)
        LQSI = 7
C-DOC         PARAMETER (LETA  = LQSI  + NPG + NNO + 2*NC)
        LETA = LQSI + NBPG(1) + NNO + 2*NC
C-DOC         PARAMETER (LWGT  = LETA  + NPG + NNO + 2*NC)
        LWGT = LETA + NBPG(1) + NNO + 2*NC
C-DOC          PARAMETER (LXYC  = LWGT  + NPG)
C-DOC          PARAMETER (LCOTE = LXYC  + 2*NC)
C-DOC          PARAMETER (LCOS  = LCOTE + NC)
C-DOC          PARAMETER (LSIN  = LCOS  + NC)
C-DOC          PARAMETER (LAIRE = LSIN  + NC)
C-DOC          PARAMETER (LAIRN = LAIRE + 1)
C_DOC          PARAMETER (LT1VE = LAIRN + 4 )
C_DOC          PARAMETER (LT2VE = LT1VE + 9 )
C_DOC          PARAMETER (LT2EV = LT2VE + 4 )
C     ------------------------------------------------------------------
C                   -------- QSI POINTS DE GAUSS
        ZR(LZR-1+LQSI)   = -0.577350269189626D0
        ZR(LZR-1+LQSI+1) =  0.577350269189626D0
        ZR(LZR-1+LQSI+2) =  0.577350269189626D0
        ZR(LZR-1+LQSI+3) = -0.577350269189626D0
C                   -------- QSI NOEUDS
        ZR(LZR-1+LQSI+4) = -1.D0
        ZR(LZR-1+LQSI+5) =  1.D0
        ZR(LZR-1+LQSI+6) =  1.D0
        ZR(LZR-1+LQSI+7) = -1.D0
C                   -------- QSI POINTS DE GAUSS DES COTES
        ZR(LZR-1+LQSI+8)  = -0.577350269189626D0
        ZR(LZR-1+LQSI+9)  =  0.577350269189626D0
        ZR(LZR-1+LQSI+10) =  1.000000000000000D0
        ZR(LZR-1+LQSI+11) =  1.000000000000000D0
        ZR(LZR-1+LQSI+12) =  0.577350269189626D0
        ZR(LZR-1+LQSI+13) = -0.577350269189626D0
        ZR(LZR-1+LQSI+14) = -1.000000000000000D0
        ZR(LZR-1+LQSI+15) = -1.000000000000000D0
C                   -------- ETA POINTS DE GAUSS
        ZR(LZR-1+LETA)   = -0.577350269189626D0
        ZR(LZR-1+LETA+1) = -0.577350269189626D0
        ZR(LZR-1+LETA+2) =  0.577350269189626D0
        ZR(LZR-1+LETA+3) =  0.577350269189626D0
C                   -------- ETA NOEUDS
        ZR(LZR-1+LETA+4) = -1.D0
        ZR(LZR-1+LETA+5) = -1.D0
        ZR(LZR-1+LETA+6) =  1.D0
        ZR(LZR-1+LETA+7) =  1.D0
C                   -------- ETA POINTS DE GAUSS DES COTES
        ZR(LZR-1+LETA+8)  = -1.000000000000000D0
        ZR(LZR-1+LETA+9)  = -1.000000000000000D0
        ZR(LZR-1+LETA+10) = -0.577350269189626D0
        ZR(LZR-1+LETA+11) =  0.577350269189626D0
        ZR(LZR-1+LETA+12) =  1.000000000000000D0
        ZR(LZR-1+LETA+13) =  1.000000000000000D0
        ZR(LZR-1+LETA+14) =  0.577350269189626D0
        ZR(LZR-1+LETA+15) = -0.577350269189626D0
C                   -------- POIDS POINTS DE GAUSS
        ZR(LZR-1+LWGT)   = 1.00D0
        ZR(LZR-1+LWGT+1) = 1.00D0
        ZR(LZR-1+LWGT+2) = 1.00D0
        ZR(LZR-1+LWGT+3) = 1.00D0

      END IF

   10 CONTINUE

      END
