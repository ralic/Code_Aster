      SUBROUTINE DXSIEF ( NOMTE, XYZL, DEPL, MATER, PGL, SIGMA )
      IMPLICIT  NONE
      INTEGER         MATER
      REAL*8          XYZL(3,4), DEPL(*), PGL(3,3), SIGMA(*)
      CHARACTER*16    NOMTE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 04/04/2011   AUTEUR DESOZA T.DESOZA 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C     ------------------------------------------------------------------
C     IN  XYZL : COORDONNEES DES NOEUDS
C     IN  UL   : DEPLACEMENT
C     IN  PGL  : MATRICE DE PASSAGE GLOBAL - LOCAL ELEMENT
C     ------------------------------------------------------------------
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER         ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8          ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16      ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL         ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8     ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                     ZK24
      CHARACTER*32                              ZK32
      CHARACTER*80                                       ZK80
      COMMON /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
C     NNO:    NOMBRE DE NOEUDS DE L'ELEMENT
CCC      PARAMETER (NNO=3)  POUR LES DKT
CCC      PARAMETER (NNO=4)  POUR LES DKQ
      INTEGER    NNOMX
      PARAMETER (NNOMX=4)
      INTEGER    NBDEPG
      PARAMETER (NBDEPG=6*NNOMX)
      INTEGER    NBEPSG
      PARAMETER (NBEPSG=8)
C
C ----------------------------------------------------------------------
      INTEGER NDIM,NNOEL,NNOS,NPG,IPOIDS,ICOOPG,IVF,IDFDX,IDFD2,JGANO
      INTEGER I, J, ICACOQ, ICOU, ICPG, IGAUH, INO, IPG, IRET,
     &        IBID, NBCON, NBCOU, NPGH, JNBSPI, ITAB(8)
      REAL*8 D(4,4), REPERE(7), INST
      REAL*8 HIC, H, ZMIN,ZIC
      REAL*8 ZERO, DEUX, SIG, KCIS
      REAL*8 EXCEN
      REAL*8 EPS2D(6), DEPG(NBDEPG)
      REAL*8 EPSG(NBEPSG), EPSTH(NBEPSG)
      REAL*8 CARA(25),BMAT(NBEPSG,NBDEPG),JACGAU
      LOGICAL DKT, DKQ ,DKG
      CHARACTER*4 FAMI
      CHARACTER*8 TYPMA
C     ------------------------------------------------------------------
C
      FAMI = 'RIGI'
      CALL ELREF5(' ',FAMI,NDIM,NNOEL,NNOS,NPG,IPOIDS,ICOOPG,
     &                                         IVF,IDFDX,IDFD2,JGANO)
C
      ZERO = 0.0D0
      DEUX = 2.0D0
C
      DKT    = .FALSE.
      DKQ    = .FALSE.
      DKG    = .FALSE.

      IF ((NOMTE.EQ.'MEDKTG3').OR.
     &    (NOMTE.EQ.'MEDKQG4')) THEN
        DKG = .TRUE.
      END IF

      CALL TEATTR(' ','S','ALIAS8',TYPMA,IRET)

      IF (TYPMA(6:8).EQ.'TR3') THEN
        DKT = .TRUE.
      ELSEIF (TYPMA(6:8).EQ.'QU4') THEN
        DKQ = .TRUE.
      ELSE
        CALL ASSERT(.FALSE.)
      END IF
C
C     -- INITIALISATIONS
C
      CALL MATINI(NBEPSG,NBDEPG,ZERO,BMAT)
      CALL VECINI(NBDEPG,ZERO,DEPG)
      REPERE(1) = ZERO
      REPERE(2) = ZERO
      REPERE(3) = ZERO
      REPERE(4) = ZERO
      REPERE(5) = ZERO
      REPERE(6) = ZERO
      REPERE(7) = ZERO

C     -- GRANDEURS GEOMETRIQUES :
C     ---------------------------
      CALL JEVECH ( 'PCACOQU', 'L', ICACOQ )
      H = ZR(ICACOQ)
      EXCEN = ZR(ICACOQ+5-1)
      KCIS  = 5.D0/6.D0
      IF ( DKT ) THEN
         CALL GTRIA3(XYZL,CARA)
      ELSEIF ( DKQ ) THEN
         CALL GQUAD4(XYZL,CARA)
      END IF
C
      CALL TECACH ( 'ONN', 'PTEMPSR', 8, ITAB, IRET )
      IBID = ITAB(1)
      IF (IRET.EQ.0) THEN
         INST = ZR(IBID)
      ELSE
         INST = ZERO
      END IF

C     -- PARTITION DU DEPLACEMENT EN MEMBRANE/FLEXION :
C     -------------------------------------------------
      DO 20, INO = 1,NNOEL
        DEPG(6*(INO-1)+1) =  DEPL(6*(INO-1)+1)
        DEPG(6*(INO-1)+2) =  DEPL(6*(INO-1)+2)
        DEPG(6*(INO-1)+3) =  DEPL(6*(INO-1)+3)
        DEPG(6*(INO-1)+4) =  DEPL(6*(INO-1)+5)
        DEPG(6*(INO-1)+5) = -DEPL(6*(INO-1)+4)
        DEPG(6*(INO-1)+6) =  0.D0
 20   CONTINUE

      NBCON = 6

      IF (DKG) THEN
        NBCOU = 1
        NPGH = 1
      ELSE
        CALL JEVECH('PNBSP_I','L',JNBSPI)
        NPGH = 3
        NBCOU = ZI(JNBSPI-1+1)
        IF (NBCOU.LE.0) CALL U2MESS('F','ELEMENTS_46')
      ENDIF
C
      HIC = H/NBCOU
      ZMIN = -H/DEUX

C --- BOUCLE SUR LES POINTS DE GAUSS DE LA SURFACE:
C     ---------------------------------------------
      DO 100, IPG = 1 , NPG
C
C ----- CALCUL DE LA MATRICE DES DEFORMATIONS GENERALISEES
C
        CALL DXBMAT(NOMTE, CARA, XYZL, PGL, IPG, JACGAU, BMAT)
        CALL PMRVEC('ZERO',NBEPSG,NBDEPG,BMAT,DEPG,EPSG)

C ----- CALCUL DE L'ECOULEMENT PLASTIQUE SUR CHAQUE COUCHE
C       PAR INTEGRATION EN TROIS POINTS:
C       --------------------------------
        DO 110, ICOU = 1 , NBCOU

          DO 120, IGAUH = 1 , NPGH

            ICPG = NBCON*NPGH*NBCOU*(IPG-1) + NBCON*NPGH*(ICOU-1) +
     &                                        NBCON*(IGAUH-1)
C       -- COTE DES POINTS D'INTEGRATION
C       --------------------------------
            IF (DKG) THEN
              ZIC = ZMIN + HIC/DEUX + (ICOU-1)*HIC + EXCEN
            ELSE
              IF (IGAUH.EQ.1) THEN
                ZIC = ZMIN + (ICOU-1)*HIC + EXCEN
              ELSE IF (IGAUH.EQ.2) THEN
                ZIC = ZMIN + HIC/DEUX + (ICOU-1)*HIC + EXCEN
              ELSE
                ZIC = ZMIN + HIC + (ICOU-1)*HIC + EXCEN
              ENDIF
            ENDIF

C         -- CALCUL DE EPS2D
C         ------------------
            EPS2D(1) = EPSG(1) + ZIC*EPSG(4)
            EPS2D(2) = EPSG(2) + ZIC*EPSG(5)
            EPS2D(3) = ZERO
            EPS2D(4) = EPSG(3) + ZIC*EPSG(6)
            EPS2D(5) = EPSG(7)
            EPS2D(6) = EPSG(8)

C
C         -- INTERPOLATION DE ALPHA EN FONCTION DE LA TEMPERATURE
C         ----------------------------------------------------
            CALL VERIFT('RIGI',IPG,IGAUH,'+',MATER,'ELAS',
     &           1,EPSTH(1),IRET)

            EPSTH(2) = EPSTH(1)
            EPSTH(3) = ZERO
            EPSTH(4) = ZERO
            EPSTH(5) = ZERO
            EPSTH(6) = ZERO

C           -- CALCUL DE LA MATRICE DE HOOKE
C           --------------------------------
            CALL DMATCP ( 'RIGI',MATER, INST,'+',IPG,IGAUH,REPERE,D )

C           -- CALCUL DE LA CONTRAINTE AU POINT D'INTEGRATION COURANT
C           ---------------------------------------------------------
            DO 130 I = 1, 4
               SIG = ZERO
               DO 132 J = 1, 4
                  SIG = SIG + (EPS2D(J)-EPSTH(J))*D(I,J)
 132           CONTINUE
               SIGMA(ICPG+I) = SIG
 130        CONTINUE
C           -- CONTRAINTES DE CISAILLEMENT
C           ------------------------------
            SIGMA(ICPG+5) = KCIS*D(4,4)*(EPS2D(5)-EPSTH(5))
            SIGMA(ICPG+6) = KCIS*D(4,4)*(EPS2D(6)-EPSTH(6))
C
 120      CONTINUE
 110    CONTINUE

 100  CONTINUE

      END
