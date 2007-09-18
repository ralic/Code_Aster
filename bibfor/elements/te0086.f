      SUBROUTINE TE0086 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 18/09/2007   AUTEUR PELLET J.PELLET 
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
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES CONTRAINTES EN 2D
C                          OPTION : 'SIGM_ELNO_DEPL  '
C                             OU  : 'SIEF_ELGA_DEPL  '
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
      CHARACTER*8      MODELI
      CHARACTER*4      FAMI
      REAL*8           SIGMA(54), REPERE(7),SIGM2(54),BARY(3)
      REAL*8           NHARM, INSTAN, DEPLA(36),CONTNO(54)
      INTEGER          IDIM
      LOGICAL          LSENS
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      IF ( OPTION(6:9) .EQ.'ELNO' ) THEN
        FAMI='GANO'
      ELSE
        FAMI='RIGI'
      ENDIF
      CALL ELREF4(' ',FAMI,NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
      MODELI(1:2) = NOMTE(3:4)

C
C ---- NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT
C      -----------------------------------------
      NBSIG1  = NBSIGM()
      NBSIG2  = 6
      NBSIG   = NBSIG1
C
C --- INITIALISATIONS :
C     -----------------
      ZERO     = 0.0D0
      INSTAN   = ZERO
      NHARM    = ZERO
      IF (OPTION(11:14).EQ.'SENS') THEN
        LSENS = .TRUE.
      ELSE
        LSENS = .FALSE.
      ENDIF
C
      DO 10 I = 1, NBSIG2*NPG
         SIGMA(I)  = ZERO
10    CONTINUE
C
C ---- RECUPERATION DES COORDONNEES DES CONNECTIVITES
C      ----------------------------------------------
      CALL JEVECH('PGEOMER','L',IGEOM)
C
C ---- RECUPERATION DU MATERIAU
C      ------------------------
      CALL JEVECH('PMATERC','L',IMATE)
C
C ---- RECUPERATION  DES DONNEEES RELATIVES AU REPERE D'ORTHOTROPIE
C      ------------------------------------------------------------
C     COORDONNEES DU BARYCENTRE ( POUR LE REPRE CYLINDRIQUE )

      BARY(1) = 0.D0
      BARY(2) = 0.D0
      BARY(3) = 0.D0
      DO 160 I = 1,NNO
        DO 150 IDIM = 1,NDIM
          BARY(IDIM) = BARY(IDIM)+ZR(IGEOM+IDIM+NDIM*(I-1)-1)/NNO
 150    CONTINUE
 160  CONTINUE
      CALL ORTREP(ZI(IMATE),NDIM,BARY,REPERE)
C
C ---- RECUPERATION DU CHAMP DE DEPLACEMENT SUR L'ELEMENT
C      --------------------------------------------------
      NBINCO = NNO*NDIM
      CALL JEVECH('PDEPLAR','L',IDEPL)
C
C ---- RECUPERATION DU CHAMP DE DEPLACEMENT DERIVE SUR L'ELEMENT
C      ---------------------------------------------------------
      IF (LSENS) CALL JEVECH('PDEPSEN','L',IDEPS)

C-----------------------------------------------------------------

      DO 20 I = 1,NBSIG*NPG
        SIGMA(I) = ZERO
20    CONTINUE

      DO 50 I = 1,NBINCO
        DEPLA(I) = ZR(IDEPL-1+I)
50    CONTINUE

C
C ---- CALCUL DES CONTRAINTES 'VRAIES' AUX POINTS D'INTEGRATION
C ---- DE L'ELEMENT :
C ---- (I.E. SIGMA_MECA - SIGMA_THERMIQUES)
C      ------------------------------------
      CALL SIGVMC(FAMI,MODELI,NNO,NDIM,NBSIG1,NPG,IPOIDS,IVF,IDFDE,
     +            ZR(IGEOM),DEPLA,
     +            INSTAN,REPERE,ZI(IMATE),NHARM,SIGMA,.FALSE.)
C
C
C ---- CALC DU TERME COMPLEMENTAIRE DE CONTR 'VRAIES' SUR L'ELEMENT
C ---- DANS LE CAS DE LA SENSIBILITE (TERME DA/DP*B*U)
C ---- (I.E. SIGMA_MECA - SIGMA_THERMIQUES)
C ATTENTION!! POUR L'INSTANT(30/9/02) ON DOIT AVOIR SIGMA_THERMIQUE=0
C      ------------------------------------
      IF (LSENS) THEN
          DO 60 I = 1,NBINCO
            DEPLA(I) = ZR(IDEPS-1+I)
60        CONTINUE
        CALL SIGVMC(FAMI,MODELI,NNO,NDIM,NBSIG1,NPG,IPOIDS,IVF,IDFDE,
     +              ZR(IGEOM),DEPLA,
     +              INSTAN,REPERE,ZI(IMATE),NHARM,SIGM2,.TRUE.)
        DO 70 I=1, NBSIG*NPG
          SIGMA(I) = SIGMA(I) + SIGM2(I)
70      CONTINUE
      ENDIF
C
      IF (OPTION(6:9).EQ.'ELGA') THEN

        CALL JEVECH('PCONTRR','E',ICONT)

C         --------------------
C ---- AFFECTATION DU VECTEUR EN SORTIE AVEC LES CONTRAINTES AUX
C ---- POINTS D'INTEGRATION
C      --------------------
        DO 80 IGAU = 1, NPG
        DO 80 ISIG = 1, NBSIG
          ZR(ICONT+NBSIG*(IGAU-1)+ISIG-1) = SIGMA(NBSIG*(IGAU-1)+ISIG)
80     CONTINUE
C
      ELSE
C
        CALL PPGAN2(JGANO,NBSIG,SIGMA,CONTNO)
C
C
C ---- RECUPERATION ET AFFECTATION DU VECTEUR EN SORTIE
C ---- AVEC LE VECTEUR DES CONTRAINTES AUX NOEUDS
C      ------------------------------------------

        CALL JEVECH('PCONTRR','E',ICONT)
        DO 140 INO = 1,NNO
          DO 130 J = 1,NBSIG
            ZR(ICONT+NBSIG* (INO-1)-1+J) = CONTNO(NBSIG* (INO-1)+J)
  130     CONTINUE
  140   CONTINUE
      END IF

      END
