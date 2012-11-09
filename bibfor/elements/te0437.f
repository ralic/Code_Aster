      SUBROUTINE TE0437(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
      INCLUDE 'jeveux.h'

      CHARACTER*16 OPTION,NOMTE
C ----------------------------------------------------------------------
C FONCTION REALISEE:  CALCUL DE L'OPTION FORC_NODA
C                        POUR ELEMENTS 3D NON LOCAUX A GRAD. DE DEF.

C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
      REAL*8 NHARM,BSIGM(81),GEO(81)
      INTEGER NBSIGM
C DEB ------------------------------------------------------------------

C ---- CARACTERISTIQUES DU TYPE D'ELEMENT :
C ---- GEOMETRIE ET INTEGRATION
C      ------------------------
C-----------------------------------------------------------------------
      INTEGER I ,ICOMP ,ICONTM ,IDEPL ,IDFDE ,IGEOM ,IPOIDS
      INTEGER IRETC ,IRETD ,IVECTU ,IVF ,JGANO ,KP ,KU
      INTEGER N ,NBSIG ,NDIM ,NDIMSI ,NNO ,NNOB ,NNOS
      INTEGER NPG1
      REAL*8 ZERO
C-----------------------------------------------------------------------
      IF (NOMTE(6:9).EQ.'HEXA') THEN
        NNOB = 8
      ELSE IF (NOMTE(6:10).EQ.'TETRA') THEN
        NNOB = 4
      ELSE IF (NOMTE(6:10).EQ.'PENTA') THEN
        NNOB = 6
      ELSE IF (NOMTE(6:10).EQ.'PYRAM') THEN
        NNOB = 5
      END IF


      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG1,IPOIDS,IVF,IDFDE,JGANO)

C --- INITIALISATIONS :
C     -----------------
      ZERO = 0.0D0
      NHARM = ZERO

C - SPECIFICATION DE LA DIMENSION

      NDIMSI = NDIM*2

C ---- NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT
C      -----------------------------------------
      NBSIG = NBSIGM()

C ---- PARAMETRES EN ENTREE
C      --------------------
C ----     COORDONNEES DES CONNECTIVITES
      CALL JEVECH('PGEOMER','L',IGEOM)
C ----     CONTRAINTES AUX POINTS D'INTEGRATION
      CALL JEVECH('PCONTMR','L',ICONTM)

C         CHAMPS POUR LA REACTUALISATION DE LA GEOMETRIE
      DO 10 I = 1,NDIM*NNO
        GEO(I) = ZR(IGEOM-1+I)
   10 CONTINUE
      CALL TECACH('ONN','PDEPLMR',1,IDEPL,IRETD)
      CALL TECACH('ONN','PCOMPOR',1,ICOMP,IRETC)
      IF ((IRETD.EQ.0) .AND. (IRETC.EQ.0)) THEN
        IF (ZK16(ICOMP+2) (1:6).NE.'PETIT ') THEN
          DO 20 I = 1,NDIM*NNO
            GEO(I) = GEO(I) + ZR(IDEPL-1+I)
   20     CONTINUE
        END IF
      END IF
C ---- PARAMETRES EN SORTIE
C      --------------------
C ----     VECTEUR DES FORCES INTERNES (BT*SIGMA)
      CALL JEVECH('PVECTUR','E',IVECTU)

C ---- CALCUL DU VECTEUR DES FORCES INTERNES (BT*SIGMA) :
C      --------------------------------------------------
      CALL BSIGMC (  NNO, NDIM, NBSIG, NPG1, IPOIDS, IVF, IDFDE,
     +              ZR(IGEOM), NHARM, ZR(ICONTM) , BSIGM )

C ---- AFFECTATION DU VECTEUR EN SORTIE :
C      ----------------------------------
      DO 50 N = 1,NNOB
        DO 30 I = 1,NDIM
          KU = (NDIMSI+NDIM)* (N-1) + I
          KP = NDIM* (N-1) + I
          ZR(IVECTU+KU-1) = BSIGM(KP)
   30   CONTINUE
        DO 40 I = 1,NDIMSI
          KU = (NDIMSI+NDIM)* (N-1) + I + NDIM
          ZR(IVECTU+KU-1) = 0.D0
   40   CONTINUE
   50 CONTINUE
      DO 70 N = NNOB + 1,NNO
        DO 60 I = 1,NDIM
          KU = (NDIMSI+NDIM)*NNOB + NDIM* (N-NNOB-1) + I
          KP = NDIM* (N-1) + I
          ZR(IVECTU+KU-1) = BSIGM(KP)
   60   CONTINUE
   70 CONTINUE

C FIN ------------------------------------------------------------------
      END
