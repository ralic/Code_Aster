      SUBROUTINE TE0007 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 21/07/2009   AUTEUR LEBOUVIER F.LEBOUVIER 
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
C ----------------------------------------------------------------------
C FONCTION REALISEE:  CALCUL DE L'OPTION FORC_NODA
C                       EN 2D POUR ELEMENTS NON LOCAUX A GRAD. DE DEF.
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
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
      REAL*8             NHARM, BSIGM(18),GEO(18)
      INTEGER            NBSIGM
      LOGICAL            LTEATT
C DEB ------------------------------------------------------------------
C ---- CARACTERISTIQUES DU TYPE D'ELEMENT :
C ---- GEOMETRIE ET INTEGRATION
C      ------------------------
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
C
C --- INITIALISATIONS :
C     -----------------
      ZERO  = 0.0D0
      NHARM = ZERO

C - SPECIFICATION DE LA DIMENSION

      IF (LTEATT(' ','AXIS','OUI')) THEN
        NDIM = 2
      ELSE IF (LTEATT(' ','C_PLAN','OUI')) THEN
        NDIM = 2
      ELSE IF (LTEATT(' ','D_PLAN','OUI')) THEN
        NDIM = 2
      ENDIF

      NDIMSI = NDIM*2
C
C ---- NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT
C      -----------------------------------------
      NBSIG = NBSIGM()
C
C ---- PARAMETRES EN ENTREE
C      --------------------
C ----     COORDONNEES DES CONNECTIVITES
      CALL JEVECH('PGEOMER','L',IGEOM)
C ----     CONTRAINTES AUX POINTS D'INTEGRATION
      CALL JEVECH('PCONTMR','L',ICONTM)
C
C         CHAMPS POUR LA REACTUALISATION DE LA GEOMETRIE
      DO 90 I = 1,NDIM*NNO
         GEO(I)  =ZR(IGEOM-1+I)
90    CONTINUE
      CALL TECACH('ONN','PDEPLMR',1,IDEPL,IRETD)
      CALL TECACH('ONN','PCOMPOR',1,ICOMP,IRETC)
      IF ((IRETD.EQ.0).AND.(IRETC.EQ.0)) THEN
         IF (ZK16(ICOMP+2)(1:6).NE.'PETIT ') THEN
            DO 80 I = 1,NDIM*NNO
               GEO(I)  =GEO(I)  + ZR(IDEPL-1+I)
80          CONTINUE
         ENDIF
      ENDIF
C ---- PARAMETRES EN SORTIE
C      --------------------
C ----     VECTEUR DES FORCES INTERNES (BT*SIGMA)
      CALL JEVECH('PVECTUR','E',IVECTU)
C

C ---- CALCUL DU VECTEUR DES FORCES INTERNES (BT*SIGMA) :
C      --------------------------------------------------
      CALL BSIGMC (  NNO, NDIM, NBSIG, NPG, IPOIDS, IVF, IDFDE,
     +              ZR(IGEOM), NHARM, ZR(ICONTM), BSIGM )
C
C ---- AFFECTATION DU VECTEUR EN SORTIE :
C      ----------------------------------
       DO 10 N=1,NNOS
         DO 20 I=1,NDIM
               KU = (NDIMSI + NDIM)*(N-1) + I
               KP = NDIM*(N-1) + I
           ZR(IVECTU+KU-1) = BSIGM(KP)
20       CONTINUE
         DO 30 I=1,NDIMSI
               KU = (NDIMSI + NDIM)*(N-1) + I + NDIM
               ZR(IVECTU+KU-1) = 0.D0
30       CONTINUE
10     CONTINUE
       DO 40 N=NNOS+1,NNO
         DO 50 I=1,NDIM
           KU = (NDIMSI + NDIM)*NNOS + NDIM*(N-NNOS-1) + I
           KP = NDIM*(N-1) + I
           ZR(IVECTU+KU-1) = BSIGM(KP)
50       CONTINUE
40     CONTINUE
C
C FIN ------------------------------------------------------------------
      END
