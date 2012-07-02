      SUBROUTINE TE0117 ( OPTION , NOMTE )
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*16        OPTION , NOMTE
C ----------------------------------------------------------------------
C FONCTION REALISEE:  CALCUL DE L'OPTION FORC_NODA
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
      REAL*8             NHARM, BSIGM(18)
      INTEGER            NBSIGM,NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE
      INTEGER            JGANO
C DEB ------------------------------------------------------------------

C-----------------------------------------------------------------------
      INTEGER I ,ICONTM ,IGEOM ,IHARMO ,IVECTU ,NBSIG ,NH 

      REAL*8 ZERO 
C-----------------------------------------------------------------------
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
C
C --- INITIALISATIONS :
C     -----------------
      ZERO  = 0.0D0
      NHARM = ZERO
C
C ---- RECUPERATION  DU NUMERO D'HARMONIQUE
C      ------------------------------------
      CALL JEVECH('PHARMON','L',IHARMO)
      NH    = ZI(IHARMO)
      NHARM = DBLE(NH)
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
C ---- PARAMETRES EN SORTIE
C      --------------------
C ----     VECTEUR DES FORCES INTERNES (BT*SIGMA)
      CALL JEVECH('PVECTUR','E',IVECTU)
C
C ---- CALCUL DU VECTEUR DES FORCES INTERNES (BT*SIGMA) :
C      --------------------------------------------------
      CALL BSIGMC (NNO,NDIM,NBSIG,NPG,IPOIDS,IVF,IDFDE,
     +              ZR(IGEOM), NHARM, ZR(ICONTM), BSIGM )
C
C ---- AFFECTATION DU VECTEUR EN SORTIE :
C      ----------------------------------
       DO 10 I=1,NDIM*NNO
          ZR(IVECTU+I-1) = BSIGM(I)
10     CONTINUE
C
C FIN ------------------------------------------------------------------
      END
