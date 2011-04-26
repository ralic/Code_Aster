      SUBROUTINE INVJAX(STOP,NNO  ,NDIM ,NDERIV,DFF  ,COOR  ,INVJAC,IPB)
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE MEUNIER S.MEUNIER
C TOLE CRS_1404
C
      IMPLICIT   NONE
      INTEGER    NNO,NDIM,NDERIV,IPB
      REAL*8     COOR(NDIM*NNO)
      REAL*8     DFF(3,NNO),INVJAC(3,3),INV(NDIM,NDIM)
      CHARACTER*1  STOP
C
C ----------------------------------------------------------------------
C
C CALCUL DE L'INVERSE DE LA JACOBIENNE EN XE
C             ***           ***           *
C
C ----------------------------------------------------------------------
C
C IN  STOP   : /'S' : ON S'ARRETE EN ERREUR <F> EN CAS D'ECHEC
C              /'C' : ON CONTINUE EN CAS D'ECHEC (IPB=1)
C IN  NNO    : NOMBRE DE NOEUDS DE L'ELT
C IN  NDIM   : DIMENSION DE L'ESPACE
C IN  DFF    : D�RIV�ES DES FONCTION DES FORMES AU POINT XE
C IN  COOR   : COORDONNEES DES NOEUDS DE L'�L�MENT
C OUT INVJAC : INVERSE DE LA JACOBIENNE AU POINT XE
C OUT IPB    : =0 SI TOUT S'EST BIEN PASSE
C              =1 SI MATRICE SINGULIERE
C
C ----------------------------------------------------------------------
C
      INTEGER       IDIM,JDIM,INO,I,J
      REAL*8        JACOBI(NDIM,NDIM),DET
C
C ----------------------------------------------------------------------
C
      IPB = 0

C --- JACOBIENNE EN XE
C
      DO 10 JDIM=1,NDERIV
        DO 20 IDIM=1,NDIM
          JACOBI(IDIM,JDIM) = DFF(JDIM,1) * COOR(IDIM)
 20     CONTINUE
 10   CONTINUE

      DO 100 INO=2,NNO
        DO 110 JDIM=1,NDERIV
          DO 120 IDIM=1,NDIM
            JACOBI(IDIM,JDIM) = JACOBI(IDIM,JDIM) +
     &                 DFF(JDIM,INO) * COOR(NDIM*(INO-1)+IDIM)
 120       CONTINUE
 110     CONTINUE
 100  CONTINUE
      IF (NDIM.NE.NDERIV) THEN
        CALL ASSERT(NDIM.EQ.NDERIV+1)
        IF (NDERIV.EQ.1) THEN
          JACOBI(1,2) = -1*JACOBI(2,1)
          JACOBI(2,2) = JACOBI(1,1)
        ELSEIF (NDERIV.EQ.2) THEN
          JACOBI(1,3) = JACOBI(2,1)*JACOBI(3,2)-JACOBI(3,1)*JACOBI(2,2)
          JACOBI(2,3) = JACOBI(3,1)*JACOBI(1,2)-JACOBI(1,1)*JACOBI(3,2)
          JACOBI(3,3) = JACOBI(1,1)*JACOBI(2,2)-JACOBI(2,1)*JACOBI(1,2)
        ENDIF
      ENDIF
C
C --- INVERSE DE LA JACOBIENNE
C
      CALL MATINV(STOP,NDIM,JACOBI,INV,DET)
      IF (DET.EQ.0.D0) IPB = 1
C
      DO 30 I=1,3
        DO 40 J=1,3
           INVJAC(I,J)=0.D0
 40     CONTINUE
 30   CONTINUE
      DO 31 I=1,NDIM
        DO 41 J=1,NDIM
           INVJAC(I,J)=INV(I,J)
 41     CONTINUE
 31   CONTINUE
C
      END
