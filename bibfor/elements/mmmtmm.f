      SUBROUTINE MMMTMM(PHASEP,NDIM  ,NNM   ,MPROJN,MPROJT,
     &                  WPG   ,FFM   ,JACOBI,COEFCP,COEFFP,
     &                  COEFFF,RESE  ,NRESE ,LAMBDA,COEFFS,
     &                  MATRMM)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 18/04/2011   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C TOLE CRP_21
C
      IMPLICIT NONE
      CHARACTER*9  PHASEP
      INTEGER      NDIM,NNM
      REAL*8       MPROJN(3,3),MPROJT(3,3)
      REAL*8       WPG,FFM(9),JACOBI
      REAL*8       RESE(3),NRESE
      REAL*8       COEFCP
      REAL*8       COEFFP,COEFFS
      REAL*8       LAMBDA,COEFFF
      REAL*8       MATRMM(27,27)
C
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
C
C CALCUL DE LA MATRICE DEPL_MAIT/DEPL_MAIT
C
C ----------------------------------------------------------------------
C
C
C IN  PHASEP : PHASE DE CALCUL
C              'CONT'      - CONTACT
C              'CONT_PENA' - CONTACT PENALISE
C              'ADHE'      - ADHERENCE
C              'ADHE_PENA' - ADHERENCE PENALISE
C              'GLIS'      - GLISSEMENT
C              'GLIS_PENA' - GLISSEMENT PENALISE
C IN  NDIM   : DIMENSION DU PROBLEME
C IN  NNM    : NOMBRE DE NOEUDS DE LA MAILLE MAITRE
C IN  MPROJN : MATRICE DE PROJECTION NORMALE
C IN  MPROJT : MATRICE DE PROJECTION TANGENTE
C IN  WPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
C IN  FFM    : FONCTIONS DE FORMES DEPL. MAIT.
C IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
C IN  COEFCP : COEF_PENA_CONT
C IN  COEFFS : COEF_STAB_FROT
C IN  LAMBDA : VALEUR DU MULT. DE CONTACT (SEUIL DE TRESCA)
C IN  RESE   : SEMI-MULTIPLICATEUR GTK DE FROTTEMENT
C               GTK = LAMBDAF + COEFFR*VITESSE
C IN  NRESE  : NORME DU SEMI-MULTIPLICATEUR GTK DE FROTTEMENT
C IN  COEFFF : COEFFICIENT DE FROTTEMENT DE COULOMB
C IN  COEFFP : COEF_PENA_FROT
C OUT MATRMM : MATRICE ELEMENTAIRE DEPL_M/DEPL_M
C
C ----------------------------------------------------------------------
C
      INTEGER   I, J, K,L,II, JJ,IDIM
      REAL*8    G(3,3),E(3,3),D(3,3)
      REAL*8    C1(3),C2(3),C3(3),D1(3),D2(3),D3(3)
C
C ----------------------------------------------------------------------
C
      CALL MATINI( 3, 3,0.D0,E     )
      CALL MATINI( 3, 3,0.D0,D     )
      CALL MATINI( 3, 3,0.D0,G     )
      CALL VECINI(3,0.D0,D1)
      CALL VECINI(3,0.D0,D2)
      CALL VECINI(3,0.D0,D3)
      CALL VECINI(3,0.D0,C1)
      CALL VECINI(3,0.D0,C2)
      CALL VECINI(3,0.D0,C3)
C
      DO 3 IDIM = 1,3
        C1(IDIM) = MPROJT(IDIM,1)
        C2(IDIM) = MPROJT(IDIM,2)
        C3(IDIM) = MPROJT(IDIM,3)
3     CONTINUE
C
C --- PRODUIT MATR_PROJ_TANG PAR MATR_PROJ_TANG
C
      DO 360 I = 1,NDIM
        DO 350 J = 1,NDIM
          DO 340 K = 1,NDIM
            E(I,J) = MPROJT(K,I)*MPROJT(K,J) + E(I,J)
  340     CONTINUE
  350   CONTINUE
  360 CONTINUE
C
C --- VECTEUR PROJ. BOULE SUR PLAN TGT1
C
      IF (PHASEP(1:4).EQ.'GLIS') THEN
        CALL MKKVEC(RESE  ,NRESE ,NDIM  ,C1  ,D1    )
        CALL MKKVEC(RESE  ,NRESE ,NDIM  ,C2  ,D2    )
        CALL MKKVEC(RESE  ,NRESE ,NDIM  ,C3  ,D3    )
C
C ----- MATRICE [G] = [{D1}{D2}{D3}]
C
        DO 416 IDIM = 1,3
          G(IDIM,1) = D1(IDIM)
          G(IDIM,2) = D2(IDIM)
          G(IDIM,3) = D3(IDIM)
  416    CONTINUE
C
C ----- MATRICE [D] = [P]*[G]t
C
        DO 423 I = 1,NDIM
          DO 424 J = 1,NDIM
            DO 425 K = 1,NDIM
              D(I,J) = G(K,I)*MPROJT(K,J) + D(I,J)
 425        CONTINUE
 424      CONTINUE
 423    CONTINUE
      ENDIF
C
C --- CALCUL DES TERMES
C
      IF (PHASEP(1:4).EQ.'CONT') THEN
        IF (PHASEP(6:9).EQ.'PENA') THEN
          DO 160 I = 1,NNM
            DO 150 J = 1,NNM
              DO 140 K = 1,NDIM
                DO 130 L = 1,NDIM
                  II = NDIM*(I-1)+L
                  JJ = NDIM*(J-1)+K
                  MATRMM(II,JJ) = MATRMM(II,JJ) + COEFCP*
     &                            WPG*JACOBI*FFM(I)*MPROJN(L,K)*FFM(J)
  130           CONTINUE
  140         CONTINUE
  150       CONTINUE
  160     CONTINUE
        ELSE
          DO 161 I = 1,NNM
            DO 151 J = 1,NNM
              DO 141 K = 1,NDIM
                DO 131 L = 1,NDIM
                  II = NDIM*(I-1)+L
                  JJ = NDIM*(J-1)+K
                  MATRMM(II,JJ) = MATRMM(II,JJ) + COEFCP*
     &                            WPG*JACOBI*FFM(I)*MPROJN(L,K)*FFM(J)
  131           CONTINUE
  141         CONTINUE
  151       CONTINUE
  161     CONTINUE
        ENDIF       
      ELSEIF (PHASEP(1:4).EQ.'ADHE') THEN
        IF (PHASEP(6:9).EQ.'PENA') THEN
          DO 165 I = 1,NNM
            DO 155 J = 1,NNM
              DO 145 K = 1,NDIM
                DO 135 L = 1,NDIM
                  II = NDIM*(I-1)+K
                  JJ = NDIM*(J-1)+L
                  MATRMM(II,JJ) = MATRMM(II,JJ) - COEFFP*COEFFF*LAMBDA*
     &                            WPG*JACOBI*FFM(I)*E(K,L)*FFM(J)
  135           CONTINUE
  145         CONTINUE
  155       CONTINUE
  165     CONTINUE        
        ELSE
          DO 166 I = 1,NNM
            DO 156 J = 1,NNM
              DO 146 K = 1,NDIM
                DO 136 L = 1,NDIM
                  II = NDIM*(I-1)+K
                  JJ = NDIM*(J-1)+L
                  MATRMM(II,JJ) = MATRMM(II,JJ) - COEFFP*COEFFF*LAMBDA*
     &                            WPG*JACOBI*FFM(I)*E(K,L)*FFM(J)
  136           CONTINUE
  146         CONTINUE
  156       CONTINUE
  166     CONTINUE
        ENDIF        
      ELSEIF (PHASEP(1:4).EQ.'GLIS') THEN
        IF (PHASEP(6:9).EQ.'PENA') THEN         
          DO 465 I = 1,NNM
            DO 455 J = 1,NNM
              DO 445 K = 1,NDIM
                DO 435 L = 1,NDIM
                  II = NDIM*(I-1)+L
                  JJ = NDIM*(J-1)+K
                  MATRMM(II,JJ) = MATRMM(II,JJ) - COEFFP*COEFFF*LAMBDA*
     &                            WPG*JACOBI*FFM(I)*D(L,K)*FFM(J)
  435           CONTINUE
  445         CONTINUE
  455       CONTINUE
  465     CONTINUE                
        ELSE
          DO 365 I = 1,NNM
            DO 355 J = 1,NNM
              DO 345 K = 1,NDIM
                DO 335 L = 1,NDIM
                  II = NDIM*(I-1)+L
                  JJ = NDIM*(J-1)+K
                  MATRMM(II,JJ) = MATRMM(II,JJ) - COEFFS*COEFFF*LAMBDA*
     &                            WPG*JACOBI*FFM(I)*D(L,K)*FFM(J)
  335           CONTINUE
  345         CONTINUE
  355       CONTINUE
  365     CONTINUE
        ENDIF
      ENDIF
C
      END
