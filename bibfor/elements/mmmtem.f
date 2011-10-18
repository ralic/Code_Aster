      SUBROUTINE MMMTEM(PHASEP,NDIM  ,NNE   ,NNM   ,MPROJN,
     &                  MPROJT,WPG   ,FFE   ,FFM   ,JACOBI,
     &                  COEFAC,COEFAF,COEFFF,RESE  ,NRESE ,
     &                  LAMBDA,MATREM)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 17/10/2011   AUTEUR ABBAS M.ABBAS 
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
      INTEGER      NDIM,NNE,NNM
      REAL*8       MPROJN(3,3),MPROJT(3,3)
      REAL*8       FFE(9),FFM(9)
      REAL*8       WPG,JACOBI
      REAL*8       RESE(3),NRESE
      REAL*8       COEFAC,COEFAF
      REAL*8       LAMBDA,COEFFF
      REAL*8       MATREM(27,27)
C
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
C
C CALCUL DE LA MATRICE DEPL_ESCL/DEPL_MAIT
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
C IN  NNE    : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
C IN  NNM    : NOMBRE DE NOEUDS DE LA MAILLE MAITRE
C IN  MPROJN : MATRICE DE PROJECTION NORMALE [Pn]
C IN  MPROJT : MATRICE DE PROJECTION TANGENTE [Pt]
C IN  WPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
C IN  FFE    : FONCTIONS DE FORMES DEPL. ESCL.
C IN  FFM    : FONCTIONS DE FORMES DEPL. MAIT.
C IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
C IN  COEFAC : COEF_AUGM_CONT
C IN  COEFAF : COEF_AUGM_FROT
C IN  LAMBDA : LAGRANGIEN DE CONTACT
C IN  RESE   : SEMI-MULTIPLICATEUR GTK DE FROTTEMENT
C               GTK = LAMBDAF + COEFAF*VITESSE
C IN  NRESE  : NORME DU SEMI-MULTIPLICATEUR GTK DE FROTTEMENT
C IN  COEFFF : COEFFICIENT DE FROTTEMENT DE COULOMB
C OUT MATREM : MATRICE ELEMENTAIRE DEPL_E/DEPL_M
C
C ----------------------------------------------------------------------
C
      INTEGER   I, J, K,L,II, JJ,IDIM
      REAL*8    G(3,3),E(3,3),D(3,3),MATPRB(3,3)
      REAL*8    C1(3),C2(3),C3(3),D1(3),D2(3),D3(3)
C
C ----------------------------------------------------------------------
C
      CALL MATINI( 3, 3,0.D0,D     )
      CALL MATINI( 3, 3,0.D0,E     )
      CALL MATINI( 3, 3,0.D0,G     )
      CALL VECINI(3,0.D0,C1)
      CALL VECINI(3,0.D0,C2)
      CALL VECINI(3,0.D0,C3)
      CALL VECINI(3,0.D0,D1)
      CALL VECINI(3,0.D0,D2)
      CALL VECINI(3,0.D0,D3)
C
      DO 3 IDIM = 1,3
        C1(IDIM) = MPROJT(IDIM,1)
        C2(IDIM) = MPROJT(IDIM,2)
        C3(IDIM) = MPROJT(IDIM,3)
3     CONTINUE
C
C --- PRODUIT [E] = [Pt]x[Pt]
C
      CALL PMAT  (3,MPROJT,MPROJT,E    )
C
C --- MATRICE DE PROJECTION SUR LA BOULE UNITE
C  
      IF (PHASEP(1:4).EQ.'GLIS') THEN
        CALL MMMMPB(RESE  ,NRESE ,NDIM  ,MATPRB)
      ENDIF
C
C --- VECTEUR PROJ. BOULE SUR PLAN TGT
C
      IF (PHASEP(1:4).EQ.'GLIS') THEN
        CALL PMAVEC('ZERO',3,MATPRB,C1,D1)
        CALL PMAVEC('ZERO',3,MATPRB,C2,D2)
        CALL PMAVEC('ZERO',3,MATPRB,C3,D3)
C
C ----- MATRICE [G] = [{D1}{D2}{D3}]
C
        DO 416 IDIM = 1,3
          G(IDIM,1) = D1(IDIM)
          G(IDIM,2) = D2(IDIM)
          G(IDIM,3) = D3(IDIM)
  416    CONTINUE
C
C ----- MATRICE [D] = [Pt]*[G]t
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
          DO 200 I = 1,NNE
            DO 190 J = 1,NNM
              DO 180 K = 1,NDIM
                DO 170 L = 1,NDIM
                  II = NDIM*(I-1)+L
                  JJ = NDIM*(J-1)+K
                  MATREM(II,JJ) = MATREM(II,JJ) - COEFAC*
     &                          WPG*JACOBI*FFE(I)*MPROJN(L,K)*FFM(J)
  170           CONTINUE
  180         CONTINUE
  190       CONTINUE
  200     CONTINUE
        ELSE
          DO 701 I = 1,NNE
            DO 691 J = 1,NNM
              DO 681 K = 1,NDIM
                DO 671 L = 1,NDIM
                  II = NDIM*(I-1)+L
                  JJ = NDIM*(J-1)+K
                  MATREM(II,JJ) = MATREM(II,JJ) - COEFAC*
     &                          WPG*JACOBI*FFE(I)*MPROJN(L,K)*FFM(J)
  671           CONTINUE
  681         CONTINUE
  691       CONTINUE
  701     CONTINUE
        ENDIF
      ELSEIF (PHASEP(1:4).EQ.'ADHE') THEN  
        IF (PHASEP(6:9).EQ.'PENA') THEN   
          DO 209 I = 1,NNE
            DO 199 J = 1,NNM
              DO 189 K = 1,NDIM
                DO 179 L = 1,NDIM
                  II = NDIM*(I-1)+L
                  JJ = NDIM*(J-1)+K
                  MATREM(II,JJ) = MATREM(II,JJ) + COEFAF*COEFFF*LAMBDA*
     &                          WPG*JACOBI*FFE(I)*E(K,L)*FFM(J)
  179           CONTINUE
  189         CONTINUE
  199       CONTINUE
  209     CONTINUE  
        ELSE
          DO 207 I = 1,NNE
            DO 197 J = 1,NNM
              DO 187 K = 1,NDIM
                DO 177 L = 1,NDIM
                  II = NDIM*(I-1)+L
                  JJ = NDIM*(J-1)+K
                  MATREM(II,JJ) = MATREM(II,JJ) + COEFAF*COEFFF*LAMBDA*
     &                          WPG*JACOBI*FFE(I)*E(K,L)*FFM(J)
  177           CONTINUE
  187         CONTINUE
  197       CONTINUE
  207     CONTINUE
        ENDIF
      ELSEIF (PHASEP(1:4).EQ.'GLIS') THEN
        IF (PHASEP(6:9).EQ.'PENA') THEN       
          DO 401 I = 1,NNE
            DO 491 J = 1,NNM
              DO 481 K = 1,NDIM
                DO 471 L = 1,NDIM
                  II = NDIM*(I-1)+L
                  JJ = NDIM*(J-1)+K
                  MATREM(II,JJ) = MATREM(II,JJ) + COEFAF*COEFFF*LAMBDA*
     &                          WPG*JACOBI*FFE(I)*D(L,K)*FFM(J)
  471           CONTINUE
  481         CONTINUE
  491       CONTINUE
  401     CONTINUE
        ELSE
          DO 801 I = 1,NNE
            DO 791 J = 1,NNM
              DO 781 K = 1,NDIM
                DO 771 L = 1,NDIM
                  II = NDIM*(I-1)+L
                  JJ = NDIM*(J-1)+K
                  MATREM(II,JJ) = MATREM(II,JJ) + COEFAF*COEFFF*LAMBDA*
     &                            WPG*JACOBI*FFE(I)*D(L,K)*FFM(J)
  771           CONTINUE
  781         CONTINUE
  791       CONTINUE
  801     CONTINUE
        ENDIF
      ELSE
        CALL ASSERT(.FALSE.)  
      ENDIF
C
      END
