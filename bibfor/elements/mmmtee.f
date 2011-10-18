      SUBROUTINE MMMTEE(PHASEP,NDIM  ,NNE   ,MPROJN,MPROJT,
     &                  WPG   ,FFE   ,JACOBI,COEFAC,COEFAF,
     &                  COEFFF,RESE  ,NRESE ,LAMBDA,MATREE)
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
      INTEGER      NDIM,NNE
      REAL*8       MPROJN(3,3),MPROJT(3,3)
      REAL*8       WPG,FFE(9),JACOBI
      REAL*8       RESE(3),NRESE
      REAL*8       COEFAC,COEFAF
      REAL*8       LAMBDA,COEFFF
      REAL*8       MATREE(27,27)
C
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
C
C CALCUL DE LA MATRICE DEPL_ESCL/DEPL_ESCL
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
C IN  MPROJN : MATRICE DE PROJECTION NORMALE [Pn]
C IN  MPROJT : MATRICE DE PROJECTION TANGENTE [Pt]
C IN  WPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
C IN  FFE    : FONCTIONS DE FORMES DEPL. ESCL.
C IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
C IN  COEFAC : COEF_AUGM_CONT
C IN  COEFAF : COEF_AUGM_FROT
C IN  LAMBDA : LAGRANGIEN DE CONTACT
C IN  RESE   : SEMI-MULTIPLICATEUR GTK DE FROTTEMENT
C               GTK = LAMBDAF + COEFAF*VITESSE
C IN  NRESE  : NORME DU SEMI-MULTIPLICATEUR GTK DE FROTTEMENT
C IN  COEFFF : COEFFICIENT DE FROTTEMENT DE COULOMB
C OUT MATREE : MATRICE ELEMENTAIRE DEPL_E/DEPL_E
C
C ----------------------------------------------------------------------
C
      INTEGER   I,J,K,II,JJ,IDIM
      REAL*8    G(3,3),E(3,3),D(3,3),MATPRB(3,3)
      REAL*8    C1(3),C2(3),C3(3),D1(3),D2(3),D3(3)
      INTEGER   INOE1,INOE2,IDIM1,IDIM2
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
      DO 3 K = 1,3
        C1(K) = MPROJT(K,1)
        C2(K) = MPROJT(K,2)
        C3(K) = MPROJT(K,3)
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
          DO 160 INOE1 = 1,NNE
            DO 150 INOE2 = 1,NNE
              DO 140 IDIM2 = 1,NDIM
                DO 130 IDIM1 = 1,NDIM
                  II = NDIM*(INOE1-1)+IDIM1
                  JJ = NDIM*(INOE2-1)+IDIM2
                  MATREE(II,JJ) = MATREE(II,JJ) +
     &              COEFAC*
     &              WPG*JACOBI*
     &              FFE(INOE1)*MPROJN(IDIM1,IDIM2)*FFE(INOE2)
  130           CONTINUE
  140         CONTINUE
  150       CONTINUE
  160     CONTINUE
        ELSE
          DO 161 INOE1 = 1,NNE
            DO 151 INOE2 = 1,NNE
              DO 141 IDIM2 = 1,NDIM
                DO 131 IDIM1 = 1,NDIM
                  II = NDIM*(INOE1-1)+IDIM1
                  JJ = NDIM*(INOE2-1)+IDIM2
                  MATREE(II,JJ) = MATREE(II,JJ) +
     &              COEFAC*
     &              WPG*JACOBI*
     &              FFE(INOE1)*MPROJN(IDIM1,IDIM2)*FFE(INOE2)
  131           CONTINUE
  141         CONTINUE
  151       CONTINUE
  161     CONTINUE
        ENDIF
      ELSEIF (PHASEP(1:4).EQ.'ADHE') THEN
        IF (PHASEP(6:9).EQ.'PENA') THEN      
          DO 167 INOE1 = 1,NNE
            DO 157 INOE2 = 1,NNE
              DO 147 IDIM1 = 1,NDIM
                DO 137 IDIM2 = 1,NDIM
                  II = NDIM*(INOE1-1)+IDIM1
                  JJ = NDIM*(INOE2-1)+IDIM2
                  MATREE(II,JJ) = MATREE(II,JJ) -
     &              COEFAF*COEFFF*LAMBDA*
     &              WPG*JACOBI*
     &              FFE(INOE1)*E(IDIM1,IDIM2)*FFE(INOE2)
  137           CONTINUE
  147         CONTINUE
  157       CONTINUE
  167     CONTINUE
        ELSE
          DO 168 INOE1 = 1,NNE
            DO 158 INOE2 = 1,NNE
              DO 148 IDIM1 = 1,NDIM
                DO 138 IDIM2 = 1,NDIM
                  II = NDIM*(INOE1-1)+IDIM1
                  JJ = NDIM*(INOE2-1)+IDIM2
                  MATREE(II,JJ) = MATREE(II,JJ) -
     &              COEFAF*COEFFF*LAMBDA*
     &              WPG*JACOBI*
     &              FFE(INOE1)*E(IDIM1,IDIM2)*FFE(INOE2)
  138           CONTINUE
  148         CONTINUE
  158       CONTINUE
  168     CONTINUE
        ENDIF
      ELSEIF (PHASEP(1:4).EQ.'GLIS') THEN
        IF (PHASEP(6:9).EQ.'PENA') THEN 
          DO 462 INOE1 = 1,NNE
            DO 452 INOE2 = 1,NNE
              DO 442 IDIM2 = 1,NDIM
                DO 432 IDIM1 = 1,NDIM
                  II = NDIM*(INOE1-1)+IDIM1
                  JJ = NDIM*(INOE2-1)+IDIM2
                  MATREE(II,JJ) = MATREE(II,JJ) -
     &              COEFAF*COEFFF*LAMBDA*
     &              WPG*JACOBI*
     &              FFE(INOE1)*D(IDIM1,IDIM2)*FFE(INOE2)
  432           CONTINUE
  442         CONTINUE
  452       CONTINUE
  462     CONTINUE
        ELSE
          DO 362 INOE1 = 1,NNE
            DO 352 INOE2 = 1,NNE
              DO 342 IDIM2 = 1,NDIM
                DO 332 IDIM1 = 1,NDIM
                  II = NDIM*(INOE1-1)+IDIM1
                  JJ = NDIM*(INOE2-1)+IDIM2
                  MATREE(II,JJ) = MATREE(II,JJ) -
     &              COEFAF*COEFFF*LAMBDA*
     &              WPG*JACOBI*
     &              FFE(INOE1)*D(IDIM1,IDIM2)*FFE(INOE2)
  332           CONTINUE
  342         CONTINUE
  352       CONTINUE
  362     CONTINUE
        ENDIF
      ENDIF
C
      END
