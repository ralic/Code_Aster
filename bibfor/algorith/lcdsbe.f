      SUBROUTINE LCDSBE (NDIM, TYPMOD, IMATE, COMPOR,EPSTM, DEPST,
     &                    VIM, OPTION, SIG, VIP,  DSIDPT, PROJ)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 24/11/2005   AUTEUR PBADEL P.BADEL 
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
      IMPLICIT NONE
      CHARACTER*8        TYPMOD(2)
      CHARACTER*16       OPTION, COMPOR(*)
      INTEGER            NDIM, IMATE
      REAL*8             EPSTM(12), DEPST(12), VIM(2)
      REAL*8             SIG(6), VIP(2), DSIDPT(6,6,2),PROJ(6,6)
C ----------------------------------------------------------------------
C     LOI DE COMPORTEMENT ELASTIQUE ENDOMMAGEMENT BETON (EN DELOCALISE)
C
C IN  NDIM    : DIMENSION DE L'ESPACE
C IN  TYPMOD  : TYPE DE MODELISATION
C IN  IMATE   : NATURE DU MATERIAU
C IN  EPSM    : DEFORMATION EN T-
C IN  EPSRM   : DEFORMATION GENERALISEE EN T-
C IN  DEPS    : INCREMENT DE DEFORMATION
C IN  DEPSR   : INCREMENT DE DEFORMATION GENERALISEE
C IN  VIM     : VARIABLES INTERNES EN T-
C IN  OPTION  : OPTION DEMANDEE
C                 RIGI_MECA_TANG ->     DSIDEP
C                 FULL_MECA      -> SIG DSIDEP VIP
C                 RAPH_MECA      -> SIG        VIP
C OUT SIG     : CONTRAINTE
C OUT VIP     : VARIABLES INTERNES
C                 1   -> VALEUR DE L'ENDOMMAGEMENT
C OUT DSIDEP  : MATRICE TANGENTE
C OUT DSIDPR  : MATRICE TANGENTE DEFO GENERALISEE
C OUT PROJ    : PROJECTEUR DE COUPURE DU TERME DE REGULARISATION
C ----------------------------------------------------------------------
C LOC EDFRC1  COMMON CARACTERISTIQUES DU MATERIAU (AFFECTE DANS EDFRMA)
      LOGICAL     RIGI, RESI,ELAS,MTG
      INTEGER     NDIMSI, K, L, I, J, M, N, P,T(3,3)
      REAL*8      EPS(6), EPSR(6), TREPS, SIGEL(6), SIGELR(6)
      REAL*8      RAC2,COEF
      REAL*8      RIGMIN, FD, D, ENER, TROISK, G
      REAL*8      TR(6),RTEMP2
      REAL*8      EPSP(3), VECP(3,3), DSPDEP(6,6),VECP2(3,3)
      REAL*8      DEUMUD(3), LAMBDD, SIGP(3),RTEMP,RTEMP3,RTEMP4
      REAL*8      EPSM(6), EPSRM(6), DEPS(6), DEPSR(6)
      REAL*8      E, NU, ALPHA, KDESS, BENDO
      REAL*8      LAMBDA, DEUXMU, GAMMA, SEUIL,TREPSM
      REAL*8      K0,K1,SICR
      CHARACTER*2 CODRET(3)
      CHARACTER*8 NOMRES(3)
      REAL*8      VALRES(3)
      REAL*8      DDOT
      PARAMETER  (RIGMIN = 1.D-5)

C ----------------------------------------------------------------------
C ======================================================================
C                            INITIALISATION
C ======================================================================
C -- OPTION ET MODELISATION
      RIGI  = (OPTION(1:4).EQ.'RIGI' .OR. OPTION(1:4).EQ.'FULL')
      RESI  = (OPTION(1:4).EQ.'RAPH' .OR. OPTION(1:4).EQ.'FULL')
      NDIMSI = 2*NDIM
      RAC2=SQRT(2.D0)
      T(1,1)=1
      T(1,2)=4
      T(1,3)=5
      T(2,1)=4
      T(2,2)=2
      T(2,3)=6
      T(3,1)=5
      T(3,2)=6
      T(3,3)=3

C -- SEPARATION DE EPSM/EPSRM, DEPS/DEPSR DANS EPSTM,DEPST

      DO 312 I=1,NDIMSI
        EPSM(I)=EPSTM(I)
        EPSRM(I)=EPSTM(I+6)
        DEPS(I)=DEPST(I)
        DEPSR(I)=DEPST(I+6)
 312  CONTINUE

C    LECTURE DES CARACTERISTIQUES DU MATERIAU

      CALL LCEIB1 (IMATE, COMPOR, NDIM, EPSRM, 0.D0,0.D0,0.D0,0.D0,0.D0,
     &             T, LAMBDA, DEUXMU,ALPHA, KDESS, BENDO, GAMMA,
     &                    SEUIL,.FALSE.)



C -- COUPURE ISOTROPE DE LA REGULARISATION SI ENDOMMAGEMENT SATURE
      CALL R8INIR(36,0.D0,PROJ,1)
      IF (VIM(2).NE.2) CALL R8INIR(6,1.D0,PROJ,7)


C    RECUPERATION DES DEFORMATIONS
C
      IF (RESI) THEN
C      MISE A JOUR DES DEFORMATIONS MECANIQUES
        DO 10 K = 1, NDIMSI
          EPS(K) = EPSM(K) + DEPS(K)
          EPSR(K) = EPSRM(K) + DEPSR(K)
 10     CONTINUE
      ELSE
        DO 40 K=1,NDIMSI
          EPS(K)=EPSM(K)
          EPSR(K) = EPSRM(K)
40      CONTINUE
        D=VIM(1)
        FD  = (1 - D) / (1 + GAMMA*D)
        ELAS=((NINT(VIM(2)).EQ.0).OR.(NINT(VIM(2)).EQ.2))
      ENDIF
C - ON MET DANS EPS LES DEFORMATIONS REELLES
      DO 45 K=4,NDIMSI
        EPS(K) = EPS(K)/RAC2
        EPSR(K) = EPSR(K)/RAC2
45    CONTINUE
      IF (NDIMSI.LT.6) THEN
        DO 46 K=NDIMSI+1,6
          EPS(K)=0.D0
          EPSR(K)=0.D0
46      CONTINUE
      ENDIF
C     MATRICE TR = (XX XY XZ YY YZ ZZ)
C
      TR(1) = EPS(1)
      TR(2) = EPS(4)
      TR(3) = EPS(5)
      TR(4) = EPS(2)
      TR(5) = EPS(6)
      TR(6) = EPS(3)
       CALL DIAGP3(TR,VECP,EPSP)
C -   CALCUL DES CONTRAINTES ELASTIQUES ASSOCIEE AUX DEFO GENERALISEES
      TREPS = EPS(1)+EPS(2)+EPS(3)
      IF (TREPS.GT.0.D0) THEN
        DO 600 K=1,3
          SIGEL(K) = LAMBDA*TREPS
 600    CONTINUE
      ELSE
        DO 610 K=1,3
          SIGEL(K) = 0.D0
 610    CONTINUE
      ENDIF
      DO 150 K=1,3
        IF (EPSP(K).GT.0.D0) THEN
          SIGEL(K) = SIGEL(K) + DEUXMU*EPSP(K)
        ENDIF
150    CONTINUE
      TR(1) = EPSR(1)
      TR(2) = EPSR(4)
      TR(3) = EPSR(5)
      TR(4) = EPSR(2)
      TR(5) = EPSR(6)
      TR(6) = EPSR(3)
       CALL DIAGP3(TR,VECP,EPSP)
C -   CALCUL DES CONTRAINTES ELASTIQUES ASSOCIEE AUX DEFO GENERALISEES
      TREPS = EPSR(1)+EPSR(2)+EPSR(3)
      IF (TREPS.GT.0.D0) THEN
        DO 60 K=1,3
          SIGELR(K) = LAMBDA*TREPS
 60     CONTINUE
      ELSE
        DO 61 K=1,3
          SIGELR(K) = 0.D0
 61     CONTINUE
      ENDIF
      DO 15 K=1,3
        IF (EPSP(K).GT.0.D0) THEN
          SIGELR(K) = SIGELR(K) + DEUXMU*EPSP(K)
        ENDIF
15    CONTINUE
      ENER = 0.5D0 * DDOT(3,EPSP,1,SIGELR,1)
C    CALCUL DE L'ETAT D'ENDOMMAGEMENT
      IF (RESI) THEN
        ELAS = .FALSE.
        D = (SQRT((1+GAMMA)/SEUIL * ENER) - 1) / GAMMA
        IF (D.LT.VIM(1)) THEN
          D = VIM(1)
          ELAS = .TRUE.
        ELSE IF (D .GT. 1) THEN
          D = 1
          ELAS = .TRUE.
        END IF
        FD  = (1 - D) / (1 + GAMMA*D)
        VIP(1) = D
        IF (ELAS) THEN
          VIP(2) = 0
          IF (FD.LE.RIGMIN) VIP(2) = 2
        ELSE
          VIP(2) = 1
        END IF
      ENDIF
C -   CALCUL DES CONTRAINTES
C     MATRICE TR = (XX XY XZ YY YZ ZZ)
C
      TR(1) = EPS(1)
      TR(2) = EPS(4)
      TR(3) = EPS(5)
      TR(4) = EPS(2)
      TR(5) = EPS(6)
      TR(6) = EPS(3)
       CALL DIAGP3(TR,VECP,EPSP)

      IF ((EPSP(1)+EPSP(2)+EPSP(3)).GT.0.D0) THEN
        LAMBDD=LAMBDA * FD
      ELSE
        LAMBDD=LAMBDA
      ENDIF
      IF (EPSP(1).GT.0.D0) THEN
        DEUMUD(1)=DEUXMU*FD
      ELSE
        DEUMUD(1)=DEUXMU
      ENDIF
      IF (EPSP(2).GT.0.D0) THEN
        DEUMUD(2)=DEUXMU*FD
      ELSE
        DEUMUD(2)=DEUXMU
      ENDIF
      IF (EPSP(3).GT.0.D0) THEN
        DEUMUD(3)=DEUXMU*FD
      ELSE
        DEUMUD(3)=DEUXMU
      ENDIF
      TREPS=EPSP(1)+EPSP(2)+EPSP(3)
      SIGP(1)=LAMBDD*TREPS+DEUMUD(1)*EPSP(1)
      SIGP(2)=LAMBDD*TREPS+DEUMUD(2)*EPSP(2)
      SIGP(3)=LAMBDD*TREPS+DEUMUD(3)*EPSP(3)
      IF (RESI) THEN
C      ON REPASSE DANS LE REPERE INITIAL LES CONTRAINTES
        TR(1) = SIGP(1)
        TR(2) = SIGP(2)
        TR(3) = SIGP(3)
        TR(4) = 0.D0
        TR(5) = 0.D0
        TR(6) = 0.D0
        CALL BPTOBG(TR,SIG,VECP)
        DO 18 K=4,NDIMSI
          SIG(K)=RAC2*SIG(K)
18      CONTINUE
      ENDIF

C - CALCUL DE LA MATRICE TANGENTE

      IF (RIGI) THEN
        IF (OPTION(11:14).EQ.'ELAS') ELAS=.TRUE.
        CALL R8INIR(72,0.D0,DSIDPT,1)
        CALL R8INIR(36, 0.D0, DSPDEP, 1)
        IF (FD.LT.RIGMIN) THEN
          IF ((EPSP(1)+EPSP(2)+EPSP(3)).GT.0.D0) THEN
            LAMBDD=LAMBDA * RIGMIN
          ENDIF
          IF (EPSP(1).GT.0.D0) THEN
            DEUMUD(1)=DEUXMU*RIGMIN
          ENDIF
          IF (EPSP(2).GT.0.D0) THEN
            DEUMUD(2)=DEUXMU*RIGMIN
          ENDIF
          IF (EPSP(3).GT.0.D0) THEN
            DEUMUD(3)=DEUXMU*RIGMIN
          ENDIF
        ENDIF
        TR(1) = SIGEL(1)
        TR(2) = SIGEL(2)
        TR(3) = SIGEL(3)
        TR(4) = 0.D0
        TR(5) = 0.D0
        TR(6) = 0.D0
        CALL BPTOBG(TR,SIGEL,VECP)
        DO 28 K=4,NDIMSI
          SIGEL(K)=RAC2*SIGEL(K)
28      CONTINUE
        TR(1) = SIGELR(1)
        TR(2) = SIGELR(2)
        TR(3) = SIGELR(3)
        TR(4) = 0.D0
        TR(5) = 0.D0
        TR(6) = 0.D0
        CALL BPTOBG(TR,SIGELR,VECP)
        DO 280 K=4,NDIMSI
          SIGELR(K)=RAC2*SIGELR(K)
280     CONTINUE
        DO 100 K = 1,3
          DO 110 L = 1,3
            DSPDEP(K,L) = LAMBDD
 110      CONTINUE
 100    CONTINUE
        DO 120 K = 1,3
          DSPDEP(K,K) = DSPDEP(K,K) + DEUMUD(K)
 120    CONTINUE
        IF (EPSP(1)*EPSP(2).GE.0.D0) THEN
          DSPDEP(4,4)=DEUMUD(1)
        ELSE
          DSPDEP(4,4)=(DEUMUD(1)*EPSP(1)-DEUMUD(2)*EPSP(2))
     &                                    /(EPSP(1)-EPSP(2))
        ENDIF
        IF (EPSP(1)*EPSP(3).GE.0.D0) THEN
          DSPDEP(5,5)=DEUMUD(1)
        ELSE
          DSPDEP(5,5)=(DEUMUD(1)*EPSP(1)-DEUMUD(3)*EPSP(3))
     &                                    /(EPSP(1)-EPSP(3))
        ENDIF
        IF (EPSP(3)*EPSP(2).GE.0.D0) THEN
          DSPDEP(6,6)=DEUMUD(3)
        ELSE
          DSPDEP(6,6)=(DEUMUD(3)*EPSP(3)-DEUMUD(2)*EPSP(2))
     &                                    /(EPSP(3)-EPSP(2))
        ENDIF
        DO 20 I=1,3
          DO 21 J=I,3
            IF (I.EQ.J) THEN
              RTEMP3=1.D0
            ELSE
              RTEMP3=RAC2
            ENDIF
            DO 22 K=1,3
              DO 23 L=1,3
                IF (T(I,J).GE.T(K,L)) THEN
                  IF (K.EQ.L) THEN
                    RTEMP4=RTEMP3
                  ELSE
                    RTEMP4=RTEMP3/RAC2
                  ENDIF
                  RTEMP2=0.D0
                  DO 24 M=1,3
                    DO 25 N=1,3
       RTEMP2=RTEMP2+VECP(K,M)*
     &       VECP(I,N)*VECP(J,N)*VECP(L,M)*DSPDEP(N,M)
25                  CONTINUE
24                CONTINUE
      RTEMP2=RTEMP2+VECP(I,1)*VECP(J,2)*VECP(K,1)*VECP(L,2)*DSPDEP(4,4)
      RTEMP2=RTEMP2+VECP(I,2)*VECP(J,1)*VECP(K,2)*VECP(L,1)*DSPDEP(4,4)
      RTEMP2=RTEMP2+VECP(I,1)*VECP(J,3)*VECP(K,1)*VECP(L,3)*DSPDEP(5,5)
      RTEMP2=RTEMP2+VECP(I,3)*VECP(J,1)*VECP(K,3)*VECP(L,1)*DSPDEP(5,5)
      RTEMP2=RTEMP2+VECP(I,2)*VECP(J,3)*VECP(K,2)*VECP(L,3)*DSPDEP(6,6)
      RTEMP2=RTEMP2+VECP(I,3)*VECP(J,2)*VECP(K,3)*VECP(L,2)*DSPDEP(6,6)
      DSIDPT(T(I,J),T(K,L),1)=DSIDPT(T(I,J),T(K,L),1)+RTEMP2*RTEMP4
                ENDIF
23            CONTINUE
22          CONTINUE
21        CONTINUE
20      CONTINUE
        DO 26 I=1,6
          DO 27 J=I+1,6
            DSIDPT(I,J,1)=DSIDPT(J,I,1)
27        CONTINUE
26      CONTINUE
C -- CONTRIBUTION DISSIPATIVE
        IF ((.NOT. ELAS).AND.(ENER.GT.0.D0)) THEN
          COEF = (1+GAMMA)/(2*GAMMA*(1+GAMMA*D)*ENER)
          DO 200 K = 1,NDIMSI
            DO 210 L = 1, NDIMSI
              DSIDPT(K,L,2) = DSIDPT(K,L,2)-COEF*SIGEL(K)*SIGELR(L)
 210        CONTINUE
 200      CONTINUE
        END IF

      ENDIF
      END
