      SUBROUTINE LCFRGV (NDIM  , TYPMOD, OPTION, IMATE , EPSM  ,
     &                   DEPS  , VIM   , RLAG  , CHAMP , LAGR  ,
     &                   PONDER, DVIDA , ENER  , SIG   , VIP   ,
     &                   DSIDEP)


C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/12/2004   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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

      IMPLICIT NONE
      CHARACTER*8  TYPMOD
      CHARACTER*16 OPTION
      INTEGER      NDIM, IMATE
      REAL*8       EPSM(6), DVIDA(0:3,0:3)
      REAL*8       DEPS(6), VIM(*)
      REAL*8       RLAG, CHAMP(0:NDIM), LAGR(0: NDIM), PONDER(0:3)
      REAL*8       ENER, VIP(*), SIG(*), DSIDEP(6,6)

C ----------------------------------------------------------------------
C     ENDOMMAGEMENT FRAGILE A GRADIENT
C ----------------------------------------------------------------------
C IN  NDIM    DIMENSION DE L'ESPACE
C IN  TYPMOD  TYPE DE MODELISATION
C IN  OPTION  OPTION DE CALCUL
C               RIGI_MECA_TANG
C               RAPH_MECA
C               DECO_LOCA
C IN  IMATE   NATURE DU MATERIAU
C IN  EPSM    CHAMP DE DEFORMATION EN T-
C IN  DEPS    INCREMENT DU CHAMP DE DEFORMATION
C IN  VIM     VARIABLES INTERNES : ENDOMMAGEMENT(1)
C                                ET GRADIENT(1:NDIM) EN T-
C IN  RLAG    COEFFICIENT DE PENALISATION DU LAGRANGIEN AUGMENTE
C IN  CHAMP   CHAMP D'ENDOMMAGEMENT LISSE
C IN  LAGR    MULTIPLICATEURS DE LAGRANGE (/LC SUR GRAD)
C OUT PONDER  COEFFICIENTS DE PONDERATION
C OUT DVIDA   MATRICE TANGENTE
C OUT ENER    ENERGIE
C OUT VIP     DENSITE DE FISSURATION ET GRADIENT
C OUT SIG     CONTRAINTE (RAPH_MECA)
C OUT DSIDEP  MATRICE SECANTE (RIGI_MECA_TANG)
C ----------------------------------------------------------------------

      LOGICAL CPLAN, TANG, RAPH, FULL, DECO, ELAS
      INTEGER NDIMSI, K, L, ETAT,NRAC
      REAL*8  VAL(5), NU, LAMBDA, DEUXMU, J, WY, SEUIL, GAMMA, WREF, A
      REAL*8  COPLAN, EPS(6), W, TREPS, EPSEPS, FD, LAP, LAPM
      REAL*8  N0, NG, D0, DG(3), DG2, D0M, D2PHIM, COEFG, Q0, Q1, Q2
      REAL*8  D2PHI0,C1,C2,RAC(3), D0C2, D0Q2, D0Q1, D0Q0
      REAL*8  DDOT
      CHARACTER*2 RET(5)
      CHARACTER*8 NOM(5)

      REAL*8 KRON(6), DMAX,RIGMIN
      PARAMETER (DMAX = 1.D0, RIGMIN = 1.D-5)
      DATA   KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/
C ----------------------------------------------------------------------


C ----------------------------------------------------------------------
C                          INITIALISATIONS
C ----------------------------------------------------------------------


C -- LECTURE DES CARACTERISTIQUES MATERIAU

      NOM(1) = 'E'
      NOM(2) = 'NU'
      NOM(3) = 'SY'
      NOM(4) = 'D_SIGM_EPSI'
      NOM(5) = 'LONG_CARA'

      CALL RCVALA(IMATE,' ','ELAS'     ,0,' ',0.D0,
     &             2,NOM(1),VAL(1),RET,'F ')
      CALL RCVALA(IMATE,' ','ECRO_LINE',0,' ',0.D0,
     &             2,NOM(3),VAL(3),RET,'F ')
      CALL RCVALA(IMATE,' ','NON_LOCAL',0,' ',0.D0,
     &             1,NOM(5),VAL(5),RET,'F ')

      NU     = VAL(2)
      LAMBDA = VAL(1)*VAL(2) / (1-2*VAL(2)) / (1+VAL(2))
      DEUXMU = VAL(1) / (1.D0+VAL(2))
      GAMMA  = -VAL(4)/VAL(1)
      A      = 1+GAMMA
      WY     = VAL(3)**2 / (2*VAL(1))
      SEUIL  = (1+GAMMA)/GAMMA * WY
      J      = 2.D0/13.D0 * VAL(5)**2


C -- OPTIONS DE CALCUL

      CPLAN = TYPMOD.EQ.'C_PLAN  '
      ELAS  = OPTION.EQ.'RIGI_MECA_ELAS'
      TANG  = OPTION.EQ.'RIGI_MECA_TANG'
      FULL  = OPTION.EQ.'FULL_MECA'
      RAPH  = OPTION.EQ.'RAPH_MECA'
      DECO  = OPTION.EQ.'DECO_LOCA'
      IF (.NOT. (ELAS .OR. TANG .OR. RAPH .OR. FULL .OR. DECO) )
     &  CALL UTMESS('F','LCFRGV','DVP : OPTION '//OPTION//' NON PREVUE')


C -- DEFORMATIONS COURANTES

      NDIMSI = 2*NDIM
      CALL DCOPY(NDIMSI, EPSM,1, EPS,1)
      IF (RAPH.OR.FULL.OR.DECO) CALL DAXPY(NDIMSI,1.D0,DEPS,1,EPS,1)

C    DEFORMATION HORS PLAN POUR LES CONTRAINTES PLANES
      IF (CPLAN) THEN
        COPLAN  = - NU/(1.D0-NU)
        EPS(3) = COPLAN * (EPS(1)+EPS(2))
      END IF

C    ENERGIE DE DEFORMATION
      TREPS  = EPS(1)+EPS(2)+EPS(3)
      EPSEPS = DDOT(NDIMSI,EPS,1,EPS,1)
      W      = 0.5D0 * (LAMBDA*TREPS**2 + DEUXMU*EPSEPS)



C ----------------------------------------------------------------------
C                       CALCUL DES CONTRAINTES
C ----------------------------------------------------------------------


      IF (RAPH .OR. FULL) THEN
        D0 = VIP(1)
        DO 20 K=1, NDIMSI
          SIG(K) = (1-D0) * (LAMBDA*TREPS*KRON(K) + DEUXMU*EPS(K))
 20     CONTINUE
      ELSE IF (TANG .OR. ELAS) THEN
        D0 = VIM(1)
      END IF



C ----------------------------------------------------------------------
C                     CALCUL DE LA RIGIDITE SECANTE
C ----------------------------------------------------------------------

      IF (TANG .OR. ELAS .OR. FULL) THEN


C -- CONTRIBUTION ELASTIQUE

        CALL R8INIR(36, 0.D0, DSIDEP,1)
        FD = MAX(1-D0, RIGMIN)
        DO 80 K=1,3
          DO 90 L=1,3
            DSIDEP(K,L) = FD*LAMBDA
 90       CONTINUE
 80     CONTINUE
        DO 100 K=1,NDIMSI
          DSIDEP(K,K) = DSIDEP(K,K) + FD*DEUXMU
 100    CONTINUE


C -- CORRECTION POUR LES CONTRAINTES PLANES

        IF (CPLAN) THEN
          DO 130 K=1,NDIMSI
            IF (K.EQ.3) GOTO 130
            DO 140 L=1,NDIMSI
              IF (L.EQ.3) GO TO 140
              DSIDEP(K,L)=DSIDEP(K,L)
     &        - 1.D0/DSIDEP(3,3)*DSIDEP(K,3)*DSIDEP(3,L)
 140        CONTINUE
 130      CONTINUE
        ENDIF

      ENDIF



C ----------------------------------------------------------------------
C                     CALCUL DE L'ENDOMMAGEMENT
C ----------------------------------------------------------------------




      IF (DECO) THEN


C      ENERGIE NORMALISEE PAR WY
C      POUR AVOIR UNE RESIDU RELATIF (PROBLEME PRIMAL)

        WREF    = WY

        WY = WY/WREF
        W  = W/WREF
        SEUIL = SEUIL/WREF

C      TERME EN GRADIENT DANS L'ENERGIE LIBRE
        D0M = VIM(1)
        D2PHIM = 2*WY*A**2/(A-D0M)**3
        D2PHI0 = 2*WY/A

C      TERME DE NORMALISATION POUR LA PENALISATION
        N0 = D2PHIM
        NG = D2PHI0 * 2*J


C -- INTEGRATION DU TERME EN GRADIENT

        COEFG = RLAG*NG + 2*J*D2PHI0
        DO 200 K = 1,NDIM
         DG(K) = (RLAG*NG*CHAMP(K)+LAGR(K))/COEFG
 200    CONTINUE


C -- INTEGRATION DU TERME EN VALEUR

        C1 = RLAG*N0 / WY / A**2
        C2 = (W + LAGR(0) + RLAG*N0*CHAMP(0)) / WY / A**2
        Q0 = (1-C2*A**2)/C1
        Q1 = (C1*A**2 + 2*C2*A)/C1
        Q2 = -(2*C1*A+C2)/C1
        CALL ZEROP3(Q2,Q1,Q0,RAC,NRAC)
        IF (NRAC.EQ.3) THEN
          D0 = RAC(3)
        ELSE
          D0 = RAC(1)
        END IF


C      PROJECTION DE L'ENDOMMAGEMENT SUR L'INTERVALLE ADMISSIBLE
        ETAT = NINT(VIM(6))
        IF (ETAT.EQ.2) THEN
          D0 = DMAX
        ELSE
          IF (D0 .LT. D0M) THEN
             D0   = VIM(1)
             ETAT = 0
          ELSE IF (D0 .GT. DMAX) THEN
             D0   = DMAX
             ETAT = 2
          ELSE
             ETAT = 1
          END IF
        END IF

C -- CALCUL DU LAPLACIEN

        IF (ETAT.EQ.1) THEN

C        ON EST SUR LE SEUIL
          LAP = (WY*(A/(A-D0))**2 - W)/NG

        ELSE IF (ETAT.EQ.0) THEN

C        ON NE PEUT ETRE QU'A L'INTERIEUR DU SEUIL
          LAPM = VIM(5)
          LAP = LAPM
C          IF (W .LE. WY*(A/(A-D0M))**2 - NG*LAPM) THEN
C            LAP = LAPM
C          ELSE
C            LAP = (WY*(A/(A-D0))**2 - W)/NG
C          END IF


        ELSE

C        ON NE SE SERT PLUS DE CE POINT POUR PILOTER
          LAP = VIM(5)
        END IF

C -- ENERGIE

        DG2  = DDOT(NDIM, DG,1, DG,1)
        ENER = W*(1-D0) + SEUIL*D0*(D0-1)/(A-D0)
     &       + D2PHI0*J*DG2 + SEUIL*(D0-D0M)


C -- MATRICE TANGENTE


        CALL R8INIR(16, 0.D0, DVIDA,1)


C      TERME EN D0.D0
        IF (ETAT.EQ.1) THEN
          D0C2 = RLAG*N0/WY/A**2
          D0Q0 = -D0C2/C1*A**2
          D0Q1 = 2*D0C2/C1*A
          D0Q2 = -D0C2/C1
          DVIDA(0,0) = - (D0Q2*D0**2 + D0Q1*D0 + D0Q0)
     &             /   (3*D0**2 + 2*Q2*D0 + Q1)
        END IF

        DO 300 K = 1, NDIM
          DVIDA(K,K) = RLAG*NG / COEFG
 300    CONTINUE


C -- STOCKAGE DES RESULTATS

        PONDER(0) = N0
        CALL R8INIR(NDIM, NG, PONDER(1),1)

        VIP(1) = D0
        CALL DCOPY(NDIM, DG,1, VIP(2),1)
        VIP(5) = LAP
        VIP(6) = ETAT

      END IF

      END
