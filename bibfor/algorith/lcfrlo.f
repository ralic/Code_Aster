      SUBROUTINE LCFRLO (NDIM, TYPMOD, IMATE, EPSM, DEPS,
     &                   VIM, OPTION, SIG, VIP,  DSIDEP)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 07/01/2003   AUTEUR GJBHHEL E.LORENTZ 
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
      CHARACTER*8        TYPMOD(*)
      CHARACTER*16       OPTION
      INTEGER            NDIM, IMATE
      REAL*8             EPSM(6), DEPS(6), VIM(2)
      REAL*8             SIG(6), VIP(2), DSIDEP(6,6)
C ----------------------------------------------------------------------
C     LOI DE COMPORTEMENT ELASTIQUE FRAGILE (SANS REGULARISATION)
C
C IN  NDIM    : DIMENSION DE L'ESPACE
C IN  TYPMOD  : TYPE DE MODELISATION
C IN  IMATE   : NATURE DU MATERIAU
C IN  EPSM    : DEFORMATION EN T-
C IN  DEPS    : INCREMENT DE DEFORMATION
C IN  VIM     : VARIABLES INTERNES EN T-
C IN  OPTION  : OPTION DEMANDEE
C                 RIGI_MECA_TANG ->     DSIDEP
C                 FULL_MECA      -> SIG DSIDEP VIP
C                 RAPH_MECA      -> SIG        VIP
C OUT SIG     : CONTRAINTE
C OUT VIP     : VARIABLES INTERNES
C                 1   -> VALEUR DE L'ENDOMMAGEMENT
C                 2   -> ELASTIQUE (0) OU DISSIPATIF (1)
C OUT DSIDEP  : MATRICE TANGENTE
C ----------------------------------------------------------------------




      LOGICAL     CPLAN, TANG, ELAS, FULL, RAPH, NONLIN
      INTEGER     NDIMSI, K, L, ETAT

      REAL*8      EPS(6), TREPS, COPLAN, SIGEL(6)
      REAL*8      KRON(6)
      REAL*8      RIGMIN, FD, D, DM, ENER, COEF
      REAL*8      E, NU, ALPHA, LAMBDA, DEUXMU, GAMMA, SY, WY

      CHARACTER*2 CODRET(3)
      CHARACTER*8 NOMRES(3)
      REAL*8      VALRES(3)

      REAL*8      R8DOT

      REAL*8      DMAX, FMIN
      PARAMETER  (DMAX = 1.D0, FMIN = 1.D-5)
      DATA  KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/
C ----------------------------------------------------------------------



C ======================================================================
C                            INITIALISATION
C ======================================================================


C -- OPTION ET MODELISATION

      FULL = OPTION .EQ. 'FULL_MECA'
      RAPH = OPTION .EQ. 'RAPH_MECA'
      ELAS = OPTION .EQ. 'RIGI_MECA_ELAS'
      TANG = OPTION .EQ. 'RIGI_MECA_TANG'
      CPLAN = (TYPMOD(1).EQ.'C_PLAN  ')
      NDIMSI = 2*NDIM


C -- LECTURE DES CARACTERISTIQUES ELASTIQUES

      NOMRES(1) = 'E'
      NOMRES(2) = 'NU'
      CALL RCVALA ( IMATE,'ELAS',0,' ',0.D0,2,
     &              NOMRES,VALRES,CODRET, 'FM')

      E     = VALRES(1)
      NU    = VALRES(2)
      LAMBDA = E * NU / (1.D0+NU) / (1.D0 - 2.D0*NU)
      DEUXMU = E/(1.D0+NU)


C -- LECTURE DES CARACTERISTIQUES D'ENDOMMAGEMENT
      NOMRES(1) = 'SY'
      NOMRES(2) = 'D_SIGM_EPSI'
      CALL RCVALA(IMATE,'ECRO_LINE',0,' ',0.D0,2,
     &            NOMRES,VALRES,CODRET,'FM')
      SY = VALRES(1)
      GAMMA  = - VALRES(2)/E
      WY  = SY**2 / (2*E)


C -- DEFORMATIONS

      CALL R8COPY(NDIMSI, EPSM,1, EPS,1)
      IF (RAPH .OR. FULL) CALL R8AXPY(NDIMSI, 1.D0, DEPS,1, EPS,1)



C ======================================================================
C                         CONTRAINTES ELASTIQUES
C ======================================================================

C -- SI CONTRAINTES PLANES

      IF (CPLAN) THEN
        COPLAN  = - NU/(1.D0-NU)
        EPS(3)  = COPLAN * (EPS(1)+EPS(2))
      END IF


C -- CALCUL DES CONTRAINTES ELASTIQUES

      TREPS = EPS(1)+EPS(2)+EPS(3)
      DO 60 K=1,NDIMSI
        SIGEL(K) = LAMBDA*TREPS*KRON(K) + DEUXMU*EPS(K)
 60   CONTINUE
      ENER = 0.5D0 * R8DOT(NDIMSI,EPS,1,SIGEL,1)



C ======================================================================
C                 INTEGRATION DE LA LOI DE COMPORTEMENT
C ======================================================================

      IF (RAPH .OR. FULL) THEN

        DM   = VIM(1)
        ETAT = NINT(VIM(2))


C -- POINT DEJA SATURE

       IF (ETAT.EQ.2) THEN
         D = DM


C -- CALCUL DE L'ETAT D'ENDOMMAGEMENT
        ELSE
          IF (ENER .LE. WY*((1+GAMMA)/(1+GAMMA-DM))**2) THEN
            D = DM
            ETAT = 0
          ELSE
            ETAT = 1
            D = MAX(DM, (1+GAMMA)*(1-SQRT(WY/ENER)))
            IF (D.GT.DMAX) THEN
              D = DMAX
              ETAT = 2
            END IF
          END IF
        END IF

C -- CALCUL DES CONTRAINTES

        DO 30 K=1,NDIMSI
          SIG(K) = (1-D) * SIGEL(K)
 30     CONTINUE


C -- STOCKAGE DES VARIABLES INTERNES

        VIP(1) = D
        VIP(2) = ETAT

      ELSE
        D   = VIM(1)
        ETAT=NINT(VIM(2))
      END IF



C ======================================================================
C                            MATRICE TANGENTE
C ======================================================================


      IF (FULL .OR. TANG .OR. ELAS) THEN


C -- RIGI_MECA_ELAS : MATRICE ELASTIQUE IMPOSEE

        IF (ELAS) NONLIN = .FALSE.


C -- RIGI_MECA_TANG : MATRICE ELASTIQUE FONCTION DU PAS PRECEDENT

        IF (TANG) NONLIN = ETAT.EQ.1


C -- FULL_MECA : MATRICE ELASTIQUE FONCTION DU COMPORTEMENT

        IF (FULL) NONLIN = ETAT.EQ.1


C -- CONTRIBUTION ELASTIQUE

        CALL R8INIR(36, 0.D0, DSIDEP, 1)
        FD = 1-D
        FD = MAX(FMIN, FD)
        DO 100 K = 1,3
          DO 110 L = 1,3
            DSIDEP(K,L) = FD*LAMBDA
 110      CONTINUE
 100    CONTINUE
        DO 120 K = 1,NDIMSI
          DSIDEP(K,K) = DSIDEP(K,K) + FD*DEUXMU
 120    CONTINUE


C -- CONTRIBUTION DISSIPATIVE

        IF (NONLIN) THEN
          COEF = (1+GAMMA-D)**3 / (WY*2*(1+GAMMA)**2)
          DO 200 K = 1,NDIMSI
            DO 210 L = 1, NDIMSI
              DSIDEP(K,L) = DSIDEP(K,L) - COEF * SIGEL(K) * SIGEL(L)
 210        CONTINUE
 200      CONTINUE
        END IF


C -- CORRECTION CONTRAINTES PLANES

        IF (CPLAN) THEN
          DO 300 K=1,NDIMSI
            IF (K.EQ.3) GO TO 300
            DO 310 L=1,NDIMSI
              IF (L.EQ.3) GO TO 310
              DSIDEP(K,L)=DSIDEP(K,L)
     &        - 1.D0/DSIDEP(3,3)*DSIDEP(K,3)*DSIDEP(3,L)
 310        CONTINUE
 300      CONTINUE
        ENDIF

      END IF
      END
