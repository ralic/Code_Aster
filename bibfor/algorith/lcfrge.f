      SUBROUTINE LCFRGE (NDIM, TYPMOD, IMATE, EPSM, DEPS,
     &                   VIM, OPTION, SIG, VIP,DSIDPT,
     &                   PROJ)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 10/05/2005   AUTEUR GJBHHEL E.LORENTZ 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C
C
C ======================================================================


      IMPLICIT NONE
      CHARACTER*8        TYPMOD(*)
      CHARACTER*16       OPTION
      INTEGER            NDIM, IMATE
      REAL*8             EPSM(12), DEPS(12), VIM(2)
      REAL*8             SIG(6), VIP(2), DSIDPT(6,6,2),PROJ(6,6)
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
C OUT PROJ    : COUPURE (0.D0) OU NON (1.D0) DU TERME DE REGULARISATION
C ----------------------------------------------------------------------




      LOGICAL     CPLAN, RESI, RIGI, ELAS, NONLIN
      INTEGER     NDIMSI, K, L, ETAT

      REAL*8      EPS(6), EPSR(6),TREPS, COPLAN, SIGEL(6)
      REAL*8      KRON(6), TREPSR, SIGELR(6)
      REAL*8      RIGMIN, FD, D, DM, ENER, COEF
      REAL*8      E, NU, ALPHA, LAMBDA, DEUXMU, GAMMA, SY, WY

      CHARACTER*2 CODRET(3)
      CHARACTER*8 NOMRES(3)
      REAL*8      VALRES(3)

      REAL*8      DDOT

      REAL*8      DMAX
      PARAMETER  (DMAX = 0.999D0)
      DATA  KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/
C ----------------------------------------------------------------------



C ======================================================================
C                            INITIALISATION
C ======================================================================


C -- OPTION ET MODELISATION

      RESI = OPTION(1:4).EQ.'RAPH' .OR. OPTION(1:4).EQ.'FULL'
      RIGI = OPTION(1:4).EQ.'RIGI' .OR. OPTION(1:4).EQ.'FULL'
      ELAS = OPTION(11:14).EQ.'ELAS'

      CPLAN = (TYPMOD(1).EQ.'C_PLAN  ')
      NDIMSI = 2*NDIM


C -- LECTURE DES CARACTERISTIQUES ELASTIQUES

      NOMRES(1) = 'E'
      NOMRES(2) = 'NU'
      CALL RCVALA(IMATE,' ','ELAS',0,' ',0.D0,2,
     &              NOMRES,VALRES,CODRET, 'FM')

      E     = VALRES(1)
      NU    = VALRES(2)
      LAMBDA = E * NU / (1.D0+NU) / (1.D0 - 2.D0*NU)
      DEUXMU = E/(1.D0+NU)


C -- LECTURE DES CARACTERISTIQUES D'ENDOMMAGEMENT
      NOMRES(1) = 'SY'
      NOMRES(2) = 'D_SIGM_EPSI'
      CALL RCVALA(IMATE,' ','ECRO_LINE',0,' ',0.D0,2,
     &            NOMRES,VALRES,CODRET,'FM')
      SY = VALRES(1)
      GAMMA  = - VALRES(2)/E
      WY  = SY**2 / (2*E)


C -- DEFORMATIONS

      CALL DCOPY(NDIMSI, EPSM,1, EPS,1)
      CALL DCOPY(NDIMSI, EPSM(7),1, EPSR,1)
      IF (RESI) THEN
        CALL DAXPY(NDIMSI, 1.D0, DEPS,1, EPS,1)
        CALL DAXPY(NDIMSI, 1.D0, DEPS(7),1, EPSR,1)
      END IF


C -- COUPURE ISOTROPE DE LA REGULARISATION SI ENDOMMAGEMENT SATURE
      CALL R8INIR(36,0.D0,PROJ,1)
      IF (VIM(2).NE.2) CALL R8INIR(6,1.D0,PROJ,7)



C ======================================================================
C                         CONTRAINTES ELASTIQUES
C ======================================================================

C -- SI CONTRAINTES PLANES

      IF (CPLAN) THEN
        COPLAN  = - NU/(1.D0-NU)
        EPS(3)   = COPLAN * (EPS(1)+EPS(2))
        EPSR(3)  = COPLAN * (EPSR(1)+EPSR(2))
      END IF


C -- CALCUL DES CONTRAINTES ELASTIQUES

      TREPS = EPS(1)+EPS(2)+EPS(3)
      TREPSR = EPSR(1)+EPSR(2)+EPSR(3)
      DO 60 K=1,NDIMSI
        SIGEL(K)  = LAMBDA*TREPS*KRON(K) + DEUXMU*EPS(K)
        SIGELR(K) = LAMBDA*TREPSR*KRON(K) + DEUXMU*EPSR(K)
 60   CONTINUE
      ENER = 0.5D0 * DDOT(NDIMSI,EPSR,1,SIGELR,1)



C ======================================================================
C                 INTEGRATION DE LA LOI DE COMPORTEMENT
C ======================================================================

      IF (RESI) THEN

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


      IF (RIGI) THEN
        NONLIN = (.NOT. ELAS) .AND. (ETAT.EQ.1)

C -- CONTRIBUTION ELASTIQUE

        CALL R8INIR(72, 0.D0, DSIDPT, 1)
        FD = 1-D
        DO 100 K = 1,3
          DO 110 L = 1,3
            DSIDPT(K,L,1) = FD*LAMBDA
 110      CONTINUE
 100    CONTINUE
        DO 120 K = 1,NDIMSI
          DSIDPT(K,K,1) = DSIDPT(K,K,1) + FD*DEUXMU
 120    CONTINUE


C -- CONTRIBUTION DISSIPATIVE

        IF (NONLIN) THEN
          COEF = (1+GAMMA-D)**3 / (WY*2*(1+GAMMA)**2)
          DO 200 K = 1,NDIMSI
            DO 210 L = 1, NDIMSI
              DSIDPT(K,L,2) = DSIDPT(K,L,2)-COEF*SIGEL(K)*SIGELR(L)
 210        CONTINUE
 200      CONTINUE
        END IF


C -- CORRECTION CONTRAINTES PLANES

        IF (CPLAN) THEN
          DO 300 K=1,NDIMSI
            IF (K.EQ.3) GO TO 300
            DO 310 L=1,NDIMSI
              IF (L.EQ.3) GO TO 310
              DSIDPT(K,L,1)=DSIDPT(K,L,1)
     &        - 1.D0/DSIDPT(3,3,1)*DSIDPT(K,3,1)*DSIDPT(3,L,1)
 310        CONTINUE
 300      CONTINUE
        ENDIF


      END IF
      END
