      SUBROUTINE NMCHDP(MAT,PM,NDIMSI,SIGEDV,NBVAR,ALFAM,ALFA2M,DEUXMU,
     &                  CRIT,SEUIL,ETA,DT,VALDEN,DP)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 18/06/2002   AUTEUR F6BHHBO P.DEBONNIERES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
C              SEE THE FILE "LICENSE.TERMS" FOR INFORMATION ON USAGE AND
C              REDISTRIBUTION OF THIS FILE.
C ======================================================================
C.======================================================================
C      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT NONE
C
C      NMCHDP   -- CETTE ROUTINE CONCERNE L'INTEGRATION DE LA LOI
C                  DE COMPORTEMENT 'VISC_CIN1_CHAB' OU VISC_CIN2_CHAB
C                  RESOLUTION DE L'EQUATION SCALAIRE NON LINEAIRE EN DP
C                  (INCREMENT DE DEFORMATION PLASTIQUE CUMULEE) :
C
C  ||(RP/DENOMI*SIGEDV - MP*GAMMAP*DP*
C                       (-2/3+DP/DENOMI*(2*MU+2/3*MP))*ALPHAM)|| = RP
C
C                  CETTE EQUATION EST RELATIVE AU MODELE DE CHABOCHE
C                  A UNE OU DEUX TENSEURS CINEMATIQUES
C                  ET ELLE EST RESOLUE PAR UNE METHODE DE SECANTES
C
C   ARGUMENT        E/S  TYPE         ROLE
C    MAT(6+2*NBVAR) IN    R       TABLEAU DES COEFFICIENTS
C                                 D'ECROUISSAGE DU MATERIAU
C    PM             IN    R       DEFORMATION PLASTIQUE CUMULEE A
C                                 L'INSTANT DU CALCUL PRECEDENT
C    NDIMSI         IN    I       DIMENSION DU VECTEUR DES CONTRAINTES
C                                 I.E. 4 EN 2D ET 6 EN 3D
C    SIGEDV(6)       IN    R       VECTEUR DES CONTRAINTES D'ESSAI, I.E.
C                                 SIGEDV = MU/(MU-)*SIGM +2MU*DELTA_EPS
C    NBVAR          IN    R       NOMBRE DE TENSEURS DE RAPPEL
C    ALFAM(6)       IN    R       LE TENSEUR DE RAPPEL XM A L'INSTANT
C    ALFA2M(6)                     DU CALCUL PRECEDENT EST RELIE
C                                 AU TENSEUR ALFAM PAR XM = 2/3*C*ALFAM
C    DEUXMU         IN    R       COEFFICIENT DE LAME :2*MU
C    CRIT(6)        IN    R       TABLEAU DE CRITERES LOCAUX
C                                 DE CONVERGENCE :
C                                 CRIT(1) : NOMBRE D'ITERATIONS
C                                 MAXIMUM A LA CONVERGENCE ...
C    SEUIL          IN    R       CRITERE DE PLASTICITE
C                                 SEUIL = F - RP
C    ETA            IN    R       PARAMETRE ETA DE VISCOSITE
C    DT             IN    R       VALEUR DE L'INCREMENT DE TEMPS DELTAT
C    VALDEN         IN    R       PARAMETRE N DE VISCOSITE
C    DP             OUT   R       INCREMENT DE DEFORMATION PLASTIQUE
C                                 CUMULEE
C
C -----  ARGUMENTS
          INTEGER             NDIMSI,NBVAR
           REAL*8             MAT(*),PM,SIGEDV(6),ALFAM(*),DEUXMU
           REAL*8             CRIT(*),SEUIL,ALFA2M(*)
           REAL*8             DP,ETA,DT,VALDEN
C -----  VARIABLES LOCALES
           INTEGER     NITER,I,ITER
           REAL*8      X(4),Y(4),MU
           REAL*8      ZERO,DEUX,TROIS,DIX
           REAL*8      PREC,F0,DPE,DPMAX,FMAX
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C --- INITIALISATIONS :
C     ===============
      ZERO   =  0.0D0
      DEUX   =  2.0D0
      TROIS  =  3.0D0
      DIX    = 10.0D0
      MU     = DEUXMU/DEUX
C
C --- EXAMEN DE LA SOLUTION DPE = 0 :
C     =============================
      DPE = ZERO
C
C --- CALCUL DE LA VALEUR F0 DE LA FONCTION DONT ON CHERCHE LA RACINE
C --- POUR DP = 0 :
C     -----------
      CALL NMCHCR(MAT,DPE,PM,NDIMSI,SIGEDV,NBVAR,ALFAM,ALFA2M,DEUXMU,
     +            ETA,DT,VALDEN,F0)
C
C --- NOMBRE D'ITERATIONS DONT ON DISPOSE POUR CONVERGER ET TOLERANCE
C --- SUR LA VALEUR CONVERGEE :
C     -----------------------
      NITER = INT(CRIT(1))
      PREC  = CRIT(3)
C
      IF (ABS(F0).LE.PREC) THEN
        DP = DPE
        GOTO 50
      ELSEIF (F0.LE.ZERO) THEN
        X(1) = DPE
        Y(1) = F0
C
C ---   F0 < 0 , ON CHERCHE DPMAX TEL QUE FMAX > 0 :
C ---   EXAMEN DE LA SOLUTION DP = SEUIL/(3*MU) :
C       =======================================
        DPMAX = SEUIL/(TROIS*MU)
C
        DO 10 I = 1, 5
C
C ---    CALCUL DE LA VALEUR FMAX DE LA FONCTION DONT ON CHERCHE LA
C ---    RACINE POUR DP = SEUIL/(3*MU) :
C        -----------------------------
         CALL NMCHCR(MAT,DPMAX,PM,NDIMSI,SIGEDV,NBVAR,ALFAM,ALFA2M,
     &               DEUXMU,ETA,DT,VALDEN,FMAX)
C
           IF (FMAX.GE.ZERO) THEN
             X(2) = DPMAX
             Y(2) = FMAX
             GOTO 20
           ELSE
             DPMAX = DPMAX*DIX
           ENDIF
C
  10    CONTINUE
C
        CALL UTMESS('A','NMCHDP','F RESTE TOUJOURS NEGATIVE.')
        GOTO 20
C
      ELSE
C
        X(2) = DPE
        Y(2) = F0
C
C ---   F0 > 0 , ON CHERCHE DPMAX TEL QUE FMAX < 0 :
C ---   EXAMEN DE LA SOLUTION DP = SEUIL/(3*MU) :
C       =======================================
        DPMAX = SEUIL/(TROIS*MU)
C
        DO 30 I = 1, 5
C
C ---    CALCUL DE LA VALEUR FMAX DE LA FONCTION DONT ON CHERCHE LA
C ---    RACINE POUR DP = SEUIL/(3*MU) :
C        -----------------------------
         CALL NMCHCR(MAT,DPMAX,PM,NDIMSI,SIGEDV,NBVAR,ALFAM,ALFA2M,
     &               DEUXMU,ETA,DT,VALDEN,FMAX)
C
           IF (FMAX.LE.ZERO) THEN
             X(1) = DPMAX
             Y(1) = FMAX
             GOTO 20
           ELSE
             DPMAX = DPMAX*DIX
           ENDIF
C
  30    CONTINUE
C
        CALL UTMESS('A','NMCHDP','F RESTE TOUJOURS POSITIVE.')
        GOTO 20
C
      ENDIF
C
   20 CONTINUE
C
C --- CALCUL DE X(4) SOLUTION DE L'EQUATION F = 0 :
C     ===========================================
      X(3) = X(1)
      Y(3) = Y(1)
      X(4) = X(2)
      Y(4) = Y(2)
C
      DO 40 ITER = 1, NITER
C
        IF (ABS(Y(4)).LT.PREC) GOTO 50
        CALL ZEROCO(X,Y)
        DP = X(4)
        CALL NMCHCR(MAT,DP,PM,NDIMSI,SIGEDV,NBVAR,ALFAM,ALFA2M,
     &               DEUXMU,ETA,DT,VALDEN,Y(4))
C
  40  CONTINUE
C
      CALL UTMESS('F','NMCHDP',' F = 0 : PAS DE CONVERGENCE ')
C
  50  CONTINUE
C
      END
