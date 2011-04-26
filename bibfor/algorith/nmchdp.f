      SUBROUTINE NMCHDP(MAT,PM,NDIMSI,SIGEDV,NBVAR,EPSPM,ALFAM,ALFA2M,
     &DEUXMU,CRIT,SEUIL,VISC,MEMO,DT,RM,QM,KSIM,RP,QP,KSIP,DP,IRET,ITER)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C TOLE CRP_21
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C.======================================================================
C RESPONSABLE JMBHH01 J.M.PROIX
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
C    EPSPM(6)       IN    R       DEFORMATION PLASTIQUE A L'INSTANT -
C    ALFAM(6)       IN    R       LE TENSEUR DE RAPPEL XM A L'INSTANT -
C    ALFA2M(6)                     DU CALCUL PRECEDENT EST RELIE
C                                 AU TENSEUR ALFAM PAR XM = 2/3*C*ALFAM
C    DEUXMU         IN    R       COEFFICIENT DE LAME :2*MU
C    CRIT(6)        IN    R       TABLEAU DE CRITERES LOCAUX
C                                 DE CONVERGENCE :
C                                 CRIT(1) : NOMBRE D'ITERATIONS
C                                 MAXIMUM A LA CONVERGENCE ...
C    SEUIL          IN    R       CRITERE DE PLASTICITE
C                                 SEUIL = F - RP
C    VISC           IN    I       INDICATEUR DE VISCOSITE
C    MEMO           IN    R       INDICATEUR EFFET DE MEMOIRE
C    DT             IN    R       VALEUR DE L'INCREMENT DE TEMPS DELTAT
C    RM             IN    R       R(PM)
C    QM             IN    R       Q(PM)
C    KSIM           IN    R       KSI(PM)
C    RP             OUT   R       R(PM+DP)
C    QP             OUT   R       Q(PM+DP)
C    KSIP           OUT   R       KSI(PM+DP)
C    DP             OUT   R       INCREMENT DE DEFORMATION PLASTIQUE
C                                 CUMULEE
C    IRET           OUT   I    CODE RETOUR DE  L'INTEGRATION DE LA LDC
C                              IRET=0 => PAS DE PROBLEME
C                              IRET=1 => ABSENCE DE CONVERGENCE DANS
C                                        LORS DE L'INTEGRATION DE LA
C                                        LOI
C    ITER           OUT    I   NOMBRE D'ITERATIONS POUR CONVERGER
C
C -----  ARGUMENTS
          INTEGER             NDIMSI,NBVAR,VISC,MEMO
           REAL*8             MAT(*),PM,SIGEDV(6),ALFAM(*),DEUXMU
           REAL*8             CRIT(*),SEUIL,ALFA2M(*)
           REAL*8             DP,DT,RP,QP,KSIP(6)
C -----  VARIABLES LOCALES
           INTEGER     NITER,I,ITER,IFM,NIV,NBP, IRET
           REAL*8      X(4),Y(4),Z,ZZ,KSIM(6),QM
           REAL*8      ZERO,DIX,RM,EPSPM(6),DPMAX1
           REAL*8      PREC,F0,DPE,DPMAX,FMAX,DDP
           REAL*8      CINF,K ,W,C2INF,CM ,C2M,KVI,VALDEN
           CHARACTER*8 NOMVAR(16)
           DATA NOMVAR/'R0','RINF','B','CINF','K','W','GAMMA0',
     &     'AINF','C2INF','GAMM20','KVI','N','ETA','QM','Q0','MU'/

C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C --- INITIALISATIONS :
C     ===============
      ZERO   =  0.0D0
      DIX    = 10.0D0
      IRET=0
C --- POUR CHERCHER LA SOLUTION, PREMIERE APPROXIMATION
      CINF       =  MAT(4)
      K          =  MAT(5)
      W          =  MAT(6)
      C2INF      =  MAT(9)
      CM  = CINF * (1.D0 + (K-1.D0)*EXP(-W*PM))
      C2M = C2INF *(1.D0 + (K-1.D0)*EXP(-W*PM))
      DPMAX = SEUIL/(1.5D0*DEUXMU+CM+C2M)
      IF (VISC.EQ.1) THEN
         VALDEN=MAT(11)
         KVI =MAT(12)
         DPMAX1=DT*(SEUIL/KVI)**VALDEN
         IF (DPMAX1.LT.1.D0) THEN
            DPMAX=MAX(DPMAX,DPMAX1)
         ENDIF
      ENDIF
C
C --- EXAMEN DE LA SOLUTION DPE = 0 :
C     =============================
      DPE = ZERO
C
C --- CALCUL DE LA VALEUR F0 DE LA FONCTION DONT ON CHERCHE LA RACINE
C --- POUR DP = 0 :
C     -----------
      CALL NMCHCR(MAT,DPE,PM,NDIMSI,SIGEDV,NBVAR,EPSPM,ALFAM,ALFA2M,
     &    DEUXMU,VISC,MEMO,RM,RP,QM,QP,KSIM,KSIP,DT,F0)
C
C --- NOMBRE D'ITERATIONS DONT ON DISPOSE POUR CONVERGER ET TOLERANCE
C --- SUR LA VALEUR CONVERGEE :
C     -----------------------
      NITER = INT(CRIT(1))
      PREC  = CRIT(3)
C
C     RECHERCHE DES BORNES 0-DPMAX
C
      IF (ABS(F0).LE.PREC) THEN
        DP = DPE
        GOTO 50
      ELSEIF (F0.LE.ZERO) THEN
        CALL U2MESS('A','ELEMENTS4_61')
        GOTO 41
      ELSE
        X(2) = DPE
        Y(2) = F0
C
C ---   F0 > 0 , ON CHERCHE DPMAX TEL QUE FMAX < 0 :
C
        CALL NMCHCR(MAT,DPMAX,PM,NDIMSI,SIGEDV,NBVAR,EPSPM,ALFAM,ALFA2M,
     &              DEUXMU,VISC,MEMO,RM,RP,QM,QP,KSIM,KSIP,DT,FMAX)
        IF (ABS(FMAX).LE.PREC) THEN
           DP = DPMAX
           ITER=1
           GOTO 50
        ELSEIF (FMAX.LT.ZERO) THEN
C          FMAX < 0.
C          VERIFICATION QUE DPMAX N'EST PAS TROP GRAND. BRACKETTING
           DO 31 I = 1, NITER
              DPMAX = DPMAX/DIX
              CALL NMCHCR(MAT,DPMAX,PM,NDIMSI,SIGEDV,NBVAR,EPSPM,ALFAM,
     &            ALFA2M,DEUXMU,VISC,MEMO,RM,RP,QM,QP,KSIM,KSIP,DT,FMAX)
              IF (ABS(FMAX).LE.PREC) THEN
                 DP = DPMAX
                 ITER=I
                 GOTO 50
              ELSEIF (FMAX.GT.ZERO) THEN
C                ON RECALCULE LA VALEUR PRECEDENTE DE DPMAX
                 DPMAX = DPMAX*DIX
                 CALL NMCHCR(MAT,DPMAX,PM,NDIMSI,SIGEDV,NBVAR,EPSPM,
     &                       ALFAM,ALFA2M,DEUXMU,VISC,MEMO,RM,RP,QM,QP,
     &                       KSIM,KSIP,DT,FMAX)
                 X(1) = DPMAX
                 Y(1) = FMAX
                 GOTO 20
              ENDIF
  31       CONTINUE
           X(1) = DPMAX
           Y(1) = FMAX
           GOTO 20

        ELSE
C          FMAX >0. On augmente DPMAX jusqu'à ce que F(DPMAX) < 0
           DO 30 I = 1, NITER
              CALL NMCHCR(MAT,DPMAX,PM,NDIMSI,SIGEDV,NBVAR,EPSPM,ALFAM,
     &          ALFA2M,DEUXMU,VISC,MEMO,RM,RP,QM,QP,KSIM,KSIP,DT,FMAX)
              IF (ABS(FMAX).LE.PREC) THEN
                 DP = DPMAX
                 ITER=I
                 GOTO 50
              ELSEIF (FMAX.LT.ZERO) THEN
                 X(1) = DPMAX
                 Y(1) = FMAX
                 GOTO 20
              ELSE
                 DPMAX = DPMAX*DIX
              ENDIF
  30       CONTINUE
           CALL U2MESS('A','ALGORITH6_79')
           GOTO 20
        ENDIF

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
      IF (ABS(Y(4)).LT.PREC) GOTO 50
      DO 40 ITER = 1, NITER
        IF (Y(1).GT.0 .OR. Y(2).LT.0) THEN
           CALL U2MESS('A','ALGORITH6_78')
           GOTO 41
        ENDIF
        IF (X(3).EQ.X(4)) THEN
           CALL U2MESS('A','ALGORITH9_84')
           GOTO 41
        ENDIF
        CALL ZEROCO(X,Y)
        DP = X(4)
        CALL NMCHCR(MAT,DP,PM,NDIMSI,SIGEDV,NBVAR,EPSPM,ALFAM,ALFA2M,
     &              DEUXMU,VISC,MEMO,RM,RP,QM,QP,KSIM,KSIP,DT,Y(4))
        IF (ABS(Y(4)).LT.PREC) GOTO 50
  40  CONTINUE
C
  41  CONTINUE

C     CAS DE NON CONVERGENCE : IMPRESSIONS SI INFO=2
      CALL INFNIV(IFM,NIV)
      IF (NIV.EQ.2) THEN
         WRITE (IFM,*) 'MODELE CINX_CHAB : ATTENTION'
         WRITE (IFM,*) 'PAS DE CONVERGENCE A LA PRECISION DEMANDEE',PREC
         WRITE (IFM,*) 'AU BOUT DU NOMBRE D ITERATION DEMANDE',NITER
         WRITE (IFM,*) 'VALEURS DE DP ET F ACTUELLES',DP,Y(4)
         WRITE (IFM,*) 'AUGMENTER ITER_INTE_MAXI'
         WRITE (IFM,*) 'PARAMETRES :'
         DO 61 I=1,16
             WRITE (IFM,*) NOMVAR(I),MAT(I)
  61     CONTINUE
         WRITE (IFM,*) 'F0',F0,'DPMAX',DPMAX
         NBP = 20
         DDP = DPMAX/NBP
         WRITE (IFM,*) 'DP     -     F(DP)'
         Z=ZERO
         DO 60 I = 1,NBP
            CALL NMCHCR(MAT,Z,PM,NDIMSI,SIGEDV,NBVAR,EPSPM,ALFAM,ALFA2M,
     &                  DEUXMU,VISC,MEMO,RM,RP,QM,QP,KSIM,KSIP,DT,ZZ)
            WRITE (IFM,*) Z,ZZ
            Z = Z + DDP
  60     CONTINUE
      ENDIF
      IRET = 1
C
  50  CONTINUE
      END
