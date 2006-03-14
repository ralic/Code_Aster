      SUBROUTINE NMVEEI (FAMI,KPG,KSP,NDIM,TYPMOD,IMATE,COMPOR,CRIT,
     &                   INSTAM,INSTAP,TM,TP,TREF,EPSM,DEPS,SIGM,VIM,
     &                   OPTION,SIGP,VIP,DSIDEP,IRET)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/03/2006   AUTEUR JOUMANA J.EL-GHARIB 
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
C-----------------------------------------------------------------------
C TOLE CRP_21
      IMPLICIT NONE
      INTEGER            NDIM,IMATE,IRET,KPG,KSP
      CHARACTER*16       COMPOR(*),OPTION
      CHARACTER*8        TYPMOD(*)
      CHARACTER*(*)      FAMI
      REAL*8             CRIT(*),INSTAM,INSTAP,TM,TP,TREF
      REAL*8             EPSM(6),DEPS(6),HSR(5,12,12)
      REAL*8             SIGM(6),VIM(9),SIGP(6),VIP(9),DSIDEP(6,6)
C ----------------------------------------------------------------------
C     INTEGRATION DE LA LOI DE COMPORTEMENT VISCO PLASTIQUE DE
C     CHABOCHE AVEC ENDOMAGEMENT
C     METHODE ITERATIVE D'EULER IMPLICITE
C     ELEMENTS ISOPARAMETRIQUES EN PETITES DEFORMATIONS
C
C ----------------------------------------------------------------------
C-- ARGUMENTS
C------------
C
C IN  FAMI    FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
C IN  KPG,KSP NUMERO DU (SOUS)POINT DE GAUSS
C IN  NDIM    : DIMENSION DE L'ESPACE
C IN  TYPMOD  : TYPE DE MODELISATION
C IN  IMATE   : ADRESSE DU MATERIAU CODE
C IN  COMPOR  : COMPORTEMENT : RELCOM ET DEFORM
C IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
C               CRIT(1) = NOMBRE D ITERATIONS MAXI A CONVERGENCE
C                         (ITER_INTE_MAXI == ITMAX)
C               CRIT(2) = TYPE DE JACOBIEN A T+DT
C                         (TYPE_MATR_COMP == MACOMP)
C                         0 = EN VITESSE     > SYMETRIQUE
C                         1 = EN INCREMENTAL > NON-SYMETRIQUE
C               CRIT(3) = VALEUR DE LA TOLERANCE DE CONVERGENCE
C                         (RESI_INTE_RELA == TOLER)
C               CRIT(5) = NOMBRE D'INCREMENTS POUR LE
C                         REDECOUPAGE LOCAL DU PAS DE TEMPS
C                         (RESI_INTE_PAS == 0)
C                         0 = PAS DE REDECOUPAGE
C                         N = NOMBRE DE PALIERS
C               CRIT(6) = TYPE D INTEGRATION LOCALE POUR LA LOI DE
C                         COMPORTEMENT
C                          (RESO_LOCA  == 0)
C                          0 = IMPLICITE
C                          1 = RUNGE_KUTTA_2
C IN  INSTAM  : INSTANT DU CALCUL PRECEDENT
C IN  INSTAP  : INSTANT DU CALCUL
C IN  TM      : TEMPERATURE A L'INSTANT PRECEDENT
C IN  TP      : TEMPERATURE A L'INSTANT DU CALCUL
C IN  EPSM    : DEFORMATIONS A L'INSTANT DU CALCUL PRECEDENT
C IN  DEPS    : INCREMENT DE DEFORMATION
C IN  SIGM    : CONTRAINTES A L'INSTANT DU CALCUL PRECEDENT
C IN  VIM     : VARIABLES INTERNES A L'INSTANT DU CALCUL PRECEDENT
C IN  OPTION  : OPTION DEMANDEE : RIGI_MECA_TANG , FULL_MECA , RAPH_MECA
C OUT SIGP    : CONTRAINTES A L'INSTANT ACTUEL
C OUT VIP     : VARIABLES INTERNES A L'INSTANT ACTUEL
C OUT DSIDEP  : MATRICE TANGENTE
C OUT IRET    : CODE RETOUR DE  L'INTEGRATION DE LA LDC
C                  IRET=0 => PAS DE PROBLEME
C                  IRET=1 => ABSENCE DE CONVERGENCE DANS L'INTEGRATION 
C                            DE LA LOI VISCO PLASTIQUE DE CHABOCHE
C                            AVEC ENDOMAGEMENT
C
C ----------------------------------------------------------------------
C INFO    MATM          (*,1) = CARACTERISTIQUES ELASTIQUES A T-
C                       (*,2) = CARACTERISTIQUES PLASTIQUES A T-
C         MATE          (*,1) = CARACTERISTIQUES ELASTIQUES A T
C                       (*,2) = CARACTERISTIQUES PLASTIQUES A T
C         MATCST        'OUI' SI MATERIAU CST ENTRE T- ET T
C                       'NAP' SI LE PARAMETRE K_D EST UNE NAPPE
C                       'NON' SINON
C         NDT            NB DE COMPOSANTES TOTALES DES TENSEURS
C                                = 6  3D
C                                = 4  AXIS  C_PLAN  D_PLAN
C         NDI            NB DE COMPOSANTES DIRECTES DES TENSEURS
C         NVI            NB DE VARIABLES INTERNES
C         NR             NB EQUATIONS SYSTEME INTEGRE A RESOUDRE
C     ATTENTION LES TENSEURS ET MATRICES SONT RANGES DANS
C     L'ORDRE :  XX YY ZZ XY XZ YZ         RR ZZ TT RZ
C ----------------------------------------------------------------------
      INTEGER      NB, NP, NI, NR, NMAT, UN, NT, IRET1
      REAL*8       ZERO, DAMMAX, DET
      PARAMETER  (NB = 6, NP = 2, NI = 9, NR = 8, NT=3*NB)
      PARAMETER  (NMAT = 90)
      PARAMETER  ( UN   = 1.D0   )
      PARAMETER  ( ZERO = 0.D0   )
      PARAMETER  (DAMMAX = 0.99D0)
C
      LOGICAL       CPLAN, BZ
C
      INTEGER       ITMAX, I, IER, ITER
      INTEGER       NDT, NVI, NRV, NDI, K, L
      INTEGER         NBCOMM(NMAT,3)
C
      REAL*8        TOLER, DELTX, SUMX, DT, SE2
      REAL*8        VIND(NI), MATM(NMAT,2), A(6,6), B(6)
      REAL*8        MATE(NMAT,2), HOOK(6,6), HOOKM(6,6)
      REAL*8        P(NP), BETA(NB), EP(NT), RM, DM
      REAL*8        DSGDE(NB,NB), DSGDB(NB,NB), DSGDP(NB,NP)
      REAL*8        RB(NB), RP(NP), DRBDB(NB,NB), DRBDP(NB,NP)
      REAL*8        DRPDB(NP,NB), DRPDP(NP,NP), DRBDE(NB,NB)
      REAL*8        DRPDE(NP,NB), EPTHM(NB)
      REAL*8        DBETA(NB), DP(NP), DSEDB(NB), DSEDB2(NB,NB), SE
      REAL*8        HYDRD,HYDRF,SECHD,SECHF, PGL(3,3),ANGMAS(3)
C
      CHARACTER*3   MATCST
      CHARACTER*16  LOI, CPMONO(5*NMAT+1)
      CHARACTER*11  METING
      CHARACTER*8   MOD, TYPMA
      CHARACTER*7   ETATF(3)
C
        COMMON /TDIM/   NDT  , NDI
        COMMON /METI/   METING
C ----------------------------------------------------------------------
C
C
C-- 1. INITIALISATIONS :
C----------------------
      ITMAX =  INT(CRIT(1))
      IF ( ITMAX .LE. 0 )ITMAX = -ITMAX
      TOLER =  CRIT(3)
      LOI   =  COMPOR(1)
      MOD   =  TYPMOD(1)
      CPLAN =  TYPMOD(1) .EQ. 'C_PLAN'
      METING = 'IMPLICITE'
      DT = INSTAP - INSTAM
      ETATF(1) = 'ELASTIC'
      ETATF(2) = 'EXPONEN'
      ETATF(3) = 'DAMMAXN'
      CALL R8INIR (NB,0.D0,DSEDB,1)
      HYDRD = 0.D0
      HYDRF = 0.D0
      SECHD = 0.D0
      SECHF = 0.D0
C
C-- 1.1. INCONNUES DU MODELES
C----------------------------
      DO 00111 I = 1,NB
        BETA(I) = ZERO
00111 CONTINUE
C
      DO 00112 I = 1,NP
        P(I) = ZERO
00112 CONTINUE
C
C-- 1.2. RECUPERATION COEF(TEMP(T))) LOI ELASTO-PLASTIQUE A T ET/OU T+DT
C        NB DE CMP DIRECTES/CISAILLEMENT + NB VAR. INTERNES
C-----------------------------------------------------------------------
      CALL LCMATE (FAMI,KPG,KSP,COMPOR,MOD, IMATE, NMAT, TM, TP,
     1               HYDRD, HYDRF, SECHD,SECHF, TYPMA,  BZ, HSR,MATM,
     3               MATE,MATCST,NBCOMM, CPMONO,  ANGMAS, PGL,ITMAX,
     2               TOLER, NDT, NDI, NRV, NVI, VIND )
      IF (NDT.NE.NB.AND.NVI.NE.NI.AND.NRV.NE.NR) GOTO 800
C
C-- 1.3. OPERATEUR DE HOOK
C-------------------------
      CALL LCOPLI('ISOTROPE', MOD, MATE, HOOK)
C
      IF (.NOT.( LOI(1:4) .EQ. 'ELAS'.OR.
     &           LOI(1:9) .EQ. 'VENDOCHAB' )) THEN
            CALL UTMESS('F','NMVPEI_01',
     &           ' COMPORTEMENT INATTENDU : '//LOI)
      ENDIF
C
C-- 1.4. DEFORMATIONS TOTALES, THERMIQUES ET VISCOPLASTIQUES
C-----------------------------------------------------------
C-- VARIABLES D'ETAT DU MODELE A T-
      RM = VIM (NB+2)
      DM = VIM (NB+3)
      IF (DM.EQ.UN) DM=DAMMAX
      DO 00141 I = 1,3
        EP(I)=0.D0
        EPTHM(I)=0.D0
        EP(I)=EP(I)+MATE(3,1)*(TP-TREF)
        EPTHM(I)=EPTHM(I)+MATM(3,1)*(TM-TREF)
        EP(3+I)=0.D0
        EPTHM(3+I)=0.D0
00141 CONTINUE
C
      IF (COMPOR(3).EQ.'PETIT_REAC') THEN
        CALL LCOPLI('ISOTROPE', MOD, MATM, HOOKM)
        CALL R8INIR ( NB*NB,0.D0,A,1)
        CALL R8INIR ( NB,0.D0,B,1)
        CALL LCEQVN ( NB,SIGM,B)
        DO 00142 I = 1,NB
          DO 00142 K = 1,NB
            A(I,K) = A(I,K)+ (UN-DM)*HOOKM(I,K)
00142   CONTINUE
        CALL MGAUSS ( 'NFVP', A , B , NB , NB , 1, DET, IRET1 )
C
        DO 00143 I = 1,NB
          EP(6+I)=0.D0
          EP(6+I) = EP(6+I)+ EPSM(I)- B(I)- EPTHM(I)
00143   CONTINUE
      ELSE
        CALL LCEQVN (NB,VIM(1),EP(7))
      ENDIF
      DO 00144 I = 1,NB
        EP(12+I) = EPSM(I)+DEPS(I)
00144 CONTINUE
C
C-- 2. CALCULS:
C---------------
C              - DES RESIDUS (RB ET RP) ET LEURS DERIVEES
C              - DES VARIABLE D'ETAT
C              - DES CONTRAINTES ET DES DERIVEES
C              - ARCHIVAGE DES VARIABLES
C-----------------------------------------------------------------------
      IF (OPTION(1:9).EQ.'RAPH_MECA'.OR.OPTION(1:9).EQ.'FULL_MECA') THEN
        DO 00200 ITER= 1,ITMAX
C
          CALL NMVECD ( IMATE, MATE, NMAT, MATCST, HOOK, DT, TP,
     &                  P, NP, BETA, NB, EP, RM, DM,
     &                  DSGDE, DSGDB, DSGDP, DRBDE, DRPDE,
     &                  RB, RP, DRBDB, DRBDP, DRPDB, DRPDP, ETATF, IER)
         IF (IER.NE.0) GOTO 803
C
C-- 2.1. RESOLUTION DU SYSTEME
C               DRBDB(NB,NB) DRBDP(NB,NP)   DB(NB)    -RB(NB)
C                                         *        =
C               DRPDB(NP,NB) DRPDP(NP,NP)   DP(NP)    -RP(NP)
C
          CALL NMVESO ( RB, NB, RP, NP, DRBDB, DRBDP, DRPDB, DRPDP,
     &                  DP, DBETA, NR, CPLAN)
C
          IF (CPLAN) THEN
             DEPS(3) = ZERO
             DBETA(3) = ZERO
          ENDIF
C
C-- 2.3. TEST DE CONVERGENCE
C-------------------------
C
          DELTX = ZERO
          SUMX = ZERO
          DO 00210 I=1,NB
            BETA(I)=BETA(I)+DBETA(I)
            DELTX=DELTX+ABS(DBETA(I))
            SUMX=SUMX+ABS(BETA(I))
00210     CONTINUE
          DO 00220 I=1,NP
            P(I)=P(I)+DP(I)
            DELTX=DELTX+ABS(DP(I))
            SUMX=SUMX+ABS(P(I))
00220     CONTINUE
C
          IF (SUMX.GT.TOLER) DELTX=DELTX/SUMX
          IF (DELTX.LT.TOLER) GOTO 00230
00200   CONTINUE
C-- NOMBRE D'ITERATIONS MAXI ATTEINT: ARRET DU PROGRAMME
        GOTO 801
C
00230   CONTINUE
        IF (ETATF(2).EQ.'TANGENT')THEN
          CALL UTMESS('A','NMVEEI',' CONVERGENCE ATTEINTE SUR'//
     &     'APPROXIMATION LINEAIRE TANGENTE DE L''EVOLUTION PLASTIQUE'//
     &     '- RISQUE D''IMPRECISION')
        ENDIF
C
C-- 2.4 ACTUALISATION DES CONTRAINTES ET DES VARIABLES INTERNES
C--------------------------------------------------------------
        CALL LCEQVN ( NB, BETA, SIGP )
        VIP(NB+2) = VIM(NB+2) + DT * P(1)
        CALL LCDVMI (BETA, 0.D0, SE2, DSEDB, DSEDB2, SE)
C
        IF (ETATF(3).EQ.'DAMMAXO')THEN
          CALL UTMESS('A','NMVEEI',' ENDOMMAGEMENT MAXIMAL ATTEINT AU'//
     &                ' COURS DES RESOLUTIONS INTERNES')
          VIP(NB+3) = UN
          VIP(NB+1) = VIM(NB+1) + DT * P(1)/(UN-DAMMAX)
          DO 00240 I= 1, NB
            VIP(I) = VIM(I) + DT * P(1)/(UN-DAMMAX) * DSEDB(I)
00240     CONTINUE
        ELSE
          VIP(NB+3) = VIM(NB+3) + DT * P(2)
          VIP(NB+1) = VIM(NB+1) + DT * P(1)/(UN-VIP(NB+3))
          DO 00241 I= 1, NB
             VIP(I) = VIM(I) + DT * P(1)/(UN-VIP(NB+3)) * DSEDB(I)
00241     CONTINUE
        ENDIF
C
      ENDIF
C-- 3. MISE A JOUR DE L'OPERATEUR TANGENT
C----------------------------------------
      IF (OPTION(1:14).EQ.'RIGI_MECA_TANG'.OR.
     &    OPTION(1:9).EQ.'FULL_MECA') THEN
        IF (OPTION(1:9).EQ.'FULL_MECA') THEN
          IF (ETATF(1).EQ.'ELASTIC') THEN
            CALL LCEQMN(NB, HOOK, DSIDEP)
          ELSE
            IF (TYPMA.EQ.'COHERENT') THEN
              CALL NMVEOT(DRBDB, DRBDP, DRPDB, DRPDP,
     &                    DRBDE, DRPDE, DSGDE, DSGDB, DSGDP,
     &                    NP, NB, NR, DSIDEP)
            ELSE
              GOTO 802
            ENDIF
          ENDIF
        ENDIF
C
C-- RIGIDITE TANGENTE (RIGI_MECA_TANG) -> MATRICE ELASTIQUE
        IF (OPTION(1:14).EQ.'RIGI_MECA_TANG') THEN
          IF (TYPMA.EQ.'COHERENT') THEN
             CALL LCEQMN(NB, HOOK, DSIDEP)
          ELSE
             GOTO 802
          ENDIF
        ENDIF
C
C-- MODIFICATION EN CONTRAINTE PLANES POUR TENIR COMPTE DE
C   SIG3=0 ET DE LA CONSERVATION DE L'ENERGIE
        IF ( MOD(1:6).EQ.'C_PLAN' )THEN
          DO 00310 K=1,NB
            IF (K.EQ.3) GO TO 00310
            DO 00320 L=1,NB
              IF (L.EQ.3) GO TO 00320
              DSIDEP(K,L)=DSIDEP(K,L)
     +          - 1.D0/DSIDEP(3,3)*DSIDEP(K,3)*DSIDEP(3,L)
00320       CONTINUE
00310     CONTINUE
        ENDIF
C
      ENDIF
C
      GOTO 900
C
C-- ERREURS
C
 800  CONTINUE
            CALL UTMESS('F','NMVEEI_01',' ERREUR RECUPERATION '//
     &                  'PARAMETRES MATERIAU')
      GOTO 900
 801  CONTINUE
      IRET = 1
      GOTO 900
 802  CONTINUE
            CALL UTMESS('F','NMVEEI_03',
     &           ' TYPE DE MATRICE DEMANDE NON DISPONIBLE ')
      GOTO 900
 803  CONTINUE
            CALL UTMESS('F','NMVEEI_04',' ERREUR DANS NMVECD ')
      GOTO 900
 900  CONTINUE
      END
