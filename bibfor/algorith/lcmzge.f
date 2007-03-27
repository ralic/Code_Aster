      SUBROUTINE LCMZGE (FAMI,KPG,KSP,NDIM, TYPMOD, IMATE,
     &                   EPSTM,DEPST, VIM,
     &                   OPTION, SIG, VIP,  DSIDPT, PROJ)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 28/03/2007   AUTEUR PELLET J.PELLET 
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
      CHARACTER*8       TYPMOD(*)
      CHARACTER*(*)     FAMI
      CHARACTER*16      OPTION
      INTEGER            NDIM, IMATE,KPG,KSP
      REAL*8             EPSTM(12), DEPST(12), VIM(4)
      REAL*8             SIG(6), VIP(4), DSIDPT(6,6,2)
      REAL*8             PROJ(6,6)

C ----------------------------------------------------------------------
C  LOI DE COMPORTEMENT ENDOMMAGEABLE : MODELE DE MAZARS (EN DELOCALISE)
C     NB. LES PARAMETRES MATERIAUX PEUVENT DEPENDRE DE LA TEMPERATURE,
C      DE L'HYDRATATION OU DU SECHAGE
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
C                 2   -> INDICATEUR D'ENDOMMAGEMENT
C                 3   -> TEMPERATURE MAXIMALE VUE PAR LE MATERIAU
C                 3   -> VALEUR DE EPSEQ (NON lOCAL)
C OUT DSIDEP  : MATRICE TANGENTE
C OUT DSIDPR  : MATRICE TANGENTE DEFO GENERALISEE
C OUT PROJ    : PROJECTEUR DE COUPURE DU TERME DE REGULARISATION
C ----------------------------------------------------------------------


      LOGICAL      RIGI, RESI, ELAS, RELA, PROG, CPLAN
      CHARACTER*1 POUM
      CHARACTER*2 CODRET(6)
      CHARACTER*8 NOMRES(6) , NOMPAR
      INTEGER     NDIMSI, NPERM, NITJAC, TRIJ, ORDREJ
      INTEGER     I,J,K,L,IRET
      REAL*8      E, NU, ALPHA, KDESS, BENDO
      REAL*8      DC, DT, AC, AT, BC, BT, BETA, EPSD0
      REAL*8      EPSM(6), EPSRM(6), DEPS(6), DEPSR(6), EPSPLU(6)
      REAL*8      EPSE(6), EPSER(6), EPSPR(3), EPST(3), EPSP(3)
      REAL*8      EPSR(6), EPS(6), TREPS, EPSEQ, EPSTIL
      REAL*8      SIGEL(6), SIGELP(3), TRSIG
      REAL*8      TEMP, TMAX, TMAXM, HYDR, SECH, SREF,TREF
      REAL*8      TOL, TOLDYN, TR(6), TU(6), TRR(6), JACAUX(3)
      REAL*8      VECPE(3,3), VECPER(3,3)
      REAL*8      COPLAN,  LAMBDA, DEUXMU, RTEMPC, RTEMPT, ALPHAT
      REAL*8      RAC2, COEF, TMP1, D
      REAL*8      VALRES(6), VALPAR
      REAL*8      KRON(6)
      DATA        KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/

C ======================================================================
C                            INITIALISATION
C ======================================================================

C -- OPTION ET MODELISATION
      RIGI  = (OPTION(1:4).EQ.'RIGI' .OR. OPTION(1:4).EQ.'FULL')
      RESI  = (OPTION(1:4).EQ.'RAPH' .OR. OPTION(1:4).EQ.'FULL')
      RELA  = OPTION(11:14) .EQ. 'ELAS'
      CPLAN = (TYPMOD(1).EQ.'C_PLAN  ')
      PROG  = .FALSE.
      ELAS  = .TRUE.
      NDIMSI = 2*NDIM
      RAC2=SQRT(2.D0)

C -- PROJECTEUR DE COUPURE
      CALL R8INIR(36,0.D0,PROJ,1)
      IF (VIM(1) .LT. 1.D0-1.D-05) CALL R8INIR(6,1.D0,PROJ,7)

C     DETERMINATION DE LA TEMPERATURE DE REFERENCE (TMAX) ET
C   DES CONDITIONS D HYDRATATION OU DE SECHAGE
      TMAXM = VIM(3)
      CALL RCVARC('F','TEMP','REF',FAMI,KPG,KSP,TREF,IRET)
      CALL RCVARC(' ','SECH','REF',FAMI,KPG,KSP,SREF,IRET)
      IF (IRET.NE.0) SREF=0.D0
      IF (RESI) THEN
        POUM='+'
        CALL RCVARC('F','TEMP',POUM,FAMI,KPG,KSP,TEMP,IRET)
        TMAX = MAX(TMAXM, TEMP)
        IF (TMAX.GT.TMAXM) VIP(3) = TMAX
      ELSE
        POUM='-'
        CALL RCVARC('F','TEMP',POUM,FAMI,KPG,KSP,TEMP,IRET)
        TMAX = TMAXM
      ENDIF
      CALL RCVARC(' ','HYDR',POUM,FAMI,KPG,KSP,HYDR,IRET)
      IF (IRET.NE.0) HYDR=0.D0
      CALL RCVARC(' ','SECH',POUM,FAMI,KPG,KSP,SECH,IRET)
      IF (IRET.NE.0) SECH=0.D0

C  RECUPERATION DES CARACTERISTIQUES MATERIAUX QUI PEUVENT VARIER
C  AVEC LA TEMPERATURE (MAXIMALE), L'HYDRATATION OU LE SECHAGE
C-----------------------------------------------------

      NOMPAR = 'TEMP'
      VALPAR = TMAX

C    LECTURE DES CARACTERISTIQUES ELASTIQUES

      NOMRES(1) = 'E'
      NOMRES(2) = 'NU'
      NOMRES(3) = 'ALPHA'
      CALL RCVALB(FAMI,KPG,KSP,POUM,IMATE,' ','ELAS',1,NOMPAR,VALPAR,2,
     &              NOMRES,VALRES,CODRET, 'FM')
      CALL RCVALB(FAMI,KPG,KSP,POUM,IMATE,' ','ELAS',1,NOMPAR,VALPAR,1,
     &              NOMRES(3),VALRES(3),CODRET(3), ' ')
      IF ( CODRET(3) .NE. 'OK' ) VALRES(3) = 0.D0
      E     = VALRES(1)
      NU    = VALRES(2)
      ALPHA = VALRES(3)
      LAMBDA = E * NU / (1.D0+NU) / (1.D0 - 2.D0*NU)
      DEUXMU = E/(1.D0+NU)

C --- LECTURE DU RETRAIT ENDOGENE ET RETRAIT DE DESSICCATION

      NOMRES(1)='B_ENDOGE'
      NOMRES(2)='K_DESSIC'
      CALL RCVALA(IMATE,' ','ELAS',0,' ',0.D0,1,
     +            NOMRES(1),VALRES(1),CODRET(1), ' ' )
      IF ( CODRET(1) .NE. 'OK' ) VALRES(1) = 0.D0
      BENDO = VALRES(1)
      CALL RCVALA(IMATE,' ','ELAS',0,' ',0.D0,1,
     +            NOMRES(2),VALRES(2),CODRET(2), ' ' )
      IF ( CODRET(2) .NE. 'OK' ) VALRES(2) = 0.D0
      KDESS = VALRES(2)

C --- LECTURE DES CARACTERISTIQUES D'ENDOMMAGEMENT
       NOMRES(1) = 'EPSD0'
       NOMRES(2) = 'BETA'
       NOMRES(3) = 'AC'
       NOMRES(4) = 'BC'
       NOMRES(5) = 'AT'
       NOMRES(6) = 'BT'

       CALL RCVALB(FAMI,KPG,KSP,POUM,IMATE,' ','MAZARS',1,NOMPAR,
     &            VALPAR,6,NOMRES,VALRES,CODRET,'FM')
      EPSD0 = VALRES(1)
      BETA  = VALRES(2)
      AC    = VALRES(3)
      BC    = VALRES(4)
      AT    = VALRES(5)
      BT    = VALRES(6)

C -- SEPARATION DE EPSM/EPSRM, DEPS/DEPSR DANS EPSTM, DEPST

      DO 10 I=1,NDIMSI
        EPSM(I)=EPSTM(I)
        EPSRM(I)=EPSTM(I+6)
        DEPS(I)=DEPST(I)
        DEPSR(I)=DEPST(I+6)
10    CONTINUE

C ======================================================================
C    CALCUL DES CONTRAINTES ET VARIABLES INTERNES
C    (OPTION FULL_MECA ET RAPH_MECA)
C ======================================================================

      CALL R8INIR(6, 0.D0, EPS,1)
      CALL R8INIR(6, 0.D0, EPSR, 1)

C  -   MISE A JOUR DES DEFORMATIONS MECANIQUES
      IF (RESI) THEN
        DO  20 K = 1, NDIMSI
          EPS(K) = EPSM(K) + DEPS(K)
          EPSR(K) = EPSRM(K) + DEPSR(K)
20      CONTINUE
      ELSE
        DO  30 K=1,NDIMSI
          EPS(K)=EPSM(K)
          EPSR(K)=EPSRM(K)
30      CONTINUE
        D=VIM(1)
      ENDIF
C  ON MET DANS EPS LES DEFORMATIONS REELES
      DO  40 K=4,NDIMSI
        EPS(K) = EPS(K)/RAC2
        EPSR(K)=EPSR(K)/RAC2
40    CONTINUE

C    CALCUL DE LA DEFORMATION ELASTIQUE (LA SEULE QUI CONTRIBUE
C    A FAIRE EVOLUER L'ENDOMMAGEMENT)

      CALL R8INIR(6, 0.D0, EPSE,1)
      CALL R8INIR(6, 0.D0, EPSER,1)
      DO 35 K=1,NDIMSI
        EPSE(K) = EPS(K) - (   ALPHA * (TEMP - TREF)
     &                      - KDESS * (SREF-SECH)
     &                      - BENDO *  HYDR         ) * KRON(K)
        EPSER(K) = EPSR(K) - (   ALPHA * (TEMP - TREF)
     &                      - KDESS * (SREF-SECH)
     &                      - BENDO *  HYDR         ) * KRON(K)
35    CONTINUE

C  -   ON PASSE DANS LE REPERE PROPRE DE EPS
      NPERM  = 12
      TOL    = 1.D-10
      TOLDYN = 1.D-2
C       MATRICE  TR = (XX XY XZ YY YZ ZZ) POUR JACOBI)
      TR(1) = EPSE(1)
      TR(2) = EPSE(4)
      TR(3) = EPSE(5)
      TR(4) = EPSE(2)
      TR(5) = EPSE(6)
      TR(6) = EPSE(3)

C       MATRICE UNITE = (1 0 0 1 0 1) (POUR JACOBI)
      TU(1) = 1.D0
      TU(2) = 0.D0
      TU(3) = 0.D0
      TU(4) = 1.D0
      TU(5) = 0.D0
      TU(6) = 1.D0
      TRIJ   = 2
      ORDREJ = 2
C
      CALL JACOBI(3,NPERM,TOL,TOLDYN,TR,TU,VECPE,EPSP,JACAUX,
     &       NITJAC,TRIJ,ORDREJ)

C ON PASSE DANS LE REPERE PROPRE DE EPSR

      TU(1) = 1.D0
      TU(2) = 0.D0
      TU(3) = 0.D0
      TU(4) = 1.D0
      TU(5) = 0.D0
      TU(6) = 1.D0
      TRIJ   = 2
      ORDREJ = 2

      TRR(1) = EPSER(1)
      TRR(2) = EPSER(4)
      TRR(3) = EPSER(5)
      TRR(4) = EPSER(2)
      TRR(5) = EPSER(6)
      TRR(6) = EPSER(3)

      CALL JACOBI(3,NPERM,TOL,TOLDYN,TRR,TU,VECPER,EPSPR,JACAUX,
     &       NITJAC,TRIJ,ORDREJ)

C -- SI CONTRAINTES PLANES

      IF (CPLAN) THEN
        COPLAN  = - NU/(1.D0-NU)
        EPSP(3)  = COPLAN * (EPS(1)+EPS(2))
        EPSPR(3)  = COPLAN * (EPSR(1)+EPSR(2))
      END IF


C--  ------------------------------
C      CALCUL DE L'ETAT D'ENDOMMAGEMENT
C -  ------------------------------
C     CALCUL DE EPSEQ (NON LOCAL) ET EPSTIL (LOCAL)
      EPSEQ = 0.D0
      EPSTIL = 0.D0
      DO 50 K = 1,3
        IF (EPSPR(K).GT.0.D0) THEN
          EPSEQ = EPSEQ + (EPSPR(K)**2)
        END IF
      IF (EPSP(K).GT.0.D0) THEN
        EPSTIL = EPSTIL + (EPSP(K)**2)
      END IF
50    CONTINUE
      EPSEQ = SQRT(EPSEQ)
      EPSTIL = SQRT(EPSTIL)

C -     CALCUL DES CONTRAINTES ELASTIQUES (REPËRE PRINCIPAL)
      TREPS = EPSP(1)+EPSP(2)+EPSP(3)
      DO 60  K=1,3
        SIGELP(K) = LAMBDA*TREPS
60    CONTINUE
      DO  70 K=1,3
        SIGELP(K) = SIGELP(K) + DEUXMU*EPSP(K)
70    CONTINUE
      TRSIG = SIGELP(1) + SIGELP(2) + SIGELP(3)
      TMP1 = 0.D0
      DO 80 K = 1,3
        IF (SIGELP(K).LT.0.D0) THEN
          TMP1 = TMP1 + SIGELP(K)
        END IF
80    CONTINUE
      IF (RESI) THEN
C      CALCUL DES PARAMETRES D'ENDOMMAGEMENT
        IF (EPSEQ.LE.EPSD0) THEN
          D = VIM(1)
        ELSE
          IF (EPSTIL.LE.1.D-15) THEN
            EPSTIL=1.D-15
          END IF
          ALPHAT = 0.D0
          DO 90 K = 1,3
            EPST(K) = ( MAX(0.D0, SIGELP(K))* (1.D0 + NU)
     &                  - NU *( TRSIG - TMP1) )/E
            ALPHAT = ALPHAT + (MAX(0.D0, EPSP(K))*EPST(K) )
90        CONTINUE
          ALPHAT = ALPHAT / EPSTIL**2
          RTEMPT = BT * (EPSEQ - EPSD0)
          RTEMPC = BC * (EPSEQ - EPSD0)
          RTEMPT = MIN(RTEMPT,700.D0)
          RTEMPC = MIN(RTEMPC,700.D0)
          DT = 1.D0 - (EPSD0*(1.D0 - AT )/EPSEQ ) -
     &                ( AT / (EXP(RTEMPT  )))
          DC = 1.D0 - (EPSD0*(1.D0 - AC )/EPSEQ ) -
     &                ( AC / (EXP(RTEMPC) ) )
          IF (DC.LT.0.D0) DC=0.D0
          IF (DC.GT.1.D0) DC=1.D0
          IF (DT.LT.0.D0) DT=0.D0
          IF (DT.GT.1.D0) DT=1.D0
          IF (ALPHAT.LT.1.D0.AND.ALPHAT.GT.0.D0) THEN
            D = ALPHAT**BETA * DT + (1.D0 - ALPHAT)**BETA * DC
          ELSE IF (ALPHAT.GE.1.D0) THEN
            D = DT
          ELSE
            D = DC
          END IF
          D = MIN (D, 1.D0-1.D-05)
          D = MAX ( VIM(1), D)
          IF (D.GT.VIM(1)) PROG = .TRUE.
          IF (D.GT.0.D0)   ELAS = .FALSE.
        END IF
      END IF

C       MISE A JOUR DES CONTRAINTES ET VARIABLES D'ENDOMMAGEMENT
      IF (RESI) THEN

C        ON PASSE DANS LE REPERE INITIAL LES CONTRAINTES REELLES
        CALL R8INIR(6, 0.D0, SIG,1)
        TR(1) = SIGELP(1)*(1.D0-D)
        TR(2) = SIGELP(2)*(1.D0-D)
        TR(3) = SIGELP(3)*(1.D0-D)
        TR(4) = 0.D0
        TR(5) = 0.D0
        TR(6) = 0.D0
        CALL BPTOBG(TR,SIG,VECPE)
        DO  100 K=4,NDIMSI
          SIG(K)=RAC2*SIG(K)
100     CONTINUE
        VIP(1) = D
        IF (D.EQ.0.D0) THEN
          VIP(2) = 0.D0
        ELSE
          VIP(2) = 1.D0
        END IF
          VIP(4) = EPSEQ
      END IF
C ======================================================================
C     CALCUL  DE LA MATRICE TANGENTE DSIDEP
C         OPTION RIGI_MECA_TANG ET FULL_MECA
C ======================================================================

C ======================================================================
C                            MATRICE TANGENTE
C ======================================================================


      IF (RIGI) THEN

C -- CONTRIBUTION ELASTIQUE

        CALL R8INIR(72, 0.D0, DSIDPT, 1)
        DO 110 K = 1,3
          DO 120 L = 1,3
            DSIDPT(K,L,1) = (1-D)*LAMBDA
120       CONTINUE
110     CONTINUE
        DO 130 K = 1,NDIMSI
          DSIDPT(K,K,1) = DSIDPT(K,K,1) + (1-D)*DEUXMU
130     CONTINUE

C -- CORRECTION DUES AUX CONTRAINTES PLANES

        IF (CPLAN) THEN
          DO 140 K=1,NDIMSI
            IF (K.EQ.3) GO TO 140
            DO 150 L=1,NDIMSI
              IF (L.EQ.3) GO TO 150
              DSIDPT(K,L,1)=DSIDPT(K,L,1)
     &        - 1.D0/DSIDPT(3,3,1)*DSIDPT(K,3,1)*DSIDPT(3,L,1)
150         CONTINUE
140       CONTINUE
        ENDIF


C -- CONTRIBUTION DISSIPATIVE

C      CONTRIBUTION DUE A  L'ENDOMMAGEMENT
        IF ((.NOT.ELAS).AND.(PROG).AND.(.NOT.RELA)) THEN
          RTEMPT = BT * (EPSEQ - EPSD0)
          RTEMPC = BC * (EPSEQ - EPSD0)
          RTEMPT = MIN(RTEMPT,700.D0)
          RTEMPC = MIN(RTEMPC,700.D0)
          IF (ALPHAT.LT.1.D0.AND.ALPHAT.GT.0.D0) THEN
            COEF =(EPSD0*(1.D0 - AT)/EPSEQ**2 +
     &             AT*BT/ EXP (RTEMPT))*(ALPHAT**BETA)+
     &            (EPSD0*(1.D0 - AC)/EPSEQ**2 +
     &        AC*BC/ EXP (RTEMPC))* ((1.D0-ALPHAT)**BETA)
          ELSEIF (ALPHAT.GE.1.D0) THEN
            COEF =(EPSD0*(1.D0 - AT)/EPSEQ**2 +
     &             AT*BT/ EXP (RTEMPT) )
          ELSE
            COEF= (EPSD0*(1.D0 - AC)/EPSEQ**2 +
     &         AC*BC/ EXP (RTEMPC))
          ENDIF
          COEF = COEF/EPSEQ

C      CALCUL DE EPS+
C
          CALL R8INIR(6, 0.D0, TR,1)
          DO 160 K = 1,3
            IF (EPSPR(K).GT.0.D0) THEN
              TR(K) = EPSPR(K)
            END IF
160       CONTINUE
          CALL BPTOBG(TR,EPSPLU,VECPER)
          DO  170 K=4,NDIMSI
            EPSPLU(K) = EPSPLU(K)*RAC2
170       CONTINUE
          CALL R8INIR(6, 0.D0, SIGEL,1)
          TR(1) = SIGELP(1)
          TR(2) = SIGELP(2)
          TR(3) = SIGELP(3)
          TR(4) = 0.D0
          TR(5) = 0.D0
          TR(6) = 0.D0
          CALL BPTOBG(TR,SIGEL,VECPE)
          DO  180 K=4,NDIMSI
            SIGEL(K)=RAC2*SIGEL(K)
180       CONTINUE
          DO  190 I=1,6
            DO 200 J=1,6
              DSIDPT(I,J,2) = - COEF * SIGEL(I)* EPSPLU(J)
200         CONTINUE
190       CONTINUE
        ENDIF

      END IF
      END
