      SUBROUTINE LCMAZA (FAMI,KPG,KSP,NDIM, TYPMOD, IMATE,COMPOR,EPSM,
     &                   DEPS, VIM, TM,TP,TREF,
     &                   OPTION, SIG, VIP,  DSIDEP)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 01/12/2008   AUTEUR COURTOIS M.COURTOIS 
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
C TOLE CRP_20
      IMPLICIT NONE
      CHARACTER*8       TYPMOD(2)
      CHARACTER*16      COMPOR(*),OPTION
      CHARACTER*(*)     FAMI
      INTEGER           NDIM, IMATE, KPG, KSP
      REAL*8            EPSM(6), DEPS(6), VIM(4), TM, TP, TREF
      REAL*8            SIG(6), VIP(*), DSIDEP(6,6)
C ----------------------------------------------------------------------
C     LOI DE COMPORTEMENT ENDOMMAGEABLE : MODELE DE MAZARS
C     POUR MAZARS  OU MAZARS_FO COMBINABLE AVEC ELAS OU ELAS_FO
C     NB. LES PARAMETRES MATERIAUX PEUVENT DEPENDRE DE LA TEMPERATURE,
C      DE L'HYDRATATION OU DU SECHAGE
C
C IN  NDIM    : DIMENSION DE L'ESPACE
C IN  TYPMOD  : TYPE DE MODELISATION
C IN  IMATE   : NATURE DU MATERIAU
C IN  EPSM    : DEFORMATION EN T-
C IN  DEPS    : INCREMENT DE DEFORMATION
C IN  VIM     : VARIABLES INTERNES EN T-
C IN  TM      : TEMPERATURE EN T-
C IN  TP      : TEMPERATURE EN T+
C IN  TREF    : TEMPERATURE DE REFERENCE
C IN  OPTION  : OPTION DEMANDEE
C                 RIGI_MECA_TANG ->     DSIDEP
C                 FULL_MECA      -> SIG DSIDEP VIP
C                 RAPH_MECA      -> SIG        VIP
C
C MODIFIE 2008 M. BOTTONI
C ON AJOUTE OPTION  RAPH_COUP POUR TRAITER LE COUPLAGE AVEC UMLV
C IN OPTION  : OPTION DEMANDEE
C                 RIGI_COUP      ->     DSIDEP
C                 RAPH_COUP      -> SIG        VIP
C
C OUT SIG     : CONTRAINTE
C OUT VIP     : VARIABLES INTERNES
C                 1   -> VALEUR DE L'ENDOMMAGEMENT
C                 2   -> INDICATEUR D'ENDOMMAGEMENT
C                 3   -> TEMPERATURE MAXIMALE ATTEINTE PAR LE MATERIAU
C                 4   -> VALEUR DE EPSEQ (UTILE POUR POSTTRAITER)
C OUT DSIDEP  : MATRICE TANGENTE
C ON A BESOIN DE
C         EPSD0 = DEFORMATION SEUIL  [REEL OU FCT]
C         BETA = COEF CORRECTEUR POUR CISAILLEMENT (1. OU 1.06)[REEL]
C         AT = CONSTANTE DE TRACTION     (0.7 A 1)[REEL OU FCT]
C         AC = CONSTANTE DE COMPRESSION (1 A 1.5)[REEL OU FCT]
C         BT = CONSTANTE DE TRACTION    (10 000 A 100 000)[REEL OU FCT]
C         BC = CONSTANTE DE COMPRESSION (1000 A 2000)[REEL OU FCT]
C ----------------------------------------------------------------------
      LOGICAL     RIGI, RESI, PROG, ELAS, CPLAN, COUP
      CHARACTER*1  POUM
      CHARACTER*2 CODRET(7)
      CHARACTER*8 NOMRES(7), NOMPAR
      INTEGER     NDIMSI, NPERM, NITJAC, TRIJ, ORDREJ
      INTEGER     I,J,K,L,IRET, IISNAN
      REAL*8      E, NU, EPSTHE, KDESS, BENDO,R8NNEM
      REAL*8      DC, DT, AC, AT, BC, BT, BETA, EPSD0
      REAL*8      EPS(6), EPSE(6), EPSPLU(6), EPSEP(3), EPST(3), EPSEQ
      REAL*8      SIGEL(6), SIGELP(3), TRSIG
      REAL*8      TEMP, TMAX, TMAXM, HYDR, SECH , SREF
      REAL*8      TOL, TOLDYN, TR(6), TU(6), JACAUX(3), VECPE(3,3)
      REAL*8      RAC2, LAMBDA, DEUXMU, ALPHAT, COEF, RTEMPC, RTEMPT
      REAL*8      VALRES(7), VALPAR, COPLAN, D, TMP1
      REAL*8      KRON(6)
      REAL*8      EPSFP(6), EPSCOU(6), EPSI(6), CHI
      REAL*8      EPSEQC, EPSEND, EPSEPC(3), VECPEC(3,3)
      INTEGER     IDC
      DATA        KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/

C ======================================================================
C                            INITIALISATION
C ======================================================================


C -- OPTION ET MODELISATION
      IF ((.NOT.( COMPOR(1)(1:6) .EQ. 'MAZARS')).AND.
     &   (.NOT.( COMPOR(1)(1:6) .EQ. 'KIT_HM')).AND.
     &   (.NOT.( COMPOR(1)(1:7) .EQ. 'KIT_HHM')).AND.
     &   (.NOT.( COMPOR(1)(1:7) .EQ. 'KIT_THM')).AND.
     &   (.NOT.( COMPOR(1)(1:7) .EQ. 'KIT_DDI')).AND.
     &   (.NOT.( COMPOR(1)(1:8) .EQ. 'KIT_THHM'))) THEN
        CALL U2MESK('F','ALGORITH4_50',1,COMPOR(1))
      ENDIF

      RIGI  = (OPTION(1:4).EQ.'RIGI' .OR. OPTION(1:4).EQ.'FULL')
      RESI  = (OPTION(1:4).EQ.'RAPH' .OR. OPTION(1:4).EQ.'FULL')

C M.B.: NOUVELLE OPTION COUP POUR LE COUPLAGE AVEC UMLV
C MEME OPTION UTILISEE LE COUPLAGE UMLV-ENDO_ISOT_BETON

      COUP  = (OPTION(6:9).EQ.'COUP')
      CPLAN = (TYPMOD(1).EQ.'C_PLAN  ')
      PROG  = .FALSE.
      ELAS  = .TRUE.
      NDIMSI = 2*NDIM
      RAC2=SQRT(2.D0)


C M.B.: INDICE POUR IDENTIFIER LES VARIABLES INTERNES DANS LES CAS:
C COUPLAGE ET ABSENCE DE COUPLAGE AVEC UMLV

      IDC = 0
      IF (COUP) THEN
        IDC = 21
      ENDIF

C   DETERMINATION DE LA TEMPERATURE DE REFERENCE (TMAX) ET
C   DES CONDITIONS D HYDRATATION OU DE SECHAGE
      TMAXM = VIM(3)
      CALL RCVARC(' ','SECH','REF',FAMI,KPG,KSP,SREF,IRET)
      IF ( IRET.NE.0) SREF=0.D0
      IF (RESI) THEN
        TEMP = TP
        CALL RCVARC(' ','HYDR','+',FAMI,KPG,KSP,HYDR,IRET)
        IF ( IRET.NE.0) HYDR=0.D0
        POUM='+'
        CALL RCVARC(' ','SECH','+',FAMI,KPG,KSP,SECH,IRET)
        IF ( IRET.NE.0) SECH=0.D0
        IF (IISNAN(TP).GT.0) THEN
          TMAX = R8NNEM()
          VIP(IDC+3) = R8NNEM()
        ELSE
          TMAX = MAX(TMAXM, TP)
          IF (TMAX.GT.TMAXM) VIP(IDC+3) = TMAX
        ENDIF
      ELSE
        TEMP = TM
        CALL RCVARC(' ','HYDR','-',FAMI,KPG,KSP,HYDR,IRET)
        IF ( IRET.NE.0) HYDR=0.D0
        CALL RCVARC(' ','SECH','-',FAMI,KPG,KSP,SECH,IRET)
        IF ( IRET.NE.0) SECH=0.D0
        POUM='-'
        TMAX = TMAXM
      ENDIF

C  RECUPERATION DES CARACTERISTIQUES MATERIAUX QUI PEUVENT VARIER
C  AVEC LA TEMPERATURE (MAXIMALE), L'HYDRATATION OU LE SECHAGE
C-----------------------------------------------------

      NOMPAR = 'TEMP'
      VALPAR = TMAX

C    LECTURE DES CARACTERISTIQUES ELASTIQUES

      NOMRES(1) = 'E'
      NOMRES(2) = 'NU'
      NOMRES(3) = 'ALPHA'

      CALL RCVALB(FAMI,KPG,KSP,POUM,IMATE,' ','ELAS',1,
     &            NOMPAR,VALPAR,2,NOMRES,VALRES,CODRET,'FM')
      CALL RCVALB(FAMI,KPG,KSP,POUM,IMATE,' ','ELAS',1,NOMPAR,
     &            VALPAR,1,NOMRES(3),VALRES(3),CODRET(3),' ')
      IF ((IISNAN(TP).EQ.0).AND.(IISNAN(TM).EQ.0)) THEN
        IF ((IISNAN(TREF).NE.0).OR.(CODRET(3).NE.'OK')) THEN
          CALL U2MESS('F','CALCULEL_15')
        ELSE
          EPSTHE = VALRES(3)*(TEMP-TREF)
        ENDIF
      ELSE
        VALRES(3) = 0.D0
        EPSTHE = 0.D0
      ENDIF

      E     = VALRES(1)
      NU    = VALRES(2)
      LAMBDA = E * NU / (1.D0+NU) / (1.D0 - 2.D0*NU)
      DEUXMU = E/(1.D0+NU)


C --- LECTURE DU RETRAIT ENDOGENE ET RETRAIT DE DESSICCATION
C     SAUF EN CAS DE COUPLAGE
      IF (COMPOR(1)(1:6) .EQ. 'MAZARS')      THEN
        NOMRES(1)='B_ENDOGE'
        NOMRES(2)='K_DESSIC'
        CALL RCVALA(IMATE,' ','ELAS',0,' ',0.D0,1,
     &            NOMRES(1),VALRES(1),CODRET(1), ' ' )
        IF ( CODRET(1) .NE. 'OK' ) VALRES(1) = 0.D0
        BENDO = VALRES(1)
        CALL RCVALA(IMATE,' ','ELAS',0,' ',0.D0,1,
     &            NOMRES(2),VALRES(2),CODRET(2), ' ' )
        IF ( CODRET(2) .NE. 'OK' ) VALRES(2) = 0.D0
        KDESS = VALRES(2)
      ELSE
        BENDO = 0.D0
        KDESS = 0.D0
      ENDIF

C    LECTURE DES CARACTERISTIQUES D'ENDOMMAGEMENT

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

C    M.B.: LECTURE DU PARAMETRE DE COUPLAGE AVEC UMLV

      IF (COUP) THEN
        NOMRES(7) = 'CHI'
        CALL RCVALB(FAMI,KPG,KSP,POUM,IMATE,' ','MAZARS',0,' ',
     &              0.D0,1,NOMRES(7),VALRES(7),CODRET(7),'FM')
        CHI   = VALRES(7)
        IF (CHI .EQ. 0.D0) THEN
          CALL U2MESS('I','COMPOR1_59')
        ENDIF
      ENDIF




C ======================================================================
C       CALCUL DES GRANDEURS UTILES QUELQUE SOIT OPTION
C
C ======================================================================

C    1 - CALCUL DES DEFORMATIONS MECANIQUES ET THERMIQUES
C--------------------------------------------------------

C  -  MISE A JOUR DE LA DEFORMATION TOTALE

      CALL R8INIR(6, 0.D0, EPS,1)
      IF (RESI) THEN
        DO  10 K = 1, NDIMSI
          EPS(K) = EPSM(K) + DEPS(K)
10      CONTINUE
      ELSE
        DO 20 K=1,NDIMSI
          EPS(K)=EPSM(K)
20      CONTINUE
        D=VIM(1)
      ENDIF

C      DO  30 K=4,NDIMSI
C        EPS(K) = EPS(K)/RAC2
C30    CONTINUE

C    CALCUL DE LA DEFORMATION ELASTIQUE (LA SEULE QUI CONTRIBUE
C    A FAIRE EVOLUER L'ENDOMMAGEMENT)

      CALL R8INIR(6, 0.D0, EPSE,1)
      DO 35 K=1,NDIMSI
        EPSE(K) = EPS(K) - (   EPSTHE
     &                      - KDESS * (SREF-SECH)
     &                      - BENDO *  HYDR         ) * KRON(K)
35    CONTINUE

      IF (CPLAN) THEN
        COPLAN  = - NU/(1.D0-NU)
        EPSE(3)  = COPLAN * (EPSE(1)+EPSE(2))
      END IF

C    M.B.: AVEC COUPLAGE, EPSF POUR L INSTANT
C    SERT SEULEMENT AVEC  RESI A L INSTANT P, CAR LA MATRICE TANGENTE
C    N EST PAS ENCORE IMPLEMENTEE

      IF (COUP .AND. RESI) THEN
        CALL LCUMVI('FT',VIP,EPSFP)
        CALL R8INIR(6, 0.D0, EPSCOU,1)
        DO 1010  K=1,NDIMSI
          EPSI(K) = EPSE(K)
          EPSE(K) = EPSI(K) - EPSFP(K)
          EPSCOU(K) = EPSI(K) - (1-CHI)*EPSFP(K)
1010    CONTINUE
      ENDIF

      DO  30 K=4,NDIMSI
        EPSE(K) = EPSE(K)/RAC2
30    CONTINUE
      IF (COUP .AND. RESI) THEN
        DO  31 K=4,NDIMSI
          EPSCOU(K) = EPSCOU(K)/RAC2
31      CONTINUE
      ENDIF


C    2 - CALCUL DE EPSEQ = SQRT(TR (<EPSE>+ * <EPSE>+)  )
C        C EST EPSEQ ELASTIQUE DANS LE CAS DU COUPLAGE
C--------------------------------------------------------
C  -   ON PASSE DANS LE REPERE PROPRE DE EPS

      NPERM  = 12
      TOL    = 1.D-10
      TOLDYN = 1.D-2
C     MATRICE  TR = (XX XY XZ YY YZ ZZ) POUR JACOBI)
      TR(1) = EPSE(1)
      TR(2) = EPSE(4)
      TR(3) = EPSE(5)
      TR(4) = EPSE(2)
      TR(5) = EPSE(6)
      TR(6) = EPSE(3)
C     MATRICE UNITE = (1 0 0 1 0 1) (POUR JACOBI)
      TU(1) = 1.D0
      TU(2) = 0.D0
      TU(3) = 0.D0
      TU(4) = 1.D0
      TU(5) = 0.D0
      TU(6) = 1.D0
      TRIJ   = 2
      ORDREJ = 2
C
      CALL JACOBI(3,NPERM,TOL,TOLDYN,TR,TU,VECPE,EPSEP,JACAUX,
     &            NITJAC,TRIJ,ORDREJ)


      EPSEQ = 0.D0
      DO 40 K = 1,3
        IF (EPSEP(K).GT.0.D0) THEN
          EPSEQ = EPSEQ + (EPSEP(K)**2)
        END IF
40    CONTINUE
      EPSEQ = SQRT(EPSEQ)



C    2BIS - CALCUL DE EPSEQC = SQRT(TR (<EPSCOU>+ * <EPSCOU>+))
C        M.B.: C EST LA DEFORMATION EQUIVALENT DANS LE CAS DU COUPLAGE
C--------------------------------------------------------
C  -   ON PASSE DANS LE REPERE PROPRE DE EPSCOU
C     MATRICE  TR = (XX XY XZ YY YZ ZZ) POUR JACOBI)
C     A CONTROLER SI LES QUANTITES SUIVANTES SERVENT AUSSI POUR LA
C      MATRICE TANGENTE  (RIGI)!

      IF (COUP .AND. RESI) THEN

        TR(1) = EPSCOU(1)
        TR(2) = EPSCOU(4)
        TR(3) = EPSCOU(5)
        TR(4) = EPSCOU(2)
        TR(5) = EPSCOU(6)
        TR(6) = EPSCOU(3)
C      MATRICE UNITE = (1 0 0 1 0 1) (POUR JACOBI)
        TU(1) = 1.D0
        TU(2) = 0.D0
        TU(3) = 0.D0
        TU(4) = 1.D0
        TU(5) = 0.D0
        TU(6) = 1.D0

        CALL JACOBI(3,NPERM,TOL,TOLDYN,TR,TU,VECPEC,EPSEPC,JACAUX,
     &              NITJAC,TRIJ,ORDREJ)

        EPSEQC = 0.D0
        DO 1040 K = 1,3
          IF (EPSEPC(K).GT.0.D0) THEN
            EPSEQC = EPSEQC + (EPSEPC(K)**2)
          END IF
1040    CONTINUE
        EPSEQC = SQRT(EPSEQC)

      ENDIF


C -  3     CALCUL DE <EPS>+
C ------------------------------------------------------

      CALL R8INIR(6, 0.D0, TR,1)
      CALL R8INIR(6, 0.D0, EPSPLU,1)
      DO 42 K = 1,3
        IF (EPSEP(K).GT.0.D0) THEN
          TR(K) = EPSEP(K)
        END IF
42    CONTINUE
      CALL BPTOBG(TR,EPSPLU,VECPE)
      DO  44 K=4,NDIMSI
        EPSPLU(K) = EPSPLU(K)*RAC2
44    CONTINUE


C   4 -  CALCUL DES CONTRAINTES ELASTIQUES (REPERE PRINCIPAL)
C----------------------------------------------------------------
      DO  50 K=1,3
        SIGELP(K) = LAMBDA*(EPSEP(1)+EPSEP(2)+EPSEP(3))
50    CONTINUE
      DO  60 K=1,3
        SIGELP(K) = SIGELP(K) + DEUXMU*EPSEP(K)
60    CONTINUE
      TRSIG = SIGELP(1) + SIGELP(2) + SIGELP(3)
      TMP1 = 0.D0
      DO 70 K = 1,3
        IF (SIGELP(K).LT.0.D0) THEN
          TMP1 = TMP1 + SIGELP(K)
        END IF
70    CONTINUE


C   5 -     CALCUL DE ALPHAT
C----------------------------------------------------------------
      ALPHAT = 0.D0
      DO 80 K = 1,3
        EPST(K) = ( MAX(0.D0, SIGELP(K))* (1.D0 + NU)
     &            - NU *( TRSIG - TMP1) ) / E
        ALPHAT = ALPHAT + (MAX(0.D0, EPSEP(K))*EPST(K) )
80    CONTINUE
      IF (EPSEQ.GT.1.D-10 ) THEN
        ALPHAT = ALPHAT / EPSEQ**2
      ELSE
        ALPHAT = 0.0D0
      ENDIF


C ======================================================================
C       CALCUL DES CONTRAINTES ET VARIABLES INTERNES
C           (OPTION FULL_MECA ET RAPH_MECA - (RESI) )
C ====================================================================
      IF (RESI) THEN
C    M.B.: EPSEND est la deformation equivalente
C    qui fait evoluer l endommagement

      IF (COUP) THEN
        EPSEND = EPSEQC
      ELSE
        EPSEND = EPSEQ
      ENDIF

      IF (EPSEND.LE.EPSD0) THEN
C         PAS DE PROGRESSION DE L'ENDOMMAGEMENT
        D = VIM(1)
      ELSE

C   1 -     CALCUL DE L'ENDOMMAGEMENT
C----------------------------------------------------------------
C      ASTUCE POUR EVITER QUE LE CALCUL NE PLANTE DANS LE CALCUL DE
C      EXP(RTEMP) SI RTEMP TROP GRAND
          RTEMPT = BT * ( EPSEND - EPSD0 )
          RTEMPC = BC * ( EPSEND - EPSD0 )
          RTEMPT = MIN(RTEMPT,700.D0)
          RTEMPC = MIN(RTEMPC,700.D0)

          DT = 1.D0 - (EPSD0*(1.D0 - AT )/EPSEND ) -
     &          ( AT / (EXP(RTEMPT  ) ))
          DC = 1.D0 - (EPSD0*(1.D0 - AC )/EPSEND ) -
     &          ( AC / (EXP(RTEMPC) ) )

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

          D = MAX ( VIM(1), D)
          D = MIN(D , 0.99999D0)
            IF (D.GT.VIM(1)) PROG = .TRUE.
            IF (D.GT.0.D0)   ELAS = .FALSE.
        END IF

C    2 -   MISE A JOUR DES VARIABLES INTERNES
C ------------------------------------------------------------

        VIP(IDC+1) = D
        IF (D.EQ.0.D0) THEN
          VIP(IDC+2) = 0.D0
        ELSE
          VIP(IDC+2) = 1.D0
        END IF
          VIP(IDC+4) = EPSEND

C    3 - CALCUL DES CONTRAINTES
C ------------------------------------------------------------

C        ON PASSE DANS LE REPERE INITIAL LES CONTRAINTES REELLES
        CALL R8INIR(6, 0.D0, SIG,1)
          TR(1) = SIGELP(1)*(1.D0-D)
          TR(2) = SIGELP(2)*(1.D0-D)
          TR(3) = SIGELP(3)*(1.D0-D)
          TR(4) = 0.D0
          TR(5) = 0.D0
          TR(6) = 0.D0
        CALL BPTOBG(TR,SIG,VECPE)
        DO  90 K=4,NDIMSI
          SIG(K)=RAC2*SIG(K)
90      CONTINUE
      END IF


C ======================================================================
C     CALCUL  DE LA MATRICE TANGENTE DSIDEP
C         OPTION RIGI_MECA_TANG ET FULL_MECA  (RIGI)
C ======================================================================


      IF (RIGI) THEN

C - M.B.: OPTION FULL_MECA POUR LE COUPLAGE AVEC UMLV
        IF (COUP) D = VIP(IDC+1)

C   1 -  CONTRIBUTION ELASTIQUE
C -------------------------------------------------------------

        CALL R8INIR(36, 0.D0, DSIDEP,1)
        LAMBDA = LAMBDA * (1.D0 - D)
        DEUXMU = DEUXMU * (1.D0 - D)
        DSIDEP(1,1)=LAMBDA+DEUXMU
        DSIDEP(2,2)=LAMBDA+DEUXMU
        DSIDEP(3,3)=LAMBDA+DEUXMU
        DSIDEP(1,2)=LAMBDA
        DSIDEP(2,1)=LAMBDA
        DSIDEP(1,3)=LAMBDA
        DSIDEP(3,1)=LAMBDA
        DSIDEP(2,3)=LAMBDA
        DSIDEP(3,2)=LAMBDA
        DSIDEP(4,4)=DEUXMU
        DSIDEP(5,5)=DEUXMU
        DSIDEP(6,6)=DEUXMU

C   2 -  CONTRIBUTION DUE A  L'ENDOMMAGEMENT
C             ON SYMETRISE LA MATRICE (K + Kt )/2
C ------------------------------------------------------------
        IF ((.NOT.ELAS).AND. PROG) THEN
            RTEMPT = BT * ( EPSEQ - EPSD0 )
            RTEMPC = BC * ( EPSEQ - EPSD0 )
            RTEMPT = MIN(RTEMPT,700.D0)
            RTEMPC = MIN(RTEMPC,700.D0)

          IF (ALPHAT.LT.1.D0.AND.ALPHAT.GT.0.D0) THEN

            COEF =(EPSD0*(1.D0 - AT)/EPSEQ**2 +
     &             AT*BT/ EXP (RTEMPT) )*(ALPHAT**BETA)+
     &            (EPSD0*(1.D0 - AC)/EPSEQ**2 +
     &             AC*BC/ EXP (RTEMPC))* ((1.D0-ALPHAT)**BETA)

          ELSEIF (ALPHAT.GE.1.D0) THEN

            COEF =(EPSD0*(1.D0 - AT)/EPSEQ**2 +
     &             AT*BT/ EXP (RTEMPT) )
          ELSE
            COEF= (EPSD0*(1.D0 - AC)/EPSEQ**2 +
     &             AC*BC/ EXP (RTEMPC))
          ENDIF

          COEF = COEF / EPSEQ
          CALL R8INIR(6, 0.D0, SIGEL,1)
          TR(1) = SIGELP(1)
          TR(2) = SIGELP(2)
          TR(3) = SIGELP(3)
          TR(4) = 0.D0
          TR(5) = 0.D0
          TR(6) = 0.D0
          CALL BPTOBG(TR,SIGEL,VECPE)
          DO  120 K=4,NDIMSI
            SIGEL(K)=RAC2*SIGEL(K)
120       CONTINUE

          DO 220 I=1,6
            DO 221 J=1,6
              DSIDEP (I,J) = DSIDEP (I,J) -
     &                    COEF * SIGEL(I)* EPSPLU(J)
221         CONTINUE
220       CONTINUE


C -- CORRECTION CONTRAINTES PLANES

          IF (CPLAN) THEN
            DO 300 K=1,NDIMSI
              IF (K.EQ.3) GO TO 300
              DO 310 L=1,NDIMSI
              IF (L.EQ.3) GO TO 310
              DSIDEP(K,L)=DSIDEP(K,L)
     &          - 1.D0/DSIDEP(3,3)*DSIDEP(K,3)*DSIDEP(3,L)
 310          CONTINUE
 300        CONTINUE
          ENDIF

        ENDIF

      ENDIF

      END
