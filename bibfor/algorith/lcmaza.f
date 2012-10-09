      SUBROUTINE LCMAZA (FAMI,KPG,KSP,NDIM, TYPMOD, IMATE,COMPOR,EPSM,
     &                   DEPS, VIM, TM,TP,TREF,
     &                   OPTION, SIG, VIP,  DSIDEP)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/10/2012   AUTEUR HAMON F.HAMON 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C         AT = CONSTANTE DE TRACTION     (0.7 A 1)[REEL OU FCT]
C         AC = CONSTANTE DE COMPRESSION (1 A 1.5)[REEL OU FCT]
C         BT = CONSTANTE DE TRACTION    (10 000 A 100 000)[REEL OU FCT]
C         BC = CONSTANTE DE COMPRESSION (1000 A 2000)[REEL OU FCT]
C ----------------------------------------------------------------------
      LOGICAL     RIGI, RESI, PROG, ELAS, CPLAN, COUP
      CHARACTER*1  POUM
      INTEGER ICODRE(7)
      CHARACTER*8 NOMRES(7), NOMPAR
      INTEGER     NDIMSI, NPERM, NITJAC, TRIJ, ORDREJ
      INTEGER     I,J,L,IRET, IISNAN
      REAL*8      E, NU, EPSTHE, KDESS, BENDO,R8NNEM
      REAL*8       AC, AT, BC, BT, EPSD0
      REAL*8      EPS(6), EPSE(6), EPSPLU(6), EPSEP(3), EPSEQ
      REAL*8      SIGEL(6), SIGELP(3)
      REAL*8      TEMP, TMAX, TMAXM, HYDR, SECH , SREF
      REAL*8      TOL, TOLDYN, TR(6), TU(6), JACAUX(3), VECPE(3,3)
      REAL*8      RAC2, LAMBDA, DEUXMU,  COEF
      REAL*8      VALRES(7), VALPAR, COPLAN, D, TMP1,VALA,R,A,B
      REAL*8      KRON(6),K
      REAL*8      EPSFP(6), EPSCOU(6), EPSI(6), CHI,GAMA,RAP
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
          VIP(IDC+3) = 0.D0
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
     &            NOMPAR,VALPAR,2,NOMRES,VALRES,ICODRE,1)
      CALL RCVALB(FAMI,KPG,KSP,POUM,IMATE,' ','ELAS',1,NOMPAR,
     &            VALPAR,1,NOMRES(3),VALRES(3),ICODRE(3),0)
      IF ((IISNAN(TP).EQ.0).AND.(IISNAN(TM).EQ.0)) THEN
        IF ((IISNAN(TREF).NE.0).OR.(ICODRE(3).NE.0)) THEN
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
     &            NOMRES(1),VALRES(1),ICODRE(1), 0)
        IF ( ICODRE(1) .NE.0    ) VALRES(1) = 0.D0
        BENDO = VALRES(1)
        CALL RCVALA(IMATE,' ','ELAS',0,' ',0.D0,1,
     &            NOMRES(2),VALRES(2),ICODRE(2), 0)
        IF ( ICODRE(2) .NE.0    ) VALRES(2) = 0.D0
        KDESS = VALRES(2)
      ELSE
        BENDO = 0.D0
        KDESS = 0.D0
      ENDIF
C    LECTURE DES CARACTERISTIQUES D'ENDOMMAGEMENT
      NOMRES(1) = 'EPSD0'
      NOMRES(2) = 'AC'
      NOMRES(3) = 'BC'
      NOMRES(4) = 'AT'
      NOMRES(5) = 'BT'
      NOMRES(6) = 'K'
      CALL RCVALB(FAMI,KPG,KSP,POUM,IMATE,' ','MAZARS',1,NOMPAR,
     &            VALPAR,6,NOMRES,VALRES,ICODRE,1)
      EPSD0 = VALRES(1)
      AC    = VALRES(2)
      BC    = VALRES(3)
      AT    = VALRES(4)
      BT    = VALRES(5)
      K  = VALRES(6)
C    M.B.: LECTURE DU PARAMETRE DE COUPLAGE AVEC UMLV
      IF (COUP) THEN
        NOMRES(7) = 'CHI'
        CALL RCVALB(FAMI,KPG,KSP,POUM,IMATE,' ','MAZARS',0,' ',
     &              0.D0,1,NOMRES(7),VALRES(7),ICODRE(7),1)
        CHI   = VALRES(7)
        IF (CHI .EQ. 0.D0) THEN
          CALL U2MESS('I','COMPOR1_59')
        ENDIF
      ENDIF
C ======================================================================
C       CALCUL DES GRANDEURS UTILES QUELQUE SOIT OPTION
C ======================================================================
C    1 - CALCUL DES DEFORMATIONS MECANIQUES ET THERMIQUES
C  -  MISE A JOUR DE LA DEFORMATION TOTALE
      CALL R8INIR(6, 0.D0, EPS,1)
      IF (RESI) THEN
        DO  10 J = 1, NDIMSI
          EPS(J) = EPSM(J) + DEPS(J)
10      CONTINUE
      ELSE
        DO 20 J=1,NDIMSI
          EPS(J)=EPSM(J)
20      CONTINUE
        D=VIM(1)
      ENDIF
C    CALCUL DE LA DEFORMATION ELASTIQUE (LA SEULE QUI CONTRIBUE
C    A FAIRE EVOLUER L'ENDOMMAGEMENT)
      CALL R8INIR(6, 0.D0, EPSE,1)
      DO 35 J=1,NDIMSI
        EPSE(J) = EPS(J) - (   EPSTHE
     &                      - KDESS * (SREF-SECH)
     &                      - BENDO *  HYDR         ) * KRON(J)
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
        DO 1010  J=1,NDIMSI
          EPSI(J) = EPSE(J)
          EPSE(J) = EPSI(J) - EPSFP(J)
          EPSCOU(J) = EPSI(J) - (1-CHI)*EPSFP(J)
1010    CONTINUE
      ENDIF
      DO  30 J=4,NDIMSI
        EPSE(J) = EPSE(J)/RAC2
30    CONTINUE
      IF (COUP .AND. RESI) THEN
        DO  31 J=4,NDIMSI
          EPSCOU(J) = EPSCOU(J)/RAC2
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
      DO 40 J = 1,3
        IF (EPSEP(J).GT.0.D0) THEN
          EPSEQ = EPSEQ + (EPSEP(J)**2)
        ENDIF
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
        DO 1040 J = 1,3
          IF (EPSEPC(J).GT.0.D0) THEN
            EPSEQC = EPSEQC + (EPSEPC(J)**2)
          END IF
1040    CONTINUE
        EPSEQC = SQRT(EPSEQC)
      ENDIF
C -  3     CALCUL DE <EPS>+
C ------------------------------------------------------
      CALL R8INIR(6, 0.D0, TR,1)
      CALL R8INIR(6, 0.D0, EPSPLU,1)
      DO 42 J = 1,3
        IF (EPSEP(J).GT.0.D0) THEN
          TR(J) = EPSEP(J)
        ENDIF
42    CONTINUE
      CALL BPTOBG(TR,EPSPLU,VECPE)
      DO  44 J=4,NDIMSI
        EPSPLU(J) = EPSPLU(J)*RAC2
44    CONTINUE
C   4 -  CALCUL DES CONTRAINTES ELASTIQUES (REPERE PRINCIPAL)
C----------------------------------------------------------------
      DO  50 J=1,3
        SIGELP(J) = LAMBDA*(EPSEP(1)+EPSEP(2)+EPSEP(3))
50    CONTINUE
      DO  60 J=1,3
        SIGELP(J) = SIGELP(J) + DEUXMU*EPSEP(J)
60    CONTINUE

      TMP1 = 0.D0
      DO 70 J = 1,3
        IF (SIGELP(J).LT.0.D0) THEN
          TMP1 = TMP1 + SIGELP(J)
        END IF
70    CONTINUE
C   5 -     CALCUL DE R
C----------------------------------------------------------------
      VALA=ABS(SIGELP(1))+ABS(SIGELP(2))
     &+ABS(SIGELP(3))
      R=0.D0
      DO 80 I = 1,3
       R = R + MAX(0.00000000D0,SIGELP(I))
80    CONTINUE
        IF (VALA.GT.1.D-10 ) THEN
                R=R/(VALA)
        ELSE
                R=1.D0
        ENDIF
        IF (R.LT.0.00001D0) R=0.D0
        IF (R.GT.0.99999D0) R=1.D0
        GAMA=0.D0
        RAP=0.D0
      DO 69 I = 1,3
        RAP = RAP + MIN(0.D0,SIGELP(I))
        GAMA = GAMA + (MIN(0.D0,SIGELP(I)))**2
69    CONTINUE
         IF (ABS(RAP).GT.1.D-10 ) THEN
                GAMA = -(SQRT(GAMA)/ RAP)
         ELSE
                GAMA=1.D0
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

      IF (R.EQ.0.D0) EPSEND=GAMA*EPSEND
      IF (EPSEND.LE.EPSD0) THEN
C         PAS DE PROGRESSION DE L'ENDOMMAGEMENT
        D = VIM(1)
      ELSE
        A=2.D0*R**2.D0*(AT-2.D0*K*AT+AC)-R*(AT*(1.D0-4.D0*K)+3.D0*AC)+AC
        B=R**2.D0*BT+(1.D0-R**2.D0)*BC
        D=1.D0-EPSD0*(1.D0-A)/EPSEND
     &-A*EXP(-B*(EPSEND-EPSD0))

          D = MAX ( VIM(1), D)
          D = MIN(D , 0.99999D0)
            IF (D.GT.VIM(1)) PROG = .TRUE.
            IF (D.GT.0.D0)   ELAS = .FALSE.
        ENDIF
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
        DO  90 J=4,NDIMSI
          SIG(J)=RAC2*SIG(J)
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
C             ON SYMETRISE LA MATRICE (J + Kt )/2
C ------------------------------------------------------------
        IF ((.NOT.ELAS).AND. PROG.AND.(D.LT.0.99999D0)) THEN
         IF (EPSEQ.LT.0.0000001D0) THEN
            COEF=0.D0
         ELSE
            COEF =(EPSD0*(1.D0- A)/EPSEQ**2 +
     &             A*B/ EXP (B*(EPSEQ - EPSD0)))
          COEF = COEF / EPSEQ
        ENDIF
          CALL R8INIR(6, 0.D0, SIGEL,1)
          TR(1) = SIGELP(1)
          TR(2) = SIGELP(2)
          TR(3) = SIGELP(3)
          TR(4) = 0.D0
          TR(5) = 0.D0
          TR(6) = 0.D0
          CALL BPTOBG(TR,SIGEL,VECPE)
          DO  120 J=4,NDIMSI
            SIGEL(J)=RAC2*SIGEL(J)
120       CONTINUE
          DO 220 I=1,6
            DO 221 J=1,6
              DSIDEP (I,J) = DSIDEP (I,J) -
     &                    COEF * SIGEL(I)* EPSPLU(J)
221         CONTINUE
220       CONTINUE
C -- CORRECTION CONTRAINTES PLANES
          IF (CPLAN) THEN
            DO 300 J=1,NDIMSI
              IF (J.EQ.3) GO TO 300
              DO 310 L=1,NDIMSI
              IF (L.EQ.3) GO TO 310
               IF (DSIDEP(3,3).NE.0.D0) THEN
                DSIDEP(J,L)=DSIDEP(J,L)
     &          - 1.D0/DSIDEP(3,3)*DSIDEP(J,3)*DSIDEP(3,L)
               ENDIF
 310          CONTINUE
 300        CONTINUE
          ENDIF
        ENDIF
      ENDIF
      END
