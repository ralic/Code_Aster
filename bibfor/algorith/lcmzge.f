      SUBROUTINE LCMZGE (FAMI,KPG,KSP,NDIM, TYPMOD, IMATE,
     &                   EPSTM,DEPST, VIM,
     &                   OPTION, SIG, VIP,  DSIDPT, PROJ)
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C TOLE CRP_20
      IMPLICIT NONE
      CHARACTER*8       TYPMOD(*)
      CHARACTER*(*)     FAMI
      CHARACTER*16      OPTION
      INTEGER            NDIM, IMATE,KPG,KSP
      REAL*8             EPSTM(12), DEPST(12), VIM(4)
      REAL*8             SIG(6), VIP(*), DSIDPT(6,6,2)
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
      INTEGER ICODRE(7)
      CHARACTER*8 NOMRES(7) , NOMPAR
      INTEGER     NDIMSI, NPERM, NITJAC, TRIJ, ORDREJ
      INTEGER     I,J,L,IRET,IRET1,IRET2,IRET3
      REAL*8      E, NU, EPSTHE, KDESS, BENDO,R8NNEM
      REAL*8      AC, AT, BC, BT, EPSD0
      REAL*8      EPSM(6), EPSRM(6), DEPS(6), DEPSR(6), EPSPLU(6)
      REAL*8      EPSE(6), EPSER(6), EPSPR(3), EPSP(3)
      REAL*8      EPSR(6), EPS(6), TREPS, EPSEQ, EPSTIL
      REAL*8      SIGEL(6), SIGELP(3)
      REAL*8      TEMP, TMAX, TMAXM, HYDR, SECH, SREF,TREF
      REAL*8      TOL, TOLDYN, TR(6), TU(6), TRR(6), JACAUX(3)
      REAL*8      VECPE(3,3), VECPER(3,3)
      REAL*8      COPLAN,  LAMBDA, DEUXMU
      REAL*8      RAC2, COEF, TMP1, D,RAP,GAMA,K
      REAL*8      VALRES(7), VALPAR
      REAL*8      KRON(6)
      REAL*8      EPSFP(6), EPSCOU(6), CHI,VALA,R,A,B
      INTEGER     IDC
      LOGICAL     COUP

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
      IRET1 = 0
      IRET2 = 0
      IRET3 = 0
C M.B.: NOUVELLE OPTION COUP POUR LE COUPLAGE AVEC UMLV
C MEME OPTION UTILISEE POUR LE COUPLAGE UMLV-ENDO_ISOT_BETON
      COUP  = (OPTION(6:9).EQ.'COUP')
C M.B.: INDICE POUR IDENTIFIER LES VARIABLES INTERNES DANS LES CAS:
C COUPLAGE ET ABSENCE DE COUPLAGE AVEC UMLV
      IDC = 0
      IF (COUP) THEN
        IDC = 21
      ENDIF
C -- PROJECTEUR DE COUPURE
      CALL R8INIR(36,0.D0,PROJ,1)
      IF (VIM(1) .LT. 1.D0-1.D-05) CALL R8INIR(6,1.D0,PROJ,7)
C     DETERMINATION DE LA TEMPERATURE DE REFERENCE (TMAX) ET
C   DES CONDITIONS D HYDRATATION OU DE SECHAGE
      TMAXM = VIM(3)
      CALL RCVARC(' ','TEMP','REF',FAMI,KPG,KSP,TREF,IRET1)
      CALL RCVARC(' ','SECH','REF',FAMI,KPG,KSP,SREF,IRET)
      IF (IRET.NE.0) SREF=0.D0
      IF (RESI) THEN
        POUM='+'
        CALL RCVARC(' ','TEMP',POUM,FAMI,KPG,KSP,TEMP,IRET2)
        IF (IRET2.EQ.1) THEN
          TMAX = R8NNEM()
        ELSE
          TMAX = MAX(TMAXM, TEMP)
          IF (TMAX.GT.TMAXM) VIP(IDC+3) = TMAX
        ENDIF
      ELSE
        POUM='-'
        CALL RCVARC(' ','TEMP',POUM,FAMI,KPG,KSP,TEMP,IRET3)
        IF (IRET3.EQ.1) THEN
          TMAX = R8NNEM()
        ELSE
          TMAX = MAX(TMAXM, TEMP)
        ENDIF
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
     &              NOMRES,VALRES,ICODRE, 1)
      CALL RCVALB(FAMI,KPG,KSP,POUM,IMATE,' ','ELAS',1,NOMPAR,VALPAR,1,
     &              NOMRES(3),VALRES(3),ICODRE(3), 0)
      IF ((IRET2+IRET3).EQ.0) THEN
        IF ((IRET1.NE.0).OR.(ICODRE(3).NE.0)) THEN
          CALL U2MESS('F','CALCULEL_15')
        ELSE
          EPSTHE = VALRES(3) * (TEMP - TREF)
        ENDIF
      ELSE
          EPSTHE = 0.D0
      ENDIF
      E     = VALRES(1)
      NU    = VALRES(2)
      LAMBDA = E * NU / (1.D0+NU) / (1.D0-2.D0*NU)
      DEUXMU = E/(1.D0+NU)
C --- LECTURE DU RETRAIT ENDOGENE ET RETRAIT DE DESSICCATION
      NOMRES(1)='B_ENDOGE'
      NOMRES(2)='K_DESSIC'
      CALL RCVALB(FAMI,1,1,'+',IMATE,' ','ELAS',0,' ',0.D0,1,
     &            NOMRES(1),VALRES(1),ICODRE(1), 0)
      IF ( ICODRE(1) .NE.0    ) VALRES(1) = 0.D0
      BENDO = VALRES(1)
      CALL RCVALB(FAMI,1,1,'+',IMATE,' ','ELAS',0,' ',0.D0,1,
     &            NOMRES(2),VALRES(2),ICODRE(2), 0)
      IF ( ICODRE(2) .NE.0    ) VALRES(2) = 0.D0
      KDESS = VALRES(2)
C --- LECTURE DES CARACTERISTIQUES D'ENDOMMAGEMENT
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
C -- SEPARATION DE EPSM/EPSRM, DEPS/DEPSR DANS EPSTM, DEPST
      DO 10 I=1,NDIMSI
        EPSM(I)=EPSTM(I)
        EPSRM(I)=EPSTM(I+6)
        DEPS(I)=DEPST(I)
        DEPSR(I)=DEPST(I+6)
10    CONTINUE
C -   M.B.: CALCUL DE LA DEFORMATION DE FLUAGE AU TEMP P
      IF (COUP .AND. RESI) THEN
        CALL LCUMVI('FT',VIP,EPSFP)
      ENDIF
C ======================================================================
C    CALCUL DES CONTRAINTES ET VARIABLES INTERNES
C    (OPTION FULL_MECA ET RAPH_MECA)
C ======================================================================
      CALL R8INIR(6, 0.D0, EPS,1)
      CALL R8INIR(6, 0.D0, EPSR, 1)
C  -   MISE A JOUR DES DEFORMATIONS MECANIQUES
      IF (RESI) THEN
        DO  20 J = 1, NDIMSI
          EPS(J) = EPSM(J) + DEPS(J)
          EPSR(J) = EPSRM(J) + DEPSR(J)
20      CONTINUE
      ELSE
        DO  30 J=1,NDIMSI
          EPS(J)=EPSM(J)
          EPSR(J)=EPSRM(J)
30      CONTINUE
        D=VIM(1)
      ENDIF
C -  MODIF M.B.: ON MET DANS EPS LES DEFORMATIONS REELES
      DO  40 J=4,NDIMSI
        EPS(J) = EPS(J)/RAC2
        EPSR(J)= EPSR(J)/RAC2
        IF (COUP .AND. RESI) THEN
        EPSFP(J) = EPSFP(J)/RAC2
        ENDIF
40    CONTINUE


C    CALCUL DE LA DEFORMATION ELASTIQUE (LA SEULE QUI CONTRIBUE
C    A FAIRE EVOLUER L'ENDOMMAGEMENT)

      CALL R8INIR(6, 0.D0, EPSE,1)
      CALL R8INIR(6, 0.D0, EPSER,1)
      DO 35 J=1,NDIMSI
        EPSE(J) = EPS(J) - (   EPSTHE
     &                      - KDESS * (SREF-SECH)
     &                      - BENDO *  HYDR         ) * KRON(J)
        EPSER(J) = EPSR(J) - (   EPSTHE
     &                      - KDESS * (SREF-SECH)
     &                      - BENDO *  HYDR         ) * KRON(J)
35    CONTINUE


C  M.B.: SI CONTRAINTES PLAN (COUP)
C  ON CALCULE LA 3EME  COMPOSANTE NORMALE
C   AVANT DE DIAGONALISER
      IF (COUP .AND. RESI) THEN
        IF (CPLAN) THEN
          COPLAN  = - NU/(1.D0-NU)
          EPSE(3) = COPLAN * (EPSE(1)+EPSE(2))
          EPSER(3) = COPLAN * (EPSER(1)+EPSER(2))
        END IF
      END IF

      IF (COUP .AND. RESI) THEN
        CALL R8INIR(6, 0.D0, EPSCOU,1)
        DO 1010  J=1,NDIMSI
          EPSE(J) = EPSE(J) - EPSFP(J)
          EPSCOU(J) = EPSER(J) - (1.D0-CHI)*EPSFP(J)
1010    CONTINUE
      ENDIF


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
      IF (COUP .AND. RESI) THEN
        TRR(1) = EPSCOU(1)
        TRR(2) = EPSCOU(4)
        TRR(3) = EPSCOU(5)
        TRR(4) = EPSCOU(2)
        TRR(5) = EPSCOU(6)
        TRR(6) = EPSCOU(3)
      ENDIF
      CALL JACOBI(3,NPERM,TOL,TOLDYN,TRR,TU,VECPER,EPSPR,JACAUX,
     &       NITJAC,TRIJ,ORDREJ)
C -- SI CONTRAINTES PLANES
C    Modifie M.B.: if NOT COUP
      IF (.NOT. COUP) THEN
        IF (CPLAN) THEN
          COPLAN  = - NU/(1.D0-NU)
          EPSP(3)  = COPLAN * (EPS(1)+EPS(2))
          EPSPR(3)  = COPLAN * (EPSR(1)+EPSR(2))
        ENDIF
      ENDIF
C--  ------------------------------
C      CALCUL DE L'ETAT D'ENDOMMAGEMENT
C -  ------------------------------
C     CALCUL DE EPSEQ (NON LOCAL) ET EPSTIL (LOCAL)
      EPSEQ = 0.D0
      EPSTIL = 0.D0
      DO 50 J = 1,3
        IF (EPSPR(J).GT.0.D0) THEN
          EPSEQ = EPSEQ + (EPSPR(J)**2)
        END IF
      IF (EPSP(J).GT.0.D0) THEN
        EPSTIL = EPSTIL + (EPSP(J)**2)
      ENDIF
50    CONTINUE
      EPSEQ = SQRT(EPSEQ)
      EPSTIL = SQRT(EPSTIL)
C -     CALCUL DES CONTRAINTES ELASTIQUES (REPERE PRINCIPAL)
      TREPS = EPSP(1)+EPSP(2)+EPSP(3)
      DO 60  J=1,3
        SIGELP(J) = LAMBDA*TREPS
60    CONTINUE
      DO  70 J=1,3
        SIGELP(J) = SIGELP(J) + DEUXMU*EPSP(J)
70    CONTINUE
      TMP1 = 0.D0
      DO 80 J = 1,3
        IF (SIGELP(J).LT.0.D0) THEN
          TMP1 = TMP1 + SIGELP(J)
        ENDIF
80    CONTINUE
      IF (RESI) THEN
C   5 -     CALCUL DE R
C----------------------------------------------------------------
      VALA=ABS(SIGELP(1))+ABS(SIGELP(2))+ABS(SIGELP(3))
      R=0.D0
      DO 81 I = 1,3
       R = R + MAX(0.00000000D0,SIGELP(I))
81    CONTINUE
         IF (VALA.GT.1.D-10 ) THEN
                R=(R/ VALA)
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
        IF (R.EQ.0.D0) EPSEQ=GAMA*EPSEQ
C      CALCUL DES PARAMETRES D'ENDOMMAGEMENT
        IF (EPSEQ.LE.EPSD0) THEN
          D=VIM(1)
        ELSE
        A=2.D0*R**2.D0*(AT-2.D0*K*AT+AC)-R*(AT*(1.D0-4.D0*K)+3.D0*AC)+AC
        B=R**2.D0*BT+(1.D0-R**2.D0)*BC
        D=1.D0-EPSD0*(1.D0-A)/EPSEQ
     &-A*EXP(-B*(EPSEQ-EPSD0))

          D = MIN(D , 0.99999D0)
          D = MAX ( VIM(1), D)
          IF (D.GT.VIM(1)) PROG = .TRUE.
          IF (D.GT.0.D0)   ELAS = .FALSE.
        ENDIF
      ENDIF
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
        DO  100 J=4,NDIMSI
          SIG(J)=RAC2*SIG(J)
100     CONTINUE
        VIP(IDC+1) = D
        IF (D.EQ.0.D0) THEN
          VIP(IDC+2) = 0.D0
        ELSE
          VIP(IDC+2) = 1.D0
        ENDIF
          VIP(IDC+4) = EPSEQ
      ENDIF
C ======================================================================
C     CALCUL  DE LA MATRICE TANGENTE DSIDEP
C         OPTION RIGI_MECA_TANG ET FULL_MECA
C ======================================================================
C ======================================================================
C                            MATRICE TANGENTE
C ======================================================================
      IF (RIGI) THEN
C - M.B.: OPTION FULL_MECA POUR LE COUPLAGE AVEC UMLV
        IF (COUP) D = VIP(IDC+1)
C -- CONTRIBUTION ELASTIQUE
        CALL R8INIR(72, 0.D0, DSIDPT, 1)
        DO 110 J = 1,3
          DO 120 L = 1,3
            DSIDPT(J,L,1) = (1.D0-D)*LAMBDA
120       CONTINUE
110     CONTINUE
        DO 130 J = 1,NDIMSI
          DSIDPT(J,J,1) = DSIDPT(J,J,1) + (1-D)*DEUXMU
130     CONTINUE

        IF ((.NOT.ELAS).AND.PROG.AND.(.NOT.RELA).AND.
     +(D.LT.0.99999D0)) THEN
               IF (EPSEQ.LT.1.D-10 ) THEN
          COEF=0.D0
                ELSE
          COEF =(EPSD0*(1.D0 - A)/EPSEQ**2 +
     &             A*B/ EXP (B*(EPSEQ - EPSD0)))
          COEF = COEF/EPSEQ
               ENDIF
C      CALCUL DE EPS+
C
          CALL R8INIR(6, 0.D0, TR,1)
          DO 160 J = 1,3
            IF (EPSPR(J).GT.0.D0) THEN
              TR(J) = EPSPR(J)
            END IF
160       CONTINUE
          CALL BPTOBG(TR,EPSPLU,VECPER)
          DO  170 J=4,NDIMSI
            EPSPLU(J) = EPSPLU(J)*RAC2
170       CONTINUE
          CALL R8INIR(6, 0.D0, SIGEL,1)
          TR(1) = SIGELP(1)
          TR(2) = SIGELP(2)
          TR(3) = SIGELP(3)
          TR(4) = 0.D0
          TR(5) = 0.D0
          TR(6) = 0.D0
          CALL BPTOBG(TR,SIGEL,VECPE)
          DO  180 J=4,NDIMSI
            SIGEL(J)=RAC2*SIGEL(J)
180       CONTINUE
          DO  190 I=1,6
            DO 200 J=1,6
              DSIDPT(I,J,2) = - COEF * SIGEL(I)* EPSPLU(J)
200         CONTINUE
190       CONTINUE
        ENDIF

      ENDIF
      END
