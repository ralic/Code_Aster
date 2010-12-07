      SUBROUTINE LCGLDM (EPSM,DEPS,VIM,OPTION,SIG,VIP,DSIDEP,
     &                   T,LAMBDA,DEUXMU,LAMF,DEUMUF,GMT,GMC,GF,
     &                   SEUIL,ALF,CRIT,CODRET)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 07/12/2010   AUTEUR GENIAUT S.GENIAUT 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE SFAYOLLE S.FAYOLLE

      IMPLICIT NONE

      INTEGER      CODRET
      REAL*8       EPSM(6), DEPS(6), VIM(*), GF, GF1, GF2, CRIT(*)
      REAL*8       SIG(6), VIP(*), DSIDEP(6,6), LAMF, DEUMUF
      CHARACTER*16 OPTION
C ----------------------------------------------------------------------
C
C      LOI GLOBALE POUR LES PLAQUES/COQUES DKT - GLRC_DM
C
C IN:
C       LAMBDA  : PARAMETRE D ELASTICITE - MEMBRANE
C       DEUXMU  : PARAMETRE D ELASTICITE - MEMBRANE
C       LAMF    : PARAMETRE D ELASTICITE - FLEXION
C       DEUMUF  : PARAMETRE D ELASTICITE - FLEXION 
C       GMT     : PARAMETRE GAMMA POUR LA MEMBRANE EN TRACTION 
C       GMC     : PARAMETRE GAMMA POUR LA MEMBRANE EN COMPRESSION
C       GF      : PARAMETRE GAMMA POUR LA FLEXION
C       SEUIL   : INITIAL MEMBRANE
C       ALF     : PARAMETRE DE SEUIL FLEXION
C       VIM     : VARIABLES INTERNES EN T-
C       OPTION  : TOUTE
C       CRIT : CRITERES DE CONVERGENCE LOCAUX
C              (1) = NB ITERATIONS MAXI A CONVERGENCE
C                    (ITER_INTE_MAXI == ITECREL)
C              (2) = TYPE DE JACOBIEN A T+DT
C                    (TYPE_MATR_COMP == MACOMP)
C                     0 = EN VITESSE     >SYMETRIQUE
C                     1 = EN INCREMENTAL >NON-SYMETRIQUE
C              (3) = VALEUR TOLERANCE DE CONVERGENCE
C                    (RESI_INTE_RELA == RESCREL)
C              (5) = NOMBRE D'INCREMENTS POUR LE
C                    REDECOUPAGE LOCAL DU PAS DE TEMPS
C                    (ITER_INTE_PAS  == ITEDEC)
C                    -1,0,1 = PAS DE REDECOUPAGE
C                     N = NOMBRE DE PALIERS
C              (6) = TYPE D INTEGRATION LOCAL POUR LA LOI DE 
C                    COMPORTEMENT (ALGO_INTE)
C OUT:
C       SIG     : CONTRAINTE
C       VIP     : VARIABLES INTERNES EN T+
C       DSIDEP  : MATRICE TANGENTE
C       D2      : ET DE L AUTRE
C       CODRET  : CODE RETOUR DE L'INTEGRATION INTEGRATION DU
C                 0 => PAS DE PROBLEME
C                 1 => ABSENCE DE CONVERGENCE
C ----------------------------------------------------------------------
C
C       QM1 ET QM2 = Tm DANS R7.01.32
C       QFF        = Tf DANS R7.01.32

      LOGICAL RIGI, RESI, COUP, LSING1, LSING2
      LOGICAL LELAS, ELAS, ELAS1, ELAS2

      INTEGER K, L, I, T(2,2),KDMAX

      REAL*8  EPS(6), TREPS, FD1, FD2, DA1, DA2
      REAL*8  EMP(2), VMP(2,2), VFP(2,2), DSPDEP(6,6)
      REAL*8  DEUMUD(3), LAMBDD, SIGP(3), RTEMP, GMT, GMC
      REAL*8  SEUIL, LAMBDA, DEUXMU, QME33
      REAL*8  MU, A11, A22, A12, A21, B1, B2
      REAL*8  TREPS2, TR2, EN0, COF1
      REAL*8  FDI1(3), FDI2(3), SD1(3), SD2(3), D1E(3), D2E(3)
      REAL*8  QDE(3), EFP(2), GI(2)
      REAL*8  TR2D, KSI2D, EPS33, DE33I, QFF(2), SIGF(2)
      REAL*8  DKSI1, DKSI2, TOLD, DE33D1, DE33D2, Q2D, GTR2
      REAL*8  LAMFD, DEMUDF(2), TROT, TROT2, DLMFD1, DLMFD2
      REAL*8  D1MUDF(2), D2MUDF(2), SFFT1, SFFT2, SFF1(2), SFF2(2)
      REAL*8  MD1(2), MD2(2), D1K(2), D2K(2), MUF
      REAL*8  ALF, QM1, QM2, DLMD1, DLMD2, D1MUD(2), D2MUD(2)

C -- OPTION ET MODELISATION
      RIGI  = (OPTION(1:4).EQ.'RIGI' .OR. OPTION(1:4).EQ.'FULL')
      RESI  = (OPTION(1:4).EQ.'RAPH' .OR. OPTION(1:4).EQ.'FULL')
      COUP  = (OPTION(6:9).EQ.'COUP')
      IF (COUP) RIGI=.TRUE.
      LELAS =  OPTION .EQ.'RIGI_MECA       '

C -- INITIALISATION
      IF(LELAS) THEN
        CALL R8INIR(6,0.D0,EPSM,1)
        CALL R8INIR(6,0.D0,DEPS,1)
      ENDIF
      MU  = DEUXMU*0.5D0
      MUF = DEUMUF*0.5D0

C-- ICI ON SUPPOSE QUE GF1=GF2, CE QUI N EST PAS NECESSAIRE
      GF1 = GF
      GF2 = GF
      IF (RESI) THEN
        DO 10 K = 1, 6
          EPS(K) = EPSM(K) + DEPS(K)
 10     CONTINUE
      ELSE
        DO 40 K=1,6
          EPS(K) = EPSM(K)
40      CONTINUE
      ENDIF

C -- ON UTILISE EPSILON SOUS FORME VECTORIELLE
C    DONC ON DIVISE LES TERMES NON DIAGONNAUX PAR 2
      EPS(3) = EPS(3)/2.0D0
      EPS(6) = EPS(6)/2.0D0

C -- DIAGONALISATION DES DEFORMATIONS
      CALL DIAGO2(EPS(1),VMP,EMP)
      CALL DIAGO2(EPS(4),VFP,EFP)

C -- CALCUL DES D1,D2,EPS33 INITIAUX
      TR2D  = EPS(1)+EPS(2)
      TROT  = EFP(1)+EFP(2)
      TROT2 = TROT**2

C   CALCULER LES CONSTANTES INDEPENDANT DE D1,D2,EPS33
      IF(TR2D .GT. 0.0D0) THEN
        GTR2 = 1.0D0 - GMT
      ELSE
        GTR2 = 1.0D0 - GMC
      ENDIF

      QFF(1) = 0.0D0
      QFF(2) = 0.0D0

      IF(TROT .GT. 0.0D0) THEN
        QFF(2) = 0.5D0*LAMF*TROT2
      ELSE
        QFF(1) = 0.5D0*LAMF*TROT2
      ENDIF

      DO 4510, K = 1,2
        IF(EFP(K) .GT. 0.0D0) THEN
          QFF(2) = QFF(2) + MUF*EFP(K)**2
        ELSE
          QFF(1) = QFF(1) + MUF*EFP(K)**2
        ENDIF
4510  CONTINUE

      QFF(1) = ALF*QFF(1)*(1.0D0 - GF1)
      QFF(2) = ALF*QFF(2)*(1.0D0 - GF2)

      DO 50, K = 1,2
        IF(EMP(K) .GT. 0.0D0) THEN
          GI(K) = 1.0D0 - GMT
        ELSE
          GI(K) = 1.0D0 - GMC
        ENDIF
 50   CONTINUE

      COF1 = 0.5D0*LAMBDA*GTR2
      Q2D  = 0.5D0*MU * (EMP(1)**2*GI(1) + EMP(2)**2*GI(2))


      IF(LELAS) THEN
        DA1 = 0.0D0
        DA2 = 0.0D0
      ELSE
        DA1 = VIM(1)
        DA2 = VIM(2)
      ENDIF

      CALL CEPS33 (LAMBDA,DEUXMU,TR2D,DA1,DA2,GMT,GMC,
     &             EPS33,DE33D1,DE33D2,KSI2D,DKSI1,DKSI2)

      TREPS  = TR2D + EPS33
      TREPS2 = TREPS**2

      QM1 = 0.5D0*COF1*TREPS2+Q2D
      QM2 = QM1

C--------CALCUL DE DA1,DA2,EPS33
      IF (RESI) THEN
        TOLD = CRIT(3)
        KDMAX = NINT(CRIT(1))

        CALL GLDLOC(LAMBDA,DEUXMU,SEUIL,ALF,GMT,GMC,COF1,VIM
     &                      ,Q2D,QFF,TR2D,EPS33,DE33D1,DE33D2,
     &                      KSI2D,DA1,DA2,KDMAX,TOLD,CODRET)

        IF (DA1.LT.VIM(1))DA1 = VIM(1)
        IF (DA2.LT.VIM(2))DA2 = VIM(2)

        ELAS1 = DA1.LE.VIM(1)
        ELAS2 = DA2.LE.VIM(2)

        ELAS1 = ELAS1 .OR. LELAS
        ELAS2 = ELAS2 .OR. LELAS
        ELAS  = ELAS1 .AND. ELAS2

        VIP(1) = DA1
        VIP(2) = DA2
        IF(ELAS1) THEN
          VIP(3) = 0.0D0
        ELSE
          VIP(3) = 1.0D0
        ENDIF
        IF(ELAS2) THEN
          VIP(4) = 0.0D0
        ELSE
          VIP(4) = 1.0D0
        ENDIF
      ELSE
        IF(LELAS) THEN
          DA1 = 0.0D0
          DA2 = 0.0D0
          ELAS1 = .TRUE.
          ELAS2 = .TRUE.
          ELAS  = .TRUE.
        ELSE
          DA1   = VIM(1)
          DA2   = VIM(2)
          ELAS1 = NINT(VIM(3)).EQ.0
          ELAS2 = NINT(VIM(4)).EQ.0
          ELAS1 = ELAS1 .OR. LELAS
          ELAS2 = ELAS2 .OR. LELAS
          ELAS=(ELAS1.AND.ELAS2)
        ENDIF
      ENDIF

      TR2 = EMP(1)**2 + EMP(2)**2 + EPS33**2
      EN0 = 0.5D0*LAMBDA*TREPS2 + MU*TR2

      IF(TR2D .GT. 0.0D0) THEN
        FD1 = (1.0D0 + GMT*DA1) / (1.0D0 + DA1)
        FD2 = (1.0D0 + GMT*DA2) / (1.0D0 + DA2)
        DLMD1 = -0.5D0*LAMBDA*(1.D0-GMT)/(1.D0+DA1)**2
        DLMD2 = -0.5D0*LAMBDA*(1.D0-GMT)/(1.D0+DA2)**2
      ELSE
        FD1 = (1.0D0 + GMC*DA1) / (1.0D0 + DA1)
        FD2 = (1.0D0 + GMC*DA2) / (1.0D0 + DA2)
        DLMD1 = -0.5D0*LAMBDA*(1.D0-GMC)/(1.D0+DA1)**2
        DLMD2 = -0.5D0*LAMBDA*(1.D0-GMC)/(1.D0+DA2)**2
      ENDIF

      LAMBDD = LAMBDA *0.5D0 *(FD1 + FD2)

      DO 80, K=1,2
        IF (EMP(K).GT.0.D0) THEN
          FDI1(K) = (1.0D0 + GMT*DA1) / (1.0D0 + DA1)
          FDI2(K) = (1.0D0 + GMT*DA2) / (1.0D0 + DA2)
          D2MUD(K) = -MU*(1.D0-GMT)/(1.D0+DA2)**2
          D1MUD(K) = -MU*(1.D0-GMT)/(1.D0+DA1)**2
        ELSE
          FDI1(K) = (1.0D0 + GMC*DA1) / (1.0D0 + DA1)
          FDI2(K) = (1.0D0 + GMC*DA2) / (1.0D0 + DA2)
          D2MUD(K) = -MU*(1.D0-GMC)/(1.D0+DA2)**2
          D1MUD(K) = -MU*(1.D0-GMC)/(1.D0+DA1)**2
        ENDIF
        DEUMUD(K) = MU*(FDI1(K) + FDI2(K))
        SIGP(K)=LAMBDD*TREPS + DEUMUD(K)*EMP(K)
 80   CONTINUE

      IF(TROT .GT. 0.0D0) THEN
        LAMFD  =  LAMF*(ALF + GF2*DA2)/(ALF + DA2)
        DLMFD2 = -LAMF*ALF*(1.0D0 - GF2)/(ALF + DA2)**2
        DLMFD1 = 0.0D0
        SFFT2  =  LAMF*ALF*(1.0D0 - GF2)
        SFFT1  =  0.0D0
      ELSE
        LAMFD  =  LAMF*(ALF + GF1*DA1)/(ALF + DA1)
        DLMFD1 = -LAMF*ALF*(1.0D0 - GF1)/(ALF + DA1)**2
        DLMFD2 = 0.0D0
        SFFT1  =  LAMF*ALF*(1.0D0 - GF1)
        SFFT2  =  0.0D0
      ENDIF

      DO 90, K = 1,2
        IF(EFP(K) .GT. 0.0D0) THEN
          DEMUDF(K) =  DEUMUF*(ALF + GF2*DA2)/(ALF + DA2)
          D2MUDF(K) = -DEUMUF*ALF*(1.0D0 - GF2)/(ALF + DA2)**2
          D1MUDF(K) =  0.0D0
          SFF2(K)   =  DEUMUF*ALF*(1.0D0 - GF2)
          SFF1(K)   =  0.0D0
        ELSE
          DEMUDF(K) =  DEUMUF*(ALF + GF1*DA1)/(ALF + DA1)
          D1MUDF(K) = -DEUMUF*ALF*(1.0D0 - GF1)/(ALF + DA1)**2
          D2MUDF(K) =  0.0D0
          SFF1(K)   =  DEUMUF*ALF*(1.0D0 - GF1)
          SFF2(K)   =  0.0D0
        ENDIF
        SIGF(K) = LAMFD*TROT + DEMUDF(K)*EFP(K)
 90   CONTINUE

      IF (RESI .AND.(.NOT.COUP)) THEN
        CALL R8INIR(6,0.D0,SIG,1)
        DO 1010 I=1,2
          RTEMP=SIGP(I)
          SIG(1)=SIG(1)+VMP(1,I)**2*RTEMP
          SIG(2)=SIG(2)+VMP(2,I)**2*RTEMP
          SIG(3)=SIG(3)+VMP(1,I)*VMP(2,I)*RTEMP

          RTEMP=SIGF(I)
          SIG(4)=SIG(4)+VFP(1,I)**2*RTEMP
          SIG(5)=SIG(5)+VFP(2,I)**2*RTEMP
          SIG(6)=SIG(6)+VFP(1,I)*VFP(2,I)*RTEMP
1010    CONTINUE
      ENDIF

C -- CALCUL DE LA MATRICE TANGENTE
      IF (RIGI) THEN
        IF (OPTION(11:14).EQ.'ELAS') THEN
          ELAS  =.TRUE.
          ELAS1 =.TRUE.
          ELAS2 =.TRUE.
        ENDIF

        CALL R8INIR(36, 0.D0, DSPDEP, 1)

        IF (COUP) THEN
          CALL R8INIR(72, 0.D0, DSIDEP, 1)
        ELSE
          CALL R8INIR(36, 0.D0, DSIDEP, 1)
        ENDIF

        DE33I = -LAMBDA*KSI2D/(DEUXMU  + LAMBDA*KSI2D)

        DO 100 K = 1,2
          DO 110 L = 1,2
            DSPDEP(K,L) = LAMBDD + LAMBDA*KSI2D*DE33I
            DSPDEP(K+3,L+3) = LAMFD
 110      CONTINUE
 100    CONTINUE

        DO 120 K = 1,2
          DSPDEP(K,K) = DSPDEP(K,K) + DEUMUD(K)
          DSPDEP(K+3,K+3) = DSPDEP(K+3,K+3) + DEMUDF(K)
 120    CONTINUE

        IF (ABS(EMP(1) - EMP(2)) .LE. 1.D-15) THEN
          DSPDEP(3,3)=DEUMUD(1)
        ELSE
          DSPDEP(3,3)=(DEUMUD(1)*EMP(1)-DEUMUD(2)*EMP(2))
     &                                    /(EMP(1)-EMP(2))
        ENDIF

        IF (ABS(EFP(1) - EFP(2)) .LE. 1.D-15) THEN
          DSPDEP(6,6)=DEMUDF(1)
        ELSE
          DSPDEP(6,6)=(DEMUDF(1)*EFP(1)-DEMUDF(2)*EFP(2))
     &                                    /(EFP(1)-EFP(2))
        ENDIF

C -- CONTRIBUTION DISSIPATIVE
        IF ((.NOT. ELAS).AND.((EN0.GT.0.D0) .OR.
     &     ((QFF(1) + QFF(2)).GT.0.0D0))) THEN

          DO 800, K=1,2
            TREPS = TR2D + EPS33
            TREPS2 = TREPS**2
            QM1 = 0.5D0*COF1*TREPS2+Q2D
            QM2 = QM1
            QME33 = COF1*TREPS
            QDE(K) = COF1*TREPS+MU*EMP(K)*GI(K)

            A11 = 2.0D0*(QM1/(1.0D0 + DA1)**3 + QFF(1)/(ALF + DA1)**3)
     &          - QME33*DE33D1/(1.0D0 + DA1)**2
            A12 = -QME33*DE33D2/(1.0D0 + DA1)**2

            A22 = 2.0D0*(QM2/(1.0D0 + DA2)**3 + QFF(2)/(ALF + DA2)**3)
     &          - QME33*DE33D2/(1.0D0 + DA2)**2
            A21 = -QME33*DE33D1/(1.0D0 + DA2)**2

            B1  = (QDE(K) - QME33* LAMBDD/(DEUXMU + LAMBDD))
     &          / (1.0D0 + DA1)**2
            B2  = (QDE(K) - QME33* LAMBDD/(DEUXMU + LAMBDD))
     &          / (1.0D0 + DA2)**2

            LSING1 = ABS(A11) .LT. MAX(1.0D-10*A22,1.0D-14)
            LSING2 = ABS(A22) .LT. MAX(1.0D-10*A11,1.0D-14)

            IF(LSING2 .AND. (.NOT. LSING1)) THEN
              D1E(K) = B1/A11
              D2E(K) = 0.0D0
            ELSEIF(LSING1 .AND. (.NOT. LSING2)) THEN
              D1E(K) = 0.0D0
              D2E(K) = B2/A22
            ELSEIF(LSING2 .AND. LSING1) THEN
              D1E(K) = 0.0D0
              D2E(K) = 0.0D0
            ELSE
              D1E(K) = (B1 - A12*B2/A22)/(A11 - A12*A21/A22)
              D2E(K) = (B2 - A21*D1E(K))/A22
            ENDIF

            SD1(K) = DLMD1*TREPS + EMP(K)*D1MUD(K)
            SD2(K) = DLMD2*TREPS + EMP(K)*D2MUD(K)

            MD1(K) = DLMFD1*TROT + D1MUDF(K)*EFP(K)
            MD2(K) = DLMFD2*TROT + D2MUDF(K)*EFP(K)

            B1 = (SFFT1*TROT + SFF1(K)*EFP(K)) / (ALF + DA1)**2
            B2 = (SFFT2*TROT + SFF2(K)*EFP(K)) / (ALF + DA2)**2

            IF(LSING2 .AND. (.NOT. LSING1)) THEN
              D1K(K) = B1/A11
              D2K(K) = 0.0D0
            ELSEIF(LSING1 .AND. (.NOT. LSING2)) THEN
              D1K(K) = 0.0D0
              D2K(K) = B2/A22
            ELSEIF(LSING2 .AND. LSING1) THEN
              D1K(K) = 0.0D0
              D2K(K) = 0.0D0
            ELSE
              D1K(K) = (B1 - A12*B2/A22)/(A11 - A12*A21/A22)
              D2K(K) = (B2 - A21*D1K(K))/A22
            ENDIF
 800      CONTINUE

          DO 910, K=1,2
            DO 900, L=1,2
C--------MEMBRANE-----------------
              IF(.NOT. ELAS1)THEN
                DSPDEP(L,K) = DSPDEP(L,K) + SD1(L)*D1E(K)
                DSPDEP(L,K) = DSPDEP(L,K) + LAMBDA*KSI2D*DE33D1*D1E(K)
              ENDIF
              IF(.NOT. ELAS2) THEN
                DSPDEP(L,K) = DSPDEP(L,K) + SD2(L)*D2E(K)
                DSPDEP(L,K) = DSPDEP(L,K) + LAMBDA*KSI2D*DE33D2*D2E(K)
              ENDIF

C--------FLEXION-----------------
              IF(.NOT. ELAS1)
     &          DSPDEP(L+3,K+3) = DSPDEP(L+3,K+3) + MD1(L)*D1K(K)
              IF(.NOT. ELAS2)
     &          DSPDEP(L+3,K+3) = DSPDEP(L+3,K+3) + MD2(L)*D2K(K)

C--------COUPLAGE M-F -----------------
              IF(.NOT. ELAS1) THEN
                DSPDEP(L,K+3) = DSPDEP(L,K+3) + SD1(L)*D1K(K)
                DSPDEP(L+3,K) = DSPDEP(L+3,K) + MD1(L)*D1E(K)
              ENDIF
              IF(.NOT. ELAS2) THEN
                DSPDEP(L,K+3) = DSPDEP(L,K+3) + SD2(L)*D2K(K)
                DSPDEP(L+3,K) = DSPDEP(L+3,K) + MD2(L)*D2E(K)
              ENDIF
 900        CONTINUE
 910      CONTINUE
        END IF

        CALL TANMGL(T,VMP,VFP,DSPDEP,DSIDEP)
      ENDIF
      END
