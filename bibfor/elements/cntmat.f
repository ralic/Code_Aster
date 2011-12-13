      SUBROUTINE CNTMAT(T,LAMBDA,DEUXMU,LAMF,DEUMUF,ALF,EMP,
     &           EFP,VMP,VFP,TR2D,TROT,TREPS,GMT,GMC,GF,DA1,
     &           DA2,KSI2D,QFF,COF1,Q2D,DE33D1,
     &           DE33D2,ELAS,ELAS1,ELAS2,COUP,RIGI,RESI,OPTION,DSIDEP,
     &           SIG)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 01/03/2011   AUTEUR SFAYOLLE S.FAYOLLE 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C RESPONSABLE SFAYOLLE S.FAYOLLE
C TOLE CRP_21
      IMPLICIT NONE
C --  IN
      LOGICAL RESI, RIGI, COUP, ELAS, ELAS1, ELAS2

      REAL*8 LAMBDA, DEUXMU, LAMF, DEUMUF, ALF, TREPS, GMT, GMC, GF
      REAL*8 DA1, DA2, COF1, Q2D, DE33D1, DE33D2, KSI2D, TR2D
      REAL*8 TROT, DE33I
      REAL*8 EMP(2), EFP(2), VMP(2,2), VFP(2,2), QFF(2)
      CHARACTER*16 OPTION

C --  OUT
      REAL*8 SIG(6), DSIDEP(6,6)

C----------------------------------------------------------------------
C        CALCUL DES CONTRAINTES GENERALISEES ET DE LA MATRICE TANGENTE
C
C     IN :
C         LAMBDA  : PARAMETRE D ELASTICITE - MEMBRANE
C         MU      : PARAMETRE D ELASTICITE - MEMBRANE
C         LAMF    : PARAMETRE D ELASTICITE - FLEXION
C         DEUMUF  : PARAMETRE D ELASTICITE - FLEXION
C         ALF     : PARAMETRE DE SEUIL FLEXION
C         EMP(2)  : VALEURS PROPRES DE EPS 2D
C         EFP(2)  : VALEURS PROPRES DE KAPPA
C         VMP(2,2): VECTEURS PROPRES DE EPS 2D
C         VFP(2,2): VECTEURS PROPRES DE KAPPA
C         TR2D    : TRACE EPSILON 2D
C         TROT    : TRACE KAPPA 2D
C         TREPS   : TRACE 3D
C         GMT     : PARAMETRE GAMMA POUR LA MEMBRANE EN TRACTION 
C         GMC     : PARAMETRE GAMMA POUR LA MEMBRANE EN COMPRESSION
C         GF      : PARAMETRE GAMMA POUR LA FLEXION
C         DA1,DA2 : VALEURS D'ENDOMMAGEMENT
C         EPS33   : COMPOSANTE 33 DE LA DEFORMATION
C         KSI2D   : FONCTION CARACTERISTIQUE D ENDOMMAGEMENT
C         QFF(2)  : TF
C         COF1    : INTERMEDIAIRE DE CALCUL
C         Q2D     : INTERMEDIAIRE DE CALCUL
C         GI(2)   : PARTIE DE LA DERIVEE DE KSI(EMP) PAR RAPPORT A DA
C         DE33D1  : DERIVEE DE E33 PAR RAPPORT A DA1
C         DE33D2  : DERIVEE DE E33 PAR RAPPORT A DA2
C         ELAS    : .TRUE. SI ELASTIQUE
C         ELAS1   : .TRUE. SI DA1 == VIM(1)
C         ELAS2   : .TRUE. SI DA2 == VIM(2)
C         COUP    : OPTION
C         RIGI    : OPTION
C         RESI    : OPTION
C         OPTION  : TOUTES
C
C     OUT :
C          SIG(6)      : CONTRAINTES GENERALISEES DANS LE REPERE GLOBAL
C          DSIDEP(6,6) : MATRICE TANGENTE
C----------------------------------------------------------------------

      INTEGER K,L,I,T(2,2)

      REAL*8 LAMBDD, LAMFD,  DLMD1, DLMD2, DLMFD1, DLMFD2
      REAL*8 TREPS2, GF1, GF2, MU, FD1, FD2
      REAL*8 QM1, QM2, QME33
      REAL*8 A(2,2), AINV(2,2)
      REAL*8 DNDD(2,2), DMDD(2,2)
      REAL*8 DEUMUD(2), DEMUDF(2), D1MUD(2), D2MUD(2)
      REAL*8 D1MUDF(2), D2MUDF(2)
      REAL*8 FDI1(2), FDI2(2), SIGP(3), SIGF(3)
      REAL*8 DSPDEP(6,6)
      REAL*8 R8PREM, DETA

      MU  = DEUXMU * 0.5D0

C --  CALCUL DE GF1 ET GF2
C     ICI ON SUPPOSE QUE GF1=GF2, CE QUI N EST PAS NECESSAIRE
      GF1 = GF
      GF2 = GF

C --  CALCUL DES CONTRAINTES GENERALISEES

C --  CALCUL DE LA CONTRAINTE DE MEMBRANE SIGP
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
        SIGP(K) = LAMBDD*TREPS + DEUMUD(K)*EMP(K)
 80   CONTINUE

C --  CALCUL DE LA CONTRAINTE DE FLEXION SIGF
      IF(TROT .GT. 0.0D0) THEN
        LAMFD  =  LAMF*(ALF + GF2*DA2)/(ALF + DA2)
        DLMFD2 = -LAMF*ALF*(1.0D0 - GF2)/(ALF + DA2)**2
        DLMFD1 = 0.0D0
      ELSE
        LAMFD  =  LAMF*(ALF + GF1*DA1)/(ALF + DA1)
        DLMFD1 = -LAMF*ALF*(1.0D0 - GF1)/(ALF + DA1)**2
        DLMFD2 = 0.0D0
      ENDIF

      DO 90, K = 1,2
        IF(EFP(K) .GT. 0.0D0) THEN
          DEMUDF(K) =  DEUMUF*(ALF + GF2*DA2)/(ALF + DA2)
          D2MUDF(K) = -DEUMUF*ALF*(1.0D0 - GF2)/(ALF + DA2)**2
          D1MUDF(K) =  0.0D0
        ELSE
          DEMUDF(K) =  DEUMUF*(ALF + GF1*DA1)/(ALF + DA1)
          D1MUDF(K) = -DEUMUF*ALF*(1.0D0 - GF1)/(ALF + DA1)**2
          D2MUDF(K) =  0.0D0
        ENDIF
        SIGF(K) = LAMFD*TROT + DEMUDF(K)*EFP(K)
 90   CONTINUE

      IF (RESI .AND.(.NOT.COUP)) THEN
        CALL R8INIR(6,0.D0,SIG,1)
        DO 1010 I=1,2
          SIG(1)=SIG(1)+VMP(1,I)**2*SIGP(I)
          SIG(2)=SIG(2)+VMP(2,I)**2*SIGP(I)
          SIG(3)=SIG(3)+VMP(1,I)*VMP(2,I)*SIGP(I)

          SIG(4)=SIG(4)+VFP(1,I)**2*SIGF(I)
          SIG(5)=SIG(5)+VFP(2,I)**2*SIGF(I)
          SIG(6)=SIG(6)+VFP(1,I)*VFP(2,I)*SIGF(I)
1010    CONTINUE
      ENDIF

C -- CALCUL DE LA MATRICE TANGENTE
      IF (RIGI) THEN
        IF (OPTION(11:14).EQ.'ELAS') THEN
          ELAS  =.TRUE.
          ELAS1 =.TRUE.
          ELAS2 =.TRUE.
        ENDIF

C --  CALCUL DE LA PARTIE ELASTIQUE
        CALL R8INIR(36, 0.D0, DSPDEP, 1)

        IF (COUP) THEN
          CALL R8INIR(72, 0.D0, DSIDEP, 1)
        ELSE
          CALL R8INIR(36, 0.D0, DSIDEP, 1)
        ENDIF

        DE33I = -LAMBDA*KSI2D/(DEUXMU + LAMBDA*KSI2D)

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

        IF (ABS(EMP(1) - EMP(2)) .LE. R8PREM()) THEN
          DSPDEP(3,3)=DEUMUD(1)
        ELSE
          DSPDEP(3,3)=(DEUMUD(1)*EMP(1)-DEUMUD(2)*EMP(2))
     &                                    /(EMP(1)-EMP(2))
        ENDIF

        IF (ABS(EFP(1) - EFP(2)) .LE. R8PREM()) THEN
          DSPDEP(6,6)=DEMUDF(1)
        ELSE
          DSPDEP(6,6)=(DEMUDF(1)*EFP(1)-DEMUDF(2)*EFP(2))
     &                                    /(EFP(1)-EFP(2))
        ENDIF

C -- CONTRIBUTION DISSIPATIVE
        IF (.NOT. ELAS) THEN

          TREPS2 = TREPS*TREPS
          QM1 = 0.5D0*COF1*TREPS2+Q2D
          QM2 = QM1
          QME33 = COF1*TREPS

C --  CALCUL DE LA DERIVEE DES CONTRAINTES MEMBRANES PAR RAPPORT A DA

          DNDD(1,1) = DLMD1*TREPS+D1MUD(1)*EMP(1)+LAMBDA*KSI2D*DE33D1
          DNDD(2,1) = DLMD1*TREPS+D1MUD(2)*EMP(2)+LAMBDA*KSI2D*DE33D1
          DNDD(1,2) = DLMD2*TREPS+D2MUD(1)*EMP(1)+LAMBDA*KSI2D*DE33D2
          DNDD(2,2) = DLMD2*TREPS+D2MUD(2)*EMP(2)+LAMBDA*KSI2D*DE33D2

C --  CALCUL DE LA DERIVEE DES CONTRAINTES FLEXION PAR RAPPORT A DA

          DMDD(1,1) = DLMFD1*TROT + D1MUDF(1) * EFP(1)
          DMDD(2,1) = DLMFD1*TROT + D1MUDF(2) * EFP(2)
          DMDD(1,2) = DLMFD2*TROT + D2MUDF(1) * EFP(1)
          DMDD(2,2) = DLMFD2*TROT + D2MUDF(2) * EFP(2)

          IF((.NOT. ELAS1).AND.(.NOT. ELAS2))THEN

            A(1,1) = 2.0D0*(QM1/(1.0D0+DA1)**3 + QFF(1)/(ALF+DA1)**3)
     &             - QME33*DE33D1/(1.0D0+DA1)**2
            A(1,2) = -QME33*DE33D2/(1.0D0+DA1)**2

            A(2,2) = 2.0D0*(QM2/(1.0D0+DA2)**3 + QFF(2)/(ALF+DA2)**3)
     &             - QME33*DE33D2/(1.0D0+DA2)**2
            A(2,1) = -QME33*DE33D1/(1.0D0 + DA2)**2

            CALL MATINV('S',2,A,AINV,DETA)

            DO 930, I=1,2
              DO 931, K=1,2
                DO 932, L=1,2
                  DSPDEP(L,K) = DSPDEP(L,K)
     &            -DNDD(L,I)*(DNDD(K,1)*AINV(I,1)+DNDD(K,2)*AINV(I,2))

                  DSPDEP(L+3,K+3) = DSPDEP(L+3,K+3)
     &            -DMDD(L,I)*(DMDD(K,1)*AINV(I,1)+DMDD(K,2)*AINV(I,2))

                  DSPDEP(L+3,K) = DSPDEP(L+3,K)
     &           - DMDD(L,I)*(DNDD(K,1)*AINV(I,1)+DNDD(K,2)*AINV(I,2))

                  DSPDEP(L,K+3) = DSPDEP(L,K+3)
     &           - DNDD(L,I)*(DMDD(K,1)*AINV(I,1)+DMDD(K,2)*AINV(I,2))
932             CONTINUE
931           CONTINUE
930         CONTINUE

          ELSEIF(.NOT. ELAS1)THEN

            A(1,1) = 2.0D0*(QM1/(1.0D0+DA1)**3 + QFF(1)/(ALF+DA1)**3)
     &             - QME33*DE33D1/(1.0D0+DA1)**2

            AINV(1,1)=1.D0/A(1,1)

            DO 950, K=1,2
              DO 951, L=1,2
                DSPDEP(L,K)=DSPDEP(L,K)-DNDD(L,1)*DNDD(K,1)*AINV(1,1)

                DSPDEP(L+3,K+3)=DSPDEP(L+3,K+3)-DMDD(L,1)*DMDD(K,1)*
     &                          AINV(1,1)

                DSPDEP(L+3,K) = DSPDEP(L+3,K) - DMDD(L,1)*DNDD(K,1)*
     &                          AINV(1,1)

                DSPDEP(L,K+3) = DSPDEP(L,K+3) - DNDD(L,1)*DMDD(K,1)*
     &                          AINV(1,1)
951           CONTINUE
950         CONTINUE

          ELSEIF(.NOT. ELAS2) THEN

            A(2,2) = 2.0D0*(QM2/(1.0D0+DA2)**3 + QFF(2)/(ALF+DA2)**3)
     &             - QME33*DE33D2/(1.0D0+DA2)**2

            AINV(2,2)=1.D0/A(2,2)

            DO 960, K=1,2
              DO 961, L=1,2
                DSPDEP(L,K)=DSPDEP(L,K)-DNDD(L,2)*DNDD(K,2)*AINV(2,2)

                DSPDEP(L+3,K+3) = DSPDEP(L+3,K+3)-DMDD(L,2)*DMDD(K,2)*
     &                            AINV(2,2)

                DSPDEP(L+3,K) = DSPDEP(L+3,K) - DMDD(L,2)*DNDD(K,2)*
     &                          AINV(2,2)

                DSPDEP(L,K+3) = DSPDEP(L,K+3) - DNDD(L,2)*DMDD(K,2)*
     &                          AINV(2,2)
961           CONTINUE
960         CONTINUE
          ENDIF
        ENDIF
        CALL TANMGL(T,VMP,VFP,DSPDEP,DSIDEP)
      ENDIF
      END
