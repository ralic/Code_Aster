      SUBROUTINE EIBEX (FAMI,KPG,KSP,NDIM,IMATE,COMPOR,INSTAM,
     &                  INSTAP,EPSM,DEPS,VIM,OPTION,SIG,
     &                  VIP,DSIDEP,CODRET)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 02/05/2011   AUTEUR DELMAS J.DELMAS 
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
C TOLE CRP_7 TOLE CRP_21

      IMPLICIT NONE
      CHARACTER*16       COMPOR(*),OPTION
      CHARACTER*(*)      FAMI
      INTEGER            NDIM, IMATE, KSP, KPG
      REAL*8             EPSM(6),DEPS(6),VIM(2),INSTAP,INSTAM
      REAL*8             SIG(6), VIP(2), DSIDEP(6,6)
C ----------------------------------------------------------------------
C     LOI DE COMPORTEMENT ENDO_ISOT_BETON avec IMPLEX (EN LOCAL)
C
C IN  NDIM    : DIMENSION DE L'ESPACE
C IN  TYPMOD  : TYPE DE MODELISATION
C IN  IMATE   : NATURE DU MATERIAU
C IN  EPSM    : DEFORMATION EN T-
C IN  DEPS    : INCREMENT DE DEFORMATION
C IN  VIM     : VARIABLES INTERNES EN T-
C IN  CRIT    : CRITERES DE CONVERGENCE
C IN  OPTION  : OPTION DEMANDEE
C                 RIGI_MECA_IMPLEX -> SIG extr  DSIDEP
C                 RAPH_MECA      -> SIG        VIP
C OUT SIG     : CONTRAINTE
C OUT VIP     : VARIABLES INTERNES
C                 1   -> VALEUR DE L'ENDOMMAGEMENT
C                 2   -> DD/DT
C OUT DSIDEP  : MATRICE TANGENTE
C ----------------------------------------------------------------------
      LOGICAL     RAPH, TANG, COUP

      INTEGER     NDIMSI, K, L, I, J, M, N, T(3,3)
      INTEGER      CODRET,IRET, IISNAN

      REAL*8      EPS(6),  TREPS, SIGEL(6)
      REAL*8      RAC2
      REAL*8      RIGMIN, FD, DM, DP, DD, DDDT, DT, ENER
      REAL*8      TR(6), RTEMP2
      REAL*8      EPSP(3), VECP(3,3), DSPDEP(6,6)
      REAL*8      DEUMUD(3), LAMBDD, SIGP(3), RTEMP, RTEMP3, RTEMP4
      REAL*8      LAMBDA, DEUXMU, GAMMA
      REAL*8      SEUIL, DDOT
      REAL*8      TM,TP,TREF,SREF,SECHM,HYDRM,EPSTHE(2),KDESS,BENDO
      REAL*8       KRON(6), ALPHA,SECHP,HYDRP
      INTEGER CERR
      DATA        KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/
C      PARAMETER   (RIGMIN = 0.00001)
C ----------------------------------------------------------------------

      RIGMIN = 1.D-5


C     RECUPERATION DES VARIABLES DE COMMANDE
      CALL RCVARC(' ','TEMP','-'  ,FAMI,KPG,KSP,TM  ,IRET)
      CALL RCVARC(' ','TEMP','+'  ,FAMI,KPG,KSP,TP  ,IRET)
      CALL RCVARC(' ','TEMP','REF',FAMI,KPG,KSP,TREF,IRET)
      CALL RCVARC(' ','HYDR','-',FAMI,KPG,KSP,HYDRM,IRET)
      IF (IRET.NE.0) HYDRM=0.D0
      CALL RCVARC(' ','HYDR','+',FAMI,KPG,KSP,HYDRP,IRET)
      IF (IRET.NE.0) HYDRP=0.D0
      CALL RCVARC(' ','SECH','-',FAMI,KPG,KSP,SECHM,IRET)
      IF (IRET.NE.0) SECHM=0.D0
      CALL RCVARC(' ','SECH','+',FAMI,KPG,KSP,SECHP,IRET)
      IF (IRET.NE.0) SECHP=0.D0
      CALL RCVARC(' ','SECH','REF',FAMI,KPG,KSP,SREF,IRET)
      IF (IRET.NE.0) SREF=0.D0

C
C -- OPTION ET MODELISATION
C

      RAPH = OPTION .EQ. 'RAPH_MECA'
      TANG = OPTION .EQ. 'RIGI_MECA_IMPLEX'
      CODRET = 0
C      couplage fluage-endommagement non autorisé en impl-ex
      COUP = .FALSE.
C
C -- RECUPERATIOIN DES VARIABLES INTERNES
C
      DM   = VIM(1)
      DDDT = VIM(2)
      DT   = INSTAP - INSTAM
C
C -- INITIALISATION
C
      NDIMSI = 2*NDIM
      RAC2=SQRT(2.D0)

C      CALL CISOLI(FAMI,NDIM,IMATE,COMPOR,EPSM,
C     &            T,LAMBDA,DEUXMU,GAMMA,SEUIL)
      CALL LCEIB1 (FAMI,IMATE, COMPOR, NDIM, EPSM, TM,TREF,SREF,
     &             SECHM,HYDRM,T, LAMBDA, DEUXMU,EPSTHE, KDESS,
     &            BENDO, GAMMA, SEUIL,COUP)

C
C -- MAJ DES DEFORMATIONS ET PASSAGE AUX DEFORMATIONS REELLES 3D
C
      IF (TANG) THEN
        DO 40 K=1,NDIMSI
          EPS(K) = EPSM(K) - (  EPSTHE(1)
     &                       - KDESS * (SREF-SECHM)
     &                       - BENDO *  HYDRM  )     * KRON(K)
40      CONTINUE
      ELSEIF (RAPH) THEN
        IF (IISNAN(TP).EQ.0) THEN
          CALL RCVALB(FAMI,KPG,KSP,'+',IMATE,' ','ELAS',1,'TEMP',
     &               0.D0,1,'ALPHA',ALPHA,CERR, 0)
          IF ((IISNAN(TREF).EQ.1).OR.(CERR.NE.0))  THEN
            CALL U2MESS('F','CALCULEL_15')
          ELSE
            EPSTHE(2) =ALPHA * (TP - TREF)
          ENDIF
        ELSE
          EPSTHE(2) = 0.D0
        ENDIF

        DO 10 K = 1, NDIMSI
          EPS(K) = EPSM(K) + DEPS(K)
     &                   - KRON(K) *  (  EPSTHE(2)
     &                                 - KDESS * (SREF-SECHP)
     &                                 - BENDO *  HYDRP     )
10     CONTINUE
      ENDIF

      DO 45 K=4,NDIMSI
        EPS(K) = EPS(K)/RAC2
45    CONTINUE

      IF (NDIMSI.LT.6) THEN
        DO 46 K=NDIMSI+1,6
          EPS(K)=0.D0
46      CONTINUE
      ENDIF

C -- DIAGONALISATION DES DEFORMATIONS
      TR(1) = EPS(1)
      TR(2) = EPS(4)
      TR(3) = EPS(5)
      TR(4) = EPS(2)
      TR(5) = EPS(6)
      TR(6) = EPS(3)
      CALL DIAGP3(TR,VECP,EPSP)

C -- CALCUL DES CONTRAINTES ELAS POSITIVES ET DE L'ENERGIE POSITIVE

      TREPS = EPS(1)+EPS(2)+EPS(3)
      IF (TREPS.GT.0.D0) THEN
        DO 60 K=1,3
          SIGEL(K) = LAMBDA*TREPS
 60     CONTINUE
      ELSE
        DO 61 K=1,3
          SIGEL(K) = 0.D0
 61     CONTINUE
      ENDIF

      DO 15 K=1,3
        IF (EPSP(K).GT.0.D0) THEN
          SIGEL(K) = SIGEL(K) + DEUXMU*EPSP(K)
        ENDIF
15    CONTINUE
      ENER = 0.5D0 * DDOT(3,EPSP,1,SIGEL,1)

C ======================================================================
C     CAS RIGI_MECA_IMPLEX : EXTRAPOLATION VARIABLES INTERNES ET MATRICE
C ======================================================================
      IF (TANG) THEN

C -- EXTRAPOLATION ENDOMMAGEMENT
        DP = MIN(DM + DDDT*DT, 1.D0)
        FD  = (1 - DP) / (1 + GAMMA*DP)

C -- PARAMETRAGE POUR L EFFET UNILATERAL
        IF ((EPSP(1)+EPSP(2)+EPSP(3)).GT.0.D0) THEN
          LAMBDD=LAMBDA*MAX(FD,RIGMIN)
        ELSE
          LAMBDD=LAMBDA
        ENDIF

        IF (EPSP(1).GT.0.D0) THEN
          DEUMUD(1)=DEUXMU*MAX(FD,RIGMIN)
        ELSE
          DEUMUD(1)=DEUXMU
        ENDIF

        IF (EPSP(2).GT.0.D0) THEN
          DEUMUD(2)=DEUXMU*MAX(FD, RIGMIN)
        ELSE
          DEUMUD(2)=DEUXMU
        ENDIF

        IF (EPSP(3).GT.0.D0) THEN
          DEUMUD(3)=DEUXMU*MAX(FD, RIGMIN)
        ELSE
          DEUMUD(3)=DEUXMU
        ENDIF

C -- CALCUL DE LA MATRICE TANGENTE
        CALL R8INIR(36, 0.D0, DSPDEP, 1)
        CALL R8INIR(36, 0.D0, DSIDEP, 1)

        DO 100 K = 1,3
          DO 110 L = 1,3
            DSPDEP(K,L) = LAMBDD
 110      CONTINUE
 100    CONTINUE

        DO 120 K = 1,3
          DSPDEP(K,K) = DSPDEP(K,K) + DEUMUD(K)
 120    CONTINUE

        IF (EPSP(1)*EPSP(2).GE.0.D0) THEN
          DSPDEP(4,4)=DEUMUD(1)
        ELSE
          DSPDEP(4,4)=(DEUMUD(1)*EPSP(1)-DEUMUD(2)*EPSP(2))
     &                                    /(EPSP(1)-EPSP(2))
        ENDIF

        IF (EPSP(1)*EPSP(3).GE.0.D0) THEN
          DSPDEP(5,5)=DEUMUD(1)
        ELSE
          DSPDEP(5,5)=(DEUMUD(1)*EPSP(1)-DEUMUD(3)*EPSP(3))
     &                                    /(EPSP(1)-EPSP(3))
        ENDIF

        IF (EPSP(3)*EPSP(2).GE.0.D0) THEN
          DSPDEP(6,6)=DEUMUD(3)
        ELSE
          DSPDEP(6,6)=(DEUMUD(3)*EPSP(3)-DEUMUD(2)*EPSP(2))
     &                                    /(EPSP(3)-EPSP(2))
        ENDIF

        DO 20 I=1,3
          DO 21 J=I,3
            IF (I.EQ.J) THEN
              RTEMP3=1.D0
            ELSE
              RTEMP3=RAC2
            ENDIF

            DO 22 K=1,3
              DO 23 L=1,3
                IF (T(I,J).GE.T(K,L)) THEN
                  IF (K.EQ.L) THEN
                    RTEMP4=RTEMP3
                  ELSE
                    RTEMP4=RTEMP3/RAC2
                  ENDIF

                  RTEMP2=0.D0

                  DO 24 M=1,3
                    DO 25 N=1,3
                      RTEMP2=RTEMP2+VECP(K,M)*
     &                VECP(I,N)*VECP(J,N)*VECP(L,M)*DSPDEP(N,M)
25                  CONTINUE
24                CONTINUE

                  RTEMP2=RTEMP2+VECP(I,1)*VECP(J,2)*VECP(K,1)*VECP(L,2)
     &                   *DSPDEP(4,4)
                  RTEMP2=RTEMP2+VECP(I,2)*VECP(J,1)*VECP(K,2)*VECP(L,1)
     &                   *DSPDEP(4,4)
                  RTEMP2=RTEMP2+VECP(I,1)*VECP(J,3)*VECP(K,1)*VECP(L,3)
     &                   *DSPDEP(5,5)
                  RTEMP2=RTEMP2+VECP(I,3)*VECP(J,1)*VECP(K,3)*VECP(L,1)
     &                   *DSPDEP(5,5)
                  RTEMP2=RTEMP2+VECP(I,2)*VECP(J,3)*VECP(K,2)*VECP(L,3)
     &                   *DSPDEP(6,6)
                  RTEMP2=RTEMP2+VECP(I,3)*VECP(J,2)*VECP(K,3)*VECP(L,2)
     &                   *DSPDEP(6,6)
                  DSIDEP(T(I,J),T(K,L)) = DSIDEP(T(I,J),T(K,L))
     &                                  + RTEMP2*RTEMP4
                ENDIF
23            CONTINUE
22          CONTINUE
21        CONTINUE
20      CONTINUE

        DO 26 I=1,6
          DO 27 J=I+1,6
            DSIDEP(I,J)=DSIDEP(J,I)
27        CONTINUE
26      CONTINUE

C -- CALCUL DES CONTRAINTES
        TREPS=EPSP(1)+EPSP(2)+EPSP(3)
        SIGP(1)=LAMBDD*TREPS+DEUMUD(1)*EPSP(1)
        SIGP(2)=LAMBDD*TREPS+DEUMUD(2)*EPSP(2)
        SIGP(3)=LAMBDD*TREPS+DEUMUD(3)*EPSP(3)

        CALL R8INIR(6,0.D0,SIG,1)

        DO 180 I=1,3
          RTEMP=SIGP(I)
          SIG(1)=SIG(1)+VECP(1,I)*VECP(1,I)*RTEMP
          SIG(2)=SIG(2)+VECP(2,I)*VECP(2,I)*RTEMP
          SIG(3)=SIG(3)+VECP(3,I)*VECP(3,I)*RTEMP
          SIG(4)=SIG(4)+VECP(1,I)*VECP(2,I)*RTEMP
          SIG(5)=SIG(5)+VECP(1,I)*VECP(3,I)*RTEMP
          SIG(6)=SIG(6)+VECP(2,I)*VECP(3,I)*RTEMP
180    CONTINUE

        DO 190 K=4,NDIMSI
          SIG(K)=RAC2*SIG(K)
190     CONTINUE

C ======================================================================
C     CAS RAPH_MECA : CALCUL VARIABLES INTERNES ET CONTRAINTE
C ======================================================================
      ELSEIF (RAPH) THEN
C -- CALCUL (OU RECUPERATION) DE L'ENDOMMAGEMENT
        DP = (SQRT((1+GAMMA)/SEUIL * ENER) - 1) / GAMMA

        IF (DP.LT.VIM(1)) THEN
          DP = VIM(1)
        ELSE IF (DP .GT. 1) THEN
          DP = 1
        END IF

        FD  = (1 - DP) / (1 + GAMMA*DP)
        DD = DP - DM

        VIP(1) = DP
        VIP(2) = DD/DT

C -- CALCUL DES CONTRAINTES
        IF ((EPSP(1)+EPSP(2)+EPSP(3)).GT.0.D0) THEN
          LAMBDD=LAMBDA * FD
        ELSE
          LAMBDD=LAMBDA
        ENDIF

        IF (EPSP(1).GT.0.D0) THEN
          DEUMUD(1)=DEUXMU*FD
        ELSE
          DEUMUD(1)=DEUXMU
        ENDIF

        IF (EPSP(2).GT.0.D0) THEN
          DEUMUD(2)=DEUXMU*FD
        ELSE
          DEUMUD(2)=DEUXMU
        ENDIF

        IF (EPSP(3).GT.0.D0) THEN
          DEUMUD(3)=DEUXMU*FD
        ELSE
          DEUMUD(3)=DEUXMU
        ENDIF

        TREPS=EPSP(1)+EPSP(2)+EPSP(3)
        SIGP(1)=LAMBDD*TREPS+DEUMUD(1)*EPSP(1)
        SIGP(2)=LAMBDD*TREPS+DEUMUD(2)*EPSP(2)
        SIGP(3)=LAMBDD*TREPS+DEUMUD(3)*EPSP(3)

        CALL R8INIR(6,0.D0,SIG,1)

        DO 1010 I=1,3
          RTEMP=SIGP(I)
          SIG(1)=SIG(1)+VECP(1,I)*VECP(1,I)*RTEMP
          SIG(2)=SIG(2)+VECP(2,I)*VECP(2,I)*RTEMP
          SIG(3)=SIG(3)+VECP(3,I)*VECP(3,I)*RTEMP
          SIG(4)=SIG(4)+VECP(1,I)*VECP(2,I)*RTEMP
          SIG(5)=SIG(5)+VECP(1,I)*VECP(3,I)*RTEMP
          SIG(6)=SIG(6)+VECP(2,I)*VECP(3,I)*RTEMP
1010    CONTINUE

        DO 18 K=4,NDIMSI
          SIG(K)=RAC2*SIG(K)
18      CONTINUE
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF

      END
