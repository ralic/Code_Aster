      SUBROUTINE LCLDSB (FAMI,KPG,KSP,NDIM, TYPMOD,IMATE,COMPOR,EPSM,
     &              DEPS,VIM,TM,TP,TREF,OPTION,SIG,VIP,DSIDEP,CRIT)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C ======================================================================
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*8        TYPMOD(*)
      CHARACTER*16       COMPOR(*),OPTION
      CHARACTER*(*)      FAMI
      INTEGER            NDIM, IMATE, KSP, KPG
      REAL*8             EPSM(6),DEPS(6),VIM(*),TP,TM,TREF,CRIT(*)
      REAL*8             SIG(6), VIP(*), DSIDEP(6,12)
C ----------------------------------------------------------------------
C     LOI DE COMPORTEMENT ENDO_ISOT_BETON (EN LOCAL)
C
C IN  NDIM    : DIMENSION DE L'ESPACE
C IN  TYPMOD  : TYPE DE MODELISATION
C IN  IMATE   : NATURE DU MATERIAU
C IN  EPSM    : DEFORMATION EN T-
C IN  DEPS    : INCREMENT DE DEFORMATION
C IN  VIM     : VARIABLES INTERNES EN T-
C IN  CRIT    : CRITERES DE CONVERGENCE
C IN  OPTION  : OPTION DEMANDEE
C                 RIGI_MECA_TANG ->     DSIDEP
C                 FULL_MECA      -> SIG DSIDEP VIP
C                 RAPH_MECA      -> SIG        VIP
C OUT SIG     : CONTRAINTE
C OUT VIP     : VARIABLES INTERNES
C                 1   -> VALEUR DE L'ENDOMMAGEMENT
C OUT DSIDEP  : MATRICE TANGENTE
C ----------------------------------------------------------------------
C LOC EDFRC1  COMMON CARACTERISTIQUES DU MATERIAU (AFFECTE DANS EDFRMA)
      LOGICAL     RIGI, RESI,ELAS, COUP
      INTEGER     NDIMSI,K,L,I,J,M,N,T(3,3),IRET,ITERAT
      REAL*8      EPS(6),  TREPS, SIGEL(6), KRON(6)
      REAL*8      RAC2,COEF,COEF2
      REAL*8      RIGMIN, FD, D, ENER
      REAL*8      TR(6), RTEMP2, EPSTHE(2)
      REAL*8      EPSP(3), VECP(3,3), DSPDEP(6,6)
      REAL*8      DEUMUD(3), LAMBDD, SIGP(3),RTEMP,RTEMP3,RTEMP4
      REAL*8      KDESS, BENDO, LAMBDA, DEUXMU, GAMMA
      REAL*8      SEUIL
      REAL*8      DDOT,TSEUIL,TSAMPL,TSRETU
      REAL*8      HYDRM,HYDRP,SECHM,SECHP,SREF
      PARAMETER  (RIGMIN = 1.D-5)
      DATA        KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/

C ----------------------------------------------------------------------

C -- OPTION ET MODELISATION

      RIGI  = (OPTION(1:4).EQ.'RIGI' .OR. OPTION(1:4).EQ.'FULL')
      RESI  = (OPTION(1:4).EQ.'RAPH' .OR. OPTION(1:4).EQ.'FULL')
      COUP  = (OPTION(6:9).EQ.'COUP')
      IF (COUP) RIGI=.TRUE.
      NDIMSI = 2*NDIM
      RAC2=SQRT(2.D0)
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

C -- INITIALISATION

      TSEUIL = CRIT(10)
      TSAMPL = CRIT(11)
      TSRETU = CRIT(12)

      CALL LCEIB1 (FAMI,KPG,KSP,IMATE,COMPOR,NDIM,EPSM,
     &             SREF,SECHM,HYDRM,T, LAMBDA,DEUXMU,EPSTHE,
     &             KDESS,BENDO,GAMMA,SEUIL)

C -- MAJ DES DEFORMATIONS ET PASSAGE AUX DEFORMATIONS REELLES 3D

      IF (RESI) THEN
        DO 10 K = 1, NDIMSI
          EPS(K) = EPSM(K) + DEPS(K)
     &                   - KRON(K) *  (  EPSTHE(2)
     &                                 - KDESS * (SREF-SECHP)
     &                                 - BENDO *  HYDRP     )
 10     CONTINUE
      ELSE
        DO 40 K=1,NDIMSI
          EPS(K) = EPSM(K) - (  EPSTHE(1)
     &                       - KDESS * (SREF-SECHM)
     &                       - BENDO *  HYDRM  )     * KRON(K)
40      CONTINUE
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

C -- CALCUL (OU RECUPERATION) DE L'ENDOMMAGEMENT

      IF (RESI) THEN
        ELAS = .FALSE.
        D = (SQRT((1+GAMMA)/SEUIL * ENER) - 1) / GAMMA
        IF (D.LT.VIM(1)) THEN
          D = VIM(1)
          ELAS = .TRUE.
        ELSE IF (D .GT. 1) THEN
          D = 1
          ELAS = .TRUE.
        END IF
        FD  = (1 - D) / (1 + GAMMA*D)
        VIP(1) = D
        IF (ELAS) THEN
          VIP(2) = 0
          IF (FD.LE.RIGMIN) VIP(2) = 2
        ELSE
          VIP(2) = 1
        END IF
      ELSE
        D=VIM(1)
        FD  = (1 - D) / (1 + GAMMA*D)
        ELAS=((NINT(VIM(2)).EQ.0).OR.(NINT(VIM(2)).EQ.2))
      ENDIF


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


      IF (RESI.AND.(.NOT.COUP)) THEN
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
      ENDIF

C -- CALCUL DE LA MATRICE TANGENTE

C----EVOLUTION DES PARALETRES DE CONTROLANT LA MATRICE TANGENTE/SECANTE
      IF(TSEUIL .GE. 0.0D0) THEN
        CALL JEVECH('PITERAT','L',ITERAT)
        ITERAT = NINT(ZR(ITERAT))
        IF ((OPTION(1:4) .EQ. 'RIGI') .OR. (ITERAT .LE. 1)) THEN
          VIP(3) = 0.0D0
        ELSE
          CALL EVOLTS(TSEUIL,TSRETU,VIP(2),VIP(3),ITERAT)
        ENDIF

      ENDIF
C-----------------------------------------------------------
      IF (RIGI) THEN
        IF (OPTION(11:14).EQ.'ELAS') ELAS=.TRUE.
        CALL R8INIR(36, 0.D0, DSPDEP, 1)
        IF (COUP) THEN
          CALL R8INIR(72, 0.D0, DSIDEP, 1)
        ELSE
          CALL R8INIR(36,0.D0,DSIDEP,1)
        ENDIF
        IF (FD.LT.RIGMIN) THEN
          IF ((EPSP(1)+EPSP(2)+EPSP(3)).GT.0.D0) THEN
            LAMBDD=LAMBDA * RIGMIN
          ENDIF
          IF (EPSP(1).GT.0.D0) THEN
            DEUMUD(1)=DEUXMU*RIGMIN
          ENDIF
          IF (EPSP(2).GT.0.D0) THEN
            DEUMUD(2)=DEUXMU*RIGMIN
          ENDIF
          IF (EPSP(3).GT.0.D0) THEN
            DEUMUD(3)=DEUXMU*RIGMIN
          ENDIF
        ENDIF
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
     &        VECP(I,N)*VECP(J,N)*VECP(L,M)*DSPDEP(N,M)
25                CONTINUE
24              CONTINUE
       RTEMP2=RTEMP2+VECP(I,1)*VECP(J,2)*VECP(K,1)*VECP(L,2)*DSPDEP(4,4)
       RTEMP2=RTEMP2+VECP(I,2)*VECP(J,1)*VECP(K,2)*VECP(L,1)*DSPDEP(4,4)
       RTEMP2=RTEMP2+VECP(I,1)*VECP(J,3)*VECP(K,1)*VECP(L,3)*DSPDEP(5,5)
       RTEMP2=RTEMP2+VECP(I,3)*VECP(J,1)*VECP(K,3)*VECP(L,1)*DSPDEP(5,5)
       RTEMP2=RTEMP2+VECP(I,2)*VECP(J,3)*VECP(K,2)*VECP(L,3)*DSPDEP(6,6)
       RTEMP2=RTEMP2+VECP(I,3)*VECP(J,2)*VECP(K,3)*VECP(L,2)*DSPDEP(6,6)
              DSIDEP(T(I,J),T(K,L))=DSIDEP(T(I,J),T(K,L))+RTEMP2*RTEMP4
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
C -- CONTRIBUTION DISSIPATIVE
        IF ((.NOT. ELAS).AND.(ENER.GT.0.D0)) THEN
          IF (COUP) THEN
            COEF = SQRT((1+GAMMA)/(2*GAMMA*(1+GAMMA*D)*ENER))
            IF ((EPSP(1)+EPSP(2)+EPSP(3)).GT.0.D0) THEN
              LAMBDD=LAMBDA * COEF
            ELSE
              LAMBDD=0.D0
            ENDIF
            IF (EPSP(1).GT.0.D0) THEN
              DEUMUD(1)=DEUXMU*COEF
            ELSE
              DEUMUD(1)=0.D0
            ENDIF
            IF (EPSP(2).GT.0.D0) THEN
              DEUMUD(2)=DEUXMU*COEF
            ELSE
              DEUMUD(2)=0.D0
            ENDIF
            IF (EPSP(3).GT.0.D0) THEN
              DEUMUD(3)=DEUXMU*COEF
            ELSE
              DEUMUD(3)=0.D0
            ENDIF
            DO 500 K = 1,3
              DO 510 L = 1,3
                DSPDEP(K,L) = LAMBDD
 510          CONTINUE
 500        CONTINUE
            DO 515 K = 1,3
              DSPDEP(K,K) = DSPDEP(K,K) + DEUMUD(K)
 515        CONTINUE
            IF (EPSP(1)*EPSP(2).GE.0.D0) THEN
              DSPDEP(4,4)=DEUMUD(1)
            ELSE
              DSPDEP(4,4)=(DEUMUD(1)*EPSP(1)-DEUMUD(2)*EPSP(2))
     &                                        /(EPSP(1)-EPSP(2))
            ENDIF
            IF (EPSP(1)*EPSP(3).GE.0.D0) THEN
              DSPDEP(5,5)=DEUMUD(1)
            ELSE
              DSPDEP(5,5)=(DEUMUD(1)*EPSP(1)-DEUMUD(3)*EPSP(3))
     &                                        /(EPSP(1)-EPSP(3))
            ENDIF
            IF (EPSP(3)*EPSP(2).GE.0.D0) THEN
              DSPDEP(6,6)=DEUMUD(3)
            ELSE
              DSPDEP(6,6)=(DEUMUD(3)*EPSP(3)-DEUMUD(2)*EPSP(2))
     &                                        /(EPSP(3)-EPSP(2))
            ENDIF
            DO 520 I=1,3
              DO 521 J=I,3
                IF (I.EQ.J) THEN
                  RTEMP3=1.D0
                ELSE
                  RTEMP3=RAC2
                ENDIF
                DO 522 K=1,3
                  DO 523 L=1,3
                    IF (T(I,J).GE.T(K,L)) THEN
                    IF (K.EQ.L) THEN
                      RTEMP4=RTEMP3
                    ELSE
                      RTEMP4=RTEMP3/RAC2
                    ENDIF
                    RTEMP2=0.D0
                    DO 524 M=1,3
                      DO 525 N=1,3
            RTEMP2=RTEMP2+VECP(K,M)*
     &            VECP(I,N)*VECP(J,N)*VECP(L,M)*DSPDEP(N,M)
525                    CONTINUE
524                  CONTINUE
      RTEMP2=RTEMP2+VECP(I,1)*VECP(J,2)*VECP(K,1)*VECP(L,2)*DSPDEP(4,4)
      RTEMP2=RTEMP2+VECP(I,2)*VECP(J,1)*VECP(K,2)*VECP(L,1)*DSPDEP(4,4)
      RTEMP2=RTEMP2+VECP(I,1)*VECP(J,3)*VECP(K,1)*VECP(L,3)*DSPDEP(5,5)
      RTEMP2=RTEMP2+VECP(I,3)*VECP(J,1)*VECP(K,3)*VECP(L,1)*DSPDEP(5,5)
      RTEMP2=RTEMP2+VECP(I,2)*VECP(J,3)*VECP(K,2)*VECP(L,3)*DSPDEP(6,6)
      RTEMP2=RTEMP2+VECP(I,3)*VECP(J,2)*VECP(K,3)*VECP(L,2)*DSPDEP(6,6)
      DSIDEP(T(I,J),T(K,L)+6)=DSIDEP(T(I,J),T(K,L)+6)+RTEMP2*RTEMP4
                   ENDIF
523                CONTINUE
522              CONTINUE
521            CONTINUE
520          CONTINUE

            DO 526 I=1,6
              DO 527 J=I+1,6
                DSIDEP(I,J+6)=DSIDEP(J,I+6)
527            CONTINUE
526          CONTINUE

          ELSE
          TR(1) = SIGEL(1)
          TR(2) = SIGEL(2)
          TR(3) = SIGEL(3)
          CALL R8INIR(6,0.D0,SIGEL,1)
          DO 1020 I=1,3
            RTEMP=TR(I)
            SIGEL(1)=SIGEL(1)+VECP(1,I)*VECP(1,I)*RTEMP
            SIGEL(2)=SIGEL(2)+VECP(2,I)*VECP(2,I)*RTEMP
            SIGEL(3)=SIGEL(3)+VECP(3,I)*VECP(3,I)*RTEMP
            SIGEL(4)=SIGEL(4)+VECP(1,I)*VECP(2,I)*RTEMP
            SIGEL(5)=SIGEL(5)+VECP(1,I)*VECP(3,I)*RTEMP
            SIGEL(6)=SIGEL(6)+VECP(2,I)*VECP(3,I)*RTEMP
1020      CONTINUE
          DO 28 K=4,NDIMSI
            SIGEL(K)=RAC2*SIGEL(K)
28        CONTINUE
          COEF = (1+GAMMA)/(2*GAMMA*(1+GAMMA*D)*ENER)

C CALCUL DE LA MATRICE EVOLUTIVE TANGENTE/SECANTE
          IF(TSEUIL .GT. 0.0D0) THEN
            IF(ABS(VIP(3)) .GT. TSEUIL) THEN
              COEF2 = COEF/(TSAMPL**(ABS(VIP(3)) - TSEUIL))
              IF (ABS(COEF2) .LT. ABS(COEF)) THEN
                COEF = COEF2
              ENDIF
            ENDIF
          ENDIF

          DO 200 K = 1,NDIMSI
            DO 210 L = 1, NDIMSI
              DSIDEP(K,L) = DSIDEP(K,L) - COEF * SIGEL(K) * SIGEL(L)
 210        CONTINUE
 200      CONTINUE
          ENDIF
        END IF
      END IF
      END
