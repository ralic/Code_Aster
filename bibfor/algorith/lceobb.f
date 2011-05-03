       SUBROUTINE LCEOBB (INTMAX,TOLER,EPSM,DEPS,BM,DM,
     &                     LAMBDA,MU,ALPHA,ECROB,
     &                     ECROD,RK,RK1,RK2,B,D,MULT,ELAS,DBLOQ,
     &                     IRET)

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

      IMPLICIT NONE
      REAL*8             EPSM(6),DEPS(6)
      REAL*8             BM(6),DM,B(6),D,MULT
      REAL*8             LAMBDA,MU,ALPHA,RK,RK1,RK2,ECROB,ECROD
      REAL*8             TOLER
      INTEGER            INTMAX,IRET
      LOGICAL            ELAS,DBLOQ

C ----------------------------------------------------------------------
C     LOI DE COMPORTEMENT DU MODELE D'ENDOMMAGEMENT ANISOTROPE
C     ROUTINE DE DECOUPAGE DE L'INCREMENT DE CHARGE LORSQUE
C     L ENDOMMAGEMENT APPROCHE DE 1
C
C  IN INTMAX  : NBRE D'ITERATION MAX POUR LE NEWTON LOCAL
C  IN TOLER   : RESIDU TOLERE POUR LE NEWTON LOCAL
C  IN  NDIM    : DIMENSION DE L'ESPACE
C  IN  TYPMOD  : TYPE DE MODELISATION
C  IN  IMATE   : NATURE DU MATERIAU
C  IN  CRIT   : CRITERES DE CONVERGENCE LOCAUX
C  IN  EPSM    : DEFORMATION EN T- REPERE GLOBAL
C  IN  DEPS    : INCREMENT DE DEFORMATION
C  IN  BM DM     : VARIABLES INTERNES EN T-
C  IN LAMBDA     : /
C  IN MU        : / COEFFICIENTS DE LAME
C  IN  ALPHA    : /
C  IN  ECROB    : /
C  IN  ECROD    : /
C  IN  RK    : /
C  IN  RK1    : /
C  IN  RK2    : / PARAMETRES DU MODELE
C
C OUT  B D     : VARIABLES INTERNES EN T+
C OUT MULT     : MULTIPLICATEUR PLASTIQUE DU PRINCIPE DE NORMALITE
C OUT ELAS     : ELASTIQUE OU DISSIPATION?
C OUT DBLOQ  : BLOQUAGE DE L'ENDOMMAGEMENT DE COMPRESSION
C OUT IRET     : CODE RETOUR
C ----------------------------------------------------------------------

      LOGICAL     REINIT,TOT1,TOT2,TOT3
      INTEGER     I,J,K,L
      INTEGER     BDIM,COMPTE,T(3,3)

      REAL*8      TOLB,UN,R8PREM,DEUX
      REAL*8      VALBM(3),VECBM(3,3),VALBR(3),VECBR(3,3)
      REAL*8      VALB(3),VECB(3,3),SEUIL
      REAL*8      BMR(6),BR(6),EPSR(6)
      REAL*8      INTERM(3,3),BINTER(6),EPI(6)
      REAL*8      EPSI(6),EPST(6),EPSF(6),TREPSM

      UN=1.D0
      DEUX=2.D0
      T(1,1)=1
      T(1,2)=4
      T(1,3)=5
      T(2,1)=4
      T(2,2)=2
      T(2,3)=6
      T(3,1)=5
      T(3,2)=6
      T(3,3)=3

      TOLB=1.D-2
      COMPTE=0
C-------------------------------------------------
C -- DEFORMATIONS
C-------------------------------------------------
      CALL R8INIR(6,0.D0,EPSI,1)
      CALL R8INIR(6,0.D0,EPSF,1)
      CALL R8INIR(6,0.D0,EPST,1)

      DO 1 K = 1,6
        EPSF(K) = EPSM(K)+DEPS(K)
        EPSI(K) = EPSM(K)
        EPST(K) = (EPSF(K)+EPSI(K))/DEUX
 1    CONTINUE

      REINIT=.FALSE.

 999  CONTINUE
        IF ((((EPSI(1).NE.EPSF(1)).OR.(EPSI(2).NE.EPSF(2)).OR.
     &      (EPSI(3).NE.EPSF(3)).OR.(EPSI(4).NE.EPSF(4)).OR.
     &      (EPSI(5).NE.EPSF(5)).OR.(EPSI(6).NE.EPSF(6))).OR.
     &      REINIT).AND.(COMPTE.LE.100)) THEN

            REINIT=.FALSE.
            COMPTE=COMPTE+1

              IF (COMPTE.EQ.100) THEN
                IRET=0
                GOTO 9999
              ENDIF

            CALL DIAGO3(BM,VECBM,VALBM)
            BDIM=3
            DO 201 I=1,3
              IF (VALBM(I)-TOLB.LE.0.D0) THEN
                BDIM=BDIM-1
              ENDIF
 201        CONTINUE

            TREPSM=EPST(1)+EPST(2)+EPST(3)
            IF (TREPSM.GT.0.D0) THEN
              TREPSM=0.D0
            ENDIF

            SEUIL=RK-RK1*TREPSM*(ATAN2(-TREPSM/RK2,UN))

C----CAS OU LES 3 VALEURS PROPRES SONT NON NULLES---------------------
            IF (BDIM.EQ.3) THEN

              CALL LCEOB3(INTMAX,TOLER,EPST,BM,DM,
     &                    LAMBDA,MU,ALPHA,ECROB,ECROD,
     &                    SEUIL,BDIM,B,D,MULT,ELAS,DBLOQ,IRET)

              CALL DIAGO3(B,VECB,VALB)
               REINIT=.FALSE.
              IF (COMPTE.LT.100) THEN
                DO 101 I=1,3
                  IF ((VALB(I).LT.0).OR.(D.GT.1.D0)) THEN
                    REINIT=.TRUE.
                  ELSE
                    IF (VALB(I)-TOLB.LE.0.D0) THEN
                      VALB(I)=TOLB-R8PREM()
                    ENDIF
                    IF (UN-D-TOLB.LE.0.D0) THEN
                      D=UN-TOLB+R8PREM()
                      DBLOQ=.TRUE.
                    ENDIF
                  ENDIF
 101            CONTINUE

                IF (REINIT) THEN
                  DO 800 I=1,6
                    EPST(I)=(EPST(I)+EPSI(I))/DEUX
 800              CONTINUE
                  GOTO 999
                ELSE
                  CALL R8INIR(6,0.D0,B,1)
                  CALL R8INIR(6,0.D0,BM,1)
                  DO 212 I=1,3
                    DO 213 J=I,3
                      DO 214 K=1,3
                      B(T(I,J))=B(T(I,J))+VECB(I,K)*VALB(K)*VECB(J,K)
                      BM(T(I,J))=BM(T(I,J))+VECB(I,K)*VALB(K)*VECB(J,K)
 214                  CONTINUE
 213                CONTINUE
 212              CONTINUE
                  DM=D
                  DO 801 I=1,6
                    EPSI(I)=EPST(I)
                    EPST(I)=EPSF(I)
 801              CONTINUE
                  GOTO 999
                ENDIF
              ELSE
                DO 701 I=1,3
                  IF ((VALB(I).LT.0).AND.(ABS(VALB(I))-TOLB.LE.
     &            0.D0)) THEN
                    VALB(I)=TOLB-R8PREM()
                  ENDIF
 701            CONTINUE
                IF (ABS(UN-D)-TOLB.LE.0.D0) THEN
                  D=UN-TOLB+R8PREM()
                            DBLOQ=.TRUE.
                ENDIF
                CALL R8INIR(6,0.D0,B,1)
                CALL R8INIR(6,0.D0,BM,1)
                DO 712 I=1,3
                  DO 713 J=I,3
                    DO 714 K=1,3
                     B(T(I,J))=B(T(I,J))+VECB(I,K)*VALB(K)*VECB(J,K)
                     BM(T(I,J))=BM(T(I,J))+VECB(I,K)*VALB(K)*VECB(J,K)
 714                CONTINUE
 713              CONTINUE
 712            CONTINUE
                DM=D
                DO 901 I=1,6
                  EPSI(I)=EPST(I)
                  EPST(I)=EPSF(I)
 901            CONTINUE

              ENDIF

C----CAS OU 1 VALEUR PROPRE EST NULLE---------------------------------

            ELSEIF (BDIM.EQ.2) THEN

              CALL R8INIR(9,0.D0,INTERM,1)
              CALL R8INIR(6,0.D0,EPI,1)
              DO 202 I=1,3
                DO 203 L=1,3
                  DO 204 K=1,3
                    INTERM(I,L)=INTERM(I,L)+VECBM(K,I)*EPST(T(K,L))
 204              CONTINUE
                DO 205 J=I,3
                EPI(T(I,J))=EPI(T(I,J))+INTERM(I,L)*VECBM(L,J)
 205            CONTINUE
 203          CONTINUE
 202        CONTINUE
            TOT1=.FALSE.
            TOT2=.FALSE.
            TOT3=.FALSE.
            CALL R8INIR(6,0.D0,BMR,1)
            IF (VALBM(1)-TOLB.LE.0.D0) THEN

              BMR(1)=VALBM(2)
              BMR(2)=VALBM(3)
              EPSR(1)=EPI(2)
              EPSR(2)=EPI(3)
              EPSR(3)=EPI(1)
              EPSR(4)=EPI(6)
              EPSR(5)=EPI(4)
              EPSR(6)=EPI(5)
              TOT1=.TRUE.
            ELSEIF (VALBM(2)-TOLB.LE.0.D0) THEN

              BMR(1)=VALBM(3)
              BMR(2)=VALBM(1)
              EPSR(1)=EPI(3)
              EPSR(2)=EPI(1)
              EPSR(3)=EPI(2)
              EPSR(4)=EPI(5)
              EPSR(5)=EPI(6)
              EPSR(6)=EPI(4)
              TOT2=.TRUE.

            ELSEIF (VALBM(3)-TOLB.LE.0.D0) THEN

              BMR(1)=VALBM(1)
              BMR(2)=VALBM(2)
              EPSR(1)=EPI(1)
              EPSR(2)=EPI(2)
              EPSR(3)=EPI(3)
              EPSR(4)=EPI(4)
              EPSR(5)=EPI(5)
              EPSR(6)=EPI(6)
              TOT3=.TRUE.

            ENDIF

            CALL LCEOB2(INTMAX,TOLER,EPSR,BMR,DM,
     &                 LAMBDA,MU,ALPHA,ECROB,ECROD,
     &                 SEUIL,BDIM,BR,D,MULT,ELAS,DBLOQ,IRET)

            CALL DIAGO3(BR,VECBR,VALBR)

            IF (COMPTE.LT.100) THEN

            DO 102 I=1,2
              IF (VALBR(I).LT.0) THEN
                REINIT=.TRUE.
              ENDIF
              IF (VALBR(I)-TOLB.LE.0.D0) THEN
                VALBR(I)=TOLB-R8PREM()
              ENDIF
 102        CONTINUE
            IF (D.GT.1.D0) THEN
              REINIT=.TRUE.
            ENDIF
            IF (UN-D-TOLB.LE.0.D0) THEN
              D=UN-TOLB+R8PREM()
              DBLOQ=.TRUE.
            ENDIF
            ELSE

            REINIT=.FALSE.
            DO 902 I=1,2
              IF (VALBR(I)-TOLB.LE.0.D0) THEN
                VALBR(I)=TOLB-R8PREM()
              ENDIF
 902        CONTINUE
            IF (D-(UN-TOLB).GE.0.D0) THEN
                D=UN-TOLB+R8PREM()
                DBLOQ=.TRUE.
            ENDIF

            ENDIF

            IF (REINIT) THEN
              DO 802 I=1,6
                EPST(I)=(EPST(I)+EPSI(I))/2
 802          CONTINUE
              GOTO 999
            ELSE

              CALL R8INIR(6,0.D0,BR,1)
                DO 222 I=1,3
                  DO 223 J=I,3
                   DO 224 K=1,3
            BR(T(I,J))=BR(T(I,J))+VECBR(I,K)*VALBR(K)*VECBR(J,K)
 224               CONTINUE
 223             CONTINUE
 222           CONTINUE

              IF (TOT1) THEN
              BINTER(1)=TOLB-R8PREM()
              BINTER(2)=BR(1)
              BINTER(3)=BR(2)
              BINTER(4)=0.D0
              BINTER(5)=0.D0
              BINTER(6)=BR(4)
              ELSEIF (TOT2) THEN
              BINTER(1)=BR(2)
              BINTER(2)=TOLB-R8PREM()
              BINTER(3)=BR(1)
              BINTER(4)=0.D0
              BINTER(5)=BR(4)
              BINTER(6)=0.D0
              ELSEIF (TOT3) THEN
              BINTER(1)=BR(1)
              BINTER(2)=BR(2)
              BINTER(3)=TOLB-R8PREM()
              BINTER(4)=BR(4)
              BINTER(5)=0.D0
              BINTER(6)=0.D0
              ENDIF

              CALL R8INIR(9,0.D0,INTERM,1)
              CALL R8INIR(6,0.D0,B,1)
              CALL R8INIR(6,0.D0,BM,1)
              DO 232 I=1,3
                DO 233 L=1,3
                  DO 234 K=1,3
                  INTERM(I,L)=INTERM(I,L)+VECBM(I,K)*BINTER(T(K,L))
 234              CONTINUE
                  DO 235 J=I,3
                  B(T(I,J))=B(T(I,J))+INTERM(I,L)*VECBM(J,L)
                  BM(T(I,J))=BM(T(I,J))+INTERM(I,L)*VECBM(J,L)
 235              CONTINUE
 233            CONTINUE
 232          CONTINUE
              DM=D

              DO 803 I=1,6
                 EPSI(I)=EPST(I)
                 EPST(I)=EPSF(I)
 803          CONTINUE
              GOTO 999
             ENDIF

C---- CAS OU 2 VALEURS PROPRES SONT NULLES-----------------------------

                ELSEIF (BDIM.EQ.1) THEN

                  CALL R8INIR(9,0.D0,INTERM,1)
                  CALL R8INIR(6,0.D0,EPI,1)
                  DO 242 I=1,3
                    DO 243 L=1,3
                      DO 244 K=1,3
            INTERM(I,L)=INTERM(I,L)+VECBM(K,I)*EPST(T(K,L))
 244                  CONTINUE
                      DO 245 J=I,3
            EPI(T(I,J))=EPI(T(I,J))+INTERM(I,L)*VECBM(L,J)
 245                  CONTINUE
 243                CONTINUE
 242              CONTINUE

                  TOT1=.FALSE.
                  TOT2=.FALSE.
                  TOT3=.FALSE.
                  CALL R8INIR(6,0.D0,BMR,1)
                  IF (VALBM(1)-TOLB.GT.0.D0) THEN
                    BMR(1)=VALBM(1)
                    EPSR(1)=EPI(1)
                    EPSR(2)=EPI(2)
                    EPSR(3)=EPI(3)
                    EPSR(4)=EPI(4)
                    EPSR(5)=EPI(5)
                    EPSR(6)=EPI(6)
                    TOT1=.TRUE.

                  ELSEIF (VALBM(2)-TOLB.GT.0.D0) THEN
                    BMR(1)=VALBM(2)
                    EPSR(1)=EPI(2)
                    EPSR(2)=EPI(3)
                    EPSR(3)=EPI(1)
                    EPSR(4)=EPI(6)
                    EPSR(5)=EPI(4)
                    EPSR(6)=EPI(5)
                    TOT2=.TRUE.

                  ELSEIF (VALBM(3)-TOLB.GT.0.D0) THEN
                    BMR(1)=VALBM(3)
                    EPSR(1)=EPI(3)
                    EPSR(2)=EPI(1)
                    EPSR(3)=EPI(2)
                    EPSR(4)=EPI(5)
                    EPSR(5)=EPI(6)
                    EPSR(6)=EPI(4)
                    TOT3=.TRUE.
                  ENDIF

               CALL LCEOB1(INTMAX,TOLER,EPSR,BMR,DM,
     &                     LAMBDA,MU,ALPHA,ECROB,ECROD,
     &                     SEUIL,BDIM,BR,D,MULT,ELAS,DBLOQ,IRET)

                  IF (COMPTE.LT.100) THEN

                  IF (BR(1).LT.0) THEN
                    REINIT=.TRUE.
                  ENDIF
                  IF (BR(1)-TOLB.LE.0.D0) THEN
                    BR(1)=TOLB-R8PREM()
                  ENDIF
                  IF (D.GT.1.D0) THEN
                    REINIT=.TRUE.
                  ENDIF
                  IF (UN-D-TOLB.LE.0.D0) THEN
                    D=UN-TOLB+R8PREM()
                    DBLOQ=.TRUE.
                  ENDIF

                  ELSE

                  REINIT=.FALSE.
                  IF (BR(1)-TOLB.LE.0.D0) THEN
                    BR(1)=TOLB-R8PREM()
                  ENDIF
                  IF (D-(UN-TOLB).GE.0.D0) THEN
                      D=UN-TOLB+R8PREM()
                      DBLOQ=.TRUE.
                  ENDIF

                  ENDIF

              IF (REINIT) THEN
                DO 804 I=1,6
                  EPST(I)=(EPST(I)+EPSI(I))/2
 804            CONTINUE
              GOTO 999

              ELSE
                    VALB(1)=TOLB-R8PREM()
                    VALB(2)=TOLB-R8PREM()
                    VALB(3)=TOLB-R8PREM()
                    IF (TOT1) VALB(1)=BR(1)
                    IF (TOT2) VALB(2)=BR(1)
                    IF (TOT3) VALB(3)=BR(1)
                    CALL R8INIR(6,0.D0,B,1)
                    CALL R8INIR(6,0.D0,BM,1)
                    DO 252 I=1,3
                      DO 253 J=I,3
                        DO 254 K=1,3
                 B(T(I,J))=B(T(I,J))+VECBM(I,K)*VALB(K)*VECBM(J,K)
                 BM(T(I,J))=BM(T(I,J))+VECBM(I,K)*VALBM(K)*VECBM(K,J)
 254                    CONTINUE
 253                  CONTINUE
 252                CONTINUE
                    DM=D
                DO 805 I=1,6
                  EPSI(I)=EPST(I)
                  EPST(I)=EPSF(I)
 805            CONTINUE
              GOTO 999
            ENDIF
            ENDIF

        ENDIF
9999  CONTINUE

      END
