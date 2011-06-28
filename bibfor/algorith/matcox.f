      SUBROUTINE MATCOX(NDIM,PP,DDT1,DDT2,DDT3,DDT4,P,
     &                  NNO,DDLH,DDLS,JAC,FFP,SINGU,RR,
     &                  MMAT)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 27/06/2011   AUTEUR MASSIN P.MASSIN 
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
      REAL*8       DDT1(3,3),DDT2(3,3),DDT3(3,3),DDT4(3,3),PP(3,3)
      REAL*8       P(3,3),MMAT(216,216)
      REAL*8       JAC,FFP(27),RR
      INTEGER      NDIM,DDLH,DDLS,NNO,SINGU
C.......................................................................
C
C         CALCUL DES MATRICES DE CONTACT FROTTEMENT 
C                 LOI COHESIVE - POUR X-FEM
C                     (METHODE CONTINUE)
C
C

C  ENTREES  --->  PP,P,JAC,FFP,RR,NDIM,DDLH,DDLS,NNO
C           --->  SINGU,DDT1,DDT2,DDT3,DDT4
C  SORTIES  --->  MMAT
C
C.......................................................................
      REAL*8       DDT11(3,3),DDT21(3,3),DDT31(3,3),DDT41(3,3)
      REAL*8       DDT111(3,3),DDT211(3,3),DDT311(3,3),DDT411(3,3)
      INTEGER      I,J,K,L

C.......................................................................
C
      CALL MATINI(3,3,0.D0,DDT11)
      CALL MATINI(3,3,0.D0,DDT21)
      CALL MATINI(3,3,0.D0,DDT31)
      CALL MATINI(3,3,0.D0,DDT41)
C
      CALL MATINI(3,3,0.D0,DDT111)
      CALL MATINI(3,3,0.D0,DDT211)
      CALL MATINI(3,3,0.D0,DDT311)
      CALL MATINI(3,3,0.D0,DDT411)
C
      DO 1 I = 1,NDIM
        DO 2 J = 1,NDIM
          DO 3 L = 1,NDIM
            DDT11(I,J)=DDT11(I,J) + PP(I,L)*DDT1(L,J)
            DDT21(I,J)=DDT21(I,J) + PP(I,L)*DDT2(L,J)
            DDT31(I,J)=DDT31(I,J) + P(I,L)*DDT3(L,J)
            DDT41(I,J)=DDT41(I,J) + P(I,L)*DDT4(L,J)
3         CONTINUE
2       CONTINUE
1     CONTINUE
C
      DO 4 I = 1,NDIM
        DO 5 J = 1,NDIM
          DO 6 L = 1,NDIM
            DDT111(I,J)=DDT111(I,J) + DDT11(I,L)*PP(L,J)
            DDT211(I,J)=DDT211(I,J) + DDT21(I,L)*P(L,J)
            DDT311(I,J)=DDT311(I,J) + DDT31(I,L)*PP(L,J)
            DDT411(I,J)=DDT411(I,J) + DDT41(I,L)*P(L,J)
6         CONTINUE
5       CONTINUE
4     CONTINUE

      DO 7 I = 1,NNO
        DO 8 J = 1,NNO
          DO 9 K = 1,DDLH
            DO 10 L = 1,DDLH
C
              MMAT(DDLS*(I-1)+NDIM+K,DDLS*(J-1)+NDIM+L) =
     &         MMAT(DDLS*(I-1)+NDIM+K,DDLS*(J-1)+NDIM+L)+
     &         4.D0*FFP(I)*DDT111(K,L)*FFP(J)*JAC
     &         +4.D0*FFP(I)*DDT211(K,L)*FFP(J)*JAC
     &         +4.D0*FFP(I)*DDT311(K,L)*FFP(J)*JAC
     &         +4.D0*FFP(I)*DDT411(K,L)*FFP(J)*JAC

10          CONTINUE

            DO 11 L = 1,SINGU*NDIM
              MMAT(DDLS*(I-1)+NDIM+K,DDLS*(J-1)+NDIM+DDLH+L) =
     &         MMAT(DDLS*(I-1)+NDIM+K,DDLS*(J-1)+NDIM+DDLH+L)+
     &         4.D0*FFP(I)*DDT111(K,L)*FFP(J)*JAC*RR
     &         +4.D0*FFP(I)*DDT211(K,L)*FFP(J)*JAC*RR
     &         +4.D0*FFP(I)*DDT311(K,L)*FFP(J)*JAC*RR
     &         +4.D0*FFP(I)*DDT411(K,L)*FFP(J)*JAC*RR
11          CONTINUE

9         CONTINUE
          DO 12 K = 1,SINGU*NDIM
            DO 13 L = 1,DDLH
              MMAT(DDLS*(I-1)+NDIM+DDLH+K,DDLS*(J-1)+NDIM+L) =
     &        MMAT(DDLS*(I-1)+NDIM+DDLH+K,DDLS*(J-1)+NDIM+L)+
     &        4.D0*FFP(I)*DDT111(K,L)*FFP(J)*JAC*RR
     &        +4.D0*FFP(I)*DDT211(K,L)*FFP(J)*JAC*RR
     &        +4.D0*FFP(I)*DDT311(K,L)*FFP(J)*JAC*RR
     &        +4.D0*FFP(I)*DDT411(K,L)*FFP(J)*JAC*RR
13          CONTINUE
C
            DO 14 L = 1,SINGU*NDIM
C
              MMAT(DDLS*(I-1)+NDIM+DDLH+K,DDLS*(J-1)+NDIM+DDLH+L) =
     &         MMAT(DDLS*(I-1)+NDIM+DDLH+K,DDLS*(J-1)+NDIM+DDLH+L)+
     &         4.D0*FFP(I)*DDT111(K,L)*FFP(J)*JAC*RR*RR
     &         +4.D0*FFP(I)*DDT211(K,L)*FFP(J)*JAC*RR*RR
     &         +4.D0*FFP(I)*DDT311(K,L)*FFP(J)*JAC*RR*RR
     &         +4.D0*FFP(I)*DDT411(K,L)*FFP(J)*JAC*RR*RR

14          CONTINUE
12        CONTINUE


8       CONTINUE
7     CONTINUE

      END
