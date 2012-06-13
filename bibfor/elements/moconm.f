      SUBROUTINE MOCONM(SIGB,SIGA,HH,NLIT,OM,RR,NUFSUP,NUFINF,
     &                  NUFSD1,NUFID1,NUFSD2,NUFID2,PREC)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
C TOLE CRS_1404
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
C RESPONSABLE SFAYOLLE S.FAYOLLE

      INCLUDE 'jeveux.h'
      CHARACTER*8 NUFSUP,NUFINF,NUFSD1,NUFID1,NUFSD2,NUFID2
      INTEGER      NLIT
      REAL*8       SIGB,SIGA(NLIT),HH,OM(NLIT),RR(NLIT),PREC,E1,SIGMA
      INTEGER   PTMAX,ORDLU
      PARAMETER (PTMAX=50)
      PARAMETER (ORDLU=2)
      REAL*8     NN(NLIT*PTMAX),MM(NLIT*PTMAX),ETA, RHOL(NLIT+2)
      REAL*8     XI(NLIT+2), OMM(NLIT+2), NN0, MM0,POLY(ORDLU+1),XX
      INTEGER    I, J, K, II, ILIT, DEB, NPT,TRI(NLIT+2)
      INTEGER    ORDOK,JVALE,JFON,JPROL

C --- TRI SELON LA POSITION DANS L'EPAISSEUR
C    POUR AVOIR: RR(TRI(I))<=RR(TRI(I+1))
C    --> DU PLUS PETIT AU PLUS GRAND

      DO 10, I = 1,NLIT
        RHOL(I+1) = RR(I)
        OMM(I+1)  = OM(I)
 10   CONTINUE
      RHOL(1) = -1.0D0
      RHOL(NLIT+2) = 1.0D0
      OMM(1)      = HH
      OMM(NLIT+2) = HH

      DO 20, I=1,NLIT+2
        TRI(I)=I
 20   CONTINUE

      IF (NLIT.GT.1) THEN
        DO 40, J=1,NLIT-1
          DO 30, I=2,NLIT+1-J
            IF (RR(TRI(I)-1).GT.RR(TRI(I+1)-1)) THEN
              II=TRI(I)
              TRI(I)=TRI(I+1)
              TRI(I+1)=II
            ENDIF
 30       CONTINUE
 40     CONTINUE
      ENDIF

C --- POSITIVE BENDING

C LE CAS DES POINTS SUPERPOSES EST TRAITE: OM=0 OU RR(I)=RR(I-1)
C    (PAR EX LINER SI RR=-1)

      DO 45, I = 1,NLIT+2
        XI(I) = -1.0D0
 45   CONTINUE
      II=0
      DO 50, ILIT=0,NLIT
        I = NLIT-ILIT+2
        XI(TRI(I)) = 1.0D0
        NN0=0.D0
        MM0=0.D0
        DO 60, J=1,NLIT
          NN0=NN0+XI(J+1)*OM(J)*SIGA(J)
          MM0=MM0+XI(J+1)*OM(J)*SIGA(J)*RHOL(J+1)*HH/2.0D0
 60     CONTINUE
        IF (OMM(TRI(I)) .LT. 1.D-8*OMM(1)) THEN
          DEB=1
        ELSE
          DEB=0
        ENDIF
        NPT = INT(ABS(RHOL(TRI(I-1))-RHOL(TRI(I)))/2.D0*PTMAX)-1
        NPT = MAX(NPT,0)
        DO 70, K=DEB,NPT
          IF (NPT .EQ. 0) THEN
            ETA=RHOL(TRI(I))
          ELSE
            ETA=RHOL(TRI(I))+K*(RHOL(TRI(I-1))-RHOL(TRI(I)))/NPT
          ENDIF
          II=II+1
          NN(II)=NN0+SIGB*HH*(1+ETA)/2.0D0-PREC
          MM(II)=MM0-SIGB*HH*HH*(1-ETA*ETA)/8.0D0
 70     CONTINUE
 50   CONTINUE

C --- AJOUT DE LA FONCTION
      E1=0.D0
      NPT = II
      CALL LSQPOL(ORDLU,E1,NPT,NN,MM,ORDOK,POLY,SIGMA)

C     --- CREATION ET REMPLISSAGE DE L'OBJET NUFSUP.VALE ---
      CALL WKVECT ( NUFSUP//'           .VALE', 'G V R', 2*NPT, JVALE )
      JFON = JVALE + NPT
      DO 72 I = 0, NPT-1
        XX = NN(1) + (NN(NPT)-NN(1))*I/(NPT-1)
        ZR(JVALE+I) = XX
        ZR(JFON +I) = 0.0D0
        DO 73, J = 0,ORDOK
          ZR(JFON +I) = ZR(JFON +I) + POLY(J+1)*(XX**J)
 73     CONTINUE
 72   CONTINUE

      CALL WKVECT ( NUFSD1//'           .VALE', 'G V R', 2*NPT, JVALE )
      JFON = JVALE + NPT
      DO 74 I = 0, NPT-1
        XX = NN(1) + (NN(NPT)-NN(1))*I/(NPT-1)
        ZR(JVALE+I) = XX
        ZR(JFON +I) = 0.0D0
        DO 75, J = 1,ORDOK
          ZR(JFON +I) = ZR(JFON +I) + J*POLY(J+1)*(XX**(J-1))
 75     CONTINUE
 74   CONTINUE

      CALL WKVECT ( NUFSD2//'           .VALE', 'G V R', 2*NPT, JVALE )
      JFON = JVALE + NPT
      DO 76 I = 0, NPT-1
        XX = NN(1) + (NN(NPT)-NN(1))*I/(NPT-1)
        ZR(JVALE+I) = XX
        ZR(JFON +I) = 0.0D0
        DO 77, J = 2,ORDOK-1
          ZR(JFON +I) = ZR(JFON +I)+ J*(J-1)*POLY(J+1)*(XX**(J-2))
 77     CONTINUE
 76   CONTINUE

C     --- CREATION ET REMPLISSAGE DE L'OBJET NUFSUP.PROL ---
      CALL WKVECT ( NUFSUP//'           .PROL', 'G V K24', 6, JPROL )
      ZK24(JPROL)   = 'FONCTION                '
      ZK24(JPROL+1) = 'LIN LIN                 '
      ZK24(JPROL+2) = 'X                       '
      ZK24(JPROL+3) = 'TOUTRESU                '
      ZK24(JPROL+4) = 'LL                      '

      CALL WKVECT ( NUFSD1//'           .PROL', 'G V K24', 6, JPROL )
      ZK24(JPROL)   = 'FONCTION                '
      ZK24(JPROL+1) = 'LIN LIN                 '
      ZK24(JPROL+2) = 'X                       '
      ZK24(JPROL+3) = 'TOUTRESU                '
      ZK24(JPROL+4) = 'CC                      '

      CALL WKVECT ( NUFSD2//'           .PROL', 'G V K24', 6, JPROL )
      ZK24(JPROL)   = 'FONCTION                '
      ZK24(JPROL+1) = 'LIN LIN                 '
      ZK24(JPROL+2) = 'X                       '
      ZK24(JPROL+3) = 'TOUTRESU                '
      ZK24(JPROL+4) = 'CC                      '

C--- NEGATIVE BENDING

      II=0
      DO 80, I=1,NLIT+1
        NN0=0.D0
        MM0=0.D0
        DO 90, J=1,NLIT
          NN0=NN0-XI(J+1)*OM(J)*SIGA(J)
          MM0=MM0-XI(J+1)*OM(J)*SIGA(J)*RHOL(J+1)*HH/2.0D0
 90     CONTINUE
        IF (OMM(TRI(I)) .LT. 1.D-8*OMM(1)) THEN
          DEB=1
        ELSE
          DEB=0
        ENDIF
        NPT = INT(ABS(RHOL(TRI(I+1))-RHOL(TRI(I)))/2.D0*PTMAX)-1
        NPT = MAX(NPT,0)
        DO 100, K=DEB,NPT
          IF (NPT .EQ. 0) THEN
            ETA=RHOL(TRI(I))
          ELSE
            ETA=RHOL(TRI(I))+K*(RHOL(TRI(I+1))-RHOL(TRI(I)))/NPT
          ENDIF
          II=II+1
          NN(II)=NN0+SIGB*HH*(1-ETA)/2.0D0-PREC
          MM(II)=MM0+SIGB*HH*HH*(1-ETA*ETA)/8.0D0
 100    CONTINUE
        XI(TRI(I+1))=-1
 80   CONTINUE

C--- AJOUT DE LA FONCTION
      E1=0.D0
      NPT = II
      CALL LSQPOL(ORDLU,E1,NPT,NN,MM,ORDOK,POLY,SIGMA)

C     --- CREATION ET REMPLISSAGE DE L'OBJET NUFINF.VALE ---
      CALL WKVECT ( NUFINF//'           .VALE', 'G V R', 2*NPT, JVALE )
      JFON = JVALE + NPT
      DO 102 I = 0, NPT-1
        XX = NN(1) + (NN(NPT)-NN(1))*I/(NPT-1)
        ZR(JVALE+I) = XX
        ZR(JFON +I) = 0.0D0
        DO 103, J = 0,ORDOK
          ZR(JFON +I) = ZR(JFON +I) + POLY(J+1)*(XX**J)
 103    CONTINUE
 102  CONTINUE

      CALL WKVECT ( NUFID1//'           .VALE', 'G V R', 2*NPT, JVALE )
      JFON = JVALE + NPT
      DO 104 I = 0, NPT-1
        XX = NN(1) + (NN(NPT)-NN(1))*I/(NPT-1)
        ZR(JVALE+I) = XX
        ZR(JFON +I) = 0.0D0
        DO 105, J = 1,ORDOK
          ZR(JFON +I) = ZR(JFON +I) + J*POLY(J+1)*(XX**(J-1))
 105    CONTINUE
 104  CONTINUE

      CALL WKVECT ( NUFID2//'           .VALE', 'G V R', 2*NPT, JVALE )
      JFON = JVALE + NPT
      DO 106 I = 0, NPT-1
        XX = NN(1) + (NN(NPT)-NN(1))*I/(NPT-1)
        ZR(JVALE+I) = XX
        ZR(JFON +I) = 0.0D0
        DO 107, J = 2,ORDOK-1
          ZR(JFON +I) = ZR(JFON +I)+ J*(J-1)*POLY(J+1)*(XX**(J-2))
 107    CONTINUE
 106  CONTINUE

C     --- CREATION ET REMPLISSAGE DE L'OBJET NUFSUP.PROL ---
      CALL WKVECT ( NUFINF//'           .PROL', 'G V K24', 6, JPROL )
      ZK24(JPROL)   = 'FONCTION                '
      ZK24(JPROL+1) = 'LIN LIN                 '
      ZK24(JPROL+2) = 'X                       '
      ZK24(JPROL+3) = 'TOUTRESU                '
      ZK24(JPROL+4) = 'LL                      '

      CALL WKVECT ( NUFID1//'           .PROL', 'G V K24', 6, JPROL )
      ZK24(JPROL)   = 'FONCTION                '
      ZK24(JPROL+1) = 'LIN LIN                 '
      ZK24(JPROL+2) = 'X                       '
      ZK24(JPROL+3) = 'TOUTRESU                '
      ZK24(JPROL+4) = 'CC                      '

      CALL WKVECT ( NUFID2//'           .PROL', 'G V K24', 6, JPROL )
      ZK24(JPROL)   = 'FONCTION                '
      ZK24(JPROL+1) = 'LIN LIN                 '
      ZK24(JPROL+2) = 'X                       '
      ZK24(JPROL+3) = 'TOUTRESU                '
      ZK24(JPROL+4) = 'CC                      '

      END
