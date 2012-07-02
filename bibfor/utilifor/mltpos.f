      SUBROUTINE MLTPOS(NBSN,PARENT,FILS,FRERE,PILE,LFRONT,SEQ,FLAG,
     +                  ESTIM,U,W,TAB,LISTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C RESPONSABLE JFBHHUC C.ROSE
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
C TOLE CRP_6
      IMPLICIT NONE
      INTEGER NBSN,PARENT(*),FILS(*),FRERE(*),PILE(*),LFRONT(*)
      INTEGER SEQ(*),ESTIM
      INTEGER FLAG(*)
      INTEGER U(NBSN),W(NBSN),TAB(NBSN),LISTE(NBSN)
C
      INTEGER INIT,FILSI,ND,IQ,MD,M,I,K,SNI,ITEMP,LP,Q1,Q2,SN
      INTEGER BLIMAX
C-----------------------------------------------------------------------
C     CALCUL DES TABLEAUX U ET W (VOIR NOTES RESP. DE  ASHCRAFT ET YANG)
C
      DO 110 SNI = 1,NBSN
      U(SNI) = (LFRONT(SNI)* (LFRONT(SNI)+1))/2
  110 CONTINUE
      DO 150 SNI = 1,NBSN
          Q1 = U(SNI)
          SN = FILS(SNI)
          IF (SN.EQ.0) THEN
              W(SNI) = Q1
          ELSE
              M = 1
              SN = FILS(SNI)
              LISTE(1) = SN
              TAB(1) = U(SN)
              SN = FRERE(SN)
C          DO WHILE (SN.NE.0)
  120         CONTINUE
              IF (SN.NE.0) THEN
                  M = M + 1
                  LISTE(M) = SN
                  TAB(M) = TAB(M-1) + U(M)
                  SN = FRERE(SN)
                  GO TO 120
C FIN DO WHILE
              END IF
              DO 130 K = 1,M
                  TAB(K) = TAB(K) + W(LISTE(K))
  130         CONTINUE
              Q2 = TAB(BLIMAX(M,TAB,1))
              DO 140 I = 1,M
                  Q1 = Q1 + U(LISTE(I))
  140         CONTINUE
              W(SNI) = MAX(Q1,Q2)
          END IF
  150 CONTINUE
C-----------------------------------------------------------------------
C      MODIFICATION DE FILS ET FRERE POUR MINIMISER LA PILE
      DO 180 SNI = 1,NBSN
          SN = FILS(SNI)
          IF (SN.NE.0) THEN
              M = 1
              LISTE(M) = SN
              TAB(M) = W(LISTE(M)) - U(LISTE(M))
              SN = FRERE(SN)
C          DO WHILE (SN.NE.0)
  160         CONTINUE
              IF (SN.NE.0) THEN
                  M = M + 1
                  LISTE(M) = SN
                  TAB(M) = W(LISTE(M)) - U(LISTE(M))
                  SN = FRERE(SN)
                  GO TO 160
C FIN DO WHILE
              END IF
              CALL TRI(TAB,LISTE,1,M)
              FILS(SNI) = LISTE(M)
              SN = FILS(SNI)
              K = M - 1
C          DO WHILE (K.GE.1)
  170         CONTINUE
              IF (K.GE.1) THEN
                  FRERE(SN) = LISTE(K)
                  SN = LISTE(K)
                  K = K - 1
                  GO TO 170
C FIN DO WHILE
              END IF
              FRERE(LISTE(1)) = 0
          END IF
  180 CONTINUE
C-----------------------------------------------------------------------
C      CALCUL DE LA SEQUENCE D'EXECUTION
C
      DO 190 I = 1,NBSN
          FLAG(I) = 0
  190 CONTINUE
      IQ = 0
      DO 240 INIT = 1,NBSN
          IF (PARENT(INIT).EQ.0) THEN
              LP = 0
              FILSI = INIT
C          DO WHILE (FILSI.NE.0)
  200         CONTINUE
              IF (FILSI.NE.0) THEN
C             ND = FILSI
                  LP = LP + 1
                  PILE(LP) = FILSI
                  FILSI = FILS(FILSI)
                  GO TO 200
C FIN DO WHILE
              END IF
C          DO WHILE (LP.GT.0)
  210         CONTINUE
              IF (LP.GT.0) THEN
  220             CONTINUE
                  ND = PILE(LP)
                  MD = FILS(ND)
C            DO WHILE (MD.NE.0)
  230             CONTINUE
                  IF (MD.NE.0) THEN
                      IF (FLAG(MD).EQ.0) THEN
                          IF (FILS(MD).EQ.0) THEN
                              IQ = IQ + 1
                              SEQ(IQ) = MD
                              FLAG(MD) = 1
                          ELSE
                              LP = LP + 1
                              PILE(LP) = MD
                              GO TO 220
                          END IF
                      END IF
                      MD = FRERE(MD)
                      GO TO 230
C FIN DO WHILE
                  END IF
                  IQ = IQ + 1
                  SEQ(IQ) = ND
                  FLAG(ND) = 1
                  LP = LP - 1
                  GO TO 210
C FIN DO WHILE
              END IF
          END IF
  240 CONTINUE
C      ESTIMATION DE LA PILE
      ESTIM = 1
      ITEMP = 1
      DO 250 I = 1,NBSN
          SNI = SEQ(I)
          M = LFRONT(SNI)
          IF (FILS(SNI).EQ.0) THEN
              PILE(SNI) = ITEMP
              ITEMP = ITEMP + (M* (M+1))/2
              ESTIM = MAX(ESTIM,ITEMP-1)
          ELSE
              ITEMP = ITEMP + (M* (M+1))/2
              ESTIM = MAX(ESTIM,ITEMP-1)
              PILE(SNI) = PILE(FILS(SNI))
              ITEMP = PILE(FILS(SNI)) + (M* (M+1))/2
          END IF
  250 CONTINUE
      END
