      SUBROUTINE MLTPAS(NBND,NBSN,SUPND,XADJ,ADJNCY,ANC,NOUV,SEQ,GLOBAL,
     +     ADRESS,NBLIGN,LGSN,NBLOC,NCBLOC,LGBLOC,DIAG,COL,
     +     LMAT,PLACE)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 26/04/2011   AUTEUR DELMAS J.DELMAS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE ROSE C.ROSE
C TOLE CRP_4

      IMPLICIT NONE
      INTEGER NBND,NBSN,NBLOC,NCBLOC(*),LGBLOC(*)
      INTEGER SUPND(NBSN+1),DIAG(0:NBND),SEQ(NBSN)
      INTEGER COL(*)
      INTEGER XADJ(NBND+1),ADJNCY(*),LMAT
      INTEGER ANC(NBND),NOUV(NBND)
      INTEGER*4 GLOBAL(*)
      INTEGER ADRESS(NBSN+1)
      INTEGER NBLIGN(NBSN),LGSN(NBSN)
C
C=========================================================
C     CALCUL DES ADRESSES DANS LA FACTORISEE DES TERMES INITIAUX
C     VERSION ASTER AVEC MATRICE INITIALE COMPACTE PAR LIGNES
C     ET DDL DE LAGRANGE
C     DANS CETTE VERSION LES ADRESSES DES TERMES INITIAUX
C     SONT RANGEES DANS COL, QUI NE SERT PLUS.
C     AUPARAVANT ON UTILISAIT UN TABLEAU ADINIT
C==========================================================
      INTEGER PLACE(NBND)
      INTEGER I,J,NDJ,SNI,ANDI,ANDJ,CODE,HAUT
      INTEGER NDI,LFAC,DEPART,AD,ISN,LONGB,IB,IC
      ISN = 0
      LONGB = 0
      LMAT=DIAG(NBND)
      DO 290 IB = 1,NBLOC
         LFAC = LONGB
         DO 280 IC = 1,NCBLOC(IB)
            ISN = ISN + 1
            SNI = SEQ(ISN)
            DO 120 I = ADRESS(SNI),ADRESS(SNI+1) - 1
               PLACE(GLOBAL(I)) = I - ADRESS(SNI) + 1
 120        CONTINUE
            HAUT = NBLIGN(SNI)
            DO 270 I = 0,LGSN(SNI) - 1
               NDI = SUPND(SNI) + I
               ANDI = ANC(NDI)
               DEPART = DIAG(ANDI-1) + 1
               COL(DIAG(ANDI))=LFAC + I*HAUT+I+1 +NBND
               DO 170 J = XADJ(ANDI),XADJ(ANDI+1) - 1
                  ANDJ = ADJNCY(J)
                  NDJ = NOUV(ANDJ)
                  IF (NDJ.GE.NDI) THEN
                     IF (ANDJ.LE.ANDI) THEN
                        DO 130 AD = DEPART,DIAG(ANDI)
                           IF (COL(AD).EQ.ANDJ) GO TO 140
 130                    CONTINUE
                        GO TO 170
 140                    CONTINUE
                        CODE = -1
                        DEPART = AD
                     ELSE
                        DO 150 AD = DIAG(ANDJ-1) + 1,
     +                       DIAG(ANDJ)
                           IF (COL(AD).EQ.ANDI) GO TO 160
 150                    CONTINUE
                        GO TO 170
 160                    CONTINUE
                        CODE = 1
                     END IF
C     CHANGTDGEMV                  ADINIT(AD) = LFAC + PLACE(NDJ) - I
C     ADINIT(AD) = LFAC + I*HAUT + PLACE(NDJ)
                     COL(AD)=  LFAC + I*HAUT + PLACE(NDJ) +NBND

                     IF(CODE.LT.0) THEN
                        COL(AD) = -COL(AD)
                     ENDIF
                  END IF
 170           CONTINUE

 270        CONTINUE
            LFAC = LFAC + HAUT*LGSN(SNI)
 280     CONTINUE
         LONGB = LGBLOC(IB) + LONGB
 290  CONTINUE
      DO 300 I=1,LMAT
         IF( COL(I).GT.NBND) THEN
            COL(I)=COL(I)-NBND
         ELSEIF(COL(I).LT.(-NBND)) THEN
            COL(I) = COL(I) +NBND
         ELSE
         ENDIF
 300  CONTINUE
      END
