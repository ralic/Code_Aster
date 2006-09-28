      SUBROUTINE MEMAXG ( NBORN,BORN,GBIL,LONVEC,RESULT )
      IMPLICIT NONE
C
      INTEGER     NBORN,LONVEC
      REAL*8      BORN(*),GBIL(*)
      CHARACTER*8 RESULT
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C      OPERATEUR :     CALC_G
C      OPTION    :     'G_MAX_GLOB'
C
C     AUTEUR : M. BONNAMY
C     ----------------------------------------------------------------
C
C     BUT: MAXIMISATION DE G SOUS CONTRAINTES BORNES
C
C     ----------------------------------------------------------------
C
C     NBORN       /IN/:NOMBRE DE BORNES
C     BORN        /IN/:CONTRAINTES BORNES
C     GBIL        /IN/:TRIANGLE INFERIEUR DE LA MATRICE G BILINEAIRE
C     LONVEC      /IN/:NOMBRE DE CHAMPS DE DEPLACEMENTS
C     RESULT     /OUT/:TABLE RESULTAT
C
C ----------------------------------------------------------------------
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
      CHARACTER*32      JEXNUM
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER      I,J,IBID,NBPRUP,NBPAR,NCOMB,NFREQ
      INTEGER      IG,IND,INDOLD,IPROV,IQ,K,IGQ,IPA,INOPR,ITYPR
C
      REAL*8       GMAX,S,R8PREC
      COMPLEX*16   CBID
C
      CHARACTER*3  CHNU
      CHARACTER*8  K8BID,TABGMA
      CHARACTER*24 COLLEC
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      NBPRUP = LONVEC+2
      NCOMB = 2**LONVEC
      CALL WKVECT('&&MEMAXG.NORU','V V K16',NBPRUP,INOPR)
      CALL WKVECT('&&MEMAXG.TYPR','V V K8',NBPRUP,ITYPR)
      DO 140 I=1,LONVEC
             CALL CODENT(I,'G',CHNU)
             ZK16(INOPR+I-1) = 'Q_'//CHNU
             ZK8 (ITYPR+I-1) = 'R'
 140  CONTINUE
      ZK16(INOPR+LONVEC) = 'G'
      ZK8 (ITYPR+LONVEC) = 'R'
      ZK16(INOPR+LONVEC+1) = 'G_MAX'
      ZK8 (ITYPR+LONVEC+1) = 'R'
C
      TABGMA = 'G_MAX'
      CALL TBCRSD ( TABGMA, 'G' )
      CALL TBAJPA ( TABGMA,NBPRUP,ZK16(INOPR),ZK8(ITYPR) )
C
      IF (NBORN.NE.(2.D0*LONVEC)) THEN
             CALL U2MESS('F','UTILITAI2_50')

      END IF
      COLLEC = '&&MEMAXG.BORNES_Q'
      CALL JECREC(COLLEC,'V V R','NU','CONTIG','CONSTANT',2**LONVEC)
      CALL JEECRA(COLLEC,'LONMAX',LONVEC,' ')
      CALL WKVECT('&&MEMAXG.GQIJ','V V R8',LONVEC,IGQ)
      CALL WKVECT('&&MEMAXG.TABL','V V R8',LONVEC+2,IPA)
C
C     STOCKAGE DES 2 PUISSANCE N COMBINAISON DE CHARGES
C
      DO 170 I=1,LONVEC
              NFREQ = 0
              IND = 0
              INDOLD = 1
              DO 180 J=1,NCOMB
                 NFREQ = NFREQ+1
                 IF (I.EQ.1) CALL JECROC(JEXNUM(COLLEC,J))
                 CALL JEVEUO(JEXNUM(COLLEC,J),'E',IQ)
                 IF (NFREQ.GT.(2**(LONVEC-I))) THEN
                   IPROV = INDOLD
                   INDOLD = IND
                   IND = IPROV
                   NFREQ = 1
                 END IF
                 ZR(IQ+I-1) = BORN(2*(I-1)+ IND + 1)
 180          CONTINUE
 170  CONTINUE
C
C     BALAYAGE DES SOMMETS DE LA FORME QUADRATIQUE G = QGQ
C
      GMAX = 0.D0
C
      DO 190 K=1,NCOMB
C
             CALL JEVEUO(JEXNUM(COLLEC,K),'L',IQ)
C
             S = 0.D0
             DO 200 I=1,LONVEC
                ZR(IGQ+I-1) = 0.D0
                DO 210 J=1,LONVEC
                   IF (I.LT.J) THEN
                     ZR(IGQ+I-1) = ZR(IGQ+I-1)
     &                            + GBIL(J*(J-1)/2 + I )*ZR(IQ+J-1)
                   ELSE
                     ZR(IGQ+I-1) = ZR(IGQ+I-1)
     &                            + GBIL(I*(I-1)/2 + J )*ZR(IQ+J-1)
                   END IF
 210            CONTINUE
                S = S + ZR(IGQ+I-1) * ZR(IQ+I-1)
                ZR(IPA+I-1) = ZR(IQ+I-1)
 200         CONTINUE
C
             IF (S.GT.GMAX) THEN
               GMAX = S
             END IF
C
        ZR(IPA+LONVEC) = S
        NBPAR = LONVEC+1
        CALL TBAJLI ( TABGMA, NBPAR, ZK16(INOPR),
     &                    IBID, ZR(IPA), CBID, K8BID, 0 )
C
 190  CONTINUE
C
      CALL TBEXVE(TABGMA,'G','&&MEMAXG.G','V',NCOMB,K8BID)
      CALL JEVEUO('&&MEMAXG.G','L',IG)

      DO 220 K=1,NCOMB
C
        IF (ZR(IG+K-1).EQ.GMAX) THEN
          CALL JEVEUO(JEXNUM(COLLEC,K),'L',IQ)
          DO 230 I=1,LONVEC
            ZR(IPA+I-1) = ZR(IQ+I-1)
 230      CONTINUE
          ZR(IPA+LONVEC)   = GMAX
          ZR(IPA+LONVEC+1) = GMAX
          CALL TBAJLI ( TABGMA, NBPRUP, ZK16(INOPR),
     &                  IBID, ZR(IPA), CBID, K8BID, K )
        END IF
C
 220  CONTINUE
C
      CALL TBTRTB(TABGMA,'G',RESULT,1,ZK16(INOPR+LONVEC),'DE',
     &               0.D0,'ABSOLU  ')
      CALL DETRSD('TABLE',TABGMA)
C
      CALL JEDETC('V','&&',1)
 9999 CONTINUE
      CALL JEDEMA()
      END
