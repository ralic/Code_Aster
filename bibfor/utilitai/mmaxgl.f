      SUBROUTINE MMAXGL ( NBORN,BORN,GBIL,NOEU,ABCUR,LONVEC,
     &                    NNOFF,RESULT )
      IMPLICIT NONE
C
      INCLUDE 'jeveux.h'
      INTEGER     NBORN,LONVEC,NNOFF
      REAL*8      BORN(*),GBIL(*),ABCUR(*)
      CHARACTER*8 RESULT,NOEU(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C
C      OPERATEUR :     CALC_G
C
C     AUTEUR : J. ANGLES
C     ----------------------------------------------------------------
C
C     BUT: MAXIMISATION DE G LOCAL SOUS CONTRAINTES BORNES
C
C     ----------------------------------------------------------------
C
C     NBORN       /IN/:NOMBRE DE BORNES
C     BORN        /IN/:CONTRAINTES BORNES
C     GBIL        /IN/:TRIANGLE INFERIEUR DE LA MATRICE G BILINEAIRE
C     NOEU        /IN/:LISTE DES NOEUDS DU FOND DE FISSURE
C     ABCUR       /IN/:ABSCISSE CURVILIGNE DU FOND DE FISSURE
C     LONVEC      /IN/:NOMBRE DE CHAMPS DE DEPLACEMENTS
C     NNOFF       /IN/:NOMBRE DE NOEUD EN FOND DE FISSURE
C     RESULT     /OUT/:TABLE RESULTAT
C
C ----------------------------------------------------------------------
C
C
      INTEGER      I,J,K,N,IBID,NBPRUP,NBPAR,NCOMB,NFREQ
      INTEGER      IG,IND,INDOLD,IPROV,IQ,IGQ,IPA,INOPR,ITYPR,NI,NJ
      INTEGER      IRMAX,IKMAX
C
      REAL*8       MGMAX,GMAX,S,RBID
      COMPLEX*16   CBID
C
      CHARACTER*3  CHNU
      CHARACTER*8  K8BID,TABGMA,TABLE(2)
      CHARACTER*24 COLLEC
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      NBPRUP = LONVEC+5
      NCOMB = 2**LONVEC
      CALL WKVECT('&&MMAXGL.NORU','V V K16',NBPRUP,INOPR)
      CALL WKVECT('&&MMAXGL.TYPR','V V K8',NBPRUP,ITYPR)
      DO 140 I=1,LONVEC
             CALL CODENT(I,'G',CHNU)
             ZK16(INOPR+I-1) = 'Q_'//CHNU
             ZK8 (ITYPR+I-1) = 'R'
 140  CONTINUE
      ZK16(INOPR+LONVEC) = 'NOEUD'
      ZK8 (ITYPR+LONVEC) = 'K8'
      ZK16(INOPR+LONVEC+1) = 'ABSC_CURV'
      ZK8 (ITYPR+LONVEC+1) = 'R'
      ZK16(INOPR+LONVEC+2) = 'G'
      ZK8 (ITYPR+LONVEC+2) = 'R'
      ZK16(INOPR+LONVEC+3) = 'G_MAX'
      ZK8 (ITYPR+LONVEC+3) = 'R'
      ZK16(INOPR+LONVEC+4) = 'MAX_G_MAX'
      ZK8 (ITYPR+LONVEC+4) = 'R'
C
      TABGMA = 'G_MAX'
      TABLE(1) = 'T1'
      TABLE(2) = 'T2'
C
      CALL TBCRSD ( 'T4', 'V' )
      CALL TBAJPA ( 'T4',NBPRUP,ZK16(INOPR),ZK8(ITYPR) )
C
      IF (NBORN.NE.(2.D0*LONVEC)) THEN
             CALL U2MESS('F','RUPTURE1_14')
C
      END IF
      COLLEC = '&&MMAXGL.BORNES_Q'
      CALL JECREC(COLLEC,'V V R','NU','CONTIG','CONSTANT',2**LONVEC)
      CALL JEECRA(COLLEC,'LONMAX',LONVEC,' ')
      CALL WKVECT('&&MMAXGL.GQIJ','V V R',LONVEC,IGQ)
      CALL WKVECT('&&MMAXGL.TABL','V V R',LONVEC+4,IPA)
      CALL WKVECT('&&MMAXGL.RMAX','V V R',LONVEC+4,IRMAX)
      CALL WKVECT('&&MMAXGL.KMAX','V V K8',1,IKMAX)
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
C     POUR CHAQUE NOEUD
C
      MGMAX = 0.D0
C
      DO 150 N=1, NNOFF
C
         CALL TBCRSD ( TABGMA, 'G' )
         CALL TBAJPA ( TABGMA,NBPRUP,ZK16(INOPR),ZK8(ITYPR) )
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
                    NJ = J*(J-1)/2 + I
                    IF ( NJ .EQ. 1 ) THEN
                       ZR(IGQ+I-1) = ZR(IGQ+I-1)
     &                               + GBIL(N)*ZR(IQ+J-1)
                    ELSE
                       ZR(IGQ+I-1) = ZR(IGQ+I-1)
     &                             + GBIL(N+((NJ-1)*NNOFF))*ZR(IQ+J-1)
                    END IF
                  ELSE
                    NI = I*(I-1)/2 + J
                    IF ( NI .EQ. 1 ) THEN
                       ZR(IGQ+I-1) = ZR(IGQ+I-1)
     &                               + GBIL(N)*ZR(IQ+J-1)
                    ELSE
                       ZR(IGQ+I-1) = ZR(IGQ+I-1)
     &                             + GBIL(N+((NI-1)*NNOFF))*ZR(IQ+J-1)
                    END IF
                  END IF
 210           CONTINUE
               S = S + ZR(IGQ+I-1) * ZR(IQ+I-1)
               ZR(IPA+I-1) = ZR(IQ+I-1)
 200        CONTINUE
C
            IF (S.GT.GMAX) THEN
              GMAX = S
            END IF
C
           ZR(IPA+LONVEC) = ABCUR(N)
           ZR(IPA+LONVEC+1) = S
           NBPAR = LONVEC+3
C
C  CONSTRUCTION DE LA TABLE DES G
           CALL TBAJLI ( TABGMA, NBPAR, ZK16(INOPR),
     &                       IBID, ZR(IPA), CBID, NOEU(N), 0 )
C
 190     CONTINUE
C
         CALL TBEXVE(TABGMA,'G','&&MMAXGL.G','V',NCOMB,K8BID)
         CALL JEVEUO('&&MMAXGL.G','L',IG)

         DO 220 K=1,NCOMB
C
           IF (ZR(IG+K-1).EQ.GMAX) THEN
             CALL JEVEUO(JEXNUM(COLLEC,K),'L',IQ)
             DO 230 I=1,LONVEC
               ZR(IPA+I-1) = ZR(IQ+I-1)
 230         CONTINUE
             ZR(IPA+LONVEC+1) = GMAX
             ZR(IPA+LONVEC+2) = GMAX
C
             IF (GMAX.GE.MGMAX) THEN
               MGMAX = GMAX
               DO 240 I=1, LONVEC
                 ZR(IRMAX+I-1) = ZR(IPA+I-1)
 240           CONTINUE
               ZR(IRMAX+LONVEC) = ZR(IPA+LONVEC)
               ZR(IRMAX+LONVEC+1) = ZR(IPA+LONVEC+1)
               ZR(IRMAX+LONVEC+2) = ZR(IPA+LONVEC+2)
               ZR(IRMAX+LONVEC+3) = MGMAX
               ZK8(IKMAX) = NOEU(N)
             END IF
C
             NBPAR = LONVEC+4
             CALL TBAJLI ( TABGMA, NBPAR, ZK16(INOPR),
     &                     IBID, ZR(IPA), CBID, NOEU(N), K )
C
C  CONSTRUCTION DE LA TABLE DES G_MAX
             CALL TBAJLI ( 'T4', NBPAR, ZK16(INOPR),
     &                     IBID, ZR(IPA), CBID, NOEU(N), 0 )
           END IF
C
 220     CONTINUE
C
         CALL TBTRTB(TABGMA,'V',TABLE(2),1,ZK16(INOPR+LONVEC+2),'DE',
     &               0.D0,'ABSOLU  ')
         CALL DETRSD('TABLE',TABGMA)
         CALL JEDETR('&&MMAXGL.G')
         IF (N .EQ. 1) THEN
            CALL COPISD('TABLE','V',TABLE(2),TABLE(1))
         ELSE
            CALL TBFUTB('T3','V',2,TABLE,' ',' ',IBID,RBID,CBID,
     &                   K8BID)
            CALL DETRSD('TABLE',TABLE(1))
            CALL COPISD('TABLE','V','T3',TABLE(1))
            CALL DETRSD('TABLE','T3')
         END IF
         CALL DETRSD('TABLE',TABLE(2))
C
 150  CONTINUE
C
C  CONSTRUCTION DE LA TABLE SYNTHETISANT LES G_MAX
C  ORDONNEE SELON G_MAX DECROISSANT
C
         CALL TBTRTB('T4','V','T5',1,ZK16(INOPR+LONVEC+3),'DE',
     &               0.D0,'ABSOLU  ')
         CALL TBAJLI('T5', NBPRUP, ZK16(INOPR),
     &               IBID, ZR(IRMAX), CBID, ZK8(IKMAX), 1 )
         CALL DETRSD('TABLE','T4')
C
C  CONSTRUCTION DE LA TABLE SYNTHETISANT LES G_MAX
C  ORDONNEE SELON L'ABSCISSE CURVILIGNE CROISSANTE
C
         CALL TBTRTB('T5','V','T4',1,ZK16(INOPR+LONVEC+1),'CR',
     &               0.D0,'ABSOLU  ')
C
C  CONSTRUCTION DE LA TABLE FINALE
C
         CALL COPISD('TABLE','V',TABLE(1),'T3')
         CALL DETRSD('TABLE',TABLE(1))
         CALL COPISD('TABLE','V','T5',TABLE(1))
         CALL COPISD('TABLE','V','T4',TABLE(2))
         CALL DETRSD('TABLE','T4')
         CALL DETRSD('TABLE','T5')
         CALL TBFUTB('T4','V',2,TABLE,' ',' ',IBID,RBID,CBID,
     &                K8BID)
         CALL DETRSD('TABLE',TABLE(1))
         CALL DETRSD('TABLE',TABLE(2))
         CALL COPISD('TABLE','V','T4',TABLE(1))
         CALL COPISD('TABLE','V','T3',TABLE(2))
         CALL DETRSD('TABLE','T3')
         CALL DETRSD('TABLE','T4')
         CALL TBFUTB('T4','V',2,TABLE,' ',' ',IBID,RBID,CBID,
     &                K8BID)
C
         CALL COPISD('TABLE','G','T4',RESULT)
C
         CALL DETRSD('TABLE',TABLE(1))
         CALL DETRSD('TABLE',TABLE(2))
         CALL DETRSD('TABLE','T4')
C
      CALL JEDEMA()
      END
