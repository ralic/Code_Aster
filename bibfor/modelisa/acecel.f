      SUBROUTINE ACECEL (NOMA,NOMO,NBOCC,NBEPO,NBEDI,NBECO,NBECA,NBEBA,
     &                   NBEMA,NBTEL,NTYELE,NPOUTR,NDISCR,
     &                   NCOQUE,NCABLE,NBARRE,NMASSI,NGRILL,NGRIBT,
     &                   JDLM,JDLN,IER)

      IMPLICIT NONE
C
      INCLUDE 'jeveux.h'
      INTEGER           NBOCC(*),NBEPO,NBEDI,NBECO,NBECA,NBEBA,NBEMA,
     &                  NBTEL
      INTEGER           NTYELE(*),NPOUTR,NDISCR,NCOQUE,NCABLE,NBARRE,
     &                  NMASSI,NGRILL,NGRIBT
C
      CHARACTER*8       NOMA,NOMO
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C TOLE CRP_21
C ----------------------------------------------------------------------
C     AFFE_CARA_ELEM
C     COMPTEUR D'ELEMENTS
C ----------------------------------------------------------------------
C IN  : NOMA   : NOM DU MAILLAGE
C IN  : NOMO   : NOM DU MODELE
C ----------------------------------------------------------------------
      CHARACTER*8  NOMU
      CHARACTER*16 CONCEP, CMD
      CHARACTER*24 MLGNMA, MODMAI, MODNOE, MODNEM
      CHARACTER*1 K1BID
C     ------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      INTEGER I ,IER ,IFM ,IUNIFI ,IXMA ,IXNO ,IXNW 
      INTEGER JDLM ,JDLN ,JDME ,JDNE ,JDNW ,K ,NBMAIL 
      INTEGER NBMTRD ,NUMMAI ,NUMNOE ,NUTYEL 
C-----------------------------------------------------------------------
      CALL JEMARQ()
      CALL GETRES(NOMU,CONCEP,CMD)
      MODMAI = NOMO//'.MAILLE'
      MODNOE = NOMO//'.NOEUD'
      MODNEM = NOMO//'.MODELE    .NEMA'
      MLGNMA = NOMA//'.NOMMAI'
      CALL JEEXIN(MODNEM,IXNW)
      CALL JEEXIN(MODMAI,IXMA)
      CALL JEEXIN(MODNOE,IXNO)
      CALL JELIRA(MLGNMA,'NOMMAX',NBMAIL,K1BID)
      NBMTRD = 0
      IF (IXNW.NE.0) THEN
         CALL JELIRA(MODNEM,'NMAXOC',NBMTRD,K1BID)
         CALL JEVEUO(MODNEM,'L',JDNW)
      ENDIF
      IF (IXMA.NE.0) CALL JEVEUO(MODMAI,'L',JDME)
      IF (IXNO.NE.0) CALL JEVEUO(MODNOE,'L',JDNE)
      IFM = IUNIFI('MESSAGE')
C
      NPOUTR = 0
      NDISCR = 0
      NCOQUE = 0
      NCABLE = 0
      NBARRE = 0
      NMASSI = 0
      NGRILL = 0
      NGRIBT = 0
C
      DO 40 NUMMAI = 1 , NBMAIL
         NUTYEL = ZI(JDME+NUMMAI-1)
         ZI(JDLM+NUMMAI-1) = NUTYEL
         DO 41 I = 1 , NBEPO
            IF (NUTYEL.EQ.NTYELE(I)) NPOUTR = NPOUTR + 1
 41      CONTINUE
         DO 42 I = NBEPO+1 , NBEPO+NBEDI
            IF (NUTYEL.EQ.NTYELE(I)) NDISCR = NDISCR + 1
 42      CONTINUE
         DO 43 I = NBEPO+NBEDI+1 , NBEPO+NBEDI+NBECO
            IF (NUTYEL.EQ.NTYELE(I)) NCOQUE = NCOQUE + 1
 43      CONTINUE
         DO 44 I = NBEPO+NBEDI+NBECO+1 , NBEPO+NBEDI+NBECO+NBECA
            IF(NUTYEL.EQ.NTYELE(I))NCABLE = NCABLE + 1
 44      CONTINUE
         DO 45 I = NBEPO+NBEDI+NBECO+NBECA+1 ,
     &             NBEPO+NBEDI+NBECO+NBECA+NBEBA
            IF(NUTYEL.EQ.NTYELE(I))NBARRE = NBARRE + 1
 45      CONTINUE
         DO 46 I= NBEPO+NBEDI+NBECO+NBECA+NBEBA+1 ,
     &            NBEPO+NBEDI+NBECO+NBECA+NBEBA+NBEMA
            IF(NUTYEL.EQ.NTYELE(I))NMASSI = NMASSI + 1
 46      CONTINUE
         DO 48 I= NBEPO+NBEDI+NBECO+NBECA+NBEBA+NBEMA+1 , NBTEL
            IF(NUTYEL.EQ.NTYELE(I))NGRIBT = NGRIBT + 1
 48      CONTINUE
C
 40   CONTINUE
      IF (IXNW.NE.0) THEN
         DO 50 K = 1 , NBMTRD
            NUMNOE = ZI(JDNW+K*2-2)
            NUTYEL = ZI(JDNE+NUMNOE-1)
            ZI(JDLN+K-1) = NUTYEL
            DO 52 I = NBEPO+1 , NBEPO+NBEDI
               IF(NUTYEL.EQ.NTYELE(I))NDISCR = NDISCR + 1
 52         CONTINUE
 50      CONTINUE
      ENDIF
      WRITE(IFM,1000)NOMO
      IF (NPOUTR.GT.0) WRITE(IFM,1041)NPOUTR
      IF (NDISCR.GT.0) WRITE(IFM,1042)NDISCR
      IF (NCOQUE.GT.0) WRITE(IFM,1043)NCOQUE
      IF (NCABLE.GT.0) WRITE(IFM,1044)NCABLE
      IF (NBARRE.GT.0) WRITE(IFM,1045)NBARRE
      IF (NGRILL.GT.0) WRITE(IFM,1047)NGRILL
      IF (NGRIBT.GT.0) WRITE(IFM,1048)NGRIBT
 1000 FORMAT(/,5X,'LE MODELE ',A8,' CONTIENT : ')
 1041 FORMAT(35X,I6,' ELEMENT(S) POUTRE')
 1042 FORMAT(35X,I6,' ELEMENT(S) DISCRET')
 1043 FORMAT(35X,I6,' ELEMENT(S) COQUE')
 1044 FORMAT(35X,I6,' ELEMENT(S) CABLE')
 1045 FORMAT(35X,I6,' ELEMENT(S) BARRE')
 1047 FORMAT(35X,I6,' ELEMENT(S) ASSE_GRIL')
 1048 FORMAT(35X,I6,' ELEMENT(S) GRILLE')
C
C --- VERIFICATION DE LA COHERENCE DES  AFFECTATIONS
C     ----------------------------------------------
      IF (NBOCC(1).NE.0 .AND. NPOUTR.EQ.0) THEN
         CALL U2MESK('E','MODELISA_29',1,NOMO)
         IER = IER + 1
      ENDIF
      IF (NBOCC(2).NE.0 .AND. NCOQUE.EQ.0) THEN
         CALL U2MESK('E','MODELISA_30',1,NOMO)
         IER = IER + 1
      ENDIF
      IF ((NBOCC(3).NE.0 .OR. NBOCC(13).NE.0) .AND. NDISCR.EQ.0) THEN
         CALL U2MESK('E','MODELISA_31',1,NOMO)
         IER = IER + 1
      ENDIF
      IF (NBOCC(4).NE.0 .AND. NPOUTR.EQ.0 .AND. NDISCR.EQ.0 .AND.
     &                                          NBARRE.EQ.0) THEN
         CALL U2MESK('E','MODELISA_32',1,NOMO)
         IER = IER + 1
      ENDIF
      IF (NBOCC(5).NE.0 .AND. NPOUTR.EQ.0) THEN
         CALL U2MESK('E','MODELISA_29',1,NOMO)
         IER = IER + 1
      ENDIF
      IF (NBOCC(6).NE.0 .AND. NCABLE.EQ.0) THEN
         CALL U2MESK('E','MODELISA_33',1,NOMO)
         IER = IER + 1
      ENDIF
      IF (NBOCC(7).NE.0 .AND. NBARRE.EQ.0) THEN
         CALL U2MESK('E','MODELISA_34',1,NOMO)
         IER = IER + 1
      ENDIF
      IF (NBOCC(8).NE.0 .AND. NMASSI.EQ.0) THEN
         CALL U2MESK('E','MODELISA_35',1,NOMO)
         IER = IER + 1
      ENDIF
      IF (NBOCC(11).NE.0 .AND. NGRIBT.EQ.0) THEN
         CALL U2MESK('E','MODELISA_36',1,NOMO)
         IER = IER + 1
      ENDIF
C
      CALL JEDEMA()
      END
