      SUBROUTINE I2FNOE(COURBE,LISTND)
      IMPLICIT NONE
C
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM,JEXATR
      CHARACTER*24 LISTND
      CHARACTER*8  COURBE
C
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C     ------------------------------------------------------------------
C     SAISIE DES NOEUDS MIS EN JEU POUR LE POST-TRAITEMENT D' UN
C     CONCEPT DE TYPE COURBE OU SURFACE
C     ------------------------------------------------------------------
C IN  COURBE : K : NOM DU CONCEPT DE TYPE COURBE OU SURFACE
C OUT LISTND : K : NOM DE L' OJB S V I REALISANT LA SAISIE
C     ------------------------------------------------------------------
C     LE VECTEUR LISTND EST DE DIMENSION NBR_NOEUD_ACTIF ET
C     CONTIENT LA LISTE TRIEE DES NOEUDS ACTIFS
C     ------------------------------------------------------------------
C
C
C
      CHARACTER*8  TYPCRB,NMAILA
      CHARACTER*15 NCONEC
      CHARACTER*19 NMAIL1
      INTEGER      ADR,PT,NBND,NBTND,ALSTND,M,P,NBPART,IM,IN,N,I
      INTEGER      ADRNDM,ADRM1,NBMAIL,NBNDM,VLCMAI,VLCCOU,ASDS,ACONEC
      CHARACTER*1 K1BID
C
C================= CORPS DE LA ROUTINE ===============================
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      CALL JEMARQ()
      NBND = 0
      PT   = 1
      CALL JEEXIN(COURBE//'.NOMA',N)
      IF ( N .NE. 0 ) THEN
         CALL JEVEUO(COURBE//'.NOMA','L',ADR)
      ELSE
         CALL JEVEUO(COURBE//'.NOMMAIL','L',ADR)
      ENDIF
      NMAILA = ZK8(ADR)
      NCONEC = NMAILA//'.CONNEX'
      CALL DISMOI('F','NB_NO_MAILLA',NMAILA,'MAILLAGE',NBTND,K1BID,I)
      CALL JEVEUO(JEXATR(NCONEC,'LONCUM'),'L',VLCMAI)
      CALL JEVEUO(JEXNUM(NCONEC,1),'L',ACONEC)
      CALL WKVECT('&&I2FNOE.VEC.TEMP','V V I',NBTND,ADR)
      DO 10, I = 1, NBTND, 1
         ZI(ADR + I-1) = 0
10    CONTINUE
      IF ( N .NE. 0 ) THEN
         CALL JELIRA(COURBE//'.NSDS','LONMAX',NBPART,K1BID)
         CALL JEVEUO(COURBE//'.NSDS','L',ASDS)
         DO 100, P = 1, NBPART, 1
            NMAIL1 = ZK24(ASDS+P-1)(1:13)//'.MAIL'
            CALL JEVEUO(JEXATR(NMAIL1,'LONCUM'),'L',VLCCOU)
            CALL JELIRA(NMAIL1,'NMAXOC',NBMAIL,K1BID)
            CALL JEVEUO(JEXNUM(NMAIL1,1),'L',ADRM1)
            DO 110, IM = 1, NBMAIL, 1
               M = ZI(ADRM1 + ZI(VLCCOU + IM-1)-1)
               ADRNDM = ACONEC + ZI(VLCMAI + M-1)-1
               NBNDM  = ZI(VLCMAI + M  ) - ZI(VLCMAI + M-1)
               DO 111, IN = 1, NBNDM, 1
                  N = ZI(ADRNDM + IN-1)
                  ZI(ADR + N-1) = 1
111            CONTINUE
110         CONTINUE
100      CONTINUE
      ELSE
         CALL JEVEUO(COURBE//'.TYPCOURBE','L',IM)
         TYPCRB = ZK8(IM)
         NMAIL1 = COURBE//'.MAIL1'
         CALL JELIRA(NMAIL1,'NMAXOC',NBPART,K1BID)
         DO 400, P = 1, NBPART, 1
            CALL JELIRA(JEXNUM(NMAIL1,P),'LONMAX',NBMAIL,K1BID)
            CALL JEVEUO(JEXNUM(NMAIL1,P),'L',ADRM1)
            IF ( TYPCRB .EQ. 'LISTMAIL' ) THEN
               NBMAIL = NBMAIL - 1
            ENDIF
            DO 410, IM = 1, NBMAIL, 1
               M = ZI(ADRM1 + IM-1)
CC             CALL JEVEUO(JEXNUM(NCONEC,M),'L',ADRNDM)
CC             CALL JELIRA(JEXNUM(NCONEC,M),'LONMAX',NBNDM)
               ADRNDM = ACONEC + ZI(VLCMAI + M-1)-1
               NBNDM  = ZI(VLCMAI + M  ) - ZI(VLCMAI + M-1)
               DO 411, IN = 1, NBNDM, 1
                  N = ZI(ADRNDM + IN-1)
                  ZI(ADR + N-1) = 1
411            CONTINUE
410         CONTINUE
400      CONTINUE
      ENDIF
      DO 500, I = 1, NBTND, 1
         NBND = NBND + MIN(ZI(ADR + I-1),1)
500   CONTINUE
      CALL WKVECT(LISTND,'V V I',NBND,ALSTND)
      DO 600, I = 1, NBTND, 1
         IF ( ZI(ADR + I-1) .NE. 0 ) THEN
            ZI(ALSTND + PT-1) = I
            PT = PT + 1
         ENDIF
600   CONTINUE
      CALL JEDETR('&&I2FNOE.VEC.TEMP')
      CALL JEDEMA()
      END
