      SUBROUTINE RVFMAI(COURBE,LISTMA)
      IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*24 LISTMA
      CHARACTER*8  COURBE
C
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 21/02/96   AUTEUR VABHHTS J.PELLET 
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
C     ------------------------------------------------------------------
C     SAISIE DES MAILLES POUR LE POST-TRAITEMENT D' UNE COURBE
C     ------------------------------------------------------------------
C IN  COURBE : K : NOM DU CONCEPT DE TYPE COURBE
C IN  LISTMA : K : NOM DE L' OJB  S V I REALISANT LA SAISIE
C     ------------------------------------------------------------------
C     LE VECTEUR LISTMA EST DE DIMENSION NBR_MAILLE_ACTIVE ET CONTIENT
C     LES NUMEROS DES MAILLES ACTIVES DANS L' ORDRE CROISSANT
C     ------------------------------------------------------------------
C
      CHARACTER*32 JEXNOM,JEXNUM,JEXATR
C
      INTEGER         ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8          ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16      ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL         ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8     ZK8
      CHARACTER*16    ZK16
      CHARACTER*24    ZK24
      CHARACTER*32    ZK32
      CHARACTER*80    ZK80
      COMMON /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
      CHARACTER*8  TYPCRB,NMAILA
      CHARACTER*14 NMAIL1,NMAIL2
      CHARACTER*24 NMAIL
      INTEGER      ADR,PT,NBMA,NBTMA,ALSTMA,M,P,NBPART,IM,I
      INTEGER      ADRM1,NBMAIL,ADRM2,D3,ASDS,VLCCOU
      CHARACTER*1 K1BID
C
C================= CORPS DE LA ROUTINE ===============================
C
      CALL JEMARQ()
      NBMA = 0
      PT   = 1
      CALL JEEXIN(COURBE//'.NOMA',D3)
      IF ( D3 .NE. 0 ) THEN
         CALL JEVEUO(COURBE//'.NOMA','L',ADR)
      ELSE
         CALL JEVEUO(COURBE//'.NOMMAIL','L',ADR)
      ENDIF
      NMAILA = ZK8(ADR)
      CALL DISMOI('F','NB_MA_MAILLA',NMAILA,'MAILLAGE',NBTMA,K1BID,I)
      CALL WKVECT('&&RVFMAI.VEC.TEMP','V V I',NBTMA,ADR)
      DO 10, I = 1, NBTMA, 1
         ZI(ADR + I-1) = 0
10    CONTINUE
      IF ( D3 .NE. 0 ) THEN
         CALL JELIRA(COURBE//'.NSDS','LONMAX',NBPART,K1BID)
         CALL JEVEUO(COURBE//'.NSDS','L',ASDS)
         DO 100, P = 1, NBPART, 1
            NMAIL  = ZK24(ASDS + P-1)(1:13)//'.MAIL'
            CALL JEVEUO(JEXATR(NMAIL,'LONCUM'),'L',VLCCOU)
            CALL JELIRA(NMAIL,'NMAXOC',NBMAIL,K1BID)
            CALL JEVEUO(NMAIL,'L',ADRM1)
            DO 110, IM = 1, ZI(VLCCOU + NBMAIL)-1,1
               M = ZI(ADRM1 + IM-1)
               ZI(ADR + M-1) = 1
110         CONTINUE
100      CONTINUE
      ELSE
         CALL JEVEUO(COURBE//'.TYPCOURBE','L',IM)
         TYPCRB = ZK8(IM)
         NMAIL1 = COURBE//'.MAIL1'
         NMAIL2 = COURBE//'.MAIL2'
         CALL JELIRA(NMAIL1,'NMAXOC',NBPART,K1BID)
         DO 200, P = 1, NBPART, 1
            CALL JELIRA(JEXNUM(NMAIL1,P),'LONMAX',NBMAIL,K1BID)
            CALL JEVEUO(JEXNUM(NMAIL1,P),'L',ADRM1)
            CALL JEVEUO(JEXNUM(NMAIL2,P),'L',ADRM2)
            IF ( TYPCRB .EQ. 'LISTMAIL' ) THEN
               NBMAIL = NBMAIL - 1
            ENDIF
            DO 210, IM = 1, NBMAIL, 1
               M = ZI(ADRM1 + IM-1)
               ZI(ADR + M-1) = 1
               M = ZI(ADRM2 + IM-1)
               IF ( M .GT. 0 ) THEN
                  ZI(ADR + M-1) = 1
               ENDIF
210         CONTINUE
200      CONTINUE
      ENDIF
C
      DO 300, I = 1, NBTMA, 1
         NBMA = NBMA + MAX(0,ZI(ADR + I-1))
300   CONTINUE
      CALL WKVECT(LISTMA,'V V I',NBMA,ALSTMA)
      DO 400, I = 1, NBTMA, 1
         IF ( ZI(ADR + I-1) .NE. 0 ) THEN
            ZI(ALSTMA + PT-1) = I
            PT = PT + 1
         ENDIF
400   CONTINUE
      CALL JEDETR('&&RVFMAI.VEC.TEMP')
      CALL JEDEMA()
      END
