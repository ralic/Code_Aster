      SUBROUTINE RITZ99(NOMRES)
      IMPLICIT REAL*8 (A-H,O-Z)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C***********************************************************************
C  P. RICHARD     DATE 10/02/92
C-----------------------------------------------------------------------
C  BUT : CREATION D'UNE BASE MODALE DE TYPE RITZ (C A D QUELCONQUE)
C-----------------------------------------------------------------------
C
C NOMRES /I/ : NOM K8 DU RESULTAT
C
C-------- DEBUT COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                  ZK32
      CHARACTER*80                                            ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C-----  FIN  COMMUNS NORMALISES  JEVEUX  -------------------------------
C
      COMPLEX*16   CBID
      INTEGER      VALI(3)
      CHARACTER*8  NOMRES,RESUL1,RESUL2,K8B,INTF,LISTAM
      CHARACTER*19 NUMREF
      CHARACTER*24 TRANG1,TRANG2,TEMPOR,TEMPI,TEMPI2
      CHARACTER*24 VALK(3)
      LOGICAL      SEUL
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C
C --- INITIALISATION
C -------SEUL EST LA VARIABLE LOGIQUE POUR L'EXISTENCE D'UNE
C        SEULE INSTANCE MODE_MECA (RESUL1)
      SEUL=.FALSE.
      NBMOD1=0
      NBMOD2=0
      TEMPOR = '&&RITZ99.GLOBAL'
      TRANG1 = '&&RITZ99.NUME.RANG1'
      TRANG2 = '&&RITZ99.NUME.RANG2'
      TEMPI  = '&&RITZ99.LISTE'
      TEMPI2  = '&&RITZ99.LISTE2'
C
C --- RECUPERATION NUMEROTATION DE REFERENCE
C
      CALL JEMARQ()
      CALL JEVEUO(NOMRES//'           .REFD','L',LLREF)
      NUMREF=ZK24(LLREF+3)
C
C --- DETERMINATION DU NOMBRE DE CONCEPT(S) MODE_* (RESUL1) DEJA
C     ISSUS DE DEFI_BASE_MODALE
C
      CALL GETVID('RITZ','BASE_MODALE',1,1,1,RESUL1,IBMO)
C
C --- DETERMINATION DU NOMBRE DE CONCEPT(S) MODE_* (RESUL2)
C
      CALL GETVID('RITZ','MODE_INTF',2,1,1,RESUL2,IBI1)

C SI IBMO <> 0 ALORS LE CONCEP EST REENTRANT
C DEBUT DE LA BOUCLE DE TRAITEMENT DE "BASE_MODALE"
      IF (IBMO.NE.0) THEN
        CALL GETVIS('RITZ','NMAX_MODE',2,1,1,NBMOD2,IBI4)
        CALL RSORAC(RESUL2,'LONUTI',IBID,BID,K8B,CBID,EBID,'ABSOLU',
     &            NBOLD,1,NBID)
        IF (IBI4.EQ.0) THEN
          NBMODB = NBOLD
        ELSE
          NBMODB = MIN(NBMOD2,NBOLD)
        ENDIF
C
        CALL DISMOI('F','NB_MODES_TOT',RESUL1,'RESULTAT',NBMOD1,
     &              K8B,IER)
        CALL DISMOI('F','NB_MODES_STA',RESUL1,'RESULTAT',NBDEF,
     &              K8B,IER)

C
C --- DETERMINATION NOMBRE TOTAL
C
        NBTOT=NBMOD1+NBMODB
        IF (NBTOT.LE.0) THEN
          CALL U2MESG('F', 'ALGORITH14_50',0,' ',0,0,0,0.D0)
        ENDIF
C
C --- ALLOCATION DE LA STRUCTURE DE DONNEES BASE_MODALE
C
        IF (NOMRES.NE.RESUL1) THEN
          CALL RSCRSD('G',NOMRES,'MODE_MECA',NBTOT)
        ELSE
          CALL RSORAC(RESUL1,'LONUTI',IBID,BID,K8B,CBID,EBID,'ABSOLU',
     &              NBOLD,1,NBID)
          IF (NBTOT.GT.NBOLD) CALL RSAGSD(NOMRES,NBTOT)
          CALL JEVEUO(NOMRES//'           .REFD','E',LDREF)
          CALL GETVID('    ','NUME_REF',1,1,1,NUMREF,IBID)
          IF (IBID.EQ.0) THEN
            CALL U2MESG('E', 'ALGORITH17_8',0,' ',0,0,0,0.D0)
          ENDIF
          NUMREF(15:19)='.NUME'
          CALL GETVID('  ','INTERF_DYNA',1,1,0,INTF,IOCI)
          IF(IOCI.LT.0) THEN
            CALL GETVID('  ','INTERF_DYNA',1,1,1,INTF,IOCI)
          ELSE
            INTF=' '
          ENDIF
          ZK24(LDREF) = ' '
          ZK24(LDREF+1) = ' '
          ZK24(LDREF+2) = ' '
          ZK24(LDREF+3) = NUMREF(1:14)
          ZK24(LDREF+4) = INTF
          ZK24(LDREF+5) = ' '
          ZK24(LDREF+6) = 'RITZ'
        ENDIF

        IF (NBMOD1.GT.0) THEN
          CALL WKVECT(TRANG1,'V V I',NBMOD1,LRANG1)
          DO 31 II=1,NBMOD1
            ZI(LRANG1+II-1)=II
 31       CONTINUE
          INORD=1
          CALL MOCO99(NOMRES,RESUL1,NBMOD1,ZI(LRANG1),INORD,.TRUE.)
          CALL JEDETR(TRANG1)
        ENDIF
        IF (NBMODB.GT.0) THEN
          CALL WKVECT(TRANG2,'V V I',NBMOD2,LRANG2)
          DO 32 II=1,NBMODB
            ZI(LRANG2+II-1)=II
 32       CONTINUE
          CALL MOCO99(NOMRES,RESUL2,NBMODB,ZI(LRANG2),INORD,.FALSE.)
          CALL JEDETR(TRANG2)
        ENDIF
        NBMODA = NBMOD1 - NBDEF
        NBMODB = NBMODB + NBDEF
        GOTO 40
      ENDIF
C---- FIN DE LA BOUCLE DE TRAITEMENT "BASE_MODALE"
C
C --- DETERMINATION DU NOMBRE DE CONCEPT(S) MODE_MECA
C
      CALL GETVID('RITZ','MODE_MECA',1,1,0,K8B,NBGL)
      NBGL = -NBGL
      IF (NBGL.EQ.0) THEN
         CALL U2MESG('F', 'ALGORITH14_51',0,' ',0,0,0,0.D0)
      ENDIF
      IF (NBGL.EQ.1)
     & CALL GETVID('RITZ','MODE_MECA',1,1,1,RESUL1,IBID)
      IF (NBGL.GT.1) THEN
       CALL WKVECT(TEMPOR,'V V K8',NBGL,IDGL)
       CALL WKVECT(TEMPI,'V V I',NBGL,IDOR)
C  ---ON RECUPERE ICI LE NB DE VAL DE LA LISTE NMAX_MODE
       CALL GETVIS('RITZ','NMAX_MODE',1,1,0,IBID,NBLI)
       NBLI=-NBLI
       IF ((NBLI.NE.0).AND.(NBLI.NE.NBGL)) THEN
         VALI(1)=NBGL
         VALI(2)=NBLI
         CALL U2MESG('F', 'ALGORITH14_31',0,' ',2,VALI,0,0.D0)
       ENDIF
       CALL GETVID('RITZ','MODE_MECA',1,1,NBGL,ZK8(IDGL),NBG)
       CALL GETVIS('RITZ','NMAX_MODE',1,1,NBLI,ZI(IDOR),NBI)
      ENDIF

C
C --- DETERMINATION NOMBRE ET NUMERO ORDRE MODE
C
      IF (IBI1.EQ.0) THEN
C ----ON N'A QUE MODE_MECA ET PAS DE MODES D'INTERFACE------
         SEUL=.TRUE.
      ENDIF

      IF (NBGL.EQ.1) THEN
        CALL GETVIS('RITZ','NMAX_MODE',1,1,1,NBMOD1,IBI5)
        NBMODA = NBMOD1
        CALL RSORAC(RESUL1,'LONUTI',IBID,BID,K8B,CBID,EBID,'ABSOLU',
     &            NBOLD,1,NBID)
        IF (IBI5.EQ.0) THEN
          NBMODA = NBOLD
        ELSE
          NBMODA = MIN(NBMOD1,NBOLD)
        ENDIF
      ELSEIF (NBGL.GT.1) THEN
        NBMODA=0
        CALL GETVIS('RITZ','NMAX_MODE',1,1,1,NBMOD1,IBI5)
        CALL WKVECT(TEMPI2,'V V I',NBGL,LNBM)
        DO 30 I =1,NBGL
          CALL RSORAC(ZK8(IDGL+I-1),'LONUTI',IBID,BID,K8B,CBID,EBID,
     &           'ABSOLU', NBOLD,1,NBID)
          IF (IBI5.EQ.0) THEN
            NBMODA = NBMODA+NBOLD
            ZI(LNBM+I-1)=NBOLD
          ELSE
            NBMODA = NBMODA+MIN(ZI(IDOR+I-1),NBOLD)
            ZI(LNBM+I-1)=MIN(ZI(IDOR+I-1),NBOLD)
          ENDIF
 30     CONTINUE
      ENDIF

      IF (NBMODA.GT.0) THEN
        CALL WKVECT(TRANG1,'V V I',NBMODA,LRANG1)
        DO 10 II=1,NBMODA
          ZI(LRANG1+II-1)=II
 10     CONTINUE
      ENDIF

      IF (.NOT.SEUL) THEN
        CALL GETVIS('RITZ','NMAX_MODE',2,1,1,NBMOD2,IBI6)
        CALL RSORAC(RESUL2,'LONUTI',IBID,BID,K8B,CBID,EBID,'ABSOLU',
     &            NBOLD,1,NBID)
        IF (IBI6.EQ.0) THEN
          NBMODB = NBOLD
        ELSE
          NBMODB = MIN(NBMOD2,NBOLD)
        ENDIF
        IF (NBMODB.GT.0) THEN
          CALL WKVECT(TRANG2,'V V I',NBMODB,LRANG2)
          DO 11 II=1,NBMODB
            ZI(LRANG2+II-1)=II
 11       CONTINUE
        ENDIF
      ELSE
        NBMODB=0
      ENDIF
C
C
C --- DETERMINATION NOMBRE TOTAL
C
      NBTOT=NBMODA+NBMODB

C --- ON AJOUTE LA LIST_AMOR--------------------------------------

      CALL GETVID(' ','LIST_AMOR',0,1,0,K8B,NAM)
      IF (NAM.NE.0) THEN
        CALL GETVID(' ','LIST_AMOR',0,1,1,LISTAM,N)
        CALL JELIRA(LISTAM//'           .VALE','LONMAX',NBAMOR,K8B)
        IF (NBAMOR.GT.NBMODA) THEN
          VALI (1) = NBMODA
          VALI (2) = NBAMOR
          VALI (3) = NBMODA
          VALK (1) = 'PREMIERS COEFFICIENTS'
          CALL U2MESG('A','ALGORITH16_18',1,VALK,3,VALI,0,0.D0)
          CALL WKVECT('&&RITZ99.AMORTI','V V R8',NBMODA,JAMOG)

          CALL JEVEUO(LISTAM//'           .VALE','L',IAMOG)
          DO 33 IAM = 1,NBMODA
            ZR(JAMOG+IAM-1) = ZR(IAMOG+IAM-1)
 33       CONTINUE
        ELSE IF (NBAMOR.LT.NBMODA) THEN
          CALL WKVECT('&&RITZ99.AMORTI','V V R8',NBAMOR,JAMOG)
          CALL JEVEUO(LISTAM//'           .VALE','L',IAMOG)
          DO 41 IAM = 1,NBAMOR
            ZR(JAMOG+IAM-1) = ZR(IAMOG+IAM-1)
 41       CONTINUE
          IDIFF = NBMODA - NBAMOR
          VALI (1) = IDIFF
          VALI (2) = NBMODA
          VALI (3) = IDIFF
          CALL U2MESI('I','ALGORITH16_19',3,VALI)
          CALL WKVECT('&&RITZ99.AMORTI2','V V R8',NBMODA,JAMO2)
          DO 51 IAM = 1,NBAMOR
              ZR(JAMO2+IAM-1) = ZR(JAMOG+IAM-1)
 51       CONTINUE
          DO 61 IAM = NBAMOR + 1,NBMODA
              ZR(JAMO2+IAM-1) = ZR(JAMOG+NBAMOR-1)
 61       CONTINUE
          JAMOG = JAMO2
        ELSE IF (NBAMOR.EQ.NBMODA) THEN
          CALL WKVECT('&&RITZ99.AMORTI','V V R8',NBAMOR,JAMOG)
          CALL JEVEUO(LISTAM//'           .VALE','L',IAMOG)
          DO 71 IAM = 1,NBAMOR
            ZR(JAMOG+IAM-1) = ZR(IAMOG+IAM-1)
 71       CONTINUE
        END IF
C   ----ON AJOUTE LA LIST_AMOR COMME VALEURS DU PARAM 'AMOR_REDUIT'
C       DU RESULT1 (SI UN SEUL MODE_MECA)
        IF (NBGL.EQ.1) THEN
          DO 81 IAM = 1,NBMODA
            CALL RSADPA(RESUL1,'E',1,'AMOR_REDUIT',IAM,0,IAMOR,K8B)
            ZR(IAMOR) = ZR(JAMOG+IAM-1)
 81       CONTINUE
        ENDIF
      ENDIF

C
C --- ALLOCATION DE LA STRUCTURE DE DONNEES BASE_MODALE
C
      IF (NBTOT.GT.0) THEN

        CALL RSCRSD('G',NOMRES,'MODE_MECA',NBTOT)
      ELSE
        CALL U2MESG('F', 'ALGORITH14_50',0,' ',0,0,0,0.D0)
      ENDIF
C
C --- COPIE DES MODES DYNAMIQUES
C
      INORD=1
      IF (NBMODA.GT.0) THEN
        IF (NBGL.EQ.1) THEN
          CALL MOCO99(NOMRES,RESUL1,NBMODA,ZI(LRANG1),INORD,.TRUE.)
        ELSEIF (NBGL.GT.1) THEN
          DO 20 I =1,NBGL
             CALL MOCO99(NOMRES,ZK8(IDGL+I-1),ZI(LNBM+I-1),ZI(LRANG1),
     &                   INORD,.TRUE.)
 20       CONTINUE
          INORD = INORD + NBMODA
        ENDIF

        CALL JEDETR(TRANG1)
      ENDIF
C
      IF (.NOT.SEUL) THEN
        IF (NBMODB.GT.0) THEN
          CALL MOCO99(NOMRES,RESUL2,NBMODB,ZI(LRANG2),INORD,.FALSE.)
          CALL JEDETR(TRANG2)
        ENDIF
      ENDIF

C     SI EN ENTREE DE LA DEFI_BASE_MODALE ON N'A QUE UN SEUL CONCEPT
C     MODE_MECA, ON COPIE SES OBJETS DU .REFD (LES MATRICES) DANS LE
C     CONCEPT SORTANT
      IF ((SEUL).AND.(NBGL.EQ.1)) THEN
        CALL JEVEUO(NOMRES//'           .REFD','E',JRFN)
        CALL JEVEUO(RESUL1//'           .REFD','L',JRFO)
        ZK24(JRFN) = ZK24(JRFO)
        ZK24(JRFN+1) = ZK24(JRFO+1)
        ZK24(JRFN+2) = ZK24(JRFO+2)
        CALL GETVID('    ','NUME_REF',1,1,1,NUMREF,IBID)
        IF (IBID.GT.0) THEN
          CALL U2MESG('A', 'ALGORITH17_7',0,' ',0,0,0,0.D0)
        ENDIF
        ZK24(JRFN+3) = ZK24(JRFO+3)
      ENDIF




 40   CONTINUE


      CALL JEDETR(TEMPOR)
      CALL JEDETR(TRANG1)
      CALL JEDETR(TRANG2)
      CALL JEDETR(TEMPI)
      CALL JEDETR(TEMPI2)
      CALL JEDETR('&&RITZ99.AMORTI')
C
      CALL JEDEMA()
      END
