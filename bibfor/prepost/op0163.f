      SUBROUTINE OP0163()
      IMPLICIT NONE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C     INTERFACE ASTER - MISS3D : COMMANDE  LIRE_MISS_3D
C     ------------------------------------------------------------------
C
      INCLUDE 'jeveux.h'
      INTEGER         IER, ITRESR(3), ITRESI(3)
      INTEGER      ULISOP
      CHARACTER*4   TYPE(3)
      CHARACTER*8  K8B, NOMRES, INTERF
      CHARACTER*8  MAEL, BASEMO
      CHARACTER*14 NUMDDL
      CHARACTER*16 CONCEP, NOMCMD, TYPRES, K16NOM
      CHARACTER*19 KREFE, KINST, KNUME
      CHARACTER*24  REFE, MATRK
      CHARACTER*24   MATRM, CHAMNO, CHAMN1, CHAMN2
      CHARACTER*32 FICHI
      CHARACTER*64 BASE
      CHARACTER*80 TITRE
      INTEGER      IARG
C     ------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER I ,IADRIF ,IARCH ,IBI ,ICH ,IDBASE ,IDRESI 
      INTEGER IDRESR ,IE ,IFMIS ,IMESS ,IORD ,IRET ,IUNIFI 
      INTEGER J ,JINST ,JNUME ,JREFE ,LINST ,LREFE ,LVAL1 
      INTEGER LVAL2 ,LVALE ,NBCHAM ,NBINST ,NBMODD ,NBMODE ,NBMODS 
      INTEGER NBSAUV ,NBSTO ,NEQ ,NF ,NMM ,NTI ,NU 

C-----------------------------------------------------------------------
      DATA  REFE  /'                  _REFE'/
      DATA  KINST /'&&OP0163.INSTANT'/
      DATA  KNUME /'&&OP0163.NUME_RANG'/
      DATA  CHAMNO/'&&OP0163.CHAMNO'/
      DATA  CHAMN1/'&&OP0163.CHAMN1'/
      DATA  CHAMN2/'&&OP0163.CHAMN2'/
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFMAJ()
      IMESS = IUNIFI('MESSAGE')
      CALL GETRES( NOMRES , CONCEP , NOMCMD )
      WRITE(IMESS,'(''TYPE DE RESULTAT :'',1X,A16)') CONCEP
C
C     ----- RECUPERATION UNITE DE MISS ---
      CALL GETVIS ( ' ', 'UNITE', 1,IARG,1, IFMIS, NU )
      CALL GETVTX ( ' ', 'NOM'  , 1,IARG,1, FICHI, NF )
      IF ( NF .EQ. 0 ) THEN
        K16NOM = ' '
        IF ( ULISOP ( IFMIS, K16NOM ) .EQ. 0 )  THEN
          CALL ULOPEN ( IFMIS,' ',' ','NEW','O')
        ENDIF
      ELSE
        BASE = './tmp_miss3d/'//FICHI
        CALL ULOPEN ( IFMIS, BASE, ' ', 'NEW', 'O' )
      ENDIF
C
      CALL GETVTX(' ','TITRE',1,IARG,1,TITRE,NTI)
      IF (NTI.NE.0) WRITE(IMESS,'(A80)') TITRE

C     ----- RECUPERATION DES MODES -----
      CALL GETVID(' ','MACR_ELEM_DYNA',1,IARG,1,MAEL,NMM)
      REFE(1:18) = MAEL//'.MAEL_RAID'
      CALL JEVEUO(REFE,'L',JREFE)
      MATRK = ZK24(JREFE+1)
      REFE(1:18) = MAEL//'.MAEL_MASS'
      CALL JEVEUO(REFE,'L',JREFE)
      BASEMO = ZK24(JREFE)
      MATRM  = ZK24(JREFE+1)
      CALL JEVEUO(BASEMO//'           .REFD','L',IADRIF)
      INTERF = ZK24(IADRIF+4) (1:8)
C
      CALL DISMOI('F','NB_MODES_DYN',BASEMO,'RESULTAT',NBMODD,K8B,IER)
      CALL DISMOI('F','NB_MODES_STA',BASEMO,'RESULTAT',NBMODS,K8B,IER)

      NBMODE = NBMODD + NBMODS
      WRITE(IMESS,'(1X,I6,1X,''MODES DYNAMIQUES'')') NBMODD
      WRITE(IMESS,'(1X,I6,1X,''MODES STATIQUES'')') NBMODS
      READ(IFMIS,1000) NBINST
      NBSAUV = NBINST
      NBSTO = NBMODE * NBSAUV
C     ----- RECUPERATION TYPE DE RESULTAT ---

      TYPRES = CONCEP
      NBCHAM = 3
      TYPE(1) = 'DEPL'
      TYPE(2) = 'VITE'
      TYPE(3) = 'ACCE'
      CALL DISMOI('F','NOM_NUME_DDL',MATRM,'MATR_ASSE',IBI,NUMDDL,IRET)
      CALL DISMOI('F','NB_EQUA',MATRM,'MATR_ASSE',NEQ,K8B,IRET)
      CALL WKVECT('&&OP0163.BASEMO','V V R',NBMODE*NEQ,IDBASE)
      IF (INTERF.NE.' ') THEN
       CALL COPMOD(BASEMO,'DEPL',NEQ,NUMDDL,NBMODE,ZR(IDBASE))
      ELSE
       CALL COPMO2(BASEMO,NEQ,NUMDDL,NBMODE,ZR(IDBASE))
      ENDIF
      CALL WKVECT('&&OP0163.DEPLR','V V R',NBSTO,ITRESR(1))
      CALL WKVECT('&&OP0163.VITER','V V R',NBSTO,ITRESR(2))
      CALL WKVECT('&&OP0163.ACCER','V V R',NBSTO,ITRESR(3))
      CALL WKVECT('&&OP0163.DEPLI','V V R',NBSTO,ITRESI(1))
      CALL WKVECT('&&OP0163.VITEI','V V R',NBSTO,ITRESI(2))
      CALL WKVECT('&&OP0163.ACCEI','V V R',NBSTO,ITRESI(3))
      CALL WKVECT(KNUME,'V V I' ,NBINST,JNUME)
      CALL WKVECT(KINST,'V V R8',NBINST,JINST)
      DO 60 IORD = 0, NBINST-1
         ZI(JNUME+IORD) = IORD + 1
 60   CONTINUE
      READ(IFMIS,1001) (ZR(JINST+IORD-1),IORD=1,NBINST)
      READ(IFMIS,1001) ((ZR(ITRESR(1)+J-1+(ZI(JNUME+I-1)-1)*NBMODE),
     &             J=1,NBMODD),I=1,NBINST)
      READ(IFMIS,1001) ((ZR(ITRESR(2)+J-1+(ZI(JNUME+I-1)-1)*NBMODE),
     &             J=1,NBMODD),I=1,NBINST)
      READ(IFMIS,1001) ((ZR(ITRESR(3)+J-1+(ZI(JNUME+I-1)-1)*NBMODE),
     &             J=1,NBMODD),I=1,NBINST)
      READ(IFMIS,1001) ((ZR(ITRESR(1)+J-1+(ZI(JNUME+I-1)-1)*NBMODE),
     &             J=NBMODD+1,NBMODE),I=1,NBINST)
      READ(IFMIS,1001) ((ZR(ITRESR(2)+J-1+(ZI(JNUME+I-1)-1)*NBMODE),
     &             J=NBMODD+1,NBMODE),I=1,NBINST)
      READ(IFMIS,1001) ((ZR(ITRESR(3)+J-1+(ZI(JNUME+I-1)-1)*NBMODE),
     &             J=NBMODD+1,NBMODE),I=1,NBINST)
      WRITE(IMESS,'(''PARAMETRES DE CALCUL :'',/6(1X,1PE12.5))')
     & (ZR(JINST+IORD-1),IORD=1,NBINST)
      IF (TYPRES.NE.'DYNA_HARMO') GOTO 9998
      READ(IFMIS,1000) NBINST
      READ(IFMIS,1001) (ZR(JINST+IORD-1),IORD=1,NBINST)
      READ(IFMIS,1001) ((ZR(ITRESI(1)+J-1+(ZI(JNUME+I-1)-1)*NBMODE),
     &             J=1,NBMODD),I=1,NBINST)
      READ(IFMIS,1001) ((ZR(ITRESI(2)+J-1+(ZI(JNUME+I-1)-1)*NBMODE),
     &             J=1,NBMODD),I=1,NBINST)
      READ(IFMIS,1001) ((ZR(ITRESI(3)+J-1+(ZI(JNUME+I-1)-1)*NBMODE),
     &             J=1,NBMODD),I=1,NBINST)
      READ(IFMIS,1001) ((ZR(ITRESI(1)+J-1+(ZI(JNUME+I-1)-1)*NBMODE),
     &             J=NBMODD+1,NBMODE),I=1,NBINST)
      READ(IFMIS,1001) ((ZR(ITRESI(2)+J-1+(ZI(JNUME+I-1)-1)*NBMODE),
     &             J=NBMODD+1,NBMODE),I=1,NBINST)
      READ(IFMIS,1001) ((ZR(ITRESI(3)+J-1+(ZI(JNUME+I-1)-1)*NBMODE),
     &             J=NBMODD+1,NBMODE),I=1,NBINST)
 9998 CONTINUE
      IARCH = 0
      CALL RSCRSD('G',NOMRES,TYPRES,NBINST)
      IF (TYPRES.EQ.'DYNA_HARMO') THEN
        CALL VTCREM(CHAMN1,MATRM,'V','R')
        CALL VTCREM(CHAMN2,MATRM,'V','R')
      ENDIF
      DO 90 I = 0,NBINST-1
         IARCH = IARCH + 1
         DO 92 ICH = 1,NBCHAM
            CALL RSEXCH(NOMRES,TYPE(ICH),IARCH,CHAMNO,IRET)
            IF ( IRET .EQ. 0 ) THEN
             CALL U2MESK('A','ALGORITH2_64',1,CHAMNO)
            ELSEIF ( IRET .EQ. 100 ) THEN
              IF (TYPRES.EQ.'DYNA_HARMO') THEN
               CALL VTCREM(CHAMNO,MATRM,'G','C')
              ELSE
               CALL VTCREM(CHAMNO,MATRM,'G','R')
              ENDIF
            ELSE
               CALL ASSERT(.FALSE.)
            ENDIF
            IDRESR = ITRESR(ICH)
            CHAMNO(20:24) = '.VALE'
            CALL JEVEUO(CHAMNO,'E',LVALE)
            IF (TYPRES.EQ.'DYNA_HARMO') THEN
             IDRESI = ITRESI(ICH)
             CHAMN1(20:24) = '.VALE'
             CALL JEVEUO(CHAMN1,'E',LVAL1)
             CHAMN2(20:24) = '.VALE'
             CALL JEVEUO(CHAMN2,'E',LVAL2)
             CALL MDGEPH(NEQ,NBMODE,ZR(IDBASE),
     &                  ZR(IDRESR+(ZI(JNUME+I)-1)*NBMODE),ZR(LVAL1))
             CALL MDGEPH(NEQ,NBMODE,ZR(IDBASE),
     &                  ZR(IDRESI+(ZI(JNUME+I)-1)*NBMODE),ZR(LVAL2))
             DO 93 IE = 1,NEQ
               ZC(LVALE+IE-1) = DCMPLX(ZR(LVAL1+IE-1),ZR(LVAL2+IE-1))
 93          CONTINUE
            ELSE
             CALL MDGEPH(NEQ,NBMODE,ZR(IDBASE),
     &                  ZR(IDRESR+(ZI(JNUME+I)-1)*NBMODE),ZR(LVALE))
            ENDIF
            CALL JELIBE(CHAMNO)
            CALL RSNOCH(NOMRES,TYPE(ICH),IARCH)
 92      CONTINUE
         IF (TYPRES.EQ.'DYNA_HARMO') THEN
           CALL RSADPA(NOMRES,'E',1,'FREQ',IARCH,0,LINST,K8B)
         ELSE
           CALL RSADPA(NOMRES,'E',1,'INST',IARCH,0,LINST,K8B)
         ENDIF
         ZR(LINST) = ZR(JINST+I)
 90   CONTINUE
C
      KREFE  = NOMRES
      CALL WKVECT(KREFE//'.REFD','G V K24',7,LREFE)
      ZK24(LREFE) = MATRK
      ZK24(LREFE+1  ) = MATRM
      ZK24(LREFE+2) = '  '
      ZK24(LREFE+3) = '  '
      ZK24(LREFE+4) = '  '
      ZK24(LREFE+5) = '  '
      ZK24(LREFE+6) = '  '
      CALL JELIBE(KREFE//'.REFD')
C
 1000 FORMAT(I6)
 1001 FORMAT(6(1PE12.5))
C
      CALL JEDEMA()
      END
