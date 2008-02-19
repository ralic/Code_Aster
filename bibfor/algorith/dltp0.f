      SUBROUTINE DLTP0 (T0)
      IMPLICIT  REAL*8  (A-H,O-Z)
      REAL*8    T0
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/02/2008   AUTEUR MACOCCO K.MACOCCO 
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
C OUT : T0   INSTANT INITIAL
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER           ZI
      INTEGER VALI
      COMMON / IVARJE / ZI(1)
      REAL*8            ZR
      REAL*8 VALR
      COMMON / RVARJE / ZR(1)
      COMPLEX*16        ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL           ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8       ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                       ZK24
      CHARACTER*32                                ZK32
      CHARACTER*80                                         ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32      JEXNUM, JEXNOM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
      CHARACTER*1   K1BID
      CHARACTER*24 VALK
      CHARACTER*8   K8B, NOMRES, DYNA, LI, CRIT, CTYPE
      CHARACTER*16  TYPRES, NOMCMD
      COMPLEX*16    C16B
C     -----------------------------------------------------------------
      CALL JEMARQ()
      CALL GETRES(NOMRES,TYPRES,NOMCMD)
C
C     --- EST-ON EN REPRISE ? ---
C
      CALL GETVID('ETAT_INIT','DYNA_TRANS',1,1,1,DYNA,NDY)
      IF ( NDY .NE. 0 ) THEN
         CALL GETVIS('ETAT_INIT','NUME_INIT' ,1,1,1,NUME,NNI)
         IF ( NNI .EQ. 0 ) THEN
            CALL GETVR8('ETAT_INIT','INST_INIT',1,1,1,TEMPS,NT)
            IF ( NT .EQ. 0 ) THEN
               CALL RSORAC(DYNA,'DERNIER',IBID,TEMPS,K8B,C16B,
     &                                         PREC,CRIT,NUME,1,NBTROU)
               IF (NBTROU.NE.1) THEN
                CALL U2MESS('F','ALGORITH3_24')
               ENDIF
            ELSE
               CALL GETVR8('ETAT_INIT','PRECISION',1,1,1,PREC ,NP)
               CALL GETVTX('ETAT_INIT','CRITERE'  ,1,1,1,CRIT ,NC)
               CALL RSORAC(DYNA,'INST',IBID,TEMPS,K8B,C16B,
     &                                        PREC,CRIT,NUME,1,NBTROU)
               IF (NBTROU.LT.0) THEN
                  VALK = DYNA
                  VALR = TEMPS
                  VALI = -NBTROU
      CALL U2MESG('F', 'ALGORITH12_83',1,VALK,1,VALI,1,VALR)
               ELSEIF (NBTROU.EQ.0) THEN
                  VALK = DYNA
                  VALR = TEMPS
                  CALL U2MESG('F', 'ALGORITH12_84',1,VALK,0,0,1,VALR)
               ENDIF
            ENDIF
         ELSE
C           --- VERIFICATION QUE NUME EXISTE ---
            CALL RSORAC(DYNA,'LONUTI',IBID,R8B,K8B,C16B,R8B,K8B,
     &                                                 NBORDR,1,IBID)
            CALL WKVECT('&&OP0048.NUME_ORDRE','V V I',NBORDR,JORDR)
            CALL RSORAC(DYNA,'TOUT_ORDRE',IBID,R8B,K8B,C16B,R8B,K8B,
     &                                        ZI(JORDR),NBORDR,IBID)
            DO 10 I = 1,NBORDR
               IF (ZI(JORDR+I-1).EQ.NUME) GOTO 12
 10         CONTINUE
            CALL U2MESK('F','ALGORITH3_36',1,DYNA)
 12         CONTINUE
         ENDIF
C
C        --- RECUPERATION DE L'INSTANT ---
         CALL RSADPA(DYNA,'L',1,'INST',NUME,1,JADR,CTYPE)
         T0 = ZR(JADR)
      ELSE
C
C     --- DEFINITION DES INSTANTS DE CALCUL A PARTIR DE "LIST_INST" ---
C
         CALL GETVID('INCREMENT','LIST_INST',1,1,1,LI,N1)
         IF (N1.NE.0) THEN
            CALL JEVEUO(LI//'           .BINT','L',JBINT)
            T0 = ZR (JBINT)
         ELSE
C
C     --- DEFINITION DES INSTANTS DE CALCUL A PARTIR DE "FONC_INST" ---
C
            CALL GETVID('INCREMENT','FONC_INST' ,1,1,1,LI ,N2)
            IF (N2.NE.0) THEN
              CALL GETVIS('INCREMENT','PAS_CALCUL',1,1,1,IPC,N3)
              CALL JEVEUO(LI//'           .PROL','L',LPROL)
              IF (ZK24(LPROL).NE.'FONCTION') CALL U2MESS('F','ALGORITH3_
     &32')
              CALL JEVEUO(LI//'           .VALE','L',LVAR)
              T0 = ZR(LVAR)
            ELSE
C
C     --- DEFINITION DE L'INSTANT INITIAL AVEC "INST_INIT" ---
C
              CALL GETVR8('INCREMENT','INST_INIT',1,1,1,T0,NP)
            ENDIF
         ENDIF
      ENDIF
C
      CALL JEDEMA()
      END
