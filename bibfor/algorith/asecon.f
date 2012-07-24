      SUBROUTINE ASECON ( NOMSY, NEQ, MOME, RESU )
      IMPLICIT  NONE
      INCLUDE 'jeveux.h'
      INTEGER             NEQ
      CHARACTER*16        NOMSY
      CHARACTER*(*)       MOME, RESU
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 24/07/2012   AUTEUR PELLET J.PELLET 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C     ------------------------------------------------------------------
C     COMMANDE : COMB_SISM_MODAL
C        CALCUL DES TERMES D'ENTRAINEMENT
C     ------------------------------------------------------------------
C IN  : NOMSY  : OPTION DE CALCUL
C IN  : NEQ    : NOMBRE D'EQUATIONS
C IN  : NOME   : MODES MECANIQUES
C IN  : RESU   : NOM UTILISATEUR DE LA COMMANDE
C     ------------------------------------------------------------------
      INTEGER      IAD, IBID, ICAS, IDEP, IDIR, IER, II, IN, INO,IOC,
     &             IOCC, IORDR, IORST, IRET, JABS, JAUX, JCAS, JCUM,
     &             JDIR, JLIN, JNO, JORD, JQUA, JREP, JSTA, JTYP,
     &             JVALE, JVAL1, LNOD, NBMODE, NBNO, NBOC, NBTROU,
     &             NCAS, NDEP, NUCAS, NUME
      REAL*8       R8B,R8VIDE,EPSMAC,XXX,XX1,XX2,XX3
      COMPLEX*16   CBID
      CHARACTER*8  K8B, NOEU, CMP, NOMCMP(3),KNUM,KDIR,STAT
      CHARACTER*8  MECA,OCCUR
      CHARACTER*16 MONACC,CONCEP,NOMCMD,DEF
      CHARACTER*19 CHEXTR,CHAMP,MONCHA
      CHARACTER*24  VALE,NOMS2,VALK(3)
      INTEGER      IARG
C     ------------------------------------------------------------------
      DATA  NOMCMP / 'DX' , 'DY' , 'DZ' /
      DATA  VALE / '                   .VALE' /
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL GETFAC('COMB_DEPL_APPUI',NBOC)
      CALL GETFAC('DEPL_MULT_APPUI',NDEP)
      CALL GETVID(' ','MODE_MECA',1,IARG,1,MECA,IBID)
C
      CALL WKVECT('&&ASECON.CUMUL','V V R',NEQ,JCUM)
      CALL WKVECT('&&ASECON.AUX','V V R',NEQ*NBOC,JAUX)

      EPSMAC = R8VIDE()
      NBMODE = 10
      II = 0
C
C -- PREPARATION DU STOCKAGE DE LA REPONSE SECONDAIRE
C
      CALL GETRES(K8B,CONCEP,NOMCMD)
C     --- CREATION DE LA STRUCTURE D'ACCUEIL ---
      CALL RSEXIS(RESU,IER)
      IF (IER.EQ.0) CALL RSCRSD('G',RESU,CONCEP,NBMODE)
      NOMS2 = NOMSY
      IF (NOMSY(1:4).EQ.'VITE') NOMS2 = 'DEPL'
C
      CALL RSORAC(MOME,'TOUT_ORDRE',IBID,R8B,K8B,CBID,R8B,K8B,
     &    IORDR,1,NBTROU)

      CALL RSEXCH('F',MECA,NOMS2,IORDR,MONCHA,IER)
      DEF = 'SECONDAIRE'
      IORDR = 200
C           --- CHAMP PAR OCCURENCE DE COMB_DPL_APPUI ---
C
      CALL JEVEUO('&&ASENAP.TYPE','L',JTYP)
      CALL WKVECT('&&ASECON.REP','V V R',NEQ,JREP)
      CALL WKVECT('&&ASECON.QUAD','V V R',NEQ,JQUA)
      CALL WKVECT('&&ASECON.LINE','V V R',NEQ,JLIN)
      CALL WKVECT('&&ASECON.ABS','V V R',NEQ,JABS)
      CALL JEEXIN('&&ASECON.NORD',IRET)
      IF (IRET.EQ.0) THEN
        CALL WKVECT('&&ASECON.NORD','V V I',NBOC+1,JORD)
      ELSE
        CALL JEVEUO('&&ASECON.NORD','E',JORD)
      ENDIF

      DO 10 IOCC =1,NBOC
C
C POUR CHAQUE OCCURENCE ON STOQUE LE CHAMP
C
        CALL RSEXCH(' ',RESU,NOMSY,IORDR,CHAMP,IER)
        IF ( IER .EQ. 100 ) THEN
          CALL VTDEFS(CHAMP,MONCHA,'G','R')
        ELSE
          VALK (1) = NOMSY
          VALK (2) = CHAMP
          CALL U2MESG('F','SEISME_25',2,VALK,1,IOCC,0,0.D0)
        ENDIF
        VALE(1:19) = CHAMP
        CALL JEEXIN(VALE(1:19)//'.VALE',IBID)
        IF (IBID.GT.0) THEN
           VALE(20:24)='.VALE'
        ELSE
           VALE(20:24)='.CELV'
        END IF
        CALL JEVEUO(VALE,'E',JVALE)
C
        DO 4 IN =1,NEQ
         ZR(JQUA+IN-1)= 0.0D0
         ZR(JLIN+IN-1)= 0.0D0
         ZR(JABS+IN-1)= 0.0D0
 4      CONTINUE
        CALL JELIRA(JEXNUM('&&ASENAP.LISTCAS',IOCC),'LONMAX',
     &                                                   NCAS,K8B)
        CALL JEVEUO(JEXNUM('&&ASENAP.LISTCAS',IOCC),'L',JCAS)
        DO 20 ICAS = 1,NCAS
          NUCAS = ZI(JCAS+ICAS-1)
          DO 40 IDEP = 1,NDEP
            CALL GETVIS('DEPL_MULT_APPUI','NUME_CAS',IDEP,IARG,1,
     &                  NUME,IBID)
             IF (NUME.EQ.NUCAS) THEN
               KNUM = 'N       '
               CALL CODENT(NUCAS, 'D0' , KNUM(2:8) )
               KDIR = 'D       '
               CALL CODENT(NUCAS, 'D0' , KDIR(2:8) )
               CALL JELIRA (JEXNOM('&&ASENAP.LINOEU',KNUM),
     &              'LONMAX',NBNO,K8B)
               CALL JEVEUO (JEXNOM('&&ASENAP.LINOEU',KNUM),
     &              'L', JNO )
               LNOD = 3*NBNO
               CALL JELIRA (JEXNOM('&&ASENAP.LIDIR',KDIR),'LONMAX',
     &              LNOD,K8B)
               CALL JEVEUO (JEXNOM('&&ASENAP.LIDIR',KDIR), 'L', JDIR )
               CALL JEVEUO('&&ASENAP.STAT','L',JSTA)
               STAT = ZK8(JSTA+ICAS-1)
               DO 12 INO = 1,NBNO
                 NOEU =ZK8(JNO+INO-1)
                 DO 14 IDIR =  1,3
                   IF (ZR(JDIR+3*(INO-1)+IDIR-1).NE.EPSMAC) THEN
                     CMP = NOMCMP(IDIR)
                     MONACC = NOEU//CMP
                     XX1    = ZR(JDIR+3*(INO-1)+IDIR-1)
                     CALL RSORAC(STAT,'NOEUD_CMP',IBID,R8B,MONACC,CBID,
     &                      R8B,K8B,IORST,1,NBTROU)
                     CALL RSEXCH('F',STAT,NOMSY,IORST,CHEXTR,IRET)
                     CALL JEEXIN(CHEXTR//'.VALE',IBID)
                     IF (IBID.GT.0) THEN
                        CALL JEVEUO(CHEXTR//'.VALE','L',JVAL1)
                     ELSE
                        CALL JEVEUO(CHEXTR//'.CELV','L',JVAL1)
                     END IF
                     DO 16 IN = 1,NEQ
                       ZR(JREP+IN-1) = ZR(JVAL1+IN-1) * XX1
 16                  CONTINUE
                  IF (ZI(JTYP+IOCC-1).EQ.1) THEN
C                 --- COMBINAISON QUADRATIQUE ---
                    DO 24 IN = 1,NEQ
                     XXX = ZR(JREP+IN-1)
                     ZR(JQUA+IN-1)= ZR(JQUA+IN-1)+ XXX*XXX
 24                 CONTINUE
                  ELSEIF (ZI(JTYP+IOCC-1).EQ.2) THEN
C               --- COMBINAISON LINEAIRE ---
                    DO 18 IN = 1,NEQ
                       ZR(JLIN+IN-1)= ZR(JLIN+IN-1)+ ZR(JREP+IN-1)
 18              CONTINUE
              ELSE
C              --- COMBINAISON VALEUR ABSOLUE ---
               DO 22 IN = 1,NEQ
                  XX1         = ABS(ZR(JREP+IN-1))
                  ZR(JABS+IN-1)= ZR(JABS+IN-1)+ XX1
 22            CONTINUE
              ENDIF
            ENDIF
 14        CONTINUE
 12       CONTINUE
         ENDIF
 40     CONTINUE
 20    CONTINUE
       DO 26 IN =1,NEQ
          XX1 = ZR(JLIN+IN-1)
          XX2 = ZR(JABS+IN-1)
          XX3 = SQRT(ZR(JQUA+IN-1))
          ZR(JVALE+IN-1) = XX1 + XX2 + XX3
          II = II + 1
          ZR(JAUX+II-1) =  ZR(JVALE+IN-1)
 26    CONTINUE

        CALL RSNOCH(RESU,NOMSY,IORDR)
        CALL RSADPA(RESU,'E',1,'NOEUD_CMP',IORDR,0,IAD,K8B)
        CALL CODENT(IOCC, 'D' , OCCUR )
        ZK16(IAD) = 'COMBI'// OCCUR
        CALL RSADPA(RESU,'E',1,'TYPE_DEFO',IORDR,0,IAD,K8B)
        ZK16(IAD) = DEF
        CALL JELIBE(VALE)

        ZI(JORD+IOCC-1) = IORDR
        IORDR = IORDR + 1
 10   CONTINUE
      ZI(JORD+NBOC) = IORDR
C
      CALL RSEXCH(' ',RESU,NOMSY,IORDR,CHAMP,IER)
      IF ( IER .EQ. 100 ) THEN
        CALL VTDEFS(CHAMP,MONCHA,'G','R')
      ELSE
        VALK(1) = NOMSY
        VALK(2) = CHAMP
        CALL U2MESG('F','SEISME_25',2,VALK,1,IORDR,0,0.D0)
      ENDIF
      VALE(1:19) = CHAMP
      CALL JEEXIN(VALE(1:19)//'.VALE',IBID)
      IF (IBID.GT.0) THEN
         VALE(20:24)='.VALE'
      ELSE
         VALE(20:24)='.CELV'
      END IF
      CALL JEVEUO(VALE,'E',JVALE)
C
       DO 32 IOC = 1,NBOC
       DO 30 IN = 1,NEQ
          XX1 =  ZR(JAUX+(IOC-1)*NEQ+IN-1)
          ZR(JCUM+IN-1) = ZR(JCUM+IN-1)+XX1*XX1
 30    CONTINUE
 32   CONTINUE
C STOCKAGE DU CUMUL QUADRATIQUE
      DO 34 IN = 1, NEQ
          ZR(JVALE+IN-1) = SQRT( ABS ( ZR(JCUM+IN-1) ) )
 34   CONTINUE
      CALL JELIBE(VALE)
      CALL RSNOCH(RESU,NOMSY,IORDR)

C        --- PARAMETRE ---
      CALL RSADPA(RESU,'E',1,'NOEUD_CMP',IORDR,0,IAD,K8B)
      ZK16(IAD) = 'CUMUL'//' QUAD'
      CALL RSADPA(RESU,'E',1,'TYPE_DEFO',IORDR,0,IAD,K8B)
      ZK16(IAD) = DEF

      CALL JEDETR('&&ASECON.CUMUL')
      CALL JEDETR('&&ASECON.AUX')
      CALL JEDETR('&&ASECON.REP')
      CALL JEDETR('&&ASECON.QUAD')
      CALL JEDETR('&&ASECON.LINE')
      CALL JEDETR('&&ASECON.ABS')
      CALL JEDEMA()
      END
