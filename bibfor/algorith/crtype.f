      SUBROUTINE CRTYPE()
      IMPLICIT  NONE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/01/2010   AUTEUR PELLET J.PELLET 
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
C     COMMANDE:  CREA_RESU /AFFE
C     CREE UNE STRUCTURE DE DONNEE DE TYPE "EVOL_THER"
C                                          "EVOL_VARC"
C                                          "EVOL_ELAS"
C                                          "MULT_ELAS"
C                                          "FOURIER_ELAS"
C                                          "FOURIER_THER"
C                                          "DYNA_TRANS"
C                                          "DYNA_HARMO"
C                                          "EVOL_CHAR"
C                                          "MODE_MECA"
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      CHARACTER*32 JEXNUM,JEXNOM
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------

      INTEGER MXPARA,IBID,IER,LG,ICOMPT,IRET,NBFAC,IOCC,NUMINI,NUMFIN,
     &        N0,N1,N2,N3,NIS,NBINST,IP,NBVAL,NUME,IADESC,IGD,L,I,J,JC,
     &        JCHAM,JCOOR,IAD,JINST,JVAL,JNOMF,JDEEQ,LPROL,NBPF,
     &        INO,NBV,JREFE,JLCHA,NCHAR,JINFC,JFCHA
      PARAMETER (MXPARA=10)
      INTEGER RSMXNO,NBTROU,JCPT,NBR,IVMX,K
      INTEGER VALI,JREFD
      INTEGER NFR,NBFREQ,N4
      REAL*8 VALPU(MXPARA),RBID,TPS,PREC,VALR(3),FREQ
      COMPLEX*16 CBID
      LOGICAL LNCAS,IDENSD,LFONC
      CHARACTER*4 TYPABS
      CHARACTER*6 TYPEGD
      CHARACTER*24 VALKK(2)
      CHARACTER*8 K8B,RESU,NOMF,NOMA,TYPMOD,CRITER,MATR
      CHARACTER*8 MODELE,MATERI,CARELE,BLAN8,VALK(3),NOMA2
      CHARACTER*14 NUMEDD
      CHARACTER*16 NOMP(MXPARA),TYPE,OPER,ACCES,K16B
      CHARACTER*19 NOMCH,CHAMP,CHAMP1,LISTR8,EXCIT,PCHN1,RESU19
      CHARACTER*19 NOMFCT
      CHARACTER*24 K24,LINST,NSYMB,TYPRES,LCPT,O1,O2,PROFCH,NOOJB

      DATA LINST,LISTR8,LCPT/'&&CRTYPE_LINST','&&CRTYPE_LISR8',
     &     '&&CPT_CRTYPE'/
C ----------------------------------------------------------------------
      CALL JEMARQ()

      BLAN8 = ' '
      EXCIT = ' '

      CALL GETRES(RESU,TYPE,OPER)
      RESU19=RESU
      CALL GETFAC('AFFE',NBFAC)
      CALL GETVTX(' ','NOM_CHAM',1,1,1,NSYMB,N1)
      CALL GETVTX(' ','TYPE_RESU',1,1,1,TYPRES,N1)

      CALL JEEXIN(RESU//'           .DESC',IRET)
      IF (IRET.EQ.0) CALL RSCRSD('G',RESU,TYPRES,100)

      LNCAS = .FALSE.
      IF (TYPRES.EQ.'MULT_ELAS' .OR. TYPRES.EQ.'FOURIER_ELAS' .OR.
     &    TYPRES.EQ.'FOURIER_THER' .OR. TYPRES.EQ.'MODE_MECA') THEN
        LNCAS = .TRUE.
      ENDIF

      NUMINI = -1
      ICOMPT = -1
      PROFCH = ' '

      CALL WKVECT('&&CRTYPE.CHAMPS','V V K8',NBFAC,JCHAM)

      DO 80 IOCC = 1,NBFAC

        MODELE = ' '
        CALL GETVID('AFFE','MODELE',IOCC,1,1,MODELE,N1)

        MATERI = BLAN8
        CALL GETVID('AFFE','CHAM_MATER',IOCC,1,1,MATERI,N1)

        CARELE = BLAN8
        CALL GETVID('AFFE','CARA_ELEM',IOCC,1,1,CARELE,N1)

C       -- POUR STOCKER INFO_CHARGE DANS LE PARAMETRE EXCIT :
        CALL GETVID('AFFE','CHARGE',IOCC,1,0,K8B,N1)
        IF (N1.LT.0) THEN
          NCHAR=-N1
          NOOJB ='12345678'//'.1234'//'.EXCIT.INFC'
          CALL GNOMSD(NOOJB,10,13)
          EXCIT = NOOJB(1:19)
C         -- ON CREE LA SD_INFO_CHARGE :
          CALL LISCCR(EXCIT,NCHAR,'G')
          CALL JEVEUO(EXCIT//'.LCHA','E',JLCHA)
          CALL JEVEUO(EXCIT//'.FCHA','E',JFCHA)
          CALL GETVID('AFFE','CHARGE',IOCC,1,NCHAR,ZK24(JLCHA),N1)
          IF (.FALSE.) THEN
            NOMFCT='&&CRTYPE.CONST1'
            CALL FOCSTE(NOMFCT,'TOUTRESU',1.D0,'G')
            DO 99,I=1,NCHAR
              ZK24(JFCHA-1+I)=NOMFCT
 99         CONTINUE
          ENDIF
        ENDIF

        CALL GETVID('AFFE','CHAM_GD',IOCC,1,1,CHAMP,N1)
        ZK8(JCHAM+IOCC-1) = CHAMP(1:8)
        CALL DISMOI('F','NOM_MAILLA',CHAMP,'CHAMP',IBID,NOMA,IER)
        IF (MODELE.NE.' ') THEN
          CALL DISMOI('F','NOM_MAILLA',MODELE,'MODELE',IBID,NOMA2,IER)
          IF (NOMA.NE.NOMA2) THEN
            VALK(1)=NOMA
            VALK(2)=NOMA2
            CALL U2MESK('F','ALGORITH2_1',2,VALK)
          ENDIF
        ENDIF

        CALL DISMOI('F','TYPE_SUPERVIS',CHAMP,'CHAMP',IBID,K24,IER)
        CALL JEVEUO(NOMA//'.COORDO    .VALE','L',JCOOR)

C       CALCUL DE LFONC ET TYPEGD :

        LFONC = .FALSE.
        DO 10 I = 24,1,-1
          IF (K24(I:I).EQ.' ') GOTO 10
          IF (K24(I-1:I).EQ.'_F') THEN
            IF (K24(1:7).NE.'CHAM_NO') CALL U2MESK('F','ALGORITH2_45',1,
     &          K24)
            LFONC = .TRUE.
            TYPEGD = K24(I-5:I-2)//'_R'

          ELSEIF (K24(I-1:I).EQ.'_R') THEN
            TYPEGD = K24(I-5:I)

          ELSEIF (K24(I-1:I).EQ.'_C') THEN
            TYPEGD = K24(I-5:I)

          ELSE
            CALL U2MESK('F','ALGORITH2_46',1,K24)
          ENDIF
          GOTO 20

   10   CONTINUE
   20   CONTINUE


        IF (K24(1:7).EQ.'CHAM_NO') THEN
C ----- ON CHERCHE A ECONOMISER LES PROF_CHNO (PARTAGE SI POSSIBLE)
          IF (PROFCH.EQ.' ') THEN
            CALL DISMOI('F','PROF_CHNO',CHAMP,'CHAM_NO',IBID,PCHN1,IER)
            NOOJB = '12345678.PRCHN00000.PRNO'
            CALL GNOMSD(NOOJB,15,19)
            PROFCH = NOOJB(1:19)
            CALL COPISD('PROF_CHNO','G',PCHN1,PROFCH)

          ELSE
            CALL DISMOI('F','PROF_CHNO',CHAMP,'CHAM_NO',IBID,PCHN1,IER)
            IF (.NOT.IDENSD('PROF_CHNO',PROFCH,PCHN1)) THEN
              NOOJB = '12345678.PRCHN00000.PRNO'
              CALL GNOMSD(NOOJB,15,19)
              PROFCH = NOOJB(1:19)
              CALL COPISD('PROF_CHNO','G',PCHN1,PROFCH)
            ENDIF
          ENDIF
        ENDIF


C ----- MOT CLE "NOM_CAS", "NUME_MODE", "FREQ"  PRESENT :
        IF (LNCAS) THEN
          CALL RSORAC(RESU,'LONUTI',IBID,RBID,K8B,CBID,RBID,K8B,NUMINI,
     &                1,NBTROU)
          NUMINI = NUMINI + 1

          CALL RSEXCH(RESU,NSYMB,NUMINI,NOMCH,IRET)
          IF (IRET.EQ.0) THEN
            VALKK(1) = CHAMP(1:8)
            VALI = NUMINI
            CALL U2MESG('A','ALGORITH12_74',1,VALKK,1,VALI,0,0.D0)
          ELSEIF (IRET.EQ.110) THEN
            CALL RSAGSD(RESU,0)
            CALL RSEXCH(RESU,NSYMB,NUMINI,NOMCH,IRET)
          ELSEIF (IRET.EQ.100) THEN
          ELSE
            CALL U2MESK('F','ALGORITH2_47',1,NSYMB)
          ENDIF

          CALL COPISD('CHAMP_GD','G',CHAMP,NOMCH)
          IF (K24(1:7).EQ.'CHAM_NO') THEN
            CALL DISMOI('F','PROF_CHNO',NOMCH,'CHAM_NO',IBID,PCHN1,IER)
            IF (PCHN1.NE.PROFCH) THEN
              CALL DETRSD('PROF_CHNO',PCHN1)
              CALL JEVEUO(NOMCH//'.REFE','E',JREFE)
              ZK24(JREFE+1) = PROFCH
            ENDIF
          ENDIF
          CALL RSNOCH(RESU,NSYMB,NUMINI,' ')

          CALL RSSEPA(RESU,NUMINI,MODELE,MATERI,CARELE,EXCIT)

          CALL GETVTX('AFFE','NOM_CAS',IOCC,1,1,ACCES,N0)
          IF (N0.NE.0) THEN
            CALL RSADPA(RESU,'E',1,'NOM_CAS',NUMINI,0,IAD,K8B)
            ZK16(IAD) = ACCES
          ENDIF

          CALL GETVIS('AFFE','NUME_MODE',IOCC,1,1,NUME,N0)
          IF (N0.NE.0) THEN
            CALL RSADPA(RESU,'E',1,'NUME_MODE',NUMINI,0,IAD,K8B)
            ZI(IAD) = NUME
          ENDIF

          CALL GETVTX('AFFE','TYPE_MODE',IOCC,1,1,TYPMOD,N0)
          IF (N0.NE.0) THEN
            CALL RSADPA(RESU,'E',1,'TYPE_MODE',NUMINI,0,IAD,K8B)
            ZK8(IAD) = TYPMOD
          ENDIF

          CALL GETVR8('AFFE','FREQ',IOCC,1,1,FREQ,N0)
          IF (N0.NE.0) THEN
            CALL RSADPA(RESU,'E',1,'FREQ',NUMINI,0,IAD,K8B)
            ZR(IAD) = FREQ
          ENDIF

          GOTO 80

        ENDIF


C ----- MOT CLE INST/FREQ PRESENT :
        NIS = 0
        NFR = 0
        NBINST = 0
        CALL GETVR8('AFFE','INST',IOCC,1,0,RBID,NIS)
        CALL GETVR8('AFFE','FREQ',IOCC,1,0,RBID,NFR)
        IF (NIS.NE.0) THEN
           TYPABS = 'INST'
           NBINST = -NIS
        ENDIF
        IF (NFR.NE.0) THEN
           TYPABS = 'FREQ'
           NBINST = -NFR
        ENDIF

        IF ((NIS.NE.0).OR.(NFR.NE.0)) THEN
          CALL WKVECT(LCPT,'V V I',NBINST,JCPT)
          CALL WKVECT(LINST,'V V R',NBINST,JINST)
          CALL GETVR8('AFFE',TYPABS,IOCC,1,NBINST,ZR(JINST),N1)
          CALL GETVR8('AFFE','PRECISION',IOCC,1,1,PREC,IBID)
          CALL GETVTX('AFFE','CRITERE',IOCC,1,1,CRITER,IBID)
          CALL RSORAC(RESU,'LONUTI',IBID,RBID,K8B,CBID,RBID,K8B,NBV,1,
     &                IBID)

          IVMX = RSMXNO(RESU)
          DO 30 K = 1,NBINST
            IF (NBV.GT.0) THEN
              CALL RSORAC(RESU,TYPABS,IBID,ZR(JINST+K-1),K8B,CBID,PREC,
     &                    CRITER,NUME,1,NBR)

            ELSE
              NBR = 0
            ENDIF
            IF (NBR.LT.0) THEN
              CALL U2MESS('F','ALGORITH2_48')

            ELSEIF (NBR.EQ.0) THEN
              ZI(JCPT+K-1) = IVMX + 1
              IVMX = IVMX + 1

            ELSE
              ZI(JCPT+K-1) = NUME
            ENDIF
   30     CONTINUE
        ELSE

C ----- MOT CLE LIST_INST/LIST_FREQ PRESENT :
          N1 = 0
          N4 = 0
          CALL GETVID('AFFE','LIST_INST',IOCC,1,1,LISTR8,N1)
          CALL GETVID('AFFE','LIST_FREQ',IOCC,1,1,LISTR8,N4)
          IF (N1.NE.0) THEN
             TYPABS = 'INST'
          ENDIF
          IF (N4.NE.0) THEN
             TYPABS = 'FREQ'
          ENDIF

          CALL GETVR8('AFFE','PRECISION',IOCC,1,1,PREC,IBID)
          CALL GETVTX('AFFE','CRITERE',IOCC,1,1,CRITER,IBID)
          CALL JELIRA(LISTR8//'.VALE','LONMAX',NBVAL,K8B)

          NBINST = NBVAL
          NUMINI = 1
          NUMFIN = NBINST
          CALL GETVIS('AFFE','NUME_INIT',IOCC,1,1,NUMINI,N2)
          CALL GETVIS('AFFE','NUME_FIN',IOCC,1,1,NUMFIN,N3)
          IF (NUMFIN.GT.NBVAL) NUMFIN = NBVAL
          IF (N2.NE.0 .AND. N3.NE.0) THEN
            IF (NUMFIN.LT.NUMINI) THEN
              CALL U2MESS('F','ALGORITH2_49')
            ENDIF
            NBINST = NUMFIN - NUMINI + 1

          ELSEIF (N2.NE.0) THEN
            NBINST = NBVAL - NUMINI + 1

          ELSEIF (N3.NE.0) THEN
            NBINST = NUMFIN

          ELSE
            NBINST = NBVAL
          ENDIF
          NBINST = MIN(NBINST,NBVAL)

          CALL WKVECT(LINST,'V V R',NBINST,JINST)
          CALL JEVEUO(LISTR8//'.VALE','L',JVAL)
          CALL RSORAC(RESU,'LONUTI',IBID,RBID,K8B,CBID,RBID,K8B,NBV,1,
     &                IBID)
          CALL WKVECT(LCPT,'V V I',NBINST,JCPT)
          IVMX = RSMXNO(RESU)
          J = 0
          DO 40 K = 1,NBVAL
            IF (K.LT.NUMINI) GOTO 40
            IF (K.GT.NUMFIN) GOTO 40
            J = J + 1
            ZR(JINST-1+J) = ZR(JVAL-1+K)
            IF (NBV.GT.0) THEN
              CALL RSORAC(RESU,TYPABS,IBID,ZR(JVAL-1+K),K8B,CBID,PREC,
     &                    CRITER,NUME,1,NBR)

            ELSE
              NBR = 0
            ENDIF
            IF (NBR.LT.0) THEN
              CALL U2MESS('F','ALGORITH2_48')

            ELSEIF (NBR.EQ.0) THEN
              ZI(JCPT+J-1) = IVMX + 1
              IVMX = IVMX + 1

            ELSE
              ZI(JCPT+J-1) = NUME
            ENDIF
   40     CONTINUE
        ENDIF

        DO 70 J = 1,NBINST
          IF (J.GE.2) CALL JEMARQ()
          CALL JERECU('V')
          ICOMPT = ZI(JCPT+J-1)
          TPS = ZR(JINST+J-1)
          CALL RSEXCH(RESU,NSYMB,ICOMPT,NOMCH,IRET)
          IF (IRET.EQ.0) THEN
            CALL RSADPA(RESU,'L',1,TYPABS,ICOMPT,0,IAD,K8B)
            VALKK(1) = ZK8(JCHAM+ICOMPT-1)
            VALKK(2) = CHAMP(1:8)
            VALR(1) = ZR(IAD)
            VALR(2) = TPS
            VALR(3) = PREC
            CALL U2MESG('A','ALGORITH11_87',2,VALKK,0,0,3,VALR)

          ELSEIF (IRET.EQ.110) THEN
            CALL RSAGSD(RESU,0)
            CALL RSEXCH(RESU,NSYMB,ICOMPT,NOMCH,IRET)
          ENDIF

          IF (K24(1:7).EQ.'CHAM_NO') THEN

            O1 = CHAMP//'.DESC'
            O2 = NOMCH//'.DESC'
            CALL JEDUPO(O1,'G',O2,.FALSE.)

            O1 = CHAMP//'.REFE'
            O2 = NOMCH//'.REFE'
            CALL JEDUPO(O1,'G',O2,.FALSE.)

            O1 = CHAMP//'.VALE'
            O2 = NOMCH//'.VALE'
            CALL JEDUPO(O1,'G',O2,.FALSE.)

            CALL JEVEUO(NOMCH//'.REFE','E',JREFE)
            ZK24(JREFE+1) = PROFCH

          ELSE
            CALL COPISD('CHAMP_GD','G',CHAMP,NOMCH)
          ENDIF

          IF (LFONC) THEN
            CALL JELIRA(CHAMP//'.VALE','LONMAX',LG,K8B)
            CALL JEVEUO(CHAMP//'.VALE','L',JNOMF)
            CALL JEVEUO(CHAMP//'.REFE','L',JREFE)
            CALL JEVEUO(ZK24(JREFE+1)(1:19)//'.DEEQ','L',JDEEQ)

            CALL JEVEUO(NOMCH//'.DESC','E',IADESC)
            CALL JENONU(JEXNOM('&CATA.GD.NOMGD',TYPEGD),IGD)
            ZI(IADESC-1+1) = IGD
            CALL JEDETR(NOMCH//'.VALE')
            CALL WKVECT(NOMCH//'.VALE','G V R',LG,JC)

C           -- CHAM_NO DE FONCTIONS A EVALUER
C           ----------------------------------
            CALL JEVEUO(NOMCH//'.VALE','E',JC)
            DO 60 L = 1,LG
              NOMF = ZK8(JNOMF+L-1)
              IF (NOMF.EQ.' ') GOTO 60
              CALL JEVEUO(NOMF//'           .PROL','L',LPROL)
              CALL FONBPA(NOMF,ZK24(LPROL),K16B,MXPARA,NBPF,NOMP)
              INO = ZI(JDEEQ+2* (L-1))
              IF (INO.EQ.0) GOTO 60
              DO 50 IP = 1,NBPF
                IF (NOMP(IP).EQ.'INST') THEN
                  VALPU(IP) = TPS

                ELSEIF (NOMP(IP).EQ.'X') THEN
                  VALPU(IP) = ZR(JCOOR-1+3* (INO-1)+1)

                ELSEIF (NOMP(IP).EQ.'Y') THEN
                  VALPU(IP) = ZR(JCOOR-1+3* (INO-1)+2)

                ELSEIF (NOMP(IP).EQ.'Z') THEN
                  VALPU(IP) = ZR(JCOOR-1+3* (INO-1)+3)

                ELSE
                  CALL U2MESS('F','ALGORITH2_50')
                ENDIF
   50         CONTINUE
              CALL FOINTE('F',NOMF,NBPF,NOMP,VALPU,ZR(JC+L-1),IER)
   60       CONTINUE
          ENDIF

          CALL RSNOCH(RESU,NSYMB,ICOMPT,' ')
          CALL RSADPA(RESU,'E',1,TYPABS,ICOMPT,0,IAD,K8B)
          ZR(IAD) = TPS
          CALL RSSEPA(RESU,ICOMPT,MODELE,MATERI,CARELE,EXCIT)
          IF (J.GE.2) CALL JEDEMA()

   70   CONTINUE

        CALL JEDETR(LINST)
        CALL JEDETR(LCPT)

   80 CONTINUE


C     -- REMPLISSAGE DE .REFD POUR LES MODE_MECA  ET DYNA_*:
      IF (TYPRES(1:9).EQ.'MODE_MECA'
     &     .OR. TYPRES(1:10).EQ.'DYNA_HARMO'
     &     .OR. TYPRES(1:10).EQ.'DYNA_TRANS') THEN
        CALL JEEXIN(RESU19//'.REFD',IER)
        IF (IER.EQ.0) THEN
          CALL WKVECT(RESU19//'.REFD','G V K24',7,JREFD)
        ELSE
          CALL JEVEUO(RESU19//'.REFD','E',JREFD)
        ENDIF
        CALL GETVID(' ','MATR_A',0,1,1,MATR,N1)
        IF (N1.EQ.1) THEN
          ZK24(JREFD-1+1)=MATR
          CALL DISMOI('F','NOM_NUME_DDL',MATR,'MATR_ASSE',IBID,
     &                NUMEDD,IER)
          ZK24(JREFD-1+4)=NUMEDD
        ENDIF
        CALL GETVID(' ','MATR_B',0,1,1,MATR,N1)
        IF (N1.EQ.1) ZK24(JREFD-1+2)=MATR
      ENDIF


      CALL JEDETR('&&CRTYPE.CHAMPS')
      CALL JEDEMA()
      END
