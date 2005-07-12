      SUBROUTINE CRTYPE ( )
      IMPLICIT  NONE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 11/07/2005   AUTEUR VABHHTS J.PELLET 
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
C     COMMANDE:  CREA_RESU
C     CREE UNE STRUCTURE DE DONNEE DE TYPE "EVOL_THER"
C                                          "EVOL_VARC"
C                                          "EVOL_ELAS"
C                                          "MULT_ELAS"
C                                          "FOURIER_ELAS"
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      CHARACTER*32       JEXNUM , JEXNOM
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
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER       MXPARA, IBID, IER, LG, ICOMPT, IRET, NBFAC, IOCC,
     +              NUMINI, NUMFIN, N0, N1, N2, N3, NIS, NBINST, IP,
     +              NBVAL, NUME, IADESC, IGD, L, I, J, JC,
     +              JCOOR, IAD, JINST, JVAL, JNOMF, IAREFE,
     +              JDEEQ, LPROL, NBPF, INO, NBV
      PARAMETER   ( MXPARA = 10 )
      INTEGER       RSMXNO, NBTROU,JCPT,NBR,IVMX,K
      REAL*8        VALPU(MXPARA), RBID, TPS, PREC
      COMPLEX*16    CBID
      LOGICAL       LNCAS, IDENSD, LFONC
      CHARACTER*6   TYPEGD
      CHARACTER*8   K8B, RESU, NOMF, NOMA, TYPMOD, CRITER
      CHARACTER*8   MODELE, MATERI, CARELE, BLAN8
      CHARACTER*16  NOMP(MXPARA), TYPE, OPER, ACCES, K16B
      CHARACTER*19  NOMCH , CHAMP, CHAMP1, LISTR8, EXCIT, PCHN1
      CHARACTER*24  K24, LINST, NSYMB, TYPRES, LCPT,
     +              O1, O2, PROFCH, NOOJB
C
      DATA          LINST,LISTR8,LCPT/'&&CRTYPE_LINST','&&CRTYPE_LISR8',
     &                                '&&CPT_CRTYPE'/
C ----------------------------------------------------------------------
      CALL JEMARQ()
C
      BLAN8 = ' '
      EXCIT = ' '
C
      CALL GETRES ( RESU, TYPE, OPER )
C
      CALL GETFAC ( 'AFFE', NBFAC )
C
      CALL GETVTX ( ' ', 'NOM_CHAM',  1,1,1, NSYMB , N1 )
C
      CALL GETVTX ( ' ', 'TYPE_RESU', 1,1,1, TYPRES, N1 )
C
      CALL JEEXIN ( RESU//'           .DESC', IRET )
      IF ( IRET .EQ. 0 ) THEN
        CALL RSCRSD ( RESU, TYPRES, 100 )
      ENDIF
C
      LNCAS=.FALSE.
      IF ( TYPRES .EQ. 'MULT_ELAS'.OR. TYPRES .EQ. 'FOURIER_ELAS') THEN
         LNCAS=.TRUE.
      END IF
C
      NUMINI = -1
      ICOMPT = -1
      PROFCH = ' '
C
      DO 100 IOCC = 1,NBFAC
C
        MODELE = BLAN8
        CALL GETVID ('AFFE', 'MODELE'    , IOCC,1,1, MODELE, N1 )
C
        MATERI = BLAN8
        CALL GETVID ('AFFE', 'CHAM_MATER', IOCC,1,1, MATERI, N1 )
C
        CARELE = BLAN8
        CALL GETVID ('AFFE', 'CARA_ELEM' , IOCC,1,1, CARELE, N1 )
C
        CALL GETVID ( 'AFFE', 'CHAM_GD'  , IOCC,1,1, CHAMP , N1 )
C
        CALL DISMOI ('F','TYPE_SUPERVIS',CHAMP,'CHAMP',IBID,K24 ,IER)
        CALL DISMOI ('F','NOM_MAILLA'   ,CHAMP,'CHAMP',IBID,NOMA,IER)
        CALL JEVEUO ( NOMA//'.COORDO    .VALE', 'L', JCOOR )
C
C       CALCUL DE LFONC ET TYPEGD :
C
        LFONC = .FALSE.
        DO 110 I = 24, 1, -1
           IF ( K24(I:I) .EQ. ' ' ) GO TO 110
           IF ( K24(I-1:I) .EQ. '_F' ) THEN
             IF (K24(1:7).NE. 'CHAM_NO')  CALL UTMESS('F','CRTYPE',
     &                           'SEULS LES CHAMPS DE FONCTIONS AUX '//
     &                           'NOEUDS SONT EVALUABLES: '//K24)
             LFONC = .TRUE.
             TYPEGD = K24(I-5:I-2)//'_R'
           ELSEIF ( K24(I-1:I) .EQ. '_R' ) THEN
             TYPEGD = K24(I-5:I)
           ELSE
             CALL UTMESS('F','CRTYPE','NOUS TRAITONS LES CHAMPS DE '//
     &                                'REELS ET DE FONCTIONS: .'//K24)
           ENDIF
           GO TO 112
 110    CONTINUE
 112    CONTINUE
C
        IF ( K24(1:7) .EQ.'CHAM_NO' ) THEN
          IF ( PROFCH .EQ. ' ' ) THEN
            CALL DISMOI('F','PROF_CHNO',CHAMP,'CHAM_NO',IBID,PCHN1,IER)
            NOOJB = '12345678.PRCHN00000.PRNO'
            CALL GNOMSD ( NOOJB,15,19 )
            PROFCH = NOOJB(1:19)
            CALL COPISD ( 'PROF_CHNO', 'G', PCHN1, PROFCH )
          ELSE
            CALL DISMOI('F','PROF_CHNO',CHAMP,'CHAM_NO',IBID,PCHN1,IER)
            IF ( .NOT. IDENSD('PROF_CHNO',PROFCH,PCHN1) ) THEN
              NOOJB = '12345678.PRCHN00000.PRNO'
              CALL GNOMSD ( NOOJB,15,19 )
              PROFCH = NOOJB(1:19)
              CALL COPISD ( 'PROF_CHNO', 'G', PCHN1, PROFCH )
            ENDIF
          ENDIF
        ENDIF
C
C ----- MOT CLE "NOM_CAS", "NUME_MODE" PRESENT :
C
        IF ( LNCAS ) THEN
           CALL RSORAC (RESU,'LONUTI',IBID,RBID,K8B,CBID,RBID,K8B,
     &                  NUMINI,1,NBTROU)
          NUMINI = NUMINI + 1
          CALL RSEXCH ( RESU, NSYMB, NUMINI, NOMCH, IRET )
          IF ( IRET .EQ. 0 ) THEN
             CALL UTDEBM('A',OPER,'*** CHAMP DEJA EXISTANT ***')
             CALL UTIMPK('L','IL SERA REMPLACE PAR LE CHAMP',1,
     +                                                     CHAMP(1:8) )
             CALL UTIMPI('S',' POUR LE NUME_ORDRE ',1,NUMINI)
             CALL UTFINM()
          ELSEIF ( IRET .EQ. 110 ) THEN
             CALL RSAGSD ( RESU, 0 )
             CALL RSEXCH ( RESU, NSYMB, NUMINI, NOMCH, IRET )
          ELSEIF ( IRET .EQ. 100 ) THEN
          ELSE
             CALL UTMESS('F','CRTYPE','LE NOM SYMBOLIQUE DU '//
     &                   'CHAMP CHERCHER N EST PAS LICITE.'//NSYMB)
          ENDIF
          CALL COPISD('CHAMP_GD','G',CHAMP,NOMCH)
          CALL RSNOCH ( RESU, NSYMB, NUMINI, ' ' )
C
          CALL GETVTX ( 'AFFE', 'NOM_CAS', IOCC,1,1, ACCES, N0 )
          IF ( N0 .NE. 0 ) THEN
            CALL RSADPA ( RESU,'E',1,'NOM_CAS',NUMINI,0,IAD,K8B)
            ZK16(IAD) = ACCES
            CALL RSSEPA (RESU,NUMINI,MODELE,MATERI,CARELE,EXCIT)
          ELSE
            CALL GETVIS ('AFFE', 'NUME_MODE', IOCC,1,1, NUME, N0 )
            CALL RSADPA ( RESU,'E',1,'NUME_MODE',NUMINI,0,IAD,K8B)
            ZI(IAD) = NUME
            CALL RSSEPA (RESU,NUMINI,MODELE,MATERI,CARELE,EXCIT)
          ENDIF
          IF ( TYPRES .EQ. 'FOURIER_ELAS' ) THEN
            CALL GETVTX ( 'AFFE', 'TYPE_MODE', IOCC,1,1, TYPMOD, N0 )
            CALL RSADPA (RESU,'E',1,'TYPE_MODE',NUMINI,0,IAD,K8B)
            ZK8(IAD) = TYPMOD
            CALL RSSEPA (RESU,NUMINI,MODELE,MATERI,CARELE,EXCIT)
          ENDIF
          GOTO 100
        ENDIF
C
C ----- MOT CLE INST PRESENT :
C
        CALL GETVR8 ( 'AFFE', 'INST', IOCC,1,0, RBID, NIS )
        IF ( NIS .NE. 0 ) THEN
          NBINST = -NIS
          CALL WKVECT ( LCPT, 'V V I', NBINST, JCPT )
          CALL WKVECT ( LINST, 'V V R', NBINST, JINST )
          CALL GETVR8 ( 'AFFE','INST', IOCC,1,NBINST, ZR(JINST), N1)
          CALL GETVR8 ( 'AFFE','PRECISION',1,1,1,PREC,IBID)
          CALL GETVTX ( 'AFFE','CRITERE',1,1,1,CRITER,IBID)
          CALL RSORAC ( RESU,'LONUTI',IBID,RBID,K8B,CBID,RBID,K8B,
     &                  NBV,1,IBID)

          IVMX = RSMXNO(RESU)
          DO 10 K=1,NBINST
            IF (NBV .GT. 0) THEN
              CALL RSORAC ( RESU,'INST',IBID,ZR(JINST+K-1),K8B,CBID,
     &                      PREC,CRITER,NUME,1,NBR)
            ELSE
              NBR = 0
            ENDIF
            IF (NBR.LT.0) THEN
               CALL UTMESS('F','CRTYPE','PLUSIEURS INSTANTS '//
     &                     'CORRESPONDENT A CELUI SPECIFIE SOUS AFFE ')
            ELSE IF (NBR.EQ.0) THEN
              ZI(JCPT + K - 1) = IVMX+1
              IVMX = IVMX+1
            ELSE
              ZI(JCPT + K - 1) = NUME
            ENDIF
 10       CONTINUE

        ELSE
C
C ----- MOT CLE LIST_INST PRESENT :
C
          CALL GETVID ( 'AFFE', 'LIST_INST', IOCC,1,1, LISTR8, N1)
          CALL GETVR8 ( 'AFFE', 'PRECISION', IOCC,1,1, PREC  ,IBID)
          CALL GETVTX ( 'AFFE', 'CRITERE'   ,IOCC,1,1, CRITER,IBID)
          CALL JELIRA ( LISTR8//'.VALE', 'LONMAX', NBVAL, K8B)

          NBINST = NBVAL
          NUMINI = 1
          NUMFIN = NBINST
          CALL GETVIS ( 'AFFE', 'NUME_INIT', IOCC,1,1, NUMINI, N2)
          CALL GETVIS ( 'AFFE', 'NUME_FIN' , IOCC,1,1, NUMFIN, N3)
          IF ( NUMFIN.GT.NBVAL ) NUMFIN = NBVAL
          IF ( N2.NE.0 .AND. N3.NE.0 ) THEN
            IF ( NUMFIN .LT. NUMINI ) THEN
             CALL UTMESS('F','CRTYPE','NUME_FIN INFERIEUR A NUME_INIT')
            ENDIF
            NBINST = NUMFIN - NUMINI + 1
          ELSEIF ( N2.NE.0 ) THEN
            NBINST = NBVAL - NUMINI + 1
          ELSEIF ( N3.NE.0 ) THEN
            NBINST = NUMFIN
          ELSE
            NBINST = NBVAL
          ENDIF
          NBINST = MIN( NBINST , NBVAL )

          CALL WKVECT ( LINST, 'V V R', NBINST, JINST )
          CALL JEVEUO ( LISTR8//'.VALE', 'L', JVAL )
          CALL RSORAC ( RESU,'LONUTI',IBID,RBID,K8B,CBID,RBID,K8B,
     &                  NBV,1,IBID)
          CALL WKVECT ( LCPT, 'V V I', NBINST, JCPT )
          IVMX = RSMXNO(RESU)
          J = 0
          DO 25 K=1,NBVAL
            IF ( K .LT. NUMINI ) GOTO 25
            IF ( K .GT. NUMFIN ) GOTO 25
            J = J + 1
            ZR(JINST-1+J) = ZR(JVAL-1+K)
            IF (NBV .GT. 0) THEN
              CALL RSORAC ( RESU,'INST',IBID,ZR(JVAL-1+K),K8B,CBID,
     &                      PREC,CRITER,NUME,1,NBR)
            ELSE
              NBR = 0
            ENDIF
            IF (NBR.LT.0) THEN
               CALL UTMESS('F','CRTYPE','PLUSIEURS INSTANTS '//
     &                     'CORRESPONDENT A CELUI SPECIFIE SOUS AFFE ')
            ELSE IF (NBR.EQ.0) THEN
              ZI(JCPT + J - 1) = IVMX+1
              IVMX = IVMX+1
            ELSE
              ZI(JCPT + J - 1) = NUME
            ENDIF
 25       CONTINUE
        ENDIF
C
        DO 3 J = 1 , NBINST
          IF (J.GE.2) CALL JEMARQ()
          CALL JERECU('V')
          ICOMPT = ZI(JCPT + J - 1)
          TPS    = ZR(JINST+ J - 1)
          CALL RSEXCH ( RESU, NSYMB, ICOMPT, NOMCH, IRET )
          IF ( IRET .EQ. 0 ) THEN
             CALL UTDEBM('A',OPER,'*** CHAMP DEJA EXISTANT ***')
             CALL UTIMPK('L','IL SERA REMPLACE PAR LE CHAMP',1,
     +                                                     CHAMP(1:8) )
             CALL UTIMPR('S',' POUR L''INSTANT ',1,TPS)
             CALL UTFINM()
          ELSEIF ( IRET .EQ. 110 ) THEN
            CALL RSAGSD ( RESU, 0 )
            CALL RSEXCH ( RESU, NSYMB, ICOMPT, NOMCH, IRET )
          ENDIF

          IF ( K24(1:7) .EQ.'CHAM_NO' ) THEN
C
            O1=CHAMP//'.DESC'
            O2=NOMCH//'.DESC'
            CALL JEDUPO ( O1, 'G', O2, .FALSE. )
C
            O1=CHAMP//'.REFE'
            O2=NOMCH//'.REFE'
            CALL JEDUPO ( O1, 'G', O2, .FALSE. )
C
            O1=CHAMP//'.VALE'
            O2=NOMCH//'.VALE'
            CALL JEDUPO ( O1, 'G', O2, .FALSE. )
C
            CALL JEVEUO ( NOMCH//'.REFE', 'E', IAREFE )
            ZK24(IAREFE+1) = PROFCH

          ELSE

            CALL COPISD ( 'CHAMP_GD', 'G', CHAMP, NOMCH )

          ENDIF

          IF ( LFONC ) THEN
            CALL JELIRA ( CHAMP//'.VALE', 'LONMAX', LG, K8B )
            CALL JEVEUO ( CHAMP//'.VALE', 'L', JNOMF )
            CALL JEVEUO ( CHAMP//'.DEEQ', 'L', JDEEQ )

            CALL JEVEUO ( NOMCH//'.DESC', 'E', IADESC )
            CALL JENONU ( JEXNOM('&CATA.GD.NOMGD',TYPEGD), IGD )
            ZI(IADESC-1+1) = IGD
            CALL JEDETR ( NOMCH//'.VALE' )
            CALL WKVECT ( NOMCH//'.VALE', 'G V R', LG, JC )

C           -- CHAM_NO DE FONCTIONS A EVALUER
C           ----------------------------------
            CALL JEVEUO ( NOMCH//'.VALE', 'E', JC )
            DO 300 L = 1 , LG
              NOMF = ZK8(JNOMF+L-1)
              IF ( NOMF .EQ. ' ' ) GOTO 300
              CALL JEVEUO ( NOMF//'           .PROL', 'L', LPROL )
              CALL FONBPA ( NOMF, ZK16(LPROL), K16B, MXPARA, NBPF, NOMP)
              INO = ZI(JDEEQ+2*(L-1))
              IF ( INO .EQ. 0 ) GOTO 300
              DO 310 IP = 1,NBPF
                IF (NOMP(IP).EQ.'INST') THEN
                  VALPU(IP) = TPS
                ELSEIF (NOMP(IP).EQ.'X') THEN
                  VALPU(IP) = ZR(JCOOR-1+3*(INO-1)+1)
                ELSEIF (NOMP(IP).EQ.'Y') THEN
                  VALPU(IP) = ZR(JCOOR-1+3*(INO-1)+2)
                ELSEIF (NOMP(IP).EQ.'Z') THEN
                  VALPU(IP) = ZR(JCOOR-1+3*(INO-1)+3)
                ELSE
                  CALL UTMESS('F','CRTYPE','CMP NON TRAITEE')
                ENDIF
 310          CONTINUE
              CALL FOINTE('F',NOMF,NBPF,NOMP,VALPU,ZR(JC+L-1),IER)
300         CONTINUE
          ENDIF

          CALL RSNOCH ( RESU, NSYMB, ICOMPT, ' ' )
          CALL RSADPA ( RESU,'E',1,'INST',ICOMPT,0,IAD,K8B)
          ZR(IAD) = TPS
          CALL RSSEPA (RESU,ICOMPT,MODELE,MATERI,CARELE,EXCIT)
          IF (J.GE.2) CALL JEDEMA()

3       CONTINUE

        CALL JEDETR ( LINST )
        CALL JEDETR ( LCPT )
C
 100  CONTINUE
C     ------------------------------------------------------------------
      CALL JEDEMA()
      END
