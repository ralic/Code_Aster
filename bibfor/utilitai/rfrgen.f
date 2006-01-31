      SUBROUTINE RFRGEN ( TRANGE )
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)       TRANGE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 30/01/2006   AUTEUR LEBOUVIE F.LEBOUVIER 
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
C     OPERATEUR "RECU_FONCTION"  MOT CLE "RESU_GENE"
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32     JEXNUM, JEXNOM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER      IBID, LREFE1, LREFE2
      INTEGER      IFM,NIV
      CHARACTER*1  TYPE,COLI,K1BID
      CHARACTER*4  INTERP(2), INTRES
      CHARACTER*8  K8B, CRIT, NOEUD, CMP, NOMA, NOMACC,  BASEMO
      CHARACTER*8  MONMOT(2), NOGNO, NONMOT
      CHARACTER*14 NUME
      CHARACTER*16 NOMCMD, TYPCON, NOMCHA, NOMSY, TYSD
      CHARACTER*19 NOMFON, KNUME, KINST, RESU, MATRAS, FONCT
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      CALL INFMAJ
      CALL INFNIV(IFM,NIV)

      CALL GETRES ( NOMFON , TYPCON , NOMCMD )

      CALL GETTCO ( TRANGE, TYSD )
      IF ( TYSD .EQ. 'MODE_GENE' ) THEN
         CALL GETVTX ( ' ', 'NOM_PARA_RESU', 1,1,1, K8B,  N1 )
         CALL GETVIS ( ' ', 'NUME_CMP_GENE', 1,1,1, IBID, N2 )
         IF ( (N1+N2) .NE. 0 ) THEN
            CALL RFMGE1 ( TRANGE )
         ELSE
            CALL RFMGE2 ( TRANGE )
         ENDIF
         GOTO 9999
      ELSEIF ( TYSD .EQ. 'HARM_GENE' ) THEN
         CALL GETVTX ( ' ', 'NOM_PARA_RESU', 1,1,1, K8B,  N1 )
         CALL GETVIS ( ' ', 'NUME_CMP_GENE', 1,1,1, IBID, N2 )
         IF ( (N1+N2) .NE. 0 ) THEN
            CALL RFHGE1 ( TRANGE )
         ELSE
            CALL RFHGE2 ( TRANGE )
         ENDIF
         GOTO 9999
      ENDIF
C
      RESU = TRANGE
      INTERP(1) = 'NON '
      INTERP(2) = 'NON '
      INTRES    = 'NON '
C
      CALL GETVTX(' ','CRITERE'     ,0,1,1,CRIT  ,N1)
      CALL GETVR8(' ','PRECISION'   ,0,1,1,EPSI  ,N1)
      CALL GETVTX(' ','INTERP_NUME' ,0,1,1,INTRES,N1)
      CALL GETVTX(' ','INTERPOL'    ,0,1,2,INTERP,N1)
      IF ( N1 .EQ. 1 ) INTERP(2) = INTERP(1)
C
      NOEUD = ' '
      CMP   = ' '
      CALL GETVID(' ','NOEUD',0,1,1,NOEUD,N1)
      CALL GETVTX(' ','NOM_CMP'      ,0,1,1,CMP   ,N2)
      CALL GETVTX(' ','NOM_CHAM' ,0,1,1,NOMCHA,N3)
C
      CALL JEEXIN(RESU//'.'//NOMCHA(1:4),IRET)
      IF (IRET.EQ.0)  THEN
         CALL UTMESS('F','RFRGEN','LE CHAMP '//NOMCHA//' N''EXISTE'//
     +                                       ' PAS DANS LE RESU_GENE.')
      ENDIF
      CALL JEVEUO(RESU//'.'//NOMCHA(1:4),'L',ITRESU)
C
      NOMACC = 'INST'
      KNUME = '&&RFRGEN.NUME_ORDR'
      KINST = '&&RFRGEN.INSTANT'
      CALL RSTRAN (INTRES,RESU,' ',1,KINST,KNUME,NBORDR,IE)
      IF (IE.NE.0) THEN
         CALL UTMESS('F','RFRGEN','PROBLEME(S) RENCONTRE(S) LORS'//
     +                                ' DE LA LECTURE DES INSTANTS.' )
      ENDIF
      CALL JEEXIN(KINST,IRET)
      IF (IRET.GT.0) THEN
        CALL JEVEUO(KINST,'L',JINST)
        CALL JEVEUO(KNUME,'L',LORDR)
      END IF
C
C     --- REMPLISSAGE DU .PROL ---
C
      CALL WKVECT(NOMFON//'.PROL','G V K16',5,LPRO)
      ZK16(LPRO)   = 'FONCTION'
      ZK16(LPRO+1) = INTERP(1)//INTERP(2)
      ZK16(LPRO+2) = NOMACC(1:8)
      ZK16(LPRO+3) = NOMCHA(1:4)
      ZK16(LPRO+4) = 'EE      '
C
C----------------------------------------------------------------------
C                            P T E M
C----------------------------------------------------------------------
C
      IF ( NOMCHA(1:4) .EQ. 'PTEM') THEN
         CALL JEVEUO(RESU//'.PTEM','L',IPAS)
         CALL JELIRA(RESU//'.PTEM','LONMAX',NBPAS,K8B)
         IF (NBPAS.LE.1) THEN
            CALL UTMESS('F','RFRGEN','PROBLEME RECUP DE PTEM'//
     +                           ' UNIQUEMENT POUR METHODE ADAPT' )
         ENDIF
         CALL WKVECT('&&RFRGEN.DT','V V R',NBPAS,LPAS)
         DO 58 IP = 1,NBPAS
            ZR(LPAS+IP-1) = LOG10(ZR(IPAS+IP-1))
 58      CONTINUE
C
         CALL WKVECT(NOMFON//'.VALE','G V R',2*NBORDR,LVAR)
         LFON = LVAR + NBORDR
         IF ( INTRES(1:3) .NE. 'NON' ) THEN
            CALL JEVEUO(RESU//'.INST','L',IDINSG)
            CALL JELIRA(RESU//'.INST','LONMAX',NBINSG,K8B)
            DO 54 IORDR = 0, NBORDR-1
               CALL EXTRAC ( INTRES,EPSI,CRIT,NBINSG-2,ZR(IDINSG),
     +                       ZR(JINST+IORDR),ZR(LPAS),1,REP,IERD)
               ZR(LVAR+IORDR) = ZR(JINST+IORDR)
               ZR(LFON+IORDR) = REP
 54         CONTINUE
         ELSE
            DO 56 IORDR = 0, NBORDR-1
               II = ZI(LORDR+IORDR)
               ZR(LVAR+IORDR) = ZR(JINST+IORDR)
               ZR(LFON+IORDR) = ZR(LPAS+IORDR)
 56          CONTINUE
         ENDIF
         CALL JEDETR('&&RFRGEN.DT')
C
C----------------------------------------------------------------------
C                 D E P L   ---   V I T E   ---   A C C E
C----------------------------------------------------------------------
C
      ELSE
         CALL JEVEUO(RESU//'.REFD','L',LREFE1)
         BASEMO = ZK24(LREFE1+5)(1:8)
         CALL JEVEUO(BASEMO//'           .REFD','L',LREFE2)
         MATRAS = ZK24(LREFE2)
         CALL JEVEUO(RESU//'.DESC','L',LDESC)
         NBMODE = ZI(LDESC+1)
         NOMSY = 'DEPL'
         IF (MATRAS.NE.' ') THEN
           CALL VPRECU ( BASEMO, NOMSY,-1,IBID, '&&RFRGEN.VECT.PROPRE',
     +                   0, K8B, K8B,  K8B, K8B,
     +                   NEQ, MXMODE, TYPE, NBPARI, NBPARR, NBPARK )
           CALL JEVEUO('&&RFRGEN.VECT.PROPRE','L',IDBASE)
           IF ( TYPE .NE. 'R' ) THEN
              CALL UTMESS('F','RFRGEN',
     +               ' ON NE TRAITE PAS LE TYPE DE MODES "'//TYPE//'".')
           ENDIF
C
           CALL DISMOI('F','NOM_NUME_DDL',MATRAS,'MATR_ASSE',IBID,NUME,
     +                 IE)
           CALL DISMOI('F','NOM_MAILLA'  ,MATRAS,'MATR_ASSE',IBID,NOMA,
     +                 IE)
         ELSE
           NUME = ZK24(LREFE2+3)(1:14)
           CALL DISMOI('F','NOM_MAILLA',NUME,'NUME_DDL',IBID,NOMA,IE)
           CALL DISMOI('F','NB_EQUA'   ,NUME,'NUME_DDL',NEQ,K8B,IE)
           CALL WKVECT('&&RFRGEN.VECT.PROPRE','V V R',NEQ*NBMODE,IDBASE)
           CALL COPMO2(BASEMO,NEQ,NUME,NBMODE,ZR(IDBASE))
         ENDIF
         CALL GETVID(' ','GROUP_NO',0,1,1,NOGNO,NGN)
         IF (NGN.NE.0) THEN
           CALL JENONU(JEXNOM(NOMA//'.GROUPENO',NOGNO),IGN2)
           IF (IGN2.LE.0)  CALL UTMESS('F','RFRGEN','LE GROUP_NO : '//
     +                                        NOGNO//'N''EXISTE PAS.')
           CALL JEVEUO(JEXNUM(NOMA//'.GROUPENO',IGN2),'L',IAGNO)

           INO = ZI(IAGNO)
           CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',INO),NOEUD)
         ENDIF
         CALL POSDDL('NUME_DDL',NUME,NOEUD,CMP,INOEUD,IDDL)
         IF ( INOEUD .EQ. 0 ) THEN
            LG1 = LXLGUT(NOEUD)
            CALL UTMESS('F','RFRGEN',
     +                  'LE NOEUD "'//NOEUD(1:LG1)//'" N''EXISTE PAS.')
         ELSEIF ( IDDL .EQ. 0 ) THEN
            LG1 = LXLGUT(NOEUD)
            LG2 = LXLGUT(CMP)
          CALL UTMESS('F','RFRGEN','LA COMPOSANTE "'//CMP(1:LG2)//'" '//
     +                  'DU NOEUD "'//NOEUD(1:LG1)//'" N''EXISTE PAS.')
         ENDIF
C
C        --- RECHERCHE SI UNE ACCELERATION D'ENTRAINEMENT EXISTE ---
         NFONCT = 0
         CALL GETVID(' ','ACCE_MONO_APPUI',1,1,1,FONCT,NFONCT)
         IF (NFONCT.NE.0) THEN
            IF (NOMCHA(1:4).NE.'ACCE') THEN
C           --- ACCE_MONO_APPUI COMPATIBLE UNIQUEMENT AVEC ACCELERATION
            CALL UTMESS('F','RFRGEN','ACCE_MONO_APPUI EST COMPATIBLE '//
     +                    'UNIQUEMENT AVEC UN CHAMP DE TYPE : ACCE ')
               GOTO 9999
            ENDIF
            ZK16(LPRO+3)(5:8) = '_ABS'
         ENDIF
C        --------------------------------------------------------------
         CALL WKVECT(NOMFON//'.VALE','G V R',2*NBORDR,LVAR)
         LFON = LVAR + NBORDR
         IF ( INTRES(1:3) .NE. 'NON' ) THEN
            CALL JEVEUO(RESU//'.INST','L',IDINSG)
            CALL JELIRA(RESU//'.INST','LONMAX',NBINSG,K8B)
            CALL WKVECT('&&RFRGEN.VECTGENE','V V R',NBMODE,IDVECG)
            DO 50 IORDR = 0, NBORDR-1
               CALL EXTRAC ( INTRES,EPSI,CRIT,NBINSG,ZR(IDINSG),
     +               ZR(JINST+IORDR),ZR(ITRESU),NBMODE,ZR(IDVECG),IERD)
               CALL MDGEP2(NEQ,NBMODE,ZR(IDBASE),ZR(IDVECG),IDDL,REP)
               ZR(LVAR+IORDR) = ZR(JINST+IORDR)
               ZR(LFON+IORDR) = REP
 50         CONTINUE
            CALL JEDETR('&&RFRGEN.VECTGENE')
C
         ELSE
            DO 52 IORDR = 0, NBORDR-1
               II = ZI(LORDR+IORDR)
               CALL MDGEP2(NEQ,NBMODE,ZR(IDBASE),
     +                      ZR(ITRESU+NBMODE*(II-1)),IDDL,REP)
               ZR(LVAR+IORDR) = ZR(JINST+IORDR)
               ZR(LFON+IORDR) = REP
 52         CONTINUE
         ENDIF
         MONMOT(1) = 'NON'
         MONMOT(2) = 'NON'
         NONMOT   = 'NON'
         CALL GETVTX(' ','MULT_APPUI',1,1,1,MONMOT(1),N1)
         CALL GETVTX(' ','CORR_STAT',1,1,1,MONMOT(2),N2)
         IF ( MONMOT(1).EQ.'OUI' .OR. MONMOT(2).EQ.'OUI' ) NONMOT='OUI'
         IF ( NONMOT(1:3) .EQ. 'OUI' ) THEN
            CALL JEVEUO(RESU//'.F'//NOMCHA(1:3),'L',JFON)
            CALL JEVEUO(RESU//'.IPSD','L',IPSDEL)
            CALL JELIRA(RESU//'.F'//NOMCHA(1:3),'LONMAX',NBEXCI,K8B)
            NBEXCI = NBEXCI / 2
            DO 100 IORDR = 0, NBORDR-1
               CALL MDGEP4 (NEQ, NBEXCI, ZR(IPSDEL), ZR(LVAR+IORDR),
     +                                   ZK8(JFON), IDDL, REP)
               ZR(LFON+IORDR) = ZR(LFON+IORDR) + REP
 100        CONTINUE
         ENDIF
         CALL JEDETR( '&&RFRGEN.VECT.PROPRE' )
C
C        --- PRISE EN COMPTE D'UNE ACCELERATION D'ENTRAINEMENT ---
         IF (NFONCT.NE.0) THEN
            DO 110 I = 0, NBORDR-1
               IRET = 0
               CALL FOINTE('F',FONCT,1,'INST',ZR(JINST+I),ALPHA,IER)
C              --- ACCELERATION ABSOLUE = RELATIVE + ENTRAINEMENT ---
               ZR(LFON+I) = ZR(LFON+I) + ALPHA
 110        CONTINUE
         ENDIF
C     ---------------------------------------------------------------
      ENDIF
      CALL JEDETR( KNUME )
      CALL JEDETR( KINST )
 9999 CONTINUE
C
      CALL FOATTR(' ',1,NOMFON)
C
C     --- VERIFICATION QU'ON A BIEN CREER UNE FONCTION ---
C         ET REMISE DES ABSCISSES EN ORDRE CROISSANT
      CALL ORDONN(NOMFON,NOMCMD,0)
C 
      CALL TITRE
      IF (NIV.GT.1) CALL FOIMPR(NOMFON,NIV,IFM,0,K8B)

      CALL JEDEMA()
      END
