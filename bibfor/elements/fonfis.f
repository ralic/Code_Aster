      SUBROUTINE FONFIS ( RESU, NOMAIL, MOTFAC, IOC, BASZ )
      IMPLICIT   NONE
      INTEGER             IOC
      CHARACTER*8         RESU, NOMAIL
      CHARACTER*(*)       MOTFAC, BASZ
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 19/03/2002   AUTEUR DURAND C.DURAND 
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
C-----------------------------------------------------------------------
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
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
      CHARACTER*32       JEXNUM, JEXNOM
C
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
      INTEGER       IER, NBTM, N1, N2, JMAIL, IM, EXISTE, KMAIL,
     +              JTYPM, NBM, IATYMA, LIBR1, IG, JNOE, NIG,
     +              NID, NUMNO, IRET, TROUV, IBID, NUMMA
      CHARACTER*1   BASE
      CHARACTER*8   K8B, NOMMA, TYPM, TYPMP, NOMGR, NOEUD
      CHARACTER*8   NOEUD1, NOEUD2, NOEUD3, NOEUD4
      CHARACTER*24  CONEC, TYPP, NOMMAI, NOMNOE, GRPMAI
      INTEGER       NJONC,N,I,K,ARDM,JCOUR1,JCOUR2,NBNO
      LOGICAL       GETEXM
C DEB-------------------------------------------------------------------
      CALL JEMARQ()
C
      BASE = BASZ
      N1   = 0
      IF (GETEXM(MOTFAC,'MAILLE')) THEN
         CALL GETVID ( MOTFAC, 'MAILLE'  , IOC,1,0, K8B, N1 )
      ENDIF
      CALL GETVID ( MOTFAC, 'GROUP_MA', IOC,1,0, K8B, N2 )
C
      IF ( N1+N2 .EQ. 0 ) THEN
        CALL UTMESS('F','FONFIS','SI NOEUD_ORIG : DONNER UN GROUP_MA '//
     &              'OU UNE LISTE DE MAILLES. ON NE REORDONNE PAS '//
     &              'LES GROUP_NO ET LES LISTES DE NOEUDS. ')
      ENDIF
C
      CONEC  = NOMAIL//'.CONNEX         '
      TYPP   = NOMAIL//'.TYPMAIL        '
      NOMMAI = NOMAIL//'.NOMMAI         '
      NOMNOE = NOMAIL//'.NOMNOE         '
      GRPMAI = NOMAIL//'.GROUPEMA       '
      CALL JEVEUO ( TYPP, 'L', IATYMA )
C
C     ------------------------------------------------------------------
C     --- VERIFICATION DE L'EXISTENCE DES MAILLES ET GROUPES DE MAILLES
C     --- VERIFICATION QUE LES MAILLES SONT TOUTES SEG2 OU TOUTES SEG3
C     ------------------------------------------------------------------
C
      N1 = -N1
      N2 = -N2
      NBTM = MAX ( N1 , N2 )
      CALL WKVECT ( '&&FONFIS.MAILLE', 'V V K8', NBTM, JMAIL )
      IER = 0
      NBTM = 0
      IF ( N1 .NE. 0 ) THEN
         CALL GETVID (MOTFAC,'MAILLE',IOC,1,N1,ZK8(JMAIL),IBID)
         DO 720, IM = 1, N1, 1
            NOMMA = ZK8(JMAIL+IM-1)
            CALL JEEXIN(JEXNOM(NOMMAI,NOMMA),EXISTE)
            IF ( EXISTE .EQ. 0 ) THEN
               IER = IER + 1
               CALL UTDEBM('E','FONFIS',MOTFAC)
               CALL UTIMPI('S',' OCCURRENCE ', 1, IOC )
               CALL UTIMPK('L',' MAILLE INEXISTANTE ',1,NOMMA)
               CALL UTFINM
            ELSE
               CALL JENONU(JEXNOM(NOMMAI,NOMMA),IBID)
               JTYPM = IATYMA-1+IBID
               CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(JTYPM)),TYPM)
               IF ( TYPM(1:3) .NE. 'SEG' ) THEN
                  IER = IER + 1
                  CALL UTDEBM('E','FONFIS',MOTFAC)
                  CALL UTIMPI('S',' OCCURRENCE ', 1, IOC )
                  CALL UTIMPK('L',' MAILLE NON LINEIQUE ',1,NOMMA)
                  CALL UTFINM
               ENDIF
               IF ((IM.GT.1).AND.(TYPM(1:4).NE.TYPMP(1:4))) THEN
                  IER = IER + 1
                  CALL UTDEBM('E','FONFIS',MOTFAC)
                  CALL UTIMPI('S',' OCCURRENCE ', 1, IOC )
                  CALL UTIMPK('L',' MELANGE SEG2 ET SEG3 ',1,NOMMA)
                  CALL UTFINM
               ENDIF
               TYPMP(1:4) = TYPM(1:4)
            ENDIF
720      CONTINUE
         NBTM = NBTM + N1
      ENDIF
      IF ( N2 .NE. 0 ) THEN
        CALL GETVID(MOTFAC,'GROUP_MA',IOC,1,N2,ZK8(JMAIL),IBID)
        DO 710, IG = 1, N2, 1
          NOMGR = ZK8(JMAIL+IG-1)
          CALL JENONU (JEXNOM(GRPMAI,NOMGR),EXISTE)
          IF ( EXISTE .EQ. 0 ) THEN
            IER = IER + 1
            CALL UTDEBM('E','FONFIS',MOTFAC)
            CALL UTIMPI('S',' OCCURRENCE ', 1, IOC )
            CALL UTIMPK('L','GROUPE DE MAILLES INEXISTANT',1,NOMGR)
            CALL UTFINM
          ELSE
            CALL JELIRA(JEXNOM(GRPMAI,NOMGR),'LONMAX',NBM,K8B)
            CALL JEVEUO(JEXNOM(GRPMAI,NOMGR),'L',KMAIL)
            DO 750, IM = 1, NBM, 1
              JTYPM=IATYMA-1+ZI(KMAIL+IM-1)
              CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(JTYPM)),TYPM)
              IF ( TYPM(1:3) .NE. 'SEG' ) THEN
                CALL JENUNO(JEXNUM(NOMMAI,ZI(KMAIL+IM-1)),NOMMA)
                IER = IER + 1
                CALL UTDEBM('E','FONFIS',MOTFAC)
                CALL UTIMPI('S',' OCCURRENCE ', 1, IOC )
                CALL UTIMPK('L',' MAILLE NON LINEIQUE ',1,NOMMA)
                CALL UTFINM
              ENDIF
              IF ((IM+IG.GT.2).AND.(TYPM(1:4).NE.TYPMP(1:4))) THEN
                IER = IER + 1
                CALL UTDEBM('E','FONFIS',MOTFAC)
                CALL UTIMPI('S',' OCCURRENCE ', 1, IOC )
                CALL UTIMPK('L',' MELANGE SEG2 ET SEG3 ',1,NOMMA)
                CALL UTFINM
              ENDIF
            TYPMP(1:4) = TYPM(1:4)
750         CONTINUE
            NBTM = NBTM + NBM
          ENDIF
710     CONTINUE
      ENDIF
C
      IF ( IER .GT. 0 ) THEN
         CALL UTDEBM('F','FONFIS',MOTFAC)
         CALL UTIMPI('S',' OCCURRENCE ', 1, IOC )
         CALL UTIMPK('L','LES OBJETS PRECEDEMMENT EVOQUES SONT '//
     +                   'INEXISTANTS OU DE TYPE INCOMPATIBLE',
     +                    1,'ARRET EN ERREUR')
         CALL UTFINM
      ENDIF
C
C     ------------------------------------------------------------------
C     --- RECUPERATION ET RANGEMENT DES MAILLES ET DES NOEUDS SOMMETS
C     --- NBTM PREMIERES VALEURS : NUMEROS DE MAILLES
C     --- NBTM SUIVANTES         : NUMEROS DE NOEUDS DE GAUCHE
C     --- NBTM SUIVANTES         : NUMEROS DE NOEUDS DE DROITE
C     ------------------------------------------------------------------
C
      CALL WKVECT('&&FONFIS.VERI_MAIL1','V V I',3*NBTM,JCOUR1)
      CALL WKVECT('&&FONFIS.VERI_MAIL2','V V I',3*NBTM,JCOUR2)
      IF ( N1 .NE. 0 ) THEN
         DO 510, IM =1, N1, 1
            CALL JENONU(JEXNOM(NOMMAI,ZK8(JMAIL+IM-1)),IBID)
            ZI(JCOUR1+IM-1)= IBID
510      CONTINUE
      ELSE
         LIBR1 = 1
         DO 520, IG = 1, N2, 1
            NOMGR = ZK8(JMAIL+IG-1)
            CALL JELIRA(JEXNOM(GRPMAI,NOMGR),'LONMAX',NBM,K8B)
            CALL JEVEUO(JEXNOM(GRPMAI,NOMGR),'L',KMAIL)
            DO 521, IM = 1, NBM, 1
               ZI(JCOUR1+LIBR1-1)=ZI(KMAIL+IM-1)
               LIBR1=LIBR1+1
521         CONTINUE
520      CONTINUE
      ENDIF
C
      DO 530 IM = 1 , NBTM
         CALL I2EXTF (ZI(JCOUR1+IM-1),1,CONEC(1:15),TYPP(1:16),NIG,NID)
         ZI(JCOUR1+  NBTM+IM-1) = NIG
         ZI(JCOUR1+2*NBTM+IM-1) = NID
 530  CONTINUE
C
C     ------------------------------------------------------------------
C     --- IDENTIFICATION DU NOEUD D'ABSCISSE CURVILIGNE 0.
C     ------------------------------------------------------------------
C
      NUMNO = 0
      CALL GETVID ( MOTFAC, 'NOEUD_ORIG', IOC,1,0, NOEUD, N1 )
      IF ( N1 .NE. 0 ) THEN
         CALL GETVID ( MOTFAC, 'NOEUD_ORIG', IOC,1,1, NOEUD, N1 )
         CALL JENONU ( JEXNOM(NOMNOE,NOEUD), NUMNO )
      ELSE
         CALL GETVID ( MOTFAC, 'GROUP_NO_ORIG', IOC,1,0, K8B, N1 )
         IF ( N1 .NE. 0 ) THEN
            CALL GETVID ( MOTFAC, 'GROUP_NO_ORIG', IOC,1,1, K8B, N1)
            CALL UTNONO ( ' ', NOMAIL, 'NOEUD', K8B, NOEUD, IRET )
            IF ( IRET .EQ. 10 ) THEN
               CALL UTMESS('F','FONFIS',
     +                      'LE GROUP_NO : '//K8B//'N''EXISTE PAS.')
            ELSEIF ( IRET .EQ. 1 ) THEN
               CALL UTDEBM('A','FONFIS',
     +                           'TROP DE NOEUDS DANS LE GROUP_NO')
               CALL UTIMPK('L','  NOEUD UTILISE: ',1,NOEUD)
               CALL UTFINM( )
            ENDIF
            CALL JENONU ( JEXNOM(NOMNOE,NOEUD), NUMNO )
         ENDIF
      ENDIF
C
      IF ( NUMNO .EQ. 0 ) THEN
            CALL UTMESS('F','FONFIS',
     +                  'PAS D''ORIGINE DEFINIE PAR NOEUD_ORIG OU '//
     +                  'GROUP_NO_ORIG, OU NOEUD INEXISTANT')
      ELSE
         TROUV = 0
         DO 540 IM = 1 , NBTM
            IF ( ZI(JCOUR1+  NBTM+IM-1) .EQ. NUMNO ) THEN
               TROUV = TROUV + 1
            ENDIF
            IF ( ZI(JCOUR1+2*NBTM+IM-1) .EQ. NUMNO ) THEN
               TROUV = TROUV + 1
            ENDIF
 540     CONTINUE
         IF ( TROUV .EQ. 0 ) THEN
            CALL UTMESS('F','FONFIS','LE NOEUD ORIGINE '//NOEUD//
     +                               'NE FAIT PAS PARTI DU CHEMIN')
         ENDIF
         IF (( TROUV .NE. 1 ).AND.(MOTFAC(6:10).NE.'FERME')) THEN
            CALL UTMESS('F','FONFIS','LE NOEUD ORIGINE '//NOEUD//
     +                  ' N''EST PAS UNE EXTREMITE')
         ENDIF
      ENDIF
C
C
C     ------------------------------------------------------------------
C     --- SI FOND FERME : RECUPERATION DE MAILLE_ORIG POUR AVOIR
C     --- LE SENS DE PARCOURS DE LA COURBE FERMEE
C     ------------------------------------------------------------------
C
      IF (MOTFAC(6:10).EQ.'FERME') THEN
C
      NUMMA = 0
      CALL GETVID ( MOTFAC, 'MAILLE_ORIG', IOC,1,0, NOMMA, N1 )
      IF ( N1 .NE. 0 ) THEN
         CALL GETVID ( MOTFAC, 'MAILLE_ORIG', IOC,1,1, NOMMA, N1 )
         CALL JENONU ( JEXNOM(NOMMAI,NOMMA), NUMMA )
      ELSE
         CALL GETVID ( MOTFAC, 'GROUP_MA_ORIG', IOC,1,0, K8B, N1 )
         IF ( N1 .NE. 0 ) THEN
            CALL GETVID ( MOTFAC, 'GROUP_MA_ORIG', IOC,1,1, K8B, N1)
            CALL UTNONO ( ' ', NOMAIL, 'MAILLE', K8B, NOMMA, IRET )
            IF ( IRET .EQ. 10 ) THEN
               CALL UTMESS('F','FONFIS',
     +                      'LE GROUP_MA : '//K8B//'N''EXISTE PAS.')
            ELSEIF ( IRET .EQ. 1 ) THEN
               CALL UTDEBM('A','FONFIS',
     +                           'TROP DE MAILLES DANS LE GROUP_MA')
               CALL UTIMPK('L','  MAILLE UTILISEE: ',1,NOEUD)
               CALL UTFINM( )
            ENDIF
         CALL JENONU ( JEXNOM(NOMMAI,NOMMA), NUMMA )
         ENDIF
      ENDIF
C
      IF ( NUMMA .EQ. 0 ) THEN
        CALL UTMESS('F','FONFIS','SI LE FOND EST UNE COURBE FERMEE '//
     +               'MAILLE_ORIG OU GROUP_MA_ORIG DOIT ACCOMPAGNER '//
     +               'NOEUD_ORIG')
      ELSE
        CALL I2EXTF (NUMMA,1,CONEC(1:15),TYPP(1:16),NIG,NID)
        IF ((NUMNO.NE.NIG).AND.(NUMNO.NE.NID)) THEN
          CALL UTMESS('F','FONFIS','LE NOEUD_ORIG N''APPARTIENT'//
     +                    ' PAS A LA MAILLE_ORIG')
        ENDIF
        TROUV = 0
        DO 545 IM = 1 , NBTM
          IF(NUMMA.EQ.ZI(JCOUR1+IM-1)) TROUV = IM
 545    CONTINUE
        IF (TROUV.EQ.0) THEN
          CALL UTMESS('F','FONFIS','LA MAILLE_ORIG'//NOMMA//
     +                ' N''APPARTIENT PAS AU FOND DE FISSURE')
        ELSE
C
C     ON REMONTE LA MAILLE_ORIG EN TETE DE LISTE
C
          DO 546 IM = TROUV , NBTM
            ZI(JCOUR2+       IM+1-TROUV-1)=ZI(JCOUR1+       IM-1)
            ZI(JCOUR2+  NBTM+IM+1-TROUV-1)=ZI(JCOUR1+  NBTM+IM-1)
            ZI(JCOUR2+2*NBTM+IM+1-TROUV-1)=ZI(JCOUR1+2*NBTM+IM-1)
 546      CONTINUE
          DO 547 IM = 1 , TROUV-1
            ZI(JCOUR2+       IM+1+NBTM-TROUV-1)=ZI(JCOUR1+       IM-1)
            ZI(JCOUR2+  NBTM+IM+1+NBTM-TROUV-1)=ZI(JCOUR1+  NBTM+IM-1)
            ZI(JCOUR2+2*NBTM+IM+1+NBTM-TROUV-1)=ZI(JCOUR1+2*NBTM+IM-1)
 547      CONTINUE
          DO 548 IM = 1 , NBTM
            ZI(JCOUR1+       IM-1)=ZI(JCOUR2+       IM-1)
            ZI(JCOUR1+  NBTM+IM-1)=ZI(JCOUR2+  NBTM+IM-1)
            ZI(JCOUR1+2*NBTM+IM-1)=ZI(JCOUR2+2*NBTM+IM-1)
 548      CONTINUE
        ENDIF
      ENDIF
      ENDIF
C
C     ------------------------------------------------------------------
C     --- ON ORDONNE LES MAILLES PAR LEUR CONNECTIVITE ET LES NOEUDS 
C     --- SOMMETS A L AIDE DE RECOPIES DANS LE VECTEUR DE TRAVAIL
C     --- ZI(JCOUR2...)
C     ------------------------------------------------------------------
C
      NJONC = NUMNO
      N = 1
550   CONTINUE
      DO 552 I=N,NBTM
        IF (ZI(JCOUR1+NBTM+I-1).EQ.NJONC) THEN
          ZI(JCOUR2+       N-1)=ZI(JCOUR1+       I-1)
          ZI(JCOUR2+  NBTM+N-1)=ZI(JCOUR1+  NBTM+I-1)
          ZI(JCOUR2+2*NBTM+N-1)=ZI(JCOUR1+2*NBTM+I-1)
          NJONC                =ZI(JCOUR1+2*NBTM+I-1)
          GOTO 555
        ENDIF
        IF (ZI(JCOUR1+2*NBTM+I-1).EQ.NJONC) THEN
          ZI(JCOUR2+       N-1)=ZI(JCOUR1+       I-1)
          ZI(JCOUR2+  NBTM+N-1)=ZI(JCOUR1+2*NBTM+I-1)
          ZI(JCOUR2+2*NBTM+N-1)=ZI(JCOUR1+  NBTM+I-1)
          NJONC                =ZI(JCOUR1+  NBTM+I-1)
          GOTO 555
        ENDIF
552   CONTINUE
C
555   CONTINUE
      DO 557 K=N,I-1
        ZI(JCOUR2+       1+K-1)=ZI(JCOUR1+       K-1)
        ZI(JCOUR2+  NBTM+1+K-1)=ZI(JCOUR1+  NBTM+K-1)
        ZI(JCOUR2+2*NBTM+1+K-1)=ZI(JCOUR1+2*NBTM+K-1)
557   CONTINUE
      DO 558 K=I+1,NBTM
        ZI(JCOUR2+       K-1)=ZI(JCOUR1+       K-1)
        ZI(JCOUR2+  NBTM+K-1)=ZI(JCOUR1+  NBTM+K-1)
        ZI(JCOUR2+2*NBTM+K-1)=ZI(JCOUR1+2*NBTM+K-1)
558   CONTINUE
      DO 559 K=N,NBTM
        ZI(JCOUR1+       K-1)=ZI(JCOUR2+       K-1)
        ZI(JCOUR1+  NBTM+K-1)=ZI(JCOUR2+  NBTM+K-1)
        ZI(JCOUR1+2*NBTM+K-1)=ZI(JCOUR2+2*NBTM+K-1)
559   CONTINUE
      N=N+1
      IF (N.GT.NBTM) GOTO 560
      GOTO 550
C
560   CONTINUE
C
C     ------------------------------------------------------------------
C     --- ON STOCKE LES NOEUDS ORDONNES DANS LA STRUCTURE DE DONNEES
C     --- AVEC RAJOUT DES NOEUDS MILIEUX SI SEG3
C     ------------------------------------------------------------------
C
      IF ( TYPM(1:4) .EQ. 'SEG2' ) THEN
        NBNO=NBTM+1
        CALL WKVECT(RESU//'.FOND      .NOEU',BASE//' V K8',NBNO,JNOE)
        DO 570 I=1,NBTM
          CALL JENUNO(JEXNUM(NOMNOE,ZI(JCOUR1+NBTM+I-1)),NOEUD)
          ZK8(JNOE+I-1) = NOEUD
570     CONTINUE
        CALL JENUNO(JEXNUM(NOMNOE,ZI(JCOUR1+3*NBTM-1)),NOEUD)
        ZK8(JNOE+NBTM+1-1) = NOEUD
C
      ELSEIF ( TYPM(1:4) .EQ. 'SEG3' ) THEN
        NBNO=2*NBTM+1
        CALL WKVECT(RESU//'.FOND      .NOEU',BASE//' V K8',NBNO,JNOE)
        DO 575 I=1,NBTM
          CALL JENUNO(JEXNUM(NOMNOE,ZI(JCOUR1+NBTM+I-1)),NOEUD)
          ZK8(JNOE+2*I-1-1)   = NOEUD
          CALL JEVEUO(JEXNUM(CONEC,ZI(JCOUR1+I-1)),'L',ARDM)
          CALL JENUNO(JEXNUM(NOMNOE,ZI(ARDM+3-1)),NOEUD)
          ZK8(JNOE+2*I  -1) = NOEUD
575     CONTINUE
        CALL JENUNO(JEXNUM(NOMNOE,ZI(JCOUR1+3*NBTM-1)),NOEUD)
        ZK8(JNOE+2*NBTM+1-1) = NOEUD

CJMP

      ELSEIF ( TYPM(1:4) .EQ. 'SEG4' ) THEN
        NBNO=3*NBTM+1
        CALL WKVECT(RESU//'.FOND      .NOEU',BASE//' V K8',NBNO,JNOE)
        DO 580 I=1,NBTM
          CALL JENUNO(JEXNUM(NOMNOE,ZI(JCOUR1+NBTM+I-1)),NOEUD)
          ZK8(JNOE+3*I-3)   = NOEUD
          CALL JEVEUO(JEXNUM(CONEC,ZI(JCOUR1+I-1)),'L',ARDM)
          CALL JENUNO(JEXNUM(NOMNOE,ZI(ARDM+1-1)),NOEUD1)
          CALL JENUNO(JEXNUM(NOMNOE,ZI(ARDM+2-1)),NOEUD2)
          CALL JENUNO(JEXNUM(NOMNOE,ZI(ARDM+3-1)),NOEUD3)
          CALL JENUNO(JEXNUM(NOMNOE,ZI(ARDM+4-1)),NOEUD4)
          IF (NOEUD1.EQ.NOEUD) THEN
             ZK8(JNOE+3*I-2) = NOEUD3
             ZK8(JNOE+3*I-1) = NOEUD4
          ELSEIF (NOEUD2.EQ.NOEUD) THEN
             ZK8(JNOE+3*I-2) = NOEUD4
             ZK8(JNOE+3*I-1) = NOEUD3
          ELSE
              CALL UTMESS('F','FONFIS','ERREUR DE PROGRAMMATION')
          ENDIF
580     CONTINUE
        CALL JENUNO(JEXNUM(NOMNOE,ZI(JCOUR1+3*NBTM-1)),NOEUD)
        ZK8(JNOE+3*NBTM+1-1) = NOEUD
      ENDIF
C
C     ------------------------------------------------------------------
C     --- VERIFICATION DU NOEUD EXTREMITE DANS LE CAS D UNE COURBE
C     --- NON FERMEE
C     ------------------------------------------------------------------
C
      IF (MOTFAC(6:10).NE.'FERME') THEN
        CALL GETVID ( MOTFAC, 'NOEUD_EXTR', IOC,1,0, NOEUD, N1 )
        IF ( N1 .NE. 0 ) THEN
           CALL GETVID ( MOTFAC, 'NOEUD_EXTR', IOC,1,1, NOEUD, N1 )
        ELSE
           CALL GETVID ( MOTFAC, 'GROUP_NO_EXTR', IOC,1,0, K8B, N1 )
           IF ( N1 .NE. 0 ) THEN
              CALL GETVID ( MOTFAC, 'GROUP_NO_EXTR', IOC,1,1, K8B, N1)
              CALL UTNONO ( ' ', NOMAIL, 'NOEUD', K8B, NOEUD, IRET )
              IF ( IRET .EQ. 10 ) THEN
                 CALL UTMESS('F','FONFIS',
     +                        'LE GROUP_NO : '//K8B//'N''EXISTE PAS.')
              ELSEIF ( IRET .EQ. 1 ) THEN
                 CALL UTDEBM('A','FONFIS',
     +                           'TROP DE NOEUDS DANS LE GROUP_NO')
                 CALL UTIMPK('L','  NOEUD UTILISE: ',1,NOEUD)
                 CALL UTFINM( )
              ENDIF
           ENDIF
        ENDIF
C
        IF ( ZK8(JNOE+NBNO-1) .NE. NOEUD ) THEN
          CALL UTMESS('F','FONFIS','LE NOEUD EXTREMITE '//NOEUD//
     +              ' N''EST PAS LE DERNIER NOEUD DU FOND DE FISSURE')
        ENDIF
      ENDIF
C     ------------------------------------------------------------------
      CALL JEDETR ( '&&FONFIS.VERI_MAIL1' )
      CALL JEDETR ( '&&FONFIS.VERI_MAIL2' )
      CALL JEDETR ( '&&FONFIS.MAILLE'     )
C
9999  CONTINUE
      CALL JEDEMA()
      END
