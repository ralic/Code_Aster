      SUBROUTINE RSINFO ( NOMCON , IFI )
C
C     RESULTAT - INFORMATION
C     * *        ****
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C TOLE CRS_602
C
C     ECRITURE DE LA STRUCTURE D'UN CONCEPT RESULTAT
C
C IN  NOMCON : NOM DU CONCEPT A IMPRIMER
C IN  IFI    : UNITE LOGIQUE D'IMPRESSION
C     ------------------------------------------------------------------
      IMPLICIT   NONE
C
C 0.1. ==> ARGUMENTS
C
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM,JEXNOM
      CHARACTER*(*)       NOMCON
C 0.2. ==> COMMUNS
C
C
C 0.3. ==> VARIABLES LOCALES
C
      CHARACTER*6 NOMPRO
      PARAMETER ( NOMPRO = 'RSINFO' )
C
      INTEGER       IFI, IBID, NBNOSY, LRES, INOMSY, I, J, K, ISY, II
      INTEGER       IATACH, LNOSY, LNOPA, LNUPA, LATAC, LG, LXLGUT, LB
      INTEGER       IRET, LTIRT, NBAC, NBPA, IAC, IPAR, IAD, JPA
      INTEGER       NBORDT, IPCD, IPCF, LPOIN, LONGT
      REAL*8        R8B, RUNDF, R8VIDE
      COMPLEX*16    C16B
      CHARACTER*8   K8B, NOMB1, NOMGD, CTYPE
      CHARACTER*16  NOMSYM, BLANC, NOPARA, NOPAR2
      CHARACTER*19  NOMD2, NOCH19
      CHARACTER*80  FORM1, FORM2, FORM3
      CHARACTER*2000  CHAIN1, CHAIN2, CHAIN3, CHAIN4
C
C     ------------------------------------------------------------------
C====
C 1. PREALABLES
C====
C
      CALL JEMARQ()
C
C 1.1. ==> EXISTENCE DE LA STRUCTURE
C
      NOMD2 = NOMCON
      CALL JELIRA(NOMD2//'.DESC','NOMMAX',NBNOSY,K8B)
      IF ( NBNOSY .EQ. 0 ) THEN
         CALL U2MESK('A','UTILITAI4_34',1,NOMD2)
         GOTO 9999
      ENDIF
C
C 1.2. ==> CONSTANTES
C
C              1234567890123456
      BLANC = '                '
      RUNDF = R8VIDE()
C
C====
C 2. ON PARCOURT LA STRUCTURE DE RESULTAT
C====
C
C 2.1. ==> UNE STRUCTURE DE RESULTAT A-T-ELLE ETE CONSTRUITE ?
C          SI NON, ON PASSE A LA SUIVANTE.
C
C                                 9012345678901234
      CALL JEEXIN ( NOMD2(1:8)//'           .DESC', J )
      IF ( J .EQ. 0 ) THEN
        GOTO 9999
      ENDIF
C
C 2.2. ==> NUMEROS D'ORDRE
C
      CALL RSORAC ( NOMD2, 'LONUTI', IBID, R8B, K8B, C16B, R8B, K8B,
     &              NBORDT, 1, IBID )
C
      IF ( NBORDT.EQ.1 ) THEN
        WRITE (IFI,10001) NOMD2(1:8)
      ELSE
        WRITE (IFI,10002) NOMD2(1:8), NBORDT
      ENDIF
C
10001 FORMAT(/,1X,'STRUCTURE DU CONCEPT ',A,' CALCULE POUR 1',
     &            ' NUMERO D''ORDRE')
10002 FORMAT(/,1X,'STRUCTURE DU CONCEPT ',A,' CALCULE POUR ',I10,
     &            ' NUMEROS D''ORDRE')
C
      CALL WKVECT('&&'//NOMPRO//'.NUME_ORDRE','V V I',NBORDT,LRES)
      CALL RSORAC(NOMD2,'TOUT_ORDRE',IBID,R8B,K8B,C16B,R8B,K8B,
     &                                             ZI(LRES),NBORDT,IBID)
C
C 2.3. ==> NOMS SYMBOLIQUES
C
      CALL WKVECT('&&'//NOMPRO//'.NOM_SYMBOL','V V K16',NBNOSY,LNOSY)
      CALL WKVECT('&&'//NOMPRO//'.NUM_SYMBOL','V V K16',NBNOSY,LATAC)
      INOMSY = 0
      DO 241 ISY = 1,NBNOSY
         CALL JENUNO(JEXNUM(NOMD2//'.DESC',ISY),NOMSYM)
         CALL JENONU ( JEXNOM(NOMD2//'.DESC',NOMSYM),IBID)
         CALL JEVEUO(JEXNUM(NOMD2//'.TACH',IBID),'L',IATACH)
         DO 2411 I = 1,NBORDT
            IF ( ZK24(IATACH-1+I)(1:1) .NE. ' ' ) THEN
               INOMSY = INOMSY + 1
               LG = LXLGUT( NOMSYM )
               LB = ( 16 - LG ) / 2
               ZK16(LNOSY+INOMSY-1) = BLANC(1:LB)//NOMSYM
               ZK16(LATAC+INOMSY-1) = NOMSYM
               GOTO 241
            ENDIF
 2411    CONTINUE
  241  CONTINUE
C
      IF ( INOMSY.EQ. 0 ) THEN
        WRITE (IFI,'(/,1X,A)') 'LISTE DES NOMS SYMBOLIQUES: AUCUN'
        GOTO 2430
      ENDIF
C
      CALL CODENT ( INOMSY, 'D', NOMB1 )
      FORM1 = '(1X,''!'',1X,A10,1X,'//NOMB1//'(''!'',A16),''!'')'
      LONGT = 17 * INOMSY
      IF (LONGT.GT.2000) THEN
         CALL U2MESS('A','UTILITAI4_36')
         GOTO 9999
      ENDIF
      CALL CODENT ( LONGT, 'G', NOMB1 )
      FORM2 = '(1X,''!'',1X,I10,1X,''!'',A'//NOMB1//')'
      FORM3 = '(1X,''!'',1X,A10,1X,''!'',A'//NOMB1//')'
C
      WRITE (IFI,'(/,1X,A)') 'LISTE DES NOMS SYMBOLIQUES:'
C
      CALL WKVECT('&&'//NOMPRO//'.TIRET','V V K16',MAX(INOMSY,1),LTIRT)
      CALL WKVECT('&&'//NOMPRO//'.POINT','V V K16',MAX(INOMSY,1),LPOIN)
      DO 242 I = 1,INOMSY
C                          1234567890123456
        ZK16(LTIRT+I-1) = '----------------'
        ZK16(LPOIN+I-1) = '      ...       '
  242 CONTINUE
      WRITE (IFI,FORM1) '----------', ( ZK16(LTIRT+J-1), J=1,INOMSY )
      WRITE (IFI,FORM1) 'NUME_ORDRE', ( ZK16(LNOSY+J-1), J=1,INOMSY )
      WRITE (IFI,FORM1) '----------', ( ZK16(LTIRT+J-1), J=1,INOMSY )
C
      CHAIN1 = ' '
      CHAIN3 = ' '
C
      DO 243 I = 1 , NBORDT
        CHAIN2 = ' '
        CHAIN4 = ' '
        IPCD = 1
C
C RECHERCHE DES NOMS SYMBOLIQUES POUR LE NUMERO D'ORDRE COURANT, I
C
        DO 2431 ISY = 1,INOMSY
           IPCF = IPCD + 16 - 1
           NOMSYM = ZK16(LATAC+ISY-1)
           CALL RSEXCH(' ',NOMD2,NOMSYM,ZI(LRES+I-1),NOCH19,IRET)
           IF ( IRET .EQ. 0 ) THEN
              CALL DISMOI('F','NOM_GD',NOCH19,'CHAMP',IBID,NOMGD,IBID)
              LG = LXLGUT( NOMGD )
              LB = ( 16 - LG ) / 2
              CHAIN2(IPCD:IPCF) = BLANC(1:LB)//NOMGD(1:LG)//BLANC
              CHAIN4(IPCD:IPCF) = ZK16(LPOIN+ISY-1)
           ELSE
              CHAIN2(IPCD:IPCF) = BLANC
              CHAIN4(IPCD:IPCF) = BLANC
           ENDIF
           IPCD = IPCF + 1
           CHAIN2(IPCD:IPCD) = '!'
           CHAIN4(IPCD:IPCD) = '!'
           IPCD = IPCD + 1
 2431     CONTINUE
C
C ECRITURE : ON ECRIT TOUJOURS LA PREMIERE ET LA DERNIERE LIGNE. AU
C            MILIEU, ON N'ECRIT QUE SI LE TEXTE A CHANGE.
C APRES CHAQUE ECRITURE, ON MEMORISE II, NUMERO D'ORDRE QUI A ETE ECRIT
C . 1ERE LIGNE : LA CHAINE COMPLETE
C
         IF ( I .EQ. 1 ) THEN
           WRITE (IFI,FORM2) ZI(LRES+I-1) , CHAIN2(1:LONGT)
           II = 1
C
C . SI LE NOUVEAU TEXTE, CHAIN2, EST DIFFERENT DE CELUI DE LA LIGNE
C   PRECEDENTE, CHAIN1
C . OU SI C'EST LA DERNIERE LIGNE
C
         ELSEIF ( CHAIN1.NE.CHAIN2 .OR. I.EQ.NBORDT ) THEN
C          . ON VIENT JUSTE D'ECRIRE CHAIN1
           IF ( II .EQ. (I-1) ) THEN
             WRITE (IFI,FORM2) ZI(LRES+I-1) , CHAIN2(1:LONGT)
C          . ON A ECRIT CHAIN1 DEUX NUMEROS AVANT : ON ECRIT LE
C          NUMERO PRECEDENT, I-1, ET LE COURANT, I.
           ELSEIF ( II .EQ. (I-2) ) THEN
             WRITE (IFI,FORM2) ZI(LRES+I-2) , CHAIN1(1:LONGT)
             WRITE (IFI,FORM2) ZI(LRES+I-1) , CHAIN2(1:LONGT)
C          . ON A ECRIT CHAIN1 PLUS DE DEUX NUMEROS AVANT : ON ECRIT
C          UNE LIGNE DE POINTILLES, LE NUMERO PRECEDENT, I-1, ET LE
C          NUMERO COURANT, I.
           ELSE
             IF ( CHAIN1.NE.CHAIN2 ) THEN
               IF ( II.EQ.(I-3) ) THEN
                 WRITE (IFI,FORM2) ZI(LRES+I-3) , CHAIN1(1:LONGT)
               ELSE
                 WRITE (IFI,FORM3) '       ...' , CHAIN3(1:LONGT)
               ENDIF
               WRITE (IFI,FORM2) ZI(LRES+I-2) , CHAIN1(1:LONGT)
             ELSE
               WRITE (IFI,FORM3) '       ...' , CHAIN3(1:LONGT)
             ENDIF
             WRITE (IFI,FORM2) ZI(LRES+I-1) , CHAIN2(1:LONGT)
           ENDIF
           II = I
C
         ENDIF
         CHAIN1(1:LONGT) = CHAIN2(1:LONGT)
         CHAIN3(1:LONGT) = CHAIN4(1:LONGT)
C
  243 CONTINUE
C
      WRITE (IFI,FORM1) '----------', ( ZK16(LTIRT+J-1), J=1,INOMSY )
C
 2430 CONTINUE
C
      CALL JEDETR ( '&&'//NOMPRO//'.POINTEUR' )
      CALL JEDETR ( '&&'//NOMPRO//'.COMPT' )
      CALL JEDETR ( '&&'//NOMPRO//'.TIRET' )
      CALL JEDETR ( '&&'//NOMPRO//'.POINT' )
      CALL JEDETR ( '&&'//NOMPRO//'.NUM_SYMBOL' )
      CALL JEDETR ( '&&'//NOMPRO//'.NOM_SYMBOL' )
C
C     ------------------------------------------------------------------
C
C 2.4. ==>      --- LES NOMS DES VARIABLES D'ACCES ---
C
      CALL RSNOPA(NOMD2,2,'&&'//NOMPRO//'.NOMS_PARA',NBAC,NBPA)
      CALL JEVEUO('&&'//NOMPRO//'.NOMS_PARA','L',JPA)
      IF ( NBAC .NE. 0 ) THEN
         WRITE (IFI,'(/,1X,A)') 'LISTE DES NOMS DE VARIABLES D''ACCES:'
         DO 25 IAC = 1,NBAC
            CALL RSADPA ( NOMD2,'L',1,ZK16(JPA-1+IAC),ZI(LRES),1,
     &                               IAD,CTYPE)
            IF (CTYPE(1:1).EQ.'I') THEN
               WRITE (IFI,'(38X,A,A)') ZK16(JPA-1+IAC),' DE TYPE  I'
            ELSEIF (CTYPE(1:1).EQ.'R') THEN
               IF ( ZR(IAD) .NE. RUNDF ) THEN
               WRITE (IFI,'(38X,A,A)') ZK16(JPA-1+IAC),' DE TYPE  R'
               ENDIF
            ELSEIF (CTYPE(1:3).EQ.'K80') THEN
               WRITE (IFI,'(38X,A,A)') ZK16(JPA-1+IAC),' DE TYPE  K80'
            ELSEIF (CTYPE(1:3).EQ.'K32') THEN
               WRITE (IFI,'(38X,A,A)') ZK16(JPA-1+IAC),' DE TYPE  K32'
            ELSEIF (CTYPE(1:3).EQ.'K24') THEN
               WRITE (IFI,'(38X,A,A)') ZK16(JPA-1+IAC),' DE TYPE  K24'
            ELSEIF (CTYPE(1:3).EQ.'K16') THEN
               WRITE (IFI,'(38X,A,A)') ZK16(JPA-1+IAC),' DE TYPE  K16'
            ELSEIF (CTYPE(1:2).EQ.'K8') THEN
               WRITE (IFI,'(38X,A,A)') ZK16(JPA-1+IAC),' DE TYPE  K8'
            ENDIF
 25      CONTINUE
      ENDIF
C
C     ------------------------------------------------------------------
C
C 2.5. ==>          --- LES NOMS DES PARAMETRES ---
C
      IF ( NBPA .NE. 0 ) THEN
         CALL WKVECT('&&'//NOMPRO//'.NOM_PARA','V V K16',NBPA,LNOPA)
         CALL WKVECT('&&'//NOMPRO//'.NUM_PARA','V V K16',NBPA,LNUPA)
         IPAR   = 0
         DO 261 I = 1,NBORDT
            DO 2611 J = 1,NBPA
               NOPARA = ZK16(JPA-1+J+NBAC)
               CALL RSADPA(NOMD2,'L',1,NOPARA,ZI(LRES+I-1),1,IAD,CTYPE)
               IF     ( CTYPE(1:1) .EQ. 'I' ) THEN
               ELSEIF ( CTYPE(1:1) .EQ. 'R' ) THEN
                  IF ( ZR(IAD) .EQ. RUNDF ) GOTO 2611
               ELSEIF ( CTYPE(1:1) .EQ. 'K' ) THEN
               ELSE
                  GOTO 2611
               ENDIF
               LG = LXLGUT( NOPARA )
               LB = ( 16 - LG ) / 2
               NOPAR2 = BLANC(1:LB)//NOPARA
               DO 2612 K = 1 , IPAR
                  IF ( ZK16(LNOPA+K-1) .EQ. NOPAR2 ) GOTO 2611
 2612          CONTINUE
               IPAR = IPAR + 1
               ZK16(LNOPA+IPAR-1) = NOPAR2
               ZK16(LNUPA+IPAR-1) = NOPARA
 2611       CONTINUE
  261    CONTINUE
C
         CALL CODENT ( IPAR , 'D', NOMB1 )
         FORM1 = '(1X,''!'',1X,A10,1X,'//NOMB1//'(''!'',A16),''!'')'
         IPCD = 17 * IPAR
         CALL CODENT ( IPCD, 'G', NOMB1 )
         FORM2 = '(1X,''!'',1X,I10,1X,''!'',A'//NOMB1//')'
C
         CALL WKVECT('&&'//NOMPRO//'.TIRET','V V K16',MAX(IPAR,1),LTIRT)
         CALL WKVECT('&&'//NOMPRO//'.POINT','V V K16',MAX(IPAR,1),LPOIN)
         DO 262 I = 1,IPAR
            ZK16(LTIRT+I-1) = '----------------'
            ZK16(LPOIN+I-1) = '      ...       '
  262    CONTINUE
C
         WRITE (IFI,'(/,1X,A)') 'LISTE DES NOMS DE PARAMETRES:'
         WRITE (IFI,FORM1) '----------', ( ZK16(LTIRT+J-1), J=1,IPAR )
         WRITE (IFI,FORM1) 'NUME_ORDRE', ( ZK16(LNOPA+J-1), J=1,IPAR )
         WRITE (IFI,FORM1) '----------', ( ZK16(LTIRT+J-1), J=1,IPAR )
C
         CHAIN1 = ' '
         DO 263 I = 1,NBORDT
C RECHERCHE DES NOMS DES PARAMETRES POUR LE NUMERO D'ORDRE COURANT, I
C
            CHAIN2 = ' '
            IPCD = 1
            DO 2631 J = 1,IPAR
               IPCF = IPCD + 15
               NOPARA = ZK16(LNUPA+J-1)
               CALL RSADPA(NOMD2,'L',1,NOPARA,ZI(LRES+I-1),1,IAD,CTYPE)
               IF (CTYPE(1:1).EQ.'I') THEN
                  CHAIN2(IPCD:IPCF) = '       I        '
               ELSEIF (CTYPE(1:1).EQ.'R'  ) THEN
                  IF ( ZR(IAD) .NE. RUNDF ) THEN
                     CHAIN2(IPCD:IPCF) = '       R        '
                  ENDIF
               ELSEIF (CTYPE(1:3).EQ.'K80') THEN
                  CHAIN2(IPCD:IPCF) = '      K80        '
               ELSEIF (CTYPE(1:3).EQ.'K32') THEN
                  CHAIN2(IPCD:IPCF) = '      K32        '
               ELSEIF (CTYPE(1:3).EQ.'K24') THEN
                  CHAIN2(IPCD:IPCF) = '      K24        '
               ELSEIF (CTYPE(1:3).EQ.'K16') THEN
                  CHAIN2(IPCD:IPCF) = '      K16        '
               ELSEIF (CTYPE(1:2).EQ.'K8') THEN
                  CHAIN2(IPCD:IPCF) = '       K8        '
               ELSE
                  CHAIN2(IPCD:IPCF) = BLANC
               ENDIF
               IPCD = IPCF + 1
               CHAIN2(IPCD:IPCD) = '!'
               IPCD = IPCD + 1
 2631       CONTINUE
C
C ECRITURE : ON ECRIT TOUJOURS LA PREMIERE ET LA DERNIERE LIGNE. AU
C            MILIEU, ON N'ECRIT QUE SI LE TEXTE A CHANGE.
C APRES CHAQUE ECRITURE, ON MEMORISE II, NUMERO D'ORDRE QUI A ETE ECRIT
C . 1ERE LIGNE : LA CHAINE COMPLETE
C
          IF ( I .EQ. 1 ) THEN
           WRITE (IFI,FORM2) ZI(LRES+I-1) , CHAIN2
           II = 1
C
C . SI LE NOUVEAU TEXTE, CHAIN2, EST DIFFERENT DE CELUI DE LA LIGNE
C   PRECEDENTE, CHAIN1
C . OU SI C'EST LA DERNIERE LIGNE
C
          ELSEIF ( CHAIN1.NE.CHAIN2 .OR. I.EQ.NBORDT ) THEN
C          . ON VIENT JUSTE D'ECRIRE CHAIN1
           IF ( II .EQ. (I-1) ) THEN
             WRITE (IFI,FORM2) ZI(LRES+I-1) , CHAIN2
C          . ON A ECRIT CHAIN1 DEUX NUMEROS AVANT : ON ECRIT LE
C          NUMERO PRECEDENT, I-1, ET LE COURANT, I.
           ELSEIF ( II .EQ. (I-2) ) THEN
             WRITE (IFI,FORM2) ZI(LRES+I-2) , CHAIN1
             WRITE (IFI,FORM2) ZI(LRES+I-1) , CHAIN2
C          . ON A ECRIT CHAIN1 PLUS DE DEUX NUMEROS AVANT : ON ECRIT
C          UNE LIGNE DE POINTILLES, LE NUMERO PRECEDENT, I-1, ET LE
C          NUMERO COURANT, I.
           ELSE
             IF ( CHAIN1.NE.CHAIN2 ) THEN
               IF ( II.EQ.(I-3) ) THEN
                 WRITE (IFI,FORM2) ZI(LRES+I-3) , CHAIN1
               ELSE
                WRITE (IFI,FORM1)'       ...',(ZK16(LPOIN+K-1),K=1,IPAR)
               ENDIF
               WRITE (IFI,FORM2) ZI(LRES+I-2) , CHAIN1
             ELSE
               WRITE (IFI,FORM1)'       ...',(ZK16(LPOIN+K-1),K=1,IPAR)
             ENDIF
             WRITE (IFI,FORM2) ZI(LRES+I-1) , CHAIN2
           ENDIF
           II = I
          ENDIF
          CHAIN1 = CHAIN2
  263   CONTINUE
         WRITE (IFI,FORM1) '----------', ( ZK16(LTIRT+K-1), K=1,IPAR )
         CALL JEDETR ( '&&'//NOMPRO//'.TIRET'     )
         CALL JEDETR ( '&&'//NOMPRO//'.POINT'     )
         CALL JEDETR ( '&&'//NOMPRO//'.NOM_PARA'  )
         CALL JEDETR ( '&&'//NOMPRO//'.NUM_PARA'  )
         CALL JEDETR ( '&&'//NOMPRO//'.PARA_EXIS' )
      ENDIF
C
C 2.6. ==> MENAGE PARTIEL
C
      CALL JEDETR ( '&&'//NOMPRO//'.NOMS_PARA'  )
      CALL JEDETR ( '&&'//NOMPRO//'.NUME_ORDRE' )
C
C 3. LA FIN
C
 9999 CONTINUE
      CALL JEDEMA()
      END
