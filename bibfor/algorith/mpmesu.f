      SUBROUTINE MPMESU ( NOMCHA , NBMESU , NBINST )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/07/2002   AUTEUR VABHHTS J.PELLET 
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
C TOLE CRP_20 CRS_512
C     ------------------------------------------------------------------
C
C     PROJ_MESU_MODAL : LECTURE DES VALEURS DE MESURE SUR FICHIER IDEAS
C                       ( DATASET 58 )
C
C     OUT : NBMESU : NOMBRE DE NOEUDS LUS
C     OUT : NBINST : NOMBRE D'INSTANTS LUS
C
C     ------------------------------------------------------------------
C
      IMPLICIT NONE
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
      CHARACTER*32 JEXNOM,JEXNUM
C
C----------  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      CHARACTER*6   NOMROU
      CHARACTER*16  NOMCMD
      CHARACTER*8   K8BID
      CHARACTER * 80  LIGNE , REPEM1
      LOGICAL       FOUND
C
      INTEGER ITYPE
      INTEGER I
      INTEGER II
      INTEGER NBPAIR
      INTEGER IDIR
      INTEGER NBMESU
      INTEGER NBINST
      INTEGER LINST
      INTEGER LDIR
      INTEGER NBNMES
      INTEGER IDEAS
      INTEGER NIDEAS
      INTEGER LNOMES
      INTEGER ICHAMP
      INTEGER IEXP
      INTEGER IFONCT
      INTEGER LMES
C
      CHARACTER * 8 LABK8
C
      INTEGER LABEL
      INTEGER IFORM
C
      INTEGER LCORR
      INTEGER NBLI3 , REST3
      INTEGER NBLI6 , REST6
      INTEGER LTE , LVA
      INTEGER LONG , NUNOE , LXLGUT
C
      REAL * 8 AMIN
      REAL * 8 XINCR
      REAL * 8 PAS
      REAL * 8 DIFF
      REAL * 8 PASNUL
C
      CHARACTER * 8 MAILLA
      CHARACTER * 16 NOMCHA
      INTEGER IBID
      INTEGER INOEUD
C
      CHARACTER * 7 CRIT
      REAL * 8 PREC, LIMITE
C
      DATA REPEM1/' '/
      DATA FOUND/.FALSE./
      DATA NOMROU/'MPMESU'/ , NOMCMD/'&PROJ_MESU_MODAL'/
C
C-----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      REPEM1 (  1 : 50 ) =
     & '    -1                                            '
C
      REPEM1 ( 51 : 80 ) =
     & '                              '
C
C++++ SAISIE DU NOM DE CHAMPS
C
      CALL GETVTX ( 'MESURE' , 'NOM_CHAM'      ,1,1,1,  NOMCHA , IBID )
C
C++++ NOM DU MAILLAGE EXPERIMENTAL ( UNITE LGOIQUE 33 )
C
      CALL GETVID ( 'MESURE' , 'MAILLAGE' ,1,1,1 , MAILLA , IBID )
C
C++++ INITIALISATION D UN COMPTEUR
C
      NBINST = 0
C
C++++ OBJETS NUAGE EXPERIMENTAL DE POINTS
C
      CALL JEVEUO ( NOMCMD//'.NOMNOE' , 'L'      , LNOMES )
C
      CALL JELIRA ( NOMCMD//'.NOMNOE' , 'LONMAX' , NBNMES , K8BID )
C
C---- CAS DU PREMIER NOEUD
C     CORRESPONDANCE AVEC LES NOEUDS DU MAILLAGE :
C
      CALL WKVECT ( NOMCMD//'.CORRES' , 'V V I' , NBNMES*3 , LCORR )
C
C---- DIRECTIONS DE MESURE
C
      CALL WKVECT ( NOMCMD//'.DIRECT' , 'V V I' , NBNMES*3 , LDIR  )
C
C++++ UNTE IDEAS VENANT DU FICHIER DE COMMANDES
C
      CALL GETVIS ( 'MESURE' , 'UNITE',1,1,1, IDEAS , NIDEAS )
C
      CALL ASOPEN ( IDEAS, ' ' )
C
C++++ PRECISION ET CRITERE POUR COMPARER SI 2 INSTANTS SONT EGAUX
C
      CALL GETVR8 ( 'MESURE' , 'PRECISION',1,1,1, PREC , IBID )
      CALL GETVTX ( 'MESURE' , 'CRITERE',1,1,1, CRIT , IBID )
C
C---- INITIALISATION DU NUMERO DU POINT DE MESURE
C
      NBMESU = 0
C
C---- RECHERCHE DU DATASET 58
C
C++++ JE CONTINUE JUSQU A TROUVER LE PREMIER -1
C
 10   CONTINUE
C
      READ ( IDEAS , 1000 , END = 999 ) LIGNE
C
      IF ( LIGNE . NE . REPEM1 ) GO TO 10
C
C++++ JE CONTINUE JUSQU A TROUVER LE DEUXIEME -1
C
 15   CONTINUE
C
      READ ( IDEAS , 1000 , END = 999 ) LIGNE
C
      IF ( LIGNE . EQ . REPEM1 ) GO TO 15
C
C++++ SI CE N EST PAS UN DATASET58
C
      IF ( LIGNE ( 1 : 6 ) . NE . '    58' ) THEN
C
         CALL UTMESS ( 'I' , NOMROU ,
     &    ' LE DATASET "'//LIGNE( 1 : 6 )//'" N EST PAS TRAITE ' )
         GO TO 10
C
      END IF
C
C---- TRAITEMENT DU DATASET 58
C
      FOUND = .TRUE.
C
      DO 20 I = 1 , 5
C
C+++++++ JE NE SAIS PAS
C
         READ ( IDEAS , 1000 , END = 999 ) LIGNE
         IF ( LIGNE . EQ . REPEM1 ) GO TO 999
C
 20   CONTINUE
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C---- LECTURE RECORD 6 ( 6 EME LIGNE APRES 58 )
C
C     IFONCT = TYPE DE FONCTION
C
C     LABK8  = NOM DU NOEUD ( NOM DANS LE FICHIER MAILLAGE POINTS)
C
C     LABEL  = LABEL DU NOEUD MESURE
C     IDIR   = DIRECTION DE LA MESURE
C-----------------------------------------------------------------------
C
      READ ( IDEAS , 1060 ) IFONCT , LABK8 , LABEL , IDIR
C
C++++ TYPE DE FONCTION = IFONCT
C
      IF ( IFONCT . NE . 1 )
     &   CALL UTMESS ( 'F' , NOMROU , 'SEULES LES DONNEES DE MESURES '//
     &               'TEMPORELLES SONT ACTUELLEMENT TRAITEES'//
     &               ' PAR L''OPERATEUR PROJ_MESU_MODAL' )
C
C++++ EXISTENCE DU NOEUD LABK8 DANS LE MAILLAGE U 21
C
      DO 34 I = 1 , NBNMES
C
         IF ( LABK8 . EQ . ZK8 ( LNOMES - 1 + I ) ) GO TO 35
C
 34   CONTINUE
C
C++++ LE NOEUD N EXISTE PAS JE L IGNORE
C
      LONG = LXLGUT ( LABK8 )
C
      CALL UTMESS ( 'A' , NOMROU ,
     & ' LE NOEUD "'//LABK8( 1 : LONG )//'" DANS LE DATASET58 ' //
     & ' ( UNITE LOGIQUE 33 PAR DEFAUT ) ' //
     & ' N''EXISTE PAS DANS LE ' //
     & ' FICHIER DE MAILLAGE EXPERIMENTAL ' //
     & ' ( UNITE LOGIQUE 21 PAR DEFAUT) : IL EST DONC IGNORE' )
C
      GO TO 10
C
C++++ LE NOEUD EXISTE JE CONTINUE
C
 35   CONTINUE
C
C++++ NUMERO DU POINT DE MESURE
C
      NBMESU = NBMESU + 1
C
C---- NOM DU NOEUD A SON NUMERO
C
      CALL JENONU ( JEXNOM ( MAILLA//'.NOMNOE' , LABK8 ) , INOEUD )
C
C---- STOCKAGE DU NUMERO DDL DU NOEUD
C
      ZI ( LCORR - 1 + NBMESU ) = INOEUD
C
      IF (IDIR.GT.3 .OR. IDIR.LT.-3 .OR. IDIR.EQ.0) THEN
         CALL UTDEBM('F',NOMROU,'SEULES LES DIRECTION DE MESURE EN '//
     &               'TRANSLATION +X -X +Y -Y +Z -Z SONT RECONNUES'//
     &              ' PAR L''OPERATEUR PROJ_MESU_MODAL')

         CALL UTIMPK('L' , 'NOEUD DE MESURE        : '  , 1 , LABK8 )
         CALL UTIMPI('L' , 'FAUSSE DIRECTION DE MESURE' , 1 , IDIR  )
         CALL UTFINM
      ENDIF
C
C---- STOCKAGE DE LA DIRECTION DE MESURE
      ZI ( LDIR  - 1 + NBMESU ) = IDIR
C
C---- ON VERIFIE QU'AUCUNE MESURE N'EXISTE POUR CE INOEUD ET +/-IDIR
C---- (PAS DE VERIF POUR LA PREMIERE MESURE)
      IF (NBMESU .GT. 1) THEN
          DO 36 I=1,NBMESU-1
              IF ((ZI(LCORR-1+I) .EQ. INOEUD) .AND.
     &           ((ZI(LDIR-1+I) .EQ. IDIR) .OR.
     &           (ZI(LDIR-1+I) .EQ. -IDIR)     )   ) THEN
                  CALL UTDEBM('F',NOMROU,'DEUX MESURES CORRESPONDENT '//
     &                'AU MEME NOEUD + OU - DANS LA MEME DIRECTION')
                  CALL UTIMPI('L','PREMIERE MESURE        : ',1,I)
                  CALL UTIMPI('L','DEUXIEME MESURE        : ',1,NBMESU)
                  CALL UTIMPK('L','NOEUD DE MESURE COMMUN : ',1,LABK8)
                  CALL UTIMPI('L','+/- DIRECTION COMMUNE  : ',1,IDIR)
                  CALL UTFINM
              END IF
 36       CONTINUE
      END IF
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C---- LECTURE RECORD 7

C     IFORM   = FORMAT ECRITURE DES DONNEES ( 2 REEL SIMPLE PRECISION)
C     NBPAIR  = NOMBRE DE PAIRES DE DONNEES (COUPLES OU PAS REGULIER)
C     ITYPE   = ECHANILLONAGE ( 0  COUPLES  OU   1  PAS REGULIER )
C     AMIN    = ABSCISSE MINIMUM (0 SI ECHATILLONAGE EXPLICITE )
C     XINCR   = INCREMENT DE L ECHATILLONAGE (0 SI ECH. EXPLICITE )
C-----------------------------------------------------------------------
C
      READ ( IDEAS , 1000 ) LIGNE
      READ ( LIGNE , 1070 ) IFORM , NBPAIR , ITYPE
C
      IF ( IFORM . NE . 2 )
     &   CALL UTMESS('F',NOMROU,'SEULES LES DONNEES EXPERIMENTALES '//
     &        'REELLES SIMPLE PRECISION SONT ACTUELLEMENT TRAITEES'//
     &        ' PAR L''OPERATEUR PROJ_MESU_MODAL')
C
      IF ((ITYPE .NE. 0) .AND. (ITYPE .NE. 1))
     &   CALL UTMESS('F',NOMROU,'SEULS LES TYPES D''ECHANTILLON '//
     &        '0 OU 1 SONT AUTORISES DANS LE DATASET 58')
C
C++++ LECTURE DU PREMIER COUPLE
C
      IF ( ITYPE . EQ . 1 ) READ ( LIGNE , 1075 ) AMIN , XINCR
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C---- LECTURE RECORD 8
C
C     IFONCT  = TYPE DE DONNEES ( 17 SI FONCTION )
C-----------------------------------------------------------------------
C
      READ ( IDEAS , 1080 ) IFONCT
C
      IF ( IFONCT . NE . 17 )
     &   CALL UTMESS('F',NOMROU,'SEULES LES DONNEES DE MESURES '//
     &               'TEMPORELLES SONT ACTUELLEMENT TRAITEES'//
     &        ' PAR L''OPERATEUR PROJ_MESU_MODAL')
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C---- LECTURE RECORD 9
C
C     ICHAMP   = TYPE DE DONNEES ( 8 SI DEPLACEMENT )
C     IEXP     = EXPOSANT DES UNITES DE LONGUEUR
C-----------------------------------------------------------------------
C
      READ ( IDEAS , 1090 ) ICHAMP , IEXP
C
      IF ( NOMCHA ( 1 : 4 ) . EQ . 'DEPL' . AND . ICHAMP . NE . 8 )
     &   CALL UTMESS ( 'F' , NOMROU ,
     & ' NOM_CHAM : DEPL DANS LE FICHIER DE COMMANDE ' //
     & ' DONC ICHAMP = 8 RECORD 8 DANS LE DATASET58 '//
     & 'OBLIGATOIREMENT' )
C
      IF ( NOMCHA ( 1 : 14 ) . EQ . 'SIGM_NOEU_DEPL'
     &       . AND . ICHAMP . NE . 2 )
     &   CALL UTMESS ( 'F' , NOMROU ,
     & ' NOM_CHAM : SIGM_NOEU_DEPL DANS LE FICHIER DE COMMANDE ' //
     & ' DONC ICHAMP = 2 RECORD 8 DANS LE DATASET58 '//
     & 'OBLIGATOIREMENT' )
C
      IF ( NOMCHA ( 1 : 14 ) . EQ . 'EPSI_NOEU_DEPL'
     &       . AND . ICHAMP . NE . 3 )
     &   CALL UTMESS ( 'F' , NOMROU ,
     & ' NOM_CHAM : EPSI_NOEU_DEPL DANS LE FICHIER DE COMMANDE ' //
     & ' DONC ICHAMP = 3 RECORD 8 DANS LE DATASET58 '//
     & 'OBLIGATOIREMENT' )
C
      IF ( ICHAMP .NE. 2 .AND. ICHAMP .NE. 3 .AND. ICHAMP .NE. 8 )
     &   CALL UTMESS ( 'F' , NOMROU ,
     &        'SEULES LES DONNEES DE MESURES DE TYPE '//
     &        'DEPLACEMENT ( ICHAMP = 8 RECORD 8 DANS DATASET58 )'//
     &        'DEFORMATION ( ICHAMP = 3 RECORD 8 DANS DATASET58 )'//
     &        'CONTRAINTE  ( ICHAMP = 2 RECORD 8 DANS DATASET58 )'//
     &        'SONT ACTUELLEMENT TRAITES PAR PROJ_MESU_MODAL ' )
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C++++ ON PASSE LES RECORD 10 ET 11
C-----------------------------------------------------------------------
C
      READ ( IDEAS , 1000 ) LIGNE
      READ ( IDEAS , 1000 ) LIGNE
C
C---- ARRANGEMENTS SELON TYPE ECHANTILLONAGE
C
      IF      ( ITYPE . EQ . 1 ) THEN
C
C++++++++++++ PAS REGULIER
C
              NBLI6 = INT ( NBPAIR / 6 )
              REST6  = NBPAIR - ( NBLI6 * 6 )
C
      ELSE IF ( ITYPE . EQ . 0 ) THEN
C
              NBLI3 = INT ( NBPAIR / 3 )
              REST3  = NBPAIR - ( NBLI3 * 3 )
C
      END IF
C
C---- CREATION DES OBJETS DE STOCKAGE
C
      IF ( NBINST . EQ . 0 ) THEN
C
         NBINST = NBPAIR
C
         CALL WKVECT ( NOMCMD//'.TEMPS','V V R' , NBINST , LTE )
         CALL WKVECT ( NOMCMD//'.VALEU','V V R' , NBINST , LVA )
C
C------- LISTE DES INSTANTS
C
         CALL WKVECT ( NOMCMD//'.INST','V V R' , NBINST , LINST)
C
C------- VALEURS DE MESURES :
C
         CALL WKVECT ( NOMCMD//'.MES.VAL','V V R',NBINST*NBNMES*3,LMES)
C
      ELSE
C
C------- SINON VERIFICATION DU NOMBRE D'INSTANTS
C
         IF ( NBPAIR . NE . NBINST )
     &     CALL UTMESS ('F' , NOMROU ,
     &             'LE NOMBRE DE PAS DE TEMPS DOIT ETRE LE MEME '//
     &             'POUR TOUTES LES COMPOSANTES MESURES')
C
      ENDIF
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C---- LECTURE RECORD 12  ( LES VALEURS DES MESURES )
C
C     DEUX CAS DE FIGURE : COUPLES OU PAS REGULIER
C-----------------------------------------------------------------------
C
      IF      ( ITYPE . EQ . 1 ) THEN
C
C++++++++++++ PAS REGULIER
C
C ON VERIFIE QUE LE PAS N'EST PAS NUL
          PASNUL=1.D-12
          IF (XINCR .LT. PASNUL)
     &        CALL UTMESS('F',NOMROU,'LE PAS D''ECHANTILLONAGE '//
     &             'CONSTANT EST QUASI NUL ')
C
          DO 110 I = 1 , NBLI6
C
              DO 111 II = 1 , 6
                  ZR(LTE - 1 + ( I - 1 ) * 6 + II) =
     &                AMIN + (( I - 1 ) * 6 + II - 1) * XINCR
 111          CONTINUE
C
              READ ( IDEAS , 1000 ) LIGNE
C
C             SI LA LIGNE VAUT -1 OU CONTIENT MOINS DE 6 TERMES
              IF ((LIGNE .EQ. REPEM1) .OR.
     &            (LIGNE(5*13+1:6*13) .EQ. '             ')) GO TO 250
C
              READ ( LIGNE , 1120 )
     &            (ZR (LVA - 1 + ( I - 1 ) * 6 + II ) , II = 1,6)
C
 110     CONTINUE
C
          IF (REST6 .GT. 0) THEN
C
              DO 112 II = 1 , REST6
                  ZR(LTE - 1 + ( NBLI6 ) * 6 + II) =
     &                AMIN + ( ( I - 1 ) * 6 + II - 1 ) * XINCR
 112          CONTINUE
C
              READ ( IDEAS , 1000 ) LIGNE
C
C             SI LA LIGNE VAUT -1 OU CONTIENT MOINS DE REST6 TERMES :
              IF ((LIGNE .EQ. REPEM1) .OR.
     &          (LIGNE((REST6-1)*13+1:REST6*13) .EQ. '             '))
     &          GO TO 250
C
C             SI LA LIGNE CONTIENT PLUS DE REST6 TERMES :
              IF ((REST6 .LT. 6) .AND.
     &          (LIGNE(REST6*13+1:(REST6+1)*13) .NE. '             '))
     &          GO TO 250
C
              READ ( LIGNE , 1120 )
     &            (ZR (LVA - 1 + ( NBLI6 ) * 6 + II ) , II = 1,REST6)
C
          END IF
C
C
      ELSE IF ( ITYPE . EQ . 0 ) THEN
C
C++++++++++++ CAS DE COUPLES
C
          DO 210 I = 1 , NBLI3
C
              READ ( IDEAS , 1000 ) LIGNE
C
C             SI LA LIGNE VAUT -1 OU CONTIENT MOINS DE 6 TERMES
              IF ((LIGNE .EQ. REPEM1) .OR.
     &            (LIGNE(5*13+1:6*13) .EQ. '             ')) GO TO 250
C
              READ ( LIGNE , 1120 )
     &            (ZR (LTE - 1 + ( I - 1 ) * 3 + II) ,
     &             ZR (LVA - 1 + ( I - 1 ) * 3 + II) , II = 1,3)
C
C
 210      CONTINUE
C
C
          IF (REST3 .GT. 0) THEN
              READ ( IDEAS , 1000 ) LIGNE
C
C             SI LA LIGNE VAUT -1 OU CONTIENT MOINS DE 2*REST3 TERMES :
              REST6=2*REST3
C
              IF ((LIGNE .EQ. REPEM1) .OR.
     &          (LIGNE((REST6-1)*13+1:REST6*13) .EQ. '             '))
     &          GO TO 250
C
C             SI LA LIGNE CONTIENT PLUS DE REST6 TERMES :
              IF ((REST6 .LT. 6) .AND.
     &          (LIGNE(REST6*13+1:(REST6+1)*13) .NE. '             '))
     &          GO TO 250
C
              READ ( LIGNE , 1120 )
     &            (ZR (LTE - 1 + ( NBLI3 ) * 3 + II) ,
     &             ZR (LVA - 1 + ( NBLI3 ) * 3 + II) , II = 1,REST3)
C
          END IF
C
      END IF
C
      GO TO 255
C
 250  CONTINUE
      CALL UTDEBM('F',NOMROU,'INCOHERANCE DANS LE DATASET 58 : '//
     &     'LE NOMBRE DE VALEURS FOURNIES NE CORRESPOND PAS AU '//
     &     'NOMBRE DE VALEURS ATTENDUES')
      CALL UTIMPI('L','MESURE CONCERNEE : ',1,NBMESU)
      CALL UTFINM
C
 255  CONTINUE
C
C
      IF ( NBMESU . EQ . 1 ) THEN
C
C+++++++ JE COPIE L OBJET NOMCMD//'.TEMPS' DANS NOMCMD//'.INST'
C
         DO 120 I = 1 ,  NBINST
C
            ZR ( LINST - 1 + I ) = ZR ( LTE - 1 + I )
C
 120     CONTINUE
C
      ELSE
C
C+++++++ JE TESTE L OBJET NOMCMD//'.TEMPS' DANS NOMCMD//'.INST'
C
         DO 130 I = 1 ,  NBINST
C
C++++++++++ PAS SELON L INSTANT
C
            IF ( I . EQ . 1 ) THEN
C
               PAS = ZR ( LINST + 1 ) - ZR ( LINST )
C
            ELSE
C
               PAS = ZR ( LINST -1+I ) - ZR ( LINST -1+I - 1 )
C
            END IF
C
C ON VERIFIE QUE LE PAS N'EST PAS NUL
            PASNUL=1.D-12
            IF (PAS .LT. PASNUL) THEN
              CALL UTDEBM('F',NOMROU,'LE PAS D''ECHANTILLONAGE '//
     &                    'VARIABLE EST QUASI NUL :')
              CALL UTIMPI('L','INSTANT : ',1,I)
              CALL UTIMPR('L','PAS     : ',1,PAS)
              CALL UTFINM
            END IF
C
C++++++++++ DIFFERENCE DE TEMPS
C
            DIFF = ABS ( ZR (LINST-1+I) - ZR (LTE-1+I) )
C
C++++++++++ CRITERE DE L EGALITE DES TEMPS
C
            IF (CRIT .EQ. 'RELATIF') THEN
              LIMITE = PAS * PREC
            ENDIF
C
            IF ( DIFF .GT. LIMITE ) CALL UTMESS ( 'F' , NOMROU ,
     &     ' LE CRITERE D EGALITE DU TEMPS' //
     &     ' SUR LES POINTS DE MESURE N EST PAS VERIFIE ' )
C
 130     CONTINUE
C
      END IF
C
      DO 140 I = 1 , NBINST
C
             ZR ( LMES - 1 + ( NBMESU - 1 ) * NBINST + I ) =
     &       ZR ( LVA  - 1 + I ) * ( 10.D0 * * IEXP )
C
 140  CONTINUE
C
C
C
C
C
C=======================================================================
C
C
C---- RECHERCHE DU NOEUD DE MESURE SUIVANT ---
C
      READ ( IDEAS , 1000 , END = 999 ) LIGNE
C
      IF ( LIGNE . NE . REPEM1 ) THEN
          CALL UTDEBM('F',NOMROU,'INCOHERANCE DANS LE DATASET 58 : '//
     &        'LE NOMBRE DE VALEURS FOURNIES EST SUPERIEUR AU NOMBRE '//
     &        'DE VALEURS ATTENDUES OU LE "-1" DE FIN DE DATASET 58'//
     &        'EST ABSENT')
          CALL UTIMPI('L','MESURE CONCERNEE : ',1,NBMESU)
          CALL UTFINM
      END IF
C
      GO TO 10
C
 999  CONTINUE
C
      IF ( . NOT . FOUND )
     &CALL UTMESS('F',NOMROU,' FICHIER IDEAS : DATASET 58 NON TROUVE')
C
      IF (NBMESU .EQ. 0)
     &CALL UTMESS('F',NOMROU,' FICHIER IDEAS : NOEUDS DE MESURE NON '//
     &                      'TROUVES')
C
C---- STOCKAGE DU NOMBRE DE NOEUDS LUS SUR LE FICHIER IDEAS
C
      CALL JEECRA ( NOMCMD//'.CORRES','LONUTI',NBMESU,K8BID)
      CALL JEECRA ( NOMCMD//'.DIRECT','LONUTI',NBMESU,K8BID)
      CALL JEECRA ( NOMCMD//'.MES.VAL','LONUTI', NBMESU ,K8BID)
C
      CALL JEDEMA()
C
 1000 FORMAT ( A80 )
C
 1060 FORMAT ( I5 , 25X , 1X , A8 , 2X , I10 , I4 )
C
 1070 FORMAT ( 3I10 )
C
 1075 FORMAT ( 30X , 2E13.5 )
C
 1080 FORMAT ( I10 )
C
 1090 FORMAT ( I10, I5 )
C
 1120 FORMAT ( 6 E13.5 )
C
      END
