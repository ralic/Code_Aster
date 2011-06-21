      SUBROUTINE CGNOOR ( RESU, NOMAIL, MOTFAC, IOCC, NBMC, MOTCLE, 
     &                  TYPMCL, TYPFON, NBMA, NDORIG, NDEXTR, TYPM)
      IMPLICIT   NONE
      INTEGER             IOCC, NBMC, NBMA
      CHARACTER*6         TYPFON
      CHARACTER*8         RESU, NOMAIL, NDORIG, NDEXTR, TYPM
      CHARACTER*16        MOTCLE(*), TYPMCL(*)
      CHARACTER*(*)       MOTFAC
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 21/06/2011   AUTEUR MACOCCO K.MACOCCO 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C-----------------------------------------------------------------------
C FONCTION REALISEE:
C
C     CONSTRUCTION D'UN VECTEUR DE NOEUDS A PARTIR DE LA DONNEE DE 
C     MAILLES (MAILLE ou GROUP_MA)
C     UTILISE DANS DEFI_GROUP ET DEFI_FOND_FISS
C
C     ENTREES:
C        RESU       : NOM DU CONCEPT RESULTAT DE L'OPERATEUR
C        NOMAIL     : NOM DU MAILLAGE
C        MOTFAC     : MOT-CLE FACTEUR
C        IOCC       : OCCURENCE COURANTE DE MOTFAC
C        NBMC       : NOMBRE DE MOT-CLE SIMPLE 
C        MOTCLE     : MOT-CLE SIMPLE TYPE MA OU GROUP_MA A TRAITER
C        TYPMCL     : TYPE D'ENTITE ENTREE SOUS LE MOT-CLE
C        TYPFON     : TYPE DE FOND 
C                       IL PEUT VALOIR OUVERT/FERME/INF/SUP
C     SORTIES:
C        NBMA       : NOMBRE DE MAILLES CONSIDEREES
C        NDORIG     : NOM DU NOEUD ORIGINE
C        NDEXTR     : NOM DU NOEUD EXTREMITE
C        TYPM       : TYPE DE MAILLE
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
      INTEGER       JMAIL, JTYPM, IATYMA
      INTEGER       IER, IM, N1, N2, NID, NIG, NBNOT
      INTEGER       NUNORI, TROUV, IBID, IN, ND
      INTEGER       EXISTE, IRET 
      INTEGER       JCOUR1,JCOUR2,JCOUR3,JCOUR4
      CHARACTER*6   NOMPRO
      CHARACTER*8   K8B, NOMMA, TYPMP
      CHARACTER*16  K16BID, NOMCMD
      CHARACTER*24  CONEC, TYPP, NOMMAI, NOMNOE, MESMAI,VALK(2)
      LOGICAL       BUG
C DEB-------------------------------------------------------------------
      CALL JEMARQ()
C
      CALL GETRES ( K8B, K16BID, NOMCMD )
C
C     ------------------------------------------------------------------
C     INITIALISATION DE VARIABLES
C     ------------------------------------------------------------------
      NOMPRO = 'CGNOOR'
      MESMAI = '&&'//NOMPRO//'.MES_MAILLES'
      CONEC  = NOMAIL//'.CONNEX         '
      TYPP   = NOMAIL//'.TYPMAIL        '
      NOMMAI = NOMAIL//'.NOMMAI         '
      NOMNOE = NOMAIL//'.NOMNOE         '
      CALL DISMOI('F','NB_NO_MAILLA',NOMAIL,'MAILLAGE',NBNOT,K8B,IER)
      CALL JEVEUO ( TYPP,   'L', IATYMA )
C
C
C     ------------------------------------------------------------------
C     RECUPERATION DES MAILLES SOUS LES MOT-CLES "MOTCLE'
C     ------------------------------------------------------------------
      CALL RELIEM ( ' ', NOMAIL, 'NO_MAILLE', MOTFAC, IOCC, NBMC,
     &                                  MOTCLE, TYPMCL, MESMAI, NBMA )
      
C     CETTE VERIFICATION N'EST NECESSAIRE QUE POUR DEFI_GROUP
      IF ( NBMA .EQ. 0 ) THEN
        CALL U2MESS('F','ELEMENTS_66')
      ENDIF     
      CALL JEVEUO ( MESMAI, 'L', JMAIL  )
C
C     ------------------------------------------------------------------
C     --- CONSTRUCTION D'UN VECTEUR DE TRAVAIL LOCAL CONTENANT 
C     --- LES NUMEROS DES MAILLES A TRAITER
C     ------------------------------------------------------------------
      CALL WKVECT(RESU//'.MAILLESFOURNIES' ,'V V I',NBMA ,JCOUR2)
      DO 10, IM = 1 , NBMA
         CALL JENONU ( JEXNOM(NOMMAI,ZK8(JMAIL-1 + IM)), IBID )
         ZI(JCOUR2-1 + IM) = IBID
 10   CONTINUE
 
C
C
C     ------------------------------------------------------------------
C     --- VERIFICATION DE L'EXISTENCE DES MAILLES ET GROUPES DE MAILLES
C     --- VERIFICATION QUE LES MAILLES SONT TOUTES SEG2 OU TOUTES SEG3
C     ------------------------------------------------------------------
C
C     LA VERIFICATION D'EXISTENCE N'EST NECESSAIRE QUE POUR DEFI_GROUP
C
      TYPMP = ' '
      IER    = 0
      DO 20, IM = 1 , NBMA 
         NOMMA = ZK8(JMAIL-1 + IM)
         CALL JEEXIN ( JEXNOM(NOMMAI,NOMMA), EXISTE )
         IF ( EXISTE .EQ. 0 ) THEN
            IER = IER + 1
            CALL U2MESG('E', 'ELEMENTS5_19',1,NOMMA,1,IOCC,0,0.D0)
         ELSE
            CALL JENONU ( JEXNOM(NOMMAI,NOMMA), IBID )
            JTYPM = IATYMA-1+IBID
            CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(JTYPM)),TYPM)
            IF ( TYPM(1:3) .NE. 'SEG' ) THEN
              IF (NOMCMD.NE.'DEFI_GROUP') THEN
                CALL U2MESS('F','RUPTURE0_63')
              ENDIF
              IER = IER + 1
              CALL U2MESG('E', 'ELEMENTS5_20',1,NOMMA,1,IOCC,0,0.D0)
            ENDIF
            IF (IM.GT.1) THEN
              IF (TYPM.NE.TYPMP) THEN
                IER = IER + 1
                CALL U2MESG('E', 'ELEMENTS5_21',1,NOMMA,1,IOCC,0,0.D0)
              ENDIF
            ENDIF
            TYPMP = TYPM
         ENDIF
 20   CONTINUE
      IF ( IER .GT. 0 ) THEN
         CALL U2MESI('F','ELEMENTS5_15',1,IOCC)
      ENDIF

      
C --- LECTURE DU NOM DU NOEUD ORIGINE (S'IL EST FOURNI)

      CALL GETVTX ( MOTFAC, 'NOEUD_ORIG',    IOCC,1,0, K8B, N1 )
      CALL GETVTX ( MOTFAC, 'GROUP_NO_ORIG', IOCC,1,0, K8B, N2 )
      IF ( N1 .NE. 0 ) THEN
        CALL GETVTX ( MOTFAC, 'NOEUD_ORIG', IOCC,1,1, NDORIG, N1 )
      ELSE IF (N2 .NE. 0) THEN
        CALL GETVTX ( MOTFAC, 'GROUP_NO_ORIG', IOCC,1,1, K8B, N2)
        CALL UTNONO ( ' ', NOMAIL, 'NOEUD', K8B, NDORIG, IRET )
        IF ( IRET .EQ. 10 ) THEN
          CALL U2MESK('F','ELEMENTS_67',1,K8B)
        ELSEIF ( IRET .EQ. 1 ) THEN
          VALK(1) = 'GROUP_NO_ORIG'
          VALK(2) = NDORIG
          CALL U2MESK('A','ELEMENTS5_17',2,VALK)
        ENDIF
      ELSE
        NDORIG = ' '
      END IF


C --- LECTURE DU NOM DU NOEUD EXTREMITE (S'IL EST FOURNI)

      CALL GETVTX ( MOTFAC, 'NOEUD_EXTR',    IOCC,1,0, K8B, N1 )
      CALL GETVTX ( MOTFAC, 'GROUP_NO_EXTR', IOCC,1,0, K8B, N2 )
      IF ( N1 .NE. 0 ) THEN
         CALL GETVTX ( MOTFAC, 'NOEUD_EXTR', IOCC,1,1, NDEXTR, N1 )
      ELSE IF (N2 .NE. 0) THEN
         CALL GETVTX ( MOTFAC, 'GROUP_NO_EXTR', IOCC,1,1, K8B, N2)
         CALL UTNONO ( ' ', NOMAIL, 'NOEUD', K8B, NDEXTR, IRET )
         IF ( IRET .EQ. 10 ) THEN
            CALL U2MESK('F','ELEMENTS_67',1,K8B)
         ELSEIF ( IRET .EQ. 1 ) THEN
            VALK(1) = 'GROUP_NO_EXTR'
            VALK(2) = NDORIG
            CALL U2MESK('A','ELEMENTS5_17',2,VALK)
         ENDIF
      ELSE
         NDEXTR = ' '
      END IF

C     ------------------------------------------------------------------
C     --- CONSTRUCTION 
C     --- 1 - VECTEUR DE TRAVAIL LOCAL CONTENANT LES NOEUDS EXTREMITES
C     ---     DE CHAQUE MAILLE
C     --- 2 - VECTEUR DE TRAVAIL LOCAL CONTENANT POUR CHAQUE NOEUD 
C     ---     DU MAILLAGE :
C     ---     - 0 SI LE NOEUD N'APPARTIENT AUX MAILLES
C     ---     - 1 SI INTERNE (APPARTIENT A DEUX MAILLES) 
C     ---     - 2 SI EXTREMITE 
C     ------------------------------------------------------------------
      CALL WKVECT('&&'//NOMPRO//'.NOEUDS_EXTREM','V V I',2*NBMA,JCOUR1)
      CALL WKVECT('&&'//NOMPRO//'.TYPE_NOEUD',   'V V I',NBNOT ,JCOUR4)
      DO 30 IM = 1 , NBMA
         CALL I2EXTF(ZI(JCOUR2-1 + IM),1,CONEC(1:15),TYPP(1:16),NIG,NID)
         ZI(JCOUR1-1 +      IM) = NIG
         ZI(JCOUR1-1 + NBMA+IM) = NID
         ZI(JCOUR4-1 + NIG) = ZI(JCOUR4-1 + NIG) + 1
         ZI(JCOUR4-1 + NID) = ZI(JCOUR4-1 + NID) + 1
 30   CONTINUE


C --- VERIFICATION QUE LA LIGNE EST CONTINUE ET UNIQUE

      N1 = 0
      N2 = 0
      BUG = .FALSE.
      DO 40 IM = 1 , NBNOT
C        COMPTAGE DES EXTREMITES
         IF ( ZI(JCOUR4-1 + IM).EQ.1 ) N1 = N1 + 1
C        COMPTAGE NOEUDS APPARTENANT A PLUS DE DEUX MAILLES
         IF ( ZI(JCOUR4-1 + IM).GT.2 ) N2 = N2 + 1
 40   CONTINUE
C     IL NE PEUT Y AVOIR QUE 2 NOEUDS EXTREMITES 
      IF ( N1 .GT. 2 )  BUG = .TRUE.
C     IL NE DOIT PAS Y AVOIR DE NOEUDS APPARTENANT A PLUS DE DEUX 
C     MAILLES
      IF ( N2 .NE. 0 )  BUG = .TRUE.
      IF ( BUG ) THEN
         CALL U2MESS('F','ELEMENTS5_16')
      ENDIF



C     ------------------------------------------------------------------
C     --- IDENTIFICATION DU NOEUD D'ABSCISSE CURVILIGNE 0.
C     ------------------------------------------------------------------

      IF (NDORIG.NE.' ') THEN
        CALL JENONU(JEXNOM(NOMNOE,NDORIG), NUNORI)

C       ON VERIFIE QU'IL S'AGIT BIEN D'UNE EXTREMITE
        TROUV = 0
        DO 50 IM = 1 , NBMA
          IF ( ZI(JCOUR1-1 +      IM) .EQ. NUNORI ) TROUV = TROUV + 1
          IF ( ZI(JCOUR1-1 + NBMA+IM) .EQ. NUNORI ) TROUV = TROUV + 1
 50     CONTINUE

        IF  ( TROUV .EQ. 0 ) CALL U2MESK('F','ELEMENTS_68',1,NDORIG)
        IF (( TROUV .NE. 1 ).AND.(TYPFON.NE.'FERME'))
     &    CALL U2MESK('F','ELEMENTS_69',1,NDORIG)

      ELSE

C     ------------------------------------------------------------------
C     --- SI L'ORIGINE EST DONNEE 
C     --- CONSTRUCTION D'UN VECTEUR DE TRAVAIL LOCAL POUR TROUVER
C     --- L'ORIGINE
C     ------------------------------------------------------------------
        CALL WKVECT('&&'//NOMPRO//'.NOEUD_APPARIES',
     &                                       'V V I',2*NBMA,JCOUR3)     
C       LISTE DES NOEUDS DEJA APPARIES
        DO 60 IN = 1,NBMA*2
          ZI(JCOUR3-1 + IN) = 0
 60     CONTINUE

C       PARCOURS DE L'ENSEMBLE DES NOEUDS
        DO 70 IN = 1, NBMA*2
          IF (ZI(JCOUR3-1 + IN) .NE. 0) GOTO 71
          NUNORI  = ZI(JCOUR1-1 + IN)

          DO 72 ND = IN+1, NBMA*2
            IF (ZI(JCOUR1-1 + ND).EQ.NUNORI) THEN
              ZI(JCOUR3-1 + ND) = 1
              GOTO 71
            END IF
 72       CONTINUE

C         NUNORI N'APPARAIT QU'UNE FOIS : C'EST L'ORIGINE
          GOTO 73
 71       CONTINUE
 70     CONTINUE
        CALL U2MESS('F','ELEMENTS_71')

 73     CONTINUE
        CALL JENUNO(JEXNUM(NOMNOE,NUNORI),NDORIG)
        CALL U2MESK('I','ELEMENTS_72',1,NDORIG)
        CALL JEDETR ( '&&'//NOMPRO//'.NOEUD_APPARIES' )

      ENDIF

      CALL JEDETR ( '&&'//NOMPRO//'.NOEUDS_EXTREM' )
      CALL JEDETR ( '&&'//NOMPRO//'.TYPE_NOEUD'    )
      CALL JEDETR ( MESMAI                         )
C  
      CALL JEDEMA()
      END
