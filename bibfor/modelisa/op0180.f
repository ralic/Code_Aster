      SUBROUTINE OP0180(IER)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 05/10/2004   AUTEUR REZETTE C.REZETTE 
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
C  DESCRIPTION :
C  -----------       O P E R A T E U R    D E F I _ C A B L E _ B P
C
C  OUT  : IER = 0 => TOUT S EST BIEN PASSE
C         IER > 0 => NOMBRE D ERREURS RENCONTREES
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
      CHARACTER*32 JEXNOM, JEXNUM
C     ----- FIN   COMMUNS NORMALISES  JEVEUX  --------------------------
C
C ARGUMENTS
C ---------
      INTEGER       IER
C
C VARIABLES LOCALES
C -----------------
      INTEGER       IBID, ICABL, ICMP, IRANA1, IRET, JCABA, JNBNO,
     &              JNCMP, JSIEF, JVALV, N1, N2, NBANCR, NBCABL, NBF0,
     &              NBMAMA, NBNOBE, NBNOMA, NCABA, NRELAX, NSIEF,
     &              NBMABE, JLIMAB
      REAL*8        DELTA, EA, F0, FRCO, FRLI, MU0, RH1000, RJ,
     &              SA, FPRG, XFLU, XRET
      LOGICAL       MAIL2D, RELAX
      CHARACTER*1   K1B
      CHARACTER*3   K3B
      CHARACTER*8   CAELEM, CHMAT, GRMABE, K8B, MAILLA, MODELE,
     &               NOANCR(2), NOMU, TYPANC(2)
      CHARACTER*16  CMD, CONCEP
      CHARACTER*19  CARSIG, CARTE, LIGRMO, LIRELA, NUMACA, NUNOBE,
     &              XNOCA, YNOCA, ZNOCA, NOMT19
      CHARACTER*24  CADESC,NCNCIN,NMABET
      CHARACTER*8   AIRE, EFFNOR, GNO(2)
      INTEGER       NBPAR
      PARAMETER    (NBPAR=13)
      CHARACTER*2   TYPPAR(NBPAR)
      CHARACTER*24  NOMPAR(NBPAR)
C
      DATA          AIRE  /'A1      '/
      DATA          EFFNOR/'N       '/
      DATA          TYPPAR /'I ','K8','R ','R ','R ',
     &                      'K8','K8','I ','I ','R ','K8','K8','K8'/
      DATA          NOMPAR /'NUME_CABLE              ',
     &                      'NOEUD_CABLE             ',
     &                      'ABSC_CURV               ',
     &                      'ALPHA                   ',
     &                      'TENSION                 ',
     &                      'MAILLE_BETON_VOISINE    ',
     &                      'NOEUD_BETON_VOISIN      ',
     &                      'INDICE_IMMERSION        ',
     &                      'INDICE_PROJECTION       ',
     &                      'EXCENTRICITE            ',
     &                      'NOM_CABLE               ',
     &                      'NOM_ANCRAGE1            ',
     &                      'NOM_ANCRAGE2            '/
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
      CALL JEMARQ()
      CALL INFMAJ()
C
      CALL GETRES(NOMU,CONCEP,CMD)
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 1   SAISIE DES ARGUMENTS POUR VERIFICATION AVANT EXECUTION
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      CALL GETFAC('DEFI_CABLE',NBCABL)
      NBCABL = ABS(NBCABL)
      CALL GETVR8(' ','TENSION_INIT',0,1,1,F0,IBID)
      CALL GETVR8(' ','RECUL_ANCRAGE',0,1,1,DELTA,IBID)
      CALL GETFAC('RELAXATION',NRELAX)
      IF ( NRELAX.NE.0 ) THEN
         CALL GETVR8('RELAXATION','R_J',1,1,1,RJ,IBID)
         IF ( RJ.EQ.0.0D0 ) THEN
            RELAX = .FALSE.
         ELSE
            RELAX = .TRUE.
         ENDIF
      ELSE
         RELAX = .FALSE.
      ENDIF

C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 2   VERIFICATION DES ARGUMENTS AVANT EXECUTION
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      DO 10 ICABL = 1, NBCABL
C
        CALL GETVID('DEFI_CABLE','NOEUD_ANCRAGE'   ,ICABL,1,0,K8B,N1)
        CALL GETVID('DEFI_CABLE','GROUP_NO_ANCRAGE',ICABL,1,0,K8B,N2)
        NBANCR = N1 + N2
        IF ( ABS(NBANCR).NE.2 ) THEN
           WRITE(K3B,'(I3)') ICABL
           IF ( N1.NE.0 ) THEN
              CALL UTMESS('F',CMD,'MOT-CLE <DEFI_CABLE>, OCCURENCE '
     &                 //'NO '//K3B//', OPERANDE <NOEUD_ANCRAGE> : '
     &                 //'IL FAUT DEFINIR 2 NOEUDS D''ANCRAGE')
           ELSE
              CALL UTMESS('F',CMD,'MOT-CLE <DEFI_CABLE>, OCCURENCE '
     &              //'NO '//K3B//', OPERANDE <GROUP_NO_ANCRAGE> : '
     &                    //'IL FAUT DEFINIR 2 GROUP_NO D''ANCRAGE')
           ENDIF
        ELSE
           IF ( N1.NE.0 ) THEN
              CALL GETVID('DEFI_CABLE','NOEUD_ANCRAGE',ICABL,1,2,
     &                    NOANCR(1),IBID)
              IF ( NOANCR(1).EQ.NOANCR(2) ) THEN
                 WRITE(K3B,'(I3)') ICABL
                 CALL UTMESS('F',CMD,'MOT-CLE <DEFI_CABLE>, '//
     &                    'OCCURENCE NO '//K3B//', OPERANDE '//
     &                    '<NOEUD_ANCRAGE> : LES 2 NOEUDS '//
     &                    'D''ANCRAGE DOIVENT ETRE DISTINCTS')
              ENDIF
           ELSE
              CALL GETVID('DEFI_CABLE','GROUP_NO_ANCRAGE',ICABL,1,2,
     &                    NOANCR(1),IBID)
              IF ( NOANCR(1).EQ.NOANCR(2) ) THEN
                 WRITE(K3B,'(I3)') ICABL
                 CALL UTMESS('F',CMD,'MOT-CLE <DEFI_CABLE>, '//
     &                    'OCCURENCE NO '//K3B//', OPERANDE '//
     &                    '<GROUP_NO_ANCRAGE> : LES 2 GROUP_NO '//
     &                    'D''ANCRAGE DOIVENT ETRE DISTINCTS')
              ENDIF
           ENDIF
        ENDIF
C
C TEST DU TYPE D'ANCRAGE
C    LE CATALOGUE ASSURE QU'IL Y A DEUX OCCURENCES DE CE MOT-CLE
        CALL GETVID(' ','TYPE_ANCRAGE',ICABL,1,2,TYPANC(1),IBID)

C    SI TYPES D'ANCRAGE SONT TOUS LES DEUX PASSIFS     
        IF ((TYPANC(1).EQ.'PASSIF').AND.(TYPANC(2).EQ.'PASSIF')) THEN
          WRITE(K3B,'(I3)') ICABL
C    SI LA TENSION EST NULLE : SIMPLE ALARME
          IF (F0.EQ.0.D0) THEN 
            CALL UTMESS('A',CMD,'MOT-CLE <DEFI_CABLE>, '//
     &                    'OCCURENCE NO '//K3B//', OPERANDE '//
     &                    'TYPE ANCRAGE : LES 2 EXTREMITES SONT '//
     &                    'PASSIVES -> ARMATURE PASSIVE')
          ELSE
            CALL UTMESS('F',CMD,'MOT-CLE <DEFI_CABLE>, '//
     &                    'OCCURENCE NO '//K3B//', OPERANDE '//
     &                    'TYPE ANCRAGE : LES 2 EXTREMITES SONT '//
     &                    'PASSIVES ET LA TENSION QUE VOUS VOULEZ' //
     &                    'IMPOSER EST NON-NULLE : IMPOSSIBLE !')
          
          ENDIF
        ENDIF
C    SI LA TENSION EST NON-NULLE : ARRET FATAL          
        
C 
  10  CONTINUE
C
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 3   SAISIE DES ARGUMENTS A L'EXECUTION
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      CALL GETVID(' ','MODELE'        ,0,1,1,MODELE,IBID)
      CALL GETVID(' ','CHAM_MATER'    ,0,1,1,CHMAT ,IBID)
      CALL GETVID(' ','CARA_ELEM'     ,0,1,1,CAELEM,IBID)
      CALL TITRE

C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 4   EXECUTION DES OPERATIONS
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C 4.1 RECUPERATION DU NOM DU CONCEPT MAILLAGE
C ---
      CALL DISMOI('F','NOM_MAILLA',MODELE,'MODELE',IBID,MAILLA,IRET)
      CALL DISMOI('F','NB_MA_MAILLA',MAILLA,'MAILLAGE',NBMAMA,K8B,IRET)
      CALL DISMOI('F','NB_NO_MAILLA',MAILLA,'MAILLAGE',NBNOMA,K8B,IRET)
C
C 4.2 CREATION DES OBJETS DE TRAVAIL
C ---
      CALL WKVECT('&&OP0180.NBNO_CABLE','V V I',NBCABL,JNBNO)
C
      NUNOBE = '&&OP0180.NUMNOE_BET'
      CALL JECREO(NUNOBE,'V V I')
      CALL JEECRA(NUNOBE,'LONMAX',NBNOMA,' ')
C
      NUMACA = '&&OP0180.NUMAIL_CAB'
      CALL JECREO(NUMACA,'V V I')
      CALL JEECRA(NUMACA,'LONMAX',NBMAMA,' ')
C
      XNOCA  = '&&OP0180.X_NOEU_CAB'
      CALL JECREO(XNOCA ,'V V R')
      CALL JEECRA(XNOCA,'LONMAX',NBNOMA,' ')
      YNOCA  = '&&OP0180.Y_NOEU_CAB'
      CALL JECREO(YNOCA ,'V V R')
      CALL JEECRA(YNOCA,'LONMAX',NBNOMA,' ')
      ZNOCA  = '&&OP0180.Z_NOEU_CAB'
      CALL JECREO(ZNOCA ,'V V R')
      CALL JEECRA(ZNOCA,'LONMAX',NBNOMA,' ')
C
C 4.3 EXTENSION DES CARTES ELEMENTAIRES : CREATION DE VECTEURS
C --- D'ADRESSES DES CARACTERISTIQUES POINTES PAR LE NUMERO DE
C     MAILLE
C
      LIGRMO = MODELE//'.MODELE    '
C
      CARTE = CHMAT//'.CHAMP_MAT '
      CADESC = CARTE//'.DESC'
      CALL JEEXIN(CADESC,IRET)
      IF ( IRET.EQ.0 )
     &    CALL UTMESS('F',CMD,'LA CARTE DES CARACTERISTIQUES '//
     &    'MATERIELLES DES ELEMENTS N EXISTE PAS. IL FAUT '//
     &    'PREALABLEMENT AFFECTER CES CARACTERISTIQUES EN '//
     &    'UTILISANT LA COMMANDE <AFFE_MATERIAU>')
      CALL ETENCA(CARTE,LIGRMO,IRET)
      IF ( IRET.NE.0 )
     &   CALL UTMESS('F',CMD,'ERREUR A L APPEL DE LA ROUTINE ETENCA '//
     &   'POUR EXTENSION DE LA CARTE '//CARTE)
C
      CARTE = CAELEM//'.CARGENBA  '
      CADESC = CARTE//'.DESC'
      CALL JEEXIN(CADESC,IRET)
      IF ( IRET.EQ.0 )
     &   CALL UTMESS('F',CMD,'LA CARTE DES CARACTERISTIQUES '//
     &   'GEOMETRIQUES DES ELEMENTS DE BARRE DE SECTION GENERALE '//
     &   'N EXISTE PAS. IL FAUT PREALABLEMENT AFFECTER CES '//
     &   'CARACTERISTIQUES EN UTILISANT LA COMMANDE '//
     &   '<AFFE_CARA_ELEM>')
      CALL ETENCA(CARTE,LIGRMO,IRET)
      IF ( IRET.NE.0 )
     &   CALL UTMESS('F',CMD,'ERREUR A L APPEL DE LA ROUTINE ETENCA '//
     &   'POUR EXTENSION DE LA CARTE '//CARTE)
C
C.... DETERMINATION DU RANG DE LA COMPOSANTE <A1>
C.... DE LA GRANDEUR <CAGNBA>
C
      CALL JELIRA(JEXNOM('&CATA.GD.NOMCMP','CAGNBA'),'LONMAX',NCABA,K1B)
      CALL JEVEUO(JEXNOM('&CATA.GD.NOMCMP','CAGNBA'),'L',JCABA)
      IRANA1 = 0
      DO 20 ICMP = 1, NCABA
         IF ( ZK8(JCABA+ICMP-1).EQ.AIRE ) THEN
            IRANA1 = ICMP
            GO TO 21
         ENDIF
  20  CONTINUE
  21  CONTINUE
      IF ( IRANA1.EQ.0 )
     &   CALL UTMESS('F',CMD,'PROBLEME POUR DETERMINER LE RANG DE LA '//
     &   'COMPOSANTE <A1> DE LA GRANDEUR <CAGNBA>')
C
C 4.4 CREATION DE LA SD TABLE RESULTAT
C ---
      CALL JEEXIN ( NOMU//'           .LTNT', IRET )
      IF (IRET.EQ.0) CALL LTCRSD ( NOMU, 'G' )
      NOMT19 = ' '
      CALL LTNOTB ( NOMU, 'CABLE_BP', NOMT19 )
      CALL JEEXIN ( NOMT19//'.TBBA', IRET )
      IF (IRET.EQ.0) CALL TBCRSD ( NOMT19, 'G' )
      CALL TBAJPA ( NOMT19, NBPAR, NOMPAR, TYPPAR )


C
C 4.5 CREATION ET INITIALISATION DE LA CARTE ELEMENTAIRE
C --- DES CONTRAINTES INITIALES
C
      CARSIG = NOMU//'.CHME.SIGIN'
      CALL ALCART('G',CARSIG,MAILLA,'SIEF_R',NBMAMA,NBMAMA)
C
      CALL JELIRA(JEXNOM('&CATA.GD.NOMCMP','SIEF_R'),'LONMAX',NSIEF,K1B)
      CALL JEVEUO(JEXNOM('&CATA.GD.NOMCMP','SIEF_R'),'L',JSIEF)
      CALL JEVEUO(CARSIG//'.NCMP','E',JNCMP)
      CALL JEVEUO(CARSIG//'.VALV','E',JVALV)
      DO 30 ICMP = 1, NSIEF
         ZK8(JNCMP+ICMP-1) = ZK8(JSIEF+ICMP-1)
         ZR(JVALV+ICMP-1) = 0.0D0
  30  CONTINUE
      CALL NOCART(CARSIG,1,' ','NOM',0,' ',0,LIGRMO,NSIEF)
      ZK8(JNCMP) = EFFNOR
      CALL JELIBE(CARSIG//'.NCMP')
      CALL JELIBE(CARSIG//'.VALV')
C
C 4.6 CREATION DE LA SD DE TYPE LISTE_DE_RELATIONS
C ---
      LIRELA = NOMU//'.LIRELA    '
      CALL CRELRL('REEL','REEL','G',LIRELA)
C
C 4.7 CARACTERISATION DE LA TOPOLOGIE DE LA STRUCTURE BETON
C --- ET RECUPERATION DES CARACTERISTIQUES DU MATERIAU CONSTITUTIF
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 3   CREATION DE LA CONNECTIVITE INVERSE LIMITEE AU GROUP_MA BETON
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      NCNCIN = '&&OP0180.CONNECINVBETON '
      NMABET = '&&OP0180.MABETON '

      CALL RELIEM(' ',MAILLA,'NU_MAILLE',' ',0,1,
     &            'GROUP_MA_BETON','GROUP_MA',
     &            NMABET,NBMABE)
C
      CALL JEEXIN(NCNCIN,N2)
      CALL JEVEUO(NMABET,'L',JLIMAB)
C
      IF ( N2 .EQ. 0 ) CALL CNCINV ( MAILLA, ZI(JLIMAB),NBMABE,
     &                               'V', NCNCIN )
C
      CALL TOMABE(CHMAT,NMABET,NBMABE,MAILLA,NBNOMA,
     &            MAIL2D,NBNOBE,NUNOBE,XFLU,XRET)
C
C 4.8 BOUCLE SUR LE NOMBRE DE CABLES
C ---
      DO 40 ICABL = 1, NBCABL
C
C 4.8.1  CARACTERISATION DE LA TOPOLOGIE DU CABLE
C .....
         CALL TOPOCA(NOMT19,MAILLA,ICABL,NBF0,ZI(JNBNO),NUMACA)
C
C 4.8.2  RECUPERATION DES CARACTERISTIQUES ELEMENTAIRES DU CABLE
C .....
         CALL CAELCA(MODELE,CHMAT,CAELEM,IRANA1,
     &               ICABL,ZI(JNBNO),NUMACA,
     &               RELAX,EA,RH1000,MU0,FPRG,FRCO,FRLI,SA)
C
C 4.8.3  INTERPOLATION DE LA TRAJECTOIRE DU CABLE
C .....
         CALL TRAJCA(NOMT19,MAILLA,ICABL,ZI(JNBNO),XNOCA,YNOCA,ZNOCA)
C
C 4.8.4  CALCUL DE LA TENSION LE LONG DU CABLE
C .....
         CALL TENSCA(NOMT19,ICABL,ZI(JNBNO),NBF0,F0,DELTA,RELAX,RJ,
     &               XFLU,XRET,EA,RH1000,MU0,FPRG,FRCO,FRLI,SA)
C
C 4.8.5  MISE A JOUR DE LA CARTE ELEMENTAIRE DES CONTRAINTES INITIALES
C .....
         CALL SIGMCA(NOMT19,CARSIG,ICABL,ZI(JNBNO),NUMACA)
C
C 4.8.6  DETERMINATION DES RELATIONS CINEMATIQUES ENTRE LES DDL DES
C .....  NOEUDS DU CABLE ET CEUX DES NOEUDS DE LA STRUCTURE BETON
C
C......  REPRESENTATION DE LA STRUCTURE BETON PAR DES MAILLES 2D : 
C......  PROJECTION DU CABLE SUR LE MAILLAGE BETON
C
         IF ( MAIL2D ) THEN
            CALL PROJCA(NOMT19,LIRELA,NMABET,NBMABE,MAILLA,
     &                  NBNOBE,NUNOBE,ICABL,ZI(JNBNO),XNOCA,YNOCA,ZNOCA)
C
C......  REPRESENTATION DE LA STRUCTURE BETON PAR DES MAILLES 3D : 
C......  IMMERSION DU CABLE DANS LE MAILLAGE BETON
C
         ELSE
            CALL IMMECA(NOMT19,LIRELA,MAILLA,
     &                  NBNOBE,NUNOBE,ICABL,ZI(JNBNO),XNOCA,YNOCA,ZNOCA,
     &                  NCNCIN,NMABET)
         ENDIF
C
  40  CONTINUE
C
      CALL JEDEMA()
C
C --- FIN DE OP0180.
      END
