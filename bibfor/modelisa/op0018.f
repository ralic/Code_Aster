      SUBROUTINE OP0018 ( IER )
      IMPLICIT REAL*8 (A-H,O-Z)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 14/05/2002   AUTEUR DURAND C.DURAND 
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
C TOLE CRP_20
C     ------------------------------------------------------------------
C
C                   AFFE_MODELE
C
C     ------------------------------------------------------------------
C        REMARQUES ET RESTRICTIONS D UTILISATION
C
C       LES SEULES VERIFICATIONS FAITES ( FAUX=EXIT ), PORTENT SUR:
C       - L AFFECTATION D ELEMENTS FINIS A TOUTES LES MAILLES DEMANDEES
C       - L AFFECTATION D ELEMENTS FINIS A TOUS LES NOEUDS DEMANDES
C       - L AFFECTATION D ELEMENTS FINIS SUR UNE MAILLE AU MOINS
C     ------------------------------------------------------------------
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
      CHARACTER*32       JEXNOM,JEXNUM,JEXATR
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      CHARACTER*1     TPM
      CHARACTER*4     KIOC,CDIM
      CHARACTER*6     KNUM
      CHARACTER*8     NOMA,   NOMU,   K8B,  VERIF(2)
      CHARACTER*8     TYPENT, TYPEMA, NOMAIL, TABMAI(8)
      CHARACTER*16    CONCEP, CMD, PHENOM, TYPELE, MODELI
      CHARACTER*19    LIGREL
      CHARACTER*24    NOMMAI, NOMNOE, TYPMAI, GRPNOE, GRPMAI, TMPDEF
      CHARACTER*24    CPTNEM, CPTNOM, CPTNBN, CPTLIE, CPTMAI, CPTNOE
C     CHARACTER*24    COOVAL
      LOGICAL         LMAIL, LNOEU, LAXIS
C     ------------------------------------------------------------------

C     FONCTIONS "FORMULES" POUR ACCEDER RAPIDEMENT A LA CONNECTIVITE :
      INTEGER ICONX1,ICONX2,ZZCONX,ZZNBNE
      ZZCONX(IMAIL,J) = ZI(ICONX1-1+ZI(ICONX2+IMAIL-1)+J-1)
      ZZNBNE(IMAIL) = ZI(ICONX2+IMAIL) - ZI(ICONX2+IMAIL-1)
C     ------------------------------------------------------------------

C
      CALL JEMARQ()
C
      LMAIL = .FALSE.
      LNOEU = .FALSE.
      LAXIS = .FALSE.
C
C     RECUPERATION DU NIVEAU D'IMPRESSION
C     -----------------------------------
      CALL INFMAJ
      CALL INFNIV ( IFM , NIV )
C
C ---   INITIALISATION DU NB D'ERREUR
C
C
C ---   RECUPERATION DES ARGUMENTS  DE LA COMMANDE
C
      CALL GETRES ( NOMU , CONCEP , CMD )
      LIGREL = NOMU//'.MODELE'
C
C - MAILLAGE
C
      CALL GETVID(' ','MAILLAGE',1,1,1,NOMA,NBV)
C
C - VERIF
C
      CALL GETVTX(' ','VERIF',1,1,2,VERIF,NBV)
      IMPVER = 0
      IF ( NIV .EQ. 2 ) IMPVER = 1
C
C - AFFE
C
      NDGM = 0
      NDGN = 0
      NDMA = 0
      NDNO = 0
C
      CALL GETFAC ( 'AFFE' , NBOC )
      CALL GETFAC ( 'AFFE_SOUS_STRUC' , NBOC2 )
C
      DO 10 IOC = 1,NBOC
         CALL CODENT(IOC,'G',KIOC)
C
         CALL GETVTX('AFFE','TOUT'        ,IOC,1,0,K8B,NTO)
         CALL GETVEM(NOMA,'GROUP_MA',
     .              'AFFE','GROUP_MA'    ,IOC,1,0,K8B,NGM)
         CALL GETVEM(NOMA,'GROUP_NO',
     .              'AFFE','GROUP_NO'    ,IOC,1,0,K8B,NGN)
         CALL GETVEM(NOMA,'MAILLE',
     .              'AFFE','MAILLE'      ,IOC,1,0,K8B,NMA)
         CALL GETVEM(NOMA,'NOEUD',
     .              'AFFE','NOEUD'       ,IOC,1,0,K8B,NNO)
         CALL GETVTX('AFFE','PHENOMENE'   ,IOC,1,0,K8B,NPH)
         CALL GETVTX('AFFE','MODELISATION',IOC,1,0,K8B,NMO)
C
C
         NDGM = MAX(NDGM,-NGM)
         NDGN = MAX(NDGN,-NGN)
         NDMA = MAX(NDMA,-NMA)
         NDNO = MAX(NDNO,-NNO)
 10   CONTINUE
C
      NDMAX1 = MAX(NDGM,NDGN)
      NDMAX2 = MAX(NDMA,NDNO)
      NDMAX  = MAX(NDMAX1,NDMAX2)
      IF(NDMAX.LE.0)NDMAX = 1
C
C
C
C
C       -- ON TRAITE CE QUI EST COMMUN AUX MODELES AVEC ELEMENTS
C                           ET AUX MODELES AVEC SOUS-STRUCTURES
C       ---------------------------------------------------------------
      CPTNOM = NOMU//'.MODELE    .NOMA'
      CPTNBN = NOMU//'.MODELE    .NBNO'
      CALL WKVECT(CPTNOM,'G V K8',1,JDNM)
      CALL WKVECT(CPTNBN,'G V I' ,1,JDNB)
      ZK8(JDNM) = NOMA
      ZI(JDNB) = 0
C
C       -- RECHERCHE DU PHENOMENE :
      IF (NBOC.GT.0) THEN
         CALL GETVTX('AFFE','PHENOMENE',1,1,1,PHENOM,IBID)
      ELSE IF (NBOC2.GT.0) THEN
         CALL GETVTX('AFFE_SOUS_STRUC','PHENOMENE',1,1,1,PHENOM,IBID)
      END IF
      CALL JEECRA(CPTNOM,'DOCU',IBID,PHENOM(1:4))
C
C       -- S'IL N'Y A PAS D'ELEMENTS ON SAUTE QUELQUES ETAPES:
      IF (NBOC.EQ.0) GO TO 9997
C
C       MODELE AVEC ELEMENTS:
C ---   RECUPERATION DES NOMS JEVEUX DU CONCEPT MAILLAGE
C
      NOMMAI = NOMA//'.NOMMAI'
      NOMNOE = NOMA//'.NOMNOE'
      TYPMAI = NOMA//'.TYPMAIL'
      GRPNOE = NOMA//'.GROUPENO'
      GRPMAI = NOMA//'.GROUPEMA'
C     COOVAL = NOMA//'.COORDO    .VALE'
C
C ---   CONSTRUCTION DES NOMS JEVEUX DU CONCEPT MODELE
C
      TMPDEF = NOMU//'.DEF'
C
      CPTMAI = NOMU//'.MAILLE'
      CPTNOE = NOMU//'.NOEUD'
      CPTLIE = NOMU//'.MODELE    .LIEL'
      CPTNEM = NOMU//'.MODELE    .NEMA'
C
C ---   VERIFICATION DE L EXISTENCE DES MAILLES/NOEUDS/GROUPES & PHE/MOD
C
      CALL WKVECT ( TMPDEF, 'V V K8', NDMAX, JDEF )
C
      DO 20 IOC = 1 , NBOC
C
         CALL CODENT(IOC,'G',KIOC)
         CALL JERAZO(TMPDEF,NDMAX,1)
C
         CALL GETVTX('AFFE','PHENOMENE',IOC,1,1,PHENOM,NPH)
         CALL GETVTX('AFFE','MODELISATION',IOC,1,1,MODELI,NMO)
C
         CALL GETVEM(NOMA,'GROUP_MA',
     .              'AFFE','GROUP_MA',IOC,1,NDMAX,ZK8(JDEF),NG)
         DO 21 J = 1 , NG
           CALL JEEXIN(JEXNOM(GRPMAI,ZK8(JDEF+J-1)),IRET)
           IF ( IRET.EQ.0 ) THEN
              CALL UTMESS('F',CMD,'OCCURENCE '//KIOC//' DE AFFE : LE '//
     +          'GROUPE DE MAILLES "'//ZK8(JDEF+J-1)//'" NE FAIT PAS '//
     +                                'PARTIE DU MAILLAGE "'//NOMA//'"')
           ENDIF
 21      CONTINUE
C
         CALL GETVEM(NOMA,'GROUP_NO',
     .              'AFFE','GROUP_NO',IOC,1,NDMAX,ZK8(JDEF),NG)
         DO 22 J = 1 , NG
           CALL JEEXIN(JEXNOM(GRPNOE,ZK8(JDEF+J-1)),IRET)
           IF ( IRET.EQ.0 ) THEN
              CALL UTMESS('F',CMD,'OCCURENCE '//KIOC//' DE AFFE : LE '//
     +           'GROUPE DE NOEUDS "'//ZK8(JDEF+J-1)//'" NE FAIT PAS '//
     +                                'PARTIE DU MAILLAGE "'//NOMA//'"')
           ENDIF
 22      CONTINUE
C
         CALL GETVEM(NOMA,'NOEUD',
     .              'AFFE','NOEUD',IOC,1,NDMAX,ZK8(JDEF),NG)
         DO 23 J = 1 , NG
           CALL JEEXIN(JEXNOM(NOMNOE,ZK8(JDEF+J-1)),IRET)
           IF ( IRET.EQ.0 ) THEN
              CALL UTMESS('F',CMD,'OCCURENCE '//KIOC//' DE AFFE : LE '//
     +                    'NOEUD "'//ZK8(JDEF+J-1)//'" NE FAIT PAS '//
     +                             'PARTIE DU MAILLAGE "'//NOMA//'"')
           ENDIF
 23      CONTINUE
C
         CALL GETVEM(NOMA,'MAILLE',
     .              'AFFE','MAILLE',IOC,1,NDMAX,ZK8(JDEF),NG)
         DO 24 J = 1 , NG
           CALL JEEXIN(JEXNOM(NOMMAI,ZK8(JDEF+J-1)),IRET)
           IF ( IRET.EQ.0 ) THEN
              CALL UTMESS('F',CMD,'OCCURENCE '//KIOC//' DE AFFE : LA '//
     +                     'MAILLE "'//ZK8(JDEF+J-1)//'" NE FAIT PAS '//
     +                     'PARTIE DU MAILLAGE "'//NOMA//'"')
           ENDIF
 24      CONTINUE
 20   CONTINUE
C
C
C ---   AFFECTATION DES ELEMENTS SUR LES MAILLES ET NOEUDS
C
C -     CREATION DES VECTEURS TAMPONS MAILLES ET NOEUDS
C
      CALL JELIRA(NOMMAI,'NOMMAX',NBMAIL,K8B)
      CALL JELIRA(NOMNOE,'NOMMAX',NBNOEU,K8B)
C
      CALL WKVECT ( CPTMAI, 'G V I', NBMAIL, JDMA )
      CALL WKVECT ( CPTNOE, 'G V I', NBNOEU, JDNO )
      CALL WKVECT ( '&&OP0018.MAILLE', 'V V I', NBMAIL, JMUT )
      CALL WKVECT ( '&&OP0018.NOEUD' , 'V V I', NBNOEU, JNUT )
C
C -     RECUPERATION DU NUMERO DU TYPE MAILLE POI1
C
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','POI1'),NTYPOI)
C
C -     BOUCLE SUR LES OCCURENCES DE AFFE
C
      CALL JEVEUO ( TYPMAI, 'L', JDTM )
C
      DO 100 IOC = 1 , NBOC
C
        CALL GETVTX('AFFE','PHENOMENE'   ,IOC,1,1,PHENOM,NPH)
        CALL GETVTX('AFFE','MODELISATION',IOC,1,1,MODELI,NMO)
C
        CALL JENONU(JEXNOM('&CATA.'//PHENOM(1:13)//'.MODL',MODELI),
     &              IMODL)
        CALL JEVEUO(JEXNUM('&CATA.'//PHENOM,IMODL),'L',JDPM)
C
        IF ( MODELI(1:4).EQ.'AXIS' .OR. MODELI.EQ.'COQUE_AXIS' )
     +      LAXIS = .TRUE.
C
        CALL JERAZO(TMPDEF,NDMAX,1)
        CALL GETVTX('AFFE','TOUT'    ,IOC,1,0    ,K8B      ,NTO)
        CALL GETVEM(NOMA,'GROUP_MA',
     .              'AFFE','GROUP_MA',IOC,1,NDMAX,ZK8(JDEF),NGM)
        CALL GETVEM(NOMA,'GROUP_NO',
     .              'AFFE','GROUP_NO',IOC,1,NDMAX,ZK8(JDEF),NGN)
        CALL GETVEM(NOMA,'NOEUD',
     .              'AFFE','NOEUD'   ,IOC,1,NDMAX,ZK8(JDEF),NNO)
        CALL GETVEM(NOMA,'MAILLE',
     .              'AFFE','MAILLE'  ,IOC,1,NDMAX,ZK8(JDEF),NMA)
C
C -     AFFECTATION DE LA TOTALITE DES MAILLES ( PAS DES NOEUDS ! )
C
        IF ( NTO .NE. 0 ) THEN
          LMAIL = .TRUE.
          DO 55 NUMAIL = 1 , NBMAIL
            NUTYPM = ZI(JDTM+NUMAIL-1)
            ZI(JDMA+NUMAIL-1) = ZI(JDPM+NUTYPM-1)
            ZI(JMUT+NUMAIL-1) = 1
 55       CONTINUE
        ENDIF
C
C -     AFFECTATION DES GROUPES DE MAILLES
C
        IF ( NGM .NE. 0 )THEN
          LMAIL = .TRUE.
          DO 30 I = 1,NGM
            CALL JEVEUO(JEXNOM(GRPMAI,ZK8(JDEF+I-1)),'L',JDGM)
           CALL JELIRA(JEXNOM(GRPMAI,ZK8(JDEF+I-1)),'LONMAX',NBGRMA,K8B)
            DO 40 J = 1 , NBGRMA
              NUMAIL = ZI(JDGM+J-1)
              NUTYPM = ZI(JDTM+NUMAIL-1)
              ZI(JDMA+NUMAIL-1) = ZI(JDPM+NUTYPM-1)
              ZI(JMUT+NUMAIL-1) = 1
 40         CONTINUE
 30       CONTINUE
        ENDIF
C
C -     AFFECTATION DES MAILLES
C
        IF ( NMA .NE. 0 )THEN
          LMAIL = .TRUE.
          DO 50 I = 1 , NMA
            CALL JENONU(JEXNOM(NOMMAI,ZK8(JDEF+I-1)),NUMAIL)
            NUTYPM = ZI(JDTM+NUMAIL-1)
            ZI(JDMA+NUMAIL-1) = ZI(JDPM+NUTYPM-1)
            ZI(JMUT+NUMAIL-1) = 1
 50       CONTINUE
        ENDIF
C
C -     AFFECTATION DES GROUPES DE NOEUDS
C
        IF ( NGN .NE. 0 ) THEN
          LNOEU = .TRUE.
          DO 60 I = 1,NGN
           CALL JEVEUO(JEXNOM(GRPNOE,ZK8(JDEF+I-1)),'L',JDGN)
           CALL JELIRA(JEXNOM(GRPNOE,ZK8(JDEF+I-1)),'LONMAX',NBGRNO,K8B)
            DO 70 J = 1 , NBGRNO
              NUMNOE = ZI(JDGN+J-1)
              ZI(JDNO+NUMNOE-1)  = ZI(JDPM+NTYPOI-1)
              ZI(JNUT+NUMNOE-1) = 1
 70         CONTINUE
 60       CONTINUE
        ENDIF
C
C -     AFFECTATION DES NOEUDS
C
        IF ( NNO .NE. 0 ) THEN
          LNOEU = .TRUE.
          DO 80 I = 1 , NNO
            CALL JENONU(JEXNOM(NOMNOE,ZK8(JDEF+I-1)),NUMNOE)
            ZI(JDNO+NUMNOE-1) = ZI(JDPM+NTYPOI-1)
            ZI(JNUT+NUMNOE-1) = 1
 80       CONTINUE
        ENDIF
C
 100  CONTINUE
C
C --- VERIFICATION QUE LES MAILLES "UTILISATEUR" ONT ETE AFFECTEES
C
      NBMPCF = 0
      NBMPAF = 0
      DO 102 I = 1,NBMAIL
         IF ( ZI(JMUT+I-1) .EQ. 1 ) THEN
            IF ( ZI(JDMA+I-1) .EQ. 0 ) THEN
                NBMPAF = NBMPAF + 1
                CALL JENUNO(JEXNUM(NOMMAI,I),K8B)
                NUTYPM = ZI(JDTM+I-1)
                CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',NUTYPM),TYPEMA)
                IF ( NIV .EQ. 2 ) THEN
                   WRITE(IFM,*)'  MAILLE QUE L''ON N''A PAS PU AFFEC',
     +                            'TER: ',K8B,' DE TYPE: ',TYPEMA
                ENDIF
            ENDIF
         ELSE
            NBMPCF = NBMPCF + 1
         ENDIF
 102  CONTINUE
C
C --- VERIFICATION QUE LES NOEUDS "UTILISATEUR" ONT ETE AFFECTEES
C
      NBNPCF = 0
      NBNPAF = 0
      DO 104 I = 1 , NBNOEU
         IF ( ZI(JNUT+I-1) .EQ. 1 ) THEN
            IF ( ZI(JDNO+I-1) .EQ. 0 ) THEN
                NBNPAF = NBNPAF + 1
                CALL JENUNO(JEXNUM(NOMNOE,I),K8B)
                IF ( NIV .EQ. 2 ) THEN
                   WRITE(IFM,*)'  NOEUD QUE L''ON N''A PAS PU AFFEC',
     +                            'TER: ',K8B
                ENDIF
            ENDIF
         ELSE
            NBNPCF = NBNPCF + 1
         ENDIF
 104  CONTINUE
C
C ---   DIMENSIONNEMENT DES OBJETS LIEL ET NEMA
C
      NBMAAF = 0
      NBNOAF = 0
      NUTYPE = 0
      NBGREL = 0
C
      DO 110 I = 1,NBMAIL
         IF ( ZI(JDMA+I-1) .NE. 0 ) THEN
            NBMAAF = NBMAAF + 1
            IF ( ZI(JDMA+I-1) .NE. NUTYPE ) THEN
               NUTYPE = ZI(JDMA+I-1)
               NBGREL = NBGREL + 1
            ENDIF
         ENDIF
 110  CONTINUE
C
C
      IF ( LMAIL ) THEN
         II = NBMAAF + NBMPAF
         WRITE(IFM,1000) NBMAIL, NOMA, II, NBMAAF
      ENDIF
C
      IF ( NBMAAF .EQ. 0 ) THEN
         CALL UTMESS('F',CMD,'AUCUNE MAILLE N A ETE AFFECTEE '//
     +               'PAR DES ELEMENTS FINIS POUR LE MAILLAGE '//NOMA)
         CALL JEDETC(' ',CPTMAI,1)
         CALL JEDETC(' ',CPTNOE,1)
         CALL JEDETC(' ',TMPDEF,1)
         CALL JEDETR ( '&&OP0018.MAILLE' )
         CALL JEDETR ( '&&OP0018.NOEUD'  )
         CALL UTMESS('F',CMD,'ERREUR D AFFECTATION DETECTEE')
      ENDIF
C
      NUTYPE = 0
C
      DO 120 I = 1,NBNOEU
         IF ( ZI(JDNO+I-1) .NE. 0 ) THEN
            NBNOAF = NBNOAF + 1
            IF ( ZI(JDNO+I-1) .NE. NUTYPE ) THEN
               NUTYPE = ZI(JDNO+I-1)
               NBGREL = NBGREL + 1
            ENDIF
         ENDIF
 120  CONTINUE
C
C
      IF ( LNOEU  ) THEN
         II = NBNOAF + NBNPAF
         WRITE(IFM,1100) NBNOEU, NOMA, II, NBNOAF
      ENDIF
C
      LONLIE = NBGREL + NBMAAF + NBNOAF
      LONNEM = NBNOAF * 2
C
C ---   CREATION DES OBJETS DU CONCEPT MODELE
C
C
C -     OBJET LIEL
C
      CALL JECREC(CPTLIE,'G V I','NU','CONTIG','VARIABLE',NBGREL)
      CALL JEECRA(CPTLIE,'LONT',LONLIE,' ')
      CALL JEVEUO(CPTLIE,'E',JDLI)
C
C -     OBJET NEMA
C
      IF ( NBNOAF .NE. 0 ) THEN
         CALL JECREC(CPTNEM,'G V I','NU','CONTIG','VARIABLE',NBNOAF)
         CALL JEECRA(CPTNEM,'LONT',LONNEM,' ')
         CALL JEVEUO(CPTNEM,'E',JDNW)
      ENDIF
C
C -     OBJET NOMA
C
C
C -     OBJET NBNO
C
C
C ---   STOCKAGE DES GROUPES ELEMENTS DANS LIEL
C
      NUTYPE = 0
      NUGREL = 0
      NMGREL = 0
      NUMVEC = 0
C
      DO 130 NUMAIL = 1,NBMAIL
         IF ( ZI(JDMA+NUMAIL-1) .NE.0 ) THEN
            IF ( ZI(JDMA+NUMAIL-1).NE.NUTYPE .AND. NUTYPE.NE.0 ) THEN
               NUGREL = NUGREL + 1
               NMGREL = NMGREL + 1
               NUMVEC = NUMVEC + 1
               ZI(JDLI+NUMVEC-1) = NUTYPE
               CALL JECROC(JEXNUM(CPTLIE,NUGREL))
               CALL JEECRA(JEXNUM(CPTLIE,NUGREL),'LONMAX',NMGREL,' ')
               NMGREL = 0
            ENDIF
            NMGREL = NMGREL + 1
            NUMVEC = NUMVEC + 1
            ZI(JDLI+NUMVEC-1) = NUMAIL
            NUTYPE = ZI(JDMA+NUMAIL-1)
         ENDIF
         IF ( NUMAIL.EQ.NBMAIL .AND. NMGREL.NE.0 ) THEN
            NUGREL = NUGREL + 1
            NMGREL = NMGREL + 1
            NUMVEC = NUMVEC + 1
            ZI(JDLI+NUMVEC-1) = NUTYPE
            CALL JECROC(JEXNUM(CPTLIE,NUGREL))
            CALL JEECRA(JEXNUM(CPTLIE,NUGREL),'LONMAX',NMGREL,' ')
         ENDIF
 130  CONTINUE
C
      NUTYPE = 0
      NUMSUP = 0
      NMGREL = 0
C
      DO 140 NUMNOE = 1,NBNOEU
         IF ( ZI(JDNO+NUMNOE-1) .NE. 0 ) THEN
            IF( ZI(JDNO+NUMNOE-1).NE.NUTYPE .AND. NUTYPE.NE.0 ) THEN
               NUGREL = NUGREL + 1
               NMGREL = NMGREL + 1
               NUMVEC = NUMVEC + 1
               ZI(JDLI+NUMVEC-1) = NUTYPE
               CALL JECROC(JEXNUM(CPTLIE,NUGREL))
               CALL JEECRA(JEXNUM(CPTLIE,NUGREL),'LONMAX',NMGREL,' ')
               NMGREL = 0
            ENDIF
            NMGREL = NMGREL + 1
            NUMVEC = NUMVEC + 1
            NUMSUP = NUMSUP + 1
            ZI(JDLI+NUMVEC-1) = - NUMSUP
            NUTYPE = ZI(JDNO+NUMNOE-1)
         ENDIF
         IF ( NUMNOE.EQ.NBNOEU .AND. NMGREL.NE.0 ) THEN
            NUGREL = NUGREL + 1
            NMGREL = NMGREL + 1
            NUMVEC = NUMVEC + 1
            ZI(JDLI+NUMVEC-1) = NUTYPE
            CALL JECROC(JEXNUM(CPTLIE,NUGREL))
            CALL JEECRA(JEXNUM(CPTLIE,NUGREL),'LONMAX',NMGREL,' ')
         ENDIF
 140  CONTINUE
C
C ---   STOCKAGE DES NOUVELLES MAILLES DANS NEMA
C
      IF ( NBNOAF .NE. 0 ) THEN
         NUMVEC = 0
         NUMSUP = 0
         DO 150 NUMNOE = 1,NBNOEU
            IF ( ZI(JDNO+NUMNOE-1) .NE. 0 ) THEN
               ZI(JDNW+NUMVEC)   = NUMNOE
               ZI(JDNW+NUMVEC+1) = NTYPOI
               NUMVEC = NUMVEC + 2
               NUMSUP = NUMSUP + 1
               CALL JECROC(JEXNUM(CPTNEM,NUMSUP))
               CALL JEECRA(JEXNUM(CPTNEM,NUMSUP),'LONMAX',2,' ')
            ENDIF
 150     CONTINUE
      ENDIF
C
C ---   IMPRESSIONS DE VERIFICATION
C
      IF ( IMPVER .EQ. 1 ) THEN
         WRITE(IFM,200) NOMA
         NUMVEC = 1
         DO 160 I = 1,NUGREL
            CALL JELIRA(JEXNUM(CPTLIE,I),'LONMAX',NMGREL,K8B)
            NUMAIL = ZI(JDLI+NUMVEC-1)
            IF (NUMAIL.LT.0)THEN
               NUTYPM =  NTYPOI
               TYPENT = 'NOEUD   '
            ELSE
               NUTYPM = ZI(JDTM+NUMAIL-1)
               TYPENT = 'MAILLE  '
            ENDIF
            NUTYPE = ZI(JDLI+NUMVEC+NMGREL-2)
            CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',NUTYPM),TYPEMA)
            CALL JENUNO(JEXNUM('&CATA.TE.NOMTE',NUTYPE),TYPELE)
            WRITE(IFM,210) TYPELE, NMGREL-1, TYPEMA, TYPENT
            JC = 0
            DO 170 J = JDLI+NUMVEC-1,JDLI+NUMVEC+NMGREL-3
               JC = JC + 1
               NUMAIL = ZI(J)
               IF ( NUMAIL .LT. 0 ) THEN
                  NUMNOE = ZI(JDNW+(-NUMAIL*2-1)-1)
                  CALL JENUNO(JEXNUM(NOMNOE,NUMNOE),NOMAIL)
               ELSE
                  CALL JENUNO(JEXNUM(NOMMAI,NUMAIL),NOMAIL)
               ENDIF
               IF ( JC.EQ.8 .OR. J.EQ.JDLI+NUMVEC+NMGREL-3 ) THEN
                  TABMAI(JC) = NOMAIL
                  WRITE(IFM,220) (TABMAI(K),K=1,JC)
                  JC = 0
               ELSE
                  TABMAI(JC) = NOMAIL
               ENDIF
 170        CONTINUE
            NUMVEC = NUMVEC + NMGREL
 160     CONTINUE
      ENDIF
C
 200  FORMAT(/,'<MODELE> LISTE DES ELEMENTS AFFECTES A DES MAILLES OU ',
     +         'NOEUDS DU MAILLAGE ',A8,/,
     + '   TYPE ELEMENT FINI    NOMBRE   TYPE MAILLE   AFFECTE SUR')
 210  FORMAT(4X,A16,2X,I8,5X,A8,6X,A8)
 220  FORMAT(4X,A8,7(2X,A8))
C
C ---   SAUVEGARDES / LIBERATIONS / DESTRUCTIONS
C
      CALL JEDETR ( '&&OP0018.MAILLE' )
      CALL JEDETR ( '&&OP0018.NOEUD'  )
      CALL JEDETC (' ',TMPDEF,1)
C
 9997 CONTINUE
C
C     MODELE AVEC SOUS-STRUCTURES:
C --- PRISE EN COMPTE DES SOUS-STRUCTURES (MOT CLEF AFFE_SOUS_STRUC):
C
      CALL SSAFMO(NOMU)
C
C
C       ---   ADAPTATION DE LA TAILLE DES GRELS
C       ----------------------------------------
      CALL ADALIG(LIGREL)


C     --- VERIFICATION DE LA DIMENSION DES TYPE_ELEM DU MODELE
C     ----------------------------------------------------------
      CALL DISMOI('F','DIM_GEOM',NOMU,'MODELE'  ,IDIM ,K8B,IBID)
      IF (IDIM.GT.3) THEN
        CALL UTMESS('A','OP0018',
     &                  'MELANGE DE MODELISATIONS PLANES ET VOLUMIQUES '
     &                  //'(OU PRESENCE DE SOUS-TRUCTURES STATIQUES)')
      ELSE
        IDIM2=3
        CALL DISMOI('F','Z_CST',NOMU,'MODELE',IBID,CDIM,IBID)
        IF (CDIM.EQ.'OUI') IDIM2=2
        IF ((IDIM.EQ.3).AND.(IDIM2.EQ.2)) THEN
        ELSE IF ((IDIM.EQ.2).AND.(IDIM2.EQ.3)) THEN
          CALL UTMESS('A','OP0018','LE MAILLAGE EST 3D MAIS '
     &       //'LES ELEMENTS DU MODELE SONT 2D.')
        END IF
      END IF


C     --- VERIFICATION DU FAIT QUE POUR UN MAILLAGE 2D ON NE PEUT
C     ---- AVOIR A LA FOIS DES ELEMENTS DISCRETS 2D ET 3D :
C     ---------------------------------------------------
      CALL MODEXI(NOMU,'DIS_',I3D)
      CALL MODEXI(NOMU,'2D_DIS_',I2D)
      IF (IDIM2.EQ.2.AND.I3D.EQ.1.AND.I2D.EQ.1) THEN
         CALL UTMESS('F','OP0018','IL EST INTERDIT D''AVOIR '//
     +               ',POUR UN MODELE DONNE, A LA FOIS DES '//
     +               'ELEMENTS DISCRETS 2D ET 3D .')
      ENDIF


C     --- CREATION DE LA CORRESPONDANCE MAILLE --> (IGREL,IM)
C     -------------------------------------------------------
      CALL CORMGI('G', LIGREL)


C     --- CREATION DE L'OBJET .NOEUD_UTIL :
C     -------------------------------------------------------
      CALL DISMOI('F','NB_NO_MAILLA',NOMU,'MODELE'  ,NBNOEU ,K8B,IBID)
      CALL WKVECT(NOMU//'.NOEUD_UTIL','G V I',NBNOEU,JNOUT )
      CALL DISMOI('F','NB_MA_MAILLA',NOMU,'MODELE'  ,NBMAIL ,K8B,IBID)
      IF (NBMAIL.EQ.0) GO TO 559
      CALL JEVEUO(NOMA//'.CONNEX','L',ICONX1)
      CALL JEVEUO(JEXATR(NOMA//'.CONNEX','LONCUM'),'L',ICONX2)
      DO 557 IMA = 1 , NBMAIL
        IF (ZI(JDMA+IMA-1).EQ.0) GO TO 557
        NBNO = ZZNBNE(IMA)
        DO 558 J = 1,NBNO
           ZI(JNOUT-1+ZZCONX(IMA,J))=1
558     CONTINUE
557   CONTINUE
559   CONTINUE


C     ---   INITIALISATION DES ELEMENTS POUR CE LIGREL
C     -------------------------------------------------
      CALL INITEL(LIGREL)


C     ---   VERIFICATION DES X > 0 POUR L'AXIS
C     -------------------------------------------------
      IF ( LAXIS ) THEN
        CALL TAXIS(NOMA,ZI(JDMA),NBMAIL)
      ENDIF


 1000 FORMAT(/,' SUR LES ',I6,' MAILLES DU MAILLAGE ',A8,/,
     +         '    ON A DEMANDE L''AFFECTATION DE ',I6,/,
     +         '    ON A PU EN AFFECTER ',I6)
 1100 FORMAT(/,' SUR LES ',I6,' NOEUDS DU MAILLAGE ',A8,/,
     +         '    ON A DEMANDE L''AFFECTATION DE ',I6,/,
     +         '    ON A PU EN AFFECTER ',I6)
      CALL JEDEMA()
      END
