      SUBROUTINE CONORI(MA)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 08/03/2004   AUTEUR REZETTE C.REZETTE 
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
C  ROUTINE CONORI
C    ROUTINE RACINE D ORIENTATION DES MAILLES DE CONTACT
C  DECLARATIONS
C    NBGCO  : NOMBRE DE GROUPE DE CONTACT
C    IC     : NB D'OCCURENCE DE GROUP_MA
C    ICOC   : INDICE COURANT DES CONNEX     POUR UNE MAILLE CONTACT
C    ICOR   : INDICE COURANT DES CONNEX     POUR UNE MAILLE REFERENCE
C    IDUM   : ENTIER DE TRAVAIL
C    IFM    : IUNIT DU FICHIER MESSAGE
C    IGCO   : INDICE COURANT SUR LES GROUPE DE CONTACT
C    IGMA   : INDICE COURANT SUR LES GROUP_MA
C    IMAC   : INDICE COURANT DES MAILLES    POUR UNE MAILLE CONTACT
C    IMAG   : INDICE COURANT SUR LES MAILLES D UN GROUPE
C    IMAR   : INDICE COURANT DES MAILLES    POUR UNE MAILLE REFERENCE
C    IMICOC : INDICE DE  CONNEX    DANS ZK8 POUR UNE MAILLE CONTACT
C    IMICOR : INDICE DE  CONNEX    DANS ZK8 POUR UNE MAILLE REFERENCE
C    IMIGMA : INDICE DE  GROUPEMA  DANS ZI
C    IMITYC : INDICE DE  TYPMAIL   DANS ZI  POUR UNE MAILLE CONTACT
C    IMITYR : INDICE DE  TYPMAIL   DANS ZI  POUR UNE MAILLE REFERENCE
C    INOC   : NUMERO D UN NOEUD             POUR UNE MAILLE CONTACT
C    INOR   : NUMERO D UN NOEUD             POUR UNE MAILLE REFERENCE
C    IO8GCO : INDICE DE OP0154 NOGCO DANS ZK8
C    ITYC   : INDICE COURANT DU TYPE        POUR UNE MAILLE CONTACT
C    ITYR   : INDICE COURANT DU TYPE        POUR UNE MAILLE REFERENCE
C    JEXNOM : FUNCTION D ASTER
C    JEXNUM : FUNCTION D ASTER
C    KBID   : CHARACTER DE TRAVAIL
C    KMAC   : NOM D UNE MAILLE              POUR UNE MAILLE CONTACT
C    KMAR   : NOM D UNE MAILLE              POUR UNE MAILLE REFERENCE
C    KNOC   : NOM D UN NOEUD                POUR UNE MAILLE CONTACT
C    KNOR   : NOM D UN NOEUD                POUR UNE MAILLE REFERENCE
C    KTYC   : NOM DU TYPE                   POUR UNE MAILLE CONTACT
C    KTYR   : NOM DU TYPE                   POUR UNE MAILLE REFERENCE
C    LOCONT : LOGICAL PRECISANT SI LA MAILLE EST UNE MAILLE CONTACT
C    LOMODI : LOGICAL PRECISANT SI LA MAILLE EST UNE MAILLE MODIFIE
C    LOREOR : LOGICAL PRECISANT SI LA MAILLE EST UNE MAILLE REORIENTEE
C    MA     : L OBJET DU MAILLAGE
C    MACOC  : TABLEAU DES NOMS DES NOEUDS   POUR UNE MAILLE CONTACT
C    MACOR  : TABLEAU DES NOMS DES NOEUDS   POUR UNE MAILLE REFERENCE
C    NBCOC  : NOMBRE DE CONNEX              POUR UNE MAILLE CONTACT
C    NBCOR  : NOMBRE DE CONNEX              POUR UNE MAILLE REFERENCE
C    NBGMA  : NOMBRE DE GROUP_MA
C    NBMAG  : NOMBRE DE MAILLE DANS UN GROUP_MA
C    NBMAR  : NOMBRE DE MAILLE              POUR UNE MAILLE REFERENCE
C    NBNOMX : NOMBRE DE NOEUD MAXIMUM POUR UNE MAILLE ( 100 )
C    NIV    : NIVEAU D'IMPRESSION (OPTION INFO)
C
C  MOT_CLEF : ORIE_CONTACT
C
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX --------------------
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX --------------------
C
      INTEGER      IDUM,IC,IFM,NIV
      INTEGER      IO8GCO,       NBGCO,IGCO
      INTEGER             IMIGMA,NBGMA,IGMA
      INTEGER                    NBMAG,IMAG
      INTEGER                          IMAC
      INTEGER             IMITYC,      ITYC
      INTEGER             IMICOC,NBCOC,ICOC
      INTEGER                          INOC
      INTEGER                    NBMAR,IMAR
      INTEGER             IMITYR,      ITYR
      INTEGER             IMICOR,NBCOR,ICOR
      INTEGER             IATYMA
C
      CHARACTER*8  KMAC,KTYC,KNOC,KMAR,KTYR,KNOR
      CHARACTER*8  MA,KBID
      CHARACTER*32 JEXNOM,JEXNUM
C
      LOGICAL     LOCONT,LOMODI,LOREO0,LOREOR,LOMOD0,LOCOR0,LFACE,LFACE0
      CHARACTER*1 K1BID
C
      PARAMETER ( NBNOMX = 100 )
      CHARACTER*8 MACOR(NBNOMX+2),MACOC(NBNOMX+2),MACOS(NBNOMX+2)
C
      CALL JEMARQ()
C
C     ==================================================================
C     ------------------------------------------------------------------
C     FORMAT ET UNIT D ECRITURE ET NIVEAU D'IMPRESSION
C     ------------------------------------------------------------------
1000  FORMAT(6X,6(2X,A8),(/,26X,4(2X,A8)))
      CALL INFNIV(IFM,NIV)
C     ------------------------------------------------------------------
C     RECHERCHE DU NOMBRE DE GROUP_MA_CONTACT DANS .COMM
C     ------------------------------------------------------------------
      IC = 1
      CALL GETVEM(MA,'GROUP_MA','ORIE_CONTACT','GROUP_MA',
     +               IC,1,0,KBID,NBGCO)
      NBGCO = -NBGCO
C
C     ==================================================================
      IF (NBGCO.NE.0) THEN
C     ------------------------------------------------------------------
C     RECHERCHE DU NOMBRE DE GROUP_MA DANS .MAIL
C     ------------------------------------------------------------------
         CALL JELIRA(MA//'.GROUPEMA','NUTIOC',NBGMA,KBID)
         IF (NIV.EQ.2) THEN
         WRITE(IFM,*) ' '
         WRITE(IFM,*) ' LA LISTE DES GROUP_MA '
         WRITE(IFM,*) ' '
         ENDIF
C     ------------------------------------------------------------------
C     RECHERCHE DES NOMS DES GROUP_MA DANS .MAIL
C     ------------------------------------------------------------------
         DO 1 IGMA = 1,NBGMA
            CALL JENUNO(JEXNUM(MA//'.GROUPEMA',IGMA),KBID)
            IF (NIV.EQ.2) THEN
            WRITE(IFM,*) '   GROUP_MA     : ',KBID
            ENDIF
1        CONTINUE
         WRITE(IFM,*) ' '
C     ------------------------------------------------------------------
C     CREATION D UN TABLEAU DE TRAVAIL
C     ------------------------------------------------------------------
         CALL WKVECT('&&OP0154.NOGCO','V V K8',NBGCO,IO8GCO)
C     ------------------------------------------------------------------
C     RECHERCHE DES NOMS DES GROUP_MA_CONTACT DANS .COMM
C     ------------------------------------------------------------------
         CALL GETVEM(MA,'GROUP_MA','ORIE_CONTACT','GROUP_MA',
     +                  IC,1,NBGCO,ZK8(IO8GCO),IDUM)
         IF (NIV.EQ.2) THEN
         WRITE(IFM,*) ' '
         WRITE(IFM,*) ' LA LISTE DES ORIE_CONTACT'
         WRITE(IFM,*) ' '
         DO 2 IGCO=1,NBGCO
            WRITE(IFM,*) '   ORIE_CONTACT: ',ZK8(IO8GCO+IGCO-1)
2        CONTINUE
         WRITE(IFM,*) ' '
         ENDIF
C     ------------------------------------------------------------------
         CALL JELIRA(MA//'.NOMMAI','NOMUTI',NBMAR,KBID)
         CALL JELIRA(MA//'.NOMNOE','NOMUTI',NBNOE,KBID)
C
      CALL WKVECT('&&OP0154.NOE','V V I',NBNOE,INOE)
      CALL WKVECT('&&OP0154.MAI','V V I',NBMAR,IMAI)
      CALL WKVECT('&&OP0154.MAR','V V I',NBMAR,IMAZ)
      CALL WKVECT('&&OP0154.KMR','V V K8',NBMAR,IKMAR)
      CALL WKVECT('&&OP0154.KTR','V V K8',NBMAR,IKTYR)
      CALL WKVECT('&&OP0154.IMI','V V I',NBMAR,JMIC)
      CALL WKVECT('&&OP0154.MBL','V V I',NBMAR,JMB)
      CALL CONINI(MA,ZI(INOE),ZI(IMAI),ZI(IMAZ),NBMAR,NBNOE,NBMARC,
     .            ZK8(IKMAR),ZI(JMIC),ZI(JMB),ZK8(IKTYR),
     .            NBGCO,IO8GCO)
      WRITE(IFM,*) 'NOMBRE DE MAILLES DE REFERENCE TESTEES : ', NBMARC
C
C     ==================================================================
C     ------------------------------------------------------------------
C     BOUCLE SUR LES GROUPE_MA_CONTACT
C     ------------------------------------------------------------------
         DO 3 IGCO=1,NBGCO
C     ------------------------------------------------------------------
C     RECHERCHE D EXISTENCE DU GROUP_MA_CONTACT CONSIDERE
C     ------------------------------------------------------------------
            CALL JENONU(JEXNOM(MA//'.GROUPEMA',ZK8(IO8GCO+IGCO-1)),IGMA)
C
            IF (NIV.EQ.2) THEN
            WRITE(IFM,*) ' '
            WRITE(IFM,*) ' TRAITEMENT DE ',
     +                 ZK8(IO8GCO+IGCO-1)
            WRITE(IFM,*) ' '
            ENDIF
            IF ( IGMA.EQ.0) THEN
C     ------------------------------------------------------------------
C     TRAITEMENT DU CAS DE NON-EXISTENCE
C     ------------------------------------------------------------------
               CALL UTMESS('I','CONORI',ZK8(IO8GCO+IGCO-1)//
     +         ' GROUPE INEXISTANT')
C
            ELSE
C     ------------------------------------------------------------------
C     TRAITEMENT DU CAS D EXISTENCE
C     ------------------------------------------------------------------
C
C     ------------------------------------------------------------------
C     RECHERCHE DE L'ADRESSE DU GROUP_MA DANS ZI
C     ------------------------------------------------------------------
               CALL JEVEUO(JEXNUM(MA//'.GROUPEMA',IGMA),'L',IMIGMA)
C     ------------------------------------------------------------------
C     RECHERCHE DU NOMBRE DE MAILLE DU GROUP_MA
C     ------------------------------------------------------------------
               CALL JELIRA(JEXNUM(MA//'.GROUPEMA',IGMA),
     +                     'LONMAX',NBMAG,K1BID)
               IF (NIV.EQ.2) THEN
               WRITE(IFM,*) '   LA LISTE DES MAILLES DU GROUPE '
               WRITE(IFM,*) ' '
               ENDIF
C
C     ------------------------------------------------------------------
C     BOUCLE SUR LES MAILLES DU GROUP_MA
C     ------------------------------------------------------------------
               DO 30 IMAG = 1,NBMAG
                  IMAC = ZI(IMIGMA+IMAG-1)
C     ------------------------------------------------------------------
C     RECHERCHE DU NOM DE LA MAILLE
C     ------------------------------------------------------------------
                  CALL JENUNO(JEXNUM(MA//'.NOMMAI' ,IMAC),KMAC)
C     ------------------------------------------------------------------
C     RECHERCHE DE L'ADRESSE DU TYPE DE LA MAILLE DANS ZI
C     ------------------------------------------------------------------
                  CALL JEVEUO(MA//'.TYPMAIL','L',IATYMA)
                  IMITYC=IATYMA-1+IMAC
                  ITYC = ZI(IMITYC)
C     ------------------------------------------------------------------
C     RECHERCHE DU TYPE DE LA MAILLE DANS CATA.TM.NOMTM
C     ------------------------------------------------------------------
                  CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ITYC),KTYC)
                  IF (NIV.EQ.2) THEN
                  WRITE(IFM,*) '     MAILLE NU : ',IMAG,
     +                                 ' NOM : ',KMAC,
     +                               ' ORDRE : ',IMAC,
     +                                ' TYPE : ',ITYC,
     +                                ' TYPE : ',KTYC
                  ENDIF
                  MACOC(1) = KMAC
                  MACOC(2) = KTYC
C
C     ------------------------------------------------------------------
C     RECHERCHE DE L ADRESSE DES CONNEXIONS DE LA MAILLE
C     ------------------------------------------------------------------
                  CALL JEVEUO(JEXNUM(MA//'.CONNEX',IMAC),'E',IMICOC)
C     ------------------------------------------------------------------
C     RECHERCHE DU NOMBRE DE CONNEXIONS DE LA MAILLE
C     ------------------------------------------------------------------
                  CALL JELIRA(JEXNUM(MA//'.CONNEX',IMAC),
     +                        'LONMAX',NBCOC,K1BID)
C
C     ------------------------------------------------------------------
C     BOUCLE SUR LES CONNEXIONS DE LA MAILLE
C     ------------------------------------------------------------------
                  DO 300 ICOC = 1,NBCOC
                     INOC = ZI(IMICOC+ICOC-1)
C     ------------------------------------------------------------------
C     RECHERCHE DU NOM DU NOEUD
C     ------------------------------------------------------------------
                     CALL JENUNO(JEXNUM(MA//'.NOMNOE',INOC),KNOC)
                     MACOC(ICOC+2)=KNOC
300               CONTINUE
C
C     ------------------------------------------------------------------
C     SAUVEGARDE DE LA MAILLE DE CONTACT
C     ------------------------------------------------------------------
                  DO 301 IDUM=1,NBCOC+2
                     MACOS(IDUM)=MACOC(IDUM)
301               CONTINUE
C     ==================================================================
C     ------------------------------------------------------------------
C     BOUCLE SUR LES MAILLES DU MAILLAGE
C     ------------------------------------------------------------------
                  LOMODI = .FALSE.
                  LOREOR = .FALSE.
                  NBMAC=0
                  DO 302 IMARC=1,NBMARC
                  IMAR=ZI(IMAZ-1+IMARC)
C     ------------------------------------------------------------------
C     RECHERCHE DU NOM DE LA MAILLE
C     ------------------------------------------------------------------
                     KMAR=ZK8(IKMAR-1+IMAR)
C     ------------------------------------------------------------------
C     RECHERCHE DU TYPE DE LA MAILLE
C     ------------------------------------------------------------------
                     KTYR=ZK8(IKTYR-1+IMAR)
C
                     MACOR(1) = KMAR
                     MACOR(2) = KTYR
C
C     ------------------------------------------------------------------
C     RECHERCHE DE L ADRESSE DES CONNEXIONS DE LA MAILLE
C     ------------------------------------------------------------------
                     IMICOR=ZI(JMIC-1+IMAR)
C     ------------------------------------------------------------------
C     RECHERCHE DU NOMBRE DE CONNEXIONS DE LA MAILLE
C     ------------------------------------------------------------------
                     NBCOR=ZI(JMB-1+IMAR)
C     ------------------------------------------------------------------
C     BOUCLE SUR LES CONNEXIONS DE LA MAILLE
C     ------------------------------------------------------------------
                     DO 3020 ICOR = 1,NBCOR
                        INOR = ZI(IMICOR+ICOR-1)
C     ------------------------------------------------------------------
C     RECHERCHE DU NOM DU NOEUD
C     ------------------------------------------------------------------
                        CALL JENUNO(JEXNUM(MA//'.NOMNOE',INOR),KNOR)
                        MACOR(ICOR+2) = KNOR
3020                 CONTINUE
C     ==================================================================
C     ------------------------------------------------------------------
C     APPEL DE CONTAC
C                      ORIENTATION DE LA MAILLE CONTACT SI NECESSAIRE
C                      LOMODI = .TRUE.  SI MODIFICATION
C                      LOMODI = .FALSE. SINON
C     ------------------------------------------------------------------
       LOMOD0=.FALSE.
       LOCOR0=.FALSE.
                        CALL CONTAC(MACOR,NBCOR,
     +                              MACOC,NBCOC,
     +                              LFACE0,LOMOD0,LOCOR0,LOREO0,MA)
      IF (LOREO0) LFACE0=.NOT.LFACE0
      IF (LOCOR0.OR.LOMOD0) THEN
        NBMAC=NBMAC+1
        IF (NIV.EQ.2) THEN
          WRITE(IFM,*) 'LA MAILLE DE CONTACT   ',
     .                 MACOC(1),' DE TYPE ',MACOC(2)
          WRITE(IFM,*) (MACOC(I+2),I=1,NBCOC)
          WRITE(IFM,*) 'S''APPUIE SUR LA MAILLE ',
     .                 MACOR(1),' DE TYPE ',MACOR(2)
          WRITE(IFM,*) (MACOR(I+2),I=1,NBCOR)
          IF (LFACE0) THEN
            WRITE(IFM,*) 'PAR SA FACE INFERIEURE'
          ELSE
            WRITE(IFM,*) 'PAR SA FACE SUPERIEURE'
          ENDIF
          IF (LOMOD0) THEN
            WRITE(IFM,*) 'UNE REORIENTATION POUR L''APPUI A EU LIEU'
          ENDIF
          IF (LOREO0) THEN
            WRITE(IFM,*) 'UNE REORIENTATION POUR LA NORMALE A EU LIEU'
          ENDIF
          WRITE(IFM,*)
        ENDIF
        IF (NBMAC.EQ.3)
     .      CALL UTMESS('F',' ','TROIS ELEMENTS')
        IF (NBMAC.EQ.2.AND.(LFACE0.EQV.LFACE))
     .      CALL UTMESS('F',' ','DEUX ELEMENTS SUR LA MEME FACE')
        LFACE=LFACE0
        IF (LOMOD0) LOMODI=.TRUE.
        IF (LOREO0) LOREOR=.TRUE.
        IF ((LOMOD0.OR.LOREO0).AND.NBMAC.EQ.2) CALL UTMESS('F',' ',
     .             'UNE REORIENTATION A EU LIEU POUR LE DEUXIEME APPUI')
      ENDIF
      
C     ==================================================================
302               CONTINUE
      IF (NBMAC.EQ.0) CALL UTMESS('E',' ',
     .               'PAS DE MAILLE DE REFERENCE TROUVEE')
C
                  IF (LOMODI.OR.LOREOR) THEN
C     ------------------------------------------------------------------
C     ECRITURE DES MAILLES MODIFIEES
C     ------------------------------------------------------------------
                     IF (NIV.EQ.2) THEN
                     WRITE(IFM,*) ' '
                     WRITE(IFM,*) '       MODIFICATION DE LA MAILLE'
                     WRITE(IFM,*) ' '
                     WRITE(IFM,*) '       AVANT'
                     WRITE(IFM,1000) (MACOS(IDUM),IDUM=1,NBCOC+2)
                     WRITE(IFM,*) '       APRES'
                     WRITE(IFM,1000) (MACOC(IDUM),IDUM=1,NBCOC+2)
                     WRITE(IFM,*) ' '
                     ENDIF
C
                     DO 304 ICOC = 1,NBCOC
                        KNOC = MACOC(ICOC+2)
C     ------------------------------------------------------------------
C     RECHERCHE DE L'ORDRE DU NOEUD
C     ------------------------------------------------------------------
                        CALL JENONU(JEXNOM(MA//'.NOMNOE',KNOC),INOC)
C     ------------------------------------------------------------------
C     MODIFICATION DE L ORIENTATION DE LA MAILLE
C     ------------------------------------------------------------------
                        ZI(IMICOC+ICOC-1) = INOC
304                  CONTINUE
                  ENDIF
C     ==================================================================
C
30             CONTINUE
C     ------------------------------------------------------------------
            ENDIF
C     ------------------------------------------------------------------
3        CONTINUE
C
      ENDIF
C     ------------------------------------------------------------------
C     ==================================================================
 9999 CONTINUE
C     EMISSION D'UNE ERREUR <F> SI UNE ERREUR <E> S'EST PRODUITE
      CALL FSIE()
      CALL JEDEMA()
      END
