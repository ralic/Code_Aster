      SUBROUTINE CONINI(MA,NOECON,MAICON,MARCON,NBMAR,NBNOE,NBMARC,
     &        NOMMAR,JMICOR,MBCOR,NOMTYR,NBGCO,IO8GCO)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C  ROUTINE CONINI
C    ROUTINE DE PREPARATION DE TABLEAUX PERMETTANT D'OPTIMISER
C    LA ROUTINE CONORI
C  DECLARATIONS
C    NBGCO  : NOMBRE DE GROUPE DE CONTACT
C    IC     : NB D'OCCURENCE DE GROUP_MA
C    ICOC   : INDICE COURANT DES CONNEX     POUR UNE MAILLE CONTACT
C    ICOR   : INDICE COURANT DES CONNEX     POUR UNE MAILLE REFERENCE
C    IDUM   : ENTIER DE TRAVAIL
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
C    NBMAG  : NOMBRE DE MAILLE DANS UN GROUP_MA
C    NBMAR  : NOMBRE DE MAILLE              POUR UNE MAILLE REFERENCE
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
      INTEGER      IDUM,IC
      INTEGER      IO8GCO,       NBGCO,IGCO
      INTEGER             IMIGMA,IGMA
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
      LOGICAL      LOCONT,LOMODI,LOREOR,LOCORR,INVAL
      LOGICAL CAS2D,CAS3D,VALID
      CHARACTER*1 K1BID
      INTEGER NOECON(NBNOE),MAICON(NBMAR),MARCON(NBMAR)
      INTEGER MBCOR(NBMAR),JMICOR(NBMAR)
      CHARACTER*8 NOMMAR(NBMAR),NOMTYR(NBMAR)
C     TYPES VALIDES POUR LES MAILLES DE REFERENCE
      VALID()=(CAS2D.AND.
     &(KTYR(:4).EQ.'TRIA'.OR.KTYC(:4).EQ.'QUAD')).OR.
     &(CAS3D.AND.
     &(KTYR(:5).EQ.'PENTA'.OR.KTYR(:4).EQ.'HEXA'.OR.
     & KTYR(:5).EQ.'PYRAM'.OR.KTYR(:5).EQ.'TETRA'))
C
      INVAL=.FALSE.
      CAS2D=.FALSE.
      CAS3D=.FALSE.
CCC     ON COMMENTE JEMARQ CAR ADRESSES PASSEES EN ARGUMENT
CCC      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)
C
C     ==================================================================
C
      DO 1 INOE=1,NBNOE
        NOECON(INOE)=0
    1 CONTINUE
C
      DO 2 IMAI=1,NBMAR
        MAICON(IMAI)=0
    2 CONTINUE
C
      NBMARC=0
      IERR=0
   33 CONTINUE
C     ------------------------------------------------------------------
C     BOUCLE SUR LES GROUPE_MA_CONTACT
C     ------------------------------------------------------------------
         DO 3 IGCO=1,NBGCO
C     ------------------------------------------------------------------
C     RECHERCHE D EXISTENCE DU GROUP_MA_CONTACT CONSIDERE
C     ------------------------------------------------------------------
            CALL JENONU(JEXNOM(MA//'.GROUPEMA',ZK8(IO8GCO+IGCO-1)),IGMA)
C
            IF ( IGMA.EQ.0) THEN
C     ------------------------------------------------------------------
C     TRAITEMENT DU CAS DE NON-EXISTENCE
C     ------------------------------------------------------------------
               CALL U2MESK('I','ALGORITH2_26',1,ZK8(IO8GCO+IGCO-1))
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
     &                     'LONMAX',NBMAG,K1BID)
C     ------------------------------------------------------------------
C     BOUCLE SUR LES MAILLES DU GROUP_MA
C     ------------------------------------------------------------------
               DO 30 IMAG = 1,NBMAG
                  IMAC = ZI(IMIGMA+IMAG-1)
                  MAICON(IMAC)=MAICON(IMAC)+1
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
C
      IF      (KTYC(:5).EQ.'QUAD4'.OR.KTYC(:5).EQ.'QUAD8') THEN
        CAS2D=.TRUE.
        IF (IERR.NE.0) WRITE(IFM,*) 'MAILLE 2D : ',KMAC,' DE TYPE ',KTYC
      ELSE IF (KTYC(:5).EQ.'PENTA'.OR.KTYC(:4).EQ.'HEXA' ) THEN
        CAS3D=.TRUE.
        IF (IERR.NE.0) WRITE(IFM,*) 'MAILLE 3D : ',KMAC,' DE TYPE ',KTYC
      ELSE
        INVAL=.TRUE.
        CALL UTMESS('E','CONORI','MAILLE '//KMAC//' DE TYPE '//KTYC//
     &         ' INVALIDE POUR LE CONTACT')
C        CALL U2MESK('E','ALGORITH2_27', 2 ,VALK)
      ENDIF
C
C     ------------------------------------------------------------------
C     RECHERCHE DE L ADRESSE DES CONNEXIONS DE LA MAILLE
C     ------------------------------------------------------------------
                  CALL JEVEUO(JEXNUM(MA//'.CONNEX',IMAC),'E',IMICOC)
C     ------------------------------------------------------------------
C     RECHERCHE DU NOMBRE DE CONNEXIONS DE LA MAILLE
C     ------------------------------------------------------------------
                  CALL JELIRA(JEXNUM(MA//'.CONNEX',IMAC),
     &                        'LONMAX',NBCOC,K1BID)
C
C     ------------------------------------------------------------------
C     BOUCLE SUR LES CONNEXIONS DE LA MAILLE
C     ------------------------------------------------------------------
                  DO 300 ICOC = 1,NBCOC
                     INOC = ZI(IMICOC+ICOC-1)
                     NOECON(INOC)=NOECON(INOC)+1
300            CONTINUE
30             CONTINUE
C     ------------------------------------------------------------------
            ENDIF
C     ------------------------------------------------------------------
3        CONTINUE
      IF (INVAL)
     &CALL U2MESS('F','ALGORITH2_28')
C
      IF (CAS2D.AND.CAS3D) THEN
        IF (IERR.EQ.0) THEN
C       ON RETOURNE DANS LA BOUCLE AVEC DEMANDE DE MESSAGES
          IERR=1
          GOTO 33
        ELSE
          CALL U2MESS('F','ALGORITH2_29')
        ENDIF
      ENDIF
      IF (CAS2D) ITEST=2
      IF (CAS3D) ITEST=3
C
C     ------------------------------------------------------------------
C     BOUCLE SUR LES MAILLES DU MAILLAGE
C     ------------------------------------------------------------------
                  DO 4 IMAR=1,NBMAR
                     IF (MAICON(IMAR).NE.0) GOTO 4
C     ------------------------------------------------------------------
C     RECHERCHE DU NOM DE LA MAILLE
C     ------------------------------------------------------------------
                     CALL JENUNO(JEXNUM(MA//'.NOMMAI' ,IMAR),KMAR)
                     NOMMAR(IMAR)=KMAR
C
C     ------------------------------------------------------------------
C     RECHERCHE DE L ADRESSE DES CONNEXIONS DE LA MAILLE
C     ------------------------------------------------------------------
                     CALL JEVEUO(JEXNUM(MA//'.CONNEX',IMAR),'L',IMICOR)
                     JMICOR(IMAR)=IMICOR
C     ------------------------------------------------------------------
C     RECHERCHE DU NOMBRE DE CONNEXIONS DE LA MAILLE
C     ------------------------------------------------------------------
                     CALL JELIRA(JEXNUM(MA//'.CONNEX',IMAR),
     &                           'LONMAX',NBCOR,K1BID)
                     MBCOR(IMAR)=NBCOR
C     ------------------------------------------------------------------
C     BOUCLE SUR LES CONNEXIONS DE LA MAILLE
C     ------------------------------------------------------------------
       NBCOM=0
                     DO 40 ICOR = 1,NBCOR
                        INOR = ZI(IMICOR+ICOR-1)
                        IF (NOECON(INOR).NE.0) NBCOM=NBCOM+1
C
  40  CONTINUE
       IF (NBCOM.GE.ITEST) THEN
C     ------------------------------------------------------------------
C     RECHERCHE DE L'ADRESSE DU TYPE DE LA MAILLE DANS ZI
C     ------------------------------------------------------------------
                     CALL JEVEUO(MA//'.TYPMAIL','L',IATYMA)
                     IMITYR=IATYMA-1+IMAR
                     ITYR = ZI(IMITYR)
C     ------------------------------------------------------------------
C     RECHERCHE DU TYPE DE LA MAILLE DANS CATA.TM.NOMTM
C     ------------------------------------------------------------------
                     CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ITYR),KTYR)
                     NOMTYR(IMAR)=KTYR
        IF (VALID()) THEN
        NBMARC=NBMARC+1
        MARCON(NBMARC)=IMAR
       ENDIF
       ENDIF
  4   CONTINUE
C     ==================================================================
CCC      ON COMMENTE JEMARQ CAR ADRESSES PASSEES EN ARGUMENT
CCC      CALL JEDEMA()
      END
