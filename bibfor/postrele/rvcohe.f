      SUBROUTINE RVCOHE ( XDICMP, XDNCMP, VCHEFF, I, IER )
      IMPLICIT NONE
      CHARACTER*24        XDICMP, XDNCMP, VCHEFF
      INTEGER                                     I, IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 08/06/2004   AUTEUR CIBHHLV L.VIVAN 
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
C     ------------------------------------------------------------------
C     VERIFICATION DE COHERENCE DES ARGUMENTS D' APPEL DE LA COMMANDE
C     DE CALCUL DU POST TRAITEMENT (OP0051)
C       1. EXISTENCE DES CHAM_GD MIS EN JEU
C       2. LEGALITE DES COMPOSANTES MISES EN JEU POUR CES CHAM_GD
C       3. CONCORDANCE ENTRE LES MAILLAGES DES CHAMPS ET DES COURBES
C          OU DES NOEUDS
C     ------------------------------------------------------------------
C IN  XDNCMP : K : NOM DE OJB XD V K8 DES NOMS DE CMP MISES EN JEU
C IN  XDICMP : K : NOM DE OJB XD V I  DES NUMS DE CMP (0 <=> ILLEGALE)
C IN  VCHEFF : K : NOM DU OJB S  V K24 DES NOMS DE CHAMPS EFFECTIF
C IN  I      : I : NUMERO DE L' OCCURENCE A TRAITER
C OUT IER    : I : CODE RETOUR, 1 --> OK, 0 --> KO
C     ------------------------------------------------------------------
C
      INTEGER         ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8          ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16      ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL         ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8     ZK8
      CHARACTER*16    ZK16
      CHARACTER*24    ZK24
      CHARACTER*32    ZK32
      CHARACTER*80    ZK80
      COMMON /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32 JEXNOM,JEXNUM
C
      CHARACTER*24 NCHEFF,NDESC
      CHARACTER*19 NCHP19
      CHARACTER*16 NCHSYM,TRESU
      CHARACTER*15 NREPND
      CHARACTER*8  NRESU,NOMCMP,NMAICH,NMAILI,NOMCRB,NOMGRN,NOMND
      CHARACTER*4  DOCU
      INTEGER      ACHEFF,AMAICB,AGRPND,ALNEUD,ANUMCP,ANOMCP,NBCMP
      INTEGER      NBGRPN,NBNEUD,NBCRB,GREL,NBGREL,JCELD,AMOD,MOD
      INTEGER      J,K,N1,IERD,IBID
      LOGICAL      CHELOK
      CHARACTER*1 K1BID
C
C=====================================================================
C
      CALL JEMARQ()
      IER = 1
      CALL JEVEUO(VCHEFF,'L',ACHEFF)
      NCHEFF = ZK24(ACHEFF + I-1)
C
      IF ( NCHEFF(1:1) .EQ.'&' ) THEN
C
C        CAS D'UN CHAMP SYMBOLIQUE ILLEGAL OU DE NON EXISTENCE
C                  D' UN CHAMP EFFECTIF ASSOCIE
C
         IER = 0
         CALL GETVID ('ACTION','RESULTAT',I,1,1,NRESU ,N1)
         CALL GETVTX ('ACTION','NOM_CHAM',I,1,1,NCHSYM,N1)
         CALL GETTCO(NRESU,TRESU)
C
         CALL UTDEBM('S','RVCOHE','ERREUR DANS LES DONNEES')
         CALL UTIMPI('L','POST-TRAITEMENT NUMERO ',1,I)
         CALL UTIMPK('L','   LE CHAMPS SYMBOLIQUE: ',1,NCHSYM)
         CALL UTIMPK('L','   N''EST PAS AUTORISE POUR LE RESULTAT: ',
     +                                                        1,NRESU)
         CALL UTIMPK('L','   LE TYPE DE CE RESULTAT EST: ',1,TRESU)
         CALL UTIMPK('L','   OU LE CHAMP SYMBOLIQUE EST AUTORISE '//
     +                   'MAIS AUCUN CHAMP EFFECTIF N''EXISTE',0,K1BID)
         CALL UTFINM
C
      ELSE
C
C        LE CHAMP SYMBOLIQUE EXISTE ET UN CHAMP EFFECTIF LUI
C        CORRESPOND OU UN CHAMP EFFECTIF EST DIRECTEMENT ARGUMENT
C        VERIFICATION POUR LES CHAM_ELEM DU CARACTERE "AUX NOEUDS"
C
         NCHP19 = NCHEFF(1:19)
         CALL JEEXIN(NCHP19//'.DESC',IBID)
         IF (IBID.GT.0) THEN
           CALL JELIRA(NCHP19//'.DESC','DOCU',N1,DOCU)
         ELSE
           CALL JELIRA(NCHP19//'.CELD','DOCU',N1,DOCU)
         END IF

         IF ( DOCU .EQ. 'CHML' ) THEN
            NDESC  = NCHP19//'.CELD'
            CALL JEVEUO(NDESC,'L',JCELD)
            NBGREL = ZI(JCELD + 2-1)
            CHELOK = .TRUE.
            GREL   = 0
10          CONTINUE
            IF ( (CHELOK) .AND. (GREL .LT. NBGREL) ) THEN
               GREL = GREL + 1
               MOD=ZI(JCELD-1+ZI(JCELD-1+4+GREL) +2)
               IF ( MOD .NE. 0 ) THEN
                  CALL JEVEUO(JEXNUM('&CATA.TE.MODELOC',MOD),
     +                        'L',AMOD)
                  CHELOK = (ZI(AMOD-1+1).EQ. 2)
               ENDIF
               GOTO 10
            ENDIF
            IF ( .NOT. CHELOK ) THEN
               CALL UTDEBM('S','RVCOHE','ERREUR DANS LES DONNEES')
               CALL UTIMPI('L','POST-TRAITEMENT NUMERO ',1,I)
               CALL UTIMPK('L','   LE OU LES CHAMP(S) ELEMENTAIRE(S)'//
     +                         ' MIS EN JEU EST OU SONT DONNES AUX '//
     +                         'POINTS DE GAUSS',0,K1BID)
               CALL UTIMPK('L','   C''EST INTERDIT',0,K1BID)
               CALL UTFINM
            ENDIF
         ENDIF
C
C        --- VERIFICATION SUR LES CMPS ---
         CALL JELIRA(JEXNUM(XDICMP,I),'LONMAX',NBCMP,K1BID)
         CALL JEVEUO(JEXNUM(XDICMP,I),'L',ANUMCP)
         DO 110, J = 1, NBCMP, 1
            IF ( ZI(ANUMCP + J-1) .EQ. 0 ) THEN
               CALL JEVEUO(JEXNUM(XDNCMP,I),'L',ANOMCP)
               NOMCMP = ZK8(ANOMCP + J-1)
               IF ( IER .EQ. 1 ) THEN
                  CALL UTDEBM('S','RVCOHE','ERREUR DANS LES DONNEES')
                  CALL UTIMPI('L','POST-TRAITEMENT NUMERO ',1,I)
               ENDIF
               CALL UTIMPK('L','   LA COMPOSANTE DE NOM: ',1,NOMCMP)
               CALL UTIMPK('L','   N''EST PAS PRESENTE AU CATALO'//
     +                                     'GUE DES GRANDEURS',0,K1BID)
               IER = 0
            ENDIF
 110     CONTINUE
         IF ( IER .EQ. 0 ) CALL UTFINM
C
C        --- VERIFICATION DE CONCORDANCE DES MAILLAGES ---
         CALL DISMOI('F','NOM_MAILLA',NCHEFF,'CHAMP',N1,NMAICH,IERD)
         CALL GETVID('ACTION','CHEMIN',I,1,0,ZK8,NBCRB)
         NBCRB = -NBCRB
         IF ( NBCRB .NE. 0 ) THEN
C           /* LE LIEU DU POST TRAITEMENT EST UNE COURBE */
            CALL GETVID('ACTION','CHEMIN',I,1,1,NOMCRB,N1)
            CALL JEEXIN(NOMCRB//'.NOMMAIL',N1)
            IF ( N1 .NE. 0 ) THEN
               CALL JEVEUO(NOMCRB//'.NOMMAIL','L',AMAICB)
            ELSE
               CALL JEVEUO(NOMCRB//'.NOMA','L',AMAICB)
            ENDIF
            NMAILI = ZK8(AMAICB)
            IF ( NMAICH .NE. NMAILI ) THEN
               IER = 0
               CALL UTDEBM('S','RVCOHE','ERREUR DANS LES DONNEES')
               CALL UTIMPI('L','POST-TRAITEMENT NUMERO ',1,I)
               CALL UTIMPK('L','   LE MAILLAGE DE LA COURBE: ',1,NMAILI)
               CALL UTIMPK('S','EST DIFFERENT DU MAILLAGE DU '//
     +                                     'CHAMP A TRAITE: ',1,NMAICH)
               CALL UTFINM
            ENDIF
         ELSE
C           /* LE LIEU DU POST TRAITEMENT EST UN ENSMBLE DE NOEUDS */
C           VERIFICATION D' EXISTENCE DES NOEUDS DANS LE MAILLAGE DU CHP
            CALL GETVID('ACTION','GROUP_NO',I,1,0,ZK8,NBGRPN)
            CALL GETVID('ACTION','NOEUD'   ,I,1,0,ZK8,NBNEUD)
            NBGRPN = -NBGRPN
            NBNEUD = -NBNEUD
            IF ( NBGRPN .NE. 0 ) THEN
               CALL JECREO('&&OP0051.NOM.GRPN','V V K8')
               CALL JEECRA('&&OP0051.NOM.GRPN','LONMAX',NBGRPN,' ')
               CALL JEVEUO('&&OP0051.NOM.GRPN','E',AGRPND)
              CALL GETVID('ACTION','GROUP_NO',I,1,NBGRPN,ZK8(AGRPND),N1)
               DO 120, K = 1, NBGRPN, 1
                  NOMGRN = ZK8(AGRPND + K-1)
                  CALL JENONU(JEXNOM(NMAICH//'.GROUPENO',NOMGRN),N1)
                  IF ( N1 .EQ. 0 ) THEN
                     IF ( IER .EQ. 1 ) THEN
                     CALL UTDEBM('S','RVCOHE','ERREUR DANS LES DONNEES')
                        CALL UTIMPI('L','POST-TRAITEMENT NUMERO ',1,I)
                     ENDIF
                    CALL UTIMPK('L','   LE GROUPE DE NOEUDS: ',1,NOMGRN)
                     CALL UTIMPK('S','NE FAIT PAS PARTI DU MAILLAGE '//
     +                  'SOUS-JACENT AU RESULTAT OU CHAMP_GD A TRAITE',
     +                                                         0,K1BID)
                     IER = 0
                  ENDIF
120            CONTINUE
               IF ( IER .EQ. 0 ) CALL UTFINM
               CALL JEDETR('&&OP0051.NOM.GRPN')
            ENDIF
            IF ( NBNEUD .NE. 0 ) THEN
               CALL WKVECT('&&OP0051.NOM.NEUD','V V K8',NBNEUD,ALNEUD)
               CALL GETVID('ACTION','NOEUD',I,1,NBNEUD,ZK8(ALNEUD),N1)
               NREPND = NMAICH//'.NOMNOE'
               DO 130, K = 1, NBNEUD, 1
                  NOMND = ZK8(ALNEUD + K-1)
                  CALL JENONU(JEXNOM(NREPND,NOMND),N1)
                  IF ( N1 .EQ. 0 ) THEN
                     IF ( IER .EQ. 1 ) THEN
                     CALL UTDEBM('S','RVCOHE','ERREUR DANS LES DONNEES')
                        CALL UTIMPI('L','POST-TRAITEMENT NUMERO ',1,I)
                     ENDIF
                     CALL UTIMPK('L','   LE NOEUD: ',1,NOMND)
                     CALL UTIMPK('S','NE FAIT PAS PARTI DU MAILLAGE '//
     +                  'SOUS-JACENT AU RESULTAT OU CHAMP_GD A TRAITE',
     +                                                         0,K1BID)
                     IER = 0
                  ENDIF
130            CONTINUE
               IF ( IER .EQ. 0 ) CALL UTFINM
               CALL JEDETR('&&OP0051.NOM.NEUD')
            ENDIF
         ENDIF
      ENDIF
      CALL JEDEMA()
      END
