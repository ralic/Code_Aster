      SUBROUTINE ASSMAT(BASE,MATAS,NBMAT,TLIMAT,LICOEF,NU,MOTCLE,TYPE)

C  ATTENTION : CETTE ROUTINE NE DOIT PAS ETRE APPELLEE DIRECTEMENT :
C              IL FAUT APPELER SON "CHAPEAU" : ASMATR.

      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*) BASE,MATAS,TLIMAT(*),NU
      REAL*8 LICOEF(*)
      INTEGER NBMAT,TYPE
      CHARACTER*4 MOTCLE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ASSEMBLA  DATE 18/04/2005   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C TOLE CRP_20
C-----------------------------------------------------------------------
C --- DESCRIPTION DES PARAMETRES
C IN  K1 BASE   : BASE SUR LAQUELLE ON CREE L'OBJET MATAS
C OUT K19 MATAS : L'OBJET MATAS DE S.D. MATR_ASSE EST CREE ET REMPLI
C IN  K19 MATAS : NOM DE L'OBJET DE S.D. MATR_ASSE A CREER
C IN  I  NBMAT  : NOMBRE DE MATR_ELEM DE LA LISTE TLIMAT
C IN  K8 TLIMAT : LISTE DES MATR_ELEM
C IN  I  LICOEF : LISTE DES COEFFICIENTS MULTIPLICATEURS DES MATR_ELEM
C IN  K14 NU    : NOM DU NUME_DDL
C IN  K4 MOTCLE : 'ZERO' OU 'CUMU'
C                 'ZERO':SI UN OBJET DE NOM MATAS ET DE S.D.
C                        MATR_ASSE EXISTE ON ECRASE SON CONTENU.
C                 'CUMU':SI UN OBJET DE NOM MATAS ET DE S.D.
C                        MATR_ASSE EXISTE ON CUMMULE DANS .VALE
C IN  I   TYPE  : TYPE DES MATRICES ELEMENTAIRES A ASSMBLEES
C                          1 --> REELLES
C                          2 --> COMPLEXES
C ----------------------------------------------------------------------
C-----------------------------------------------------------------------
C     FONCTIONS JEVEUX
C-----------------------------------------------------------------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR
C-----------------------------------------------------------------------
C     COMMUNS   JEVEUX
C-----------------------------------------------------------------------
      CHARACTER*1 BASE1
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      CHARACTER*8 ZK8,NOMACR,EXIELE
      CHARACTER*14 NUM2
      CHARACTER*16 ZK16,OPTIO,OPTIO2
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C-----------------------------------------------------------------------
C     COMMUNS   LOCAUX DE L'OPERATEUR ASSE_MATRICE
C-----------------------------------------------------------------------
      PARAMETER (NBECMX=10)
      INTEGER ICODLA(NBECMX),ICODGE(NBECMX)
      INTEGER GD,NEC,NMALIL,DIGDEL
      LOGICAL CUMUL,ACREER
      CHARACTER*1 TYPSCA
      CHARACTER*5 K5BID
      CHARACTER*8 MATEL,MO,MO2,MA,NOGDCO,NOGDSI,MA2
      CHARACTER*14 NUDEV
      CHARACTER*19 MATDEV
      CHARACTER*24 K24PRN,KNULIL,KMALIL,KMAREF,RESU,NOMLI,KHCOL,KADIA,
     &             KABLO,KIABL,KVALE,KDESC,KTMP1,KTMP2,KCONL
      REAL*8 R,RINF,RSUP,COEF
      INTEGER IMO,ILAGR,ILIMO,NBEC,ADMODL,LCMODL
      CHARACTER*8 K8BID,K1BID
C-----------------------------------------------------------------------
C     FONCTIONS FORMULES :
C-----------------------------------------------------------------------
      INTEGER ZZCONX,ZZNBNE,ZZLIEL,ZZNGEL,ZZNSUP,ZZNELG,ZZNELS
      INTEGER ZZNEMA,ZZPRNO,IZZPRN,EPDMS,JPDMS,POSDD2
      ZZCONX(IMAIL,J) = ZI(ICONX1-1+ZI(ICONX2+IMAIL-1)+J-1)
      ZZNBNE(IMAIL) = ZI(ICONX2+IMAIL) - ZI(ICONX2+IMAIL-1)
      ZZLIEL(ILI,IGREL,J) = ZI(ZI(IADLIE+3* (ILI-1)+1)-1+
     &                      ZI(ZI(IADLIE+3* (ILI-1)+2)+IGREL-1)+J-1)
      ZZNGEL(ILI) = ZI(IADLIE+3* (ILI-1))
      ZZNSUP(ILI,IEL) = ZI(ZI(IADNEM+3* (ILI-1)+2)+IEL) -
     &                  ZI(ZI(IADNEM+3* (ILI-1)+2)+IEL-1) - 1
      ZZNELG(ILI,IGREL) = ZI(ZI(IADLIE+3* (ILI-1)+2)+IGREL) -
     &                    ZI(ZI(IADLIE+3* (ILI-1)+2)+IGREL-1) - 1
      ZZNELS(ILI) = ZI(IADNEM+3* (ILI-1))
      ZZNEMA(ILI,IEL,J) = ZI(ZI(IADNEM+3* (ILI-1)+1)-1+
     &                    ZI(ZI(IADNEM+3* (ILI-1)+2)+IEL-1)+J-1)
      IZZPRN(ILI,NUNOEL,L) = (IDPRN1-1+ZI(IDPRN2+ILI-1)+
     &                       (NUNOEL-1)* (NEC+2)+L-1)
      ZZPRNO(ILI,NUNOEL,L) = ZI(IDPRN1-1+ZI(IDPRN2+ILI-1)+
     &                       (NUNOEL-1)* (NEC+2)+L-1)
      NUMLOC(KNO,K) = ZI(IANULO-1+2* (KNO-1)+K)
      POSDD2(KNO,KDDL) = ZI(IAPSDL-1+NMXCMP* (KNO-1)+KDDL)
C----------------------------------------------------------------------
      CALL JEMARQ()
      CALL JEDBG2(IDBGAV,0)
      CALL INFNIV(IFM,NIV)
      BASE1 = BASE
      MATDEV = MATAS
      NUDEV = NU

      CALL DISMOI('F','NOM_MODELE',NUDEV,'NUME_DDL',IBID,MO,IERD)
      CALL DISMOI('F','NB_NO_MAILLA',MO,'MODELE',NM,K8BID,IER)
      CALL DISMOI('F','EXI_ELEM',MO,'MODELE',IBID,EXIELE,IERD)
      CALL DISMOI('F','NB_SM_MAILLA',MO,'MODELE',NBSMA,K8BID,IERD)
      CALL DISMOI('F','NOM_MAILLA',MO,'MODELE',IBID,MA,IERD)
      CALL DISMOI('F','NOM_MAILLA',NUDEV,'NUME_DDL',IBID,MA2,IERD)
      CALL ASSERT(MA.EQ.MA2)
      CALL DISMOI('F','NB_NO_SS_MAX',MA,'MAILLAGE',NBNOSS,K8BID,IERD)
      CALL DISMOI('F','NOM_GD',NUDEV,'NUME_DDL',IBID,NOGDCO,IERD)
      CALL DISMOI('F','NOM_GD_SI',NOGDCO,'GRANDEUR',IBID,NOGDSI,IERD)
      CALL DISMOI('F','NB_CMP_MAX',NOGDSI,'GRANDEUR',NMXCMP,K8BID,IERD)
      CALL DISMOI('F','NUM_GD_SI',NOGDSI,'GRANDEUR',NUGD,K8BID,IERD)
      NEC = NBEC(NUGD)
      CALL ASSERT(NEC.LE.NBECMX)
C
C
      CALL JEVEUO(JEXATR('&CATA.TE.MODELOC','LONCUM'),'L',LCMODL)
      CALL JEVEUO(JEXNUM('&CATA.TE.MODELOC',1),'L',ADMODL)

C     -- TEST EXISTENCE &&POIDS_MAILLE
C     ---------------------------------
      CALL JEEXIN('&&POIDS_MAILLE',EPDMS)
      IF (EPDMS.GT.0) CALL JEVEUO('&&POIDS_MAILLE','L',JPDMS)


C     -- ALLOCATION DES OBJETS .NUMLOC ET .POSDDL:
C     ----------------------------------------------
C     50 EST SUPPOSE ETRE LE + GD NOMBRE DE NOEUDS D'UNE MAILLE
C        STANDARD (JUSQU'A PRESENT : 27 (HEXA27))
      NBNOMX = MAX(NBNOSS,50)
      CALL WKVECT('&&ASSMAT.NUMLOC','V V I',2*NBNOMX,IANULO)
      CALL WKVECT('&&ASSMAT.POSDDL','V V I',NBNOMX*NMXCMP,IAPSDL)


C     -- ZERO OU CUMU :
C     -----------------
      IF (MOTCLE(1:4).EQ.'ZERO') THEN
        CUMUL = .FALSE.
      ELSE IF (MOTCLE(1:4).EQ.'CUMU') THEN
        CUMUL = .TRUE.
      ELSE
        CALL ASSERT(.FALSE.)
      END IF


      DO 10 I = 1,NBECMX
        ICODLA(I) = 0
        ICODGE(I) = 0
   10 CONTINUE


C     -- ON MET QUELQUES OBJETS EN MEMOIRE :
C     --------------------------------------
      CALL JEEXIN(MA//'.CONNEX',IRET)
      IF (IRET.GT.0) THEN
        CALL JEVEUO(MA//'.CONNEX','L',ICONX1)
        CALL JEVEUO(JEXATR(MA//'.CONNEX','LONCUM'),'L',ICONX2)
      END IF
      CALL JEVEUO(NUDEV//'.NUME.NUEQ','L',IANUEQ)
      CALL JEEXIN(MA//'.NOMACR',IRET)
      IF (IRET.GT.0) CALL JEVEUO(MA//'.NOMACR','L',IANMCR)
      CALL JEVEUO(JEXNOM('&CATA.GD.NOMCMP',NOGDSI),'L',IANCMP)
      CALL JELIRA(JEXNOM('&CATA.GD.NOMCMP',NOGDSI),'LONMAX',LGNCMP,
     &            K8BID)
      ICMP = INDIK8(ZK8(IANCMP),'LAGR',1,LGNCMP)
      IF (ICMP.GT.0) THEN
        JEC = (ICMP-1)/30 + 1
        ICODLA(JEC) = 2**(ICMP-(JEC-1)*30)
      END IF


      KMALIL = MATDEV//'.LILI'
      KMAREF = MATDEV//'.REFA'
      KVALE = MATDEV//'.VALE'


      IF (MOTCLE(1:4).EQ.'ZERO') THEN
         ACREER = .TRUE.
      ELSE IF (MOTCLE(1:4).EQ.'CUMU') THEN
         ACREER = .FALSE.
      ELSE
        CALL ASSERT(.FALSE.)
      END IF
      IF (ACREER) THEN
        CALL DETRSD('MATR_ASSE',MATDEV)
      ELSE
        CALL JEVEUO(KMAREF,'L',IDMARF)
        CALL ASSERT(ZK24(IDMARF-1+2) (1:14).EQ.NUDEV)
        CALL ASSERT(ZK24(IDMARF-1+3) (17:18).EQ.'LC')
        CALL JEDETR(KMAREF(1:19)//'.LIME')
        CALL JEDETR(MATDEV(1:19)//'.CONI')
        CALL JEDETR(MATDEV(1:19)//'.ABLI')
        CALL JEDETR(MATDEV(1:19)//'.LLIG')
        CALL JEDETR(MATDEV(1:19)//'.ALIG')
        CALL JEDETR(MATDEV(1:19)//'.VALI')
      END IF


C---- RECOPIE DE LA LISTE DES MATR_ELEM DANS 1 OBJET JEVEUX DONT ON
C     GARDE L'ADRESSE DANS LE COMMON /CADMAT/

      CALL WKVECT(MATDEV//'.LIME',BASE1//' V K8 ',NBMAT,ILIMAT)
      DO 20 I = 1,NBMAT
        ZK8(ILIMAT+I-1) = TLIMAT(I)
   20 CONTINUE

      K5BID(1:5) = '.SLCS'
C---- POUR LE MOMMENT PAR DEFAUT LE PROFIL EST LIGN_CIEL
      KHCOL = NUDEV//K5BID(1:5)//'.HCOL'
      CALL JEVEUO(KHCOL,'L',IDHCOL)
      KADIA = NUDEV//K5BID(1:5)//'.ADIA'
      CALL JEVEUO(KADIA,'L',IDADIA)
      KABLO = NUDEV//K5BID(1:5)//'.ABLO'
      CALL JEVEUO(KABLO,'L',IDABLO)
      KIABL = NUDEV//K5BID(1:5)//'.IABL'
      CALL JEVEUO(KIABL,'L',IDIABL)
      KDESC = NUDEV//K5BID(1:5)//'.DESC'

C---- CALCUL D UN REPERTOIRE,TEMPORAIRE, MATDEV.LILI A PARTIR DE LA
C     LISTE DE MATRICES ELEMENTAIRES MATDEV.LIME

      CALL CRELIL(NBMAT,ILIMAT,KMALIL,'V','&MAILLA',MATDEV,IBID,MA,IBID,
     &            IBID,ILIMO,NMALIL,NBELM)
      CALL JEVEUO(MATDEV(1:19)//'.ADLI','E',IADLIE)
      CALL JEVEUO(MATDEV(1:19)//'.ADNE','E',IADNEM)
      NCMP = NMXCMP


C---- ON SUPPOSE QUE LE NOM '&MAILLA' EST LE PREMIER DU REPERTOIRE
C     NU.LILI CE QUI EST VRAI CF S.P. CRELIL
      ILIMNU = 1

C---- NOMS DES PRINCIPAUX OBJETS JEVEUX LIES A NU
      K24PRN = NUDEV//'.NUME.PRNO'
      KNULIL = NUDEV//'.NUME.LILI'

C---- RECUPERATION DE PRNO
      CALL JEVEUO(K24PRN,'L',IDPRN1)
      CALL JEVEUO(JEXATR(K24PRN,'LONCUM'),'L',IDPRN2)

C---- CREATION ET REMPLISSAGE DE REFE
      IF (ACREER) CALL WKVECT(KMAREF,BASE1//' V K24',4,IDMARF)
      CALL JEVEUO(KMAREF,'E',IDMARF)
      CALL JEECRA(KMAREF,'DOCU',IBID,'ASSE')
      ZK24(IDMARF) = MA
      ZK24(IDMARF+1) = NUDEV//'.NUME'
      ZK24(IDMARF+2) = NUDEV//K5BID(1:5)


C---- CREATION DE 2 OBJETS VOLATILES POUR ACCELERER ASCOPR:
C------------------------------------------------------------

C---- TMP1 : (1:NBLC) INDIQUE LE NOMBRE DE REELS D'1 MATRICE ELEMENTAIRE
C            S'INJECTANT DANS LE BLOC I.
C---- TMP2 : (1:2*DIM(MATRICE ELEMENTAIRE))
C            POSITION RELATIVE DANS LES BLOCS
C            POUR LE I-EME REEL DE LA MATRICE ELEM :
C     TMP2(2*(I-1)+1) --> NUMERO DU BLOC OU S'INJECTE I.
C     TMP2(2*(I-1)+2) --> POSITION DANS LE BLOC DU REEL I.

C     -- L'OBJET .TMP1 EST CREE TOUT DE SUITE
C     -- L'OBJET .TMP2 SERA CREE PLUTARD

      KTMP1 = KVALE(1:19)//'.TMP1           '
      KTMP2 = KVALE(1:19)//'.TMP2           '
      CALL JEVEUO(KDESC,'L',IDDESC)
      NEQU = ZI(IDDESC)
      ITBLOC = ZI(IDDESC+1)
      NBLC = ZI(IDDESC+2)

      CALL WKVECT(KTMP1,' V V I',NBLC,IATMP1)

C---- ALLOCATION VALE EN R OU C SUIVANT TYPE
      IF (ACREER) THEN
        IF (TYPE.EQ.1) THEN
          CALL JECREC(KVALE,BASE1//' V R','NU','DISPERSE','CONSTANT',
     &                NBLC)
        ELSE IF (TYPE.EQ.2) THEN
          CALL JECREC(KVALE,BASE1//' V C','NU','DISPERSE','CONSTANT',
     &                NBLC)
        ELSE
          CALL UTMESS('F','ASSMAT',' ON NE PEUT ASSEMBLER QUE DES'//
     &                ' MATRICES REELLES OU COMPLEXES')
        END IF
        CALL JEECRA(KVALE,'LONMAX',ITBLOC,' ')
        CALL JEECRA(KVALE,'DOCU',IBID,'MS')
        DO 30 I = 1,NBLC
          CALL JECROC(JEXNUM(KVALE,I))
          CALL JEVEUO(JEXNUM(KVALE,I),'E',IABIDO)
          CALL JELIBE(JEXNUM(KVALE,I))
   30   CONTINUE
      ELSE
        IF (.NOT.CUMUL) THEN
C        -- REMISE A ZERO DE .VALE :
          DO 40 I = 1,NBLC
            CALL JERAZO(JEXNUM(KVALE,I),ITBLOC,1)
            CALL JELIBE(JEXNUM(KVALE,I))
   40     CONTINUE
        END IF
      END IF

C---- CALCUL (REMPLISSAGE) DE VALE
C     ON COMMENCE PAR ASSEMBLER SUR LE MODELE
      IMO = 1
      RINF = R8MAEM()
      RSUP = -1.D0
      COEF = 1.D0
   50 CONTINUE


      ILONG = 0
      DO 280 IMAT = 1,NBMAT
        MATEL = ZK8(ILIMAT+IMAT-1)
        CALL DISMOI('F','NOM_MODELE',MATEL,'MATR_ELEM',IBID,MO2,IERD)
        CALL DISMOI('F','SUR_OPTION',MATEL,'MATR_ELEM',IBID,OPTIO,IERD)

        IF (IMAT.EQ.1) THEN
          OPTIO2 = OPTIO
        ELSE
          IF (OPTIO2.NE.OPTIO) OPTIO2 = '&&MELANGE'
        END IF

        IF (MO2.NE.MO) CALL UTMESS('F','ASSMAT','MODELES DISCORDANTS')
        IF (IMO.NE.1) GO TO 140


C         -- TRAITEMENT DES SOUS-STRUCTURES :
C         -----------------------------------
        CALL DISMOI('F','NB_SS_ACTI',MATEL,'MATR_ELEM',NBSSA,K8BID,IERD)

        IF (NBSSA.GT.0) THEN
          CALL JEVEUO(MO//'.SSSA','L',IASSSA)

          CALL SSVALM('DEBUT',OPTIO,MO,MA,IMA,IDRESL,NCMPEL)

          DO 130,IMA = 1,NBSMA
            IF (ZI(IASSSA-1+IMA).EQ.0) GO TO 130

            CALL JEVEUO(JEXNUM(MA//'.SUPMAIL',IMA),'L',IAMAIL)
            CALL JELIRA(JEXNUM(MA//'.SUPMAIL',IMA),'LONMAX',NNOE,K8BID)

            IREEL = 0
            R = LICOEF(IMAT)

            CALL SSVALM(' ',OPTIO,MO,MA,IMA,IDRESL,NCMPEL)
            IF (NCMPEL.GT.ILONG) THEN
              ILONG = NCMPEL
              CALL JEDETR(KTMP2)
              CALL WKVECT(KTMP2,' V V I',2*ILONG,IATMP2)
            END IF

            NOMACR = ZK8(IANMCR-1+IMA)
            CALL DISMOI('F','NOM_NUME_DDL',NOMACR,'MACR_ELEM_STAT',IBID,
     &                  NUM2,IERD)
            CALL JEVEUO(NOMACR//'.CONX','L',IACONX)
            CALL JEVEUO(JEXNUM(NUM2//'.NUME.PRNO',1),'L',IAPROL)

            DO 120 K1 = 1,NNOE
              N1 = ZI(IAMAIL-1+K1)
              IF (N1.GT.NM) THEN
                DO 60 IEC = 1,NBECMX
                  ICODGE(IEC) = ICODLA(IEC)
   60           CONTINUE
              ELSE
                INOLD = ZI(IACONX-1+3* (K1-1)+2)
                DO 70 IEC = 1,NEC
                  ICODGE(IEC) = ZI(IAPROL-1+ (NEC+2)* (INOLD-1)+2+IEC)
   70           CONTINUE
              END IF

              IAD1 = ZZPRNO(ILIMNU,N1,1)
              CALL CORDD2(IDPRN1,IDPRN2,ILIMNU,ICODGE,NEC,NCMP,N1,NDDL1,
     &                    ZI(IAPSDL-1+NMXCMP* (K1-1)+1))
              ZI(IANULO-1+2* (K1-1)+1) = IAD1
              ZI(IANULO-1+2* (K1-1)+2) = NDDL1
              DO 110 I1 = 1,NDDL1
                DO 90 K2 = 1,K1 - 1
                  IAD2 = NUMLOC(K2,1)
                  NDDL2 = NUMLOC(K2,2)
                  DO 80 I2 = 1,NDDL2
                    IAD11 = ZI(IANUEQ-1+IAD1+POSDD2(K1,I1)-1)
                    IAD21 = ZI(IANUEQ-1+IAD2+POSDD2(K2,I2)-1)
                    IF (IAD11.LE.IAD21) THEN
                      IADLI = IAD11
                      IADCO = IAD21
                    ELSE
                      IADLI = IAD21
                      IADCO = IAD11
                    END IF
                    NUMBL = ZI(IDIABL-1+IADCO)
                    ZI(IATMP1-1+NUMBL) = ZI(IATMP1-1+NUMBL) + 1
                    CALL ASRETI(IATMP2,IREEL,IDHCOL,IDADIA,IDABLO,IADLI,
     &                          IADCO,NUMBL)
   80             CONTINUE
   90           CONTINUE
                K2 = K1
                IAD2 = NUMLOC(K2,1)
                NDDL2 = NUMLOC(K2,2)
                DO 100 I2 = 1,I1
                  IAD11 = ZI(IANUEQ-1+IAD1+POSDD2(K1,I1)-1)
                  IAD21 = ZI(IANUEQ-1+IAD2+POSDD2(K2,I2)-1)
                  IF (IAD11.LE.IAD21) THEN
                    IADLI = IAD11
                    IADCO = IAD21
                  ELSE
                    IADLI = IAD21
                    IADCO = IAD11
                  END IF
                  NUMBL = ZI(IDIABL-1+IADCO)
                  ZI(IATMP1-1+NUMBL) = ZI(IATMP1-1+NUMBL) + 1
                  CALL ASRETI(IATMP2,IREEL,IDHCOL,IDADIA,IDABLO,IADLI,
     &                        IADCO,NUMBL)
  100           CONTINUE
  110         CONTINUE
  120       CONTINUE
C         ---- POUR FINIR, ON RECOPIE EFFECTIVEMENT LES TERMES:
            IF (TYPE.EQ.1) CALL ASCOPR(IATMP1,IATMP2,IREEL,IDRESL,NBLC,
     &                                 KVALE,R,0,0)
            IF (TYPE.EQ.2) CALL ASCOPC(IATMP1,IATMP2,IREEL,IDRESL,NBLC,
     &                                 KVALE,R,0,0)
  130     CONTINUE
          CALL SSVALM('FIN',OPTIO,MO,MA,IMA,IDRESL,NCMPEL)
        END IF


C         -- TRAITEMENT DES ELEMENTS FINIS CLASSIQUES
C         -----------------------------------------------------------
  140   CONTINUE
        CALL JEEXIN(MATEL//'.LISTE_RESU',IRET)
        IF (IRET.GT.0) THEN
          CALL JEVEUO(MATEL//'.LISTE_RESU','L',IDLRES)
          CALL JELIRA(MATEL//'.LISTE_RESU','LONUTI ',NBRESU,K1BID)
          DO 270 IRESU = 1,NBRESU
            RESU = ZK24(IDLRES+IRESU-1)
            CALL JEEXIN(RESU(1:19)//'.DESC',IER)
            IF (IER.EQ.0) GO TO 270

            CALL JEEXIN(RESU(1:19)//'.DESC',IER)
            IF (IER.EQ.0) GO TO 270

            CALL JEVEUO(RESU(1:19)//'.NOLI','L',IAD)
            NOMLI = ZK24(IAD)
            CALL JENONU(JEXNOM(KMALIL,NOMLI),ILIMA)
            CALL JENONU(JEXNOM(KNULIL,NOMLI),ILINU)
            IF ((IMO.EQ.1) .AND. (ILIMA.NE.ILIMO)) GO TO 270
            IF ((IMO.EQ.0) .AND. (ILIMA.EQ.ILIMO)) GO TO 270

            CALL DISMOI('F','TYPE_SCA',RESU,'RESUELEM',IBID,TYPSCA,IERD)

            DO 260 IGR = 1,ZZNGEL(ILIMA)
              CALL JEVEUO(RESU(1:19)//'.DESC','L',IADESC)
              MODE = ZI(IADESC+IGR+1)
              IF (MODE.GT.0) THEN
                NNOE = NBNO(MODE)
                NEL = ZZNELG(ILIMA,IGR)
                CALL JEVEUO(JEXNUM(RESU(1:19)//'.RESL',IGR),'L',IDRESL)
                NCMPEL = DIGDEL(MODE)
                IF (NCMPEL.GT.ILONG) THEN
                  ILONG = NCMPEL
                  CALL JEDETR(KTMP2)
                  CALL WKVECT(KTMP2,' V V I',2*ILONG,IATMP2)
                END IF
                DO 250 IEL = 1,NEL
                  IREEL = 0
                  ILAGR = 0
                  R = LICOEF(IMAT)
                  NUMA = ZZLIEL(ILIMA,IGR,IEL)
                  IF (NUMA.GT.0) THEN
                    IF (EPDMS.GT.0) R = R*ZR(JPDMS-1+NUMA)
                    DO 190 K1 = 1,NNOE
                      N1 = ZZCONX(NUMA,K1)
                      IAD1 = ZZPRNO(ILIMNU,N1,1)
                      CALL CORDDL(ADMODL,LCMODL,IDPRN1,IDPRN2,ILIMNU,
     &                            MODE,NEC,NCMP,N1,K1,NDDL1,
     &                            ZI(IAPSDL-1+NMXCMP* (K1-1)+1))
                      ZI(IANULO-1+2* (K1-1)+1) = IAD1
                      ZI(IANULO-1+2* (K1-1)+2) = NDDL1
                      DO 180 I1 = 1,NDDL1
                        DO 160 K2 = 1,K1 - 1
                          IAD2 = NUMLOC(K2,1)
                          NDDL2 = NUMLOC(K2,2)
                          DO 150 I2 = 1,NDDL2
                            IAD11 = ZI(IANUEQ-1+IAD1+POSDD2(K1,I1)-1)
                            IAD21 = ZI(IANUEQ-1+IAD2+POSDD2(K2,I2)-1)
                            IF (IAD11.LE.IAD21) THEN
                              IADLI = IAD11
                              IADCO = IAD21
                            ELSE
                              IADLI = IAD21
                              IADCO = IAD11
                            END IF
                            NUMBL = ZI(IDIABL-1+IADCO)
                            ZI(IATMP1-1+NUMBL) = ZI(IATMP1-1+NUMBL) + 1
                            CALL ASRETI(IATMP2,IREEL,IDHCOL,IDADIA,
     &                                  IDABLO,IADLI,IADCO,NUMBL)
  150                     CONTINUE
  160                   CONTINUE
                        K2 = K1
                        IAD2 = NUMLOC(K2,1)
                        NDDL2 = NUMLOC(K2,2)
                        DO 170 I2 = 1,I1
                          IAD11 = ZI(IANUEQ-1+IAD1+POSDD2(K1,I1)-1)
                          IAD21 = ZI(IANUEQ-1+IAD2+POSDD2(K2,I2)-1)
                          IF (IAD11.LE.IAD21) THEN
                            IADLI = IAD11
                            IADCO = IAD21
                          ELSE
                            IADLI = IAD21
                            IADCO = IAD11
                          END IF
                          NUMBL = ZI(IDIABL-1+IADCO)
                          ZI(IATMP1-1+NUMBL) = ZI(IATMP1-1+NUMBL) + 1
                          CALL ASRETI(IATMP2,IREEL,IDHCOL,IDADIA,IDABLO,
     &                                IADLI,IADCO,NUMBL)
  170                   CONTINUE
  180                 CONTINUE
  190               CONTINUE
                  ELSE
C---- CONDITIONNEMENT DES LAGRANGE SI OPTION RIGI_XXXX NO
C---- SI MAILLE A 3 NOEUDS          -        - ILAGR = 1 POSSIBILITE DE
C----      ET                       ! ALORS  ! NOEUDS LAGRANGE DANS LA
C---- SI ON N'EST PAS SUR LE MODELE -        - MAILLE
C----
C---- SI ILAGR=1 ET SI LA MAILLE CONTIENT UN NOEUD A UN DDL
C---- ALORS ON EST SUR UNE MAILLE TARDIVE DE "LAGRANGE"
                    IF ((IMO.EQ.0) .AND. (NNOE.EQ.3)) ILAGR = 1
                    NUMA = -NUMA
                    NSUP = ZZNSUP(ILIMA,NUMA)
                    IF (NSUP.NE.NNOE) THEN
                      CALL UTDEBM('F','ASSMAT','GROSSE ERREUR')
                      CALL UTIMPI('L','LE NBRE DE NOEUDS DE LA '//
     &                            ' MAILLE SUP : ',1,NUMA)
                      CALL UTIMPI('S','DU LIGREL : ',1,NNOLI)
                      CALL UTIMPK('L','EST DIFFERENT DE CELUI '//
     &                            'ASSOCIE AU RESUELEM : ',1,RESU)
                      CALL UTFINM()
                    END IF
                    DO 240 K1 = 1,NNOE
                      N1 = ZZNEMA(ILIMA,NUMA,K1)
                      IF (N1.LT.0) THEN
                        N1 = -N1
                        IAD1 = ZZPRNO(ILINU,N1,1)
                        CALL CORDDL(ADMODL,LCMODL,IDPRN1,IDPRN2,ILINU,
     &                              MODE,NEC,NCMP,N1,K1,NDDL1,
     &                              ZI(IAPSDL-1+NMXCMP* (K1-1)+1))

C---- SI NOEUD LAGR ( NNOE=3,NDDL1=1,N1<0,NUMA<0 ) ALORS CONL(IAD1)=R
C---- ET POUR TOUTE LA MATRICE ELEMENTAIRE ON POSE R = COEF*LICOEF(IMAT)
                        IF ((ILAGR.EQ.1) .AND. (NDDL1.EQ.1)) THEN
                          R = COEF*LICOEF(IMAT)
                          ZR(IDCONL-1+ZI(IANUEQ-1+IAD1)) = R
                        END IF
                      ELSE
                        IAD1 = ZZPRNO(ILIMNU,N1,1)
                        CALL CORDDL(ADMODL,LCMODL,IDPRN1,IDPRN2,ILIMNU,
     &                              MODE,NEC,NCMP,N1,K1,NDDL1,
     &                              ZI(IAPSDL-1+NMXCMP* (K1-1)+1))
                      END IF
                      ZI(IANULO-1+2* (K1-1)+1) = IAD1
                      ZI(IANULO-1+2* (K1-1)+2) = NDDL1
                      DO 230 I1 = 1,NDDL1
                        DO 210 K2 = 1,K1 - 1
                          IAD2 = NUMLOC(K2,1)
                          NDDL2 = NUMLOC(K2,2)
                          DO 200 I2 = 1,NDDL2
                            IAD11 = ZI(IANUEQ-1+IAD1+POSDD2(K1,I1)-1)
                            IAD21 = ZI(IANUEQ-1+IAD2+POSDD2(K2,I2)-1)
                            IF (IAD11.LE.IAD21) THEN
                              IADLI = IAD11
                              IADCO = IAD21
                            ELSE
                              IADLI = IAD21
                              IADCO = IAD11
                            END IF
                            NUMBL = ZI(IDIABL-1+IADCO)
                            ZI(IATMP1-1+NUMBL) = ZI(IATMP1-1+NUMBL) + 1
                            CALL ASRETI(IATMP2,IREEL,IDHCOL,IDADIA,
     &                                  IDABLO,IADLI,IADCO,NUMBL)
  200                     CONTINUE
  210                   CONTINUE
                        K2 = K1
                        IAD2 = NUMLOC(K2,1)
                        NDDL2 = NUMLOC(K2,2)
                        DO 220 I2 = 1,I1
                          IAD11 = ZI(IANUEQ-1+IAD1+POSDD2(K1,I1)-1)
                          IAD21 = ZI(IANUEQ-1+IAD2+POSDD2(K2,I2)-1)
                          IF (IAD11.LE.IAD21) THEN
                            IADLI = IAD11
                            IADCO = IAD21
                          ELSE
                            IADLI = IAD21
                            IADCO = IAD11
                          END IF
                          NUMBL = ZI(IDIABL-1+IADCO)
                          ZI(IATMP1-1+NUMBL) = ZI(IATMP1-1+NUMBL) + 1
                          CALL ASRETI(IATMP2,IREEL,IDHCOL,IDADIA,IDABLO,
     &                                IADLI,IADCO,NUMBL)
  220                   CONTINUE
  230                 CONTINUE
  240               CONTINUE
                  END IF
C---- POUR FINIR, ON RECOPIE EFFECTIVEMENT LES TERMES:
                  IF (TYPE.EQ.1) THEN
                    CALL ASCOPR(IATMP1,IATMP2,IREEL,
     &                          IDRESL+NCMPEL* (IEL-1),NBLC,KVALE,R,0,0)
                  ELSE IF (TYPE.EQ.2) THEN
                    IF (TYPSCA.EQ.'R') THEN
                      CALL ASCOPN(IATMP1,IATMP2,IREEL,
     &                            IDRESL+NCMPEL* (IEL-1),NBLC,KVALE,R,0,
     &                            0)
                    ELSE IF (TYPSCA.EQ.'C') THEN
                      CALL ASCOPC(IATMP1,IATMP2,IREEL,
     &                            IDRESL+NCMPEL* (IEL-1),NBLC,KVALE,R,0,
     &                            0)
                    END IF
                  END IF
  250           CONTINUE
                CALL JELIBE(JEXNUM(RESU(1:19)//'.RESL',IGR))
              END IF
  260       CONTINUE
  270     CONTINUE
        END IF
  280 CONTINUE
      IF (IMO.EQ.1) THEN

C        --- SI ON TRAITE LE MODELE ---
        DO 300 IBLC = 1,NBLC
          CALL JEVEUO(JEXNUM(KVALE,IBLC),'L',IDIBLC)
          DO 290 I = ZI(IDABLO+IBLC-1) + 1,ZI(IDABLO+IBLC)
            IDI = ZI(IDADIA+I-1)
            IF (TYPE.EQ.1) R = ABS(ZR(IDIBLC+IDI-1))
            IF (TYPE.EQ.2) R = ABS(ZC(IDIBLC+IDI-1))
            IF ((R.NE.0.D0) .AND. (R.LT.RINF)) RINF = R
            IF ((R.NE.0.D0) .AND. (R.GT.RSUP)) RSUP = R
  290     CONTINUE
          CALL JELIBE(JEXNUM(KVALE,IBLC))
  300   CONTINUE
        COEF = (RSUP+RINF)/2.D0
        IF (RINF.GE.R8MAEM()) COEF = RSUP/2.D0
        IMO = 0
C---- CREATION DE L'OBJET .CONL EN OPTION RIGI SI AU MOINS UNE CHARGE
        IF (NMALIL.GT.2) THEN
          KCONL = MATDEV(1:19)//'.CONL'
          IF (ACREER) CALL WKVECT(KCONL,BASE1//' V R',NEQU,IBID)
          CALL JEVEUO(KCONL,'E',IDCONL)
          DO 310 I = 1,NEQU
            ZR(IDCONL-1+I) = 1.D0
  310     CONTINUE
          IF (NIV.EQ.2) THEN
            WRITE (IFM,*) 'COEFFICIENT DE CONDITIONNEMENT'//
     &        ' DES LAGRANGES :',COEF
          END IF
        END IF
        GO TO 50
      END IF

C     -- MISE A JOUR DE REFA(4)
      CALL JEVEUO(KMAREF,'E',IDMARF)
      IF (MOTCLE(1:4).EQ.'ZERO') THEN
        ZK24(IDMARF-1+4) = OPTIO2
      ELSE
        IF (ZK24(IDMARF-1+4).NE.OPTIO2) ZK24(IDMARF-1+4) = '&&MELANGE'
      END IF

      CALL JEDETR('&&ASSMAT.NUMLOC')
      CALL JEDETR('&&ASSMAT.POSDDL')
      CALL JEDETR(KTMP1)
      CALL JEDETR(KTMP2)
      CALL JEDETR(MATDEV//'.ADNE')
      CALL JEDETR(MATDEV//'.ADLI')
      CALL JEDETR(KVALE(1:19)//'.TMP1')
      CALL JEDETR(KVALE(1:19)//'.TMP2')

      CALL JEDBG2(IBID,IDBGAV)
      CALL JEDEMA()
      END
