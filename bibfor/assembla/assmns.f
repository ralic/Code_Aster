      SUBROUTINE ASSMNS(BASE,MATAS,NBMAT,TLIMAT,LICOEF,NU,MOTCLE,TYPE)

C  ATTENTION : CETTE ROUTINE NE DOIT PAS ETRE APPELLEE DIRECTEMENT :
C              IL FAUT APPELER SON "CHAPEAU" : ASMATR.

      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*) BASE,MATAS,TLIMAT(*),NU
      REAL*8 LICOEF(*)
      INTEGER NBMAT,TYPE
      CHARACTER*4 MOTCLE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ASSEMBLA  DATE 11/09/2002   AUTEUR VABHHTS J.PELLET 
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
C-----------------------------------------------------------------------
C --- DESCRIPTION DES PARAMETRES
C IN  K* BASE   : BASE SUR LAQUELLE ON CREE L'OBJET MATAS
C OUT K* MATAS  : L'OBJET MATAS DE TYPE MATR_ASSE EST CREE ET REMPLI
C IN  K* MATAS  : NOM DE L'OBJET DE TYPE MATR_ASSE A CREER
C IN  I  NBMAT  : NOMBRE DE MATR_ELEM DE LA LISTE TLIMAT
C IN  K* TLIMAT : LISTE DES MATR_ELEM
C IN  I  LICOEF : LISTE DES COEFFICIENTS MULTIPLICATEURS DES MATR_ELEM
C IN  K* NU     : NOM DU NUME_DDL
C IN  K4 MOTCLE : 'ZERO' OU 'CUMU'
C                 'ZERO':SI UN OBJET DE NOM MATAS ET DE TYPE
C                        MATR_ASSE EXISTE ON ECRASE SON CONTENU.
C                 'CUMU':SI UN OBJET DE NOM MATAS ET DE TYPE
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
      CHARACTER*1 BAS2
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
      PARAMETER (NBECMX = 10)
      INTEGER ICODLA(NBECMX), ICODGE(NBECMX)
      INTEGER N,GD,NEC,NMALIL,DIGDEL
      LOGICAL CUMUL,ACREER
      CHARACTER*1 TYMAT
      CHARACTER*7 SYM
      CHARACTER*5 K5BID
      CHARACTER*8 MATEL,MO,MO2,MA,NOGDCO,NOGDSI,NOGD
      CHARACTER*8 KBID
      CHARACTER*14 NUDEV
      CHARACTER*19 MATDEV,RESUL
      CHARACTER*24 KMAILL,K24PRN,KNULIL,KMALIL,KMAREF,RESU,
     +             NOMLI,KHCOL,KADIA,KABLO,KIABL,KVALE,KDESC,KTMP1,
     +             KTMP2,KCONL
      REAL*8 R,RINF,RSUP,COEF
      INTEGER HMAX,IMO,ILAGR,ILIMO
      INTEGER NBNO
      INTEGER NBEC
      CHARACTER*1 K1BID
C-----------------------------------------------------------------------
C     FONCTIONS LOCALES D'ACCES AUX DIFFERENTS CHAMPS DES
C     TYPE MANIPULEES DANS LE SOUS PROGRAMME
C-----------------------------------------------------------------------
      INTEGER ZZCONX,ZZNBNE,ZZLIEL,ZZNGEL,ZZNSUP,ZZNELG,ZZNELS
      INTEGER ZZNEMA,ZZPRNO,IZZPRN
C
C---- FONCTION D ACCES AU CHAMP CONNEX DE LA TYPE MA DE TYPE
C     MAILLAGE
C     ZZCONX(IMAIL,J) = NUMERO DANS LA NUMEROTATION DU MAILLAGE
C         DU NOEUD J DE LA MAILLE IMAIL
      ZZCONX(IMAIL,J) = ZI(ICONX1-1+ZI(ICONX2+IMAIL-1)+J-1)
C
C---- NBRE DE NOEUDS DE LA MAILLE IMAIL DU MAILLAGE
C
      ZZNBNE(IMAIL) = ZI(ICONX2+IMAIL) - ZI(ICONX2+IMAIL-1)
C
C---- FONCTION D ACCES AUX ELEMENTS DES CHAMPS LIEL DES TYPE LIGREL
C     REPERTORIEES DANS LE REPERTOIRE TEMPORAIRE .MATAS.LILI
C     ZZLIEL(ILI,IGREL,J) =
C      SI LA JIEME MAILLE DU LIEL IGREL DU LIGREL ILI EST:
C          -UNE MAILLE DU MAILLAGE : SON NUMERO DANS LE MAILLAGE
C          -UNE MAILLE TARDIVE : -POINTEUR DANS LE CHAMP .NEMA
C
      ZZLIEL(ILI,IGREL,J) = ZI(ZI(IADLIE+3* (ILI-1)+1)-1+
     +                      ZI(ZI(IADLIE+3* (ILI-1)+2)+IGREL-1)+J-1)
C
C---- NBRE DE GROUPES D'ELEMENTS (DE LIEL) DU LIGREL ILI
C
      ZZNGEL(ILI) = ZI(IADLIE+3* (ILI-1))
C
C---- NBRE DE NOEUDS DE LA MAILLE TARDIVE IEL ( .NEMA(IEL))
C     DU LIGREL ILI REPERTOIRE .LILI
C     (DIM DU VECTEUR D'ENTIERS .LILI(ILI).NEMA(IEL) )
C
      ZZNSUP(ILI,IEL) = ZI(ZI(IADNEM+3* (ILI-1)+2)+IEL) -
     +                  ZI(ZI(IADNEM+3* (ILI-1)+2)+IEL-1) - 1
C
C---- NBRE D ELEMENTS DU LIEL IGREL DU LIGREL ILI DU REPERTOIRE TEMP.
C     .MATAS.LILI(DIM DU VECTEUR D'ENTIERS .LILI(ILI).LIEL(IGREL) )
C
      ZZNELG(ILI,IGREL) = ZI(ZI(IADLIE+3* (ILI-1)+2)+IGREL) -
     +                    ZI(ZI(IADLIE+3* (ILI-1)+2)+IGREL-1) - 1
C
C---- NBRE D ELEMENTS SUPPLEMENTAIRE (.NEMA) DU LIGREL ILI DU
C     REPERTOIRE TEMPORAIRE .MATAS.LILI
C
      ZZNELS(ILI) = ZI(IADNEM+3* (ILI-1))
C
C---- FONCTION D ACCES AUX ELEMENTS DES CHAMPS NEMA DES TYPE LIGREL
C     REPERTORIEES DANS LE REPERTOIRE TEMPO. .MATAS.LILI
C     ZZNEMA(ILI,IEL,J) =  1.LE. J .GE. ZZNELS(ILI)
C      SI LE J IEME NOEUD DE LA MAILE TARDIVE IEL DU LIGREL ILI EST:
C          -UN NOEUD DU MAILLAGE : SON NUMERO DANS LE MAILLAGE
C          -UN NOEUD TARDIF : -SON NUMERO DANS LA NUMEROTATION LOCALE
C                              AU LIGREL ILI
C     ZZNEMA(ILI,IEL,ZZNELS(ILI)+1)=NUMERO DU TYPE_MAILLE DE LA MAILLE
C                                   IEL DU LIGREL ILI
C
      ZZNEMA(ILI,IEL,J) = ZI(ZI(IADNEM+3* (ILI-1)+1)-1+
     +                    ZI(ZI(IADNEM+3* (ILI-1)+2)+IEL-1)+J-1)
C
C---- FONCTION D ACCES AUX ELEMENTS DES CHAMPS PRNO DES TYPE LIGREL
C     REPERTORIEES DANS NU.LILI DE LA TYPE NUME_DDL ET A LEURS ADRESSES
C     ZZPRNO(ILI,NUNOEL,1) = NUMERO DE L'EQUATION ASSOCIEES AU 1ER DDL
C                            DU NOEUD NUNOEL DANS LA NUMEROTATION LOCALE
C                            AU LIGREL ILI DE .LILI
C     ZZPRNO(ILI,NUNOEL,2) = NOMBRE DE DDL PORTES PAR LE NOEUD NUNOEL
C     ZZPRNO(ILI,NUNOEL,2+1) = 1ER CODE
C     ZZPRNO(ILI,NUNOEL,2+NEC) = NEC IEME CODE
C
      IZZPRN(ILI,NUNOEL,L) = (IDPRN1-1+ZI(IDPRN2+ILI-1)+
     +                       (NUNOEL-1)* (NEC+2)+L-1)
      ZZPRNO(ILI,NUNOEL,L) = ZI(IDPRN1-1+ZI(IDPRN2+ILI-1)+
     +                       (NUNOEL-1)* (NEC+2)+L-1)
C----------------------------------------------------------------------
C                DEBUT DES INSTRUCTIONS
      CALL JEMARQ()
      CALL JEDBG2(IDBGAV,0)
C
C----------------------------------------------------------------------
C
C-----INTERROGATION SUR LA VALEUR DU MOT CLE IMPR
C
      CALL INFNIV(IFM,NIV)
C---------------------------------------------------------------------
C
      BAS2 = BASE
      IFM = IUNIFI('MESSAGE')
      MATDEV = MATAS
C
      DO 1 I = 1, NBECMX
        ICODLA(I) = 0
        ICODGE(I) = 0
 1    CONTINUE
C
C---- NOMS DES PRINCIPAUX OBJETS JEVEUX LIES A MATAS
C
      KMAILL = '&MAILLA                 '
      KMALIL = MATDEV//'.LILI'
      KMAREF = MATDEV//'.REFA'
      KVALE = MATDEV//'.VALE'
C
C---- NOMS DES PRINCIPAUX OBJETS JEVEUX LIES AU PROFIL DE NU
C
      NUDEV = NU
C
C     ---- ALLOCATION DES OBJETS .NUMLOC ET .POSDDL:
C     ----------------------------------------------
      CALL DISMOI('F','NOM_MAILLA',NUDEV,'NUME_DDL',IBID,MA,IERD)
      CALL DISMOI('F','NOM_MODELE',NUDEV,'NUME_DDL',IBID,MO,IERD)
      CALL DISMOI('F','NB_NO_SS_MAX',MA,'MAILLAGE',NBNOSS,KBID,IERD)
C     100 EST SUPPOSE ETRE LA + GDE DIMENSION D'UNE MAILLE STANDARD:
      NBNOSS= MAX(NBNOSS,100)
C     -- NUMLOC(K,INO) (K=1,3)(INO=1,NBNO(MAILLE))
      CALL WKVECT('&&ASSMNS.NUMLOC','V V I',3*NBNOSS,IANULO)
C
      CALL DISMOI('F','NOM_GD',NUDEV,'NUME_DDL',IBID,NOGDCO,IERD)
      CALL DISMOI('F','NOM_GD_SI',NOGDCO,'GRANDEUR',IBID,NOGDSI,IERD)
      CALL DISMOI('F','NB_CMP_MAX',NOGDSI,'GRANDEUR',NMXCMP,KBID,IERD)
C     -- POSDDL(ICMP,INO) (ICMP=1,NMXCMP(GD_SI))(INO=1,NBNO(MAILLE))
      CALL WKVECT('&&ASSMNS.POSDDL','V V I',NBNOSS*NMXCMP,IAPSDL)
C
C     -- ON PREPARE L'ASSEMBLAGE DES SOUS-STRUCTURES:
C     -----------------------------------------------
      CALL DISMOI('F','NB_NO_MAILLA',MO,'MODELE',NM,KBID,IER)
C
C---- VERIF DE MOTCLE: SI ZERO ON ECRASE SI CUMU ON CUMULE
C
      IF (MOTCLE(1:4).EQ.'ZERO') THEN
        CUMUL = .FALSE.
      ELSE IF (MOTCLE(1:4).EQ.'CUMU') THEN
        CUMUL = .TRUE.
      ELSE
        CALL UTMESS('F','ASSMNS',' LE PARAMETRE : '//MOTCLE//
     +              ' EST INCORRECT. ON ATTEND : "CUMU" OU "ZERO" ')
      END IF
C
C --- SI LE CONCEPT MATAS EXISTE, MAIS QUE LA NUMEROTATION SUPPORT EST
C --- CORRECTE ET QUE LE STOCKAGE EST IDENTIQUE ALORS ON CONSERVE LES
C     OBJETS QUI SONT A LA BONNE DIMENSION:
C     (SAUF .LIME QUE L'ON DETRUIT TOUT LE TEMPS)
C     ET .CONI,.VALI,.LLIG,.ALIG ,.ABLI QUI DOIVENT ETRE DETRUITS
      ACREER = .TRUE.
      CALL JEEXIN(MATDEV//'.REFA',IRET)
      IF (IRET.NE.0) THEN
        CALL JEVEUO(KMAREF,'L',IDMARF)
        IF ((ZK24(IDMARF-1+2)(1:14).EQ.NUDEV).AND.
     +     (ZK24(IDMARF-1+3)(17:18).EQ.'LC')) THEN
          ACREER = .FALSE.
          CALL JEDETR(KMAREF(1:19)//'.LIME')
          CALL JEDETR(MATDEV(1:19)//'.CONI')
          CALL JEDETR(MATDEV(1:19)//'.ABLI')
          CALL JEDETR(MATDEV(1:19)//'.LLIG')
          CALL JEDETR(MATDEV(1:19)//'.ALIG')
          CALL JEDETR(MATDEV(1:19)//'.VALI')
        ELSE
          CALL DETRSD('MATR_ASSE',MATDEV)
        END IF
      END IF
C
C
C---- RECOPIE DE LA LISTE DES MATR_ELEM DANS 1 OBJET JEVEUX DONT ON
C     GARDE L'ADRESSE DANS LE COMMON /CADMAT/
C
      CALL WKVECT(MATDEV//'.LIME',BAS2//' V K8 ',NBMAT,ILIMAT)
      DO 5 I = 1,NBMAT
        ZK8(ILIMAT+I-1) = TLIMAT(I)
    5 CONTINUE
C
      K5BID(1:5) = '.SLCS'
      CALL JEVEUO(NUDEV(1:14)//'.NUME.NUEQ','L',IANUEQ)
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
C
C---- CALCUL D UN REPERTOIRE,TEMPORAIRE, MATDEV.LILI A PARTIR DE LA
C     LISTE DE MATRICES ELEMENTAIRES MATDEV.LIME
C
      CALL CRELIL(NBMAT,ILIMAT,KMALIL,'V',KMAILL,MATDEV,
     +            GD,MA,NEC,NCMP,ILIMO,NMALIL,NBELM)
       CALL JEVEUO(MATDEV(1:19)//'.ADLI','E',IADLIE)
       CALL JEVEUO(MATDEV(1:19)//'.ADNE','E',IADNEM)
       CALL JEEXIN(MA(1:8)//'.CONNEX',IRET)
       IF (IRET.GT.0) THEN
         CALL JEVEUO(MA(1:8)//'.CONNEX','L',ICONX1)
         CALL JEVEUO(JEXATR(MA(1:8)//'.CONNEX','LONCUM'),'L',ICONX2)
       END IF
      CALL DISMOI('F','NUM_GD_SI',NOGDSI,'GRANDEUR',NUGD,KBID,IERD)
      NEC  = NBEC(NUGD)
      NCMP = NMXCMP
C
C --- BLOC D'INSTRUCTIONS DEPLACE :
C     ---------------------------
      CALL JEEXIN(MA//'.NOMACR',IRET)
      IF (IRET.GT.0) CALL JEVEUO(MA//'.NOMACR','L',IANMCR)
      CALL JEVEUO(JEXNOM('&CATA.GD.NOMCMP',NOGDSI),'L',IANCMP)
      CALL JELIRA(JEXNOM('&CATA.GD.NOMCMP',NOGDSI),'LONMAX',
     &            LGNCMP,KBID)
      ICMP = INDIK8(ZK8(IANCMP),'LAGR',1,LGNCMP)
C      IF (ICMP.EQ.0) CALL UTMESS('F','ASSMAT','ON NE TROUVE PAS'
C     +                 //' LE CMP "LAGR" DANS LA GRANDEUR')
C -- AJOUT ET MODIF :
C    ==============
C      IF (ICMP.GT.30) CALL UTMESS('F','ASSMAT','IL EST IMPREVU '
C     +                 //'D AVOIR LE CMP "LAGR" AU DELA DE 30')
C     -- ICODLA EST L'ENTIER CODE CORRESPONDANT A LA CMP "LAGR"
C
      JEC = (ICMP-1)/30 + 1
      ICODLA(JEC) = 2**ICMP
C
C---- ILIMO = NUMERO DU 1ER LIGREL DE MODELE DE KMALIL
C
C---- ON SUPPOSE QUE LE NOM '&MAILLA' EST LE PREMIER DU REPERTOIRE
C     NU.LILI CE QUI EST VRAI CF S.P. CRELIL
C
      ILIMNU = 1
C
C---- NOMS DES PRINCIPAUX OBJETS JEVEUX LIES A NU
C
      K24PRN = NUDEV//'.NUME.PRNO'
      KNULIL = NUDEV//'.NUME.LILI'
C
C---- RECUPERATION DE PRNO
C
      CALL JEVEUO(K24PRN,'L',IDPRN1)
      CALL JEVEUO(JEXATR(K24PRN,'LONCUM'),'L',IDPRN2)
C
C---- CREATION ET REMPLISSAGE DE REFE
C
      IF (ACREER) CALL WKVECT(KMAREF,BAS2//' V K24',4,IDMARF)
      CALL JEVEUO(KMAREF,'E',IDMARF)
      CALL JEECRA(KMAREF,'DOCU',IBID,'ASSE')
      ZK24(IDMARF) = MA
      ZK24(IDMARF+1) = NUDEV(1:14)//'.NUME'
      ZK24(IDMARF+2) = NUDEV(1:14)//K5BID(1:5)
C
C---- CREATION DE 2 OBJETS VOLATILES POUR ACCELERER ASCOPR:
C
C---- TMP1 : (1:NBLC) INDIQUE LE NOMBRE DE REELS D'1 MATRICE ELEMENTAIRE
C            S'INJECTANT DANS LE BLOC I.
C---- TMP2 : (1:2*DIM(MATRICE ELEMENTAIRE))
C            POSITION RELATIVE DANS LES BLOCS
C            POUR LE I-EME REEL DE LA MATRICE ELEM :
C     TMP2(2*(I-1)+1) --> NUMERO DU BLOC OU S'INJECTE I.
C     TMP2(2*(I-1)+2) --> POSITION DANS LE BLOC DU REEL I.
C
C     -- L'OBJET .TMP1 EST CREE TOUT DE SUITE
C     -- L'OBJET .TMP2 SERA CREE DANS LA BOUCLE 530
C
      KTMP1 = KVALE(1:19)//'.TMP1           '
      KTMP2 = KVALE(1:19)//'.TMP2           '
      CALL JEVEUO(KDESC,'L',IDDESC)
      NEQU = ZI(IDDESC)
      ITBLOC = ZI(IDDESC+1)
      NBLC = ZI(IDDESC+2)
C
C- CAS NON_SYMETRIQUE : ON DOUBLE LE NOMBRE DE BLOCS QUI A ETE DEFINI
C-                      DANS PROLCI POUR DES MATRICES SYMETRIQUES
C
      NBLOC = NBLC
      NBLC = NBLC + NBLC
C
      CALL WKVECT(KTMP1,' V V I',NBLC,IATMP1)
C
C---- ALLOCATION VALE EN R OU C SUIVANT TYPE
C
      IF (ACREER) THEN
        IF (TYPE.EQ.1) THEN
          CALL JECREC(KVALE,BAS2//' V R','NU','DISPERSE','CONSTANT',
     +                NBLC)
        ELSE IF (TYPE.EQ.2) THEN
          CALL JECREC(KVALE,BAS2//' V C','NU','DISPERSE','CONSTANT',
     +                NBLC)
        ELSE
          CALL UTMESS('F','ASSMNS',' ON NE PEUT ASSEMBLER QUE DES'//
     +                    ' MATRICES REELLES OU COMPLEXES')
        END IF
        CALL JEECRA(KVALE,'LONMAX',ITBLOC,' ')
        CALL JEECRA(KVALE,'DOCU',IBID,'MR')
        DO 123 I = 1,NBLC
          CALL JECROC(JEXNUM(KVALE,I))
          CALL JEVEUO(JEXNUM(KVALE,I),'E',IABIDO)
          CALL JELIBE(JEXNUM(KVALE,I))
  123   CONTINUE
      ELSE
        IF (.NOT.CUMUL) THEN
C        -- REMISE A ZERO DE .VALE :
          DO 124 I = 1,NBLC
            CALL JERAZO(JEXNUM(KVALE,I),ITBLOC,1)
            CALL JELIBE(JEXNUM(KVALE,I))
  124     CONTINUE
        END IF
      END IF
C
C---- CALCUL (REMPLISSAGE) DE VALE
C     ON COMMENCE PAR ASSEMBLER SUR LE MODELE
      IMO = 1
      RINF = R8MAEM()
      RSUP = -1.D0
      COEF = 1.D0
   50 CONTINUE


      ILONG = 0
      DO 500 IMAT = 1,NBMAT
        MATEL = ZK8(ILIMAT+IMAT-1)
        CALL DISMOI('F','NOM_MODELE',MATEL,'MATR_ELEM',IBID,MO2,IERD)
        CALL DISMOI('F','SUR_OPTION',MATEL,'MATR_ELEM',IBID,OPTIO,IERD)
        IF (MO2.NE.MO) CALL UTMESS('F','ASSMNS','MODELES DISCORDANTS')

C       -- ON REGARDE SI TOUS LES MATR_ELEM ONT ETE CALCULES AVEC LA
C          MEME SUR_OPTION :
        IF (IMAT.EQ.1) THEN
           OPTIO2=OPTIO
        ELSE
           IF (OPTIO2.NE.OPTIO) OPTIO2='&&MELANGE'
        ENDIF

      IF (IMO.NE.1) GOTO 501
C
C         -- TRAITEMENT DES SOUS-STRUCTURES (JUSQU A FIN BOUCLE 739)
C         ----------------------------------------------------------
        CALL DISMOI('F','EXI_ELEM',MO,'MODELE',IBID,EXIELE,IERD)
        CALL DISMOI('F','NB_SS_ACTI',MATEL,'MATR_ELEM',NBSSA,
     +              KBID,IERD)
C
      IF (NBSSA.GT.0) THEN
C
C        IF (NEC.GT.1) CALL UTMESS('F','ASSMNS','NEC DOIT ETRE =1')
        CALL DISMOI('F','NB_SM_MAILLA',MO,'MODELE',NBSMA,KBID,IERD)
        CALL DISMOI('F','NOM_MAILLA',MO,'MODELE',IBID,MA,IERD)
        CALL JEVEUO(MO//'.SSSA','L',IASSSA)
C
        CALL SSVALM('DEBUT',OPTIO,MO,MA,IMA,IDRESL,NCMPEL)
C
        DO 739, IMA = 1, NBSMA
C---- ON ASSEMBLE QUE LES SSS ACTIVES POUR LA MATRICE ELEMENTAIRE
          IF(ZI(IASSSA-1+IMA).EQ.0) GO TO 739
C
          CALL JEVEUO(JEXNUM(MA//'.SUPMAIL',IMA),'L',IAMAIL)
          CALL JELIRA(JEXNUM(MA//'.SUPMAIL',IMA),'LONMAX',
     +                     NNOE,KBID)
C
          IREEL = 0
          R = LICOEF(IMAT)
C
          CALL SSVALM(' ',OPTIO,MO,MA,IMA,IDRESL,NCMPEL)
          IF ( NCMPEL.GT.ILONG) THEN
            ILONG = NCMPEL
            CALL JEDETR(KTMP2)
            CALL WKVECT(KTMP2,' V V I',2*ILONG,IATMP2)
          ENDIF
C
          CALL JEVEUO(JEXNUM(MA//'.SUPMAIL',IMA),'L',IAMAIL)
          CALL JELIRA(JEXNUM(MA//'.SUPMAIL',IMA),'LONMAX',NNOE,KBID)
C
          NOMACR= ZK8(IANMCR-1+IMA)
          CALL DISMOI('F','NOM_NUME_DDL',NOMACR,'MACR_ELEM_STAT',
     +                 IBID,NUM2,IERD)
          CALL JEVEUO(NOMACR//'.CONX','L',IACONX)
          CALL JEVEUO(JEXNUM(NUM2//'.NUME.PRNO',1),'L',IAPROL)
C
          DO 740 K1 = 1,NNOE
            N1=ZI(IAMAIL-1+K1)
            IF (N1.GT.NM) THEN
              DO 2 IEC = 1, NBECMX
                ICODGE(IEC) = ICODLA(IEC)
 2            CONTINUE
            ELSE
              INOLD= ZI(IACONX-1+3*(K1-1)+2)
              DO 3 IEC = 1, NEC
                ICODGE(IEC)= ZI(IAPROL-1+(NEC+2)* (INOLD-1)+2+IEC)
 3            CONTINUE
            END IF
C
            IAD1 = ZZPRNO(ILIMNU,N1,1)
            CALL CORDD2(IDPRN1,IDPRN2,ILIMNU,ICODGE,NEC,NCMP,N1,
     +                 K1,NDDL1,ZI(IAPSDL-1+NMXCMP*(K1-1)+1))
            ZI(IANULO-4+3*K1+1) = N1
            ZI(IANULO-4+3*K1+2) = IAD1
            ZI(IANULO-4+3*K1+3) = NDDL1
            DO 741 I1 = 1,NDDL1
              DO 742 K2 = 1,K1 - 1
                IAD2 = ZI(IANULO-4+3*K2+2)
                NDDL2 = ZI(IANULO-4+3*K2+3)
                DO 743 I2 = 1,NDDL2
                  IAD11 = ZI(IANUEQ-1+IAD1+
     +                    ZI(IAPSDL-1+NMXCMP*(K1-1)+I1)-1)
                  IAD21 = ZI(IANUEQ-1+IAD2+
     +                    ZI(IAPSDL-1+NMXCMP*(K2-1)+I2)-1)
                  IF (IAD11.LE.IAD21) THEN
                    IADLI = IAD11
                    IADCO = IAD21
                  ELSE
                    IADLI = IAD21
                    IADCO = IAD11
                  END IF
C
C-           NUMBL EST LE NUMERO DU BLOC SUP
C
                  NUMBL = ZI(IDIABL-1+IADCO)

C
C-           INCREMENTATION DU NOMBRE DE TERMES DANS LE BLOC INF
C

                  ZI(IATMP1-1+NUMBL)=ZI(IATMP1-1+NUMBL)+1

C
C-           STOCKAGE DU NUMERO DE BLOC AUQUEL APPARTIENT LE
C-           TERME COURANT DE LA MATRICE ELEMENTAIRE ET DE SA
C-           LOCALISATION DANS CE BLOC
C
                 CALL ASRETI(IATMP2,IREEL,IDHCOL,IDADIA,
     +                      IDABLO,IADLI,IADCO,NUMBL)
 743            CONTINUE
 742          CONTINUE
              K2 = K1
              IAD2 = ZI(IANULO-4+3*K2+2)
              NDDL2 = ZI(IANULO-4+3*K2+3)
              DO 745 I2 = 1,I1
                IAD11 = ZI(IANUEQ-1+IAD1+
     +          ZI(IAPSDL-1+NMXCMP*(K1-1)+I1)-1)
                IAD21 = ZI(IANUEQ-1+IAD2+
     +          ZI(IAPSDL-1+NMXCMP*(K2-1)+I2)-1)
                IF (IAD11.LE.IAD21) THEN
                  IADLI = IAD11
                  IADCO = IAD21
                ELSE
                  IADLI = IAD21
                  IADCO = IAD11
                END IF
C
C-           NUMBL EST LE NUMERO DU BLOC SUP
C
                NUMBL = ZI(IDIABL-1+IADCO)

C
C-           INCREMENTATION DU NOMBRE DE TERMES DANS LE BLOC SUP
C

                ZI(IATMP1-1+NUMBL)=ZI(IATMP1-1+NUMBL)+1
C
C-           STOCKAGE DU NUMERO DE BLOC AUQUEL APPARTIENT LE
C-           TERME COURANT DE LA MATRICE ELEMENTAIRE ET DE SA
C-           LOCALISATION DANS CE BLOC
C
               CALL ASRETI(IATMP2,IREEL,IDHCOL,IDADIA,
     +                      IDABLO,IADLI,IADCO,NUMBL)
 745          CONTINUE
 741        CONTINUE
 740      CONTINUE
C         ---- POUR FINIR, ON RECOPIE EFFECTIVEMENT LES TERMES:
C             (IREEL CONTIENT LE NOMBRE DE REELS A TRAITER)
          IF (TYPE.EQ.1) THEN
                CALL ASCNPR(IATMP1,IATMP2,IREEL,IDRESL,NBLOC,KVALE,R)
          ELSE IF (TYPE.EQ.2) THEN
                CALL ASCOPC(IATMP1,IATMP2,IREEL,IDRESL,NBLOC,
     &               KVALE,R,0,0)
          ENDIF

 739    CONTINUE
        CALL SSVALM('FIN',OPTIO,MO,MA,IMA,IDRESL,NCMPEL)
      END IF
501   CONTINUE
C
C
C         -- TRAITEMENT DES ELEMENTS FINIS CLASSIQUES (FIN BOUCLE 510)
C         -----------------------------------------------------------
      CALL JEEXIN(MATEL//'.LISTE_RESU',IRET)
      IF (IRET.GT.0) THEN
C
C
C---- BOUCLE SUR LES MATR_ELEM:
        CALL JEVEUO(MATEL//'.LISTE_RESU','L',IDLRES)
        CALL JELIRA(MATEL//'.LISTE_RESU','LONUTI ',NBRESU,K1BID)
        DO 510 IRESU = 1,NBRESU
C
C---- BOUCLE SUR LES CHAMPS DES RESU_ELEM
C
          RESU = ZK24(IDLRES+IRESU-1)
          CALL JEEXIN(RESU(1:19)//'.DESC',IER)
          IF (IER.EQ.0) GO TO 510
          TYMAT = 'S'
          RESUL = RESU
          CALL DISMOI('F','TYPE_MATRICE',RESUL,'RESUELEM',IBID,SYM,IERD)
          IF (SYM.EQ.'NON_SYM') TYMAT ='N'
          CALL JEVEUO(RESU(1:19)//'.NOLI','L',IAD)
          NOMLI = ZK24(IAD)
          CALL JENONU(JEXNOM(KMALIL,NOMLI),ILIMA)
          CALL JENONU(JEXNOM(KNULIL,NOMLI),ILINU)
          IF ((IMO.EQ.1) .AND. (ILIMA.NE.ILIMO)) GO TO 510
          IF ((IMO.EQ.0) .AND. (ILIMA.EQ.ILIMO)) GO TO 510
          DO 520 IGR = 1,ZZNGEL(ILIMA)
C
C---- BOUCLE SUR LES GROUPES D'ELEMENTS
C
            CALL JEVEUO(RESU(1:19)//'.DESC','L',IADESC)
            MODE = ZI(IADESC+IGR+1)
            IF (MODE.GT.0) THEN
              NNOE = NBNO(MODE)
              NEL = ZZNELG(ILIMA,IGR)
              CALL JEVEUO(JEXNUM(RESU(1:19)//'.RESL',IGR),
     +                      'L',IDRESL)
              NCMPEL= DIGDEL(MODE)
              IF ( NCMPEL.GT.ILONG) THEN
                ILONG = NCMPEL
                CALL JEDETR(KTMP2)
                CALL WKVECT(KTMP2,' V V I',2*ILONG,IATMP2)
              ENDIF
              IF (TYMAT.EQ.'N') THEN
              DO 830 IEL = 1,NEL
C
C---- BOUCLE SUR LES ELEMENTS D'UN GROUPE D'ELEMENT
C
                IREEL = 0
                ILAGR = 0
C     R = COEFF MULTIPLICATEUR
                R = LICOEF(IMAT)
                NUMA = ZZLIEL(ILIMA,IGR,IEL)
                IF (NUMA.GT.0) THEN
C
C- CHARGEMENT DE ZI(IANUL0)
C
                  DO 839 K1 = 1,NNOE
                    N1 = ZZCONX(NUMA,K1)
                    IAD1 = ZZPRNO(ILIMNU,N1,1)
                    CALL CORDDL(IDPRN1,IDPRN2,ILIMNU,MODE,NEC,NCMP,N1,
     +         K1,NDDL1,ZI(IAPSDL-1+NMXCMP*(K1-1)+1))
                    ZI(IANULO-4+3*K1+1) = N1
                    ZI(IANULO-4+3*K1+2) = IAD1
                    ZI(IANULO-4+3*K1+3) = NDDL1
  839             CONTINUE
C
C- PREMIERE BOUCLE NOEUDS
C
                  DO 840 K1 = 1,NNOE
                    N1 = ZI(IANULO-4+3*K1+1)
                    IAD1 = ZI(IANULO-4+3*K1+2)
                    NDDL1 = ZI(IANULO-4+3*K1+3)
C
C-           PARCOURS DES DDL DU NOEUD 1
C
                    DO 841 I1 = 1,NDDL1
                        IAD11 = ZI(IANUEQ-1+IAD1+
     +                  ZI(IAPSDL-1+NMXCMP*(K1-1)+I1)-1)
C
C-      DEUXIEME BOUCLE NOEUDS
C
                      DO 842 K2 = 1,NNOE
                        IAD2 = ZI(IANULO-4+3*K2+2)
                        NDDL2 = ZI(IANULO-4+3*K2+3)
C
C-                 PARCOURS DES DDL DU NOEUD 2
C
                        DO 843 I2 = 1,NDDL2
                          IAD21 = ZI(IANUEQ-1+IAD2+
     +                  ZI(IAPSDL-1+NMXCMP*(K2-1)+I2)-1)
C
C-                     ASSEMBLAGE DANS LE BLOC INFERIEUR DONT LE NUMERO
C-                     EST DONNE PAR IABL(IAD11)
C
                          IF (IAD11.GE.IAD21) THEN
                            IADCO = IAD11
                            IADLI = IAD21
                            INDBLO = 1
C
C-                     ASSEMBLAGE DANS LE BLOC SUPERIEUR DONT LE NUMERO
C-                     EST DONNE PAR IABL(IAD21)
C
                          ELSE
                            IADCO = IAD21
                            IADLI = IAD11
                            INDBLO = 0
                          END IF
C
C-                     RECUPERATION DU NUMERO DU BLOC QUE L'ON VA
C-                     REMPLIR SELON LA NUMEROTATION DEFINIE DANS
C-                     PROLCI
C
                          NUMBL = ZI(IDIABL-1+IADCO)
C
C-                     ON PREND LA CONVENTION D'AVOIR TOUS LES BLOCS
C-                     SUP PUIS TOUS LES BLOCS INF
C
                          NUMBLV = NUMBL+INDBLO*NBLOC
C
C-                     INCREMENTATION DU COMPTEUR DU NOMBRE DE TERMES
C-                     DANS LE BLOC NUMBLV
C
                          ZI(IATMP1-1+NUMBLV)=ZI(IATMP1-1+NUMBLV)+1
C
C-                     STOCKAGE DU NUMERO DE BLOC AUQUEL APPARTIENT LE
C-                     TERME COURANT DE LA MATRICE ELEMENTAIRE ET DE SA
C-                     LOCALISATION DANS CE BLOC
C
                          CALL ASRETJ(IATMP2,IREEL,IDHCOL,IDADIA,
     +                                IDABLO,IADLI,IADCO,NUMBL,NUMBLV)
  843                   CONTINUE
  842                 CONTINUE
  841               CONTINUE
  840             CONTINUE
                ENDIF
C         ---- POUR FINIR, ON RECOPIE EFFECTIVEMENT LES TERMES:
C             (IREEL CONTIENT LE NOMBRE DE REELS A TRAITER)
          IF (TYPE.EQ.1) THEN
                CALL ASCOPR(IATMP1,IATMP2,IREEL,
     +                   IDRESL+NCMPEL*(IEL-1),NBLC,KVALE,R,0,0)
          ELSE IF (TYPE.EQ.2) THEN
                CALL ASCOPC(IATMP1,IATMP2,IREEL,
     +                   IDRESL+NCMPEL*(IEL-1),NBLC,KVALE,R,0,0)
          ENDIF
  830         CONTINUE
C
C --- ON AFFECTE AUX TERMES DIAGONAUX DU BLOC SUPERIEUR LES VALEURS
C --- DES TERMES DIAGONAUX DU BLOC INFERIEUR (PAR SOUCI DE COHERENCE
C --- VIS-A-VIS DU TRAITEMENT DES TERMES DIAGONAUX POUR LES
C --- MATRICES ELEMENTAIRES SYMETRIQUES) :
C     ----------------------------------
        DO 844 IBLC = 1,NBLOC
          CALL JEVEUO(JEXNUM(KVALE,IBLC+NBLOC),'L',IDIBLI)
          CALL JEVEUO(JEXNUM(KVALE,IBLC),'E',IDIBLS)
          DO 845 I = ZI(IDABLO+IBLC-1) + 1,ZI(IDABLO+IBLC)
            IDI = ZI(IDADIA+I-1)
            ZR(IDIBLS+IDI-1) = ZR(IDIBLI+IDI-1)
 845     CONTINUE
          CALL JELIBE(JEXNUM(KVALE,IBLC+NBLOC))
          CALL JELIBE(JEXNUM(KVALE,IBLC))
 844     CONTINUE
C
              ELSE IF (TYMAT.EQ.'S') THEN
              DO 530 IEL = 1,NEL
C
C---- BOUCLE SUR LES ELEMENTS D'UN GROUPE D'ELEMENT
C
                IREEL = 0
                ILAGR = 0
C     R = COEFF MULTIPLICATEUR
                R = LICOEF(IMAT)
                NUMA = ZZLIEL(ILIMA,IGR,IEL)
                IF (NUMA.GT.0) THEN
                  DO 540 K1 = 1,NNOE
                    N1 = ZZCONX(NUMA,K1)
                    IAD1 = ZZPRNO(ILIMNU,N1,1)
                    CALL CORDDL(IDPRN1,IDPRN2,ILIMNU,MODE,NEC,NCMP,N1,
     +         K1,NDDL1,ZI(IAPSDL-1+NMXCMP*(K1-1)+1))
                    ZI(IANULO-4+3*K1+1) = N1
                    ZI(IANULO-4+3*K1+2) = IAD1
                    ZI(IANULO-4+3*K1+3) = NDDL1
                    DO 541 I1 = 1,NDDL1
                      DO 542 K2 = 1,K1 - 1
                        IAD2 = ZI(IANULO-4+3*K2+2)
                        NDDL2 = ZI(IANULO-4+3*K2+3)
                        DO 543 I2 = 1,NDDL2
                          IAD11 = ZI(IANUEQ-1+IAD1+
     +                  ZI(IAPSDL-1+NMXCMP*(K1-1)+I1)-1)
                          IAD21 = ZI(IANUEQ-1+IAD2+
     +                  ZI(IAPSDL-1+NMXCMP*(K2-1)+I2)-1)
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
     +                                IDABLO,IADLI,IADCO,NUMBL)
  543                   CONTINUE
  542                 CONTINUE
                      K2 = K1
                      IAD2 = ZI(IANULO-4+3*K2+2)
                      NDDL2 = ZI(IANULO-4+3*K2+3)
                      DO 545 I2 = 1,I1
                        IAD11 = ZI(IANUEQ-1+IAD1+
     +                  ZI(IAPSDL-1+NMXCMP*(K1-1)+I1)-1)
                        IAD21 = ZI(IANUEQ-1+IAD2+
     +                  ZI(IAPSDL-1+NMXCMP*(K2-1)+I2)-1)
                        IF (IAD11.LE.IAD21) THEN
                          IADLI = IAD11
                          IADCO = IAD21
                        ELSE
                          IADLI = IAD21
                          IADCO = IAD11
                        END IF
                        NUMBL = ZI(IDIABL-1+IADCO)
                        ZI(IATMP1-1+NUMBL) = ZI(IATMP1- 1+NUMBL) + 1
                        CALL ASRETI(IATMP2,IREEL,IDHCOL,IDADIA,
     +                              IDABLO,IADLI,IADCO,NUMBL)
  545                 CONTINUE
  541               CONTINUE
  540             CONTINUE
                ELSE
C---- CONDITIONNEMENT DES LAGRANGE SI OPTION RIGI_XXXX NO
C---- SI MAILLE A 3 NOEUDS          -        - ILAGR = 1 POSSIBILITE DE
C----      ET                       ! ALORS  ! NOEUDS LAGRANGE DANS LA
C---- SI ON N'EST PAS SUR LE MODELE -        - MAILLE
C----
C---- SI ILAGR=1 ET SI LA MAILLE CONTIENT UN NOEUD A UN DDL
C---- ALORS ON EST SUR UNE MAILLE TARDIVE DE "LAGRANGE"
                  IF ((IMO.EQ.0) .AND.(NNOE.EQ.3)) ILAGR = 1
                  NUMA = -NUMA
                  NSUP = ZZNSUP(ILIMA,NUMA)
                  IF (NSUP.NE.NNOE) THEN
                    CALL UTDEBM('F','ASSMNS','GROSSE ERREUR')
                    CALL UTIMPI('L','LE NBRE DE NOEUDS DE LA '
     +                           //' MAILLE SUP : ',1,NUMA)
                    CALL UTIMPI('S','DU LIGREL : ',1,NNOLI)
                    CALL UTIMPK('L','EST DIFFERENT DE CELUI '
     +                           //'ASSOCIE AU RESUELEM : ',1,RESU)
                    CALL UTFINM()
                  END IF
                  DO 560 K1 = 1,NNOE
                    N1 = ZZNEMA(ILIMA,NUMA,K1)
                    IF (N1.LT.0) THEN
                      N1 = -N1
                      IAD1 = ZZPRNO(ILINU,N1,1)
                      CALL CORDDL(IDPRN1,IDPRN2,ILINU,MODE,NEC,NCMP,
     +                            N1,K1,NDDL1,
     +                  ZI(IAPSDL-1+NMXCMP*(K1-1)+1))
C
C---- SI NOEUD LAGR ( NNOE=3,NDDL1=1,N1<0,NUMA<0 ) ALORS CONL(IAD1)=R
C---- ET POUR TOUTE LA MATRICE ELEMENTAIRE ON POSE R = COEF*LICOEF(IMAT)
                      IF ((ILAGR.EQ.1) .AND.(NDDL1.EQ.1)) THEN
                        R = COEF*LICOEF(IMAT)
                        ZR(IDCONL-1+ZI(IANUEQ-1+IAD1)) = R
                      END IF
                    ELSE
                      IAD1 = ZZPRNO(ILIMNU,N1,1)
                      CALL CORDDL(IDPRN1,IDPRN2,ILIMNU,MODE,NEC,
     +                            NCMP,N1,K1,NDDL1,
     +                  ZI(IAPSDL-1+NMXCMP*(K1-1)+1))
                    END IF
                    ZI(IANULO-4+3*K1+1) = N1
                    ZI(IANULO-4+3*K1+2) = IAD1
                    ZI(IANULO-4+3*K1+3) = NDDL1
                    DO 561 I1 = 1,NDDL1
                      DO 562 K2 = 1,K1 - 1
                        IAD2 = ZI(IANULO-4+3*K2+2)
                        NDDL2 = ZI(IANULO-4+3*K2+3)
                        DO 563 I2 = 1,NDDL2
                          IAD11 = ZI(IANUEQ-1+IAD1+
     +                  ZI(IAPSDL-1+NMXCMP*(K1-1)+I1)-1)
                          IAD21 = ZI(IANUEQ-1+IAD2+
     +                  ZI(IAPSDL-1+NMXCMP*(K2-1)+I2)-1)
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
     +                                IDABLO,IADLI,IADCO,NUMBL)
  563                   CONTINUE
  562                 CONTINUE
                      K2 = K1
                      IAD2 = ZI(IANULO-4+3*K2+2)
                      NDDL2 = ZI(IANULO-4+3*K2+3)
                      DO 565 I2 = 1,I1
                        IAD11 = ZI(IANUEQ-1+IAD1+
     +                  ZI(IAPSDL-1+NMXCMP*(K1-1)+I1)-1)
                        IAD21 = ZI(IANUEQ-1+IAD2+
     +                  ZI(IAPSDL-1+NMXCMP*(K2-1)+I2)-1)
                        IF (IAD11.LE.IAD21) THEN
                          IADLI = IAD11
                          IADCO = IAD21
                        ELSE
                          IADLI = IAD21
                          IADCO = IAD11
                        END IF
                        NUMBL = ZI(IDIABL-1+IADCO)
                        ZI(IATMP1-1+NUMBL) = ZI(IATMP1- 1+NUMBL) + 1
                        CALL ASRETI(IATMP2,IREEL,IDHCOL,IDADIA,
     +                              IDABLO,IADLI,IADCO,NUMBL)
  565                 CONTINUE
  561               CONTINUE
  560             CONTINUE
                END IF
C---- POUR FINIR, ON RECOPIE EFFECTIVEMENT LES TERMES:
C     (IREEL CONTIENT LE NOMBRE DE REELS A TRAITER)
               IF (TYPE.EQ.1) CALL ASCNPR(IATMP1,IATMP2,IREEL,
     +                   IDRESL+NCMPEL*(IEL-1),NBLOC,KVALE,R)
                IF (TYPE.EQ.2) CALL ASCOPC(IATMP1,IATMP2,IREEL,
     +                   IDRESL+NCMPEL*(IEL-1),NBLOC,KVALE,R,0,0)
  530         CONTINUE
              END IF
              CALL JELIBE(JEXNUM(RESU(1:19)//'.RESL',IGR))
            END IF
  520     CONTINUE
  510   CONTINUE
      END IF
 500  CONTINUE
      IF (IMO.EQ.1) THEN
C
C        --- SI ON TRAITE LE MODELE ---
        DO 700 IBLC = 1,NBLOC
          CALL JEVEUO(JEXNUM(KVALE,IBLC+NBLOC),'L',IDIBLC)
          DO 710 I = ZI(IDABLO+IBLC-1) + 1,ZI(IDABLO+IBLC)
            IDI = ZI(IDADIA+I-1)
            IF (TYPE.EQ.1) R = ABS(ZR(IDIBLC+IDI-1))
            IF (TYPE.EQ.2) R = ABS(ZC(IDIBLC+IDI-1))
            IF ((R.NE.0.D0) .AND. (R.LT.RINF)) RINF = R
            IF ((R.NE.0.D0) .AND. (R.GT.RSUP)) RSUP = R
  710     CONTINUE
          CALL JELIBE(JEXNUM(KVALE,IBLC+NBLOC))
  700   CONTINUE
        COEF = (RSUP+RINF)/2.D0
        IMO = 0
C---- CREATION DE L'OBJET .CONL EN OPTION RIGI SI AU MOINS UNE CHARGE
        IF (NMALIL.GT.2) THEN
          KCONL = MATDEV(1:19)//'.CONL'
          IF (ACREER) CALL WKVECT(KCONL,BAS2//' V R',NEQU,IBID)
          CALL JEVEUO(KCONL,'E',IDCONL)
          DO 800 I = 1,NEQU
            ZR(IDCONL-1+I) = 1.D0
  800     CONTINUE
          IF (NIV.EQ.2) THEN
            WRITE(IFM,*)'COEFFICIENT DE CONDITIONNEMENT'//
     *      ' DES LAGRANGES :',COEF
          END IF
        END IF
        GO TO 50
      END IF

C     -- MISE A JOUR DE REFA(4)
      CALL JEVEUO(KMAREF,'E',IDMARF)
      IF (MOTCLE(1:4).EQ.'ZERO') THEN
        ZK24(IDMARF-1+4) = OPTIO2
      ELSE
        IF (ZK24(IDMARF-1+4).NE.OPTIO2) ZK24(IDMARF-1+4)='&&MELANGE'
      END IF
C
      CALL JEDETR('&&ASSMNS.NUMLOC')
      CALL JEDETR('&&ASSMNS.POSDDL')
      CALL JEDETR(KTMP1)
      CALL JEDETR(KTMP2)
      CALL JEDETR(MATDEV//'.ADNE')
      CALL JEDETR(MATDEV//'.ADLI')
      CALL JEDETR(KVALE(1:19)//'.TMP1')
      CALL JEDETR(KVALE(1:19)//'.TMP2')
C
      CALL JEDBG2(IBID,IDBGAV)
      CALL JEDEMA()
      END
