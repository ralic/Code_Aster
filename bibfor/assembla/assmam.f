      SUBROUTINE ASSMAM(BASE,MATAS,NBMAT,TLIMAT,LICOEF,NU,MOTCLE,TYPE)

C  ATTENTION : CETTE ROUTINE NE DOIT PAS ETRE APPELLEE DIRECTEMENT :
C              IL FAUT APPELER SON "CHAPEAU" : ASMATR.

      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*) BASE,MATAS,TLIMAT(*),NU
      INTEGER NBMAT,TYPE
      REAL*8 LICOEF(*)
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
C     ASSEMBLAGE MORSE AVEC PRECONDITIONNEMENT DES MATR_ELEM DE MAILLES
C     "LAGRANGE" PAR -(MAX(!A(I,I)!)+MIN(!A(I,I)!))/2
C-----------------------------------------------------------------------
C --- DESCRIPTION DES PARAMETRES
C INT K* BASE   : BASE SUR LAQUELLE ON VEUT CREER LA MATR_ASSE
C OUT K* MATAS  :L'OBJET MATAS DE TYPE MATR_ASSE EST CREE ET REMPLI
C IN  K* MATAS  : NOM DE L'OBJET DE TYPE MATR_ASSE A CREER
C IN  I  NBMAT  : NOMBRE DE MAT_ELE  DE LA LISTE TLIMAT
C IN  K* TLIMAT : LISTE DES MAT_ELE
C IN  I  LICOEF : LISTE DES COEFFICIENTS MULTIPLICATEURS DES MAT_ELE
C IN  K* NU     : NOM DU NUMERO_DDL
C IN  K4 MOTCLE : 'ZERO' OU 'CUMU'
C                 'ZERO':SI UN OBJET DE NOM MATAS ET DE TYPE
C                        MATR_ASSE EXISTE ON L'ECRASE
C                 'CUMU':SI UN OBJET DE NOM MATAS ET DE TYPE
C                        MATR_ASSE EXISTE ON L'ENRICHI
C IN  I   TYPE  : TYPE DES MATRICES ELEMENTAIRES A ASSMBLEES
C                          1 --> REELLES
C                          2 --> COMPLEXES
C
C  SI IL EXISTE UN OBJET '&&POIDS_MAILLE' VR CONTENANT
C  DES PONDERATIONS POUR CHAQUE MAILLE, ON S'EN SERT.
C
C ----------------------------------------------------------------------
C-----------------------------------------------------------------------
C     FONCTIONS JEVEUX
C-----------------------------------------------------------------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR
C-----------------------------------------------------------------------
C     COMMUNS   JEVEUX
C-----------------------------------------------------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16,OPTIO,OPTIO2
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C-----------------------------------------------------------------------
      INTEGER GD,NEC,NLILI,DIGDEL
      PARAMETER (NNOEMA=100)
C     NNOEMA : NBRE MAXI DE NOEUDS PAR MAILLE ADMIS PAR LE S.P.
      PARAMETER (NDDLMA=300)
C     NNOEMA : NBRE MAXI DE DDLS PAR NOEUD ADMIS PAR LE S.P.
      INTEGER NUMLOC(NNOEMA,3)
      INTEGER POSDDL(NDDLMA,NNOEMA)
      LOGICAL CUMUL,ACREER
      CHARACTER*1 BASE1
      CHARACTER*5 K5BID
      CHARACTER*8 MATEL,MAILLA,K8BID,NOGDCO,NOGDSI
      CHARACTER*14 NUDEV
      CHARACTER*19 MATDEV
      CHARACTER*24 KMAILL,K24PRN,KNULIL,KMALIL,KMAREF,RESU,NOMLI,KHCOL,
     &             KADIA,KVALE,KDESC,KTMP1,KTMP2,KCONL
      REAL*8 R,RINF,RSUP
      INTEGER NMALIL,IMO,ILAGR,ILIMO
      INTEGER NBNO
      INTEGER NBEC
      CHARACTER*1 TYPSCA
C-----------------------------------------------------------------------
C     FONCTIONS LOCALES D'ACCES AUX DIFFERENTS CHAMPS DES
C     TYPE MANIPULEES DANS LE SOUS PROGRAMME
C-----------------------------------------------------------------------
      INTEGER ZZCONX,ZZNBNE,ZZLIEL,ZZNGEL,ZZNSUP,ZZNELG,ZZNELS
      INTEGER ZZNEMA,ZZPRNO,IZZPRN,EPDMS,JPDMS
      CHARACTER*1 K1BID

C---- FONCTION D ACCES AU CHAMP CONNEX DE LA TYPE MAILLA DE TYPE
C     MAILLAGE
C     ZZCONX(IMAIL,J) = NUMERO DANS LA NUMEROTATION DU MAILLAGE
C         DU NOEUD J DE LA MAILLE IMAIL
      ZZCONX(IMAIL,J) = ZI(ICONX1-1+ZI(ICONX2+IMAIL-1)+J-1)

C---- NBRE DE NOEUDS DE LA MAILLE IMAIL DU MAILLAGE

      ZZNBNE(IMAIL) = ZI(ICONX2+IMAIL) - ZI(ICONX2+IMAIL-1)

C---- FONCTION D ACCES AUX ELEMENTS DES CHAMPS LIEL DES TYPE LIGREL
C     REPERTORIEES DANS LE REPERTOIRE TEMPORAIRE .MATAS.LILI
C     ZZLIEL(ILI,IGREL,J) =
C      SI LA JIEME MAILLE DU LIEL IGREL DU LIGREL ILI EST:
C          -UNE MAILLE DU MAILLAGE : SON NUMERO DANS LE MAILLAGE
C          -UNE MAILLE TARDIVE : -POINTEUR DANS LE CHAMP .NEMA

      ZZLIEL(ILI,IGREL,J) = ZI(ZI(IADLIE+3* (ILI-1)+1)-1+
     &                      ZI(ZI(IADLIE+3* (ILI-1)+2)+IGREL-1)+J-1)

C---- NBRE DE GROUPES D'ELEMENTS (DE LIEL) DU LIGREL ILI

      ZZNGEL(ILI) = ZI(IADLIE+3* (ILI-1))

C---- NBRE DE NOEUDS DE LA MAILLE TARDIVE IEL ( .NEMA(IEL))
C     DU LIGREL ILI REPERTOIRE .LILI
C     (DIM DU VECTEUR D'ENTIERS .LILI(ILI).NEMA(IEL) )

      ZZNSUP(ILI,IEL) = ZI(ZI(IADNEM+3* (ILI-1)+2)+IEL) -
     &                  ZI(ZI(IADNEM+3* (ILI-1)+2)+IEL-1) - 1

C---- NBRE D ELEMENTS DU LIEL IGREL DU LIGREL ILI DU REPERTOIRE TEMP.
C     .MATAS.LILI(DIM DU VECTEUR D'ENTIERS .LILI(ILI).LIEL(IGREL) )

      ZZNELG(ILI,IGREL) = ZI(ZI(IADLIE+3* (ILI-1)+2)+IGREL) -
     &                    ZI(ZI(IADLIE+3* (ILI-1)+2)+IGREL-1) - 1

C---- NBRE D ELEMENTS SUPPLEMENTAIRE (.NEMA) DU LIGREL ILI DU
C     REPERTOIRE TEMPORAIRE .MATAS.LILI

      ZZNELS(ILI) = ZI(IADNEM+3* (ILI-1))

C---- FONCTION D ACCES AUX ELEMENTS DES CHAMPS NEMA DES TYPE LIGREL
C     REPERTORIEES DANS LE REPERTOIRE TEMPO. .MATAS.LILI
C     ZZNEMA(ILI,IEL,J) =  1.LE. J .GE. ZZNELS(ILI)
C      SI LE J IEME NOEUD DE LA MAILE TARDIVE IEL DU LIGREL ILI EST:
C          -UN NOEUD DU MAILLAGE : SON NUMERO DANS LE MAILLAGE
C          -UN NOEUD TARDIF : -SON NUMERO DANS LA NUMEROTATION LOCALE
C                              AU LIGREL ILI
C     ZZNEMA(ILI,IEL,ZZNELS(ILI)+1)=NUMERO DU TYPE_MAILLE DE LA MAILLE
C                                   IEL DU LIGREL ILI

      ZZNEMA(ILI,IEL,J) = ZI(ZI(IADNEM+3* (ILI-1)+1)-1+
     &                    ZI(ZI(IADNEM+3* (ILI-1)+2)+IEL-1)+J-1)

C---- FONCTION D ACCES AUX ELEMENTS DES CHAMPS PRNO DES TYPE LIGREL
C     REPERTORIEES DANS NU.LILI DE LA TYPE NUME_DDL ET A LEURS ADRESSES
C     ZZPRNO(ILI,NUNOEL,1) = NUMERO DE L'EQUATION ASSOCIEES AU 1ER DDL
C                            DU NOEUD NUNOEL DANS LA NUMEROTATION LOCALE
C                            AU LIGREL ILI DE .LILI
C     ZZPRNO(ILI,NUNOEL,2) = NOMBRE DE DDL PORTES PAR LE NOEUD NUNOEL
C     ZZPRNO(ILI,NUNOEL,2+1) = 1ER CODE
C     ZZPRNO(ILI,NUNOEL,2+NEC) = NEC IEME CODE

      IZZPRN(ILI,NUNOEL,L) = (IDPRN1-1+ZI(IDPRN2+ILI-1)+
     &                       (NUNOEL-1)* (NEC+2)+L-1)
      ZZPRNO(ILI,NUNOEL,L) = ZI(IDPRN1-1+ZI(IDPRN2+ILI-1)+
     &                       (NUNOEL-1)* (NEC+2)+L-1)
C----------------------------------------------------------------------
C                DEBUT DES INSTRUCTIONS
      CALL JEMARQ()
      CALL JEDBG2(IDBGAV,0)
C----------------------------------------------------------------------

C----RECUPERATION DU NIVEAU D'IMPRESSION


      CALL INFNIV(IFM,NIV)
C---------------------------------------------------------------------
      MATDEV = MATAS
      BASE1 = BASE

C---- VERIF DE MOTCLE: SI ZERO ON ECRASE SI CUMU ON CUMULE
      IF (MOTCLE(1:4).EQ.'ZERO') THEN
        CUMUL = .FALSE.
      ELSE IF (MOTCLE(1:4).EQ.'CUMU') THEN
        CUMUL = .TRUE.
      ELSE
        CALL UTMESS('F','ASSMAM',' LE PARAMETRE : '//MOTCLE//
     &              ' EST INCORRECT. ON ATTEND : "CUMU" OU "ZERO" ')
      END IF

C---- SI LE CONCEPT MATAS EXISTE DEJA, ON LE DETRUIT:==> OPTION ZERO
      CALL JEEXIN(MATDEV//'.REFA',IRET)
      IF (IRET.GT.0) THEN

        NUDEV = NU
        KMAREF = MATDEV//'.REFA'
        CALL JEVEUO(KMAREF,'L',IDMARF)
        IF ((ZK24(IDMARF-1+2) (1:14).EQ.NUDEV) .AND.
     &      (ZK24(IDMARF-1+3) (17:18).EQ.'MO')) THEN
          ACREER = .FALSE.
C --- ON DETRUIT LES OBJETS QUI SONT TOUJOURS A DETRUIRE
          CALL JEDETR(KMAREF(1:19)//'.LIME')
          CALL JEDETR(KMAREF(1:19)//'.CONI')
          CALL JEDETR(KMAREF(1:19)//'.ABLI')
          CALL JEDETR(KMAREF(1:19)//'.LLIG')
          CALL JEDETR(KMAREF(1:19)//'.ALIG')
          CALL JEDETR(KMAREF(1:19)//'.VALI')
          CALL JEDETR(KMAREF(1:19)//'.JDRF')
          CALL JEDETR(KMAREF(1:19)//'.JDDC')
          CALL JEDETR(KMAREF(1:19)//'.JDFF')
          CALL JEDETR(KMAREF(1:19)//'.JDHF')
          CALL JEDETR(KMAREF(1:19)//'.JDPM')
          CALL JEDETR(KMAREF(1:19)//'.JDES')
          CALL JEDETR(KMAREF(1:19)//'.JDVL')
          CALL JEDETR(KMAREF)
        ELSE
          ACREER = .TRUE.
        END IF
      ELSE
        ACREER = .TRUE.
      END IF
      IF (ACREER) CALL DETRSD('MATR_ASSE',MATDEV)

C---- RECOPIE DE LA LISTE DES MAT_ELE DANS 1 OBJET JEVEUX DONT ON
C     GARDE L'ADRESSE DANS LE COMMON /CADMAT/
      CALL WKVECT(MATDEV//'.LIME',BASE1//' V K8 ',NBMAT,ILIMAT)
      DO 10 I = 1,NBMAT
        ZK8(ILIMAT+I-1) = TLIMAT(I)
   10 CONTINUE

C---- NOMS DES PRINCIPAUX OBJETS JEVEUX LIES A MATAS

      KMAILL = '&MAILLA                 '
      KMALIL = MATDEV//'.LILI'
      KMAREF = MATDEV//'.REFA'
      KVALE = MATDEV//'.VALE'

C---- NOMS DES PRINCIPAUX OBJETS JEVEUX LIES AU STOCKAGE
      NUDEV = NU
      K5BID(1:5) = '.SMOS'
      KHCOL = NUDEV(1:14)//K5BID(1:5)//'.HCOL'
      CALL JEVEUO(KHCOL,'L',IDHCOL)
      KADIA = NUDEV(1:14)//K5BID(1:5)//'.ADIA'
      CALL JEVEUO(KADIA,'L',IDADIA)
      KDESC = NUDEV(1:14)//K5BID(1:5)//'.DESC'

C---- CALCUL D UN REPERTOIRE,TEMPORAIRE, MATDEV.LILI A PARTIR DE LA
C     LISTE DE MATRICES ELEMENTAIRES MATDEV.LIME
      CALL CRELIL(NBMAT,ILIMAT,KMALIL,'V',KMAILL,MATDEV(1:19),GD,MAILLA,
     &            NEC,NCMP,ILIMO,NLILI,NBELM)
      CALL JEVEUO(MATDEV(1:19)//'.ADLI','E',IADLIE)
      CALL JEVEUO(MATDEV(1:19)//'.ADNE','E',IADNEM)
      CALL JEEXIN(MAILLA(1:8)//'.CONNEX',IRET)
      IF (IRET.GT.0) THEN
        CALL JEVEUO(MAILLA(1:8)//'.CONNEX','L',ICONX1)
        CALL JEVEUO(JEXATR(MAILLA(1:8)//'.CONNEX','LONCUM'),'L',ICONX2)
      END IF

      CALL DISMOI('F','NOM_GD',NUDEV,'NUME_DDL',IBID,NOGDCO,IERD)
      CALL DISMOI('F','NOM_GD_SI',NOGDCO,'GRANDEUR',IBID,NOGDSI,IERD)
      CALL DISMOI('F','NB_CMP_MAX',NOGDSI,'GRANDEUR',NMXCMP,K8BID,IERD)
      CALL DISMOI('F','NUM_GD_SI',NOGDSI,'GRANDEUR',NUGD,K8BID,IERD)
      NEC = NBEC(NUGD)
      NCMP = NMXCMP

C---- TEST EXISTENCE &&POIDS_MAILLE

      CALL JEEXIN('&&POIDS_MAILLE',EPDMS)
      IF (EPDMS.GT.0) CALL JEVEUO('&&POIDS_MAILLE','L',JPDMS)

C---- NMALIL= NOMBRE DE NOM DU REPERTOIRE MATDEV.LILI DE NOM KMALIL
C     ILIMO = NUMERO DU 1ER LIGREL DE MODELE DE KMALIL
      NMALIL = NLILI

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
      CALL JECREO(KMAREF,BASE1//' V K24')
      CALL JEECRA(KMAREF,'LONMAX',4,' ')
      CALL JEECRA(KMAREF,'DOCU',IBID,'ASSE')
      CALL JEVEUO(KMAREF,'E',IDMARF)
      ZK24(IDMARF) = MAILLA
      ZK24(IDMARF+1) = NUDEV(1:14)//'.NUME'
      ZK24(IDMARF+2) = NUDEV(1:14)//K5BID(1:5)

C---- CREATION DE 2 OBJETS VOLATILS POUR ACCELERER CHARGER:

C---- TMP1 : (1:NBLC) INDIQUE LE NOMBRE DE REELS
C            S'INJECTANT DANS 1 BLOC
C---- TMP2 : (1:2*DIM(MATR_ELEM)) POSITION RELATIVE DANS LES BLOCS
C            POUR LE I-EME REEL DE LA MATRICE ELEM :
C     TMP2(2*(I-1)+1) --> NUMERO DU BLOC OU S'INJECTE I.
C     TMP2(2*(I-1)+2) --> POSITION DANS LE BLOC DU REEL I.

      KTMP1 = KVALE(1:19)//'.TMP1           '
      KTMP2 = KVALE(1:19)//'.TMP2           '
      CALL JEEXIN(KTMP1,IRET)
      CALL JEVEUO(KDESC,'L',IDDESC)
      NEQU = ZI(IDDESC)
      ITBLOC = ZI(IDDESC+1)
      NBLC = ZI(IDDESC+2)
      IF (NBLC.NE.1) CALL UTMESS('F','ASSMAM_1',
     &                           'LE NOMBRE DE BLOCS DU'//
     &                           'STOCKAGE "MORSE" '//KDESC(1:19)//
     &                           ' EST DIFFERENT DE 1.')
      IF (IRET.LE.0) THEN
        CALL JECREO(KTMP1,' V V I')
        CALL JEECRA(KTMP1,'LONMAX',NBLC,' ')
      END IF
      CALL JEVEUO(KTMP1,'E',IATMP1)

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
        DO 20 I = 1,NBLC
          CALL JECROC(JEXNUM(KVALE,I))
   20   CONTINUE
      ELSE
        IF (.NOT.CUMUL) THEN
C         -- REMISE A ZERO DE .VALE :
          DO 30 I = 1,NBLC
            CALL JERAZO(JEXNUM(KVALE,I),ITBLOC,1)
            CALL JELIBE(JEXNUM(KVALE,I))
   30     CONTINUE
        END IF
      END IF

C---- CALCUL DE VALE
C     ON COMMENCE PAR ASSEMBLER SUR LE MODELE
      IMO = 1
      RINF = R8MAEM()
      RSUP = -1.D0
      COEF = +1.D0
   40 CONTINUE

      ILONG = 0


C     POUR LES MATRICES MORSE : IL N'Y A QU'1 BLOC. ON LE MET EN MEMOIRE
      CALL JEVEUO(JEXNUM(KVALE,1),'E',JVAL1)
      DO 180 IMAT = 1,NBMAT

C---- BOUCLE SUR LES MATR_ELEM

        MATEL = ZK8(ILIMAT+IMAT-1)
        CALL JEVEUO(MATEL//'.LISTE_RESU','L',IDLRES)
        CALL JELIRA(MATEL//'.LISTE_RESU','LONUTI ',NBRESU,K1BID)
        CALL DISMOI('F','SUR_OPTION',MATEL,'MATR_ELEM',IBID,OPTIO,IERD)

C       -- ON REGARDE SI TOUS LES MATR_ELEM ONT ETE CALCULES AVEC LA
C          MEME SUR_OPTION :
        IF (IMAT.EQ.1) THEN
          OPTIO2 = OPTIO
        ELSE
          IF (OPTIO2.NE.OPTIO) OPTIO2 = '&&MELANGE'
        END IF

        DO 170 IRESU = 1,NBRESU

C---- BOUCLE SUR LES RESU_ELEM


          RESU = ZK24(IDLRES+IRESU-1)
          CALL JEEXIN(RESU(1:19)//'.DESC',IER)
          IF (IER.EQ.0) GO TO 170

          CALL JEVEUO(RESU(1:19)//'.NOLI','L',IAD)
          NOMLI = ZK24(IAD)
          CALL JENONU(JEXNOM(KMALIL,NOMLI),ILIMA)
          CALL JENONU(JEXNOM(KNULIL,NOMLI),ILINU)
          IF ((IMO.EQ.1) .AND. (ILIMA.NE.ILIMO)) GO TO 170
          IF ((IMO.EQ.0) .AND. (ILIMA.EQ.ILIMO)) GO TO 170

          CALL DISMOI('F','TYPE_SCA',RESU,'RESUELEM',IBID,TYPSCA,IERD)

          DO 160 IGR = 1,ZZNGEL(ILIMA)

C---- BOUCLE SUR LES GOUPES D'ELEMENTS

            CALL JEVEUO(RESU(1:19)//'.DESC','L',IADESC)
            MODE = ZI(IADESC+IGR+1)
            IF (MODE.GT.0) THEN
              NNOE = NBNO(MODE)
              NCMPEL = DIGDEL(MODE)
              NEL = ZZNELG(ILIMA,IGR)
              CALL JEVEUO(JEXNUM(RESU(1:19)//'.RESL',IGR),'L',IDRESL)
              IF (NCMPEL.GT.ILONG) THEN
                ILONG = NCMPEL
                CALL JEEXIN(KTMP2,IRET2)
                IF (IRET2.GT.0) CALL JEDETR(KTMP2)
                CALL WKVECT(KTMP2,' V V I',2*ILONG,IATMP2)
              END IF
              DO 150 IEL = 1,NEL

C---- BOUCLE SUR LES ELEMENTS D'UN GROUPE D'ELEMENT

                IREEL = 0
                ILAGR = 0
C---- R = COEF MULTIPLICATEUR
                R = LICOEF(IMAT)
                NUMA = ZZLIEL(ILIMA,IGR,IEL)
                IF (NUMA.GT.0) THEN
                  IF (EPDMS.GT.0) R=R*ZR(JPDMS-1+NUMA)
                  DO 90 K1 = 1,NNOE
                    N1 = ZZCONX(NUMA,K1)
                    IAD1 = ZZPRNO(ILIMNU,N1,1)
                    CALL CORDDL(IDPRN1,IDPRN2,ILIMNU,MODE,NEC,NCMP,N1,
     &                          K1,NDDL1,POSDDL(1,K1))
                    IF (NDDL1.GT.NDDLMA) THEN
                      CALL UTDEBM('F','ASSMAM','11')
                      CALL UTIMPI('L','  NDDL : ',1,NDDL1)
                      CALL UTIMPI('S',' > NDDL_MAX :',1,NDDLMA)
                      CALL UTFINM()
                    END IF
                    NUMLOC(K1,1) = N1
                    NUMLOC(K1,2) = IAD1
                    NUMLOC(K1,3) = NDDL1
                    DO 80 I1 = 1,NDDL1
                      DO 60 K2 = 1,K1 - 1
                        IAD2 = NUMLOC(K2,2)
                        NDDL2 = NUMLOC(K2,3)
                        DO 50 I2 = 1,NDDL2
                          IAD11 = IAD1 + POSDDL(I1,K1) - 1
                          IAD21 = IAD2 + POSDDL(I2,K2) - 1
                          IF (IAD11.LE.IAD21) THEN
                            IADLI = IAD11
                            IADCO = IAD21
                          ELSE
                            IADLI = IAD21
                            IADCO = IAD11
                          END IF
                          CALL ASRETM(IATMP2,IREEL,IDHCOL,IDADIA,IADLI,
     &                                IADCO)
   50                   CONTINUE
   60                 CONTINUE
                      K2 = K1
                      IAD2 = NUMLOC(K2,2)
                      NDDL2 = NUMLOC(K2,3)
                      DO 70 I2 = 1,I1
                        IAD11 = IAD1 + POSDDL(I1,K1) - 1
                        IAD21 = IAD2 + POSDDL(I2,K2) - 1
                        IF (IAD11.LE.IAD21) THEN
                          IADLI = IAD11
                          IADCO = IAD21
                        ELSE
                          IADLI = IAD21
                          IADCO = IAD11
                        END IF
                        CALL ASRETM(IATMP2,IREEL,IDHCOL,IDADIA,IADLI,
     &                              IADCO)
   70                 CONTINUE
   80               CONTINUE
   90             CONTINUE
                ELSE
C---- CONDITIONNEMENT DES LAGRANGE
C---- SI MAILLE A 3 NOEUDS ET SI ON N'EST PAS SUR LE MODELE ALORS :
C---- ILAGR = 1 POSSIBILITE DE NOEUDS DE LAGRANGE DANS LA MAILLE
                  IF ((NNOE.EQ.3) .AND. (IMO.EQ.0)) ILAGR = 1
                  NUMA = -NUMA
                  N1 = ZZNSUP(ILIMA,NUMA)
                  DO 140 K1 = 1,NNOE
                    N1 = ZZNEMA(ILIMA,NUMA,K1)
                    IF (N1.LT.0) THEN
                      N1 = -N1
                      IAD1 = ZZPRNO(ILINU,N1,1)
                      CALL CORDDL(IDPRN1,IDPRN2,ILINU,MODE,NEC,NCMP,N1,
     &                            K1,NDDL1,POSDDL(1,K1))

C---- SI NOEUD LAGR ( ILAGR=1,NDDL1=1,N1<0,NUMA<0 ) ALORS CONL(IAD1)=R
C---- ET POUR TOUTE LA MATRICE ELEMENTAIRE ON POSE R = COEF*LICOEF(IMAT)
                      IF ((ILAGR.EQ.1) .AND. (NDDL1.EQ.1)) THEN
                        R = COEF*LICOEF(IMAT)
                        ZR(IDCONL-1+IAD1) = R
                      END IF
                      IF (NDDL1.GT.NDDLMA) THEN
                        CALL UTDEBM('F','ASSMAM','12')
                        CALL UTIMPI('L','  NDDL : ',1,NDDL1)
                        CALL UTIMPI('S',' > NDDL_MAX :',1,NDDLMA)
                        CALL UTFINM()
                      END IF
                    ELSE
                      IAD1 = ZZPRNO(ILIMNU,N1,1)
                      CALL CORDDL(IDPRN1,IDPRN2,ILIMNU,MODE,NEC,NCMP,N1,
     &                            K1,NDDL1,POSDDL(1,K1))
                      IF (NDDL1.GT.NDDLMA) THEN
                        CALL UTDEBM('F','ASSMAM','13')
                        CALL UTIMPI('L','- NDDL : ',1,NDDL1)
                        CALL UTIMPI('S',' > NDDL_MAX :',1,NDDLMA)
                        CALL UTFINM()
                      END IF
                    END IF
                    NUMLOC(K1,1) = N1
                    NUMLOC(K1,2) = IAD1
                    NUMLOC(K1,3) = NDDL1
                    DO 130 I1 = 1,NDDL1
                      DO 110 K2 = 1,K1 - 1
                        IAD2 = NUMLOC(K2,2)
                        NDDL2 = NUMLOC(K2,3)
                        DO 100 I2 = 1,NDDL2
                          IAD11 = IAD1 + POSDDL(I1,K1) - 1
                          IAD21 = IAD2 + POSDDL(I2,K2) - 1
                          IF (IAD11.LE.IAD21) THEN
                            IADLI = IAD11
                            IADCO = IAD21
                          ELSE
                            IADLI = IAD21
                            IADCO = IAD11
                          END IF
                          CALL ASRETM(IATMP2,IREEL,IDHCOL,IDADIA,IADLI,
     &                                IADCO)
  100                   CONTINUE
  110                 CONTINUE
                      K2 = K1
                      IAD2 = NUMLOC(K2,2)
                      NDDL2 = NUMLOC(K2,3)
                      DO 120 I2 = 1,I1
                        IAD11 = IAD1 + POSDDL(I1,K1) - 1
                        IAD21 = IAD2 + POSDDL(I2,K2) - 1
                        IF (IAD11.LE.IAD21) THEN
                          IADLI = IAD11
                          IADCO = IAD21
                        ELSE
                          IADLI = IAD21
                          IADCO = IAD11
                        END IF
                        CALL ASRETM(IATMP2,IREEL,IDHCOL,IDADIA,IADLI,
     &                              IADCO)
  120                 CONTINUE
  130               CONTINUE
  140             CONTINUE
                END IF
C---- POUR FINIR, ON RECOPIE EFFECTIVEMENT LES TERMES:
C     (IREEL CONTIENT LE NOMBRE DE REELS A TRAITER)
                ZI(IATMP1) = 1
                IF (TYPE.EQ.1) THEN
                  CALL ASCOPR(IATMP1,IATMP2,IREEL,
     &                IDRESL+NCMPEL* (IEL-1),NBLC,KVALE,R,1,JVAL1)
                ELSE IF (TYPE.EQ.2) THEN
                  IF (TYPSCA.EQ.'R') THEN
                    CALL ASCOPN(IATMP1,IATMP2,IREEL,
     &                IDRESL+NCMPEL* (IEL-1),NBLC,KVALE,R,1,JVAL1)
                  ELSE IF (TYPSCA.EQ.'C') THEN
                    CALL ASCOPC(IATMP1,IATMP2,IREEL,
     &                IDRESL+NCMPEL* (IEL-1),NBLC,KVALE,R,1,JVAL1)
                  END IF
                END IF
  150         CONTINUE
              CALL JELIBE(JEXNUM(RESU(1:19)//'.RESL',IGR))
            END IF
  160     CONTINUE
  170   CONTINUE
  180 CONTINUE
      IF (IMO.EQ.1) THEN

C---- SI ON VIENT DE TRAITER LE MODELE
        CALL JEVEUO(JEXNUM(KVALE,1),'L',IDV)
        DO 190 I = 1,NEQU
          IDI = ZI(IDADIA+I-1)
          IF (TYPE.EQ.1) R = ABS(ZR(IDV-1+IDI))
          IF (TYPE.EQ.2) R = ABS(ZC(IDV-1+IDI))
          IF ((R.NE.0.D0) .AND. (R.LT.RINF)) RINF = R
          IF ((R.NE.0.D0) .AND. (R.GT.RSUP)) RSUP = R
  190   CONTINUE
        CALL JELIBE(JEXNUM(KVALE,1))
        COEF = (RSUP+RINF)/2.D0
        IF (RINF.GE.R8MAEM()) COEF = RSUP/2.D0
        IF (NIV.EQ.2) THEN
          WRITE (IFM,1000) COEF
        END IF
        IMO = 0
C---- CREATION DE L'OBJET .CONL EN OPTION RIGI SI AU MOINS UNE CHARGE
        IF (NMALIL.GT.2) THEN
          KCONL = MATDEV(1:19)//'.CONL'
          CALL JEDETR(KCONL)
          CALL WKVECT(KCONL,BASE1//' V R',NEQU,IDCONL)
          DO 200 I = 1,NEQU
            ZR(IDCONL-1+I) = 1.D0
  200     CONTINUE
        END IF
        GO TO 40
      END IF

C     -- MISE A JOUR DE REFA(4)
      CALL JEVEUO(KMAREF,'E',IDMARF)
      IF (MOTCLE(1:4).EQ.'ZERO') THEN
        ZK24(IDMARF-1+4) = OPTIO2
      ELSE
        IF (ZK24(IDMARF-1+4).NE.OPTIO2) ZK24(IDMARF-1+4) = '&&MELANGE'
      END IF

      CALL JEDETR(KTMP1)
      CALL JEDETR(KTMP2)
      CALL JEDETR(MATDEV//'.ADNE')
      CALL JEDETR(MATDEV//'.ADLI')
  210 CONTINUE
 1000 FORMAT (1P,'COEFFICIENT DE CONDITIONNEMENT DES LAGRANGES',E12.5)
      CALL JEDBG2(IBID,IDBGAV)
      CALL JEDEMA()
      END
