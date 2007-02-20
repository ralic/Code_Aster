      SUBROUTINE ASSMMN(BASE,MATAS,NBMAT,TLIMAT,LICOEF,NU,MOTCLE,TYPE)

C  ATTENTION : CETTE ROUTINE NE DOIT PAS ETRE APPELLEE DIRECTEMENT :
C              IL FAUT APPELER SON "CHAPEAU" : ASMATR.

      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*) BASE,MATAS,TLIMAT(*),NU
      INTEGER NBMAT,TYPE
      REAL*8 LICOEF(*)
      CHARACTER*4 MOTCLE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ASSEMBLA  DATE 20/02/2007   AUTEUR LEBOUVIER F.LEBOUVIER 
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
C     ASSEMBLAGE MORSE D'UNE MATRICE NON-SYMETRIQUE
C      AVEC PRECONDITIONNEMENT DES MATR_ELEM DE MAILLES
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
C IN  I   TYPE  : TYPE DES MATRICES ELEMENTAIRES A ASSEMBLEES
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
      INTEGER N,GD,NEC,NLILI,DIGDEL
      PARAMETER (NNOEMA=100)
C     NNOEMA : NBRE MAXI DE NOEUDS PAR MAILLE ADMIS PAR LE S.P.
      PARAMETER (NDDLMA=300)
C     NNOEMA : NBRE MAXI DE DDLS PAR NOEUD ADMIS PAR LE S.P.
      INTEGER NUMLOC(NNOEMA,3)
      INTEGER POSDDL(NDDLMA,NNOEMA)
      LOGICAL CUMUL,ACREER
      CHARACTER*1 BASE1,TYMAT
      CHARACTER*5 K5BID
      CHARACTER*7 SYM
      CHARACTER*8 MATEL,MAILLA
      CHARACTER*14 NUDEV
      CHARACTER*19 MATDEV,RESUL
      CHARACTER*24 KMAILL,K24PRN,KNULIL,KMALIL,RESU,NOMLI,KSMHC,KSMDI,
     &             KVALM,KTMP1,KTMP2,KCONL
      REAL*8 R,RINF,RSUP
      INTEGER HMAX,NMALIL,IMO,ILAGR,ILIMO
      INTEGER ADMODL,LCMODL,NBNO
C-----------------------------------------------------------------------
C     FONCTIONS LOCALES D'ACCES AUX DIFFERENTS CHAMPS DES
C     TYPE MANIPULEES DANS LE SOUS PROGRAMME
C-----------------------------------------------------------------------
      INTEGER ZZCONX,ZZNBNE,ZZLIEL,ZZNGEL,ZZNSUP,ZZNELG,ZZNELS
      INTEGER ZZNEMA,ZZPRNO,IZZPRN,JVAL(2)
      INTEGER VALI(2)
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

C --- RECUPERATION DU NIVEAU D'IMPRESSION :
C     -----------------------------------

      CALL INFNIV(IFM,NIV)
C---------------------------------------------------------------------
      MATDEV = MATAS
      BASE1 = BASE

C --- VERIF DE MOTCLE: SI ZERO ON ECRASE SI CUMU ON CUMULE :
C     ----------------------------------------------------
      IF (MOTCLE(1:4).EQ.'ZERO') THEN
        CUMUL = .FALSE.

      ELSE IF (MOTCLE(1:4).EQ.'CUMU') THEN
        CUMUL = .TRUE.

      ELSE
        CALL U2MESK('F','ASSEMBLA_3',1,MOTCLE)
      END IF
C
      CALL JEVEUO(JEXATR('&CATA.TE.MODELOC','LONCUM'),'L',LCMODL)
      CALL JEVEUO(JEXNUM('&CATA.TE.MODELOC',1),'L',ADMODL)

C --- SI LE CONCEPT MATAS EXISTE DEJA, ON LE DETRUIT:==> OPTION ZERO :
C     --------------------------------------------------------------
      CALL JEEXIN(MATDEV//'.REFA',IRET)
      IF (IRET.GT.0) THEN

        NUDEV = NU
        CALL JEVEUO(MATDEV//'.REFA','L',JREFA)
        IF (ZK24(JREFA-1+2) (1:14).EQ.NUDEV) THEN
          ACREER = .FALSE.

C --- ON DETRUIT LES OBJETS QUI SONT TOUJOURS A DETRUIRE :
C     --------------------------------------------------
          CALL JEDETR(MATDEV//'.LIME')
          CALL JEDETR(MATDEV//'.JDRF')
          CALL JEDETR(MATDEV//'.JDDC')
          CALL JEDETR(MATDEV//'.JDFF')
          CALL JEDETR(MATDEV//'.JDHF')
          CALL JEDETR(MATDEV//'.JDPM')
          CALL JEDETR(MATDEV//'.JDES')
          CALL JEDETR(MATDEV//'.JDVL')
          CALL JEDETR(MATDEV//'.REFA')

        ELSE
          ACREER = .TRUE.
        END IF

      ELSE
        ACREER = .TRUE.
      END IF

      IF (ACREER) CALL DETRSD('MATR_ASSE',MATDEV)

C --- RECOPIE DE LA LISTE DES MAT_ELE DANS 1 OBJET JEVEUX DONT ON
C --- GARDE L'ADRESSE DANS LE COMMON /CADMAT/ :
C     ---------------------------------------
      CALL WKVECT(MATDEV//'.LIME',BASE1//' V K8 ',NBMAT,ILIMAT)
      DO 10 I = 1,NBMAT
        ZK8(ILIMAT+I-1) = TLIMAT(I)
   10 CONTINUE

C --- NOMS DES PRINCIPAUX OBJETS JEVEUX LIES A MATAS :
C     ----------------------------------------------
      KMAILL = '&MAILLA                 '
      KMALIL = MATDEV//'.LILI'
      KVALM = MATDEV//'.VALM'

C --- NOMS DES PRINCIPAUX OBJETS JEVEUX LIES AU STOCKAGE :
C     --------------------------------------------------
      NUDEV = NU
      KSMHC = NUDEV(1:14)//'.SMOS.SMHC'
      CALL JEVEUO(KSMHC,'L',IDSMHC)
      KSMDI = NUDEV(1:14)//'.SMOS.SMDI'
      CALL JEVEUO(KSMDI,'L',IDSMDI)

      CALL JEVEUO(NUDEV(1:14)//'.SMOS.SMDE','L',JSMDE)
      NEQU = ZI(JSMDE-1+1)
      ITBLOC = ZI(JSMDE-1+2)

      CALL JEVEUO(NUDEV//'.NUME.NUEQ','L',JNUEQ)
      DO 20,IEQ = 1,NEQU
        CALL ASSERT(ZI(JNUEQ-1+IEQ).EQ.IEQ)
   20 CONTINUE

C --- CALCUL D UN REPERTOIRE,TEMPORAIRE, MATDEV.LILI A PARTIR DE LA
C --- LISTE DE MATRICES ELEMENTAIRES MATDEV.LIME :
C     ------------------------------------------
      CALL CRELIL(NBMAT,ILIMAT,KMALIL,'V',KMAILL,MATDEV(1:19),GD,MAILLA,
     &            NEC,NCMP,ILIMO,NLILI,NBELM)
      CALL JEVEUO(MATDEV(1:19)//'.ADLI','E',IADLIE)
      CALL JEVEUO(MATDEV(1:19)//'.ADNE','E',IADNEM)
      CALL JEEXIN(MAILLA(1:8)//'.CONNEX',IRET)
      IF (IRET.GT.0) THEN
        CALL JEVEUO(MAILLA(1:8)//'.CONNEX','L',ICONX1)
        CALL JEVEUO(JEXATR(MAILLA(1:8)//'.CONNEX','LONCUM'),'L',ICONX2)
      END IF

C --- NMALIL= NOMBRE DE NOM DU REPERTOIRE MATDEV.LILI DE NOM KMALIL
C --- ILIMO = NUMERO DU 1ER LIGREL DE MODELE DE KMALIL :
C     ------------------------------------------------
      NMALIL = NLILI

C --- ON SUPPOSE QUE LE NOM '&MAILLA' EST LE PREMIER DU REPERTOIRE
C --- NU.LILI CE QUI EST VRAI CF S.P. CRELIL :
C     --------------------------------------
      ILIMNU = 1

C --- NOMS DES PRINCIPAUX OBJETS JEVEUX LIES A NU :
C     -------------------------------------------
      K24PRN = NUDEV//'.NUME.PRNO'
      KNULIL = NUDEV//'.NUME.LILI'

C --- RECUPERATION DE PRNO :
C     --------------------
      CALL JEVEUO(K24PRN,'L',IDPRN1)
      CALL JEVEUO(JEXATR(K24PRN,'LONCUM'),'L',IDPRN2)

C --- CREATION ET REMPLISSAGE DE REFE :
C     -------------------------------
      CALL WKVECT(MATDEV//'.REFA',BASE1//' V K24',10,JREFA)
      ZK24(JREFA-1+1) = MAILLA
      ZK24(JREFA-1+2) = NUDEV(1:14)
      ZK24(JREFA-1+8) = 'ASSE'
      ZK24(JREFA-1+9) = 'MR'
      ZK24(JREFA-1+10) = 'NOEU'

C --- CREATION DE 2 OBJETS VOLATILS POUR ACCELERER CHARGER:
C ---
C --- TMP1 : (1:NBLC) INDIQUE LE NOMBRE DE REELS
C ---        S'INJECTANT DANS 1 BLOC
C --- TMP2 : (1:2*DIM(MATR_ELEM)) POSITION RELATIVE DANS LES BLOCS
C ---        POUR LE I-EME REEL DE LA MATRICE ELEM :
C --- TMP2(2*(I-1)+1) --> NUMERO DU BLOC OU S'INJECTE I
C --- TMP2(2*(I-1)+2) --> POSITION DANS LE BLOC DU REEL I :
C     ---------------------------------------------------
      KTMP1 = KVALM(1:19)//'.TMP1           '
      KTMP2 = KVALM(1:19)//'.TMP2           '
      CALL JEEXIN(KTMP1,IRET)

C --- CAS NON_SYMETRIQUE : ON DOUBLE LE NOMBRE DE BLOCS QUI A ETE DEFINI
C ---  -----------------   DANS PROMOR POUR DES MATRICES SYMETRIQUES :
C                          -----------------------------------------
      NBLC = 2

      IF (IRET.LE.0) THEN
        CALL JECREO(KTMP1,' V V I')
        CALL JEECRA(KTMP1,'LONMAX',NBLC,' ')
      END IF

      CALL JEVEUO(KTMP1,'E',IATMP1)

C --- ALLOCATION VALM EN R OU C SUIVANT TYPE :
C     --------------------------------------
      IF (ACREER) THEN
        IF (TYPE.EQ.1) THEN
          CALL JECREC(KVALM,BASE1//' V R','NU','DISPERSE','CONSTANT',
     &                NBLC)

        ELSE IF (TYPE.EQ.2) THEN
          CALL JECREC(KVALM,BASE1//' V C','NU','DISPERSE','CONSTANT',
     &                NBLC)

        ELSE
          CALL U2MESS('F','ASSEMBLA_4')
        END IF

        CALL JEECRA(KVALM,'LONMAX',ITBLOC,' ')
        DO 30 I = 1,NBLC
          CALL JECROC(JEXNUM(KVALM,I))
   30   CONTINUE

      ELSE
        IF (.NOT.CUMUL) THEN
C         -- REMISE A ZERO DE .VALM :
          DO 40 I = 1,NBLC
            CALL JERAZO(JEXNUM(KVALM,I),ITBLOC,1)
            CALL JELIBE(JEXNUM(KVALM,I))
   40     CONTINUE
        END IF

      END IF

C --- CALCUL DE VALM
C --- ON COMMENCE PAR ASSEMBLER SUR LE MODELE :
C     ---------------------------------------
      IMO = 1
      RINF = R8MAEM()
      RSUP = -1.D0
      COEF = +1.D0
   50 CONTINUE


C     ========================================
C --- = BOUCLE SUR LES MATR_ELEM
C     ========================================
      ILONG = 0
      DO 260 IMAT = 1,NBMAT
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

C     =======================================
C --- = BOUCLE SUR LES RESU_ELEM =
C     =======================================
        DO 250 IRESU = 1,NBRESU

          RESU = ZK24(IDLRES+IRESU-1)
          CALL JEEXIN(RESU(1:19)//'.DESC',IER)
          IF (IER.EQ.0) GO TO 250
          TYMAT = 'S'

          RESUL = RESU
          CALL DISMOI('F','TYPE_MATRICE',RESUL,'RESUELEM',IBID,SYM,IERD)
          IF (SYM.EQ.'NON_SYM') TYMAT = 'N'

          CALL JEVEUO(RESU(1:19)//'.NOLI','L',IAD)
          NOMLI = ZK24(IAD)
          CALL JENONU(JEXNOM(KMALIL,NOMLI),ILIMA)
          CALL JENONU(JEXNOM(KNULIL,NOMLI),ILINU)
          IF ((IMO.EQ.1) .AND. (ILIMA.NE.ILIMO)) GO TO 250
          IF ((IMO.EQ.0) .AND. (ILIMA.EQ.ILIMO)) GO TO 250
          DO 240 IGR = 1,ZZNGEL(ILIMA)
C     ====================================
C --- = BOUCLE SUR LES GOUPES D'ELEMENTS =
C     ====================================
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

C    =================================================
C--- = CAS DES MATRICES ELEMENTAIRES NON-SYMETRIQUES =
C    =================================================
              IF (TYMAT.EQ.'N') THEN

C--- BOUCLE SUR LES ELEMENTS D'UN GROUPE D'ELEMENT :
C    ---------------------------------------------
                DO 110 IEL = 1,NEL

                  IREEL = 0
                  ILAGR = 0
C---- R = COEF MULTIPLICATEUR
                  R = LICOEF(IMAT)
                  NUMA = ZZLIEL(ILIMA,IGR,IEL)

C ---   NUMA > 0 : CAS DES ELEMENTS 'PHYSIQUES' :
C       --------------------------------------
                  IF (NUMA.GT.0) THEN

C ---   AFFECTATION DE NUMLOC :
C       ---------------------
                    DO 60 K1 = 1,NNOE
                      N1 = ZZCONX(NUMA,K1)
                      IAD1 = ZZPRNO(ILIMNU,N1,1)
                      CALL CORDDL(ADMODL,LCMODL,IDPRN1,IDPRN2,ILIMNU,
     &                            MODE,NEC,NCMP,N1,K1,NDDL1,
     &                            POSDDL(1,K1))
                      IF (NDDL1.GT.NDDLMA) THEN
                          VALI (1) = NDDL1
                          VALI (2) = NDDLMA
      CALL U2MESG('F', 'ASSEMBLA_50',0,' ',2,VALI,0,0.D0)
                      END IF

                      NUMLOC(K1,1) = N1
                      NUMLOC(K1,2) = IAD1
                      NUMLOC(K1,3) = NDDL1
   60               CONTINUE

C ---   BOUCLE 1 SUR LES NOEUDS DE L'ELEMENT :
C       ------------------------------------
                    DO 100 K1 = 1,NNOE

                      N1 = NUMLOC(K1,1)
                      IAD1 = NUMLOC(K1,2)
                      NDDL1 = NUMLOC(K1,3)

C ---    PARCOURS DES DDLS DU NOEUD 1 :
C        ----------------------------
                      DO 90 I1 = 1,NDDL1

C ---      NUMERO D'EQUATION DU DDL COURANT DANS LA MATRICE ASSEMBLEE :
C          ---------------------------------------------------------
                        IAD11 = IAD1 + POSDDL(I1,K1) - 1

C ---      BOUCLE 2 SUR LES NOEUDS DE L'ELEMENT :
C          ------------------------------------
                        DO 80 K2 = 1,NNOE

                          IAD2 = NUMLOC(K2,2)
                          NDDL2 = NUMLOC(K2,3)

C ---       PARCOURS DES DDLS DU NOEUD 2 :
C           ----------------------------
                          DO 70 I2 = 1,NDDL2

C ---        NUMERO D'EQUATION DU DDL COURANT DANS LA MATRICE ASSEMBLEE:
C            ----------------------------------------------------------
                            IAD21 = IAD2 + POSDDL(I2,K2) - 1

C ---        LE NUMERO DE LIGNE EST PLUS PETIT QUE LE NUMERO DE
C ---        COLONNE : ON ASSEMBLE DANS LE BLOC SUPERIEUR DONT ON
C ---        PRIS CONVENTIONNELLEMENT LE NUMERO EGAL A 1 :
C            -------------------------------------------
                            IF (IAD11.LE.IAD21) THEN
                              IADLI = IAD11
                              IADCO = IAD21
                              NUBLOC = 1

                            ELSE

C ---        LE NUMERO DE LIGNE EST PLUS GRAND QUE LE NUMERO DE
C ---        COLONNE : ON ASSEMBLE DANS LE BLOC INFERIEUR DONT ON
C ---        PRIS CONVENTIONNELLEMENT LE NUMERO EGAL A 2 :
C            -------------------------------------------
                              IADLI = IAD21
                              IADCO = IAD11
                              NUBLOC = 2
                            END IF

C ---        ON STOCKE LA POSITION DANS LE BLOC NUBLOC DE LA MATRICE
C ---        ASSEMBLEE, DU TERME COURANT IREEL DE LA MATRICE
C ---        ELEMENTAIRE, CES INFORMATIONS SONT STOCKEES DANS KTMP2 :
C            ------------------------------------------------------
                            CALL ASRETN(IATMP2,IREEL,IDSMHC,IDSMDI,
     &                                  IADLI,IADCO,NUBLOC)
   70                     CONTINUE
   80                   CONTINUE
   90                 CONTINUE
  100               CONTINUE
                  END IF

C --- POUR FINIR, ON RECOPIE EFFECTIVEMENT LES TERMES:
C --- ON AFFECTE DES VALEURS BIDON A KTMP1 POUR PASSER DANS ASCOPR
C --- (IREEL CONTIENT LE NOMBRE DE REELS A TRAITER)
C     ---------------------------------------------
                  ZI(IATMP1) = 1
                  ZI(IATMP1+1) = 1
                  IF (TYPE.EQ.1) THEN
                    CALL ASCOPR(IATMP1,IATMP2,IREEL,
     &                          IDRESL+NCMPEL* (IEL-1),NBLC,KVALM,R,0,0)

                  ELSE IF (TYPE.EQ.2) THEN
                    CALL ASCOPC(IATMP1,IATMP2,IREEL,
     &                          IDRESL+NCMPEL* (IEL-1),NBLC,KVALM,R,0,0)
                  END IF

  110           CONTINUE

C --- ON AFFECTE AUX TERMES DIAGONAUX DU BLOC INFERIEUR LES VALEURS
C --- DES TERMES DIAGONAUX DU BLOC SUPERIEUR (PAR SOUCI D'HOMOGENEITE
C --- VIS-A-VIS DU TRAITEMENT DES TERMES DIAGONAUX POUR LES
C --- MATRICES ELEMENTAIRES SYMETRIQUES) :
C     ----------------------------------
                CALL JEVEUO(JEXNUM(KVALM,1),'L',IADVAS)
                CALL JEVEUO(JEXNUM(KVALM,2),'E',IADVAI)

                DO 120 IEQ = 1,NEQU
                  IDIA = ZI(IDSMDI+IEQ-1)
                  ZR(IADVAI+IDIA-1) = ZR(IADVAS+IDIA-1)
  120           CONTINUE

                CALL JELIBE(JEXNUM(KVALM,1))
                CALL JELIBE(JEXNUM(KVALM,2))

C    =============================================
C--- = CAS DES MATRICES ELEMENTAIRES SYMETRIQUES =
C    =============================================
              ELSE IF (TYMAT.EQ.'S') THEN

C--- BOUCLE SUR LES ELEMENTS D'UN GROUPE D'ELEMENT :
C    ---------------------------------------------
                DO 230 IEL = 1,NEL
                  IREEL = 0
                  ILAGR = 0
C---- R = COEF MULTIPLICATEUR
                  R = LICOEF(IMAT)
                  NUMA = ZZLIEL(ILIMA,IGR,IEL)

C ---   NUMA > 0 : CAS DES ELEMENTS 'PHYSIQUES' :
C       --------------------------------------
                  IF (NUMA.GT.0) THEN

C ---   BOUCLE 1 SUR LES NOEUDS DE L'ELEMENT :
C       ------------------------------------
                    DO 170 K1 = 1,NNOE
                      N1 = ZZCONX(NUMA,K1)
                      IAD1 = ZZPRNO(ILIMNU,N1,1)
                      CALL CORDDL(ADMODL,LCMODL,IDPRN1,IDPRN2,ILIMNU,
     &                            MODE,NEC,NCMP,N1,K1,NDDL1,
     &                            POSDDL(1,K1))
                      IF (NDDL1.GT.NDDLMA) THEN
                          VALI (1) = NDDL1
                          VALI (2) = NDDLMA
      CALL U2MESG('F', 'ASSEMBLA_50',0,' ',2,VALI,0,0.D0)
                      END IF

                      NUMLOC(K1,1) = N1
                      NUMLOC(K1,2) = IAD1
                      NUMLOC(K1,3) = NDDL1

C ---    PARCOURS DES DDLS DU NOEUD 1 :
C        ----------------------------
                      DO 160 I1 = 1,NDDL1

C ---      BOUCLE 2 SUR LES NOEUDS DE L'ELEMENT :
C          ------------------------------------
                        DO 140 K2 = 1,K1 - 1
                          IAD2 = NUMLOC(K2,2)
                          NDDL2 = NUMLOC(K2,3)

C ---       PARCOURS DES DDLS DU NOEUD 2 :
C           ----------------------------
                          DO 130 I2 = 1,NDDL2

C ---        IAD11 NUMERO D'EQUATION DU DDL 1 DANS LA MATRICE ASSEMBLEE
C ---        IAD21 NUMERO D'EQUATION DU DDL 2 DANS LA MATRICE ASSEMBLEE:
C            ----------------------------------------------------------
                            IAD11 = IAD1 + POSDDL(I1,K1) - 1
                            IAD21 = IAD2 + POSDDL(I2,K2) - 1
                            IF (IAD11.LE.IAD21) THEN
                              IADLI = IAD11
                              IADCO = IAD21

                            ELSE
                              IADLI = IAD21
                              IADCO = IAD11
                            END IF

C ---        ON STOCKE LA POSITION DANS UN BLOC DE LA MATRICE
C ---        ASSEMBLEE, DU TERME COURANT IREEL DE LA MATRICE
C ---        ELEMENTAIRE, CETTE INFORMATION SONT STOCKEE DANS KTMP2,
C ---        L'INFORMATION DU NUMERO DU BLOC N'EST PAS STOCKEE
C ---        CAR LA MATRICE ELEMENTAIRE ETANT SYMETRIQUE, LE TERME
C ---        COURANT EXISTE A LA MEME POSITION DANS LE BLOC
C ---        SUPERIEUR ET DANS LE BLOC INFERIEUR :
C            -----------------------------------
                            CALL ASRETM(IATMP2,IREEL,IDSMHC,IDSMDI,
     &                                  IADLI,IADCO)
  130                     CONTINUE
  140                   CONTINUE

C ---       TRAITEMENT DU TERME DIAGONAL DE LA MATRICE ELEMENTAIRE :
C           -----------------------------------------------------
                        K2 = K1
                        IAD2 = NUMLOC(K2,2)
                        NDDL2 = NUMLOC(K2,3)

C ---       PARCOURS DES DDLS DU NOEUD 2 :
C           ----------------------------
                        DO 150 I2 = 1,I1

C ---        IAD11 NUMERO D'EQUATION DU DDL 1 DANS LA MATRICE ASSEMBLEE
C ---        IAD21 NUMERO D'EQUATION DU DDL 2 DANS LA MATRICE ASSEMBLEE:
C            ----------------------------------------------------------
                          IAD11 = IAD1 + POSDDL(I1,K1) - 1
                          IAD21 = IAD2 + POSDDL(I2,K2) - 1
                          IF (IAD11.LE.IAD21) THEN
                            IADLI = IAD11
                            IADCO = IAD21

                          ELSE
                            IADLI = IAD21
                            IADCO = IAD11
                          END IF

C ---        ON STOCKE LA POSITION DANS UN BLOC DE LA MATRICE
C ---        ASSEMBLEE, DU TERME COURANT IREEL DE LA MATRICE
C ---        ELEMENTAIRE, CETTE INFORMATION SONT STOCKEE DANS KTMP2,
C ---        L'INFORMATION DU NUMERO DU BLOC N'EST PAS STOCKEE
C ---        CAR LA MATRICE ELEMENTAIRE ETANT SYMETRIQUE, LE TERME
C ---        COURANT EXISTE A LA MEME POSITION DANS LE BLOC
C ---        SUPERIEUR ET DANS LE BLOC INFERIEUR :
C            -----------------------------------
                          CALL ASRETM(IATMP2,IREEL,IDSMHC,IDSMDI,IADLI,
     &                                IADCO)
  150                   CONTINUE
  160                 CONTINUE
  170               CONTINUE

                  ELSE

C ---   NUMA < 0 : CAS DES ELEMENTS 'LAGRANGE' :
C ---  CONDITIONNEMENT DES LAGRANGE
C ---  SI MAILLE A 3 NOEUDS ET SI ON N'EST PAS SUR LE MODELE ALORS
C ---  ILAGR = 1 POSSIBILITE DE NOEUDS DE LAGRANGE DANS LA MAILLE :
C      ----------------------------------------------------------
                    IF ((NNOE.EQ.3) .AND. (IMO.EQ.0)) ILAGR = 1
                    NUMA = -NUMA
                    N1 = ZZNSUP(ILIMA,NUMA)

C ---   BOUCLE 1 SUR LES NOEUDS DE L'ELEMENT :
C       ------------------------------------
                    DO 220 K1 = 1,NNOE
                      N1 = ZZNEMA(ILIMA,NUMA,K1)
                      IF (N1.LT.0) THEN
                        N1 = -N1
                        IAD1 = ZZPRNO(ILINU,N1,1)
                        CALL CORDDL(ADMODL,LCMODL,IDPRN1,IDPRN2,ILINU,
     &                              MODE,NEC,NCMP,N1,K1,NDDL1,
     &                              POSDDL(1,K1))

C --- SI NOEUD LAGR ( ILAGR=1,NDDL1=1,N1<0,NUMA<0 ) ALORS CONL(IAD1)=R
C --- ET POUR TOUTE LA MATRICE ELEMENTAIRE ON POSE R = COEF*LICOEF(IMAT)
C     ------------------------------------------------------------------
                        IF ((ILAGR.EQ.1) .AND. (NDDL1.EQ.1)) THEN
                          R = COEF*LICOEF(IMAT)
                          ZR(IDCONL-1+ZI(JNUEQ-1+IAD1)) = R
                        END IF

                        IF (NDDL1.GT.NDDLMA) THEN
                          VALI (1) = NDDL1
                          VALI (2) = NDDLMA
      CALL U2MESG('F', 'ASSEMBLA_52',0,' ',2,VALI,0,0.D0)
                        END IF

                      ELSE
                        IAD1 = ZZPRNO(ILIMNU,N1,1)
                        CALL CORDDL(ADMODL,LCMODL,IDPRN1,IDPRN2,ILIMNU,
     &                              MODE,NEC,NCMP,N1,K1,NDDL1,
     &                              POSDDL(1,K1))
                        IF (NDDL1.GT.NDDLMA) THEN
                          VALI (1) = NDDL1
                          VALI (2) = NDDLMA
      CALL U2MESG('F', 'ASSEMBLA_53',0,' ',2,VALI,0,0.D0)
                        END IF

                      END IF

                      NUMLOC(K1,1) = N1
                      NUMLOC(K1,2) = IAD1
                      NUMLOC(K1,3) = NDDL1

C ---    PARCOURS DES DDLS DU NOEUD 1 :
C        ----------------------------
                      DO 210 I1 = 1,NDDL1

C ---      BOUCLE 2 SUR LES NOEUDS DE L'ELEMENT :
C          ------------------------------------
                        DO 190 K2 = 1,K1 - 1
                          IAD2 = NUMLOC(K2,2)
                          NDDL2 = NUMLOC(K2,3)

C ---       PARCOURS DES DDLS DU NOEUD 2 :
C           ----------------------------
                          DO 180 I2 = 1,NDDL2

C ---        IAD11 NUMERO D'EQUATION DU DDL 1 DANS LA MATRICE ASSEMBLEE
C ---        IAD21 NUMERO D'EQUATION DU DDL 2 DANS LA MATRICE ASSEMBLEE:
C            ----------------------------------------------------------
                            IAD11 = IAD1 + POSDDL(I1,K1) - 1
                            IAD21 = IAD2 + POSDDL(I2,K2) - 1
                            IF (IAD11.LE.IAD21) THEN
                              IADLI = IAD11
                              IADCO = IAD21

                            ELSE
                              IADLI = IAD21
                              IADCO = IAD11
                            END IF

C ---        ON STOCKE LA POSITION DANS UN BLOC DE LA MATRICE
C ---        ASSEMBLEE, DU TERME COURANT IREEL DE LA MATRICE
C ---        ELEMENTAIRE, CETTE INFORMATION SONT STOCKEE DANS KTMP2,
C ---        L'INFORMATION DU NUMERO DU BLOC N'EST PAS STOCKEE
C ---        CAR LA MATRICE ELEMENTAIRE ETANT SYMETRIQUE, LE TERME
C ---        COURANT EXISTE A LA MEME POSITION DANS LE BLOC
C ---        SUPERIEUR ET DANS LE BLOC INFERIEUR :
C            -----------------------------------
                            CALL ASRETM(IATMP2,IREEL,IDSMHC,IDSMDI,
     &                                  IADLI,IADCO)
  180                     CONTINUE
  190                   CONTINUE

C ---       TRAITEMENT DU TERME DIAGONAL DE LA MATRICE ELEMENTAIRE :
C           -----------------------------------------------------
                        K2 = K1
                        IAD2 = NUMLOC(K2,2)
                        NDDL2 = NUMLOC(K2,3)

C ---       PARCOURS DES DDLS DU NOEUD 2 :
C           ----------------------------
                        DO 200 I2 = 1,I1

C ---        IAD11 NUMERO D'EQUATION DU DDL 1 DANS LA MATRICE ASSEMBLEE
C ---        IAD21 NUMERO D'EQUATION DU DDL 2 DANS LA MATRICE ASSEMBLEE:
C            ----------------------------------------------------------
                          IAD11 = IAD1 + POSDDL(I1,K1) - 1
                          IAD21 = IAD2 + POSDDL(I2,K2) - 1
                          IF (IAD11.LE.IAD21) THEN
                            IADLI = IAD11
                            IADCO = IAD21

                          ELSE
                            IADLI = IAD21
                            IADCO = IAD11
                          END IF

C ---        ON STOCKE LA POSITION DANS UN BLOC DE LA MATRICE
C ---        ASSEMBLEE, DU TERME COURANT IREEL DE LA MATRICE
C ---        ELEMENTAIRE, CETTE INFORMATION SONT STOCKEE DANS KTMP2,
C ---        L'INFORMATION DU NUMERO DU BLOC N'EST PAS STOCKEE
C ---        CAR LA MATRICE ELEMENTAIRE ETANT SYMETRIQUE, LE TERME
C ---        COURANT EXISTE A LA MEME POSITION DANS LE BLOC
C ---        SUPERIEUR ET DANS LE BLOC INFERIEUR :
C            -----------------------------------
                          CALL ASRETM(IATMP2,IREEL,IDSMHC,IDSMDI,IADLI,
     &                                IADCO)
  200                   CONTINUE
  210                 CONTINUE
  220               CONTINUE
                  END IF

C --- POUR FINIR, ON RECOPIE EFFECTIVEMENT LES TERMES:
C --- ON AFFECTE DES VALEURS BIDON A KTMP1 POUR PASSER DANS ASCOPR
C --- (IREEL CONTIENT LE NOMBRE DE REELS A TRAITER)
C     ---------------------------------------------
                  ZI(IATMP1) = 1
                  ZI(IATMP1+1) = 1
                  IF (TYPE.EQ.1) THEN
                    CALL ASCNPR(IATMP1,IATMP2,IREEL,
     &                          IDRESL+NCMPEL* (IEL-1),1,KVALM,R)

                  ELSE IF (TYPE.EQ.2) THEN
                    CALL ASCOPC(IATMP1,IATMP2,IREEL,
     &                          IDRESL+NCMPEL* (IEL-1),NBLC,KVALM,R,0,0)
                  END IF

  230           CONTINUE
C --- FIN DU BLOC CONCERNANT LE CAS TYMAT = 'S'
C     ----------------------------------------
              END IF

              CALL JELIBE(JEXNUM(RESU(1:19)//'.RESL',IGR))
C --- FIN DU BLOC CONCERNANT LE CAS MODE DU LIGREL > 0
C     ------------------------------------------------
            END IF

  240     CONTINUE
  250   CONTINUE
  260 CONTINUE
      IF (IMO.EQ.1) THEN

C--- SI ON VIENT DE TRAITER LE MODELE :
C    -------------------------------
        CALL JEVEUO(JEXNUM(KVALM,1),'L',IDV)
        DO 270 I = 1,NEQU
          IDI = ZI(IDSMDI+I-1)
          IF (TYPE.EQ.1) R = ABS(ZR(IDV-1+IDI))
          IF (TYPE.EQ.2) R = ABS(ZC(IDV-1+IDI))
          IF ((R.NE.0.D0) .AND. (R.LT.RINF)) RINF = R
          IF ((R.NE.0.D0) .AND. (R.GT.RSUP)) RSUP = R
  270   CONTINUE
        CALL JELIBE(JEXNUM(KVALM,1))
        IF (RINF.GE.R8MAEM()) THEN
          COEF = RSUP/2.D0

        ELSE
          COEF = (RSUP+RINF)/2.D0
        END IF

        IF (NIV.EQ.2) THEN
          WRITE (IFM,9000) COEF
        END IF

        IMO = 0

C --- CREATION DE L'OBJET .CONL EN OPTION RIGI SI AU MOINS UNE CHARGE :
C     ---------------------------------------------------------------
        IF (NMALIL.GT.2) THEN
          KCONL = MATDEV(1:19)//'.CONL'
          CALL JEDETR(KCONL)
          CALL WKVECT(KCONL,BASE1//' V R',NEQU,IDCONL)
          DO 280 I = 1,NEQU
            ZR(IDCONL-1+I) = 1.D0
  280     CONTINUE
        END IF

        GO TO 50

      END IF

C     -- MISE A JOUR DE REFA(4)
      CALL JEVEUO(MATDEV//'.REFA','E',JREFA)
      IF (MOTCLE(1:4).EQ.'ZERO') THEN
        ZK24(JREFA-1+4) = OPTIO2

      ELSE
        IF (ZK24(JREFA-1+4).NE.OPTIO2) ZK24(JREFA-1+4) = '&&MELANGE'
      END IF

      CALL JEDETR(KTMP1)
      CALL JEDETR(KTMP2)
      CALL JEDETR(MATDEV//'.ADNE')
      CALL JEDETR(MATDEV//'.ADLI')
  290 CONTINUE
      CALL JEDBG2(IBID,IDBGAV)
      CALL JEDEMA()
C     CALL CHEKSD('sd_matr_asse',MATDEV,IRET)
 9000 FORMAT (1P,'COEFFICIENT DE CONDITIONNEMENT DES LAGRANGES',E12.5)
      END
