      SUBROUTINE ASSMIV(BASE,VEC,NBVEC,TLIVEC,LICOEF,NU,VECPRO,MOTCLE,
     &                  TYPE)
      IMPLICIT REAL*8 (A-H,O-Z)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ASSEMBLA  DATE 10/07/2007   AUTEUR PELLET J.PELLET 
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

      CHARACTER*(*) VEC,TLIVEC(*),VECPRO,BASE
      CHARACTER*(*) NU
      CHARACTER*4 MOTCLE
      INTEGER NBVEC,TYPE
      REAL*8 LICOEF(*),RCOEF,R
C ----------------------------------------------------------------------
C    ASSEMBLAGE "PARTICULIER" POUR CONVERGENCE EN CONTRAINTES
C    GENERALISEES
C    REALISE LE MIN DES VECT_
C    GROSSIEREMENT POMPE SUR ASSVEC
C OUT K19 VEC   : NOM DU CHAM_NO RESULTAT
C                CHAM_NO ::= CHAM_NO_GD + OBJETS PROVISOIRES POUR L'ASS.
C IN  K* BASE   : NOM DE LA BASE SUR LAQUELLE ON VEUT CREER LE CHAM_NO
C IN  I  NBVEC  : NOMBRE DE VECT_ELEM A ASSEMBLER DANS VEC
C IN  K* TLIVEC : LISTE DES VECT_ELEM A ASSEMBLER
C IN  R  LICOEF : LISTE DES COEF. MULTIPLICATEURS DES VECT_ELEM
C IN  K* NU     : NOM D'UN NUMERO_DDL
C IN  K* VECPRO: NOM D'UN CHAM_NO MODELE(NU OU VECPRO EST OBLIGATOIRE)
C IN  K4 MOTCLE : 'ZERO' OU 'CUMU'
C IN  I  TYPE   : TYPE DU VECTEUR ASSEMBLE : 1 --> REEL
C                                            2 --> COMPLEXE
C
C  S'IL EXISTE UN OBJET '&&POIDS_MAILLE' VR, PONDERATIONS POUR CHAQUE
C  MAILLE, ON S'EN SERT POUR LES OPTIONS RAPH_MECA ET FULL_MECA
C
C ----------------------------------------------------------------------
C ----------------------------------------------------------------------
C     FONCTIONS JEVEUX
C ----------------------------------------------------------------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR
C ----------------------------------------------------------------------
C     COMMUNS   JEVEUX
C ----------------------------------------------------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      CHARACTER*8 ZK8,NOMACR,EXIELE
      CHARACTER*14 NUM2
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*24 VALK(4)
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C ----------------------------------------------------------------------
C     COMMUNS   LOCAUX DE L'OPERATEUR ASSE_VECTEUR
C ----------------------------------------------------------------------
      INTEGER GD,NEC,NLILI,DIGDEL
C ---------------------------------------------------------------------
C     VARIABLES LOCALES
C ---------------------------------------------------------------------
      PARAMETER (NBECMX=10)
      INTEGER ICODLA(NBECMX),ICODGE(NBECMX)
      CHARACTER*1 BAS
      CHARACTER*8 VECEL,MA,MO,MO2,NOGDSI,NOGDCO,NOMCAS
      CHARACTER*8 KBID
      CHARACTER*14 NUDEV
      CHARACTER*19 VECAS,VPROF
      CHARACTER*24 KMAILA,K24PRN,KNULIL,KVELIL,KVEREF,KVEDSC,RESU,NOMLI,
     &             KNEQUA,KVALE,NOMOPT
      CHARACTER*1  K1BID
      INTEGER      ADMODL, LCMODL, NBEC, EPDMS, JPDMS
C ----------------------------------------------------------------------
C     FONCTIONS LOCALES D'ACCES AUX DIFFERENTS CHAMPS DES
C     S.D. MANIPULEES DANS LE SOUS PROGRAMME
C ----------------------------------------------------------------------
      INTEGER ZZCONX,ZZNBNE,ZZLIEL,ZZNGEL,ZZNSUP,ZZNELG,ZZNELS
      INTEGER ZZNEMA,ZZPRNO,IZZPRN
      INTEGER VALI(4)
      REAL*8  R8MAEM

      ZZCONX(IMAIL,J) = ZI(ICONX1-1+ZI(ICONX2+IMAIL-1)+J-1)

C --- NBRE DE NOEUDS DE LA MAILLE IMAIL DU MAILLAGE
      ZZNBNE(IMAIL) = ZI(ICONX2+IMAIL) - ZI(ICONX2+IMAIL-1)

C --- FONCTION D ACCES AUX ELEMENTS DES CHAMPS LIEL DES S.D. LIGREL
C     REPERTORIEES DANS LE REPERTOIRE TEMPORAIRE .MATAS.LILI
C     ZZLIEL(ILI,IGREL,J) =
C      SI LA JIEME MAILLE DU LIEL IGREL DU LIGREL ILI EST:
C          -UNE MAILLE DU MAILLAGE : SON NUMERO DANS LE MAILLAGE
C          -UNE MAILLE TARDIVE : -POINTEUR DANS LE CHAMP .NEMA
      ZZLIEL(ILI,IGREL,J) = ZI(ZI(IADLIE+3* (ILI-1)+1)-1+
     &                      ZI(ZI(IADLIE+3* (ILI-1)+2)+IGREL-1)+J-1)

C --- NBRE DE GROUPES D'ELEMENTS (DE LIEL) DU LIGREL ILI
      ZZNGEL(ILI) = ZI(IADLIE+3* (ILI-1))

C --- NBRE DE NOEUDS DE LA MAILLE TARDIVE IEL ( .NEMA(IEL))
C     DU LIGREL ILI REPERTOIRE .LILI
C     (DIM DU VECTEUR D'ENTIERS .LILI(ILI).NEMA(IEL) )
      ZZNSUP(ILI,IEL) = ZI(ZI(IADNEM+3* (ILI-1)+2)+IEL) -
     &                  ZI(ZI(IADNEM+3* (ILI-1)+2)+IEL-1) - 1

C --- NBRE D ELEMENTS DU LIEL IGREL DU LIGREL ILI DU REPERTOIRE TEMP.
C     .MATAS.LILI(DIM DU VECTEUR D'ENTIERS .LILI(ILI).LIEL(IGREL) )
      ZZNELG(ILI,IGREL) = ZI(ZI(IADLIE+3* (ILI-1)+2)+IGREL) -
     &                    ZI(ZI(IADLIE+3* (ILI-1)+2)+IGREL-1) - 1

C --- NBRE D ELEMENTS SUPPLEMENTAIRE (.NEMA) DU LIGREL ILI DU
C     REPERTOIRE TEMPORAIRE .MATAS.LILI
      ZZNELS(ILI) = ZI(IADNEM+3* (ILI-1))

C --- FONCTION D ACCES AUX ELEMENTS DES CHAMPS NEMA DES S.D. LIGREL
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

C --- FONCTION D ACCES AUX ELEMENTS DES CHAMPS PRNO DES S.D. LIGREL
C     REPERTORIEES DANS NU.LILI DE LA S.D. NUME_DDL ET A LEURS ADRESSES
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

C --- DEBUT ------------------------------------------------------------
      CALL JEMARQ()

C-----RECUPERATION DU NIVEAU D'IMPRESSION

      CALL INFNIV(IFM,NIV)

C     IFM = IUNIFI('MESSAGE')
C----------------------------------------------------------------------

C --- VERIF DE MOTCLE:
      IF (MOTCLE(1:4).EQ.'ZERO') THEN

      ELSE IF (MOTCLE(1:4).EQ.'CUMU') THEN

      ELSE
        CALL U2MESK('F','ASSEMBLA_8',1,MOTCLE)
      END IF
C
      CALL JEVEUO(JEXATR('&CATA.TE.MODELOC','LONCUM'),'L',LCMODL)
      CALL JEVEUO(JEXNUM('&CATA.TE.MODELOC',1),'L',ADMODL)

      VECAS = VEC
      BAS = BASE

C --- SI LE CONCEPT VECAS EXISTE DEJA,ON LE DETRUIT:
      CALL DETRSD('CHAMP_GD',VECAS)
      CALL WKVECT(VECAS//'.LIVE',BAS//' V K8 ',NBVEC,ILIVEC)
      DO 10 I = 1,NBVEC
        ZK8(ILIVEC-1+I) = TLIVEC(I)
   10 CONTINUE

C --- NOMS DES PRINCIPAUX OBJETS JEVEUX LIES A VECAS
      KMAILA = '&MAILLA                 '
      KVELIL = VECAS//'.LILI'
      KVEREF = VECAS//'.REFE'
      KVALE = VECAS//'.VALE'
      KVEDSC = VECAS//'.DESC'

C --- CREATION DE REFE ET DESC
      CALL JECREO(KVEREF,BAS//' V K24')
      CALL JEECRA(KVEREF,'LONMAX',2,' ')
      CALL JEVEUO(KVEREF,'E',IDVERF)
      CALL JECREO(KVEDSC,BAS//' V I')
      CALL JEECRA(KVEDSC,'LONMAX',2,' ')
      CALL JEECRA(KVEDSC,'DOCU',IBID,'CHNO')
      CALL JEVEUO(KVEDSC,'E',IDVEDS)

C --- CALCUL D UN LILI POUR VECAS
C --- CREATION D'UN VECAS(1:19).ADNE ET VECAS(1:19).ADLI SUR 'V'
      CALL CRELIL(NBVEC,ILIVEC,KVELIL,'V',KMAILA,VECAS,GD,MA,NEC,NCMP,
     &            ILIM,NLILI,NBELM)
      CALL JEVEUO(VECAS(1:19)//'.ADLI','E',IADLIE)
      CALL JEVEUO(VECAS(1:19)//'.ADNE','E',IADNEM)
      CALL JEEXIN(MA(1:8)//'.CONNEX',IRET)
      IF (IRET.GT.0) THEN
        CALL JEVEUO(MA(1:8)//'.CONNEX','L',ICONX1)
        CALL JEVEUO(JEXATR(MA(1:8)//'.CONNEX','LONCUM'),'L',ICONX2)
      END IF

C --- ON SUPPOSE QUE LE LE LIGREL DE &MAILLA EST LE PREMIER DE LILINU
      ILIMNU = 1

C --- NOMS DES PRINCIPAUX OBJETS JEVEUX LIES A NU
C --- IL FAUT ESPERER QUE LE CHAM_NO EST EN INDIRECTION AVEC UN
C     PROF_CHNO APPARTENANT A UNE NUMEROTATION SINON CA VA PLANTER
C     DANS LE JEVEUO SUR KNEQUA
      NUDEV = NU
      IF (NUDEV(1:1).EQ.' ') THEN
        VPROF = VECPRO
        CALL JEVEUO(VPROF//'.REFE','L',IDVREF)
        NUDEV = ZK24(IDVREF-1+2) (1:14)
      END IF

      KNEQUA = NUDEV//'.NUME.NEQU'
      K24PRN = NUDEV//'.NUME.PRNO'
      KNULIL = NUDEV//'.NUME.LILI'
      CALL JEVEUO(NUDEV//'.NUME.NUEQ','L',IANUEQ)

      CALL DISMOI('F','NOM_MODELE',NUDEV,'NUME_DDL',IBID,MO,IERD)
      CALL DISMOI('F','NOM_MAILLA',NUDEV,'NUME_DDL',IBID,MA,IERD)
      CALL DISMOI('F','NB_NO_SS_MAX',MA,'MAILLAGE',NBNOSS,KBID,IERD)

C     100 EST SUPPOSE ETRE LA + GDE DIMENSION D'UNE MAILLE STANDARD:
      NBNOSS = MAX(NBNOSS,100)
C     -- NUMLOC(K,INO) (K=1,3)(INO=1,NBNO(MAILLE))
      CALL WKVECT('&&ASSVEC.NUMLOC','V V I',3*NBNOSS,IANULO)

      CALL DISMOI('F','NOM_GD',NUDEV,'NUME_DDL',IBID,NOGDCO,IERD)
      CALL DISMOI('F','NOM_GD_SI',NOGDCO,'GRANDEUR',IBID,NOGDSI,IERD)
      CALL DISMOI('F','NB_CMP_MAX',NOGDSI,'GRANDEUR',NMXCMP,KBID,IERD)
      CALL DISMOI('F','NUM_GD_SI',NOGDSI,'GRANDEUR',NUGD,KBID,IERD)
      NEC = NBEC(NUGD)
      NCMP = NMXCMP

      DO 20 I = 1,NBECMX
        ICODLA(I) = 0
        ICODGE(I) = 0
   20 CONTINUE

C     -- POSDDL(ICMP) (ICMP=1,NMXCMP(GD_SI))
      CALL WKVECT('&&ASSVEC.POSDDL','V V I',NMXCMP,IAPSDL)

C     -- ON PREPARE L'ASSEMBLAGE DES SOUS-STRUCTURES:
C     -----------------------------------------------
      CALL DISMOI('F','NB_NO_MAILLA',MO,'MODELE',NM,KBID,IER)

      CALL JEEXIN(MA//'.NOMACR',IRET)
      IF (IRET.GT.0) THEN
        CALL JEVEUO(MA//'.NOMACR','L',IANMCR)
        CALL JEVEUO(JEXNOM('&CATA.GD.NOMCMP',NOGDSI),'L',IANCMP)
        CALL JELIRA(JEXNOM('&CATA.GD.NOMCMP',NOGDSI),'LONMAX',LGNCMP,
     &              KBID)
        ICMP = INDIK8(ZK8(IANCMP),'LAGR',1,LGNCMP)
        IF (ICMP.EQ.0) CALL U2MESS('F','ASSEMBLA_9')
        IF (ICMP.GT.30) CALL U2MESS('F','ASSEMBLA_10')
C       -- ICODLA EST L'ENTIER CODE CORRESPONDANT A LA CMP "LAGR"
        JEC = (ICMP-1)/30 + 1
        ICODLA(JEC) = 2**ICMP
C        ICODLA = 2**ICMP
      END IF


C ---  RECUPERATION DE PRNO
      CALL JEVEUO(K24PRN,'L',IDPRN1)
      CALL JEVEUO(JEXATR(K24PRN,'LONCUM'),'L',IDPRN2)

C ---  RECUPERATION DE NEQUA
      CALL JEVEUO(KNEQUA,'L',IDNEQU)
      NEQUA = ZI(IDNEQU)

C ---  REMPLISSAGE DE REFE ET DESC
      ZK24(IDVERF) = MA
      ZK24(IDVERF+1) = K24PRN(1:14)//'.NUME'
      ZI(IDVEDS) = GD
      ZI(IDVEDS+1) = 1


C --- ALLOCATION VALE EN R OU C SUIVANT TYPE
      IF (TYPE.EQ.1) THEN
        CALL JECREO(KVALE,BAS//' V R8')

      ELSE IF (TYPE.EQ.2) THEN
        CALL JECREO(KVALE,BAS//' V C16')

      ELSE
        CALL U2MESS('F','ASSEMBLA_11')
      END IF

      CALL JEECRA(KVALE,'LONMAX',NEQUA,' ')
      CALL JEVEUO(KVALE,'E',IADVAL)

      DO 666 I=1,NEQUA
        ZR(IADVAL+I-1)=R8MAEM()
 666   CONTINUE


C --- REMPLISSAGE DE .VALE
C ------------------------
      DO 190 IMAT = 1,NBVEC
        RCOEF = LICOEF(IMAT)
        VECEL = ZK8(ILIVEC+IMAT-1)

        CALL DISMOI('F','NOM_MODELE',VECEL,'VECT_ELEM',IBID,MO2,IERD)
        IF (MO2.NE.MO) CALL U2MESS('F','ASSEMBLA_5')


C       -- TRAITEMENT DES SOUS-STRUCTURES (JUSQU A FIN BOUCLE 738)
C       ----------------------------------------------------------
        CALL DISMOI('F','EXI_ELEM',MO,'MODELE',IBID,EXIELE,IERD)
        CALL DISMOI('F','NB_SS_ACTI',VECEL,'VECT_ELEM',NBSSA,KBID,IERD)

        IF (NBSSA.GT.0) THEN
          NOMCAS = ' '
          CALL DISMOI('F','NB_SM_MAILLA',MO,'MODELE',NBSMA,KBID,IERD)
          CALL DISMOI('F','NOM_MAILLA',MO,'MODELE',IBID,MA,IERD)
          CALL JEVEUO(MO//'.MODELE    .SSSA','L',IASSSA)
          CALL SSVALV('DEBUT',NOMCAS,MO,MA,0,IDRESL,NCMPEL)
          CALL JELIRA(VECEL//'.LISTE_CHAR','NUTIOC',NBCHAR,KBID)

          DO 90 ICHAR = 1,NBCHAR
            CALL JENUNO(JEXNUM(VECEL//'.LISTE_CHAR',ICHAR),NOMCAS)
            CALL JEVEUO(JEXNUM(VECEL//'.LISTE_CHAR',ICHAR),'L',IALCHA)

            DO 80 IMA = 1,NBSMA
C             -- ON N'ASSEMBLE QUE LES SSS VRAIMENT ACTIVES :
              IF (ZI(IASSSA-1+IMA).EQ.0) GO TO 80
              IF (ZI(IALCHA-1+IMA).EQ.0) GO TO 80
              CALL JEVEUO(JEXNUM(MA//'.SUPMAIL',IMA),'L',IAMAIL)
              CALL JELIRA(JEXNUM(MA//'.SUPMAIL',IMA),'LONMAX',NNOE,KBID)
              CALL SSVALV(' ',NOMCAS,MO,MA,IMA,IDRESL,NCMPEL)

              NOMACR = ZK8(IANMCR-1+IMA)
              CALL DISMOI('F','NOM_NUME_DDL',NOMACR,'MACR_ELEM_STAT',
     &                    IBID,NUM2,IERD)
              CALL JEVEUO(NOMACR//'.CONX','L',IACONX)
              CALL JEVEUO(JEXNUM(NUM2//'.NUME.PRNO',1),'L',IAPROL)

              IL = 0
              DO 70 K1 = 1,NNOE
                N1 = ZI(IAMAIL-1+K1)
                IF (N1.GT.NM) THEN
                  DO 30 IEC = 1,NBECMX
                    ICODGE(IEC) = ICODLA(IEC)
   30             CONTINUE
                ELSE
                  INOLD = ZI(IACONX-1+3* (K1-1)+2)
                  DO 40 IEC = 1,NEC
                    ICODGE(IEC) = ZI(IAPROL-1+ (NEC+2)* (INOLD-1)+2+IEC)
   40             CONTINUE
                END IF

                IAD1 = ZI(IDPRN1-1+ZI(IDPRN2+ILIMNU-1)+ (N1-1)* (NEC+2))
                CALL CORDD2(IDPRN1,IDPRN2,ILIMNU,ICODGE,NEC,NCMP,N1,
     &                      NDDL1,ZI(IAPSDL))

                IF (TYPE.EQ.1) THEN
                  DO 50 I1 = 1,NDDL1
                    IL = IL + 1
                    ZR(IADVAL-1+ZI(IANUEQ-1+IAD1+ZI(IAPSDL-1+I1)-
     &                1)) = ZR(IADVAL-1+ZI(IANUEQ-1+IAD1+ZI(IAPSDL-1+
     &                I1)-1)) + ZR(IDRESL+IL-1)*RCOEF
   50             CONTINUE

                ELSE IF (TYPE.EQ.2) THEN
                  DO 60 I1 = 1,NDDL1
                    IL = IL + 1
                    ZC(IADVAL-1+ZI(IANUEQ-1+IAD1+ZI(IAPSDL-1+I1)-
     &                1)) = ZC(IADVAL-1+ZI(IANUEQ-1+IAD1+ZI(IAPSDL-1+
     &                I1)-1)) + ZC(IDRESL+IL-1)*RCOEF
   60             CONTINUE
                END IF

   70         CONTINUE
   80       CONTINUE
   90     CONTINUE
          CALL SSVALV('FIN',NOMCAS,MO,MA,0,IDRESL,NCMPEL)
        END IF


C         -- TRAITEMENT DES ELEMENTS FINIS CLASSIQUES (FIN BOUCLE 510)
C         -----------------------------------------------------------
        CALL JEEXIN(VECEL//'.LISTE_RESU',IRET)
        IF (IRET.GT.0) THEN


C---- BOUCLE SUR LES VECT_ELEM:
C------------------------------
          CALL JEVEUO(VECEL//'.LISTE_RESU','L',IDLRES)
          CALL JELIRA(VECEL//'.LISTE_RESU','LONUTI ',NBRESU,K1BID)
          DO 180 IRESU = 1,NBRESU
            RESU = ZK24(IDLRES+IRESU-1)
            CALL JEVEUO(RESU(1:19)//'.NOLI','L',IAD)
            NOMLI = ZK24(IAD)
            NOMOPT = ZK24(IAD+1)

C---- TEST EXISTENCE &&POIDS_MAILLE
C------------------------------
            IF ( NOMOPT(1:9).EQ.'FULL_MECA'.OR.
     &           NOMOPT(1:9).EQ.'RAPH_MECA'     ) THEN
              CALL JEEXIN('&&POIDS_MAILLE',EPDMS)
              IF (EPDMS.GT.0) CALL JEVEUO('&&POIDS_MAILLE','L',JPDMS)
            ELSE
              EPDMS = 0
            ENDIF

            CALL JENONU(JEXNOM(KVELIL,NOMLI),ILIVE)
            CALL JENONU(JEXNOM(KNULIL,NOMLI),ILINU)
            DO 170 IGR = 1,ZI(IADLIE+3* (ILIVE-1))
              CALL JEVEUO(RESU(1:19)//'.DESC','L',IDDESC)
              MODE = ZI(IDDESC+IGR+1)
              IF (MODE.GT.0) THEN
                NNOE = NBNO(MODE)
                NEL = ZI(ZI(IADLIE+3* (ILIVE-1)+2)+IGR) -
     &                ZI(ZI(IADLIE+3* (ILIVE-1)+2)+IGR-1) - 1
                CALL JEVEUO(JEXNUM(RESU(1:19)//'.RESL',IGR),'L',IDRESL)
                NCMPEL = DIGDEL(MODE)
                DO 160 IEL = 1,NEL
                  NUMA = ZI(ZI(IADLIE+3* (ILIVE-1)+1)-1+
     &                   ZI(ZI(IADLIE+3* (ILIVE-1)+2)+IGR-1)+IEL-1)
                  R = RCOEF
                  IF (NUMA.GT.0) THEN
                    IF (EPDMS.GT.0) R=R*ZR(JPDMS-1+NUMA)
                    IL = 0
                    DO 120 K1 = 1,NNOE
                      N1 = ZI(ICONX1-1+ZI(ICONX2+NUMA-1)+K1-1)
                      IAD1 = ZI(IDPRN1-1+ZI(IDPRN2+ILIMNU-1)+
     &                       (N1-1)* (NEC+2)+1-1)
                      CALL CORDDL(ADMODL,LCMODL,IDPRN1,IDPRN2,ILIMNU,
     &                            MODE,NEC,NCMP,N1,K1,NDDL1,ZI(IAPSDL))
                      IF (NDDL1.EQ.0) GO TO 120
                      IF (IAD1.EQ.0) THEN
                        VALI (1) = N1
                        VALK (1) = RESU
                        VALK (2) = VECEL
                        VALK (3) = NUDEV
      CALL U2MESG('F', 'ASSEMBLA_41',3,VALK,1,VALI,0,0.D0)
                      END IF

                      IF (IAD1.GT.NEQUA) THEN
                        VALI (1) = N1
                        VALI (2) = IAD1
                        VALI (3) = NEQUA
                        VALK (1) = RESU
                        VALK (2) = VECEL
      CALL U2MESG('F', 'ASSEMBLA_42',2,VALK,3,VALI,0,0.D0)
                      END IF

                      IF (NDDL1.GT.100) THEN
                        VALI (1) = NDDL1
                        VALI (2) = 100
      CALL U2MESG('F', 'ASSEMBLA_43',0,' ',2,VALI,0,0.D0)
                      END IF

                      IF (TYPE.EQ.1) THEN
CCDIR$ IVDEP
                        DO 100 I1 = 1,NDDL1
                          IL = IL + 1
                          ZR(IADVAL-1+ZI(IANUEQ-1+IAD1+ZI(IAPSDL-1+I1)-
     &                      1)) = MIN(ZR(IADVAL-1+ZI(IANUEQ-1+IAD1+
     &                            ZI(IAPSDL-1+I1)-1)),
     &                            ZR(IDRESL+ (IEL-1)*NCMPEL+IL-1)*R)
  100                   CONTINUE

                      ELSE
CCDIR$ IVDEP
                        DO 110 I1 = 1,NDDL1
                          IL = IL + 1
                          ZC(IADVAL-1+ZI(IANUEQ-1+IAD1+ZI(IAPSDL-1+I1)-
     &                      1)) = ZC(IADVAL-1+ZI(IANUEQ-1+IAD1+
     &                            ZI(IAPSDL-1+I1)-1)) +
     &                            ZC(IDRESL+ (IEL-1)*NCMPEL+IL-1)*R
  110                   CONTINUE
                      END IF

  120               CONTINUE

                  ELSE
                    NUMA = -NUMA
                    N1 = ZI(ZI(IADNEM+3* (ILIVE-1)+2)+NUMA) -
     &                   ZI(ZI(IADNEM+3* (ILIVE-1)+2)+NUMA-1) - 1
                    IF (NNOE.NE.N1) THEN
                        VALK (1) = VECEL
                        VALK (2) = RESU
                        VALK (3) = NOMLI
                        VALI (1) = IGR
                        VALI (2) = NUMA
                        VALI (3) = N1
                        VALI (4) = NNOE
      CALL U2MESG('F', 'ASSEMBLA_44',3,VALK,4,VALI,0,0.D0)
                    END IF

                    IL = 0
                    DO 150 K1 = 1,NNOE
                      N1 = ZI(ZI(IADNEM+3* (ILIVE-1)+1)-1+
     &                     ZI(ZI(IADNEM+3* (ILIVE-1)+2)+NUMA-1)+K1-1)
                      IF (N1.LT.0) THEN
                        N1 = -N1
                        IF (ILINU.EQ.0) THEN
                        VALK (1) = NOMLI
                        VALK (2) = RESU
                        VALK (3) = VECEL
                        VALK (4) = NUDEV
                        VALI (1) = N1
                        VALI (2) = NUMA
      CALL U2MESG('F', 'ASSEMBLA_45',4,VALK,2,VALI,0,0.D0)
                        END IF

                        IAD1 = ZI(IDPRN1-1+ZI(IDPRN2+ILINU-1)+
     &                         (N1-1)* (NEC+2)+1-1)
                        CALL CORDDL(ADMODL,LCMODL,IDPRN1,IDPRN2,ILINU,
     &                            MODE,NEC,NCMP,N1,K1,NDDL1,ZI(IAPSDL))
                        IF (NDDL1.GT.100) THEN
                        VALI (1) = NDDL1
                        VALI (2) = 100
      CALL U2MESG('F', 'ASSEMBLA_46',0,' ',2,VALI,0,0.D0)
                        END IF

                      ELSE
                        IAD1 = ZI(IDPRN1-1+ZI(IDPRN2+ILIMNU-1)+
     &                         (N1-1)* (NEC+2)+1-1)
                        CALL CORDDL(ADMODL,LCMODL,IDPRN1,IDPRN2,ILIMNU,
     &                            MODE,NEC,NCMP,N1,K1,NDDL1,ZI(IAPSDL))
                        IF (NDDL1.GT.100) THEN
                        VALI (1) = NDDL1
                        VALI (2) = 100
      CALL U2MESG('F', 'ASSEMBLA_47',0,' ',2,VALI,0,0.D0)
                        END IF

                      END IF

                      IF (IAD1.EQ.0) THEN
                        VALI (1) = N1
                        VALK (1) = RESU
                        VALK (2) = VECEL
                        VALK (3) = NUDEV
      CALL U2MESG('F', 'ASSEMBLA_48',3,VALK,1,VALI,0,0.D0)
                      END IF

                      IF (IAD1.GT.NEQUA) THEN
                        VALI (1) = N1
                        VALI (2) = IAD1
                        VALI (3) = NEQUA
                        VALK (1) = RESU
                        VALK (2) = VECEL
      CALL U2MESG('F', 'ASSEMBLA_49',2,VALK,3,VALI,0,0.D0)
                      END IF

                      IF (TYPE.EQ.1) THEN
CCDIR$ IVDEP
                        DO 130 I1 = 1,NDDL1
                          IL = IL + 1
                          ZR(IADVAL-1+ZI(IANUEQ-1+IAD1+ZI(IAPSDL-1+I1)-
     &                      1)) = MIN(ZR(IADVAL-1+ZI(IANUEQ-1+IAD1+
     &                            ZI(IAPSDL-1+I1)-1)),
     &                            ZR(IDRESL+ (IEL-1)*NCMPEL+IL-1)*R)
  130                   CONTINUE

                      ELSE
CCDIR$ IVDEP
                        DO 140 I1 = 1,NDDL1
                          IL = IL + 1
                          ZC(IADVAL-1+ZI(IANUEQ-1+IAD1+ZI(IAPSDL-1+I1)-
     &                      1)) = ZC(IADVAL-1+ZI(IANUEQ-1+IAD1+
     &                            ZI(IAPSDL-1+I1)-1)) +
     &                            ZC(IDRESL+ (IEL-1)*NCMPEL+IL-1)*R
  140                   CONTINUE
                      END IF

  150               CONTINUE
                  END IF

  160           CONTINUE
                CALL JELIBE(JEXNUM(RESU(1:19)//'.RESL',IGR))
              END IF

  170       CONTINUE
  180     CONTINUE
        END IF

  190 CONTINUE
      CALL JEDETR(VECAS//'.LILI')
      CALL JEDETR(VECAS//'.LIVE')
      CALL JEDETR(VECAS//'.ADNE')
      CALL JEDETR(VECAS//'.ADLI')
C      IF (NIV.EQ.2) THEN
C        WRITE (IFM,*) ' --- '
C        WRITE (IFM,*) ' --- VECTEUR ASSEMBLE '

C        WRITE (IFM,*) ' --- '
C        IF (TYPE.EQ.1) THEN
C          DO 1000 IEQUA = 1,NEQUA
C            WRITE (IFM,*) ' -   CHAM_NO( ',IEQUA,' ) = ',
C     +        ZR(IADVAL+IEQUA-1)
C 1000     CONTINUE

C        ELSE
C          DO 1001 IEQUA = 1,NEQUA
C            WRITE (IFM,*) ' -   CHAM_NO( ',IEQUA,' ) = ',
C     +        ZC(IADVAL+IEQUA-1)
C 1001     CONTINUE
C        END IF

C        WRITE (IFM,*) ' --------------------------- '
C      END IF

C      IF (NIV.EQ.2) THEN
C        WRITE (IFM,*) ' --- '
C        WRITE (IFM,*) ' --- REFE DU VECTEUR    CREE '
C        WRITE (IFM,*) ' --- '
C        WRITE (IFM,*) ' -   REFE(1) = MAILLAGE        ',ZK24(IDVERF)
C        WRITE (IFM,*) ' -   REFE(2) = NUMEROTATION    ',ZK24(IDVERF+1)
C        WRITE (IFM,*) ' --------------------------- '
C      END IF
      CALL JEDETR('&&ASSVEC.POSDDL')
      CALL JEDETR('&&ASSVEC.NUMLOC')
      CALL JEDEMA()
      END
