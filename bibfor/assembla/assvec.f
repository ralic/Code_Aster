      SUBROUTINE ASSVEC(BASE,VEC,NBVEC,TLIVEC,LICOEF,NU,VECPRO,MOTCLE,
     &                  TYPE)
      IMPLICIT REAL*8 (A-H,O-Z)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ASSEMBLA  DATE 03/11/2008   AUTEUR PELLET J.PELLET 
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

      CHARACTER*(*) VEC,TLIVEC(*),VECPRO,BASE,NU
      CHARACTER*4 MOTCLE
      INTEGER NBVEC,TYPE
      REAL*8 LICOEF(*),RCOEF,R
C ----------------------------------------------------------------------
C OUT K19 VEC   : NOM DU CHAM_NO RESULTAT
C                CHAM_NO ::= CHAM_NO_GD + OBJETS PROVISOIRES POUR L'ASS.
C IN  K* BASE   : NOM DE LA BASE SUR LAQUELLE ON VEUT CREER LE CHAM_NO
C IN  I  NBVEC  : NOMBRE DE VECT_ELEM A ASSEMBLER DANS VEC
C IN  K* TLIVEC : LISTE DES VECT_ELEM A ASSEMBLER
C IN  R  LICOEF : LISTE DES COEF. MULTIPLICATEURS DES VECT_ELEM
C IN  K14 NU    : NOM D'UN NUME_DDL (LE STOCKAGE N'EST PAS NECESSAIRE)
C
C IN  K* VECPRO: NOM D'UN CHAM_NO MODELE(NU OU VECPRO EST OBLIGATOIRE)
C IN  K4 MOTCLE : 'ZERO' OU 'CUMU'
C IN  I  TYPE   : TYPE DU VECTEUR ASSEMBLE : 1 --> REEL
C                                            2 --> COMPLEXE
C
C  S'IL EXISTE UN OBJET '&&POIDS_MAILLE' VR, PONDERATIONS POUR CHAQUE
C  MAILLE, ON S'EN SERT POUR LES OPTIONS RAPH_MECA ET FULL_MECA
C
C----------------------------------------------------------------------
C     FONCTIONS JEVEUX
C ----------------------------------------------------------------------
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
      CHARACTER*24 VALK(5)
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

      CHARACTER*1  K1BID,BAS
      CHARACTER*8  NOMSD,KBID,MA,MO,MO2,NOGDSI,NOGDCO,NOMCAS
      CHARACTER*11 K11B
      CHARACTER*14 K14B,NUDEV
      CHARACTER*19 K19B,VECAS,VPROF,VECEL
      CHARACTER*24 METHOD,SDFETI,K24B,SDFETS,KMAILA,K24PRN,KNUEQ,
     &             KNULIL,KVELIL,KVEREF,KVEDSC,RESU,NOMLI,
     &             KVALE,NOMOPT,NOMLOG,NOMLID,INFOFE,SDFETA
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR
      LOGICAL      LFETI,LLIMO,LLICH,LLICHD,IDDOK,LFEL2,LLICHP,LFETIC,
     &             LSAUTE,LBID,LGOTO,LMUMPS,LARLQ
      INTEGER      ICODLA(NBECMX),ICODGE(NBECMX),NBEC,EPDMS,JPDMS,NBSD,
     &             IDIME,IDD,ILIGRP,IFETN,IFETC,IREFN,NBREFN,ILIGRT,
     &             ADMODL,LCMODL,ILIGRB,IRET1,ILIGRC,IFEL1,IFEL2,IFEL3,
     &             IINF,IFCPU,IBID,IFM,NIV,ILIMPI,IFEL4,IFEL5,ILIMPB,
     &             IRET2,IRET3,IAUX1,JFEL4,IAUX2,IAUX3,IAUX4,COMPT,
     &             NIVMPI,RANG,NBLOG,JMUMPS,NBPROC
      REAL*8       TEMPS(6),RBID
C ----------------------------------------------------------------------
C     FONCTIONS LOCALES D'ACCES AUX DIFFERENTS CHAMPS DES
C     S.D. MANIPULEES DANS LE SOUS PROGRAMME
C ----------------------------------------------------------------------
      INTEGER ZZCONX,ZZNBNE,ZZLIEL,ZZNGEL,ZZNSUP,ZZNELG,ZZNELS
      INTEGER ZZNEMA,ZZPRNO,IZZPRN
      INTEGER VALI(4)

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
C     REPERTORIEES DANS NU.LILI DE LA S.D. PROF_CHNO ET A LEURS ADRESSES
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
      INFOFE='FFFFFFFFFFFFFFFFFFFFFFFF'

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
      CALL WKVECT(VECAS//'.LIVE',BAS//' V K24 ',NBVEC,ILIVEC)
      DO 10 I = 1,NBVEC
        ZK24(ILIVEC-1+I) = TLIVEC(I)
   10 CONTINUE

C --- NOMS DES PRINCIPAUX OBJETS JEVEUX LIES A VECAS
      KMAILA = '&MAILLA                 '
      KVELIL = VECAS//'.LILI'


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
      NUDEV = NU
      IF (NUDEV(1:1).EQ.' ') THEN
        VPROF = VECPRO
        CALL JEVEUO(VPROF//'.REFE','L',IDVREF)
        NUDEV = ZK24(IDVREF-1+2) (1:14)
      END IF

C --- TEST POUR SAVOIR SI LE SOLVEUR EST DE TYPE FETI
C --- NUME_DDL ET DONC CHAM_NO ETENDU, OUI OU NON ?
      CALL JELIRA(NUDEV(1:14)//'.NUME.REFN','LONMAX',NBREFN,KBID)
      IF (NBREFN.NE.4) THEN
        WRITE(IFM,*)'<FETI/ASSVEC> NUME_DDL/CHAM_NO NON ETENDU '
     &              //'POUR FETI',NUDEV(1:14)//'.NUME.REFN'
        METHOD='XXXX'
        SDFETI='XXXX'
      ELSE
        CALL JEVEUO(NUDEV(1:14)//'.NUME.REFN','L',IREFN)
        METHOD=ZK24(IREFN+2)
        SDFETI=ZK24(IREFN+3)
        SDFETS=SDFETI
        SDFETA=SDFETS(1:19)//'.FETA'
      ENDIF

      LFETI=.FALSE.
      LFETIC = .FALSE.
      NBSD=0
      IF (METHOD(1:4).EQ.'FETI') THEN
        LFETI=.TRUE.
        CALL JEVEUO(SDFETI(1:19)//'.FDIM','L',IDIME)
C NOMBRE DE SOUS-DOMAINES
        NBSD=ZI(IDIME)
C CONSTITUTION DE L'OBJET JEVEUX VECAS.FETC COMPLEMENTAIRE
        CALL WKVECT(VECAS//'.FETC',BAS//' V K24',NBSD,IFETC)
        CALL JEVEUO('&FETI.FINF','L',IINF)
        INFOFE=ZK24(IINF)
        IF (INFOFE(11:11).EQ.'T') LFETIC=.TRUE.
C PREPARATION DE DONNEES AUXILIAIRES POUR TEST
        CALL FETTSD(INFOFE,IBID,IBID,IBID,SDFETI(1:19),K24B,IBID,IBID,
     &              IBID,IFM,LBID,IBID,IBID,IBID,K19B,2,LBID)
      ENDIF

C --- TEST POUR SAVOIR SI LE SOLVEUR EST DE TYPE MUMPS DISTRIBUE
      LMUMPS=.FALSE.
      RANG=0
      NBPROC=1
      CALL JEEXIN('&MUMPS.MAILLE.NUMSD',IRET)
      IF (IRET.NE.0) THEN
        CALL MUMMPI(2,IFM,NIV,K24B,RANG,IBID)
        CALL MUMMPI(3,IFM,NIV,K24B,NBPROC,IBID)
        LMUMPS=.TRUE.
        CALL JEVEUO('&MUMPS.MAILLE.NUMSD','L',JNUMSD)
      ENDIF

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
        IF (LFETI)
     &    CALL U2MESK('F','ASSEMBLA_12',1,MA(1:8))
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

C ADRESSE JEVEUX DE LA LISTE DES NUME_DDL ASSOCIES AUX SOUS-DOMAINES
      IF (LFETI) THEN
        CALL JEVEUO(NUDEV//'.FETN','L',IFETN)
C STOCKE &&//NOMPRO(1:6)//'.2.' POUR COHERENCE AVEC L'EXISTANT
        K11B=VECAS(1:10)//'.'
C ADRESSE JEVEUX DE L'OBJET '&FETI.MAILLE.NUMSD'
        NOMLOG='&FETI.MAILLE.NUMSD'
        CALL JEVEUO(NOMLOG,'L',ILIGRP)
        ILIGRP=ILIGRP-1
C ADRESSE JEVEUX OBJET AFFICHAGE CPU
        CALL JEVEUO('&FETI.INFO.CPU.ASSE','E',IFCPU)
C ADRESSE JEVEUX OBJET FETI & MPI
        CALL JEVEUO('&FETI.LISTE.SD.MPI','L',ILIMPI)
        CALL JEVEUO('&FETI.LISTE.SD.MPIB','L',ILIMPB)
        IF (INFOFE(10:10).EQ.'T') THEN
          NIVMPI=2
        ELSE
          NIVMPI=1
        ENDIF
        CALL FETMPI(2,IBID,IFM,NIVMPI,RANG,IBID,K24B,K24B,K24B,RBID)
      ENDIF

C========================================
C BOUCLE SUR LES SOUS-DOMAINES + IF MPI:
C========================================
C IDD=0 --> DOMAINE GLOBAL/ IDD=I --> IEME SOUS-DOMAINE
      DO 195 IDD=0,NBSD

C TRAVAIL PREALABLE POUR DETERMINER SI ON EFFECTUE LA BOUCLE SUIVANT
C LE SOLVEUR (FETI OU NON), LE TYPE DE RESOLUTION (PARALLELE OU
C SEQUENTIELLE) ET L'ADEQUATION "RANG DU PROCESSEUR-NUMERO DU SD"
        IF (.NOT.LFETI) THEN
          IDDOK=.TRUE.
        ELSE
          IF (ZI(ILIMPI+IDD).EQ.1) THEN
            IDDOK=.TRUE.
          ELSE
            IDDOK=.FALSE.
          ENDIF
        ENDIF
        IF (IDDOK) THEN

        IF (LFETI) CALL JEMARQ()
C CALCUL TEMPS
        IF ((NIV.GE.2).OR.(LFETIC)) THEN
          CALL UTTCPU(90,'INIT ',6,TEMPS)
          CALL UTTCPU(90,'DEBUT',6,TEMPS)
        ENDIF
C ---  RECUPERATION DE PRNO/LILI/NUEQ
        IF (IDD.EQ.0) THEN
          K24PRN = NUDEV//'.NUME.PRNO'
          KNULIL = NUDEV//'.NUME.LILI'
          KNUEQ  = NUDEV//'.NUME.NUEQ'
        ELSE
          K14B=ZK24(IFETN+IDD-1)(1:14)
          K24PRN(1:14)=K14B
          KNULIL(1:14)=K14B
          KNUEQ(1:14)=K14B
        ENDIF
        CALL JEVEUO(K24PRN,'L',IDPRN1)
        CALL JEVEUO(JEXATR(K24PRN,'LONCUM'),'L',IDPRN2)
        CALL JEVEUO(KNUEQ,'L',IANUEQ)
        CALL JELIRA(KNUEQ,'LONMAX',NEQUA,KBID)

C ---  REMPLISSAGE DES .REFE ET .DESC
        IF (IDD.EQ.0) THEN
C SI NON FETI OU FETI DOMAINE GLOBAL
          KVEREF = VECAS//'.REFE'
          KVALE  = VECAS//'.VALE'
          KVEDSC = VECAS//'.DESC'
        ELSE
C SI SOUS-DOMAINE FETI
          CALL JENUNO(JEXNUM(SDFETA,IDD),NOMSD)
C          K19B=K11B//NOMSD
C NOUVELLE CONVENTION POUR LES CHAM_NOS FILS, GESTTION DE NOMS
C ALEATOIRES
          CALL GCNCON('.',KBID)
          KBID(1:1)='F'
          K19B=K11B(1:11)//KBID
          ZK24(IFETC+IDD-1)=K19B
          KVEREF(1:19)=K19B
          KVEDSC(1:19)=K19B
          KVALE(1:19)=K19B
C RECUPERATION DANS LE .NUME.REFN DU NOM DE LA METHODE
          CALL JEVEUO(K14B//'.NUME.REFN','L',IREFN)
          METHOD=ZK24(IREFN+2)
          SDFETI=ZK24(IREFN+3)
        ENDIF
        CALL JECREO(KVEREF,BAS//' V K24')
        CALL JEECRA(KVEREF,'LONMAX',4,' ')
        CALL JEVEUO(KVEREF,'E',IDVERF)
        CALL JECREO(KVEDSC,BAS//' V I')
        CALL JEECRA(KVEDSC,'LONMAX',2,' ')
        CALL JEECRA(KVEDSC,'DOCU',IBID,'CHNO')
        CALL JEVEUO(KVEDSC,'E',IDVEDS)
        ZK24(IDVERF) = MA
        ZK24(IDVERF+1) = K24PRN(1:14)//'.NUME'
        ZK24(IDVERF+2) = METHOD
        ZK24(IDVERF+3) = SDFETI
        ZI(IDVEDS) = GD
        ZI(IDVEDS+1) = 1

C --- ALLOCATION .VALE EN R OU C SUIVANT TYPE
        IF (TYPE.EQ.1) THEN
          CALL JECREO(KVALE,BAS//' V R8')
        ELSE IF (TYPE.EQ.2) THEN
          CALL JECREO(KVALE,BAS//' V C16')
        ELSE
          CALL U2MESS('F','ASSEMBLA_11')
        ENDIF
        CALL JEECRA(KVALE,'LONMAX',NEQUA,' ')
        CALL JEVEUO(KVALE,'E',JVALE)

C PREPARATION DE DONNEES AUXILIAIRES POUR TEST
        LGOTO=.FALSE.
        K24B(1:14)=NUDEV
        CALL FETTSD(INFOFE,IDD,NEQUA,IBID,SDFETI(1:19),K24B,IFETN,
     &              JVALE,IBID,IFM,LBID,IBID,IBID,IBID,K19B,4,LGOTO)
        IF (LGOTO) GOTO 9999

C --- REMPLISSAGE DE .VALE
C ------------------------
C==========================
C BOUCLE SUR LES VECT_ELEM
C==========================
        DO 190 IMAT = 1,NBVEC
          RCOEF = LICOEF(IMAT)
          VECEL = ZK24(ILIVEC+IMAT-1)(1:19)
          CALL DISMOI('F','NOM_MODELE',VECEL,'VECT_ELEM',IBID,MO2,IERD)
          IF (MO2.NE.MO) CALL U2MESS('F','ASSEMBLA_5')

C       -- TRAITEMENT DES SOUS-STRUCTURES (JUSQU A FIN BOUCLE 738)
C       ----------------------------------------------------------
          CALL DISMOI('F','EXI_ELEM',MO,'MODELE',IBID,EXIELE,IERD)
          CALL DISMOI('F','NB_SS_ACTI',VECEL,'VECT_ELEM',NBSSA,KBID,
     &                IERD)
          IF (NBSSA.GT.0) THEN
            NOMCAS = ' '
            CALL DISMOI('F','NB_SM_MAILLA',MO,'MODELE',NBSMA,KBID,IERD)
            CALL DISMOI('F','NOM_MAILLA',MO,'MODELE',IBID,MA,IERD)
            CALL JEVEUO(MO//'.MODELE    .SSSA','L',IASSSA)
            CALL SSVALV('DEBUT',NOMCAS,MO,MA,0,IDRESL,NCMPEL)
            CALL JELIRA(VECEL//'.RELC','NUTIOC',NBCHAR,KBID)

            DO 90 ICHAR = 1,NBCHAR
              CALL JENUNO(JEXNUM(VECEL//'.RELC',ICHAR),NOMCAS)
              CALL JEVEUO(JEXNUM(VECEL//'.RELC',ICHAR),'L',IALCHA)

              DO 80 IMA = 1,NBSMA
C             -- ON N'ASSEMBLE QUE LES SSS VRAIMENT ACTIVES :
                IF (ZI(IASSSA-1+IMA).EQ.0) GO TO 80
                IF (ZI(IALCHA-1+IMA).EQ.0) GO TO 80
                CALL JEVEUO(JEXNUM(MA//'.SUPMAIL',IMA),'L',IAMAIL)
                CALL JELIRA(JEXNUM(MA//'.SUPMAIL',IMA),'LONMAX',NNOE,
     &                      KBID)
                CALL SSVALV(' ',NOMCAS,MO,MA,IMA,IDRESL,NCMPEL)
                NOMACR = ZK8(IANMCR-1+IMA)
                CALL DISMOI('F','NOM_NUME_DDL',NOMACR,'MACR_ELEM_STAT',
     &                     IBID,NUM2,IERD)
                CALL JEVEUO(NOMACR//'.CONX','L',IACONX)
                CALL JEVEUO(JEXNUM(NUM2//'.NUME.PRNO',1),'L',IAPROL)
                IL = 0
                DO 70 K1 = 1,NNOE
                  N1 = ZI(IAMAIL-1+K1)
                  IF (N1.GT.NM) THEN
                    DO 30 IEC = 1,NBECMX
                      ICODGE(IEC) = ICODLA(IEC)
   30               CONTINUE
                  ELSE
                    INOLD = ZI(IACONX-1+3* (K1-1)+2)
                    DO 40 IEC = 1,NEC
                      ICODGE(IEC)=ZI(IAPROL-1+(NEC+2)*(INOLD-1)+2+IEC)
   40              CONTINUE
                  END IF

                  IAD1=ZI(IDPRN1-1+ZI(IDPRN2+ILIMNU-1)+ (N1-1)* (NEC+2))
                  CALL CORDD2(IDPRN1,IDPRN2,ILIMNU,ICODGE,NEC,NCMP,N1,
     &                        NDDL1,ZI(IAPSDL))

                  IF (TYPE.EQ.1) THEN
                    DO 50 I1 = 1,NDDL1
                      IL = IL + 1
                      ZR(JVALE-1+ZI(IANUEQ-1+IAD1+ZI(IAPSDL-1+I1)-
     &                1)) = ZR(JVALE-1+ZI(IANUEQ-1+IAD1+ZI(IAPSDL-1+
     &                I1)-1)) + ZR(IDRESL+IL-1)*RCOEF
   50               CONTINUE
                  ELSE IF (TYPE.EQ.2) THEN
                    DO 60 I1 = 1,NDDL1
                      IL = IL + 1
                      ZC(JVALE-1+ZI(IANUEQ-1+IAD1+ZI(IAPSDL-1+I1)-
     &                1)) = ZC(JVALE-1+ZI(IANUEQ-1+IAD1+ZI(IAPSDL-1+
     &                I1)-1)) + ZC(IDRESL+IL-1)*RCOEF
   60               CONTINUE
                  END IF
   70           CONTINUE
   80         CONTINUE
   90       CONTINUE
            CALL SSVALV('FIN',NOMCAS,MO,MA,0,IDRESL,NCMPEL)
          ENDIF


C         -- TRAITEMENT DES ELEMENTS FINIS CLASSIQUES (FIN BOUCLE 510)
C         -----------------------------------------------------------
          CALL JEEXIN(VECEL//'.RELR',IRET)
          IF (IRET.GT.0) THEN


C==========================
C BOUCLE SUR LES RESU_ELEM
C==========================
            CALL JELIRA(VECEL//'.RELR','LONUTI ',NBRESU,K1BID)
            IF(NBRESU.GT.0)CALL JEVEUO(VECEL//'.RELR','L',IDLRES)
            DO 180 IRESU = 1,NBRESU
              RESU = ZK24(IDLRES+IRESU-1)
              CALL JEVEUO(RESU(1:19)//'.NOLI','L',IAD)
C NOM DU LIGREL GLOBAL
              NOMLI = ZK24(IAD)
              NOMOPT = ZK24(IAD+1)

C --- TEST EXISTENCE &&POIDS_MAILLE (ARLEQUIN)
              CALL JEEXIN('&&POIDS_MAILLE',EPDMS)
              IF (EPDMS.GT.0) THEN
                IF ( (NOMOPT(1:9).EQ.'FULL_MECA').OR.
     &               (NOMOPT(1:9).EQ.'RAPH_MECA').OR.
     &               (NOMOPT(1:9).EQ.'FORC_NODA')) THEN
                  CALL JEVEUO('&&POIDS_MAILLE','L',JPDMS)
                  LARLQ = .TRUE.
                ELSEIF ( (NOMOPT(1:9 ).EQ.'CHAR_MECA') .OR.
     &                   (NOMOPT(1:10).EQ.'MECA_DDLI_').OR.
     &                   (NOMOPT(1:8) .EQ.'MECA_BU_')  .OR.
     &                   (NOMOPT(1:10).EQ.'MECA_BTLA_')) THEN
                  LARLQ = .FALSE.
                ELSEIF ( (NOMOPT(1:9 ).EQ.'CHAR_MECA').OR.
     &                   (NOMOPT(1:8) .EQ.'MECA_BU_') .OR.
     &                   (NOMOPT(1:10).EQ.'MECA_BTLA_')) THEN
                  LARLQ = .FALSE.
                ELSE
                  CALL U2MESK('F','DVP_99',1,NOMOPT)
                ENDIF
              ELSE
                LARLQ = .FALSE.
              ENDIF

C--------- POUR FETI & LIGREL TARDIF: DEBUT
C RECHERCHE D'OBJET TEMPORAIRE SI FETI
C PAR DEFAUT LIGREL DE MODELE
              LLIMO=.TRUE.
              LLICH=.FALSE.
              LLICHD=.FALSE.
              LLICHP=.FALSE.
              IF (LFETI) THEN
                NOMLOG=NOMLI(1:19)//'.FEL1'
                CALL JEEXIN(NOMLOG,IRET1)
                IF (IRET1.NE.0) THEN
C LIGREL DE CHARGE A MAILLES TARDIVES
                  CALL JEVEUO(NOMLOG,'L',IFEL1)
                  LLICH=.TRUE.
                  LLIMO=.FALSE.
                  IF (IDD.EQ.0) THEN
                    LSAUTE=.TRUE.
                    CALL JELIRA(NOMLOG,'LONMAX',NBLOG,KBID)
                    DO 95 I=1,NBLOG
                      IF (ZK24(IFEL1-1+I).NE.' ') LSAUTE=.FALSE.
   95               CONTINUE
                  ELSE
                    LSAUTE=.FALSE.
                    IF (ZK24(IFEL1-1+IDD).EQ.' ') LSAUTE=.TRUE.
                  ENDIF
C LIGREL NE CONCERNANT PAS LE SOUS-DOMAINE IDD OU LE DOMAINE GLOBAL
                  IF (LSAUTE) GOTO 180

                  CALL JEEXIN(NOMLI(1:19)//'.FEL2',IRET2)
                  IF (IRET2.NE.0) THEN
C LIGREL DE CHARGE A MAILLES TARDIVES DUPLIQUEES DE FILS NOMLID
C DDL_IMPO, FORCE_NODALE...
                    LLICHD=.TRUE.
C VRAI NOM DU LIGREL DUPLIQUE CONTENU DANS PROF_CHNO.LILI LOCAL
                    IF (IDD.NE.0) NOMLID=ZK24(IFEL1-1+IDD)
                    CALL JEVEUO(NOMLI(1:19)//'.FEL2','L',IFEL2)
                    CALL JEEXIN(NOMLI(1:19)//'.FEL3',IRET3)
                    IF (IRET3.NE.0) THEN
                      CALL JEVEUO(NOMLI(1:19)//'.FEL3','L',IFEL3)
C LIGREL DE CHARGE A NOEUDS TARDIFS DUPLIQUES (DDL_IMPO...)
                      LLICHP=.TRUE.
                    ELSE
C PAS DE NOEUD TARDIF DUPLIQUE (FORCE_NODALE)
                      LLICHP=.FALSE.
                    ENDIF
                    CALL JEEXIN(NOMLI(1:19)//'.FEL4',IRET3)
                    IF (IRET3.NE.0)
     &                CALL JEVEUO(NOMLI(1:19)//'.FEL4','L',IFEL4)
                    CALL JEEXIN(NOMLI(1:19)//'.FEL5',IRET3)
                    IF (IRET3.NE.0)
     &                CALL JEVEUO(NOMLI(1:19)//'.FEL5','L',IFEL5)
                  ELSE
C LIGREL DE CHARGE NON DUPLIQUE
                    LLICHD=.FALSE.
                  ENDIF
                ELSE
C LIGREL DE MODELE
                  LLIMO=.TRUE.
                ENDIF
              ENDIF
C--------- POUR FETI & LIGREL TARDIF: FIN

C---- TEST EXISTENCE &&POIDS_MAILLE
C------------------------------

C ILIVE: INDICE DANS LIST_RESU (GLOBAL) DES VECT_ELEM.LILI DU NOMLI
C ILINU: INDICE DANS PROF_CHNO.LILI (GLOBAL OU LOCAL) DU NOMLI
              CALL JENONU(JEXNOM(KVELIL,NOMLI),ILIVE)
              IF ((LLICHD).AND.(IDD.NE.0)) THEN
                CALL JENONU(JEXNOM(KNULIL,NOMLID),ILINU)
              ELSE
                CALL JENONU(JEXNOM(KNULIL,NOMLI),ILINU)
              ENDIF

C==========================
C BOUCLE SUR LES GRELS DU LIGREL GLOBAL NOMLI/ILIMA
C==========================
              DO 170 IGR = 1,ZI(IADLIE+3* (ILIVE-1))
                CALL JEVEUO(RESU(1:19)//'.DESC','L',IDDESC)
                MODE = ZI(IDDESC+IGR+1)

                IF (MODE.GT.0) THEN
                  NNOE = NBNO(MODE)
C NOMBRE D'ELEMENTS DU GREL IGR DU LIGREL NOMLI/ILIVE
                  NEL = ZI(ZI(IADLIE+3* (ILIVE-1)+2)+IGR) -
     &                ZI(ZI(IADLIE+3* (ILIVE-1)+2)+IGR-1) - 1
                  CALL JEVEUO(JEXNUM(RESU(1:19)//'.RESL',IGR),'L',
     &                        IDRESL)
                  NCMPEL = DIGDEL(MODE)

C==========================
C BOUCLE SUR LES ELEMENTS DU GREL IGR
C==========================
                  DO 160 IEL = 1,NEL
C NUMA : NUMERO DE LA MAILLE
                    NUMA = ZI(ZI(IADLIE+3* (ILIVE-1)+1)-1+
     &                     ZI(ZI(IADLIE+3* (ILIVE-1)+2)+IGR-1)+IEL-1)

C MONITORING
                    IF ((INFOFE(5:5).EQ.'T') .AND. (LFETI)) THEN
                      WRITE(IFM,*)'<FETI/ASSVEC>','IDD',IDD,'LIGREL',
     &                  NOMLI,'ILIVE',ILIVE,'RANG',RANG
                      WRITE(IFM,*)'IGR',IGR,'IEL',IEL,'NUMA',NUMA
                      IF (LLIMO)
     &                  WRITE(IFM,*)'.LOGI',ZI(ILIGRP+ABS(NUMA))
                      IF (LLICH) THEN
                        IF (LLICHD) THEN
                          WRITE(IFM,*)'LIGREL DE CHARGE PROJETE '//
     &                      'DE FILS ',NOMLID
                        ELSE
                          WRITE(IFM,*)'LIGREL DE CHARGE INITIAL'
                        ENDIF
                        WRITE(IFM,*)'MAILLE ET/OU NOEUD TARDIF'
                      ENDIF
                    ENDIF

                    R = RCOEF

C SI ON EST DANS UN CALCUL FETI SUR UN SOUS-DOMAINE, ON SE POSE LA
C QUESTION DE L'APPARTENANCE DE LA MAILLE NUMA AU SOUS-DOMAINE IDD
                    IF (LFETI) THEN
                      IF (NUMA.GT.0) THEN
                        IF (LLICH)
     &                    CALL U2MESS('F','ASSEMBLA_6')
C ELLE APPARTIENT AU GREL IGR DU LIGREL PHYSIQUE ILIMA
                        IF (IDD.NE.0) THEN
C CHAQUE PROC ASSEMBLE LA PARTIE PHYSIQUE DES SECONDS MEMBRES FETI LE
C CONCERNANT
                          IF (ZI(ILIGRP+NUMA).NE.IDD) GOTO 160
                        ELSE
C IDEM POUR LA PARTIE PHYSIQUE DU CHAM_NO GLOBAL
                          IBID=ZI(ILIGRP+NUMA)
                          IF (IBID.GT.0) THEN
                            IF (ZI(ILIMPB+IBID-1).NE.RANG) GOTO 160
                          ENDIF
                        ENDIF
                      ELSE
C ELLE APPARTIENT AU GREL IGR DU LIGREL TARDIF ILIMA
                        IF (LLIMO)
     &                    CALL U2MESS('F','ASSEMBLA_7')
                      ENDIF
                    ENDIF


C SI ON EST DANS UN CALCUL MUMPS DISTRIBUE, ON SE POSE LA QUESTION DE
C L'APPARTENANCE DE LA MAILLE NUMA AUX DONNEES ATTRIBUEES AU PROC
C SI MAILLE PHYSIQUE: CHAQUE PROC NE TRAITE QUE CELLES ASSOCIEES AUX
C                     SD QUI LUI SONT ATTRIBUES
C SI MAILLE TARDIVE: ELLES SONT TRAITEES PAR LE PROC 0
                    IF (LMUMPS) THEN
                      IF (NUMA.GT.0) THEN
                        IF (ZI(JNUMSD-1+NUMA).NE.RANG) GOTO 160
                       ELSE
                        IF (RANG.NE.0)  GOTO 160
                      ENDIF
                    ENDIF

C---- LIGREL DE MODELE:
C--------------------
                    IF (NUMA.GT.0) THEN
                      IF (LARLQ) R=R*ZR(JPDMS-1+NUMA)
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
                        ENDIF

                        IF (IAD1.GT.NEQUA) THEN
                          VALI (1) = N1
                          VALI (2) = IAD1
                          VALI (3) = NEQUA
                          VALK (1) = RESU
                          VALK (2) = VECEL
      CALL U2MESG('F', 'ASSEMBLA_42',2,VALK,3,VALI,0,0.D0)
                        ENDIF

                        IF (NDDL1.GT.100) THEN
                          VALI (1) = NDDL1
                          VALI (2) = 100
      CALL U2MESG('F', 'ASSEMBLA_43',0,' ',2,VALI,0,0.D0)
                        ENDIF

                        IF (TYPE.EQ.1) THEN
                          DO 100 I1 = 1,NDDL1
                            IL = IL + 1
                          ZR(JVALE-1+ZI(IANUEQ-1+IAD1+ZI(IAPSDL-1+I1)-
     &                      1)) = ZR(JVALE-1+ZI(IANUEQ-1+IAD1+
     &                            ZI(IAPSDL-1+I1)-1)) +
     &                            ZR(IDRESL+ (IEL-1)*NCMPEL+IL-1)*R
  100                     CONTINUE

                        ELSE
                          DO 110 I1 = 1,NDDL1
                            IL = IL + 1
                          ZC(JVALE-1+ZI(IANUEQ-1+IAD1+ZI(IAPSDL-1+I1)-
     &                      1)) = ZC(JVALE-1+ZI(IANUEQ-1+IAD1+
     &                            ZI(IAPSDL-1+I1)-1)) +
     &                            ZC(IDRESL+ (IEL-1)*NCMPEL+IL-1)*R
  110                     CONTINUE
                        END IF

  120                 CONTINUE

                    ELSE
C---- LIGREL TARDIF:
C-------------------

                      NUMA = -NUMA

C--------- POUR FETI & LIGREL TARDIF: DEBUT
C SI POUR FETI, MAILLE TARDIVE DUPLIQUEE, ON SE POSE LA QUESTION DE
C L'APPARTENANCE DE CETTE MAILLE TARDIVE AU SOUS-DOMAINE IDD VIA
C L'OBJET .FEL2 (C'EST LE PENDANT DE &FETI.MAILLE.NUMSD POUR LES
C MAILLES DU MODELE)
                      IF (LLICHD) THEN
C LFEL2=.TRUE. ON ASSEMBLE LES CONTRIBUTIONS DE CETTE MAILLE TARDIVE
C LFEL2=.FALSE. ON LA SAUTE
                        LFEL2=.FALSE.
                        IAUX1=ZI(IFEL2+2*(NUMA-1)+1)
C C'EST UNE MAILLE TARDIVE NON SITUEE SUR UNE INTERFACE
                        IF (IAUX1.GT.0) THEN
                          IF (IDD.NE.0) THEN
C ELLE CONCERNE LE SD, ON L'ASSEMBLE
                            IF (IAUX1.EQ.IDD) LFEL2=.TRUE.
                          ELSE
                            IF (ZI(ILIMPB+IAUX1-1).EQ.RANG) LFEL2=.TRUE.
                          ENDIF
C C'EST UNE MAILLE TRADIVE SITUEE SUR UNE INTERFACE, DONC PARTAGEE
C ENTRE PLUSIEURS SOUS-DOMAINES
                        ELSE IF (IAUX1.LT.0) THEN
                          COMPT=0
                          IAUX2=(ZI(IFEL4)/3)-1
                          DO 125 JFEL4=0,IAUX2
                            IAUX3=IFEL4+3*JFEL4+3
                            IF (ZI(IAUX3).EQ.NUMA) THEN
                              COMPT=COMPT+1
                              IF (ZI(IAUX3-1).EQ.IDD) THEN
C ELLE CONCERNE LE SD, ON L'ASSEMBLE
                                LFEL2=.TRUE.
                                GOTO 126
                              ENDIF
C ON A LU TOUTES LES VALEURS POSSIBLES, ON SORT DE LA BOUCLE
                              IF (COMPT.EQ.-IAUX1) GOTO 126
                            ENDIF
  125                     CONTINUE
  126                     CONTINUE
                        ENDIF
C ON SAUTE LA CONTRIBUTION
                        IF (.NOT.LFEL2) GOTO 160
                      ENDIF
C--------- POUR FETI & LIGREL TARDIF: FIN


C N1 : NBRE DE NOEUDS DE LA MAILLE NUMA
                      N1 = ZI(ZI(IADNEM+3* (ILIVE-1)+2)+NUMA) -
     &                     ZI(ZI(IADNEM+3* (ILIVE-1)+2)+NUMA-1) - 1
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
C N1 : INDICE DU NOEUDS DS LE .NEMA DU LIGREL DE CHARGE GLOBAL OU LOCAL
                        N1 = ZI(ZI(IADNEM+3* (ILIVE-1)+1)-1+
     &                       ZI(ZI(IADNEM+3* (ILIVE-1)+2)+NUMA-1)+K1-1)
C NOEUD TARDIF
                        IF (N1.LT.0) THEN
                          N1 = -N1

C--------- POUR FETI & LIGREL TARDIF: DEBUT
C SI POUR FETI, NOEUD TARDIF DUPLIQUE, VERITABLE N1 DANS LE LIGREL DUPL
                          IF ((LLICHP).AND.(IDD.NE.0)) THEN
                            IAUX1=ZI(IFEL3+2*(N1-1)+1)
                            IF (IAUX1.GT.0) THEN
C C'EST UN NOEUD TARDIF LIE A UN DDL PHYSIQUE NON SUR L'INTERFACE
                              N1=-ZI(IFEL3+2*(N1-1))
                            ELSE IF (IAUX1.LT.0) THEN
C C'EST UN NOEUD TARDIF LIE A UN DDL PHYSIQUE DE L'INTERFACE
                              IAUX2=(ZI(IFEL5)/3)-1
                              DO 127 JFEL4=0,IAUX2
                                IAUX3=IFEL5+3*JFEL4+3
                                IF (ZI(IAUX3).EQ.N1) THEN
                                  IF (ZI(IAUX3-1).EQ.IDD) THEN
C VOICI SON NUMERO LOCAL CONCERNANT LE SD
                                    N1=-ZI(IAUX3-2)
                                    GOTO 128
                                  ENDIF
                                ENDIF
  127                         CONTINUE
  128                         CONTINUE
                            ENDIF
                          ENDIF
C--------- POUR FETI & LIGREL TARDIF: FIN

                          IF (ILINU.EQ.0) THEN
                          VALK (1) = NOMLI
                          VALK (2) = RESU
                          VALK (3) = VECEL
                          VALK (4) = NUDEV
                          VALK (5) = NOMLI(1:8)
                          VALI (1) = N1
                          VALI (2) = NUMA
      CALL U2MESG('F', 'ASSEMBLA_45',5,VALK,2,VALI,0,0.D0)
                          END IF

C NUMERO D'EQUATION DU PREMIER DDL DE N1
                          IAD1 = ZI(IDPRN1-1+ZI(IDPRN2+ILINU-1)+
     &                           (N1-1)* (NEC+2)+1-1)
                         CALL CORDDL(ADMODL,LCMODL,IDPRN1,IDPRN2,ILINU,
     &                            MODE,NEC,NCMP,N1,K1,NDDL1,ZI(IAPSDL))
                          IF (NDDL1.GT.100) THEN
                          VALI (1) = NDDL1
                          VALI (2) = 100
      CALL U2MESG('F', 'ASSEMBLA_46',0,' ',2,VALI,0,0.D0)
                          END IF
                        ELSE
C NOEUD PHYSIQUE

                          IAD1 = ZI(IDPRN1-1+ZI(IDPRN2+ILIMNU-1)+
     &                           (N1-1)* (NEC+2)+1-1)
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
                          DO 130 I1 = 1,NDDL1
                            IL = IL + 1
                          ZR(JVALE-1+ZI(IANUEQ-1+IAD1+ZI(IAPSDL-1+I1)-
     &                      1)) = ZR(JVALE-1+ZI(IANUEQ-1+IAD1+
     &                            ZI(IAPSDL-1+I1)-1)) +
     &                            ZR(IDRESL+ (IEL-1)*NCMPEL+IL-1)*R
  130                     CONTINUE
                        ELSE
                          DO 140 I1 = 1,NDDL1
                            IL = IL + 1
                          ZC(JVALE-1+ZI(IANUEQ-1+IAD1+ZI(IAPSDL-1+I1)-
     &                      1)) = ZC(JVALE-1+ZI(IANUEQ-1+IAD1+
     &                            ZI(IAPSDL-1+I1)-1)) +
     &                            ZC(IDRESL+ (IEL-1)*NCMPEL+IL-1)*R
  140                     CONTINUE
                        END IF
  150                 CONTINUE

C---- FIN IF LIGREL TARDIF:
C-------------------------
                    END IF
C---- BOUCLE SUR LES MAILLES:
C---------------------------
  160             CONTINUE
                  CALL JELIBE(JEXNUM(RESU(1:19)//'.RESL',IGR))
                ENDIF
C---- BOUCLE SUR LES LIGREL:
C---------------------------
  170         CONTINUE
C---- FIN BOUCLE SUR LES RESU_ELEM:
C--------------------------------
  180       CONTINUE
C----  FIN IF IRET
C--------------------------------
          END IF
C---- FIN BOUCLE SUR LES VECT_ELEM:
C----------------------------------
  190   CONTINUE
 9999   CONTINUE

C MONITORING
        IF (LFETI.AND.(INFOFE(1:1).EQ.'T')) THEN
          WRITE(IFM,*)
          WRITE(IFM,*)'DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD'
          IF (IDD.EQ.0) THEN
            WRITE(IFM,*)'<FETI/ASSVEC> DOMAINE GLOBAL'
          ELSE
            WRITE(IFM,*)'<FETI/ASSVEC>NUMERO DE SOUS-DOMAINE: ',IDD
          ENDIF
          WRITE(IFM,*)'<FETI/ASSVEC> REMPLISSAGE OBJETS JEVEUX ',
     &        KVALE(1:19)
          WRITE(IFM,*)
        ENDIF
        IF ((INFOFE(3:3).EQ.'T').AND.(IDD.NE.0))
     &    CALL UTIMSD(IFM,2,.FALSE.,.TRUE.,KVALE(1:19),1,' ')
        IF ((INFOFE(3:3).EQ.'T').AND.(IDD.EQ.NBSD))
     &    CALL UTIMSD(IFM,2,.FALSE.,.TRUE.,VECAS(1:19),1,' ')

        IF ((NIV.GE.2).OR.(LFETIC)) THEN
          CALL UTTCPU(90,'FIN  ',6,TEMPS)
          IF (NIV.GE.2) WRITE(IFM,'(A44,D11.4,D11.4)')
     &      'TEMPS CPU/SYS ASSEMBLAGE V                : ',TEMPS(5),
     &       TEMPS(6)
          IF (LFETIC) ZR(IFCPU+IDD)=ZR(IFCPU+IDD)+TEMPS(5)+TEMPS(6)
C POUR QUE CELA FONCTIONNE AVEC MUMPS CENTRALISE
          CALL JEEXIN('&MUMPS.INFO.CPU.ASSE',IRET)
          IF (IRET.NE.0) THEN
            CALL MUMMPI(2,IFM,NIV,K24B,RANG,IBID)
            CALL JEVEUO('&MUMPS.INFO.CPU.ASSE','E',IFCPU)
            ZR(IFCPU+RANG)=ZR(IFCPU+RANG)+TEMPS(5)+TEMPS(6)
          ENDIF
        ENDIF
        IF (LFETI) CALL JEDEMA()

C========================================
C BOUCLE SUR LES SOUS-DOMAINES + IF MPI:
C========================================
        ENDIF
  195 CONTINUE

C POUR MONITORING
C      CALL UTIMSD(IFM,2,.FALSE.,.TRUE.,VECAS(1:19),1,' ')

C        -- REDUCTION + DIFFUSION DE VECAS A TOUS LES PROC
      IF (LMUMPS)
     &  CALL MUMMPI(5,IFM,NIV,KVALE,NEQUA,IBID)

      CALL JEDETR(VECAS//'.LILI')
      CALL JEDETR(VECAS//'.LIVE')
      CALL JEDETR(VECAS//'.ADNE')
      CALL JEDETR(VECAS//'.ADLI')
      CALL JEDETR('&&ASSVEC.POSDDL')
      CALL JEDETR('&&ASSVEC.NUMLOC')
      CALL JEDEMA()
      END
