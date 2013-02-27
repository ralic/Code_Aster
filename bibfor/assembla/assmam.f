      SUBROUTINE ASSMAM(BASE,MATAS,NBMAT,TLIMAT,LICOEF,NU,
     &           MOTCLE,ITYSCA)

C  ATTENTION : CETTE ROUTINE NE DOIT PAS ETRE APPELLEE DIRECTEMENT :
C              IL FAUT APPELER SON "CHAPEAU" : ASMATR.

      IMPLICIT NONE

      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM,JEXNOM,JEXATR
      CHARACTER*(*) BASE,MATAS,TLIMAT(*),NU
      INTEGER NBMAT,ITYSCA
      REAL*8 LICOEF(*)
      CHARACTER*4 MOTCLE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ASSEMBLA  DATE 26/02/2013   AUTEUR BOITEAU O.BOITEAU 
C RESPONSABLE PELLET J.PELLET
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ASSEMBLAGE MORSE AVEC PRECONDITIONNEMENT DES MATR_ELEM DE MAILLES
C     "LAGRANGE" PAR -(MAX(!A(I,I)!)+MIN(!A(I,I)!))/2
C-----------------------------------------------------------------------
C --- DESCRIPTION DES PARAMETRES
C INT K* BASE   : BASE SUR LAQUELLE ON VEUT CREER LA MATR_ASSE
C OUT K* MATAS  :L'OBJET MATAS DE TYPE MATR_ASSE EST CREE ET REMPLI
C IN  K* MATAS  : NOM DE L'OBJET DE TYPE MATR_ASSE A CREER
C IN  I  NBMAT  : NOMBRE DE MATR_ELEM  DE LA LISTE TLIMAT
C IN  K* TLIMAT : LISTE DES MATR_ELEM
C IN  I  LICOEF : LISTE DES COEFFICIENTS MULTIPLICATEURS DES MATR_ELEM
C IN  K* NU     : NOM DU NUMERO_DDL
C IN  K4 MOTCLE : 'ZERO' OU 'CUMU'
C                 'ZERO':SI UN OBJET DE NOM MATAS ET DE TYPE
C                        MATR_ASSE EXISTE ON L'ECRASE
C                 'CUMU':SI UN OBJET DE NOM MATAS ET DE TYPE
C                        MATR_ASSE EXISTE ON L'ENRICHI
C IN  I   ITYSCA  : TYPE (R/C) DE LA MATR_ASSE
C                          1 --> REELLES
C                          2 --> COMPLEXES
C-----------------------------------------------------------------------
      CHARACTER*16 OPTIO,OPTIO2,CODVOI,NOMTE
C-----------------------------------------------------------------------
      CHARACTER*1  BASE1,TYPSCA,KBID
      CHARACTER*2  TT
      CHARACTER*8  K8BID,NOGDCO,NOGDSI,MA,MA2,MO,MO2,PARTIT
      CHARACTER*8  KNUMER,SYMEL,KEMPIC,KAMPIC,EXIVF
      CHARACTER*12 VGE
      CHARACTER*14 NUDEV,NU14
      CHARACTER*16 K16BID,NOMCMD
      CHARACTER*19 MATDEV,MAT19,RESU,MATEL,LIGRE1,LIGRE2,SDFETI
      CHARACTER*24 METHOD,K24B,NOMLOG,INFOFE
      CHARACTER*1  MATSYM,TYPMAT
      REAL*8       C1,TEMPS(6),RBID
      COMPLEX*16   CBID

      LOGICAL ACREER,CUMUL,DBG,IDDOK,LBID,LFETI,LFETIC
      LOGICAL LGOTO,LLICH,LLICHD,LLICHP,LLIMO,LDIST
      LOGICAL LMASYM,LMESYM,ZEROBJ,LDGREL

      INTEGER ADMODL,I
      INTEGER IAD,JDESC
      INTEGER JADLI,JADNE,JNUEQ,JNULO1,JNULO2
      INTEGER JPOSD1,JPOSD2,JTMP2,LGTMP2
      INTEGER IBID,ICONX1,ICONX2,IDBGAV,IDD
      INTEGER IDIME,JLRES,JPRN1,JPRN2,JRESL,JRSVI
      INTEGER IEL,IER,IERD,IFCPU,IFEL1,IFEL2,IFEL3,IFEL4
      INTEGER IFEL5,JFETM,JFETN,IFM,IGR,IGREL
      INTEGER ILI,JFNUSD,ILIMA,ILIMAT,ILIMO,ILIMPI,ILINU
      INTEGER IMAT,JNUMSD,JREFN,IRESU
      INTEGER IRET,IRET1,IRET2,IRET3,ITBLOC
      INTEGER JREFA,JSMDE,JSMDI,JSMHC,JVALM(2)
      INTEGER LCMODL,MODE,N1,NBELM,NBEC,NBNO,DIGDEL
      INTEGER NBLC,NBNOMX,NBNOSS,NBRESU,NBSD,JNVGE
      INTEGER NCMP,NBVEL,NEC,NEL,NEQU,NBPROC,VALI(4)
      INTEGER NIV,NLILI,NMXCMP,NNOE,JPTVOI,JELVOI,JPRTI,JPRTK
      INTEGER NUGD,RANG,IEQ,IDIA,ELLAGR,JREPE,ITYPEL,IMATD,IEXI

C-----------------------------------------------------------------------
C     FONCTIONS FORMULES :
C-----------------------------------------------------------------------
      INTEGER ZZNGEL,ZZNELG,ZZLIEL

      ZZNGEL(ILI)=ZI(JADLI+3*(ILI-1))
      ZZNELG(ILI,IGREL)=ZI(ZI(JADLI+3*(ILI-1)+2)+IGREL)-
     &                  ZI(ZI(JADLI+3*(ILI-1)+2)+IGREL-1)-1
      ZZLIEL(ILI,IGREL,IEL)=ZI(ZI(JADLI+3*(ILI-1)+1)-1+
     &                    ZI(ZI(JADLI+3*(ILI-1)+2)+IGREL-1)+IEL-1)
C----------------------------------------------------------------------



      CALL JEMARQ()
      DBG=.FALSE.
      CALL JEDBG2(IDBGAV,0)
      CALL INFNIV(IFM,NIV)
      CALL MPICM1('BARRIER',' ',IBID,IBID,IBID,RBID,CBID)
      CALL UTTCPU('CPU.CALC.1','DEBUT',' ')
      CALL UTTCPU('CPU.ASSE.1','DEBUT',' ')
      CALL UTTCPU('CPU.ASSE.2','DEBUT',' ')

      BASE1=BASE
      MATDEV=MATAS
      NUDEV=NU
      IF (DBG) CALL CHEKSD(NUDEV,'SD_NUME_DDL',IRET)

      CALL DISMOI('F','NOM_MODELE',NUDEV,'NUME_DDL',IBID,MO,IERD)
      CALL DISMOI('F','NOM_MAILLA',MO,'MODELE',IBID,MA,IERD)
      CALL DISMOI('F','NOM_MAILLA',NUDEV,'NUME_DDL',IBID,MA2,IERD)
      CALL ASSERT(MA.EQ.MA2)
      CALL DISMOI('F','NB_NO_SS_MAX',MA,'MAILLAGE',NBNOSS,K8BID,IERD)
      CALL DISMOI('F','NOM_GD',NUDEV,'NUME_DDL',IBID,NOGDCO,IERD)
      CALL DISMOI('F','NOM_GD_SI',NOGDCO,'GRANDEUR',IBID,NOGDSI,IERD)
      CALL DISMOI('F','NB_CMP_MAX',NOGDSI,'GRANDEUR',NMXCMP,K8BID,IERD)
      NCMP=NMXCMP
      CALL DISMOI('F','NUM_GD_SI',NOGDSI,'GRANDEUR',NUGD,K8BID,IERD)
      NEC=NBEC(NUGD)
      CALL JEVEUO(JEXATR('&CATA.TE.MODELOC','LONCUM'),'L',LCMODL)
      CALL JEVEUO(JEXNUM('&CATA.TE.MODELOC',1),'L',ADMODL)
      CALL JEEXIN(MA//'.CONNEX',IRET)
      IF (IRET.GT.0) THEN
        CALL JEVEUO(MA//'.CONNEX','L',ICONX1)
        CALL JEVEUO(JEXATR(MA//'.CONNEX','LONCUM'),'L',ICONX2)
      ELSE
        ICONX1=0
        ICONX2=0
      ENDIF

      CALL JEEXIN(NUDEV//'.NUML.NULG',IMATD)
      IF ( IMATD.NE.0 ) THEN
        CALL JEVEUO(NUDEV//'.NUML.NUEQ','L',JNUEQ)
      ELSE
        CALL JEVEUO(NUDEV//'.NUME.NUEQ','L',JNUEQ)
      ENDIF

C     ELLAGR : 0 : PAS D'ELEMENT DE LAGRANGE
C              1 : IL EXISTE DES ELEMENTS DE LAGRANGE
      ELLAGR=0
C     KAMPIC : 'OUI' -> LA MATR_ASSE EST 'MPI_COMPLET'
      KAMPIC='OUI'




C     -- CALCUL DE :
C       LMASYM: .TRUE   : MATRICE ASSEMBLEE SYMETRIQUE
C               .FALSE. : MATRICE ASSEMBLEE NON-SYMETRIQUE
C       ACREER: .TRUE.  : IL FAUT CREER LA MATR_ASSE
C               .FALSE. : LA MATR_ASSE EXISTE DEJA
C       CUMUL : .TRUE.  : ON ACCUMULE DANS LA MATR_ASSE
C               .FALSE. : ON REMET LA MATR_ASSE A ZERO
C                         (ELLE DOIT EXISTER)
C       TT  : TT(1) : TYPE (R/C) DE CE QUE L'ON ASSEMBLE
C             TT(2) : TYPE (R/C) DE LA SD_MATR_ASSE
C     ------------------------------------------------------
      MATSYM=TYPMAT(NBMAT,TLIMAT)
      CALL ASSERT(MATSYM.EQ.'S' .OR. MATSYM.EQ.'N')
      LMASYM=(MATSYM.EQ.'S')
      IF (MOTCLE(1:4).EQ.'ZERO') THEN
        CUMUL=.FALSE.
        ACREER=.TRUE.
      ELSEIF (MOTCLE(1:4).EQ.'CUMU') THEN
        CUMUL=.TRUE.
        ACREER=.FALSE.
        CALL JELIRA(MATDEV//'.VALM','NMAXOC',NBLC,KBID)
        CALL ASSERT(NBLC.EQ.1 .OR. NBLC.EQ.2)
        IF (NBLC.EQ.2)LMASYM=.FALSE.
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
      CALL ASSERT((ITYSCA.EQ.1) .OR. (ITYSCA.EQ.2))
      IF (ITYSCA.EQ.1) THEN
        TT='?R'
      ELSEIF (ITYSCA.EQ.2) THEN
        TT='?C'
      ENDIF


C ------------------------------------------------------------------
C     -- SOLVEUR FETI :
C        CALCUL DE :
C           * LFETI  : .TRUE. : ON VA RESOUDRE AVEC FETI
C           * NBSD   : SI FETI : NOMBRE DE SOUS-DOMAINES (SINON:0)
C           * INFOFE : POUR LE MONITORING (SI FETI)
C           * LFETIC : POUR LE MONITORING (SI FETI)
C           * SDFETI : NOM DU PARTITIONNEMENT FETI (SI FETI)
C           * METHOD : METHODE DE RESOLUTION : FETI / AUTRE
C ------------------------------------------------------------------
      CALL JELIRA(NUDEV//'.NUME.REFN','LONMAX',N1,KBID)
      CALL ASSERT(N1.EQ.4)
      CALL JEVEUO(NUDEV//'.NUME.REFN','L',JREFN)
      METHOD=ZK24(JREFN+2)
      SDFETI=ZK24(JREFN+3)(1:19)
      LFETI=.FALSE.
      LFETIC=.FALSE.
      NBSD=0
      INFOFE='FFFFFFFFFFFFFFFFFFFFFFFF'
      IF (METHOD(1:4).EQ.'FETI') THEN
        LFETI=.TRUE.
        CALL ASSERT(LMASYM)
        CALL JEVEUO(SDFETI//'.FDIM','L',IDIME)
        NBSD=ZI(IDIME)
        CALL JEVEUO(NUDEV//'.FETN','L',JFETN)
        IF (.NOT.ACREER) CALL JEVEUO(MATDEV//'.FETM','L',JFETM)
        CALL JEVEUO('&FETI.FINF','L',IAD)
        INFOFE=ZK24(IAD)
        IF (INFOFE(11:11).EQ.'T')LFETIC=.TRUE.
        CALL JEVEUO('&FETI.LISTE.SD.MPI','L',ILIMPI)
        CALL FETTSD(INFOFE,IBID,IBID,IBID,SDFETI,K24B,IBID,IBID,
     &              IBID,IFM,LBID,IBID,IBID,IBID,MAT19,2,LBID)
      ENDIF


C ------------------------------------------------------------------
C
C     -- IL EXISTE TROIS FORMES DE CALCUL DISTRIBUE (DS LE SENS TOUS LES
C        PROCESSEURS NE CONNAISSENT QU'UNE PARTIE DE LA MATRICE) BASES
C        SUR UNE PARTITION:
C        * FETI SEQ ET //
C        * // DISTRIBUE
C        * // DISTRIBUE AVEC MUMPS + OPTION MATR_DISTIBUEE
C
C     -- IL EXISTE TROIS FORMES DE CALCUL CENTRALISE (TOUS LES PROC CON
C        NAISSENT LA MATRICE):
C        * SEQ HORS FETI
C        * // CENTRALISE
C        * // DISTRIBUE DANS ASSE_MATRICE
C
C        D'AUTRE PART ON TRACE DS LE REFA(11) DE LA MATRICE, SON CARAC
C        TERE COMPLET OU INCOMPLET AU SENS PARALLELISME:
C        * 'MPI_COMPLET': TOUS LES PROCS CONNAISSENT LA STRUCTURE DE
C           DONNEES, PAS BESOIN DE LA COMPLETER POUR FAIRE PAR EX. UN
C           PRODUIT MATRICE-VECTEUR. C'EST LE CAS D'UN CALCUL NON
C           DISTRIBUE OU D'UN CALCUL FETI (DISTRIBUE TYPE 1).
C        * 'MPI_INCOMPLET': CHAQUE PROC NE CONNAIT QUE QUELQUES COMPO
C           SANTES, IL FAUT ALORS PARFOIS COMPLETER PAR UNE COMM
C           MPI AD HOC (CF APPEL A ROUTINE MPICM1). C'EST LE CAS D'UN
C           CALCUL DISTRIBUE DE TYPE 2 (CF LISTE CI-DESSUS).
C
C         DS CES DEUX CAS DE FIGURE, LA SD EST DIMENSIONNEE DE LA MEME
C         FACON SUR TOUS LES PROCS (MEME TAILLE QU'EN SEQ, DES ZEROS COM
C         PLETENT LES TERMES NON CALCULES).
C         DANS LE TROISIEME CAS DE FIGURE (MATR_DISTRIBUEE), ON RETAILLE
C         AU PLUS JUSTE LEUR DIMENSION PAR PROC POUR GAGNER EN MEMOIRE.
C         ON NE PEUT PLUS COMPLETER SI SIMPLEMENT, CE CAS DE FIGURE EST
C         TAGGE PAR REFA(11)='MATR_DISTR'
C
C         EN BREF ON A 4 CAS DE FIGURES DE CALCUL ASTER ET ILS SE DECLI
C         NENT COMME SUIT VIS-A-VIS DES VARIABLES DE ASSMAM:
C        1/ CALCUL STD SEQ PAS FETI OU CALCUL // CENTRALISE OU CALCUL //
C            DISTRIBUE AVEC LA CMDE ECLATE (ASSE_MATRICE) POUR LAQUELLE
C            ON A COMPLETE AU PREALABLE LES MATR_ELEMS(CALC_MATR_ELEM).
C            LFETI='F',LDIST='F',KAMPIC='OUI',REFA(11)='MPI_COMPLET',,
C            IMATD=0
C        2/ CALCUL FETI SEQ OU PARALLELE (MATRICES MERE ET FILLES)
C            LFETI='T',LDIST='F',KAMPIC='OUI',REFA(11)='MPI_COMPLET'
C            IMATD=0
C        3/ CALCUL PARALLELE (AVEC OU SANS MUMPS) DISTRIBUE STD:
C            LFETI='F',LDIST='T',KAMPIC='NON',REFA(11)='MPI_INCOMPLET',
C            IMATD=0
C        4/ CAS PARTICULIER DU PRECEDENT: SOLVEUR=MUMPS + OPTION MATR
C          DISTRIBUEE ACTIVEE
C            LFETI='F',LDIST='T',KAMPIC='NON',REFA(11)='MATR_DISTR',
C            IMATD NON NUL
C
C         FINALEMENT LES VARIABLES KAMPIC ET LDIST TRADUISENT LES MEMES
C         CHOSES MAIS LEURS PROVENANCES SONT DISTINCTES:
C           - LDIST: EXISTENCE D'UNE SD_PARTITION
C           - KAMPIC: COMPLETUDE DES MATR_ELEMS
C           - LDGREL: LA SD_PARTITION EST DE TYPE 'GROUP_ELEM'
C         CELA PERMET DE CORROBORER LES INFORMATIONS
C ------------------------------------------------------------------
      RANG=0
      LDIST=.FALSE.
      LDGREL=.FALSE.
      CALL PARTI0(NBMAT,TLIMAT,PARTIT)
C     -- FETI N'EST PAS UN CALCUL DISTRIBUE. SES DONNEES SONT COMPLETES
C        POUR CHAQUE PROC
      IF ((PARTIT.NE.' ').AND.(.NOT.LFETI)) THEN
        LDIST=.TRUE.
        CALL MPICM0(RANG,NBPROC)
        CALL JEVEUO(PARTIT//'.PRTI','L',JPRTI)
        IF (ZI(JPRTI).NE.NBPROC) THEN
          VALI(1)=ZI(JPRTI)
          VALI(2)=NBPROC
          CALL U2MESI('F','CALCULEL_13',2,VALI)
        ENDIF
        CALL JEVEUO(PARTIT//'.PRTK','L',JPRTK)
        LDGREL=ZK24(JPRTK-1+1).EQ.'GROUP_ELEM'
        IF (.NOT.LDGREL) THEN
          CALL JEVEUO(PARTIT//'.NUPROC.MAILLE','L',JNUMSD)
        ENDIF
      ENDIF

C     -- SI C'EST LA COMMANDE ASSE_MATRICE, LES RESUELEM ONT ETE
C        COMPLETES. ON SE RETROUVE DANS LE CAS LDIST=.FALSE.
      CALL GETRES(K8BID,K16BID,NOMCMD)
      IF (NOMCMD(1:12).EQ.'ASSE_MATRICE') THEN
        LDIST=.FALSE.
        LDGREL=.FALSE.
      ENDIF


C     -- ALLOCATION DES OBJETS .NUMLOX ET .POSDDX:
C     ----------------------------------------------
C     50 EST SUPPOSE ETRE LE + GD NOMBRE DE NOEUDS D'UNE MAILLE
C        STANDARD (JUSQU'A PRESENT : 27 (HEXA27))
      NBNOMX=MAX(NBNOSS,50)
      CALL WKVECT('&&ASSMAM.NUMLO1','V V I',2*NBNOMX,JNULO1)
      CALL WKVECT('&&ASSMAM.NUMLO2','V V I',2*NBNOMX,JNULO2)
      CALL WKVECT('&&ASSMAM.POSDD1','V V I',NBNOMX*NMXCMP,JPOSD1)
      CALL WKVECT('&&ASSMAM.POSDD2','V V I',NBNOMX*NMXCMP,JPOSD2)

C     -- ALLOCATION D'UN OBJET DE TRAVAIL UTILISE DANS ASRETM :
C        CE VECTEUR EST AGRANDI SI NECESSAIRE DANS ASRETM
      LGTMP2=400
      CALL WKVECT('&&ASSMAM.TMP2','V V I',LGTMP2,JTMP2)

      IF (ACREER) THEN
        CALL DETRSD('MATR_ASSE',MATDEV)
      ELSE

        DO 10 IDD=0,NBSD
C========================================
C BOUCLE SUR LES SOUS-DOMAINES + IF MPI:
C========================================
C IDD=0 --> DOMAINE GLOBAL/ IDD=I --> IEME SOUS-DOMAINE

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
            IF (IDD.EQ.0) THEN
C             -- MATR_ASSE MAITRE LIEE AU DOMAINE GLOBAL
              MAT19=MATDEV
              NU14=NUDEV
            ELSE
C             -- MATR_ASSE ESCLAVE LIEE AU SOUS-DOMAINE IDD
              MAT19=ZK24(JFETM+IDD-1)(1:19)
              NU14=ZK24(JFETN+IDD-1)(1:14)
            ENDIF
            CALL JEVEUO(MAT19//'.REFA','L',JREFA)
            CALL ASSERT(ZK24(JREFA-1+2)(1:14).EQ.NU14)
            CALL JEDETR(MAT19//'.LIME')
            CALL JEDETR(MAT19//'.REFA')
          ENDIF
   10   CONTINUE
      ENDIF



C     -- RECOPIE DE LA LISTE DES MATR_ELEM DANS 1 OBJET JEVEUX
      CALL WKVECT(MATDEV//'.LIME',BASE1//' V K24 ',NBMAT,ILIMAT)
      DO 20 I=1,NBMAT
        ZK24(ILIMAT+I-1)=TLIMAT(I)
        IF (DBG .AND. TLIMAT(I).NE.' ') CALL CHEKSD(TLIMAT(I),
     &      'SD_MATR_ELEM',IRET)
   20 CONTINUE



C     -- CALCUL D UN REPERTOIRE,TEMPORAIRE, MATDEV.LILI A PARTIR
C     DE LA LISTE DE MATRICES ELEMENTAIRES MATDEV.LIME
      CALL CRELIL('F',NBMAT,ILIMAT,MATDEV//'.LILI','V','&MAILLA',
     &            MATDEV,IBID,MA,IBID,IBID,ILIMO,NLILI,NBELM)
      CALL JEVEUO(MATDEV//'.ADLI','E',JADLI)
      CALL JEVEUO(MATDEV//'.ADNE','E',JADNE)



      IF (LFETI) THEN
C       STOCKE &&//NOMPRO(1:6)//'_M.' POUR COHERENCE AVEC L'EXISTANT
        NOMLOG='&FETI.MAILLE.NUMSD'
        CALL JEVEUO(NOMLOG,'L',JFNUSD)
C       CONSTITUTION DE L'OBJET JEVEUX MATDEV.FETM COMPLEMENTAIRE
        IF (ACREER) CALL WKVECT(MATDEV//'.FETM',BASE1//' V K24',NBSD,
     &                          JFETM)
        CALL JEVEUO('&FETI.INFO.CPU.ASSE','E',IFCPU)
      ENDIF



C========================================
C BOUCLE SUR LES SOUS-DOMAINES + IF MPI:
C========================================
C IDD=0 --> DOMAINE GLOBAL/ IDD=I --> IEME SOUS-DOMAINE
      DO 130 IDD=0,NBSD

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
          IF ((NIV.GE.2) .OR.LFETIC) THEN
            CALL UTTCPU('CPU.ASSMAM','INIT ',' ')
            CALL UTTCPU('CPU.ASSMAM','DEBUT',' ')
          ENDIF

C         -- CALCUL DE MAT19 ET NU14 :
C         -------------------------------------
          IF (IDD.EQ.0) THEN
            MAT19=MATDEV
            NU14=NUDEV
          ELSE
            MAT19=ZK24(JFETM+IDD-1)(1:19)
            NU14=ZK24(JFETN+IDD-1)(1:14)
          ENDIF

          IF ((.NOT.LFETI) .OR. (IDD.GT.0)) THEN
            CALL JEVEUO(NU14//'.SMOS.SMHC','L',JSMHC)
            CALL JEVEUO(NU14//'.SMOS.SMDI','L',JSMDI)
            IF ( IMATD.NE.0 ) THEN
              CALL JEVEUO(NU14//'.NUML.PRNO','L',JPRN1)
              CALL JEVEUO(JEXATR(NU14//'.NUML.PRNO','LONCUM'),'L',JPRN2)
            ELSE
              CALL JEVEUO(NU14//'.NUME.PRNO','L',JPRN1)
              CALL JEVEUO(JEXATR(NU14//'.NUME.PRNO','LONCUM'),'L',JPRN2)
            ENDIF
          ENDIF


          IF (IDD.GT.0) THEN
            IF (ACREER) THEN
C             -- SI SOUS-DOMAINE FETI NOUVEAU
              CALL GCNCON('.',KNUMER)
              KNUMER(1:1)='F'
              ZK24(JFETM+IDD-1)=MATDEV(1:11)//KNUMER
              MAT19=ZK24(JFETM+IDD-1)(1:19)
            ENDIF
            CALL JEVEUO(NU14//'.NUME.REFN','L',JREFN)
            METHOD=ZK24(JREFN+2)
            SDFETI=ZK24(JREFN+3)(1:19)
          ENDIF

C         -- CREATION ET REMPLISSAGE DE .REFA
C         -------------------------------------
          CALL WKVECT(MAT19//'.REFA',BASE1//' V K24',11,JREFA)
          ZK24(JREFA-1+1)=MA
          ZK24(JREFA-1+2)=NU14
          ZK24(JREFA-1+8)='ASSE'
          IF (LMASYM) THEN
            ZK24(JREFA-1+9)='MS'
          ELSE
            ZK24(JREFA-1+9)='MR'
          ENDIF
          ZK24(JREFA-1+10)='NOEU'
          IF (METHOD(1:4).EQ.'FETI') THEN
            ZK24(JREFA-1+5)='FETI'
            ZK24(JREFA-1+6)=SDFETI
          ENDIF

C         -- SI FETI ET DOMAINE GLOBAL ON N'A RIEN A ASSEMBLER :
          IF (LFETI.AND. (IDD.EQ.0)) GOTO 110



          CALL JEVEUO(NU14//'.SMOS.SMDE','L',JSMDE)
          NEQU=ZI(JSMDE-1+1)
          ITBLOC=ZI(JSMDE-1+2)
          CALL ASSERT(ZI(JSMDE-1+3).EQ.1)
          IF (LMASYM) THEN
            NBLC=1
          ELSE
            NBLC=2
          ENDIF



C         -- ALLOCATION (OU NON) DE .VALM :
C         ---------------------------------
          IF (ACREER) THEN
            CALL JECREC(MAT19//'.VALM',BASE1//' V '//TT(2:2),'NU',
     &                  'DISPERSE','CONSTANT',NBLC)

            CALL JEECRA(MAT19//'.VALM','LONMAX',ITBLOC,' ')
            DO 30 I=1,NBLC
              CALL JECROC(JEXNUM(MAT19//'.VALM',I))
   30       CONTINUE
          ELSE
            IF (.NOT.CUMUL) THEN
              DO 40 I=1,NBLC
                CALL JERAZO(JEXNUM(MAT19//'.VALM',I),ITBLOC,1)
   40         CONTINUE
            ENDIF
          ENDIF

C         -- MISE EN MEMOIRE DES 1 (OU 2) BLOCS DE .VALM :
          CALL JEVEUO(JEXNUM(MAT19//'.VALM',1),'E',JVALM(1))
          CALL JELIRA(JEXNUM(MAT19//'.VALM',1),'TYPE',IBID,TYPSCA)
          CALL ASSERT(TT(2:2).EQ.TYPSCA)
          IF (.NOT.LMASYM) THEN
            CALL JEVEUO(JEXNUM(MAT19//'.VALM',2),'E',JVALM(2))
          ELSE
            JVALM(2)=0
          ENDIF



          LGOTO=.FALSE.
          K24B(1:14)=NUDEV
          CALL FETTSD(INFOFE,IDD,IBID,IBID,SDFETI,K24B,JFETN,
     &                JVALM(1),IBID,IFM,LBID,IBID,IBID,IBID,MAT19,3,
     &                LGOTO)
          IF (LGOTO)GOTO 100



C         3. BOUCLE SUR LES MATR_ELEM
C         =============================
          DO 90 IMAT=1,NBMAT
            C1=LICOEF(IMAT)
            MATEL=ZK24(ILIMAT+IMAT-1)(1:19)
            CALL DISMOI('F','NOM_MODELE',MATEL,'MATR_ELEM',IBID,MO2,
     &                  IERD)
            CALL DISMOI('F','SUR_OPTION',MATEL,'MATR_ELEM',IBID,OPTIO,
     &                  IERD)

            IF (IMAT.EQ.1) THEN
              OPTIO2=OPTIO
            ELSE
              IF (OPTIO2.NE.OPTIO)OPTIO2='&&MELANGE'
            ENDIF

            IF (MO2.NE.MO) CALL U2MESS('F','ASSEMBLA_5')


C           3.1 TRAITEMENT DES MACRO-ELEMENTS :
C           ----------------------------------
            CALL ASSMA2(LMASYM,TT,NU14,NCMP,MATEL,C1,JVALM,JTMP2,LGTMP2)


C           3.2 TRAITEMENT DES ELEMENTS FINIS CLASSIQUES
C           -------------------------------------------
            CALL JEEXIN(MATEL//'.RELR',IRET)
            IF (IRET.EQ.0)GOTO 80

            CALL JELIRA(MATEL//'.RELR','LONUTI ',NBRESU,KBID)
            IF (NBRESU.GT.0) CALL JEVEUO(MATEL//'.RELR','L',JLRES)

C           BOUCLE SUR LES RESU_ELEM
C           ==========================
            DO 70 IRESU=1,NBRESU
              RESU=ZK24(JLRES+IRESU-1)(1:19)
              CALL JEEXIN(RESU//'.DESC',IER)
              IF (IER.EQ.0)GOTO 70


C             -- CALCUL DE KAMPIC :
              CALL DISMOI('F','MPI_COMPLET',RESU,'RESUELEM',IBID,KEMPIC,
     &                    IERD)
              IF (KEMPIC.EQ.'NON') THEN
                CALL ASSERT(LDIST.OR.LFETI)
C             -- POUR FETI IL EST NORMAL QUE CHAQUE PROC NE CONNAISSE
C                QUE SA PARTIE DE MATRICE: LA MATRICE LOCALE AU SD IDD
                IF (.NOT.LFETI) KAMPIC='NON'
              ELSE
                CALL ASSERT(.NOT.LDIST)
              ENDIF


C             -- PARFOIS, CERTAINS RESUELEM SONT == 0.
              IF (ZEROBJ(RESU//'.RESL'))GOTO 70


C             -- NOM DU LIGREL
              CALL JEVEUO(RESU//'.NOLI','L',IAD)
              LIGRE1=ZK24(IAD)(1:19)

              CALL DISMOI('F','EXI_VF',LIGRE1,'LIGREL',IBID,EXIVF,IERD)
              IF (EXIVF.EQ.'OUI') THEN
                CALL ASSERT(.NOT.LMASYM)
                CALL JEVEUO(LIGRE1//'.REPE','L',JREPE)
                CALL JEVEUO(LIGRE1//'.NVGE','L',JNVGE)
                VGE=ZK16(JNVGE-1+1)(1:12)
                CALL JEVEUO(VGE//'.PTVOIS','L',JPTVOI)
                CALL JEVEUO(VGE//'.ELVOIS','L',JELVOI)
              ENDIF


C             SI FETI & LIGREL TARDIF:
C             -------------------------
              LLIMO=.TRUE.
              LLICH=.FALSE.
              LLICHD=.FALSE.
              LLICHP=.FALSE.
              IF (LFETI.AND. (IDD.NE.0)) THEN
C               RECHERCHE D'OBJET TEMPORAIRE SI FETI
                NOMLOG=LIGRE1//'.FEL1'
                CALL JEEXIN(NOMLOG,IRET1)
                IF (IRET1.NE.0) THEN
C                 LIGREL DE CHARGE A MAILLES TARDIVES OU CONTACT
C                 CONTINUE 1ERE PASSE
                  CALL JEVEUO(NOMLOG,'L',IFEL1)
                  LLICH=.TRUE.
                  LLIMO=.FALSE.
                  LIGRE2=ZK24(IFEL1-1+IDD)(1:19)
                  IF (LIGRE2.EQ.' ') THEN
C                   LIGREL NE CONCERNANT PAS LE SOUS-DOMAINE IDD
                    GOTO 70

                  ELSE
                    CALL JEEXIN(LIGRE1//'.FEL2',IRET2)
                    IF (IRET2.NE.0) THEN
C                     LIGREL DE CHARGE A MAILLES TARDIVES DUPLIQUEES
C                     DE FILS LIGRE2 DDL_IMPO, FORCE_NODALE...
                      LLICHD=.TRUE.
C                     VRAI NOM DU LIGREL DUPLIQUE CONTENU DANS
C                     PROF_CHNO.LILI LOCAL
                      CALL JEVEUO(LIGRE1//'.FEL2','L',IFEL2)
                      CALL JEEXIN(LIGRE1//'.FEL3',IRET3)
                      IF (IRET3.NE.0) THEN
                        CALL JEVEUO(LIGRE1//'.FEL3','L',IFEL3)
C                       LIGREL DE CHARGE A NOEUDS TARDIFS DUPLIQUES
C                       (DDL_IMPO...)
                        LLICHP=.TRUE.
                      ELSE
C                       PAS DE NOEUD TARDIF DUPLIQUE (FORCE_NODALE)
                        LLICHP=.FALSE.
                      ENDIF
                      CALL JEEXIN(LIGRE1//'.FEL4',IRET3)
                      IF (IRET3.NE.0) CALL JEVEUO(LIGRE1//'.FEL4',
     &                                     'L',IFEL4)
                      CALL JEEXIN(LIGRE1//'.FEL5',IRET3)
                      IF (IRET3.NE.0) CALL JEVEUO(LIGRE1//'.FEL5',
     &                                     'L',IFEL5)
                    ELSE
C                     -- LIGREL DE CHARGE NON DUPLIQUE
                      LLICHD=.FALSE.
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF


              CALL JENONU(JEXNOM(MATDEV//'.LILI',LIGRE1),ILIMA)
              IF (LLICHD) THEN
                CALL JENONU(JEXNOM(NU14//'.NUME.LILI',LIGRE2),ILINU)
              ELSE
                CALL JENONU(JEXNOM(NU14//'.NUME.LILI',LIGRE1),ILINU)
              ENDIF


C               -- MONITORING:
              IF ((INFOFE(5:5).EQ.'T') .AND.LFETI) THEN
                WRITE (IFM,*)'**************** IDD ',IDD
                WRITE (IFM,*)'<FETI/ASSMAM> ILIMO',ILIMO,'ILIMA',ILIMA
                WRITE (IFM,*)'<FETI/ASSMAM> LIGRE1/2 ',LIGRE1,LIGRE2
              ENDIF
              CALL DISMOI('F','TYPE_SCA',RESU,'RESUELEM',IBID,TYPSCA,
     &                    IERD)
              CALL ASSERT(TYPSCA.EQ.'R' .OR. TYPSCA.EQ.'C')
              TT(1:1)=TYPSCA
              CALL DISMOI('F','TYPE_MATRICE',RESU,'RESUELEM',IBID,SYMEL,
     &                    IERD)
              CALL ASSERT(SYMEL(1:1).EQ.'S' .OR. SYMEL(1:1).EQ.'N')
              LMESYM=(SYMEL(1:1).EQ.'S')
              IF (LMASYM) CALL ASSERT(LMESYM)

C             BOUCLE SUR LES GRELS DU LIGREL
C             ==============================
              DO 60 IGR=1,ZZNGEL(ILIMA)
                IF (LDGREL.AND.MOD(IGR,NBPROC).NE.RANG) GOTO 60

C               -- IL SE PEUT QUE LE GREL IGR SOIT VIDE :
                CALL JAEXIN(JEXNUM(RESU//'.RESL',IGR),IEXI)
                IF (IEXI.EQ.0) GOTO 60

                CALL JEVEUO(RESU//'.DESC','L',JDESC)
                MODE=ZI(JDESC+IGR+1)
                IF (MODE.GT.0) THEN
                  NNOE=NBNO(MODE)
                  CALL ASSERT(NNOE.LE.NBNOMX)
                  NBVEL=DIGDEL(MODE)
C                 NOMBRE D'ELEMENTS DU GREL IGR DU LIGREL LIGRE1/ILIMA
                  NEL=ZZNELG(ILIMA,IGR)
                  CALL JEVEUO(JEXNUM(RESU//'.RESL',IGR),'L',JRESL)
                  IF (EXIVF.EQ.'OUI') THEN
                    CALL JEVEUO(JEXNUM(RESU//'.RSVI',IGR),'L',JRSVI)
                    ITYPEL=ZZLIEL(ILIMA,IGR,NEL+1)
                    CALL JENUNO(JEXNUM('&CATA.TE.NOMTE',ITYPEL),NOMTE)
                    CALL TEATTR(NOMTE,'S','TYPE_VOISIN',CODVOI,IBID)
                  ENDIF

C                 BOUCLE SUR LES ELEMENTS DU GREL
C                 ================================
                  DO 50 IEL=1,NEL
                    CALL ASSMA3(LMASYM,LMESYM,TT,
     &                          IGR,IEL,C1,RANG,IFEL2,IFEL3,IFEL4,IFEL5,
     &                          IFM,JFNUSD,JNUEQ,JNUMSD,JRESL,
     &                          JRSVI,NBVEL,NNOE,LFETI,LLICH,LLICHD,
     &                          LLICHP,LLIMO,LDIST,LDGREL,ILIMA,JADLI,
     &                          JADNE,JPRN1,JPRN2,JNULO1,JNULO2,JPOSD1,
     &                          JPOSD2,ADMODL,LCMODL,MODE,NEC,NMXCMP,
     &                          NCMP,JSMHC,JSMDI,ICONX1,ICONX2,
     &                          LIGRE1,LIGRE2,INFOFE,JTMP2,LGTMP2,JVALM,
     &                          ILINU,IDD,ELLAGR,EXIVF,JDESC,
     &                          JREPE,JPTVOI,JELVOI,CODVOI)
   50             CONTINUE
                  CALL JELIBE(JEXNUM(RESU//'.RESL',IGR))
                ENDIF
   60         CONTINUE
   70       CONTINUE
   80       CONTINUE
   90     CONTINUE
  100     CONTINUE


C         -- MISE A JOUR DE REFA(4)
          CALL JEVEUO(MAT19//'.REFA','E',JREFA)
          IF (ACREER) THEN
            ZK24(JREFA-1+4)=OPTIO2
          ELSE
            IF (ZK24(JREFA-1+4).NE.OPTIO2)ZK24(JREFA-1+4)='&&MELANGE'
          ENDIF

  110     CONTINUE


C         -- MONITORING:
          IF (LFETI .AND. (INFOFE(1:1).EQ.'T')) THEN
            IF (IDD.EQ.0) THEN
              WRITE (IFM,*)'<FETI/ASSMAM> DOMAINE GLOBAL',MAT19
            ELSE
              WRITE (IFM,*)'<FETI/ASSMAM> SD: ',IDD,' ',MAT19
            ENDIF
          ENDIF
          IF ((INFOFE(3:3).EQ.'T') .AND. (IDD.NE.0)) CALL UTIMSD(IFM,2,
     &        .FALSE.,.TRUE.,MATDEV,1,' ')
          IF ((INFOFE(3:3).EQ.'T') .AND. (IDD.EQ.NBSD)) CALL UTIMSD(IFM,
     &        2,.FALSE.,.TRUE.,MATDEV,1,' ')
          IF ((NIV.GE.2) .OR.LFETIC) THEN
            CALL UTTCPU('CPU.ASSMAM','FIN',' ')
            CALL UTTCPR('CPU.ASSMAM',6,TEMPS)
            IF (NIV.GE.2)WRITE (IFM,'(A44,D11.4,D11.4)')
     &          'TEMPS CPU/SYS ASSEMBLAGE M                : ',TEMPS(5),
     &          TEMPS(6)
            IF (LFETIC)ZR(IFCPU+IDD)=ZR(IFCPU+IDD)+TEMPS(5)+TEMPS(6)
          ENDIF


C         -- ECRITURE DANS FICHIER SI FETI ET INFO_FETI(14:14)='T'
          K24B(1:14)=NUDEV
          CALL FETTSD(INFOFE,IDD,IBID,IBID,SDFETI,K24B,JFETN,
     &                JVALM(1),IBID,IFM,LBID,IBID,IBID,IBID,MAT19,6,
     &                LBID)


          IF (.NOT.LMASYM) THEN
C           -- ON AFFECTE AUX TERMES DIAGONAUX DU BLOC INFERIEUR
C              LES VALEURS DES TERMES DIAGONAUX DU BLOC SUPERIEUR
            DO 120 IEQ=1,NEQU
              IDIA=ZI(JSMDI+IEQ-1)
              ZR(JVALM(2)+IDIA-1)=ZR(JVALM(1)+IDIA-1)
  120       CONTINUE
          ENDIF

C         -- IL FAUT COMMUNIQUER ELLAGR ENTRE LES PROCS :
          IF (LDIST) CALL MPICM1('MPI_MAX','I',1,IBID,ELLAGR,RBID,CBID)


C         -- MISE A L'ECHELLE DES COEF. DE LAGRANGE SI NECESSAIRE :
          IF (LFETI) THEN
            IF ((ELLAGR.GT.0).AND.(IDD.GT.0))
     &        CALL ASSMA1(MAT19,LDIST)
            CALL JEDEMA()
          ELSE
            IF (ELLAGR.GT.0) CALL ASSMA1(MAT19,LDIST)
          ENDIF
        ENDIF


        IF (KAMPIC.EQ.'OUI') THEN
C         -- CALCUL STD OU CALCUL FETI OU CALCUL DISTRIBUE COMPLETE
C            (CMD ECLATEE ASSE_MATRICE)
          CALL ASSERT(.NOT.LDIST)
          ZK24(JREFA-1+11)='MPI_COMPLET'
        ELSE
C         -- CALCUL DISTRIBUE AVEC OU SANS MUMPS
          CALL ASSERT(LDIST)
          ZK24(JREFA-1+11)='MPI_INCOMPLET'
        ENDIF
C        -- DANGEREUX DE CUMULER DEUX TYPES D'INFORMATIONS EN REFA(11)
C           LE CARACTERE MPI_COMPLET/INCOMPLET ET MATR_DISTR
C           TOUT DEPEND PAR LA SUITE DU TYPE DE QUESTION QUE L'ON POSE
C           POUR RECUPERER CETTE INFO
        IF ( IMATD.NE.0 ) THEN
C        -- CALCUL DISTRIBUE AVEC MUMPS + OPTION MATR_DISTRIBUEE='OUI'
          CALL ASSERT(LDIST)
          ZK24(JREFA-1+11)='MATR_DISTR'
        ENDIF
  130 CONTINUE



      CALL JEDETR(MATDEV//'.ADNE')
      CALL JEDETR(MATDEV//'.ADLI')
      CALL JEDETR('&&ASSMAM.NUMLO1')
      CALL JEDETR('&&ASSMAM.NUMLO2')
      CALL JEDETR('&&ASSMAM.POSDD1')
      CALL JEDETR('&&ASSMAM.POSDD2')
      CALL JEDETR('&&ASSMAM.TMP2')
      CALL JEDBG2(IBID,IDBGAV)
      IF (DBG .AND. (.NOT.LFETI)) CALL CHEKSD(MATDEV,'SD_MATR_ASSE',
     &    IRET)

C     CALL UTIMSD(6,-1,.FALSE.,.TRUE.,MATAS,1,' ')
      CALL MPICM1('BARRIER',' ',IBID,IBID,IBID,RBID,CBID)
      CALL UTTCPU('CPU.CALC.1','FIN',' ')
      CALL UTTCPU('CPU.ASSE.1','FIN',' ')
      CALL UTTCPU('CPU.ASSE.2','FIN',' ')
      CALL JEDEMA()
      END
