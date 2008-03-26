      SUBROUTINE ASSMAM(BASE,MATAS,NBMAT,TLIMAT,LICOEF,NU,MOTCLE,ITYSCA)

C  ATTENTION : CETTE ROUTINE NE DOIT PAS ETRE APPELLEE DIRECTEMENT :
C              IL FAUT APPELER SON "CHAPEAU" : ASMATR.

      IMPLICIT NONE

      CHARACTER*(*) BASE,MATAS,TLIMAT(*),NU
      INTEGER NBMAT,ITYSCA
      REAL*8 LICOEF(*)
      CHARACTER*4 MOTCLE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ASSEMBLA  DATE 25/03/2008   AUTEUR REZETTE C.REZETTE 
C RESPONSABLE PELLET J.PELLET
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

C  SI IL EXISTE UN OBJET '&&POIDS_MAILLE' VR CONTENANT
C  DES PONDERATIONS POUR CHAQUE MAILLE, ON S'EN SERT.
C-----------------------------------------------------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16,OPTIO,OPTIO2
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C-----------------------------------------------------------------------
      INTEGER CADIST
      COMMON /CAII18/CADIST
      CHARACTER*1 BASE1,TYPSCA,KBID
      CHARACTER*2 TT
      CHARACTER*8 K8BID,NOGDCO,NOGDSI,MA,MA2,MO,MO2
      CHARACTER*8 KNUMER,SYMEL,KEMPIC,KAMPIC
      CHARACTER*14 NUDEV,NU14
      CHARACTER*19 MATDEV,MAT19,RESU,MATEL
      CHARACTER*24 NOMLI,METHOD,SDFETI,K24B,NOMLOG,NOMLID,INFOFE
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR
      CHARACTER*1 MATSYM,TYPMAT
      REAL*8 C1,C2,TEMPS(6),RBID
      COMPLEX*16 CBID

      LOGICAL ACREER,CUMUL,DBG,IDDOK,LBID,LFETI,LFETIC
      LOGICAL LGOTO,LLICH,LLICHD,LLICHP,LLIMO,LMUMPS,LPDMS
      LOGICAL LMASYM,LMESYM,ZEROBJ

      INTEGER ADMODL,EPDMS,I
      INTEGER IAD,IADESC
      INTEGER JADLI,JADNE,JNUEQ,JNULOC
      INTEGER JPOSDL,JTMP2
      INTEGER IBID,ICONX1,ICONX2,IDBGAV,IDD
      INTEGER IDIME,JLRES,JPRN1,JPRN2,JRESL
      INTEGER IEL,IER,IERD,IFCPU,IFEL1,IFEL2,IFEL3,IFEL4
      INTEGER IFEL5,JFETM,JFETN,IFM,IGR,IGREL
      INTEGER ILI,JFNUSD
      INTEGER ILIMA,ILIMAT,ILIMO,ILIMPI,ILINU,ILONG
      INTEGER IMAT,JNUMSD,JREFN,IRESU
      INTEGER IRET,IRET1,IRET2,IRET3,ITBLOC
      INTEGER JPDMS,JREFA,JSMDE,JSMDI,JSMHC,JVALM(2)
      INTEGER LCMODL
      INTEGER MODE,N1,NBELM,NBEC
      INTEGER NBNO,DIGDEL
      INTEGER NBLC,NBNOMX,NBNOSS,NBRESU,NBSD
      INTEGER NCMP,NBVEL,NEC,NEL,NEQU
      INTEGER NIV,NLILI,NMXCMP,NNOE
      INTEGER NUGD,RANG,IEQ,IDIA,ELLAGR

C-----------------------------------------------------------------------
C     FONCTIONS FORMULES :
C-----------------------------------------------------------------------
      INTEGER ZZNGEL,ZZNELG

      ZZNGEL(ILI)=ZI(JADLI+3*(ILI-1))
      ZZNELG(ILI,IGREL)=ZI(ZI(JADLI+3*(ILI-1)+2)+IGREL)-
     &                  ZI(ZI(JADLI+3*(ILI-1)+2)+IGREL-1)-1
C----------------------------------------------------------------------



      CALL JEMARQ()
      DBG=.TRUE.
      DBG=.FALSE.
      CALL JEDBG2(IDBGAV,0)
      CALL INFNIV(IFM,NIV)

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
      CALL JEVEUO(NUDEV//'.NUME.NUEQ','L',JNUEQ)
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
      CALL ASSERT(MATSYM.EQ.'S'.OR.MATSYM.EQ.'N')
      LMASYM=(MATSYM.EQ.'S')
      IF (MOTCLE(1:4).EQ.'ZERO') THEN
        CUMUL=.FALSE.
        ACREER=.TRUE.
      ELSEIF (MOTCLE(1:4).EQ.'CUMU') THEN
        CUMUL=.TRUE.
        ACREER=.FALSE.
        CALL JELIRA(MATDEV//'.VALM','NMAXOC',NBLC,KBID)
        CALL ASSERT(NBLC.EQ.1.OR.NBLC.EQ.2)
        IF (NBLC.EQ.2) LMASYM=.FALSE.
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
      CALL ASSERT((ITYSCA.EQ.1).OR.(ITYSCA.EQ.2))
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
      SDFETI=ZK24(JREFN+3)
      LFETI=.FALSE.
      LFETIC=.FALSE.
      NBSD=0
      INFOFE='FFFFFFFFFFFFFFFFFFFFFFFF'
      IF (METHOD(1:4).EQ.'FETI') THEN
        LFETI=.TRUE.
        CALL ASSERT(LMASYM)
        CALL JEVEUO(SDFETI(1:19)//'.FDIM','L',IDIME)
        NBSD=ZI(IDIME)
        CALL JEVEUO(NUDEV//'.FETN','L',JFETN)
        IF (.NOT.ACREER) CALL JEVEUO(MATDEV//'.FETM','L',JFETM)
        CALL JEVEUO('&FETI.FINF','L',IAD)
        INFOFE=ZK24(IAD)
        IF (INFOFE(11:11).EQ.'T')LFETIC=.TRUE.
        CALL JEVEUO('&FETI.LISTE.SD.MPI','L',ILIMPI)
        CALL FETTSD(INFOFE,IBID,IBID,IBID,SDFETI(1:19),K24B,IBID,IBID,
     &              IBID,IFM,LBID,IBID,IBID,IBID,MAT19,2,LBID)
      ENDIF


C ------------------------------------------------------------------
C     -- SOLVEUR MUMPS DISTRIBUE :
C        CALCUL DE :
C           * LMUMPS : .TRUE. : ON VA RESOUDRE AVEC MUMPS DISTRIBUE
C           * JNUMSD : ADRESSE DE '&MUMPS.MAILLE.NUMSD'
C ------------------------------------------------------------------
      CALL ASSERT(CADIST.GE.-1.AND.CADIST.LE.1)
      LMUMPS=.FALSE.
      CALL JEEXIN('&MUMPS.MAILLE.NUMSD',IRET)
      IF (IRET.NE.0) THEN
        CALL ASSERT(CADIST.EQ.1)
        CALL MUMMPI(2,IFM,NIV,K24B,RANG,IBID)
        LMUMPS=.TRUE.
        CALL JEVEUO('&MUMPS.MAILLE.NUMSD','L',JNUMSD)
      ENDIF



C ------------------------------------------------------------------
C     -- METHODE ARLEQUIN :
C        CALCUL DE :
C           * LPDMS : .TRUE. : ILFAUT TENIR COMPTE DU POIDS DES MAILLES
C           * JPDMS : ADRESSE DE '&&POIDS_MAILLE'
C ------------------------------------------------------------------
      CALL JEEXIN('&&POIDS_MAILLE',EPDMS)
      LPDMS=(EPDMS.GT.0)
      IF (LPDMS) CALL JEVEUO('&&POIDS_MAILLE','L',JPDMS)



C     -- ALLOCATION DES OBJETS .NUMLOC ET .POSDDL:
C     ----------------------------------------------
C     50 EST SUPPOSE ETRE LE + GD NOMBRE DE NOEUDS D'UNE MAILLE
C        STANDARD (JUSQU'A PRESENT : 27 (HEXA27))
      NBNOMX=MAX(NBNOSS,50)
      CALL WKVECT('&&ASSMAM.NUMLOC','V V I',2*NBNOMX,JNULOC)
      CALL WKVECT('&&ASSMAM.POSDDL','V V I',NBNOMX*NMXCMP,JPOSDL)






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
              MAT19=ZK24(JFETM+IDD-1)
              NU14=ZK24(JFETN+IDD-1)
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
      CALL CRELIL(NBMAT,ILIMAT,MATDEV//'.LILI','V','&MAILLA',MATDEV,
     &            IBID,MA,IBID,IBID,ILIMO,NLILI,NBELM)
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
      DO 260 IDD=0,NBSD

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
          IF ((NIV.GE.2) .OR. (LFETIC)) THEN
            CALL UTTCPU(90,'INIT ',6,TEMPS)
            CALL UTTCPU(90,'DEBUT',6,TEMPS)
          ENDIF

C         -- CALCUL DE MAT19 ET NU14 :
C         -------------------------------------
          IF (IDD.EQ.0) THEN
            MAT19=MATDEV
            NU14=NUDEV
          ELSE
            MAT19=ZK24(JFETM+IDD-1)
            NU14=ZK24(JFETN+IDD-1)
          ENDIF

          IF ((.NOT.LFETI) .OR. (IDD.GT.0)) THEN
            CALL JEVEUO(NU14//'.SMOS.SMHC','L',JSMHC)
            CALL JEVEUO(NU14//'.SMOS.SMDI','L',JSMDI)
            CALL JEVEUO(NU14//'.NUME.PRNO','L',JPRN1)
            CALL JEVEUO(JEXATR(NU14//'.NUME.PRNO','LONCUM'),'L',JPRN2)
          ENDIF


          IF (IDD.GT.0) THEN
            IF (ACREER) THEN
C             -- SI SOUS-DOMAINE FETI NOUVEAU
              CALL GCNCON('.',KNUMER)
              KNUMER(1:1)='F'
              ZK24(JFETM+IDD-1)=MATDEV(1:11)//KNUMER
              MAT19=ZK24(JFETM+IDD-1)
            ENDIF
            CALL JEVEUO(NU14//'.NUME.REFN','L',JREFN)
            METHOD=ZK24(JREFN+2)
            SDFETI=ZK24(JREFN+3)
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
          IF ((LFETI) .AND. (IDD.EQ.0))GOTO 250



C-------------------------------------------------------------------
C         CREATION DE 2 OBJETS VOLATILES POUR ACCELERER :
C         TMP2 : (1:2*DIM(MATR_ELEM)) POSITION RELATIVE DANS LES BLOCS
C                POUR LE I-EME REEL DE LA MATRICE ELEM :
C         TMP2(2*(I-1)+1) --> NUMERO DU BLOC OU S'INJECTE I.
C         TMP2(2*(I-1)+2) --> POSITION DANS LE BLOC DU REEL I.
C-------------------------------------------------------------------
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
     &                    'DISPERSE','CONSTANT',NBLC)

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
          CALL FETTSD(INFOFE,IDD,IBID,IBID,SDFETI(1:19),K24B,JFETN,
     &             JVALM(1),IBID,IFM,LBID,IBID,IBID,IBID,MAT19,3,LGOTO)
          IF (LGOTO)GOTO 240



C         3. BOUCLE SUR LES MATR_ELEM
C         =============================
          ILONG=0
          DO 230 IMAT=1,NBMAT
            C1=LICOEF(IMAT)
            C2=C1
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
            CALL ASSMA2(LMASYM,TT,MAT19,NU14,NCMP,MATEL,C2,JVALM)


C           3.2 TRAITEMENT DES ELEMENTS FINIS CLASSIQUES
C           -------------------------------------------
            CALL JEEXIN(MATEL//'.RELR',IRET)
            IF (IRET.EQ.0)GOTO 220

            CALL JELIRA(MATEL//'.RELR','LONUTI ',NBRESU,KBID)
            IF(NBRESU.GT.0)CALL JEVEUO(MATEL//'.RELR','L',JLRES)

C           BOUCLE SUR LES RESU_ELEM
C           ==========================
            DO 210 IRESU=1,NBRESU
              RESU=ZK24(JLRES+IRESU-1)
              CALL JEEXIN(RESU//'.DESC',IER)
              IF (IER.EQ.0)GOTO 210
C             -- PARFOIS, CERTAINS RESUELEM SONT == 0.
              IF (ZEROBJ(RESU//'.RESL')) GOTO 210


C             -- NOM DU LIGREL
              CALL JEVEUO(RESU//'.NOLI','L',IAD)
              NOMLI=ZK24(IAD)


C             SI FETI & LIGREL TARDIF:
C             -------------------------
              LLIMO=.TRUE.
              LLICH=.FALSE.
              LLICHD=.FALSE.
              LLICHP=.FALSE.
              IF ((LFETI) .AND. (IDD.NE.0)) THEN
C               RECHERCHE D'OBJET TEMPORAIRE SI FETI
                NOMLOG=NOMLI(1:19)//'.FEL1'
                CALL JEEXIN(NOMLOG,IRET1)
                IF (IRET1.NE.0) THEN
C                 LIGREL DE CHARGE A MAILLES TARDIVES OU CONTACT
C                 CONTINUE 1ERE PASSE
                  CALL JEVEUO(NOMLOG,'L',IFEL1)
                  LLICH=.TRUE.
                  LLIMO=.FALSE.
                  NOMLID=ZK24(IFEL1-1+IDD)
                  IF (NOMLID(1:19).EQ.' ') THEN
C                   LIGREL NE CONCERNANT PAS LE SOUS-DOMAINE IDD
                    GOTO 210
                  ELSE
                    CALL JEEXIN(NOMLI(1:19)//'.FEL2',IRET2)
                    IF (IRET2.NE.0) THEN
C                     LIGREL DE CHARGE A MAILLES TARDIVES DUPLIQUEES
C                     DE FILS NOMLID DDL_IMPO, FORCE_NODALE...
                      LLICHD=.TRUE.
C                     VRAI NOM DU LIGREL DUPLIQUE CONTENU DANS
C                     PROF_CHNO.LILI LOCAL
                      CALL JEVEUO(NOMLI(1:19)//'.FEL2','L',IFEL2)
                      CALL JEEXIN(NOMLI(1:19)//'.FEL3',IRET3)
                      IF (IRET3.NE.0) THEN
                        CALL JEVEUO(NOMLI(1:19)//'.FEL3','L',IFEL3)
C                       LIGREL DE CHARGE A NOEUDS TARDIFS DUPLIQUES
C                       (DDL_IMPO...)
                        LLICHP=.TRUE.
                      ELSE
C                       PAS DE NOEUD TARDIF DUPLIQUE (FORCE_NODALE)
                        LLICHP=.FALSE.
                      ENDIF
                      CALL JEEXIN(NOMLI(1:19)//'.FEL4',IRET3)
                      IF (IRET3.NE.0) CALL JEVEUO(NOMLI(1:19)//'.FEL4',
     &                                     'L',IFEL4)
                      CALL JEEXIN(NOMLI(1:19)//'.FEL5',IRET3)
                      IF (IRET3.NE.0) CALL JEVEUO(NOMLI(1:19)//'.FEL5',
     &                                     'L',IFEL5)
                    ELSE
C                     -- LIGREL DE CHARGE NON DUPLIQUE
                      LLICHD=.FALSE.
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF


              CALL JENONU(JEXNOM(MATDEV//'.LILI',NOMLI),ILIMA)
              IF (LLICHD) THEN
                CALL JENONU(JEXNOM(NU14//'.NUME.LILI',NOMLID),ILINU)
              ELSE
                CALL JENONU(JEXNOM(NU14//'.NUME.LILI',NOMLI),ILINU)
              ENDIF


C               -- MONITORING:
              IF ((INFOFE(5:5).EQ.'T') .AND. (LFETI)) THEN
                WRITE (IFM,*)'**************** IDD ',IDD
                WRITE (IFM,*)'<FETI/ASSMAM> ILIMO',ILIMO,'ILIMA',ILIMA
                WRITE (IFM,*)'<FETI/ASSMAM> NOMLI/NOMLID ',NOMLI,NOMLID
              ENDIF
              CALL DISMOI('F','MPI_COMPLET',RESU,'RESUELEM',IBID,KEMPIC,
     &                    IERD)
              IF (KEMPIC.EQ.'NON') THEN
                CALL ASSERT(CADIST.EQ.1)
                CALL ASSERT(LMUMPS.OR.LFETI)
                KAMPIC='NON'
              ENDIF
              CALL DISMOI('F','TYPE_SCA',RESU,'RESUELEM',IBID,TYPSCA,
     &                    IERD)
              CALL ASSERT(TYPSCA.EQ.'R'.OR.TYPSCA.EQ.'C')
              TT(1:1)=TYPSCA
              CALL DISMOI('F','TYPE_MATRICE',RESU,'RESUELEM',IBID,
     &                    SYMEL,IERD)
              CALL ASSERT(SYMEL(1:1).EQ.'S'.OR.SYMEL(1:1).EQ.'N')
              LMESYM=(SYMEL(1:1).EQ.'S')
              IF (LMASYM) CALL ASSERT(LMESYM)

C             BOUCLE SUR LES GRELS DU LIGREL
C             ==============================
              DO 200 IGR=1,ZZNGEL(ILIMA)
                CALL JEVEUO(RESU//'.DESC','L',IADESC)
                MODE=ZI(IADESC+IGR+1)
                IF (MODE.GT.0) THEN
                  NNOE=NBNO(MODE)
                  CALL ASSERT(NNOE.LE.NBNOMX)
                  NBVEL=DIGDEL(MODE)
C                 NOMBRE D'ELEMENTS DU GREL IGR DU LIGREL NOMLI/ILIMA
                  NEL=ZZNELG(ILIMA,IGR)
                  CALL JEVEUO(JEXNUM(RESU//'.RESL',IGR),'L',JRESL)
                  IF (NBVEL.GT.ILONG) THEN
                    ILONG=NBVEL
                    CALL JEEXIN(MAT19//'.TMP2',IRET2)
                    IF (IRET2.GT.0) CALL JEDETR(MAT19//'.TMP2')
                    CALL WKVECT(MAT19//'.TMP2',' V V I',2*ILONG,JTMP2)
                  ENDIF

C                 BOUCLE SUR LES ELEMENTS DU GREL
C                 ================================
                  DO 190 IEL=1,NEL
                  CALL ASSMA3(LMASYM,LMESYM,TT,MAT19,NU14,
     &    MATEL,C2,IGR,IEL,C1,RANG,IFEL2,IFEL3,IFEL4,IFEL5,IFM,JFNUSD,
     &              JNUEQ,JNUMSD,JPDMS,JRESL,NBVEL,NNOE,LFETI,LLICH,
     &              LLICHD,LLICHP,LLIMO,LMUMPS,LPDMS,ILIMA,JADLI,JADNE,
     &              JPRN1,JPRN2,JNULOC,JPOSDL,ADMODL,LCMODL,MODE,NEC,
     &              NMXCMP,NCMP,NBLC,JSMHC,JSMDI,ICONX1,ICONX2,
     &              NOMLI,NOMLID,INFOFE,
     &              JTMP2,JVALM,ILINU,IDD,ELLAGR)
  190             CONTINUE
                  CALL JELIBE(JEXNUM(RESU//'.RESL',IGR))
                ENDIF
  200         CONTINUE
  210       CONTINUE
  220       CONTINUE
  230     CONTINUE
  240     CONTINUE


C         -- MISE A JOUR DE REFA(4)
          CALL JEVEUO(MAT19//'.REFA','E',JREFA)
          IF (ACREER) THEN
            ZK24(JREFA-1+4)=OPTIO2
          ELSE
            IF (ZK24(JREFA-1+4).NE.OPTIO2)ZK24(JREFA-1+4)='&&MELANGE'
          ENDIF

          CALL JEDETR(MAT19//'.TMP2')

  250     CONTINUE


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
          IF ((NIV.GE.2) .OR. (LFETIC)) THEN
            CALL UTTCPU(90,'FIN  ',6,TEMPS)
            IF (NIV.GE.2)WRITE (IFM,'(A44,D11.4,D11.4)')
     &          'TEMPS CPU/SYS ASSEMBLAGE M                : ',TEMPS(5),
     &          TEMPS(6)
            IF (LFETIC)ZR(IFCPU+IDD)=ZR(IFCPU+IDD)+TEMPS(5)+TEMPS(6)
C           POUR QUE CELA FONCTIONNE AVEC MUMPS CENTRALISE
            CALL JEEXIN('&MUMPS.INFO.CPU.ASSE',IRET)
            IF (IRET.NE.0) THEN
              CALL MUMMPI(2,IFM,NIV,K24B,RANG,IBID)
              CALL JEVEUO('&MUMPS.INFO.CPU.ASSE','E',IFCPU)
              ZR(IFCPU+RANG)=ZR(IFCPU+RANG)+TEMPS(5)+TEMPS(6)
            ENDIF
          ENDIF


C         -- ECRITURE DANS FICHIER SI FETI ET INFO_FETI(14:14)='T'
          K24B(1:14)=NUDEV
          CALL FETTSD(INFOFE,IDD,IBID,IBID,SDFETI(1:19),K24B,JFETN,
     &             JVALM(1),IBID,IFM,LBID,IBID,IBID,IBID,MAT19,6,LBID)


          IF (.NOT.LMASYM) THEN
C           -- ON AFFECTE AUX TERMES DIAGONAUX DU BLOC INFERIEUR
C              LES VALEURS DES TERMES DIAGONAUX DU BLOC SUPERIEUR
                DO 120 IEQ = 1,NEQU
                  IDIA = ZI(JSMDI+IEQ-1)
                  ZR(JVALM(2)+IDIA-1) = ZR(JVALM(1)+IDIA-1)
  120           CONTINUE
          ENDIF

C         -- IL FAUT COMMUNIQUER ELLAGR ENTRE LES PROCS :
          IF (CADIST.EQ.1) CALL MPICM1('MPI_MAX','I',1,ELLAGR,RBID)


C         -- MISE A L'ECHELLE DES COEF. DE LAGRANGE SI NECESSAIRE :
          IF (LFETI) THEN
            IF ((ELLAGR.GT.0).AND.(IDD.GT.0)) CALL ASSMA1(MAT19)
            CALL JEDEMA()
          ELSE
            IF (ELLAGR.GT.0) CALL ASSMA1(MAT19)
          ENDIF
        ENDIF


        IF (KAMPIC.EQ.'OUI') THEN
          ZK24(JREFA-1+11)='MPI_COMPLET'
        ELSE
          CALL ASSERT(CADIST.EQ.1)
          ZK24(JREFA-1+11)='MPI_INCOMPLET'
        ENDIF
  260 CONTINUE



      CALL JEDETR(MATDEV//'.ADNE')
      CALL JEDETR(MATDEV//'.ADLI')
      CALL JEDETR('&&ASSMAM.NUMLOC')
      CALL JEDETR('&&ASSMAM.POSDDL')
      CALL JEDBG2(IBID,IDBGAV)
      IF (DBG.AND.(.NOT.LFETI)) CALL CHEKSD(MATDEV,'SD_MATR_ASSE',IRET)

C     CALL UTIMSD(6,-1,.FALSE.,.TRUE.,MATAS,1,' ')
      CALL JEDEMA()
      END
