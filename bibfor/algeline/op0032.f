      SUBROUTINE OP0032()
      IMPLICIT NONE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 26/02/2013   AUTEUR BOITEAU O.BOITEAU 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     EIGENVALUE-COUNTING METHODS FOR GEP OR QEP
C     ------------------------------------------------------------------
C RESPONSABLE BOITEAU O.BOITEAU
C TOLE CRP_20
C
C
      INCLUDE 'jeveux.h'
      INTEGER      ISLVK,ISLVI,JREFA,ITEST,NMULTC,LAMOR,JLMOD,JLMOE,
     &             PIVOT1,PIVOT2,MXDDL,NBRSS,IERD,II,IFAPM,K,NBMOD,
     &             NBLAGR,NBCINE,NEQACT,NEQ,NITERC,NPIVOT(2),MPICOU,
     &             L,LMASSE,LRAIDE,LDDL,LDYNAM,NK,NBROW,
     &             LPROD,IRET,NBFREQ,KREFA,IDET(2),JSTU,VALI(3),
     &             IFM,NIV,NBTETC,NBTET0,NBTET1,TYPECO,RANG,NBPROC,
     &             NBTET2,NBEV0,NBEV1,NBEV2,MITERC,IARG,IBID,MPICOW,
     &             K1,K2,JKPAR,L1,L2,L3,L11,L21,FRECOU,IZERO
      INTEGER*4    COMCOU
      REAL*8       OMEGA2,OMGMIN,OMGMAX,OMIN,OMAX,FCORIG,OMECOR,FREQOM,
     &             PRECSH,RAYONC,DIMC1,RZERO,CALPAR(2),CALPAC(3),
     &             CALPAF(2),RBID,DET(2)
      COMPLEX*16   CENTRC,ZIMC1,CBID
      LOGICAL      LTEST,LC,LDYNA,LFLAMB,LFIRST,LCOMOD
      CHARACTER*1  TYPEP,TPPARN(1),TPPARR(2),TPPARC(3),TPPARF(2),
     &             TPPARM(2)
      CHARACTER*3  IMPR
      CHARACTER*8  TYPCON,TYPMET,TYPCHA,TABLE,KOPT1,KOPTN
      CHARACTER*14 MATRA,MATRB,MATRC
      CHARACTER*16 CONCEP,NOMCMD,TYPMOD,
     &             NMPARN(1),NMPARR(2),NMPARC(3),NMPARF(2),NMPARM(2)
      CHARACTER*19 MASSE,RAIDE,DYNAM,SOLVEU,AMOR,MATREF
      CHARACTER*24 VALK(3),METRES,K24RC,KBID,K24MOD,K24STU,K24MOE,
     &             K24PAR
      PARAMETER   ( MXDDL=1,MITERC=10000,NMULTC=2)
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFMAJ()
      CALL INFNIV(IFM,NIV)
      RZERO=0.D0
      IZERO=0
C-----------------------------------------------------------------------
C------------------ INITIALIZATIONS/READING OF THE USER-DATA -----------
C-----------------------------------------------------------------------

C     --- OUTPUT CONCEPT ---
      CALL GETRES(TABLE,CONCEP,NOMCMD)

C     ------------------------------------------------------------------
C     ------------------------MACRO_MODE_MECA PARALLELE (PART I)--------
C     ------------------------------------------------------------------
C     --- RECUPERATION ET TEST DE VALIDITE DES PARAMETRES
C     ------------------------------------------------------------------
      MPICOW=COMCOU(0)
      MPICOU=COMCOU(1)
      CALL MPIEXE('MPI_RANG_SIZE',MPICOW,IBID,RANG,NBPROC)
      IF (MPICOW.NE.MPICOU) CALL ASSERT(.FALSE.)
      CALL GETVIS('PARALLELISME_MACRO','TYPE_COM',1,IARG,1,TYPECO,L)
      VALK(1)='TYPE_COM'
      VALK(2)='RANG'
      VALK(3)='NBPROC'
      VALI(1)=TYPECO
      VALI(2)=RANG
      VALI(3)=NBPROC
      IF (L.NE.1)
     &  CALL U2MESG('F','APPELMPI_6',1,VALK,1,VALI,IZERO,RZERO)

      IF ((((TYPECO.NE.1).AND.(TYPECO.NE.2).AND.(TYPECO.NE.-999))
     &      .OR.(NBPROC.LT.1).OR.(RANG.LT.0)))
     &  CALL U2MESG('F','APPELMPI_8',3,VALK,3,VALI,IZERO,RZERO)

      IF (((TYPECO.EQ.1).OR.(TYPECO.EQ.2)).AND.(NBPROC.GT.1)) THEN
        LCOMOD=.TRUE.
      ELSE
        LCOMOD=.FALSE.
      ENDIF
C     ------------------------------------------------------------------


C     --- READ OF MATRICES, CHECK OF REFERENCES ---
C     --- COMPUTATION OF THE MATRIX DESCRIPTORS ---
      CALL GETVTX(' ','TYPE_MODE',1,IARG,1,TYPMOD,IBID)
      IF (TYPMOD .EQ. 'DYNAMIQUE') THEN
        MATRA = 'MATR_RIGI'
        MATRB = 'MATR_MASS'
        MATRC = 'MATR_AMOR'
      ELSEIF (TYPMOD .EQ. 'MODE_FLAMB') THEN
        MATRA = 'MATR_RIGI'
        MATRB = 'MATR_RIGI_GEOM'
      ELSEIF (TYPMOD .EQ. 'GENERAL') THEN
        MATRA = 'MATR_A'
        MATRB = 'MATR_B'
        MATRC = 'MATR_C'
      ELSEIF (TYPMOD .EQ. 'MODE_COMPLEXE') THEN
        MATRA = 'MATR_RIGI'
        MATRB = 'MATR_MASS'
        MATRC = 'MATR_AMOR'
      ENDIF
      CALL GETVID(' ',MATRA,1,IARG,1,RAIDE,L)
      CALL GETVID(' ',MATRB,1,IARG,1,MASSE,L)
      AMOR=' '
      LAMOR=0
      IF ((TYPMOD.EQ.'GENERAL').OR.(TYPMOD.EQ.'MODE_COMPLEXE'))
     &  CALL GETVID(' ',MATRC,1,IARG,1,AMOR,LAMOR)
      IF (LAMOR.EQ.0) THEN
        LC=.FALSE.
      ELSE
        LC=.TRUE.
      ENDIF

      CALL VRREFE(MASSE,RAIDE,IRET)
      IF ( IRET .NE. 0 ) THEN
        VALK(1) = RAIDE
        VALK(2) = MASSE
        CALL U2MESK('F','ALGELINE2_58', 2 ,VALK)
      ENDIF
      CALL MTDSCR(MASSE)
      CALL JEVEUO(MASSE//'.&INT','E',LMASSE)
      CALL MTDSCR(RAIDE)
      CALL JEVEUO(RAIDE//'.&INT','E',LRAIDE)
C   --- REFERENCE MATRICE TO BE USE AS A PATTERN FOR BUILDING THE ---
C   --- DYNAMIC MATRICES (THE ISSUE IS SYMMETRIC OR NOT)          ---
      MATREF=RAIDE
      IF (ZI(LMASSE+4).EQ.0) MATREF=MASSE
      IF (ZI(LRAIDE+4).EQ.0) MATREF=RAIDE
      IF (LC) THEN
        CALL MTDSCR(AMOR)
        CALL JEVEUO(AMOR//'.&INT','E',LAMOR)
        IF (ZI(LAMOR+4).EQ.0) MATREF=AMOR
      ENDIF

C     --- READING/TREATEMENT SD LINEAR SOLVER  ---
      CALL JEVEUO(RAIDE//'.REFA','L',JREFA)
      SOLVEU='&&OP0032.SOLVEUR'
      CALL CRESOL(SOLVEU)
      CALL JEVEUO(SOLVEU//'.SLVK','L',ISLVK)
      CALL JEVEUO(SOLVEU//'.SLVI','L',ISLVI)
      METRES=ZK24(ISLVK)
      IF((METRES(1:4).NE.'LDLT').AND.(METRES(1:10).NE.'MULT_FRONT').AND.
     &   (METRES(1:5).NE.'MUMPS')) CALL U2MESS('F','ALGELINE5_71')


C     --- TYPE OF EIGENVALUE-COUNTING METHOD ---
      CALL GETVTX('COMPTAGE','METHODE',1,IARG,1,TYPMET,IBID)

C     --- AUTOMATIC PARAMETRIZATION WITH 'AUTO'                 ---
      IF (TYPMET(1:4).EQ.'AUTO') THEN
        IF (ZI(LMASSE+3)*ZI(LMASSE+4)*ZI(LRAIDE+3)*ZI(LRAIDE+4).NE.1
     &      .OR.LC) THEN
          TYPMET='APM'
        ELSE
          TYPMET='STURM'
        ENDIF
        VALK(1)=TYPMET
        CALL U2MESK('I','ALGELINE2_27',1,VALK)
      ENDIF
C     --- IF GENERAL: KIND OF COMPUTATION   ---
      IF (TYPMOD(1:7).EQ.'GENERAL') THEN
        IF (ZI(LMASSE+3)*ZI(LMASSE+4)*ZI(LRAIDE+3)*ZI(LRAIDE+4).NE.1
     &      .OR.LC) THEN
          TYPMOD='MODE_COMPLEXE'
        ELSE
          TYPMOD='MODE_FLAMB'
        ENDIF
        VALK(1)=TYPMOD
        CALL U2MESK('I','ALGELINE2_31',1,VALK)
      ENDIF

C     --- TEMPORARY EXCLUSION RULES                             ---
C     --- + DEFAULT VALUES                                      ---
      IF ((TYPMOD(1:13).EQ.'MODE_COMPLEXE').AND.
     &    (TYPMET(1:3).NE.'APM')) THEN
        CALL U2MESS('I','ALGELINE4_20')
        TYPMET='APM'
      ENDIF
      IF ((TYPMOD(1:13).NE.'MODE_COMPLEXE').AND.
     &    (TYPMET(1:5).NE.'STURM')) THEN
        CALL U2MESS('I','ALGELINE4_20')
        TYPMET='STURM'
      ENDIF

C     --- KIND OF COMPUTATION : REAL (GEP), DYNAMIC OR BUCKLING    ---
C     --- NBMOD: SIZE OF THE LIST OF MODES (FREQUENCIES OR BUCKLING---
C     --- MODES).
C     --- NAME OF JEVEUX OBJECTS NEEDED FOR BUILDING THE CARTE ---
      K24MOD='&&OP0032.LISTE_MODE'
      K24MOE='&&OP0032.LISTE_MODE_EFF'
      K24STU='&&OP0032.RESU_STURM'
      LDYNA=.FALSE.
      LFLAMB=.FALSE.
      NBMOD=-9999
      IF (TYPMOD(1:9).EQ.'DYNAMIQUE') THEN
C     --- COUPLE OR LIST OF FREQUENCIES ---
        LDYNA=.TRUE.
        CALL GETVR8(' ','FREQ',1,IARG,0,RBID,L)
        NBMOD=ABS(L)
        CALL WKVECT(K24MOD,'V V R',NBMOD,JLMOD)
        CALL WKVECT(K24MOE,'V V R',NBMOD,JLMOE)
        CALL WKVECT(K24STU,'V V I',NBMOD-1,JSTU)
        CALL GETVR8(' ','FREQ',1,IARG,NBMOD,ZR(JLMOD),L)
        IF (L.NE.NBMOD) CALL ASSERT(.FALSE.)
        DO 10 K=1,NBMOD-1
          ZI(JSTU+K-1)=IZERO
          ZR(JLMOE+K-1)=RZERO
          IF (ZR(JLMOD+K).LE.ZR(JLMOD+K-1)) CALL ASSERT(.FALSE.)
   10   CONTINUE
        ZR(JLMOE+NBMOD-1)=RZERO

      ELSE IF (TYPMOD(1:13).EQ.'MODE_COMPLEXE') THEN
C     --- CHARACTERISTIC OF THE COMPLEX SHAPE ---
        CALL GETVTX(' ','TYPE_CONTOUR',1,IARG,1,TYPCON,L1)
        CALL GETVR8(' ','RAYON_CONTOUR',1,IARG,1,RAYONC,L2)
        CALL GETVC8(' ','CENTRE_CONTOUR',1,IARG,1,CENTRC,L3)
        IF ((ABS(L1)*ABS(L2)*ABS(L3)).NE.1) CALL ASSERT(.FALSE.)
        CALPAC(1) = DBLE(CENTRC)
        CALPAC(2) = DIMAG(CENTRC)
        CALPAC(3) = RAYONC
        NBMOD=2

      ELSE IF (TYPMOD(1:10).EQ.'MODE_FLAMB') THEN
C     --- COUPLE OR LIST OF BUCKLING MODES ---
        LFLAMB=.TRUE.
        CALL GETVR8(' ','CHAR_CRIT',1,IARG,0,RBID,L)
        IF (ABS(L).GE.2) THEN
          NBMOD=ABS(L)
          CALL WKVECT(K24MOD,'V V R',NBMOD,JLMOD)
          CALL WKVECT(K24MOE,'V V R',NBMOD,JLMOE)
          CALL WKVECT(K24STU,'V V I',NBMOD-1,JSTU)
          CALL GETVR8(' ','CHAR_CRIT',1,IARG,NBMOD,ZR(JLMOD),L)
          IF (L.NE.NBMOD) CALL ASSERT(.FALSE.)
          DO 12 K=1,NBMOD-1
            ZI(JSTU+K-1)=IZERO
            ZR(JLMOE+K-1)=RZERO
            IF (ZR(JLMOD+K).LE.ZR(JLMOD+K-1)) CALL ASSERT(.FALSE.)
   12     CONTINUE
          ZR(JLMOE+NBMOD-1)=RZERO
        ELSE
C       --- PARAMETRIZATION PB
          CALL ASSERT(.FALSE.)
        ENDIF

      ELSE
C     --- BAD VALUE OF TYMOD ---
        CALL ASSERT(.FALSE.)

      ENDIF

C     --- GET THE PARAMETERS OF THE METHOD                      ---
C     --- INITIALIZATIONS JUST IN CASE                          ---
      FCORIG=1.D-2
      PRECSH=1.D-2
      NBRSS=5
      NBTETC=40
      NITERC=3
      IF (TYPMET(1:5).EQ.'STURM') THEN
        IF (TYPMOD(1:9).EQ.'DYNAMIQUE') THEN
          CALL GETVR8('COMPTAGE','SEUIL_FREQ',1,IARG,1,FCORIG,IBID)
          OMECOR=OMEGA2(FCORIG)
        ELSE
          CALL GETVR8('COMPTAGE','SEUIL_CHAR_CRIT',1,IARG,1,FCORIG,
     &                IBID)
          OMECOR=FCORIG
        ENDIF
        CALL GETVR8('COMPTAGE','PREC_SHIFT',1,IARG,1,PRECSH,IBID)
        CALL GETVIS('COMPTAGE','NMAX_ITER_SHIFT',1,IARG,1,NBRSS,IBID)
      ELSE IF (TYPMET(1:3).EQ.'APM') THEN
        CALL GETVIS('COMPTAGE','NBPOINT_CONTOUR',1,IARG,1,NBTETC,IBID)
        CALL GETVIS('COMPTAGE','NMAX_ITER_CONTOUR',1,IARG,1,NITERC,IBID)
C     --- TEMPORARY, WE UNPLUG THE USE OF ROMBOUT METHOD, IT NEEDS ---
C     --- TO BE MORE RELIABLE                                      ---
        TYPCHA='LDLT'
C        TYPCHA='ROMBOUT'
C        CALL GETVTX('COMPTAGE','POLYNOME_CHARAC',1,IARG,1,TYPCHA,IBID)
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF

C-----------------------------------------------------------------------
C-------------------- EXCLUSION RULES, PARTICULAR CASES ----------------
C-----------------------------------------------------------------------


C     --- EXCLUSION RULE IF NONSYMETRIC OR COMPLEXE GEP OR QEP  ---
      IF ((ZI(LMASSE+3)*ZI(LMASSE+4)*ZI(LRAIDE+3)*ZI(LRAIDE+4).NE.1
     &      .OR.LC)) THEN
        IF (TYPMOD(1:13).NE.'MODE_COMPLEXE')
     &    CALL U2MESS('F','ALGELINE4_10')
      ENDIF

C     --- CURRENT SCOPE OF USE OF THE OPTION TYPCHA='ROMBOUT'   ---
      IF ((TYPMET(1:3).EQ.'APM').AND.(TYPCHA(1:7).EQ.'ROMBOUT')) THEN
        IF (LC.OR.(ZK24(JREFA+9)(1:4).EQ.'GENE').OR.
     &     (ZI(LMASSE+3)*ZI(LMASSE+4)*ZI(LRAIDE+3)*ZI(LRAIDE+4).NE.1))
     &    CALL U2MESS('F','ALGELINE4_17')
      ENDIF


C-----------------------------------------------------------------------
C-------------------------- PRE-TRAITEMENTS ----------------------------
C-----------------------------------------------------------------------

C     --- PREPARATION FOR THE COMPUTATION OF THE DYNAMIC MATRIX ---
C     --- IN GEP ONLY DYNAM, IN QEP DYNAM            ---
      IF ((TYPMET(1:5).EQ.'STURM').OR.
     &   ((TYPMET(1:3).EQ.'APM').AND.(TYPCHA(1:4).EQ.'LDLT'))) THEN
        DYNAM = '&&OP0032.MATR_DYNAM'
        IF (TYPMET(1:5).EQ.'STURM') THEN
C     --- IF STURM TEST, DYNAM'TYPE IS THE SAME AS RAIDE'S ONE: ---
C     --- OFTEN REAL                                            ---
          CALL MTDEFS(DYNAM,RAIDE,'V',' ')
        ELSE
C     --- IF APM TEST, DYNAM'TYPE IS ALWAYS COMPLEX.            ---
          CALL MTDEFS(DYNAM,MATREF,'V','C')
        ENDIF
        CALL JEVEUO(DYNAM(1:19)//'.REFA','E',KREFA)
        ZK24(KREFA-1+7)=SOLVEU
        CALL MTDSCR(DYNAM)
        CALL JEVEUO(DYNAM(1:19)//'.&INT','E',LDYNAM)
      ENDIF

C     --- COMPUTATION OF THE LAGRANGE MULTIPLIERS ---
      IF (TYPMET(1:5).EQ.'STURM') THEN
        NEQ = ZI(LRAIDE+2)
        CALL WKVECT('&&OP0032.POSITION.DDL','V V I',NEQ*MXDDL,LDDL)
        CALL WKVECT('&&OP0032.DDL.BLOQ.CINE','V V I',NEQ,LPROD)
        CALL VPDDL(RAIDE, MASSE, NEQ, NBLAGR, NBCINE, NEQACT, ZI(LDDL),
     &             ZI(LPROD),IERD)
      ENDIF

C-----------------------------------------------------------------------
C-----------------------------STURM METHOD -----------------------------
C-----------------------------------------------------------------------
      NBROW=-9999
      IF (TYPMET(1:5).EQ.'STURM') THEN

        IF (NBMOD.LT.2) CALL ASSERT(.FALSE.)
        NBROW=NBMOD-1

C     ------------------------------------------------------------------
C     ------------------------MACRO_MODE_MECA PARALLELE (PART II)-------
C     ------------------------------------------------------------------
C     --- SI TYPECO=1 OU 2 ON PASSE EN COM LOCAL + DISTRIBUTION DES
C     ---     TESTS DE STURM + ON REVIENT AU COMM_WORLD.
C     ------------------------------------------------------------------
        IF (LCOMOD) THEN
C         --- CALCUL DU VECTEUR DE COULEURS POUR DETERMINER LES SOUS-
C         --- COMMUNICATEURS ASSOCIES A CHAQUE ANALYSE+FACTO. MUMPS
C         --- VECTEUR COULEUR ZI(JKPAR+I)= FREQ A TRAITER PAR LE PROC
C         --- DE RANG I. ON COMMENCE PAR FREQ=0,1...NBROW.
C         --- DONC ZI(JKPAR+NBPROC-1)=NBROW (IMPORTANT POUR VPFOPR).
C         --- ON GARDE CONTIGUES LES PROCS DEDIES A UNE FACTO MUMPS 
C         --- ET EN CAS DE DESEQUILIBRAGE DE CHARGE ON DONNE 1 PROC
C         --- DE PLUS AUX PREMIERES FREQUENCES.
          K24PAR='&&OP0032.COULEUR'
          CALL WKVECT(K24PAR,'V V I',NBPROC,JKPAR)
          CALL VECINT(NBPROC,-9999,ZI(JKPAR))
          IF (TYPECO.EQ.1) THEN
C           --- VERIF REDONDANTE AU CAS OU (DEJA FAIT DS MACRO PYTHON)
            IF (NBPROC.LT.NBROW) CALL ASSERT(.FALSE.)
            IF ((NBPROC.GT.NBROW).AND.(METRES(1:5).NE.'MUMPS'))
     &        CALL ASSERT(.FALSE.)
            L1=NBPROC/NBROW
            L11=L1+1
            L2=NBPROC-L1*NBROW
            L21=L2+1
            L3=L11*L2
            DO 40 K=1,L2
              CALL VECINT(L11,K,ZI(JKPAR+(K-1)*L11))
   40       CONTINUE
            DO 41 K=L21,NBROW
              CALL VECINT(L1,K,ZI(JKPAR+L3+(K-L21)*L1))
   41       CONTINUE             
          ELSE IF (TYPECO.EQ.2) THEN
            IF (NBROW.NE.1) CALL ASSERT(.FALSE.)
            L1=NBPROC/2
            L2=NBPROC-2*L1
            L11=L1+L2
            CALL VECINT(L11,0,ZI(JKPAR))
            CALL VECINT(L1,1,ZI(JKPAR+L11))
            IF (L11.NE.L1) THEN
              VALI(1)=L11
              VALI(2)=L1
              CALL U2MESG('I','MODAL_13',0,KBID,2,VALI,0,RBID)
            ENDIF
          ENDIF
C         --- ULTIME VERIF VECTEUR COULEUR
          DO 42 K=1,NBPROC
            L1=ZI(JKPAR+K-1)
            IF ((L1.LT.0).OR.(L1.GT.NBROW)) CALL ASSERT(.FALSE.)
   42     CONTINUE

C         --- FREQUENCE COURANTE CAD FREQ A TRAITER PAR LE PROC COURANT
          FRECOU=ZI(JKPAR+RANG)
C         --- ON AFFECTE UN COMMUNICATEUR LOCAL MPICOU POUR NE PAS
C         --- INTERFERER AVEC LA FACTORISATION NUMERIQUE.
C         --- ON REMET LE COMM_WORLD MPICOW AU SEIN DE VPFOPR.
C         --- ON DETRUIT LE MPICOU QU'APRES LA DESTRUCTION DE L'OCCU
C         --- RENCE MUMPS ASSOCIEE.
          CALL MPIEXE('MPI_COMM_SPLIT',MPICOW,MPICOU,FRECOU,0)
          IF (MPICOW.EQ.MPICOU) CALL ASSERT(.FALSE.)
          CALL MPIEXE('AFFE_COMM_REFE',MPICOU,IBID,1,IBID)
          IF (TYPECO.EQ.1) THEN
C         --- CALCUL // TYPE 1
            KOPT1='STURML1P'
            KOPTN='STURMLNP'
            IF (FRECOU.EQ.1) THEN
C         --- LE PROC (ET SES AMIS DU MEME SOUS-COMMUNICATEUR) TRAITE LA
C         --- PREMIERE SOUS-BANDE
              LFIRST=.TRUE.
              K1=1
              K2=0
            ELSE
C         --- LE PROC (ET SES AMIS DU MEME SOUS-COMMUNICATEUR) SAUTENT
C         --- LA PREMIERE SOUS-BANDE ET TRAITE LA FREQ FRECOU
              LFIRST=.FALSE.
              K1=FRECOU
              K2=K1
            ENDIF
          ELSE IF (TYPECO.EQ.2) THEN
C         --- CALCUL // TYPE 2
            IF (FRECOU.EQ.0) THEN
C         --- LE PROC (ET SES AMIS DU MEME SOUS-COMMUNICATEUR) TRAITE
C         --- LA PREMIERE FREQUENCE DE LA PREMIERE SOUS-BANDE
              KOPT1='STURML10'
            ELSE IF (FRECOU.EQ.1) THEN
C         --- LE PROC (ET SES AMIS DU MEME SOUS-COMMUNICATEUR) TRAITE
C         --- LA SECONDE FREQUENCE DE LA PREMIERE SOUS-BANDE
              KOPT1='STURML11'
            ENDIF
            KOPTN='XXXXXXXX'
            LFIRST=.TRUE.
            K1=1
            K2=0
          ELSE
            CALL ASSERT(.FALSE.)
          ENDIF

        ELSE
C         --- CALCUL SEQ: LE PROC FAIT LES NBROW CALCULS
          K1=2
          K2=NBROW
          LFIRST=.TRUE.
          KOPT1='STURML1'
          KOPTN='STURMLN'
        ENDIF
        NPIVOT(1)=-9999
        NPIVOT(2)=-9999

C       --- TO PERFORM A LIST OF BANDES ---
C       --- STEP 1: FIRST BANDE         ---
        IF (LDYNA) THEN
          OMIN=OMEGA2(ZR(JLMOD))
          OMAX=OMEGA2(ZR(JLMOD+1))
        ELSE IF (LFLAMB) THEN
          OMIN=ZR(JLMOD)
          OMAX=ZR(JLMOD+1)
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
        IF (LFIRST) THEN
          CALL VPFOPR(KOPT1,TYPMOD,LMASSE,LRAIDE,LDYNAM,OMIN,OMAX,
     &                RBID,ZI(JSTU),NPIVOT,OMECOR,PRECSH,NBRSS,NBLAGR,
     &                SOLVEU,DET,IDET)
C          --- WE STORE THE POSSIBLY CORRECTED FREQUENCY/BUCKLING MODE
          IF (LDYNA) THEN
            ZR(JLMOE)=FREQOM(OMIN)
            ZR(JLMOE+1)=FREQOM(OMAX)
          ELSE
            ZR(JLMOE)=OMIN
            ZR(JLMOE+1)=OMAX
          ENDIF
        ENDIF
        DO 20 K=K1,K2
C        --- STEP K: BANDE NUMBER K
          IF (LDYNA) THEN
            OMIN=OMEGA2(ZR(JLMOD+K-1))
            OMAX=OMEGA2(ZR(JLMOD+K))
          ELSE
            OMIN=ZR(JLMOD+K-1)
            OMAX=ZR(JLMOD+K)
          ENDIF
          NPIVOT(1)=NPIVOT(2)
          NPIVOT(2)=K
          CALL VPFOPR(KOPTN,TYPMOD,LMASSE,LRAIDE,LDYNAM,OMIN,OMAX,
     &                RBID,ZI(JSTU+K-1),NPIVOT,OMECOR,PRECSH,NBRSS,
     &                NBLAGR,SOLVEU,DET,IDET)
          IF (LDYNA) THEN
            ZR(JLMOE+K)=FREQOM(OMAX)
          ELSE
            ZR(JLMOE+K)=OMAX
          ENDIF
   20   CONTINUE

C     ------------------------------------------------------------------
C     -----------------------MACRO_MODE_MECA PARALLELE (PART III)-------
C     ------------------------------------------------------------------
C     --- SI TYPECO=1/2 ON COMMUNIQUE TOUS LES RESULTATS DES CALCULS.
C     ------------------------------------------------------------------
        IF (LCOMOD) THEN
          CALL MPICM1('MPI_SUM','I',NBMOD-1,IBID,ZI(JSTU),RBID,CBID)
          CALL MPICM1('MPI_SUM','R',NBMOD,IBID,IBID,ZR(JLMOE),CBID)
          CALL JEDETR(K24PAR)
        ENDIF

C-----------------------------------------------------------------------
C------------------------ ARGUMENT PRINCIPAL METHOD --------------------
C-----------------------------------------------------------------------
C   --- COMBO OF THE WORK OF H.J.JUNG (HYUNDAI)/O.BERTRAND (PHD INRIA)
      ELSE IF (TYPMET(1:3).EQ.'APM') THEN

        NBROW=1
C   --- VALUE TO START SOME SELF-TESTING PROCEDURES: ONLY FOR ---
C   --- DEVELOPPERS AND FOR DEBBUGING PHASE                   ---
        LTEST=.FALSE.
        ITEST=0
C       LTEST=.TRUE.

C   --- FOR PRINT IN THE FILE IFAPM THE DISPLAY Z/ARG(PC(Z)) ONLY  ---
C   --- FOR DEBUGGING ISSUES                                       ---
        IFAPM=18
        IMPR='OUI'
        IMPR='NON'

C   --- FOR TEST ISSUE ONLY (SEE APM012/APTEST)---
        IF ((LTEST).AND.(TYPCHA(1:7).EQ.'ROMBOUT')) THEN
          IF (ITEST.EQ.3) THEN
            NK=20
          ELSE IF (ITEST.EQ.4) THEN
            NK=10
          ELSE IF (ITEST.EQ.1) THEN
            NK=4
          ELSE IF (ITEST.EQ.0) THEN
            NK=2
          ENDIF
        ELSE
          NK=ZI(LMASSE+2)
        ENDIF
C   --- STEPS 0/1/2 OF THE APM ALGORITHM IF WE USE ROMBOUT VARIANT ---
        IF (TYPCHA(1:7).EQ.'ROMBOUT')
     &    CALL APM012(NK,K24RC,LTEST,ITEST,RAYONC,CENTRC,LRAIDE,
     &                LMASSE,SOLVEU)

C   --- STEPS 3, 4 AND 5 OF THE APM ALGORITHM
C   --- ITERATION LOOP TO DETERMINE THE STABILIZED NUMBER OF   ---
C   --- EIGENVALUES. TRICKS TO LIMIT THE NUMBER OF COMPUTATION ---
C   --- WITH THE PARAMETERS MITERC AND NMULTC
        NBTET0=MIN(MITERC,MAX(1,NBTETC/NMULTC))
        NBTET1=MIN(MITERC,NBTETC)
        NBTET2=MIN(MITERC,NBTETC*NMULTC)
        PIVOT1=0
        PIVOT2=-9999
        NBEV0=0
        NBEV1=0
        NBEV2=0
        DO 30 II=1,NITERC
          IF (II.EQ.1) THEN
            IF (IMPR.EQ.'NON')
     &        CALL APM345(NBTET0,TYPCON,RAYONC,CENTRC,NK,K24RC,NBEV0,
     &                  LTEST,TYPCHA,LRAIDE,LMASSE,LDYNAM,SOLVEU,LAMOR,
     &                  LC,IMPR,IFAPM)
              CALL APM345(NBTET1,TYPCON,RAYONC,CENTRC,NK,K24RC,NBEV1,
     &                  LTEST,TYPCHA,LRAIDE,LMASSE,LDYNAM,SOLVEU,LAMOR,
     &                  LC,IMPR,IFAPM)
          ENDIF
          IF (IMPR.EQ.'NON')
     &      CALL APM345(NBTET2,TYPCON,RAYONC,CENTRC,NK,K24RC,NBEV2,
     &                  LTEST,TYPCHA,LRAIDE,LMASSE,LDYNAM,SOLVEU,LAMOR,
     &                  LC,IMPR,IFAPM)

          WRITE(IFM,4000)NBTET0,NBTET1,NBTET2,NBEV0,NBEV1,NBEV2


C   --- SHIFT OF THE THREE LEVELS OF DISCRETISATIONS ---
C   --- TO CONTINUE THE HEURISTIC                    ---
          IF (((NBEV0.NE.NBEV1).OR.(NBEV1.NE.NBEV2)).AND.
     &        (IMPR.EQ.'NON')) THEN
            NBTET0=NBTET1
            NBTET1=NBTET2
            NBTET2=NMULTC*NBTET2

            NBEV0=NBEV1
            NBEV1=NBEV2

C   --- ERROR MESSAGES
            IF (NBTET2.GT.MITERC)
     &        CALL U2MESI('F','ALGELINE4_13',1,MITERC)
            IF (II.EQ.NITERC) CALL U2MESI('F','ALGELINE4_14',1,NITERC)

          ELSE IF (IMPR.EQ.'NON') THEN
C    --- THE HEURISTIC CONVERGES
            PIVOT2=NBEV1
            WRITE(IFM,4010)
            GOTO 31
          ELSE IF (IMPR.EQ.'OUI') THEN
            WRITE(IFM,4020)
            CALL ASSERT(.FALSE.)
          ENDIF
   30   CONTINUE
   31   CONTINUE
        IF (TYPCHA(1:7).EQ.'ROMBOUT') CALL JEDETR(K24RC)
        IF (PIVOT2.LT.0) CALL U2MESS('F','ALGELINE4_22')

      ELSE
C   --- ILLEGAL OPTION ---
        CALL ASSERT(.FALSE.)
      ENDIF

C-----------------------------------------------------------------------
C-------------------------- POSTTRAITEMENTS ----------------------------
C-----------------------------------------------------------------------

C   --- DESTRUCTION OF THE DYNAMIC MATRIX
      IF ((TYPMET(1:5).EQ.'STURM').OR.
     &   ((TYPMET(1:3).EQ.'APM').AND.(TYPCHA(1:4).EQ.'LDLT')))
     &  CALL DETRSD('MATR_ASSE',DYNAM)

C     ------------------------------------------------------------------
C     -----------------------MACRO_MODE_MECA PARALLELE (PART IV) -------
C     ------------------------------------------------------------------
C     --- AVANT DE QUITTER L'OP. ON REMET LE COM WORLD (AU CAS OU)
C     --- DESTRUCTION DES SOUS-COMMUNICATEURS EVENTUELLEMENT ASSOCIES A
C     --- UNE OCCURENCE MUMPS (APRES CELLE DE LADITE OCCURENCE)
C     ------------------------------------------------------------------
      IF (LCOMOD) THEN
        CALL MPIEXE('AFFE_COMM_REFE',MPICOW,IBID,1,IBID)
        CALL MPIEXE('MPI_COMM_FREE',MPICOU,IBID,IBID,IBID)
      ENDIF

C   --- PRINT THE RESULTS TO THE MSG FILE AND SAVE  THE EVALUATED ---
C   --- NUMBER OF FREQUENCIES AS WELL AS THE CALCULATION PARAMS   ---
C   --- TO AN SD_TABLE                                            ---

      IF (TYPMET(1:3).EQ.'APM') THEN
        TYPEP='C'
        IF (TYPCON(1:6).EQ.'CERCLE') THEN
          DIMC1=RAYONC
          ZIMC1=CENTRC
        ENDIF
        CALL VPECST(IFM,TYPMOD,OMGMIN,OMGMAX,PIVOT1,PIVOT2,
     &              NBFREQ,NBLAGR,TYPEP,TYPCON,DIMC1,ZIMC1)
      ENDIF

      CALL TBCRSD(TABLE,'G')
      CALL TITRE

C   --- BUILDING OF THE DATA STRUCTURE CARTE  ---
      NMPARN(1) = 'NB_MODE'
      NMPARR(1) = 'FREQ_MIN'
      NMPARR(2) = 'FREQ_MAX'
      NMPARC(1) = 'CENTRE_R'
      NMPARC(2) = 'CENTRE_I'
      NMPARC(3) = 'RAYON'
      NMPARF(1) = 'CHAR_CRIT_MIN'
      NMPARF(2) = 'CHAR_CRIT_MAX'
      NMPARM(1) = 'BORNE_MIN_EFFECT'
      NMPARM(2) = 'BORNE_MAX_EFFECT'

      TPPARN(1) = 'I'
      TPPARR(1) = 'R'
      TPPARR(2) = 'R'
      TPPARC(1) = 'R'
      TPPARC(2) = 'R'
      TPPARC(3) = 'R'
      TPPARF(1) = 'R'
      TPPARF(2) = 'R'
      TPPARM(1) = 'R'
      TPPARM(2) = 'R'

      CALL TBAJPA(TABLE,2,NMPARR,TPPARR)
      CALL TBAJPA(TABLE,3,NMPARC,TPPARC)
      CALL TBAJPA(TABLE,2,NMPARF,TPPARF)
      CALL TBAJPA(TABLE,2,NMPARM,TPPARM)
      CALL TBAJPA(TABLE,1,NMPARN,TPPARN)

C     --- FIRST ROW (TITLE OF THE COLUMNS) OF THE ASTER TABLE ---
C     --- NUMERICAL VALUES OF THE OTHERS ROWS ---

      IF (TYPMOD(1:9).EQ.'DYNAMIQUE') THEN
        DO 50 K=1,NBROW
          CALL TBAJLI(TABLE,1,NMPARN,ZI(JSTU+K-1),RBID,CBID,KBID,0)
          CALPAR(1)=ZR(JLMOD+K-1)
          CALPAR(2)=ZR(JLMOD+K)
          CALL TBAJLI(TABLE,2,NMPARR,IBID,CALPAR,CBID,KBID,K)
          CALPAR(1)=ZR(JLMOE+K-1)
          CALPAR(2)=ZR(JLMOE+K)
          CALL TBAJLI(TABLE,2,NMPARM,IBID,CALPAR,CBID,KBID,K)
   50   CONTINUE

      ELSE IF (TYPMOD(1:13).EQ.'MODE_COMPLEXE') THEN
        CALL TBAJLI(TABLE,1,NMPARN,NBFREQ,RBID,CBID,KBID,0)
        CALL TBAJLI(TABLE,3,NMPARC,IBID,CALPAC,CBID,KBID,1)

      ELSE IF (TYPMOD(1:10).EQ.'MODE_FLAMB') THEN
        DO 55 K=1,NBROW
          CALL TBAJLI(TABLE,1,NMPARN,ZI(JSTU+K-1),RBID,CBID,KBID,0)
          CALPAF(1)=ZR(JLMOD+K-1)
          CALPAF(2)=ZR(JLMOD+K)
          CALL TBAJLI(TABLE,2,NMPARF,IBID,CALPAF,CBID,KBID,K)
          CALPAF(1)=ZR(JLMOE+K-1)
          CALPAF(2)=ZR(JLMOE+K)
          CALL TBAJLI(TABLE,2,NMPARM,IBID,CALPAF,CBID,KBID,K)
   55  CONTINUE

      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF

C  ---- DESTRUCTION OF THE TEMPORARY DATA STRUCTURES ---
      IF (TYPMOD.NE.'MODE_COMPLEXE') THEN
        CALL JEDETR(K24MOD)
        CALL JEDETR(K24MOE)
        CALL JEDETR(K24STU)
      ENDIF
      CALL JEDEMA()

C-----------------------------------------------------------------------
C-------------------------- FORTRAN PRINT FORMAT -----------------------
C-----------------------------------------------------------------------
 4000 FORMAT('(METHODE APM) POUR LES 3 NIVEAUX DE DISCRETISATION '
     &       'SUIVANTS',/,
     &       ' --- ',I5,' --- ',I5,' --- ',I5,' ---',/,
     &       ' NOMBRE DE VALEURS PROPRES DETECTEES ',/,
     &       ' --- ',I5,' --- ',I5,' --- ',I5,' ---')
 4010 FORMAT('(METHODE APM) CONVERGENCE DE L''HEURISTIQUE ')
 4020 FORMAT('(METHODE APM) ATTENTION CALCUL DE TEST POUR IMPRIMER LA',
     &       ' COURBE DES NOMBRES DE TOURS ')

      END
