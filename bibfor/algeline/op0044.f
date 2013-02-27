      SUBROUTINE OP0044()
C-----------------------------------------------------------------------
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
C TOLE CRP_20
C        MODE_ITER_INV
C     RECHERCHE DE MODES PROPRES PAR LA METHODE D'ITERATION INVERSE
C     ------------------------------------------------------------------
C        - POUR LE PROBLEME GENERALISE AUX VALEURS PROPRES.
C                         2
C                        L (M) Y  + (K) Y = 0
C
C          LES MATRICES (K), (C) ET (M) SONT REELLES SYMETRIQUES
C          LES VALEURS PROPRES ET DES VECTEURS PROPRES SONT REELS
C
C        - POUR LE PROBLEME QUADRATIQUE AUX VALEURS PROPRES.
C                         2
C                        L (M) Y  + L (C) Y + (K) Y = 0
C
C          LES MATRICES (K), (C) ET (M) SONT REELLES SYMETRIQUES
C          LES VALEURS PROPRES ET DES VECTEURS PROPRES SONT REELS OU
C          COMPLEXES CONJUGUEES
C
C     ------------------------------------------------------------------
C LOC NFREQ  : IS : NB DE FREQUENCES DONNEES PAR L'UTILISATEUR
C LOC MXFREQ : IS : NB MAXIMUM DE FREQUENCES A CALCULER
C LOC NFREQB : IS : NB DE FREQUENCES EFFECTIVES DANS LA BANDE DONNEE
C-----------------------------------------------------------------------
C RESPONSABLE BOITEAU O.BOITEAU
      IMPLICIT NONE

C VARIABLES LOCALES
      INCLUDE 'jeveux.h'

      INTEGER NBPARI, NBPARR, NBPARK, NBPARA, MXDDL
      PARAMETER     ( NBPARI=8 , NBPARR=16 , NBPARK=3, NBPARA=27 )
      PARAMETER     ( MXDDL = 1 )

      INTEGER INDF, ISNNEM, IFREQ, IFM, IRET, IERFR, IERD,
     &  IDET1, IEME1, IERX, IDET2, IEME2, IEQ, I, IBID, NPIVOT(2)
      INTEGER JVALP, JDET, JIDET, JIEME, JNPAS, KFREQ, K
      INTEGER LMAT(3), L, LAMOR, LTYPRE, LBRSS, LMO, LMF, LBORNE,
     &  LMASSE, LRAIDE, LDYNAM, LFREQ, LAMORT, LDDL, LPROD, NBLAGR,
     &  LRESUI, LRESUR, LRESUK, LVALP, LVEC
      INTEGER MXFREQ, NCRITR, NBRSS, NITSEP, NITAJU, NITV, IDET(2),
     &  NFREQR, NFREQ, NCRIT, NBMOD, NA1, NAMORR, NIV, NBCINE,KREFA,
     &  NEQACT, NFREQB, MXRESF, NDIM, NPARR, NEQ, ISLVK, ISLVI, JREFA

      REAL*8 TOLSEP, TOLAJU, TOLV, FCORIG, OMECOR, PRECSH, OMEG,
     &  DET1, DET2, FR, AM, ZAM(3), ZFR(3), SEUIL, FMIN, FMAX, R8DEPI,
     &  OMEGA2, OMGMIN, OMGMAX, RBID, DEPI, R8VIDE, UNDF, RAUX1, RAUX2,
     &  DET(2)
      CHARACTER*1 CTYP, TYPER
      CHARACTER*8  OPTIOV, MODES, KNEGA
      CHARACTER*9  TYPEVP
      CHARACTER*14 MATRA,MATRB,MATRC
      CHARACTER*16 NOMCMD,TYPCON,OPTIOM,OPTIOF,OPTIOR,TYPRES,K16BID
      CHARACTER*19 MASSE,RAIDE,AMOR,DYNAM,NUMEDD,SOLVEU
      CHARACTER*24 CBORNE,WORK(5),CAMOR,CFREQ,NOPARA(NBPARA),METRES
      CHARACTER*24 VALK(2)
      COMPLEX*16 CBID, DCMPLX
      LOGICAL LBID
      CHARACTER*1 KTYP
      INTEGER      IARG
C     ------------------------------------------------------------------
      DATA ZAM     / 0.01D0 , 0.02D0 , 0.03D0 /
      DATA ZFR     / -1.0D0 , 1.00D0 , 0.00D0 /
      DATA  WORK(1)/ '&&OP0044.VALEURS_PROPRES' /
      DATA  WORK(2)/ '&&OP0044.MANTISSE_DET   ' /
      DATA  WORK(3)/ '&&OP0044.EXPOSANT_DET   ' /
      DATA  WORK(4)/ '&&OP0044.POSITION       ' /
      DATA  WORK(5)/ '&&OP0044.NOMBRE_ITERE   ' /
      DATA  CBORNE / '&&OP0044.BORNE.USR ' /
      DATA  CAMOR  / '&&OP0044.AMOR.USR ' /
      DATA  CFREQ  / '&&OP0044.CFREQ.USR ' /
      DATA  NOPARA /
     &  'NUME_MODE'       , 'ITER_QR'         , 'ITER_BATHE'      ,
     &  'ITER_ARNO'       , 'ITER_JACOBI'     , 'ITER_SEPARE'     ,
     &  'ITER_AJUSTE'     , 'ITER_INVERSE'    ,
     &  'NORME'           , 'METHODE'         , 'TYPE_MODE'       ,
     &  'FREQ'            ,
     &  'OMEGA2'          , 'AMOR_REDUIT'     , 'ERREUR'          ,
     &  'MASS_GENE'       , 'RIGI_GENE'       , 'AMOR_GENE'       ,
     &  'MASS_EFFE_DX'    , 'MASS_EFFE_DY'    , 'MASS_EFFE_DZ'    ,
     &  'FACT_PARTICI_DX' , 'FACT_PARTICI_DY' , 'FACT_PARTICI_DZ' ,
     &  'MASS_EFFE_UN_DX' , 'MASS_EFFE_UN_DY' , 'MASS_EFFE_UN_DZ' /
C     ------------------------------------------------------------------

      CALL JEMARQ()
C
      UNDF = R8VIDE()
      INDF = ISNNEM()
      DET1 = 0.D0
      DET2 = 0.D0
      IDET1 = 0
      IDET2 = 0

C     --- RECUPERATION DU RESULTAT  ---
      CALL GETRES( MODES, TYPCON, NOMCMD )
C     ------------------------------------------------------------------

C     --- TYPE DE CALCUL : DYNAMIQUE OU FLAMBEMENT OU GENERAL  ---
C     TYPE_RESU : 'DYNAMIQUE' OU 'MODE_FLAMB' OU 'GENERAL'
      CALL GETVTX(' ','TYPE_RESU',1,IARG,1,TYPRES,LTYPRE)

C     --- CATALOGUE DE COMMANDE, DIFFERENT SELON LE TYPE_RESU
C     -> ON STOCKE DANS DES VARIABLES POUR EVITER DE FAIRE DES GETXXX
C     POUR CHAQUE TYPE_RESU
C     POUR L'INSTANT TYPE_RESU='GENERAL' REVIENT A 'MODE_FLAMB'
C     SAUF LE NOM DES MATRICES
      IF (TYPRES .EQ. 'DYNAMIQUE') THEN
        MATRA = 'MATR_RIGI'
        MATRB = 'MATR_MASS'
        MATRC = 'MATR_AMOR'
        TYPEVP = 'FREQ'
      ELSEIF (TYPRES .EQ. 'MODE_FLAMB') THEN
        MATRA = 'MATR_RIGI'
        MATRB = 'MATR_RIGI_GEOM'
        TYPEVP = 'CHAR_CRIT'
      ELSEIF (TYPRES .EQ. 'GENERAL') THEN
        MATRA = 'MATR_A'
        MATRB = 'MATR_B'
        MATRC = 'MATR_C'
        TYPEVP = 'CHAR_CRIT'
        TYPRES='MODE_FLAMB'
      ENDIF

C     --- RECUPERATION DES ARGUMENTS MATRICIELS
      AMOR = ' '
      CALL GETVID(' ',MATRA,1,IARG,1,RAIDE,L)
      CALL GETVID(' ',MATRB,1,IARG,1,MASSE,L)
      LAMOR=0
      IF (TYPRES.NE.'MODE_FLAMB')
     &  CALL GETVID(' ',MATRC,1,IARG,1,AMOR ,LAMOR)

C     ON NE SAIT TRAITER QUE LE CAS DE LA MATRICE DE RAIDEUR REELLE
      KTYP='R'

C     --- COMPATIBILITE DES MODES (DONNEES ALTEREES) ---
      CALL EXISD('MATR_ASSE',RAIDE,IBID)
      IF (IBID.NE.0) THEN
        CALL DISMOI('F','NOM_NUME_DDL',RAIDE,'MATR_ASSE',IBID,
     &              NUMEDD,IRET)
      ELSE
        NUMEDD=' '
      ENDIF
      CALL VPCREA( 0,MODES, MASSE, AMOR, RAIDE, NUMEDD,I)

C     TYPE_RESU : 'DYNAMIQUE' OU 'FLAMBEMENT'
      CALL GETVR8('CALC_'//TYPEVP,TYPEVP,1,IARG,0,RBID,NCRITR)

C     --- RECUPERATION DES ARGUMENTS CONCERNANT LE NOMBRE DE SHIFT ---
      CALL GETVIS('CALC_'//TYPEVP,'NMAX_ITER_SHIFT',1,IARG,
     &                                       1,NBRSS,LBRSS)

C     --- OPTION DES FREQUENCES ET DES MODES  ---
      CALL GETVTX('CALC_MODE','OPTION',1,IARG,1,OPTIOM,LMO)
      CALL GETVTX('CALC_'//TYPEVP,'OPTION',1,IARG,1,OPTIOF,LMF)

C     --- RECUPERATION DES ARGUMENTS POUR LE CALCUL DES FREQUENCES ---
      CALL GETVIS('CALC_'//TYPEVP,'NMAX_'//TYPEVP   ,1,IARG,1,MXFREQ,L)
      CALL GETVR8('CALC_'//TYPEVP,'PREC_SEPARE'     ,1,IARG,1,TOLSEP,L)
      CALL GETVIS('CALC_'//TYPEVP,'NMAX_ITER_SEPARE',1,IARG,1,NITSEP,L)
      CALL GETVR8('CALC_'//TYPEVP,'PREC_AJUSTE'     ,1,IARG,1,TOLAJU,L)
      CALL GETVIS('CALC_'//TYPEVP,'NMAX_ITER_AJUSTE',1,IARG,1,NITAJU,L)
      CALL GETVR8('CALC_'//TYPEVP,'SEUIL_'//TYPEVP  ,1,IARG,1,FCORIG,L)
      CALL GETVR8('CALC_'//TYPEVP,'PREC_SHIFT'      ,1,IARG,1,PRECSH,L)
      IF (TYPRES.EQ.'DYNAMIQUE') OMECOR = OMEGA2(FCORIG)

C     --- RECUPERATION DES ARGUMENTS POUR LE CALCUL DES MODES ---
      CALL GETVR8('CALC_MODE','PREC'     ,1,IARG,1,TOLV ,L)
      CALL GETVIS('CALC_MODE','NMAX_ITER',1,IARG,1,NITV ,L)


      IF ( OPTIOF.EQ.'SEPARE' .OR. OPTIOF.EQ.'AJUSTE' ) THEN
         CALL GETVR8('CALC_'//TYPEVP,TYPEVP,1,IARG,0,RBID,NFREQR)
         CALL GETVR8('CALC_'//TYPEVP,TYPEVP,1,IARG,0,RBID,NCRITR)

         NFREQ = -NFREQR
         NCRIT = -NCRITR
         NBMOD = MAX(NFREQ,NCRIT)
         IF ( NBMOD.LT.2 ) THEN
            VALK(1) = OPTIOF
            VALK(2) = TYPEVP
            CALL U2MESK('E','ALGELINE2_52', 2, VALK)
         ELSE
            CALL WKVECT(CBORNE,'V V R',NBMOD,LBORNE)
            IF (NFREQ .NE. 0) THEN
              CALL GETVR8('CALC_'//TYPEVP,TYPEVP,1,IARG,NFREQ,
     &                                           ZR(LBORNE),L)
            ELSE
              CALL GETVR8('CALC_'//TYPEVP,TYPEVP,1,IARG,NFREQ,
     &                    ZR(LBORNE),L)
            ENDIF
            CALL JEDETR( CBORNE )
         ENDIF
      ENDIF
      NA1=0
      IF (TYPRES.NE.'MODE_FLAMB')
     &  CALL GETVR8 ( 'CALC_'//TYPEVP,'AMOR_REDUIT', 1,IARG,0,RBID,NA1)
      NAMORR = NA1
      IF ((LAMOR.EQ.0).AND.(NAMORR.NE.0))
     &  CALL U2MESS('E','ALGELINE2_55')
      IF ((LAMOR.NE.0).AND.(NAMORR.NE.0).AND.
     &                                    (OPTIOF.NE.'PROCHE')) THEN
         CALL U2MESS('E','ALGELINE2_56')
      ENDIF
      IF (OPTIOF.EQ.'PROCHE') THEN
         CALL GETVR8('CALC_'//TYPEVP,TYPEVP, 1,IARG,0,RBID,NFREQR)
         IF ((NAMORR.NE.0).AND.(NAMORR.NE.NFREQR)) THEN
            CALL U2MESS('E','ALGELINE2_57')
         ENDIF
      ENDIF

C     ------------------------------------------------------------------

C     ---RECUPERATION DU NIVEAU D'IMPRESSION---

      CALL INFMAJ
      CALL INFNIV(IFM,NIV)

C     --- VERIFICATION DES "REFE" ---
      CALL VRREFE(MASSE,RAIDE,IRET)
      IF (IRET .GT. 0 ) THEN
          VALK(1) = RAIDE
          VALK(2) = MASSE
          CALL U2MESK('F','ALGELINE2_58', 2 ,VALK)
      ENDIF
      IF (LAMOR.NE.0) CALL VRREFE(MASSE,AMOR ,IRET)
      IF (IRET .GT. 0 ) THEN
          VALK(1) = AMOR
          VALK(2) = MASSE
          CALL U2MESK('F','ALGELINE2_58', 2 ,VALK)
      ENDIF


C     -----------------------------------------------------------------
C     ----------- LECTURE/TRAITEMENT SD SOLVEUR LINEAIRE  -----------
C     -----------------------------------------------------------------
C     -- LECTURE DES PARAMETRES SOLVEURS LINEAIRES ET CREATION DE
C        LA SD SOLVEUR ASSOCIEE. CETTE SD SOLVEUR EST LOCALE A L'OPERA
C        TEUR. POUR CE CALCUL, C'EST ELLE QUI EST UTILISEE POUR PARAME
C        TREE LE SOLVEUR LINEAIRE, ET NON PAS LA SD SOLVEUR CREE PAR LA
C        CMDE ECLATEE NUME_DDL LORS DE LA CONSTITUTION DES MATRICES.
      CALL JEVEUO(RAIDE//'.REFA','L',JREFA)
      SOLVEU='&&OP0044.SOLVEUR'
      CALL CRESOL(SOLVEU)
      CALL JEVEUO(SOLVEU//'.SLVK','L',ISLVK)
      CALL JEVEUO(SOLVEU//'.SLVI','L',ISLVI)
      METRES=ZK24(ISLVK)
      IF((METRES(1:4).NE.'LDLT').AND.(METRES(1:10).NE.'MULT_FRONT').AND.
     &   (METRES(1:5).NE.'MUMPS')) CALL U2MESS('F','ALGELINE5_71')

C     --- CREATION DE LA MATRICE DYNAMIQUE ---
      TYPER = 'R'
      DYNAM = '&&OP0044.DYNAMIQUE'
      CALL MTDEFS(DYNAM,RAIDE,'V',TYPER)
      CALL JEVEUO(DYNAM(1:19)//'.REFA','E',KREFA)
      ZK24(KREFA-1+7)=SOLVEU

C     --- CREATION DES DESCRIPTEURS NORMALISES DE MATRICE ---
      CALL MTDSCR(MASSE)
      CALL JEVEUO(MASSE(1:19)//'.&INT','E',LMASSE)
      IF (LAMOR.NE.0) THEN
         CALL MTDSCR(AMOR )
         CALL JEVEUO(AMOR(1:19)//'.&INT','E',LAMOR )
      END IF
      CALL MTDSCR(RAIDE)
      CALL JEVEUO(RAIDE(1:19)//'.&INT','E',LRAIDE)
      CALL MTDSCR(DYNAM)
      CALL JEVEUO(DYNAM(1:19)//'.&INT','E',LDYNAM)

      NEQ = ZI( LDYNAM + 2 )

C     TEST DE LA VALIDITE DES MATRICES PAR RAPPORT AU PERIMETRE DU
C     TEST DE STURM
      IF ((ZI(LMASSE+3).NE.1).OR.(ZI(LMASSE+4).NE.1)) THEN
         VALK(1)=MASSE
         CALL U2MESK('F','ALGELINE3_48', 1 ,VALK)
      ENDIF
      IF ((ZI(LRAIDE+3).NE.1).OR.(ZI(LRAIDE+4).NE.1)) THEN
         VALK(1)=RAIDE
         CALL U2MESK('F','ALGELINE3_48', 1 ,VALK)
      ENDIF
C     ------------------------------------------------------------------

C     --- OPTION DES FREQUENCES ET DES MODES  ---
      CALL GETVTX('CALC_'//TYPEVP,'OPTION',1,IARG,1,OPTIOF,LMF)
      CALL GETVTX('CALC_MODE','OPTION',1,IARG,1,OPTIOM,LMO)

      OPTIOR = 'SEPARE'
      IF ((LAMOR.NE.0).AND.(OPTIOF.EQ.'AJUSTE')) THEN
         OPTIOF = 'SEPARE'
         OPTIOR = 'AJUSTE'
      ENDIF

C     --- LISTE DE FREQUENCES REELLES ---
      NFREQR = 0
      NCRITR = 0
      IF (TYPRES.EQ.'DYNAMIQUE') THEN
         CALL GETVR8('CALC_'//TYPEVP,TYPEVP,1,IARG,0,RBID,NFREQR )
      ELSE
        CALL GETVR8('CALC_'//TYPEVP,TYPEVP,1,IARG,0,RBID,NCRITR )
      ENDIF
      NA1=0
      IF (TYPRES.NE.'MODE_FLAMB')
     &  CALL GETVR8('CALC_'//TYPEVP,'AMOR_REDUIT',1,IARG,0,RBID, NA1 )
      NAMORR = NA1
      NFREQ = - NFREQR
      NCRIT = - NCRITR
      NBMOD = MAX (NFREQ, NCRIT)

      IF ( (NFREQR .NE. 0).AND.(NAMORR.EQ.0) ) THEN
         NFREQ = -NFREQR
         CALL WKVECT(CBORNE,'V V R',NFREQ,LBORNE)
         CALL GETVR8('CALC_'//TYPEVP,TYPEVP,1,IARG,NFREQ,ZR(LBORNE),L)
C         --- CONTROLE DE FREQUENCE NEGATIVE ---
         IERFR  = 0
         DO 4 IFREQ = 0, NFREQ - 1
            IF ( ZR(LBORNE+IFREQ) .LT. 0.D0 ) IERFR = IERFR + 1
 4       CONTINUE
         IF (IERFR.GT.0) THEN
            CALL U2MESK('A','ALGELINE2_59',1,TYPEVP)
         ENDIF

      ENDIF
      IF ((TYPRES.EQ.'MODE_FLAMB').AND.(NAMORR.EQ.0)) THEN
         NCRIT = -NCRITR
         CALL WKVECT(CBORNE,'V V R',NCRIT,LBORNE)
         CALL GETVR8('CALC_'//TYPEVP,TYPEVP,1,IARG,NCRIT,
     &                                           ZR(LBORNE),L)
      ENDIF

C     --- LISTE DES AMORTISSEMENTS (CAS QUADRATIQUE) ---
      IF ((NFREQR .NE. 0).AND.(NAMORR.NE.0)) THEN
         NFREQ = -NFREQR
         CALL WKVECT(CBORNE,'V V R',2*NFREQ,LBORNE)
         CALL WKVECT(CFREQ,'V V R',NFREQ,LFREQ)
         CALL GETVR8('CALC_'//TYPEVP,TYPEVP,1,IARG,NFREQ,ZR(LFREQ),L)
         CALL WKVECT(CAMOR,'V V R',NFREQ,LAMORT)
         IF ( NA1 .NE. 0 ) THEN
          CALL GETVR8('CALC_'//TYPEVP,'AMOR_REDUIT',1,IARG,NFREQ,
     &                ZR(LAMORT),L)
         ENDIF

C         --- PASSAGE EN VALEURS PROPRES COMPLEXES ---
C         ZR(0) ZR(1)   ZR(2) ZR(3)   ZR(4), ZR(5)
C        (AM0 , FR0)   (AM1  , FR1)   (AM2,   FR2)   ETC  ETC

         DEPI = R8DEPI()
         DO 6 IFREQ = 0, NFREQ-1
            AM = ZR(LAMORT+IFREQ)
            FR = ZR(LFREQ+IFREQ)
            OMEG   = FR * DEPI
            AM = -ABS(AM*OMEG)/SQRT(1.D0-AM*AM)
            ZR(LBORNE+2*IFREQ) = AM
            ZR(LBORNE+2*IFREQ+1) = OMEG
 6       CONTINUE
      ENDIF

C     ------------------------------------------------------------------
C     ----------- DDL : LAGRANGE, BLOQUE PAR AFFE_CHAR_CINE  -----------
C     ------------------------------------------------------------------

      CALL WKVECT('&&OP0044.POSITION.DDL' ,'V V I',NEQ*MXDDL,LDDL )
      CALL WKVECT('&&OP0044.DDL.BLOQ.CINE','V V I',NEQ      ,LPROD)
      CALL VPDDL(RAIDE, MASSE, NEQ, NBLAGR, NBCINE, NEQACT, ZI(LDDL),
     &           ZI(LPROD), IERD)
      IF (IERD .NE. 0) GOTO 9999

C     ==================================================================
C
C     ----------------- CALCUL DES VALEURS PROPRES ---------------------
C
C     ==================================================================

C     ------------------------------------------------------------------
C                     --- OPTION SEPAREE OU AJUSTEE ---
C                  --- CAS GENERALISE OU QUADRATIQUE ----
C     ------------------------------------------------------------------

      IF ( OPTIOF.EQ.'SEPARE ' .OR. OPTIOF.EQ.'AJUSTE ' ) THEN

C         --- PASSAGE EN OMEGA**2 ---
         IF (NFREQ.NE.0) THEN
           DO 100 IFREQ = 0, NBMOD - 1
              ZR(LBORNE+IFREQ)  = OMEGA2(ZR(LBORNE+IFREQ))
 100       CONTINUE
         ENDIF

         OMGMAX=ZR(LBORNE+NBMOD-1)
         OMGMIN=ZR(LBORNE)
         CALL VPFOPR('STURMAD',TYPRES,LMASSE,LRAIDE,LDYNAM,OMGMIN,
     &             OMGMAX,RBID,NFREQB,NPIVOT,OMECOR,PRECSH,NBRSS,NBLAGR,
     &             SOLVEU,DET,IDET)
         DET1=DET(1)
         DET2=DET(2)
         IDET1=IDET(1)
         IDET2=IDET(2)
         IEME1=NPIVOT(1)
         IEME2=NPIVOT(2)
         ZR(LBORNE+NBMOD-1)=OMGMAX
         ZR(LBORNE)        =OMGMIN


         IF ( NFREQB .GT. 0 ) THEN

C        --- MODIFICATION EVENTUELLE DE MXFREQ

          IF (MXFREQ .EQ. 0) THEN
            MXFREQ = NFREQB
          ENDIF

C           --- CREATION DU RESUFREQ ---
          MXRESF = NFREQB
          CALL WKVECT('&&OP0044.RESU_I','V V I'  ,NBPARI*MXRESF, LRESUI)
          CALL WKVECT('&&OP0044.RESU_R','V V R'  ,NBPARR*MXRESF, LRESUR)
          CALL WKVECT('&&OP0044.RESU_K','V V K24',NBPARK*MXRESF, LRESUK)

C     --- INITIALISATION A UNDEF DE LA STRUCTURE DE DONNEES RESUF --

            DO 110 IEQ = 1, NBPARR*MXRESF
               ZR(LRESUR+IEQ-1) = UNDF
  110       CONTINUE
            DO 112 IEQ = 1, NBPARI*MXRESF
               ZI(LRESUI+IEQ-1) = INDF
  112       CONTINUE

            NDIM = 2*NFREQB + NFREQ
            CALL WKVECT(WORK(1),' V V R ',NDIM,JVALP)
            CALL WKVECT(WORK(2),' V V R ',NDIM,JDET)
            CALL WKVECT(WORK(3),' V V I ',NDIM,JIDET)
            CALL WKVECT(WORK(4),' V V I ',NDIM,JIEME)
            CALL WKVECT(WORK(5),' V V I ',NDIM,JNPAS)

            ZI(JIDET) = IDET1
            ZR(JDET ) = DET1
            ZI(JIEME) = IEME1-NBLAGR
            ZI(JIDET+NBMOD-1) = IDET2
            ZR(JDET +NBMOD-1) = DET2
            ZI(JIEME+NBMOD-1) = IEME2-NBLAGR
            IF (TYPRES .NE. 'DYNAMIQUE') THEN
              IF (ZR(LBORNE).LT.0.D0) THEN
                ZI(JIEME) = - ZI(JIEME)
              ENDIF
              IF (ZR(LBORNE+NBMOD-1).LT.0.D0) THEN
                ZI(JIEME+NBMOD-1) = - ZI(JIEME+NBMOD-1)
              ENDIF
            ENDIF

            DO 120 IFREQ= 0, NBMOD-1
               ZR(JVALP+IFREQ) = ZR(LBORNE+IFREQ)
 120        CONTINUE

C           --- CALCUL DES FREQUENCES PAR DICHOTOMIE
            CALL VPDICH(LRAIDE,LMASSE,LDYNAM,TOLSEP,NITSEP,MXFREQ,
     &        NBMOD,ZR(JVALP),ZI(JIEME),ZR(JDET),ZI(JIDET),ZI(JNPAS),
     &        TYPRES, NBLAGR,SOLVEU)

C                  --- AJUSTEMENT DES VALEURS PROPRES ---
C           --- PRISE EN COMPTE DES VALEURS PROPRES MULTIPLES ---
            CALL VPINTE(OPTIOF,NBMOD,ZR(JVALP),ZR(JDET),ZI(JIDET),
     &           ZI(JIEME),ZI(JNPAS),TOLAJU,NITAJU,LRAIDE,LMASSE,
     &           LDYNAM, ZI(LRESUI), ZR(LRESUR), MXRESF, SOLVEU)


         ELSE
            CALL U2MESS('F','ALGELINE2_62')
         ENDIF

C        --- CAS QUADRATIQUE ---

         IF (LAMOR.NE.0) THEN
            DO 130 IFREQ = 0, NBMOD - 1
              ZR(LRESUR+MXRESF+IFREQ) = SQRT(ZR(LRESUR+MXRESF+IFREQ))
              AM = 0.02D0
              OMEG = ZR(LRESUR+MXRESF+IFREQ)
              ZR(LRESUR+2*MXRESF+IFREQ)=-ABS(AM*OMEG)/SQRT(1.D0-AM*AM)
              ZR(LRESUR+3*MXRESF+IFREQ) = 0.0D0
              ZR(LRESUR+4*MXRESF+IFREQ) = 0.0D0
              ZR(LRESUR+5*MXRESF+IFREQ) = 0.0D0
              ZR(LRESUR+6*MXRESF+IFREQ) = 0.0D0
 130        CONTINUE

         ENDIF

C        --- CAS QUADRATIQUE : OPTION AJUSTE ---

         IF ((LAMOR.NE.0) .AND. OPTIOR.EQ.'AJUSTE') THEN
            CALL WKVECT('&&OP0044.VP.MULLER','V V C',3*MXRESF,LVALP)
            KFREQ = 0
            DO 140 IFREQ = 0, NBMOD-1
               OMEG = ZR(LRESUR+MXRESF+IFREQ)
               DO 142 I = 1,3
                  KFREQ =  KFREQ + 1
                  RBID  =  OMEG+ZFR(I)
                  AM    = -ABS(ZAM(I)*RBID)/SQRT(1.D0-ZAM(I)*ZAM(I))
                  ZC(LVALP+KFREQ-1) = DCMPLX(AM,OMEG)
  142          CONTINUE
  140       CONTINUE

            CALL WP1MUL(LMASSE,LAMOR,LRAIDE,ZC(LVALP),TOLAJU,NITAJU,
     &                  NBMOD,MXRESF,NBMOD,ZI(LRESUI),ZR(LRESUR))
         ENDIF

         IF (MXFREQ .NE. 0) THEN
           NBMOD = MIN ( MXFREQ , NBMOD )
         ENDIF

C     ------------------------------------------------------------------
C                          --- OPTION PROCHE ---
C                         --- CAS GENERALISE ----
C     ------------------------------------------------------------------

      ELSEIF ( LAMOR.EQ.0 .AND. OPTIOF.EQ.'PROCHE  ') THEN

         MXRESF = NBMOD
         JVALP  = LBORNE
         CALL WKVECT('&&OP0044.RESU_I','V V I'  ,NBPARI*MXRESF, LRESUI)
         CALL WKVECT('&&OP0044.RESU_R','V V R'  ,NBPARR*MXRESF, LRESUR)
         CALL WKVECT('&&OP0044.RESU_K','V V K24',NBPARK*MXRESF, LRESUK)

C     --- INITIALISATION A UNDEF DE LA STRUCTURE DE DONNEES RESUF --

         DO 200 IEQ = 1, NBPARR*MXRESF
            ZR(LRESUR+IEQ-1) = UNDF
  200    CONTINUE
         DO 202 IEQ = 1, NBPARI*MXRESF
            ZI(LRESUI+IEQ-1) = INDF
  202    CONTINUE

         CALL WKVECT('&&OP0044.POSITION','V V I',NBMOD,JIEME)

C        --- REMPLISSAGE DU RESUFREQ ET PASSAGE EN OMEGA**2 ---
         DO 210 IFREQ = 0, NBMOD-1
            ZI(JIEME+IFREQ)           = IFREQ+1
            ZI(LRESUI+IFREQ)          = 0
            ZR(LRESUR+IFREQ)          = ZR(LBORNE+IFREQ)
            ZR(LRESUR+2*MXRESF+IFREQ) = 0.0D0
 210     CONTINUE
         IF (NFREQ .NE. 0) THEN
           DO 211 IFREQ = 0, NBMOD -1
             ZR(LRESUR+MXRESF+IFREQ)   = OMEGA2(ZR(LBORNE+IFREQ))
 211       CONTINUE
         ELSE
           DO 212 IFREQ = 0, NBMOD -1
             ZR(LRESUR+MXRESF+IFREQ)   = ZR(LBORNE+IFREQ)
 212       CONTINUE
         ENDIF

C     ------------------------------------------------------------------
C                          --- OPTION PROCHE ---
C                         --- CAS QUADRATIQUE ----
C     ------------------------------------------------------------------

      ELSEIF ( LAMOR.NE.0 .AND. OPTIOF.EQ.'PROCHE  ') THEN

         MXRESF = NFREQ
         CALL WKVECT('&&OP0044.RESU_I','V V I'  ,NBPARI*MXRESF, LRESUI)
         CALL WKVECT('&&OP0044.RESU_R','V V R'  ,NBPARR*MXRESF, LRESUR)
         CALL WKVECT('&&OP0044.RESU_K','V V K24',NBPARK*MXRESF, LRESUK)

C     --- INITIALISATION A UNDEF DE LA STRUCTURE DE DONNEES RESUF --

         DO 300 IEQ = 1, NBPARR*MXRESF
            ZR(LRESUR+IEQ-1) = UNDF
 300     CONTINUE
         DO 302 IEQ = 1, NBPARI*MXRESF
            ZI(LRESUI+IEQ-1) = INDF
 302     CONTINUE

         DEPI = R8DEPI()
         K = -1
         DO 310 IFREQ = 0, NFREQ-1
            ZI(LRESUI+IFREQ)  = IFREQ+1
            K = K + 1
            ZR(LRESUR+IFREQ)  = ZR(LBORNE+K)
            IF ( NAMORR .NE. 0 ) THEN
               K = K + 1
               ZR(LRESUR+2*MXRESF+IFREQ) = ZR(LBORNE+K)
            ELSE
               OMEG = DEPI * ZR(LBORNE+K)
               AM = 0.02D0
               ZR(LRESUR+MXRESF+IFREQ)  = OMEG
               RAUX1=-ABS(AM*OMEG)
               RAUX2=SQRT(1.D0-AM*AM)
               ZR(LRESUR+2*MXRESF+IFREQ)=RAUX1/RAUX2
            ENDIF
  310    CONTINUE
         NBLAGR = 0
         NBMOD = NFREQ

      ELSE
C        --- ERREUR ---
         CALL ASSERT(.FALSE.)
      ENDIF

C     ------------------------------------------------------------------
C         --- CALCUL DES VECTEURS PROPRES PAR ITERATION INVERSE ---
C     ------------------------------------------------------------------

      IF ( LAMOR .EQ. 0 ) THEN

C        --- CAS GENERALISE

         CALL WKVECT('&&OP0044.VECTEUR.PROPRE','V V R',NEQ*NBMOD,LVEC)
         CALL VP1PRO(OPTIOM,LRAIDE,LMASSE,LDYNAM,NEQ,NBMOD,MXRESF,
     &         TOLV,NITV,ZI(LPROD),OMECOR,ZR(LVEC),
     &         ZI(LRESUI), ZR(LRESUR), ZK24(LRESUK),NBRSS,
     &         NBPARI,NBPARR,NBPARK,TYPRES,OPTIOF,SOLVEU)

      ELSE

C        --- CAS QUADRATIQUE

         CALL WKVECT('&&OP0044.VECTEUR.PROPRE','V V C',NEQ*NBMOD,LVEC)
         CALL WP1INV(LMASSE,LAMOR,LRAIDE,TOLV,NITV,MXRESF,NBMOD,NEQ,
     &            ZI(LRESUI),ZR(LRESUR),ZK24(LRESUK),ZC(LVEC),SOLVEU)
      ENDIF

C     ------------------------------------------------------------------
C     ------------------------- POSITION DES MODES ---------------------
C     ------------------------------------------------------------------

      IF ((TYPRES.EQ.'DYNAMIQUE').AND.(OPTIOF.NE.'PROCHE')
     &    .AND.(LAMOR.EQ.0)) THEN

        DO 400 IFREQ = 0, NBMOD-1
           ZI(LRESUI+IFREQ) = ZI(LRESUI+IFREQ) - NBLAGR
 400    CONTINUE


      ENDIF
      IF (ZI(LRESUI).EQ.0) ZI(LRESUI) = 1

C     ------------------------------------------------------------------
C     -------------- CALCUL DES PARAMETRES GENERALISES  ----------------
C     ----------- CALCUL DE LA NORME D'ERREUR SUR LE MODE  -------------
C     ---------------- STOCKAGE DES VECTEURS PROPRES  ------------------
C     ------------------------------------------------------------------

C     POSITION MODALE NEGATIVE DES MODES INTERDITE
      KNEGA = 'NON'

      NPARR = NBPARR
      IF (TYPCON.EQ.'MODE_ACOU') NPARR = 7

      IF (LAMOR.EQ.0) THEN
         CALL VPPARA(MODES,TYPCON,KNEGA,LRAIDE,LMASSE,LAMOR,
     &               MXRESF,NEQ,NBMOD,OMECOR,ZI(LDDL),ZI(LPROD),
     &           ZR(LVEC),CBID, NBPARI, NPARR, NBPARK, NOPARA,'    ',
     &               ZI(LRESUI), ZR(LRESUR), ZK24(LRESUK),KTYP,
     &               .FALSE.,IBID,IBID,K16BID,IBID)
      ELSE
         CALL VPPARA(MODES,TYPCON,KNEGA,LRAIDE,LMASSE,LAMOR,
     &               MXRESF,NEQ,NBMOD,OMECOR,ZI(LDDL),ZI(LPROD),
     &            RBID,ZC(LVEC), NBPARI, NPARR, NBPARK, NOPARA,'    ',
     &               ZI(LRESUI), ZR(LRESUR), ZK24(LRESUK),KTYP,
     &               .FALSE.,IBID,IBID,K16BID,IBID)
      ENDIF

C     --- IMPRESSION PROPRE A LA METHODE ----
      CALL VPWECF ( OPTIOF, TYPRES,NBMOD, MXRESF, ZI(LRESUI),
     &              ZR(LRESUR), ZK24(LRESUK), LAMOR,KTYP,LBID)

      CALL TITRE

C     ------------------------------------------------------------------
C     ----------- CONTROLE DE VALIDITE DES MODES CALCULES  -------------
C     ------------------------------------------------------------------

      CALL GETVTX('VERI_MODE','STOP_ERREUR',1,IARG,1,OPTIOV,LMF)
      IF ( OPTIOV .EQ. 'OUI' ) THEN
         CTYP = 'E'
      ELSE
         CTYP = 'A'
      ENDIF
      OPTIOV = ' '

      CALL GETVR8('VERI_MODE','SEUIL',1,IARG,1,SEUIL,LMF)
      LMAT(1) = LRAIDE
      LMAT(2) = LMASSE
      LMAT(3) = 0

      CALL VPCNTL(CTYP,MODES,OPTIOV,FMIN,FMAX,SEUIL,NBMOD,ZI(LRESUI),
     &         LMAT,OMECOR,RBID,IERX,FMIN,FMAX,
     &         ZR(LRESUR),ZR(LRESUR+3*MXRESF),ZR(LRESUR+MXRESF),TYPRES,
     &         NBLAGR,SOLVEU,NBRSS,PRECSH)

C     ------------------------------------------------------------------


C     ------------------------------------------------------------------
 9999 CONTINUE

C     --- DESTRUCTION DE LA MATRICE DYNAMIQUE
      CALL DETRSD('MATR_ASSE',DYNAM)
      CALL JEDEMA()

      END
