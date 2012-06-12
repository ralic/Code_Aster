      SUBROUTINE OP0044()
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 11/06/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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

C PARAMETRES D'APPEL

C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                  ZK32
      CHARACTER*80                                            ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------

C VARIABLES LOCALES
      INTEGER NBPARI, NBPARR, NBPARK, NBPARA, MXDDL
      PARAMETER     ( NBPARI=8 , NBPARR=16 , NBPARK=3, NBPARA=27 )
      PARAMETER     ( MXDDL = 1 )

      INTEGER INDF, ISNNEM, IFREQ, IFM, IRET, IERFR, IERD, ICOMP,
     &  IDET1, IEME1, IERX, IDET2, IEME2, IEQ, I, IBID
      INTEGER JVALP, JDET, JIDET, JIEME, JNPAS, KFREQ, K
      INTEGER LMAT(3), L, LAMOR, LTYPRE, LBRSS, LMO, LMF, LBORNE,
     &  LMASSE, LRAIDE, LDYNAM, LFREQ, LAMORT, LDDL, LPROD, NBLAGR,
     &  LRESUI, LRESUR, LRESUK, LVALP, LVEC
      INTEGER MXFREQ, NCRITR, NBRSS, NITSEP, NITAJU, NITV,
     &  NFREQR, NFREQ, NCRIT, NBMOD, NA1, NAMORR, NIV, NBCINE,KREFA,
     &  NEQACT, NFREQB, MXRESF, NDIM, NPARR, NEQ, ISLVK, ISLVI, JREFA

      REAL*8 TOLSEP, TOLAJU, TOLV, ERRF, FCORIG, OMECOR, PRECDC, OMEG,
     &  DET1, DET2, FR, AM, ZAM(3), ZFR(3), SEUIL, FMIN, FMAX, R8DEPI,
     &  OMEGA2, OMGMIN, OMGMAX, RBID, DEPI, R8VIDE, UNDF, RAUX1, RAUX2
      CHARACTER*1 CTYP, TYPER
      CHARACTER*8  OPTIOV, MODES, KNEGA, K8BID
      CHARACTER*16 NOMCMD, TYPCON, OPTIOM, OPTIOF, OPTIOR, TYPRES
      CHARACTER*19 MASSE, RAIDE, AMOR, DYNAM, NUMEDD, SOLVEU
      CHARACTER*24 CBORNE, WORK(5), CAMOR, CFREQ, NOPARA(NBPARA),METRES
      CHARACTER*24 VALK(2)
      COMPLEX*16 CBID, DCMPLX
      LOGICAL STURM,LBID
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

C     --- RECUPERATION DES ARGUMENTS MATRICIELS
      AMOR = ' '
      CALL GETVID(' ','MATR_A',1,IARG,1,RAIDE,L)
      CALL GETVID(' ','MATR_B',1,IARG,1,MASSE,L)
      CALL GETVID(' ','MATR_C',1,IARG,1,AMOR ,LAMOR)
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
      CALL GETVTX(' ','TYPE_RESU',1,IARG,1,TYPRES,LTYPRE)
      CALL GETVR8('CALC_FREQ','CHAR_CRIT  ',1,IARG,0,RBID,NCRITR)

C     --- RECUPERATION DES ARGUMENTS CONCERNANT LE NOMBRE DE SHIFT ---
      CALL GETVIS('CALC_FREQ','NMAX_ITER_SHIFT',1,IARG,1,NBRSS,LBRSS)

C     --- OPTION DES FREQUENCES ET DES MODES  ---
      CALL GETVTX('CALC_MODE','OPTION',1,IARG,1,OPTIOM,LMO)
      CALL GETVTX('CALC_FREQ','OPTION',1,IARG,1,OPTIOF,LMF)

C     --- RECUPERATION DES ARGUMENTS POUR LE CALCUL DES FREQUENCES ---
      CALL GETVIS('CALC_FREQ','NMAX_FREQ'       ,1,IARG,1,MXFREQ,L)
      CALL GETVR8('CALC_FREQ','PREC_SEPARE'     ,1,IARG,1,TOLSEP,L)
      CALL GETVIS('CALC_FREQ','NMAX_ITER_SEPARE',1,IARG,1,NITSEP,L)
      CALL GETVR8('CALC_FREQ','PREC_AJUSTE'     ,1,IARG,1,TOLAJU,L)
      CALL GETVIS('CALC_FREQ','NMAX_ITER_AJUSTE',1,IARG,1,NITAJU,L)
      CALL GETVR8('CALC_FREQ','SEUIL_FREQ'     ,1,IARG,1,FCORIG,L)
      CALL GETVR8('CALC_FREQ','PREC_SHIFT'     ,1,IARG,1,PRECDC,L)
      OMECOR = OMEGA2(FCORIG)

C     --- RECUPERATION DES ARGUMENTS POUR LE CALCUL DES MODES ---
      CALL GETVR8('CALC_MODE','PREC'     ,1,IARG,1,TOLV ,L)
      CALL GETVIS('CALC_MODE','NMAX_ITER',1,IARG,1,NITV ,L)


C     ---- CONTROLE DES ARGUMENTS ---

      IF ((TYPRES.NE.'DYNAMIQUE').AND.(NCRITR.EQ.0)) THEN
        CALL U2MESS('F','ALGELINE2_45')
      ENDIF
      IF ((TYPRES.NE.'DYNAMIQUE').AND.(LAMOR.NE.0)) THEN
        CALL U2MESS('F','ALGELINE2_46')
      ENDIF
C     --- VERIFICATION ET CORRECTION DES DONNEES DE DECOUPAGE ---
      IF ( MXFREQ .LT. 0 ) THEN
         CALL U2MESS('E','ALGELINE2_47')
      ENDIF
      IF ( NITAJU.LE.0 .OR. NITSEP.LE.0 ) THEN
         CALL U2MESS('E','ALGELINE2_48')
      ENDIF
      IF (NITV.LE.0) THEN
         CALL U2MESS('E','ALGELINE2_49')
      ENDIF
      IF (TOLAJU.LE.1.D-70 .OR. TOLSEP.LE.1.D-70  ) THEN
         CALL U2MESS('E','ALGELINE2_50')
      ENDIF
      IF (TOLV.LE.1.D-70) THEN
         CALL U2MESS('E','ALGELINE2_51')
      ENDIF

      IF ( OPTIOF.EQ.'SEPARE' .OR. OPTIOF.EQ.'AJUSTE' ) THEN
         CALL GETVR8('CALC_FREQ','FREQ  ',1,IARG,0,RBID,NFREQR)
         CALL GETVR8('CALC_FREQ','CHAR_CRIT  ',1,IARG,0,RBID,NCRITR)

         NFREQ = -NFREQR
         NCRIT = -NCRITR
         NBMOD = MAX(NFREQ,NCRIT)
         IF ( NBMOD.EQ.0 ) THEN
            CALL U2MESS('E','ALGELINE2_52')
         ELSEIF ( NBMOD.EQ.1 ) THEN
            CALL U2MESS('E','ALGELINE2_53')
         ELSEIF ( NBMOD .NE. 0 ) THEN
            CALL WKVECT(CBORNE,'V V R',NBMOD,LBORNE)
            IF (NFREQ .NE. 0) THEN
              CALL GETVR8('CALC_FREQ','FREQ',1,IARG,NFREQ,ZR(LBORNE),L)
            ELSE
              CALL GETVR8('CALC_FREQ','CHAR_CRIT',1,IARG,NFREQ,
     &                    ZR(LBORNE),L)
            ENDIF
            ERRF = -1.D0
            DO 2 IFREQ = LBORNE+1, LBORNE+NFREQ-1
               ERRF = MAX( ERRF, ZR(IFREQ-1)-ZR(IFREQ) )
 2          CONTINUE
            IF ( ERRF .GT. 0.0D0 ) THEN
               CALL U2MESS('E','ALGELINE2_54')
            ENDIF
            CALL JEDETR( CBORNE )
         ENDIF
      ENDIF

      CALL GETVR8 ( 'CALC_FREQ', 'AMOR_REDUIT', 1,IARG,0,RBID, NA1 )
      NAMORR = NA1
      IF ((LAMOR.EQ.0).AND.(NAMORR.NE.0)) THEN
         CALL U2MESS('E','ALGELINE2_55')
      ENDIF
      IF ((LAMOR.NE.0).AND.(NAMORR.NE.0).AND.
     &                                    (OPTIOF.NE.'PROCHE')) THEN
         CALL U2MESS('E','ALGELINE2_56')
      ENDIF
      IF (OPTIOF.EQ.'PROCHE') THEN
         CALL GETVR8('CALC_FREQ','FREQ  ', 1,IARG,0,RBID,NFREQR)
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
      CALL GETVTX('CALC_FREQ','OPTION',1,IARG,1,OPTIOF,LMF)
      CALL GETVTX('CALC_MODE','OPTION',1,IARG,1,OPTIOM,LMO)

      OPTIOR = 'SEPARE'
      IF ((LAMOR.NE.0).AND.(OPTIOF.EQ.'AJUSTE')) THEN
         OPTIOF = 'SEPARE'
         OPTIOR = 'AJUSTE'
      ENDIF

C     --- LISTE DE FREQUENCES REELLES ---
      CALL GETVR8('CALC_FREQ','FREQ',1,IARG,0,RBID,NFREQR )
      CALL GETVR8('CALC_FREQ','CHAR_CRIT',1,IARG,0,RBID,NCRITR )
      CALL GETVR8('CALC_FREQ','AMOR_REDUIT',1,IARG,0,RBID, NA1 )
      NAMORR = NA1
      NFREQ = - NFREQR
      NCRIT = - NCRITR
      NBMOD = MAX (NFREQ, NCRIT)

      IF ( (NFREQR .NE. 0).AND.(NAMORR.EQ.0) ) THEN
         NFREQ = -NFREQR
         CALL WKVECT(CBORNE,'V V R',NFREQ,LBORNE)
         CALL GETVR8('CALC_FREQ','FREQ',     1,IARG,NFREQ,ZR(LBORNE),L)
C         --- CONTROLE DE FREQUENCE NEGATIVE ---
         IERFR  = 0
         DO 4 IFREQ = 0, NFREQ - 1
            IF ( ZR(LBORNE+IFREQ) .LT. 0.D0 ) IERFR = IERFR + 1
 4       CONTINUE
         IF (IERFR.GT.0) THEN
            CALL U2MESS('A','ALGELINE2_59')
         ENDIF

      ENDIF
      IF ( (NCRITR .NE. 0).AND.(NAMORR.EQ.0) ) THEN
         NCRIT = -NCRITR
         CALL WKVECT(CBORNE,'V V R',NCRIT,LBORNE)
         CALL GETVR8('CALC_FREQ','CHAR_CRIT',1,IARG,NCRIT,ZR(LBORNE),L)
      ENDIF

C     --- LISTE DES AMORTISSEMENTS (CAS QUADRATIQUE) ---
      IF ((NFREQR .NE. 0).AND.(NAMORR.NE.0)) THEN
         NFREQ = -NFREQR
         CALL WKVECT(CBORNE,'V V R',2*NFREQ,LBORNE)
         CALL WKVECT(CFREQ,'V V R',NFREQ,LFREQ)
         CALL GETVR8('CALC_FREQ','FREQ',     1,IARG,NFREQ,ZR(LFREQ),L)
         CALL WKVECT(CAMOR,'V V R',NFREQ,LAMORT)
         IF ( NA1 .NE. 0 ) THEN
          CALL GETVR8('CALC_FREQ','AMOR_REDUIT',1,IARG,NFREQ,
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

C        --- CALCUL DU NOMBRE DE VALEURS PROPRES A TROUVER ---
         OMGMIN = ZR(LBORNE)
         ICOMP  = 0
 102     CONTINUE
         CALL VPSTUR(LRAIDE,OMGMIN,LMASSE,LDYNAM,DET1,IDET1,
     &               IEME1,IERX,SOLVEU)
         IF (IERX .NE. 0 ) THEN
            ICOMP = ICOMP + 1
            IF (ICOMP.GT.NBRSS) THEN
               CALL U2MESS('A','ALGELINE2_60')
            ENDIF
            OMGMIN = (1.D0-SIGN(PRECDC,OMGMIN)) * OMGMIN
            IF (NIV .GE. 1) THEN
              WRITE(IFM,1000)
              WRITE(IFM,1100) OMGMIN
              WRITE(IFM,1200)
            ENDIF
            GOTO 102
         ENDIF
         ZR(LBORNE) = OMGMIN

         OMGMAX = ZR(LBORNE+NBMOD-1)
         ICOMP  = 0
 104     CONTINUE
         CALL VPSTUR(LRAIDE,OMGMAX,LMASSE,LDYNAM,DET2,IDET2,
     &               IEME2,IERX,SOLVEU)
         IF (IERX .NE. 0 ) THEN
            ICOMP = ICOMP + 1
            IF (ICOMP.GT.NBRSS) THEN
               CALL U2MESS('A','ALGELINE2_61')
            ENDIF
            OMGMAX = (1.D0+SIGN(PRECDC,OMGMAX)) * OMGMAX
            IF (NIV .GE. 1) THEN
              WRITE(IFM,1300)
              WRITE(IFM,1400) OMGMAX
              WRITE(IFM,1200)
            ENDIF
            GOTO 104
         ENDIF
         ZR(LBORNE+NBMOD-1) = OMGMAX

C        --- AFFICHAGE DES INFORMATIONS SUR LE NOMBRE DE FREQUENCES

            CALL VPECST(IFM,TYPRES,OMGMIN,OMGMAX,IEME1,IEME2,
     &                  NFREQB,NBLAGR,'R',K8BID,0.D0,DCMPLX(0.D0,0.D0))


C
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
         CALL U2MESK('F','ALGELINE2_63',1,OPTIOF)
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
     &               ZI(LRESUI), ZR(LRESUR), ZK24(LRESUK),KTYP )
      ELSE
         CALL VPPARA(MODES,TYPCON,KNEGA,LRAIDE,LMASSE,LAMOR,
     &               MXRESF,NEQ,NBMOD,OMECOR,ZI(LDDL),ZI(LPROD),
     &            RBID,ZC(LVEC), NBPARI, NPARR, NBPARK, NOPARA,'    ',
     &               ZI(LRESUI), ZR(LRESUR), ZK24(LRESUK),KTYP )
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

C PARAMETRE POUR VERIF. DE STURM ETENDUE (INACTIVE DANS OP0044)
      STURM = .FALSE.
      CALL VPCNTL(CTYP,MODES,OPTIOV,FMIN,FMAX,SEUIL,NBMOD,ZI(LRESUI),
     &         LMAT,OMECOR,PRECDC,IERX,FMIN,FMAX,
     &         ZR(LRESUR),ZR(LRESUR+3*MXRESF),ZR(LRESUR+MXRESF),TYPRES,
     &         STURM,NBLAGR,SOLVEU)

C     ------------------------------------------------------------------


C     ------------------------------------------------------------------
 9999 CONTINUE

C     --- DESTRUCTION DE LA MATRICE DYNAMIQUE
      CALL DETRSD('MATR_ASSE',DYNAM)

 1000 FORMAT('ON DIMINUE LA BORNE MIN. EN OMEGA2  ')
 1100 FORMAT('LA BORNE MIN. EN OMEGA2 DEVIENT ',1X,1PE12.5)
 1200 FORMAT(7X)
 1300 FORMAT('ON AUGMENTE LA BORNE MAX. EN OMEGA2  ')
 1400 FORMAT('LA BORNE MAX. EN OMEGA2 DEVIENT ',1X,1PE12.5)

      CALL JEDEMA()

      END
