      SUBROUTINE OPS017 ( ICMD , ICOND , IER )
      IMPLICIT   NONE
      INTEGER             ICMD , ICOND , IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 02/10/2002   AUTEUR F1BHHAJ J.ANGLES 
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
C RESPONSABLE F1BHHAJ J.ANGLES
C TOLE  CRP_20
C                   MACR_ASPIC_CALC
C     ------------------------------------------------------------------
C     ----- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C     ----- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER      N1, NTORSC, NCFX(6), NCFY(6), NCFZ(6), NCMX(6), IORD,
     +             NCMY(6), NCMZ(6), I, NTORST, NTFX(6), NTFY(6), IBOR,
     +             NTFZ(6), NTMX(6), NTMY(6), NTMZ(6), NSOLV, NBORN,
     +             LMETHO, LRENUM, NPREC, LPRECO, LKSTOP, INFO, NBCO,
     +             NMAXIT, IERUSR,I3,I4,I8,I14,I16, L3I16(3),L2I16(2),
     +             I37, I80, NTIT, NBMATE, NRM, I8V(11), IPAS, NBRUPT,
     +             JRINF, JRSUP, NBAZIM, I5, NFPR, NFMC(6), NFMT(6),
     +             LRELCO, NCOEL, NCONV, NRGMX, NRGRE, IGLMAX,
     +             I57, NRLI, LARRET, IINMAX, IINPAS, LMATR, LRINTE,
     +             NEWT, REAINC, LPREDI, NPRED, LMATRI, REAITE, ILIMAX,
     +             NUMINI, NIN, NUMFIN, NFI, NCON,LFORMA, VERSIO,NIVGIB,
     +             J, NBFIS,NIREMP,NCOIN,NPRE,
     +             K, NMOD, NTHER, NMATE, NCARA, NCHTHE, NFOFI1, NFOFI2
      INTEGER      I8V2(9), N12, NBORDR, JORDR, NBINST, JINST, IBID
      INTEGER      NINSTI, NINSTF, NEVOL, NSP, NSPM, NCSP, SUBPA, LEVOL
      REAL*8       INSINI, INSFIN, SUBPAM, COEFSP
      REAL*8       KTRDN(6), PRES, TCFX(6),
     +             TCFY(6), TCFZ(6), TCMX(6), TCMY(6), TCMZ(6), TTFX(6),
     +             TTFY(6), TTFZ(6), TTMX(6), TTMY(6), TTMZ(6), TAILBL ,
     +             RESREL, ORIGI(3), AXEZ(3), R8B, TPREF(3), RGLMAX,
     +             RGLREL, RLIREL, RINREL
      REAL*8       MODULE,PRECIS,RBID
      LOGICAL      EFFOND, TOEFCO, TOEFTU, THERM, IMPR, FERME
      CHARACTER*2  NUME,NUME2
      CHARACTER*3  TOUCHA, TOUORD, KRCCM
      CHARACTER*4  EFFF, NOCHTH
      CHARACTER*6  TYPSOU
      CHARACTER*8  LINTT,LINTC
      CHARACTER*8  MAILLA, ENCAST, MODELE, CARAEL, KSTOP, ATORCO,
     +             NOCO1, NOCO2, NOTU, APPRES(3), APECHT, APECHC,
     +             NENCAS, AEFOCO, AEFOTU, ATORTU, AFFMAT, CONLIM,
     +             CHPRES, NEWMA, VERIPO, MRCCM,
     +             INTITI, NOPOSD, GLOCAL(2), GTHETA(2),
     +             NMAT(3), NZON(3), K8B, GRMAIL(7), MACO1, MACO2, MATU,
     +             FOND3D(2), THETA(2), K8B1, GBIL(2), GLBIL(2), K8B2,
     +             COEFHT, COEFHC, TPEXT, FMULP, FMULTC(6), FMULTT(6),
     +             LINST, CHTORC(6), CHTORT(6), MODTHE, CHTHER,
     +             RESUTH, CHMETH, GRNOLD, GRNOLI, NOPOSI
      CHARACTER*8  NOM8,NOMGRO(2,2),NOMGRE(2,2),NOMNOE(3),TABMA8(2),
     +             NOMMA(2),RTHAZD,RMOTHD,RTHAZI,RMOTHI,
     +             PMPBSD, PMPBSI, PLEVRE,
     +             NOFDFI(3),RTHFIS(2)
      CHARACTER*16 TYPRES, NOMCMD, METHOD, RENUM, PRECON, INTITD,
     +             INTI16, NOMRES, SIGM, K16B1, K16B2, K16B3, K16B4(3),
     +             PIQUAG, RELCO, ARRET, MATR, RINTE, PREDI, MATRI,
     +             FORMAT, IMPRT1(11), IMPRT2(11), IMPRT3(9), OPTION,
     +             OPTIO2, NOMCHA(3),EVOL
      CHARACTER*80 MONTIT, TITRED
C     ------------------------------------------------------------------
      DATA ORIGI / 0.0D0 , 0.0D0 , 0.0D0  /
      DATA AXEZ  / 0.0D0 , 0.0D0 , 1.0D0  /
      DATA KTRDN / 0.0D0 , 0.0D0 , 0.0D0 , 0.0D0 , 0.0D0 , 0.0D0 /
      DATA GRMAIL / 'EQUERRE' , 'PEAUINT' , 'EXCORP1' ,
     &              'EXCORP2' , 'EXTUBU' , 'LEVRTUBU' , 'LEVRCORP' /
      DATA NOMNOE / 'P1_CORP' , 'P2_CORP' , 'P_TUBU ' /
      DATA I8V    / 11*16 /
      DATA I8V2   /  9*16 /
      DATA L2I16   /  2*16 /
      DATA L3I16   /  3*16 /
      DATA IMPRT1 / 'NUME_ORDRE' , 'INTITULE' , 'RESU'   , 'NOM_CHAM' ,
     +              'ABSC_CURV' , 'COOR_X'   , 'COOR_Y' , 'COOR_Z' ,
     +              'SIXX'     , 'SIXY'   , 'SIXZ'   /

      DATA IMPRT2 / 'NUME_ORDRE' , 'INTITULE' , 'RESU'   , 'NOM_CHAM' ,
     +              'ABSC_CURV' , 'COOR_X'   , 'COOR_Y' , 'COOR_Z' ,
     +              'SIYY'     , 'SIXY'   , 'SIYZ'   /

      DATA IMPRT3 / 'NUME_ORDRE' , 'INTITULE' , 'RESU'   , 'NOM_CHAM' ,
     +              'ABSC_CURV' , 'COOR_X'   , 'COOR_Y' , 'COOR_Z' ,
     +              'TEMP'   /
C     ------------------------------------------------------------------
C     NUME_ORDRE
      CALL JEMARQ()
      IER = 0
      IF ( ICOND .NE. -1 ) GOTO 9999
C     ------------------------------------------------------------------
C
      CALL GETRES ( NOMRES, TYPRES, NOMCMD)
C
C     --- LE TYPE DU PIQUAGE ---
C
      CALL GETVTX ( ' ','TYPE_MAILLAGE' ,1,1,1, PIQUAG, N1 )
C
C     --- LE MODELE ---
C
      CALL GETVID ( ' ', 'MODELE'    , 1,1,1, MODELE, NMOD )
      IF (NMOD.EQ.0) CALL GCNCON ( '.' , MODELE )
C
C     --- LE MATERIAU ---
C
      CALL GETVID ( ' ', 'CHAM_MATER'    , 1,1,1, AFFMAT, NMATE )
      IF (NMATE.EQ.0) CALL GCNCON ( '.' , AFFMAT )
C
C     --- AFFECTATION DE CARACTERISTIQUES GEOMETRIQUE ET MATERIAU ---
C
      CALL GETVID ( ' ', 'CARA_ELEM'    , 1,1,1, CARAEL, NCARA )
      IF (NCARA.EQ.0) CALL GCNCON ( '.' , CARAEL )
C
C     --- LE NOM DU PREMIER FOND DE FISSURE ---
C
         CALL GETVID ( ' ', 'FOND_FISS_1', 1,1,1, FOND3D(1), NFOFI1 )
         IF (NFOFI1.EQ.0) CALL GCNCON ( '.' , FOND3D(1) )
C
C     --- LE NOM DU SECOND FOND DE FISSURE ---
C
         CALL GETVID ( ' ', 'FOND_FISS_2', 1,1,1, FOND3D(2), NFOFI2 )
         IF (NFOFI2.EQ.0) CALL GCNCON ( '.' , FOND3D(2) )
C
C     --- LE NOM DU CHARGEMENT MECANIQUE DU A UN CALCUL THERMIQUE ---
C
         CALL GETVID ( ' ', 'CHARGE', 1,1,1, CHMETH, NCHTHE )
         IF (NCHTHE.EQ.0) CALL GCNCON ( '.' , CHMETH )
C
C     --- LE NOM DU RESULTAT DU CALCUL THERMIQUE ---
C
         CALL GETVID ( ' ', 'RESU_THER', 1,1,1, RESUTH, NTHER )
         IF (NTHER.EQ.0) CALL GCNCON ( '.' , RESUTH )
C
C
C     --- LE TYPE DE SOUDURE (CARACTERISTIQUE DE LA TUBULURE) ---
C
      CALL GETFAC ( 'TUBULURE',N1)
      IF ((PIQUAG(1:4) .EQ. 'SAIN') .AND. (N1 .EQ. 0)) THEN
          CALL UTMESS ('F',NOMCMD,'POUR LES PIQUAGES SAINS, LE '//
     +         'MOT CLEF <TUBULURE> DOIT ETRE RENSEIGNE')
      ENDIF
      IF (N1 .NE. 0) THEN
          CALL GETVTX ( 'TUBULURE','TYPE' ,1,1,1, TYPSOU, N1 )
      ENDIF
C
C     --- LES AZIMUTS ---
C
      CALL GETVIS ( ' ','PAS_AZIMUT' ,1,1,1, IPAS, N1 )

C
C     --- LE MAILLAGE ---
C
      CALL GETVID ( ' ', 'MAILLAGE'  , 1,1,1, MAILLA, N1 )
      APPRES(1) = 'PEAUINT '
      APPRES(2) = 'LEVRTUBU'
      APPRES(3) = 'LEVRCORP'
      APECHT = 'PEAUTUBU'
      APECHC = 'PEAUCORP'
      NOCO1  = 'P1_CORP '
      NOCO2  = 'P2_CORP '
      NOTU   = 'P_TUBU  '
      MACO1  = 'EXCORP1 '
      MACO2  = 'EXCORP2 '
      MATU   = 'EXTUBU  '
      LINTT  = 'L_INT_TU'
C
C     --- LE MATERIAU ---
C
      MRCCM = '        '
      J = 0
      CALL GETFAC ( 'AFFE_MATERIAU', NBMATE )
      DO 100 I = 1 , NBMATE
         CALL GETVTX ( 'AFFE_MATERIAU', 'TOUT' , I,1,0, K8B , N1 )
         IF ( N1 .NE. 0 ) THEN
            NZON(I) = 'TOUT'
         ELSE
            CALL GETVTX ( 'AFFE_MATERIAU','GROUP_MA' ,I,1,1, NZON(I),N1)
         ENDIF
         CALL GETVID ( 'AFFE_MATERIAU', 'MATER', I,1,1, NMAT(I), N1 )
         CALL GETVR8 ( 'AFFE_MATERIAU', 'TEMP_REF', I,1,1, TPREF(I), N1)
         CALL GETVTX ( 'AFFE_MATERIAU', 'RCCM' , I,1,1, KRCCM , N1 )
         IF ( KRCCM(1:3) .EQ. 'OUI' ) THEN
            MRCCM = NMAT(I)
            J = J + 1
            IF ( J .GT. 1 ) THEN
               CALL UTMESS ('F',NOMCMD,'ATTENTION VOUS AFFECTEZ '//
     +             'PLUS D"UN MATERIAU CONTENANT L"OPTION RCCM. ')
            ENDIF
         ENDIF
 100  CONTINUE
C
C     --- L'ENCASTREMENT ---
C
      CALL GETVID ( 'EQUILIBRE', 'NOEUD', 1,1,1, ENCAST, N1 )
         IF (ENCAST.NE.'P1_CORP'.AND.ENCAST.NE.'P2_CORP') THEN
            CALL UTDEBM('F',NOMCMD,'ERREUR DONNEE')
            CALL UTIMPK('S',' MOT CLE FACTEUR ', 1, 'EQUILIBRE' )
            CALL UTIMPK('S',' MOT CLE SIMPLE ', 1, 'NOEUD' )
            CALL UTIMPK('L','  ON ATTEND ', 1, 'P1_CORP' )
            CALL UTIMPK('S',' OU ', 1, 'P2_CORP' )
            CALL UTFINM( )
         ENDIF
C
      IF ( ENCAST .EQ. 'P1_CORP' ) THEN
         NENCAS = NOCO1
         AEFOTU = MATU
         AEFOCO = MACO2
         ATORTU = NOTU
         ATORCO = NOCO2
         LINTC  = 'L_INT_C2'
      ELSEIF ( ENCAST .EQ. 'P2_CORP' ) THEN
         NENCAS = NOCO2
         AEFOTU = MATU
         AEFOCO = MACO1
         ATORTU = NOTU
         ATORCO = NOCO1
         LINTC  = 'L_INT_C1'
      ENDIF
C
C     --- PRESSION QUI S'APPLIQUE SUR LA PEAU INTERNE ---
C
      CALL GETVR8 ( 'PRES_REP', 'PRES', 1,1,1, PRES , N1 )
      CALL GETVTX ( 'PRES_REP', 'EFFE_FOND'  , 1,1,1, EFFF ,N1)
      CALL GETVTX ( 'PRES_REP', 'PRES_LEVRE' , 1,1,1, PLEVRE, NPRE)
      CALL GETVID ( 'PRES_REP', 'FONC_MULT', 1,1,1, FMULP, NFPR )
      EFFOND = EFFF.EQ.'OUI'
C
      IF (EFFOND) THEN
      CALL GETVID ( 'PRES_REP', 'NOEUD'      , 1,1,1, VERIPO, N1 )
      IF ( N1.EQ.0 ) THEN
         CALL UTDEBM('F',NOMCMD,'ERREUR DONNEE')
         CALL UTIMPK('L','  IL FAUT PRECISER UN NOEUD POUR'//
     +                   ' EFFE_FOND ', 0, VERIPO )
         CALL UTFINM( )
      ENDIF
         IF (VERIPO.NE.'P1_CORP'.AND.VERIPO.NE.'P2_CORP') THEN
            CALL UTDEBM('F',NOMCMD,'ERREUR DONNEE')
            CALL UTIMPK('S',' MOT CLE FACTEUR ', 1, 'PRES_REP' )
            CALL UTIMPK('S',' MOT CLE SIMPLE ', 1, 'NOEUD' )
            CALL UTIMPK('L','  ON ATTEND ', 1, 'P1_CORP' )
            CALL UTIMPK('S',' OU ', 1, 'P2_CORP' )
            CALL UTFINM( )
         ENDIF
C
      IF ( VERIPO .EQ. ENCAST ) THEN
         CALL UTDEBM('F',NOMCMD,'ERREUR DONNEE')
         CALL UTIMPK('L','  ON NE PEUT APPLIQUER UN "EFFE_FOND"'//
     +                   ' SUR ', 1, VERIPO )
         CALL UTIMPK('L','  CAR CE NOEUD EST BLOQUE ',0,VERIPO )
         CALL UTFINM( )
      ENDIF
      ENDIF
C
C     --- CONDITIONS D'ECHANGE THERMIQUE ---
C
      THERM = .FALSE.
      CALL GETFAC ( 'ECHANGE', N1 )
      IF ( N1 .NE. 0 ) THEN
         THERM = .TRUE.
         CALL GETVID ( 'ECHANGE', 'COEF_H_TUBU' , 1,1,1, COEFHT, N1 )
         CALL GETVID ( 'ECHANGE', 'COEF_H_CORP' , 1,1,1, COEFHC, N1 )
         CALL GETVID ( 'ECHANGE', 'TEMP_EXT'    , 1,1,1, TPEXT, N1 )
      ENDIF
C
C     --- TORSEUR D'EFFORTS SUR LE CORPS ---
C
      TOEFCO = .FALSE.
      CALL GETFAC ( 'TORS_CORP', NTORSC )
      IF ( NTORSC .NE. 0 ) THEN
         TOEFCO = .TRUE.
         CALL GETVID ( 'TORS_CORP', 'NOEUD', 1,1,1, VERIPO, N1 )
         IF (VERIPO.NE.'P1_CORP'.AND.VERIPO.NE.'P2_CORP') THEN
            CALL UTDEBM('F',NOMCMD,'ERREUR DONNEE')
            CALL UTIMPK('S',' MOT CLE FACTEUR ', 1, 'TORS_CORP' )
            CALL UTIMPK('S',' MOT CLE SIMPLE ', 1, 'NOEUD' )
            CALL UTIMPK('L','  ON ATTEND ', 1, 'P1_CORP' )
            CALL UTIMPK('S',' OU ', 1, 'P2_CORP' )
            CALL UTFINM( )
         ENDIF
C
         IF ( VERIPO .EQ. ENCAST ) THEN
            CALL UTDEBM('F',NOMCMD,'ERREUR DONNEE')
            CALL UTIMPK('L','  ON NE PEUT APPLIQUER UN TORSEUR SUR ',1,
     +                       VERIPO )
            CALL UTIMPK('L','  CAR CE NOEUD EST BLOQUE ',0,VERIPO )
            CALL UTFINM( )
         ENDIF
C
         DO 105 I=1,NTORSC
           CALL GETVR8('TORS_CORP', 'FX' , I,1,1, TCFX(I), NCFX(I) )
           CALL GETVR8('TORS_CORP', 'FY' , I,1,1, TCFY(I), NCFY(I) )
           CALL GETVR8('TORS_CORP', 'FZ' , I,1,1, TCFZ(I), NCFZ(I) )
           CALL GETVR8('TORS_CORP', 'MX' , I,1,1, TCMX(I), NCMX(I) )
           CALL GETVR8('TORS_CORP', 'MY' , I,1,1, TCMY(I), NCMY(I) )
           CALL GETVR8('TORS_CORP', 'MZ' , I,1,1, TCMZ(I), NCMZ(I) )
           CALL GETVID('TORS_CORP', 'FONC_MULT',I,1,1,FMULTC(I),NFMC(I))
 105     CONTINUE
      ENDIF
C
C     --- TORSEUR D'EFFORTS SUR LA TUBULURE ---
C
      TOEFTU = .FALSE.
      CALL GETFAC ( 'TORS_TUBU', NTORST )
      IF ( NTORST .NE. 0 ) THEN
         TOEFTU = .TRUE.
         DO 106 I=1,NTORST
           CALL GETVR8('TORS_TUBU', 'FX' , I,1,1, TTFX(I), NTFX(I) )
           CALL GETVR8('TORS_TUBU', 'FY' , I,1,1, TTFY(I), NTFY(I) )
           CALL GETVR8('TORS_TUBU', 'FZ' , I,1,1, TTFZ(I), NTFZ(I) )
           CALL GETVR8('TORS_TUBU', 'MX' , I,1,1, TTMX(I), NTMX(I) )
           CALL GETVR8('TORS_TUBU', 'MY' , I,1,1, TTMY(I), NTMY(I) )
           CALL GETVR8('TORS_TUBU', 'MZ' , I,1,1, TTMZ(I), NTMZ(I) )
           CALL GETVID('TORS_TUBU', 'FONC_MULT',I,1,1,FMULTT(I),NFMT(I))
 106     CONTINUE
      ENDIF
C
C     --- COMP_INCR ---
C
      CALL GETFAC ( 'COMP_INCR', NCOIN )
      IF ( NCOIN .NE. 0 ) THEN
         CALL GETVTX ( 'COMP_INCR', 'RELATION'    , 1,1,1, RELCO, N1 )
         CALL GETLTX ( 'COMP_INCR', 'RELATION'    , 1,1,1, LRELCO, N1 )
      ENDIF
C
C     --- COMP_ELAS ---
C
      CALL GETFAC ( 'COMP_ELAS', NCOEL )
      IF ( NCOEL .NE. 0 ) THEN
         CALL GETVTX ( 'COMP_ELAS', 'RELATION'    , 1,1,1, RELCO, N1 )
         CALL GETLTX ( 'COMP_ELAS', 'RELATION'    , 1,1,1, LRELCO, N1 )
      ENDIF
C
C     --- SOLVEUR ---
C
      CALL GETFAC ( 'SOLVEUR', NSOLV )
      IF ( NSOLV .NE. 0 ) THEN
         CALL GETVTX ( 'SOLVEUR', 'METHODE'       , 1,1,1, METHOD, N1 )
         CALL GETLTX ( 'SOLVEUR', 'METHODE'       , 1,1,1, LMETHO, N1 )
         CALL GETVTX ( 'SOLVEUR', 'RENUM'         , 1,1,1, RENUM , NRM )
         CALL GETLTX ( 'SOLVEUR', 'RENUM'         , 1,1,1, LRENUM, N1 )
         CALL GETVIS ( 'SOLVEUR', 'NPREC'         , 1,1,1, NPREC , N1 )
         CALL GETVTX ( 'SOLVEUR', 'PRE_COND'      , 1,1,1, PRECON, N1 )
         CALL GETLTX ( 'SOLVEUR', 'PRE_COND'      , 1,1,1, LPRECO, NCON)
         CALL GETVTX ( 'SOLVEUR', 'STOP_SINGULIER', 1,1,1, KSTOP , N1 )
         CALL GETLTX ( 'SOLVEUR', 'STOP_SINGULIER', 1,1,1, LKSTOP, N1 )
         CALL GETVIS ( 'SOLVEUR','NIVE_REMPLISSAGE',1,1,1, NIREMP, N1 )
         CALL GETVR8 ( 'SOLVEUR', 'RESI_RELA'     , 1,1,1, RESREL, N1 )
         CALL GETVIS ( 'SOLVEUR', 'NMAX_ITER'     , 1,1,1, NMAXIT, N1 )
      ENDIF
C
C     --- CONVERGENCE ---
C
      CALL GETFAC ( 'CONVERGENCE', NCONV )
      IF ( NCONV .NE. 0 ) THEN
       CALL GETVR8 ( 'CONVERGENCE','RESI_GLOB_MAXI', 1,1,1,RGLMAX,NRGMX)
       CALL GETVR8 ( 'CONVERGENCE','RESI_GLOB_RELA', 1,1,1,RGLREL,NRGRE)
       CALL GETVIS ( 'CONVERGENCE','ITER_GLOB_MAXI', 1,1,1,IGLMAX,N1)
       CALL GETVTX ( 'CONVERGENCE','ARRET'           , 1,1,1,ARRET,N1)
       CALL GETLTX ( 'CONVERGENCE','ARRET'           , 1,1,1,LARRET,N1)
       CALL GETVR8 ( 'CONVERGENCE','RESI_INTE_RELA', 1,1,1,RINREL,N1)
       CALL GETVIS ( 'CONVERGENCE','ITER_INTE_MAXI', 1,1,1,IINMAX,N1)
       CALL GETVIS ( 'CONVERGENCE','ITER_INTE_PAS',  1,1,1,IINPAS,N1)
       CALL GETVTX ( 'CONVERGENCE','TYPE_MATR_COMP', 1,1,1,MATR,N1)
       CALL GETLTX ( 'CONVERGENCE','TYPE_MATR_COMP', 1,1,1,LMATR,N1)
       CALL GETVTX ( 'CONVERGENCE','RESO_INTE',      1,1,1,RINTE,N1)
       CALL GETLTX ( 'CONVERGENCE','RESO_INTE',      1,1,1,LRINTE,N1)
      ENDIF
C
C     --- NEWTON ---
C
      CALL GETFAC ( 'NEWTON', NEWT )
      IF ( NEWT .NE. 0 ) THEN
        CALL GETVIS ( 'NEWTON', 'REAC_INCR', 1,1,1,REAINC,N1)
        CALL GETVTX ( 'NEWTON', 'PREDICTION', 1,1,1,PREDI,NPRED)
        CALL GETLTX ( 'NEWTON', 'PREDICTION', 1,1,1,LPREDI,N1)
        CALL GETVTX ( 'NEWTON', 'MATRICE', 1,1,1,MATRI,N1)
        CALL GETLTX ( 'NEWTON', 'MATRICE', 1,1,1,LMATRI,N1)
        CALL GETVIS ( 'NEWTON', 'REAC_ITER', 1,1,1,REAITE,N1)
      ENDIF
C
C     --- RECHERCHE LINEAIRE ---
C
      CALL GETFAC ( 'RECH_LINEAIRE', NRLI )
      IF ( NRLI .NE. 0 ) THEN
        CALL GETVR8 ( 'RECH_LINEAIRE','RESI_LINE_RELA', 1,1,1,RLIREL,N1)
        CALL GETVIS ( 'RECH_LINEAIRE','ITER_LINE_MAXI', 1,1,1,ILIMAX,N1)
      ENDIF
C
C     --- INCREMENT ---
C
      CALL GETVID ( 'INCREMENT', 'LIST_INST', 1,1,1,LINST,N1)
      CALL GETVIS ( 'INCREMENT', 'NUME_INST_INIT', 1,1,0,NUMINI,NIN)
      CALL GETVIS ( 'INCREMENT', 'NUME_INST_INIT', 1,1,1,NUMINI,N1)
      CALL GETVIS ( 'INCREMENT', 'NUME_INST_FIN' , 1,1,0,NUMFIN,NFI)
      CALL GETVIS ( 'INCREMENT', 'NUME_INST_FIN' , 1,1,1,NUMFIN,N1)
      CALL GETVIS ( 'INCREMENT', 'NUME_INST_FIN' , 1,1,1,NUMFIN,N1)
      CALL GETVR8 ( 'INCREMENT', 'INST_INIT'      , 1,1,1,INSINI,NINSTI)
      CALL GETVR8 ( 'INCREMENT', 'INST_FIN'       , 1,1,1,INSFIN,NINSTF)
      CALL GETVTX ( 'INCREMENT', 'EVOLUTION'     , 1,1,1,EVOL,NEVOL)
      CALL GETLTX ( 'INCREMENT', 'EVOLUTION'     , 1,1,1,LEVOL,N1)
      CALL GETVIS ( 'INCREMENT', 'SUBD_PAS'      , 1,1,1,SUBPA, NSP)
      CALL GETVR8 ( 'INCREMENT', 'SUBD_PAS_MINI' , 1,1,1,SUBPAM,NSPM)
      CALL GETVR8 ( 'INCREMENT','COEF_SUBD_PAS_1', 1,1,1,COEFSP,NCSP)
C
C
C     --- MECANIQUE DE LA RUPTURE ---
C
      CALL GETFAC ( 'THETA_3D', NBRUPT )
      IF ( NBRUPT .NE. 0 ) THEN
         IF ( PIQUAG(1:4) .EQ. 'SAIN' ) THEN
            CALL UTMESS('F',NOMCMD,'ON NE PEUT PAS DEFINIR LE "TYPE_'//
     +         'MAILLAGE" SAIN ET FAIRE DE LA MECANIQUE DE LA RUPTURE.')
         ENDIF
         CALL WKVECT ( '&&OPS017.R_INF', 'V V R8', NBRUPT, JRINF )
         CALL WKVECT ( '&&OPS017.R_SUP', 'V V R8', NBRUPT, JRSUP )
         DO 110 I = 1 , NBRUPT
           CALL GETVR8 ( 'THETA_3D', 'R_INF', I,1,1, ZR(JRINF+I-1), N1 )
           CALL GETVR8 ( 'THETA_3D', 'R_SUP', I,1,1, ZR(JRSUP+I-1), N1 )
 110     CONTINUE
      ENDIF
C
      CALL GETVTX ( ' ','OPTION' ,1,1,1, OPTION, N1 )
      NBORN = 0
      IF ((OPTION.EQ.'CALC_G_MAX') .OR. 
     +    (OPTION.EQ.'CALC_G_MAX_LOCAL')) THEN
            CALL GETFAC ('BORNES', NBORN )
            IF (NBORN.NE.0) THEN
              NBCO = 2*NBORN
              CALL WKVECT('&&OPS017.COUPLES_BORNES','V V R8',NBCO,IBOR)
              CALL WKVECT('&&OPS017.NUM_ORDRE','V V I',NBORN,IORD)
              DO 120 I=1, NBORN
                CALL GETVIS('BORNES','NUME_ORDRE',I,1,1,ZI(IORD+I-1),N1)
                CALL GETVR8('BORNES','VALE_MIN',I,1,1,
     +                               ZR(IBOR+2*(I-1)),N1)
                CALL GETVR8('BORNES','VALE_MAX',I,1,1,
     +                               ZR(IBOR+2*(I-1)+1),N1)
 120          CONTINUE
            ELSE
              CALL UTMESS('F',NOMCMD,'MOT-CLEF <BORNES> OBLIGATOIRE'//
     +                    ' AVEC CETTE OPTION !')
            END IF
      END IF
C
C     --- IMPRESSION ---
C
      IMPR = .FALSE.
      CALL GETFAC ( 'IMPRESSION', N1 )
      IF ( N1 .NE. 0 ) THEN
         IMPR = .TRUE.
         CALL GETVTX ( 'IMPRESSION', 'FORMAT' , 1,1,1, FORMAT, N1 )
         CALL GETLTX ( 'IMPRESSION', 'FORMAT' , 1,1,1, LFORMA, N1 )
         CALL GETVIS ( 'IMPRESSION', 'VERSION', 1,1,1, VERSIO, N1 )
         CALL GETVIS ( 'IMPRESSION', 'NIVE_GIBI', 1,1,1, NIVGIB, N1 )
         IF ( (FORMAT(1:5) .EQ. 'IDEAS') .OR. 
     +        (FORMAT(1:6) .EQ. 'CASTEM') ) THEN
           CALL GETVTX ('IMPRESSION', 'NOM_CHAM', 1,1,3, NOMCHA, N12)
           IF ( N12 .NE. 0 ) THEN
             CALL GETVTX ('IMPRESSION', 'TOUT_ORDRE', 1,1,1,TOUORD,N1)
C     --- CAS "NUME_ORDRE" ---
             CALL GETVIS('IMPRESSION','NUME_ORDRE',1,1,0,IBID,N1)
             NBORDR = -N1
             IF (NBORDR.NE.0) THEN
               CALL WKVECT('&&OPS017.IMP_NUM_ORDRE','V V I',
     +                      NBORDR,JORDR)
               CALL GETVIS('IMPRESSION','NUME_ORDRE',1,1,
     +                      NBORDR,ZI(JORDR),N1)
             END IF
C     --- CAS "INST" ---
             CALL GETVR8('IMPRESSION','INST',1,1,0,RBID,N1)
             NBINST = -N1
             IF (NBINST.NE.0) THEN
               CALL WKVECT('&&OPS017.IMPR_INST','V V R8',
     +                      NBINST,JINST)
               CALL GETVR8('IMPRESSION','INST',1,1,
     +                      NBINST,ZR(JINST),N1)
             ENDIF
           ELSE
             CALL UTMESS('E',NOMCMD,'VOUS DEMANDEZ L"IMPRESSION '//
     +            'DE RESULTATS SANS PRECISER LE NOM DES CHAMPS, '//
     +            'cf. LA DOCUMENTATION UTILISATEUR : U4.PC.20.')
           ENDIF
         ENDIF
      ENDIF
C
C     --- INFO ET TITRE ---
C
      CALL GETVIS ( ' ', 'INFO' , 1,1,1, INFO  , N1 )
      CALL GETVTX ( ' ', 'TITRE', 1,1,1, MONTIT, NTIT )
C
C     ------------------------------------------------------------------
C
C     --- DESTRUCTION DE LA COMMANDE COURANTE --
      IERUSR = 0
      CALL SMCDEL(ICMD,0,IERUSR)
      ICMD   = ICMD - 1
C
C     ------------------------------------------------------------------
C
      I3  = 3
      I4  = 4
      I5  = 5
      I8  = 8
      I14 = 14
      I16 = 16
      I37 = 37
      I57 = 57
      I80 = 80
      SIGM = 'SIEF_ELNO_ELGA'
      NOCHTH = 'TEMP'
C
C
C     --- COMMANDE AFFE_MODELE ---
C
      K16B1 = 'MECANIQUE'
      K16B2 = 'DIS_TR'
      K16B3 = '3D'
      K8B1 = 'OUI'
      ICMD = ICMD + 1
      CALL SMDCMD ( ICMD, MODELE, 'AFFE_MODELE', IERUSR )
        CALL PUTVID ( 'MAILLAGE', 1, MAILLA, IERUSR )
        CALL SMDMCF ( 'AFFE', IERUSR )
          IF ((PLEVRE .EQ. 'OUI') .AND. (PIQUAG(11:13) .EQ. 'DEB')) THEN
             CALL PUTVID ( 'GROUP_MA', 7, GRMAIL, IERUSR)
          ELSE
             CALL PUTVID ( 'GROUP_MA', 5, GRMAIL, IERUSR)
          ENDIF
          CALL PUTVTX ( 'PHENOMENE'   , 1, K16B1, I16, IERUSR)
          CALL PUTVTX ( 'MODELISATION', 1, K16B3, I16, IERUSR)
        CALL SMFMCF ( IERUSR )
        CALL SMDMCF ( 'AFFE', IERUSR )
          CALL PUTVID ( 'GROUP_MA'    , 1, NOCO1, IERUSR )
          CALL PUTVTX ( 'PHENOMENE'   , 1, K16B1, I16, IERUSR)
          CALL PUTVTX ( 'MODELISATION', 1, K16B2, I16, IERUSR)
        CALL SMFMCF ( IERUSR )
        CALL SMDMCF ( 'AFFE', IERUSR )
          CALL PUTVID ( 'GROUP_MA'    , 1, NOCO2, IERUSR )
          CALL PUTVTX ( 'PHENOMENE'   , 1, K16B1, I16, IERUSR)
          CALL PUTVTX ( 'MODELISATION', 1, K16B2, I16, IERUSR)
        CALL SMFMCF ( IERUSR )
        CALL SMDMCF ( 'AFFE', IERUSR )
          CALL PUTVID ( 'GROUP_MA'    , 1, NOTU, IERUSR )
          CALL PUTVTX ( 'PHENOMENE'   , 1, K16B1, I16, IERUSR)
          CALL PUTVTX ( 'MODELISATION', 1, K16B2, I16, IERUSR)
        CALL SMFMCF ( IERUSR )
      CALL SMFCMD ( IERUSR )
C
      IF (THERM) THEN
        K16B1 = 'THERMIQUE'
        K8B = 'OUI'
        ICMD = ICMD + 1
        CALL GCNCON ( '.' , MODTHE )
        CALL SMDCMD ( ICMD, MODTHE, 'AFFE_MODELE', IERUSR )
          CALL PUTVID ( 'MAILLAGE', 1, MAILLA, IERUSR )
          CALL SMDMCF ( 'AFFE', IERUSR )
            CALL PUTVID ( 'GROUP_MA'    , 5, GRMAIL, IERUSR )
            CALL PUTVTX ( 'PHENOMENE'   , 1, K16B1, I16, IERUSR)
            CALL PUTVTX ( 'MODELISATION', 1, K16B3, I16, IERUSR)
          CALL SMFMCF ( IERUSR )
        CALL SMFCMD ( IERUSR )
      END IF
C
C     --- COMMANDE AFFE_MATERIAU ---
C
      K8B = 'OUI'
      ICMD = ICMD + 1
      CALL SMDCMD ( ICMD, AFFMAT, 'AFFE_MATERIAU', IERUSR )
        CALL PUTVID ( 'MAILLAGE', 1, MAILLA, IERUSR )
        CALL PUTVID ( 'MODELE'  , 1, MODELE, IERUSR )
        DO 200 I = 1 , NBMATE
           CALL SMDMCF ( 'AFFE', IERUSR )
             IF ( NZON(I)(1:4) .EQ. 'TOUT' ) THEN
                CALL PUTVTX ( 'TOUT', 1, K8B, I8, IERUSR)
             ELSE
                CALL PUTVID ( 'GROUP_MA', 1, NZON(I), IERUSR )
             ENDIF
             CALL PUTVID ( 'MATER', 1, NMAT(I), IERUSR )
             CALL PUTVR8 ( 'TEMP_REF', 1, TPREF(I), IERUSR )
           CALL SMFMCF ( IERUSR )
 200    CONTINUE
      CALL SMFCMD ( IERUSR )
C
C     --- COMMANDE AFFE_CARA_ELEM ---
C
      K8B = 'K_TR_D_N'
      ICMD = ICMD + 1
      CALL SMDCMD ( ICMD, CARAEL, 'AFFE_CARA_ELEM', IERUSR )
        CALL PUTVID ( 'MODELE'  , 1, MODELE, IERUSR )
        CALL SMDMCF ( 'DISCRET', IERUSR )
          CALL PUTVID ( 'GROUP_MA', 1, NOCO1, IERUSR )
          CALL PUTVTX ( 'CARA'    , 1, K8B, I8, IERUSR)
          CALL PUTVR8 ( 'VALE'    , 6, KTRDN  , IERUSR )
        CALL SMFMCF ( IERUSR )
        CALL SMDMCF ( 'DISCRET', IERUSR )
          CALL PUTVID ( 'GROUP_MA', 1, NOCO2, IERUSR )
          CALL PUTVTX ( 'CARA'    , 1, K8B, I8, IERUSR)
          CALL PUTVR8 ( 'VALE'    , 6, KTRDN  , IERUSR )
        CALL SMFMCF ( IERUSR )
        CALL SMDMCF ( 'DISCRET', IERUSR )
          CALL PUTVID ( 'GROUP_MA', 1, NOTU, IERUSR )
          CALL PUTVTX ( 'CARA'    , 1, K8B, I8, IERUSR)
          CALL PUTVR8 ( 'VALE'    , 6, KTRDN  , IERUSR )
        CALL SMFMCF ( IERUSR )
      CALL SMFCMD ( IERUSR )
C
C     --- COMMANDE AFFE_CHAR_THER_F ---
C         CONDITION AUX LIMITES
C
      IF (THERM) THEN
C
        ICMD = ICMD + 1
        CALL GCNCON ( '.' , CHTHER )
        CALL SMDCMD ( ICMD, CHTHER, 'AFFE_CHAR_THER_F', IERUSR )
          CALL PUTVID ( 'MODELE'  , 1, MODTHE, IERUSR )
          CALL SMDMCF ( 'ECHANGE', IERUSR )
            CALL PUTVID ( 'GROUP_MA' , 1, APECHT, IERUSR)
            CALL PUTVID ( 'COEF_H'   , 1, COEFHT, IERUSR)
            CALL PUTVID ( 'TEMP_EXT' , 1, TPEXT, IERUSR )
          CALL SMFMCF ( IERUSR )
          CALL SMDMCF ( 'ECHANGE', IERUSR )
            CALL PUTVID ( 'GROUP_MA' , 1, APECHC, IERUSR)
            CALL PUTVID ( 'COEF_H'   , 1, COEFHC, IERUSR)
            CALL PUTVID ( 'TEMP_EXT' , 1, TPEXT, IERUSR )
          CALL SMFMCF ( IERUSR )
        CALL SMFCMD ( IERUSR )
C
C     --- CALCUL THERMIQUE ---
C
        ICMD = ICMD + 1
        CALL SMDCMD ( ICMD, RESUTH, 'THER_LINEAIRE', IERUSR )
          CALL PUTVID ( 'MODELE'      , 1, MODTHE, IERUSR )
          CALL PUTVID ( 'CHAM_MATER'  , 1, AFFMAT, IERUSR )
          CALL SMDMCF ( 'TEMP_INIT', IERUSR )
            K8B = 'OUI'
            CALL PUTVTX ( 'STATIONNAIRE', 1, K8B, I8, IERUSR)
          CALL SMFMCF ( IERUSR )
          CALL SMDMCF ( 'EXCIT', IERUSR )
            CALL PUTVID ( 'CHARGE'   , 1, CHTHER, IERUSR)
          CALL SMFMCF ( IERUSR )
          CALL SMDMCF ( 'INCREMENT', IERUSR )
            CALL PUTVID ( 'LIST_INST'  , 1, LINST, IERUSR )
            IF (NIN.NE.0) THEN
            CALL PUTVIS ( 'NUME_INIT'  , 1, NUMINI, IERUSR )
            END IF
            IF (NFI.NE.0) THEN
            CALL PUTVIS ( 'NUME_FIN'   , 1, NUMFIN, IERUSR )
            END IF
          CALL SMFMCF ( IERUSR )
        CALL SMFCMD ( IERUSR )
C
        ICMD = ICMD + 1
        CALL SMDCMD ( ICMD, CHMETH, 'AFFE_CHAR_MECA', IERUSR )
          CALL PUTVID ( 'MODELE'  , 1, MODELE, IERUSR )
          CALL PUTVID ( 'TEMP_CALCULEE', 1, RESUTH  , IERUSR )
        CALL SMFCMD ( IERUSR )
C
      END IF
C
C     --- COMMANDE AFFE_CHAR_MECA ---
C         CONDITION AUX LIMITES
C
      K8B = '3D_POU'
      R8B = 0.0D0
      ICMD = ICMD + 1
      CALL GCNCON ( '.' , CONLIM )
      CALL SMDCMD ( ICMD, CONLIM, 'AFFE_CHAR_MECA', IERUSR )
        CALL PUTVID ( 'MODELE'  , 1, MODELE, IERUSR )
        CALL SMDMCF ( 'LIAISON_ELEM', IERUSR )
          CALL PUTVTX ( 'OPTION'    , 1, K8B, I8, IERUSR)
          CALL PUTVID ( 'GROUP_MA_1', 1, MACO1  , IERUSR )
          CALL PUTVID ( 'GROUP_NO_2', 1, NOCO1  , IERUSR )
        CALL SMFMCF ( IERUSR )
        CALL SMDMCF ( 'LIAISON_ELEM', IERUSR )
          CALL PUTVTX ( 'OPTION'    , 1, K8B, I8, IERUSR)
          CALL PUTVID ( 'GROUP_MA_1', 1, MACO2  , IERUSR )
          CALL PUTVID ( 'GROUP_NO_2', 1, NOCO2  , IERUSR )
        CALL SMFMCF ( IERUSR )
        CALL SMDMCF ( 'LIAISON_ELEM', IERUSR )
          CALL PUTVTX ( 'OPTION'    , 1, K8B, I8, IERUSR)
          CALL PUTVID ( 'GROUP_MA_1', 1, MATU   , IERUSR )
          CALL PUTVID ( 'GROUP_NO_2', 1, NOTU   , IERUSR )
        CALL SMFMCF ( IERUSR )
        CALL SMDMCF ( 'DDL_IMPO', IERUSR )
          CALL PUTVID ( 'GROUP_NO', 1, NENCAS, IERUSR )
          CALL PUTVR8 ( 'DX' , 1, R8B, IERUSR )
          CALL PUTVR8 ( 'DY' , 1, R8B, IERUSR )
          CALL PUTVR8 ( 'DZ' , 1, R8B, IERUSR )
          CALL PUTVR8 ( 'DRX', 1, R8B, IERUSR )
          CALL PUTVR8 ( 'DRY', 1, R8B, IERUSR )
          CALL PUTVR8 ( 'DRZ', 1, R8B, IERUSR )
        CALL SMFMCF ( IERUSR )
      CALL SMFCMD ( IERUSR )
C
C     --- COMMANDE AFFE_CHAR_MECA ---
C         CHARGEMENT MECANIQUE ( PRES_REP, EFFET DE FOND)
C
      ICMD = ICMD + 1
      CALL GCNCON ( '.' , CHPRES )
      CALL SMDCMD ( ICMD, CHPRES, 'AFFE_CHAR_MECA', IERUSR )
        CALL PUTVID ( 'MODELE'  , 1, MODELE, IERUSR )
        CALL SMDMCF ( 'PRES_REP', IERUSR )
           IF ((PLEVRE .EQ. 'OUI').AND.(PIQUAG(11:13) .EQ. 'DEB')) THEN
              CALL PUTVID ( 'GROUP_MA', 3, APPRES, IERUSR )
           ELSE
              CALL PUTVID ( 'GROUP_MA', 1, APPRES(1), IERUSR )
           ENDIF
          CALL PUTVR8 ( 'PRES'    , 1, PRES  , IERUSR )
        CALL SMFMCF ( IERUSR )
        IF ( EFFOND ) THEN
          CALL SMDMCF ( 'EFFE_FOND', IERUSR )
            CALL PUTVID ( 'GROUP_MA', 1, AEFOTU, IERUSR )
            CALL PUTVID ( 'GROUP_MA_INT', 1, LINTT, IERUSR )
            CALL PUTVR8 ( 'PRES'    , 1, PRES  , IERUSR )
          CALL SMFMCF ( IERUSR )
          CALL SMDMCF ( 'EFFE_FOND', IERUSR )
            CALL PUTVID ( 'GROUP_MA', 1, AEFOCO, IERUSR )
            CALL PUTVID ( 'GROUP_MA_INT', 1, LINTC, IERUSR )
            CALL PUTVR8 ( 'PRES'    , 1, PRES  , IERUSR )
          CALL SMFMCF ( IERUSR )
        ENDIF
         CALL SMFCMD ( IERUSR )
C
C     --- COMMANDE AFFE_CHAR_MECA ---
C         CHARGEMENT MECANIQUE ( TORSEUR SUR LE CORPS)
C
        IF ( TOEFCO ) THEN
          DO 130 I=1,NTORSC
           ICMD = ICMD + 1
           CALL GCNCON ('.' , CHTORC(I) )
           CALL SMDCMD ( ICMD, CHTORC(I), 'AFFE_CHAR_MECA', IERUSR )
           CALL PUTVID ( 'MODELE'  , 1, MODELE, IERUSR )
            CALL SMDMCF ( 'FORCE_NODALE', IERUSR )
              CALL PUTVID ( 'GROUP_NO', 1, ATORCO, IERUSR )
              IF (NCFX(I) .NE. 0) CALL PUTVR8 ('FX', 1, TCFX(I), IERUSR)
              IF (NCFY(I) .NE. 0) CALL PUTVR8 ('FY', 1, TCFY(I), IERUSR)
              IF (NCFZ(I) .NE. 0) CALL PUTVR8 ('FZ', 1, TCFZ(I), IERUSR)
              IF (NCMX(I) .NE. 0) CALL PUTVR8 ('MX', 1, TCMX(I), IERUSR)
              IF (NCMY(I) .NE. 0) CALL PUTVR8 ('MY', 1, TCMY(I), IERUSR)
              IF (NCMZ(I) .NE. 0) CALL PUTVR8 ('MZ', 1, TCMZ(I), IERUSR)
            CALL SMFMCF ( IERUSR )
           CALL SMFCMD ( IERUSR )
 130     CONTINUE
        ENDIF
C
C     --- COMMANDE AFFE_CHAR_MECA ---
C         CHARGEMENT MECANIQUE ( TORSEUR SUR LA TUBULURE)
C
        IF ( TOEFTU ) THEN
         DO 140 I=1,NTORST
           ICMD = ICMD + 1
           CALL GCNCON ('.' , CHTORT(I) )
           CALL SMDCMD ( ICMD, CHTORT(I), 'AFFE_CHAR_MECA', IERUSR )
           CALL PUTVID ( 'MODELE'  , 1, MODELE, IERUSR )
            CALL SMDMCF ( 'FORCE_NODALE', IERUSR )
              CALL PUTVID ( 'GROUP_NO', 1, ATORTU, IERUSR )
              IF (NTFX(I) .NE. 0) CALL PUTVR8 ('FX', 1, TTFX(I), IERUSR)
              IF (NTFY(I) .NE. 0) CALL PUTVR8 ('FY', 1, TTFY(I), IERUSR)
              IF (NTFZ(I) .NE. 0) CALL PUTVR8 ('FZ', 1, TTFZ(I), IERUSR)
              IF (NTMX(I) .NE. 0) CALL PUTVR8 ('MX', 1, TTMX(I), IERUSR)
              IF (NTMY(I) .NE. 0) CALL PUTVR8 ('MY', 1, TTMY(I), IERUSR)
              IF (NTMZ(I) .NE. 0) CALL PUTVR8 ('MZ', 1, TTMZ(I), IERUSR)
            CALL SMFMCF ( IERUSR )
           CALL SMFCMD ( IERUSR )
 140     CONTINUE
        ENDIF
C
C     --- COMMANDE STAT_NON_LINE ---
C
      K8B2 = 'ELAS'
      ICMD = ICMD + 1
      CALL SMDCMD ( ICMD, NOMRES, 'STAT_NON_LINE', IERUSR )
        IF ( NTIT .NE. 0 ) THEN
           CALL PUTVTX ( 'TITRE'  , 1, MONTIT, I80, IERUSR)
        ENDIF
        CALL PUTVIS ( 'INFO'      , 1, INFO  , IERUSR )
        CALL PUTVID ( 'MODELE'    , 1, MODELE, IERUSR )
        CALL PUTVID ( 'CHAM_MATER', 1, AFFMAT, IERUSR )
        CALL PUTVID ( 'CARA_ELEM' , 1, CARAEL, IERUSR )
        CALL SMDMCF ( 'EXCIT', IERUSR )
          CALL PUTVID ( 'CHARGE', 1, CONLIM, IERUSR )
        CALL SMFMCF ( IERUSR )
        IF (THERM) THEN
          CALL SMDMCF ( 'EXCIT', IERUSR )
            CALL PUTVID ( 'CHARGE', 1, CHMETH, IERUSR )
          CALL SMFMCF ( IERUSR )
        END IF
        CALL SMDMCF ( 'EXCIT', IERUSR )
          CALL PUTVID ( 'CHARGE', 1, CHPRES, IERUSR )
          IF (NFPR.NE.0) CALL PUTVID ( 'FONC_MULT', 1, FMULP, IERUSR)
        CALL SMFMCF ( IERUSR )
C
        IF ( TOEFCO ) THEN
          DO 131 I= 1, NTORSC
             CALL SMDMCF ( 'EXCIT', IERUSR )
               CALL PUTVID ( 'CHARGE', 1, CHTORC(I), IERUSR )
               IF (NFMC(I).NE.0)
     +                CALL PUTVID ( 'FONC_MULT', 1, FMULTC(I), IERUSR)
             CALL SMFMCF ( IERUSR )
 131      CONTINUE
        END IF
C
        IF ( TOEFTU ) THEN
          DO 141 I= 1, NTORST
             CALL SMDMCF ( 'EXCIT', IERUSR )
               CALL PUTVID ( 'CHARGE', 1, CHTORT(I), IERUSR )
               IF (NFMT(I).NE.0)
     +               CALL PUTVID ( 'FONC_MULT', 1, FMULTT(I), IERUSR)
             CALL SMFMCF ( IERUSR )
 141      CONTINUE
        END IF
C
        IF (NCOIN.NE.0) THEN
         CALL SMDMCF ( 'COMP_INCR', IERUSR )
           CALL PUTVTX ( 'TOUT'    , 1, 'OUI',      3, IERUSR )
           CALL PUTVTX ( 'RELATION', 1, RELCO, LRELCO, IERUSR )
         CALL SMFMCF ( IERUSR )
        END IF
C
        IF (NCOEL.NE.0) THEN
         CALL SMDMCF ( 'COMP_ELAS', IERUSR )
           CALL PUTVTX ( 'RELATION', 1, RELCO, LRELCO, IERUSR )
           CALL PUTVID ( 'GROUP_MA', 5, GRMAIL, IERUSR )
         CALL SMFMCF ( IERUSR )
        END IF
C
C POUR LES NOEUDS DISCRETS, COMP_INCR EST OBLIGATOIRE
         CALL SMDMCF ( 'COMP_INCR', IERUSR )
           CALL PUTVTX ( 'RELATION', 1, K8B2, I8, IERUSR )
           CALL PUTVID ( 'GROUP_MA', 3, NOMNOE, IERUSR )
         CALL SMFMCF ( IERUSR )
C
        IF ( NSOLV .NE. 0 ) THEN
          CALL SMDMCF ( 'SOLVEUR', IERUSR )
            CALL PUTVTX ( 'METHODE'       , 1, METHOD, LMETHO, IERUSR)
            IF  (METHOD(1:10).EQ.'MULT_FRONT') THEN
              IF ( NRM .NE. 0 )
     +        CALL PUTVTX ( 'RENUM'         , 1, RENUM , LRENUM, IERUSR)
            ELSEIF  (METHOD(1:4).EQ.'LDLT') THEN
              IF ( NRM .NE. 0 )
     +        CALL PUTVTX ( 'RENUM'         , 1, RENUM , LRENUM, IERUSR)
            ELSEIF (METHOD(1:4).EQ.'GCPC') THEN
              IF ( NCON . NE. 0 )
     +        CALL PUTVTX ( 'PRE_COND'      , 1, PRECON, LPRECO, IERUSR)
              CALL PUTVIS ('NIVE_REMPLISSAGE', 1, NIREMP,IERUSR)
              IF ( NRM .NE. 0 )
     +        CALL PUTVTX ( 'RENUM'         , 1, RENUM , LRENUM, IERUSR)
              CALL PUTVR8 ( 'RESI_RELA'     , 1, RESREL, IERUSR )
              CALL PUTVIS ( 'NMAX_ITER'     , 1, NMAXIT, IERUSR )
            ENDIF
            IF ((METHOD(1:4).EQ.'LDLT').OR.
     +          (METHOD(1:10).EQ.'MULT_FRONT')) THEN
              CALL PUTVIS ( 'NPREC'         , 1, NPREC , IERUSR )
              CALL PUTVTX ( 'STOP_SINGULIER', 1, KSTOP , LKSTOP, IERUSR)
            ENDIF
          CALL SMFMCF ( IERUSR )
        ENDIF
C
       IF ( NCONV .NE. 0 ) THEN
          CALL SMDMCF ( 'CONVERGENCE', IERUSR )
           IF (NRGMX.NE.0)
     +      CALL PUTVR8 ( 'RESI_GLOB_MAXI', 1,RGLMAX,IERUSR)
           IF (NRGRE.NE.0)
     +      CALL PUTVR8 ( 'RESI_GLOB_RELA', 1,RGLREL,IERUSR)
           CALL PUTVIS ( 'ITER_GLOB_MAXI', 1,IGLMAX,IERUSR)
           CALL PUTVTX ( 'ARRET'         , 1,ARRET,LARRET,IERUSR)
           CALL PUTVR8 ( 'RESI_INTE_RELA', 1,RINREL,IERUSR)
           CALL PUTVIS ( 'ITER_INTE_MAXI', 1,IINMAX,IERUSR)
           CALL PUTVIS ( 'ITER_INTE_PAS' , 1,IINPAS,IERUSR)
           CALL PUTVTX ( 'TYPE_MATR_COMP', 1,MATR,LMATR,IERUSR)
           CALL PUTVTX ( 'RESO_INTE'     , 1,RINTE,LRINTE,IERUSR)
          CALL SMFMCF ( IERUSR )
        ENDIF
C
      IF ( NEWT .NE. 0 ) THEN
        CALL SMDMCF ( 'NEWTON', IERUSR )
        CALL PUTVIS ( 'REAC_INCR',  1,REAINC,IERUSR)
        IF (NPRED.NE.0)
     +   CALL PUTVTX ( 'PREDICTION', 1,PREDI,LPREDI,IERUSR)
        CALL PUTVTX ( 'MATRICE'       , 1,MATRI,LMATRI,IERUSR)
        CALL PUTVIS ( 'REAC_ITER'     , 1,REAITE,IERUSR)
        CALL SMFMCF ( IERUSR )
      ENDIF
C
      IF ( NRLI .NE. 0 ) THEN
        CALL SMDMCF ( 'RECH_LINEAIRE', IERUSR )
        CALL PUTVR8 ( 'RESI_LINE_RELA', 1,RLIREL,IERUSR)
        CALL PUTVIS ( 'ITER_LINE_MAXI', 1,ILIMAX,IERUSR)
        CALL SMFMCF ( IERUSR )
      ENDIF
C
      CALL SMDMCF ( 'INCREMENT', IERUSR )
      CALL PUTVID ( 'LIST_INST', 1,LINST,IERUSR)
      IF (NIN.NE.0)
     + CALL PUTVIS ( 'NUME_INST_INIT', 1,NUMINI,IERUSR)
      IF (NINSTI.NE.0) THEN
        CALL PUTVR8 ( 'INST_INIT'  , 1, INSINI, IERUSR )
      END IF
      IF (NFI.NE.0)
     + CALL PUTVIS ( 'NUME_INST_FIN' , 1,NUMFIN,IERUSR)
      IF (NINSTF.NE.0) THEN
        CALL PUTVR8 ( 'INST_FIN'  , 1, INSFIN, IERUSR )
      END IF
      IF (NEVOL.NE.0) THEN
        CALL PUTVTX ( 'EVOLUTION'  , 1, EVOL, LEVOL, IERUSR )
      END IF
      IF (NSP.NE.0) THEN
        CALL PUTVIS ( 'SUBD_PAS'  , 1, SUBPA, IERUSR )
      ENDIF
      IF (NSPM.NE.0) THEN
        CALL PUTVR8 ( 'SUBD_PAS_MINI'  , 1, SUBPAM, IERUSR )
      ENDIF
      IF (NCSP.NE.0) THEN
        CALL PUTVR8 ( 'COEF_SUBD_PAS_1', 1, COEFSP, IERUSR )
      ENDIF
      CALL SMFMCF ( IERUSR )
C
      CALL SMFCMD ( IERUSR )
C
C     --- COMMANDE CALC_ELEM ---
C
      K8B = 'OUI'
      K16B4(1) = 'SIEF_ELNO_ELGA'
      K16B4(2) = 'VARI_ELNO_ELGA'
      K16B4(3) = 'EQUI_ELNO_SIGM'
      ICMD = ICMD + 1
      CALL SMDCMD ( ICMD, '&'//NOMRES, 'CALC_ELEM', IERUSR )
        CALL PUTVID ( 'RESULTAT', 1, NOMRES, IERUSR )
        CALL PUTVID ( 'MODELE', 1, MODELE, IERUSR )
        CALL PUTVID ( 'CHAM_MATER', 1, AFFMAT, IERUSR )
        CALL PUTVID ( 'CARA_ELEM', 1, CARAEL, IERUSR )
        IF (THERM) THEN
          CALL SMDMCF ( 'EXCIT', IERUSR )
            CALL PUTVID ( 'CHARGE', 1, CHMETH, IERUSR )
          CALL SMFMCF ( IERUSR )
        END IF
        CALL PUTVTX ( 'TOUT_ORDRE', 1, K8B, I8, IERUSR)
        CALL PUTVTX ( 'OPTION', 3, K16B4, L3I16, IERUSR )
      CALL SMFCMD ( IERUSR )
C
C-----------------------------------------------------------------------
      IF ( PIQUAG(1:4) .EQ. 'SAIN' ) THEN
C-----------------------------------------------------------------------
C
C     --- POST TRAITEMENT :  POST_RELEVE_T  --- AZIMUTS DROITS
C
      NBAZIM = 48
      IF ( PIQUAG .EQ. 'SAIN_GROS' ) NBAZIM = 40
      PRECIS= 55.D-1
      K8B   = 'OUI'
      K8B1  = 'LOCAL'
      K16B1 = 'EXTRACTION'
      K16B2 = 'CYLINDRIQUE'
      K16B3 = 'MOYENNE'
C
      TITRED = '-- TRAITEMENT DES AZIMUTS DROITS --'
C
C     ----  CHAMPS DE CONTRAINTE SI, SII ET SIII  ----
C
      ICMD = ICMD + 1
      CALL GCNCON ( '.' , NOPOSD )
      CALL SMDCMD ( ICMD, NOPOSD, 'POST_RELEVE_T', IERUSR )
         DO 220 I = 1 , NBAZIM , IPAS
           CALL CODENT ( I , 'G' , NUME )
           IF (I.LE.9) THEN
              NUME2 =  '0' // NUME(1:1)
           ELSE
              NUME2 =  NUME
           ENDIF
           GRNOLD = 'LD'//NUME
C           INTITD = 'AZI_'//NUME2//'_D'
           CALL SMDMCF ( 'ACTION', IERUSR )
             CALL PUTVID ( 'GROUP_NO'  , 1, GRNOLD, IERUSR )
             CALL PUTVID ( 'RESULTAT'  , 1, NOMRES, IERUSR )
C             CALL PUTVIS ( 'NUME_ORDRE', 1, I1    , IERUSR )
             CALL PUTVTX ( 'TOUT_ORDRE' , 1, K8B , I8 , IERUSR)
             CALL PUTVTX ( 'NOM_CHAM'  , 1, SIGM  , I14, IERUSR)
             CALL PUTVR8 ( 'PRECISION' , 1, PRECIS, IERUSR )
             CALL PUTVTX ( 'TOUT_CMP'  , 1, K8B   , I8 , IERUSR)
             CALL PUTVTX ( 'OPERATION' , 1, K16B1 , I16, IERUSR)
             IF (TYPSOU .EQ. 'TYPE_1') THEN
                 CALL PUTVTX ( 'REPERE' , 1, K16B2 , I16, IERUSR)
                 CALL PUTVR8 ( 'ORIGINE'   , 3, ORIGI , IERUSR )
                 CALL PUTVR8 ( 'AXE_Z'     , 3, AXEZ  , IERUSR )
                 INTITD = 'AZI_'//NUME2//'_D-REP_CYL'
             ELSE
                 CALL PUTVTX ( 'REPERE' , 1, K8B1  , I5, IERUSR)
                 CALL PUTVR8 ( 'VECT_Y'    , 3, AXEZ  , IERUSR )
                 INTITD = 'AZI_'//NUME2//'_D-REP_LOC'
             ENDIF
             CALL PUTVTX ( 'INTITULE'  , 1, INTITD, I16 , IERUSR)
           CALL SMFMCF ( IERUSR )
C
 220     CONTINUE
         CALL PUTVTX ( 'TITRE'     , 1, TITRED, I37, IERUSR)
      CALL SMFCMD ( IERUSR )
C
C     --- IMPR_TABLE DANS UN REPERE CYLINDRIQUE OU LOCAL
C         DES CHAMPS DE CONTRAINTE SI, SII ET SIII
C
      ICMD = ICMD + 1
      CALL SMDCMD ( ICMD, ' ', 'IMPR_TABLE', IERUSR )
        CALL PUTVID ( 'TABLE', 1, NOPOSD, IERUSR )
        IF (TYPSOU .EQ. 'TYPE_1') THEN
           CALL PUTVTX ( 'NOM_PARA', 11, IMPRT1, I8V, IERUSR )
        ELSE
           CALL PUTVTX ( 'NOM_PARA', 11, IMPRT2, I8V, IERUSR )
        ENDIF
      CALL SMFCMD ( IERUSR )
C
C     ----  Pm, Pm+Pb SUR LES LIGNES DE DEPOUILLEMENT  ----
C
      IF ( KRCCM(1:3) .EQ. 'OUI' ) THEN 
         ICMD = ICMD + 1
         CALL GCNCON ( '.' , PMPBSD )
         CALL SMDCMD ( ICMD, PMPBSD, 'POST_RCCM', IERUSR )
           CALL PUTVID ( 'MATER', 1, MRCCM, IERUSR )
           CALL PUTVID ( 'MAILLAGE', 1, MAILLA, IERUSR )
           CALL PUTVTX ( 'TYPE_RESU', 1, 'VALE_MAX', I8, IERUSR)
           CALL PUTVTX ( 'OPTION', 1, 'PM_PB', I5, IERUSR)
           DO 230 I = 1 , NBAZIM , IPAS
             CALL CODENT ( I , 'G' , NUME )
             IF (I.LE.9) THEN
                NUME2 =  '0' // NUME(1:1)
             ELSE
                NUME2 =  NUME
             ENDIF
             GRNOLD = 'LD'//NUME
             INTITD = 'AZI_'//NUME2//'_D'
             CALL SMDMCF ( 'SEGMENT', IERUSR )
               CALL PUTVID ( 'GROUP_NO'  , 1, GRNOLD, IERUSR )
C               CALL PUTVTX ( 'INTITULE'  , 1, INTITD, I8, IERUSR)
               CALL PUTVR8 ( 'PRECISION' , 1, PRECIS, IERUSR )
             CALL SMFMCF ( IERUSR )
C
 230       CONTINUE
           CALL SMDMCF ( 'TRANSITOIRE', IERUSR )
             CALL PUTVID ( 'RESULTAT', 1, NOMRES, IERUSR )
             CALL PUTVTX ( 'NOM_CHAM', 1, SIGM, I16, IERUSR )
             CALL PUTVTX ( 'TOUT_ORDRE', 1, K8B, I8, IERUSR )
C             CALL PUTVTX ( 'INTITULE', 1, 'TOTO1', I5, IERUSR )
           CALL SMFMCF ( IERUSR )
           CALL PUTVTX ( 'TITRE'     , 1, TITRED, I37, IERUSR)
         CALL SMFCMD ( IERUSR )
      ENDIF
C
C     ----  IMPR_TABLE DE Pm, Pm+Pb
C                                SUR LES LIGNES DE DEPOUILLEMENT  ----
C
      IF ( KRCCM(1:3) .EQ. 'OUI' ) THEN 
         ICMD = ICMD + 1
         CALL SMDCMD ( ICMD, ' ', 'IMPR_TABLE', IERUSR )
           CALL PUTVID ( 'TABLE', 1, PMPBSD, IERUSR )
         CALL SMFCMD ( IERUSR )
      ENDIF
C
C     ----  CHAMPS DE TEMPERATURE (SI IL A ETE CALCULE)
C                       SUR LES LIGNES DE DEPOUILLEMENT  ----
C
      IF (THERM) THEN
         ICMD = ICMD + 1
         CALL GCNCON ( '.' , RTHAZD )
         CALL SMDCMD ( ICMD, RTHAZD, 'POST_RELEVE_T', IERUSR )
            DO 240 I = 1 , NBAZIM , IPAS
              CALL CODENT ( I , 'G' , NUME )
              IF (I.LE.9) THEN
                 NUME2 =  '0' // NUME(1:1)
              ELSE
                 NUME2 =  NUME
              ENDIF
              GRNOLD = 'LD'//NUME
              INTITD = 'AZI_'//NUME2//'_D'
              CALL SMDMCF ( 'ACTION', IERUSR )
                CALL PUTVID ( 'GROUP_NO'  , 1, GRNOLD, IERUSR )
                CALL PUTVID ( 'RESULTAT'  , 1, RESUTH, IERUSR )
                CALL PUTVTX ( 'TOUT_ORDRE' , 1, K8B , I8 , IERUSR)
                CALL PUTVTX ( 'NOM_CHAM'  , 1, NOCHTH  , I4, IERUSR)
                CALL PUTVR8 ( 'PRECISION' , 1, PRECIS, IERUSR )
                CALL PUTVTX ( 'TOUT_CMP'  , 1, K8B   , I8 , IERUSR)
                CALL PUTVTX ( 'INTITULE'  , 1, INTITD, I8 , IERUSR)
                CALL PUTVTX ( 'OPERATION' , 1, K16B1 , I16, IERUSR)
              CALL SMFMCF ( IERUSR )
C
 240        CONTINUE
         CALL SMFCMD ( IERUSR )
C
C     ----  IMPR_TABLE DU CHAMP DE TEMPERATURE
C                         SUR LES LIGNES DE DEPOUILLEMENT  ----
C
         ICMD = ICMD + 1
         CALL SMDCMD ( ICMD, ' ', 'IMPR_TABLE', IERUSR )
           CALL PUTVID ( 'TABLE', 1, RTHAZD, IERUSR )
C           CALL PUTVTX ( 'NOM_PARA', 9, IMPRT3, I8V2, IERUSR )
         CALL SMFCMD ( IERUSR )
C
C     ----  PARAMETRES CARACTERISANT LA DISTRIBUTION DE TEMPERATURE
C           (SI ELLE A ETE CALCULEE) DANS L'EPAISSEUR DU LIGAMENT  ----
C
         ICMD = ICMD + 1
         CALL GCNCON ( '.' , RMOTHD )
         CALL SMDCMD ( ICMD, RMOTHD, 'POST_RELEVE_T', IERUSR )
            DO 250 I = 1 , NBAZIM , IPAS
              CALL CODENT ( I , 'G' , NUME )
              IF (I.LE.9) THEN
                 NUME2 =  '0' // NUME(1:1)
              ELSE
                 NUME2 =  NUME
              ENDIF
              GRNOLD = 'LD'//NUME
              INTITD = 'AZI_'//NUME2//'_D'
              CALL SMDMCF ( 'ACTION', IERUSR )
                CALL PUTVID ( 'GROUP_NO'  , 1, GRNOLD, IERUSR )
                CALL PUTVID ( 'RESULTAT'  , 1, RESUTH, IERUSR )
                CALL PUTVTX ( 'TOUT_ORDRE' , 1, K8B , I8 , IERUSR)
                CALL PUTVTX ( 'NOM_CHAM'  , 1, NOCHTH  , I4, IERUSR)
                CALL PUTVR8 ( 'PRECISION' , 1, PRECIS, IERUSR )
                CALL PUTVTX ( 'TOUT_CMP'  , 1, K8B   , I8 , IERUSR)
                CALL PUTVTX ( 'INTITULE'  , 1, INTITD, I8 , IERUSR)
                CALL PUTVTX ( 'OPERATION' , 1, K16B3 , I16, IERUSR)
              CALL SMFMCF ( IERUSR )
C
 250        CONTINUE
         CALL SMFCMD ( IERUSR )
C
C     ----  IMPR_TABLE DU CHAMP DE TEMPERATURE
C                         SUR LES LIGNES DE DEPOUILLEMENT  ----
C
         ICMD = ICMD + 1
         CALL SMDCMD ( ICMD, ' ', 'IMPR_TABLE', IERUSR )
           CALL PUTVID ( 'TABLE', 1, RMOTHD, IERUSR )
C           CALL PUTVTX ( 'NOM_PARA', 9, IMPRT3, I8V2, IERUSR )
         CALL SMFCMD ( IERUSR )
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C     --- POST TRAITEMENT :  POST_RELEVE_T  --- AZIMUTS INCLINES
C
      TITRED = '-- TRAITEMENT DES AZIMUTS INCLINES --'
C
C     ----  CHAMPS DE CONTRAINTE SI, SII ET SIII  ----
C
      ICMD = ICMD + 1
      CALL GCNCON ( '.' , NOPOSI )
      CALL SMDCMD ( ICMD, NOPOSI, 'POST_RELEVE_T', IERUSR )
         DO 260 I = 1 , NBAZIM , IPAS
           CALL CODENT ( I , 'G' , NUME )
           IF (I.LE.9) THEN
              NUME2 =  '0' // NUME(1:1)
           ELSE
              NUME2 =  NUME
           ENDIF
           GRNOLI = 'LI'//NUME
           INTI16 = 'AZI_'//NUME2//'_I-REP_LOC'
           CALL SMDMCF ( 'ACTION', IERUSR )
             CALL PUTVID ( 'GROUP_NO'  , 1, GRNOLI, IERUSR )
             CALL PUTVID ( 'RESULTAT'  , 1, NOMRES, IERUSR )
             CALL PUTVTX ( 'TOUT_ORDRE' , 1, K8B , I8 , IERUSR)
             CALL PUTVTX ( 'NOM_CHAM'  , 1, SIGM  , I14, IERUSR)
             CALL PUTVR8 ( 'PRECISION' , 1, PRECIS, IERUSR )
             CALL PUTVTX ( 'TOUT_CMP'  , 1, K8B   , I8 , IERUSR)
             CALL PUTVTX ( 'REPERE'    , 1, K8B1  , I5, IERUSR)
             CALL PUTVR8 ( 'VECT_Y'    , 3, AXEZ  , IERUSR )
             CALL PUTVTX ( 'INTITULE'  , 1, INTI16 , I16 , IERUSR)
             CALL PUTVTX ( 'OPERATION' , 1, K16B1, I16, IERUSR)
           CALL SMFMCF ( IERUSR )
 260     CONTINUE
         CALL PUTVTX ( 'TITRE'     , 1, TITRED, I37, IERUSR)
      CALL SMFCMD ( IERUSR )
C
C     --- IMPR_TABLE DANS UN REPERE LOCAL DES CHAMPS
C               DE CONTRAINTE SI, SII ET SIII
C
      ICMD = ICMD + 1
      CALL SMDCMD ( ICMD, ' ', 'IMPR_TABLE', IERUSR )
        CALL PUTVID ( 'TABLE', 1, NOPOSI, IERUSR )
        CALL PUTVTX ( 'NOM_PARA', 11, IMPRT2, I8V, IERUSR )
      CALL SMFCMD ( IERUSR )
C
C     ----  Pm, Pm+Pb SUR LES LIGNES DE DEPOUILLEMENT  ----
C
      IF ( KRCCM(1:3) .EQ. 'OUI' ) THEN 
         ICMD = ICMD + 1
         CALL GCNCON ( '.' , PMPBSI )
         CALL SMDCMD ( ICMD, PMPBSI, 'POST_RCCM', IERUSR )
           CALL PUTVID ( 'MATER', 1, MRCCM, IERUSR )
           CALL PUTVID ( 'MAILLAGE', 1, MAILLA, IERUSR )
           CALL PUTVTX ( 'TYPE_RESU', 1, 'VALE_MAX', I8, IERUSR)
           CALL PUTVTX ( 'OPTION', 1, 'PM_PB', I5, IERUSR)
           DO 270 I = 1 , NBAZIM , IPAS
             CALL CODENT ( I , 'G' , NUME )
             IF (I.LE.9) THEN
                NUME2 =  '0' // NUME(1:1)
             ELSE
                NUME2 =  NUME
             ENDIF
             GRNOLI = 'LI'//NUME
             INTITI = 'AZI_'//NUME2//'_I'
             CALL SMDMCF ( 'SEGMENT', IERUSR )
               CALL PUTVID ( 'GROUP_NO', 1, GRNOLI, IERUSR )
C               CALL PUTVTX ( 'INTITULE', 1, INTITI, I8, IERUSR )
               CALL PUTVR8 ( 'PRECISION', 1, PRECIS, IERUSR )
             CALL SMFMCF ( IERUSR )
C
 270       CONTINUE
           CALL SMDMCF ( 'TRANSITOIRE', IERUSR )
             CALL PUTVID ( 'RESULTAT', 1, NOMRES, IERUSR )
             CALL PUTVTX ( 'NOM_CHAM', 1, SIGM, I16, IERUSR )
             CALL PUTVTX ( 'TOUT_ORDRE', 1, K8B, I8, IERUSR )
C             CALL PUTVTX ( 'INTITULE', 1, 'TOTO2', I5, IERUSR )
           CALL SMFMCF ( IERUSR )
           CALL PUTVTX ( 'TITRE'     , 1, TITRED, I37, IERUSR)
         CALL SMFCMD ( IERUSR )
      ENDIF
C
C     ----  IMPR_TABLE DE Pm, Pm+Pb
C                                SUR LES LIGNES DE DEPOUILLEMENT  ----
C
      IF ( KRCCM(1:3) .EQ. 'OUI' ) THEN 
         ICMD = ICMD + 1
         CALL SMDCMD ( ICMD, ' ', 'IMPR_TABLE', IERUSR )
           CALL PUTVID ( 'TABLE', 1, PMPBSI, IERUSR )
         CALL SMFCMD ( IERUSR )
      ENDIF
C
C     ----  CHAMPS DE TEMPERATURE (SI IL A ETE CALCULE)
C                       SUR LES LIGNES DE DEPOUILLEMENT  ----
C
      IF (THERM) THEN
         ICMD = ICMD + 1
         CALL GCNCON ( '.' , RTHAZI )
         CALL SMDCMD ( ICMD, RTHAZI, 'POST_RELEVE_T', IERUSR )
            DO 280 I = 1 , NBAZIM , IPAS
              CALL CODENT ( I , 'G' , NUME )
              IF (I.LE.9) THEN
                 NUME2 =  '0' // NUME(1:1)
              ELSE
                 NUME2 =  NUME
              ENDIF
              GRNOLI = 'LI'//NUME
              INTITI = 'AZI_'//NUME2//'_I'
              CALL SMDMCF ( 'ACTION', IERUSR )
                CALL PUTVID ( 'GROUP_NO'  , 1, GRNOLI, IERUSR )
                CALL PUTVID ( 'RESULTAT'  , 1, RESUTH, IERUSR )
                CALL PUTVTX ( 'TOUT_ORDRE' , 1, K8B , I8 , IERUSR)
                CALL PUTVTX ( 'NOM_CHAM'  , 1, NOCHTH  , I4, IERUSR)
                CALL PUTVR8 ( 'PRECISION' , 1, PRECIS, IERUSR )
                CALL PUTVTX ( 'TOUT_CMP'  , 1, K8B   , I8 , IERUSR)
                CALL PUTVTX ( 'INTITULE'  , 1, INTITI , I8 , IERUSR)
                CALL PUTVTX ( 'OPERATION' , 1, K16B1, I16, IERUSR)
              CALL SMFMCF ( IERUSR )
 280        CONTINUE
         CALL SMFCMD ( IERUSR )
C
C     ----  IMPR_TABLE DU CHAMP DE TEMPERATURE
C                      SUR LES LIGNES DE DEPOUILLEMENT  ----
C
         ICMD = ICMD + 1
         CALL SMDCMD ( ICMD, ' ', 'IMPR_TABLE', IERUSR )
           CALL PUTVID ( 'TABLE', 1, RTHAZI, IERUSR )
           CALL PUTVTX ( 'NOM_PARA', 9, IMPRT3, I8V2, IERUSR )
         CALL SMFCMD ( IERUSR )
C
C     ----  PARAMETRES CARACTERISANT LA DISTRIBUTION DE TEMPERATURE
C           (SI ELLE A ETE CALCULEE) DANS L'EPAISSEUR DU LIGAMENT  ----
C
         ICMD = ICMD + 1
         CALL GCNCON ( '.' , RMOTHI )
         CALL SMDCMD ( ICMD, RMOTHI, 'POST_RELEVE_T', IERUSR )
            DO 290 I = 1 , NBAZIM , IPAS
              CALL CODENT ( I , 'G' , NUME )
              IF (I.LE.9) THEN
                 NUME2 =  '0' // NUME(1:1)
              ELSE
                 NUME2 =  NUME
              ENDIF
              GRNOLI = 'LI'//NUME
              INTITI = 'AZI_'//NUME2//'_I'
              CALL SMDMCF ( 'ACTION', IERUSR )
                CALL PUTVID ( 'GROUP_NO'  , 1, GRNOLI, IERUSR )
                CALL PUTVID ( 'RESULTAT'  , 1, RESUTH, IERUSR )
                CALL PUTVTX ( 'TOUT_ORDRE' , 1, K8B , I8 , IERUSR)
                CALL PUTVTX ( 'NOM_CHAM'  , 1, NOCHTH  , I4, IERUSR)
                CALL PUTVR8 ( 'PRECISION' , 1, PRECIS, IERUSR )
                CALL PUTVTX ( 'TOUT_CMP'  , 1, K8B   , I8 , IERUSR)
                CALL PUTVTX ( 'INTITULE'  , 1, INTITI, I8 , IERUSR)
                CALL PUTVTX ( 'OPERATION' , 1, K16B3 , I16, IERUSR)
              CALL SMFMCF ( IERUSR )
C
 290        CONTINUE
         CALL SMFCMD ( IERUSR )
C
C     ----  IMPR_TABLE DU CHAMP DE TEMPERATURE
C                         SUR LES LIGNES DE DEPOUILLEMENT  ----
C
         ICMD = ICMD + 1
         CALL SMDCMD ( ICMD, ' ', 'IMPR_TABLE', IERUSR )
           CALL PUTVID ( 'TABLE', 1, RMOTHI, IERUSR )
C           CALL PUTVTX ( 'NOM_PARA', 9, IMPRT3, I8V2, IERUSR )
         CALL SMFCMD ( IERUSR )
C
      ENDIF
C
C-----------------------------------------------------------------------
      ELSEIF (PIQUAG(1:4).EQ.'FISS') THEN
C-----------------------------------------------------------------------
C
         K16B1 = 'EXTRACTION'
C
         IF ((PIQUAG.EQ.'FISS_COUR_DEB') .OR.
     &       (PIQUAG.EQ.'FISS_LONG_DEB') .OR.
     &       (PIQUAG.EQ.'FISS_AXIS_DEB') .OR.
     &       (PIQUAG.EQ.'FISS_COUR_NONDEB') ) THEN
            NBFIS = 1
            NOMGRO(1,1)='P_FON1'
            NOMGRO(2,1)='P_FIS1'
            NOMGRE(1,1)='P_FON2'
            NOMGRE(2,1)='P_FIS2'
            TABMA8(1)='FONDFISS'
            IF ((PIQUAG.EQ.'FISS_COUR_DEB') .OR.
     &          (PIQUAG.EQ.'FISS_LONG_DEB') ) THEN
               FERME=.FALSE.
            ELSE
               FERME=.TRUE.
               NOMMA(1)='MAIL_ORI'
            ENDIF
         ELSEIF ((PIQUAG.EQ.'FISS_LONG_NONDEB') .OR.
     &           (PIQUAG.EQ.'FISS_AXIS_NONDEB') ) THEN
            NBFIS = 2
            NOMGRO(1,1)='PS_FON1'
            NOMGRO(2,1)='PS_FIS1'
            NOMGRE(1,1)='PS_FON2'
            NOMGRE(2,1)='PS_FIS2'
            NOMGRO(1,2)='PI_FON1'
            NOMGRO(2,2)='PI_FIS1'
            NOMGRE(1,2)='PI_FON2'
            NOMGRE(2,2)='PI_FIS2'
            TABMA8(1)='FOND_SUP'
            TABMA8(2)='FOND_INF'
            IF (PIQUAG.EQ.'FISS_LONG_NONDEB') THEN
               FERME=.FALSE.
            ELSE
               FERME=.TRUE.
               NOMMA(1)='MA_ORI_S'
               NOMMA(2)='MA_ORI_I'
            ENDIF
         ENDIF
C
C
C
         IF (THERM) THEN
C
C     ----  CHAMPS DE TEMPERATURE EN FOND DE FISSURE 
C           (SI IL A ETE CALCULE, cas 1 fond de fissure)  ----
C
            IF (NBFIS .EQ. 1) THEN
C
C            --- COMMANDE  DEFI_GROUP  ---
C
               ICMD = ICMD + 1
               CALL SMDCMD ( ICMD, '&'//MAILLA, 'DEFI_GROUP', IERUSR )
                 CALL PUTVID ( 'MAILLAGE', 1, MAILLA, IERUSR )
                 NOFDFI(1) = 'FONDFISS'
                 CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
                   CALL PUTVID ( 'GROUP_MA',1 , NOFDFI(1), IERUSR )
                 CALL SMFMCF ( IERUSR )
               CALL SMFCMD ( IERUSR )
C
               ICMD = ICMD + 1
               CALL GCNCON ( '.' , RTHFIS(1) )
               CALL SMDCMD (ICMD, RTHFIS(1), 'POST_RELEVE_T', IERUSR)
                 CALL SMDMCF ( 'ACTION', IERUSR )
                   CALL PUTVID ('GROUP_NO'  , 1, NOFDFI(1), IERUSR)
                   CALL PUTVID ('RESULTAT'  , 1, RESUTH, IERUSR )
                   CALL PUTVTX ('TOUT_ORDRE' , 1, K8B , I8 , IERUSR)
                   CALL PUTVTX ('NOM_CHAM'  , 1, NOCHTH  , I4, IERUSR)
                   CALL PUTVR8 ('PRECISION' , 1, PRECIS, IERUSR )
                   CALL PUTVTX ('TOUT_CMP'  , 1, K8B   , I8 , IERUSR)
                   CALL PUTVTX ('INTITULE', 1, NOFDFI(1), I8 , IERUSR)
                   CALL PUTVTX ('OPERATION', 1, K16B1 , I16, IERUSR)
                 CALL SMFMCF ( IERUSR )
               CALL SMFCMD ( IERUSR )
C
C     ----  IMPR_TABLE DU CHAMP DE TEMPERATURE
C                    EN FOND DE FISSURE (Cas 1 fond de fissure)  ----
C
               ICMD = ICMD + 1
               CALL SMDCMD ( ICMD, ' ', 'IMPR_TABLE', IERUSR )
                 CALL PUTVID ( 'TABLE', 1, RTHFIS(1), IERUSR )
C                 CALL PUTVTX ( 'NOM_PARA', 9, IMPRT3, I8V2, IERUSR )
               CALL SMFCMD ( IERUSR )
C
C     ----  CHAMPS DE TEMPERATURE EN FOND DE FISSURE 
C           (SI IL A ETE CALCULE, cas 2 fonds de fissure)  ----
C
            ELSEIF (NBFIS .EQ. 2) THEN
C
C            --- COMMANDE  DEFI_GROUP  ---
C
               ICMD = ICMD + 1
               CALL SMDCMD ( ICMD, '&'//MAILLA, 'DEFI_GROUP', IERUSR )
                 CALL PUTVID ( 'MAILLAGE', 1, MAILLA, IERUSR )
                   NOFDFI(2) = 'FOND_SUP'
                   NOFDFI(3) = 'FOND_INF'
                 CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
                   CALL PUTVID ( 'GROUP_MA',1 , NOFDFI(2), IERUSR )
                 CALL SMFMCF ( IERUSR )
                 CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
                   CALL PUTVID ( 'GROUP_MA',1 , NOFDFI(3), IERUSR )
                 CALL SMFMCF ( IERUSR )
               CALL SMFCMD ( IERUSR )
C
               DO 310 I = 1, NBFIS
                 ICMD = ICMD + 1
                 CALL GCNCON ( '.' , RTHFIS(I) )
                 CALL SMDCMD (ICMD, RTHFIS(I), 'POST_RELEVE_T', IERUSR)
                   CALL SMDMCF ( 'ACTION', IERUSR )
                     CALL PUTVID ('GROUP_NO' , 1, NOFDFI(I+1), IERUSR)
                     CALL PUTVID ('RESULTAT'  , 1, RESUTH, IERUSR )
                     CALL PUTVTX ('TOUT_ORDRE', 1, K8B, I8, IERUSR)
                     CALL PUTVTX ('NOM_CHAM', 1, NOCHTH, I4, IERUSR)
                     CALL PUTVR8 ('PRECISION' , 1, PRECIS, IERUSR )
                     CALL PUTVTX ('TOUT_CMP'  , 1,K8B , I8 , IERUSR)
                     CALL PUTVTX ('INTITULE',1, NOFDFI(I+1),I8, IERUSR)
                     CALL PUTVTX ('OPERATION', 1, K16B1, I16, IERUSR)
                   CALL SMFMCF ( IERUSR )
                 CALL SMFCMD ( IERUSR )
C
C     ----  IMPR_TABLE DU CHAMP DE TEMPERATURE
C                    EN FOND DE FISSURE (Cas 2 fonds de fissure)  ----
C
                 ICMD = ICMD + 1
                 CALL SMDCMD ( ICMD, ' ', 'IMPR_TABLE', IERUSR )
                   CALL PUTVID ( 'TABLE', 1, RTHFIS(I), IERUSR )
C                   CALL PUTVTX ( 'NOM_PARA', 9, IMPRT3, I8V2, IERUSR )
                 CALL SMFCMD ( IERUSR )
 310           CONTINUE
            ENDIF
         ENDIF
C
C        BOUCLE SUR LE NOMBRE DE FOND DE FISSURE
C
         DO 320 J = 1 , NBFIS
C
C          --- COMMANDE DEFI_FOND_FISS ---
C
         ICMD = ICMD + 1
C         CALL GCNCON ( '.' , FOND3D(J) )
         CALL SMDCMD ( ICMD, FOND3D(J), 'DEFI_FOND_FISS', IERUSR )
           CALL PUTVID ( 'MAILLAGE', 1, MAILLA, IERUSR )
           IF (.NOT. FERME) THEN
              CALL SMDMCF ( 'FOND', IERUSR )
                CALL PUTVID ( 'GROUP_MA' , 1, TABMA8(J), IERUSR)
                CALL PUTVID ( 'GROUP_NO_ORIG' , 1, NOMGRO(1,J), IERUSR)
                CALL PUTVID ( 'GROUP_NO_EXTR' , 1, NOMGRE(1,J), IERUSR)
              CALL SMFMCF ( IERUSR )
              CALL PUTVID('VECT_GRNO_ORIG',2,NOMGRO(1,J),IERUSR)
              CALL PUTVID('VECT_GRNO_EXTR',2,NOMGRE(1,J),IERUSR)
           ELSE
              CALL SMDMCF ( 'FOND_FERME', IERUSR )
                CALL PUTVID ( 'GROUP_MA' , 1, TABMA8(J), IERUSR)
                IF (PIQUAG(6:9).EQ.'AXIS') THEN
C                  SI AXIS, P_FON1 EST REMPLACE PAR P_FON2 POUR
C                  FERMER LE FOND DE FISSURE
                   CALL PUTVID ('GROUP_NO_ORIG',1,NOMGRE(1,J),IERUSR)
                ELSE
                   CALL PUTVID ('GROUP_NO_ORIG',1,NOMGRO(1,J),IERUSR)
                ENDIF
                CALL PUTVID ( 'GROUP_MA_ORIG' , 1, NOMMA(J), IERUSR)
              CALL SMFMCF ( IERUSR )
           ENDIF
           CALL SMDMCF ( 'LEVRE_SUP', IERUSR )
             NOM8='LEVRCORP'
             CALL PUTVID ( 'GROUP_MA' , 1, NOM8, IERUSR)
           CALL SMFMCF ( IERUSR )
           CALL SMDMCF ( 'LEVRE_INF', IERUSR )
             NOM8='LEVRTUBU'
             CALL PUTVID ( 'GROUP_MA' , 1, NOM8, IERUSR)
           CALL SMFMCF ( IERUSR )
         CALL SMFCMD ( IERUSR )
C
         K8B1 = 'OUI'
         DO 330 I = 1 , NBRUPT
C
C          --- COMMANDE CALC_THETA ---
C
           ICMD = ICMD + 1
           CALL GCNCON ( '.' , THETA(J) )
           CALL SMDCMD ( ICMD, THETA(J), 'CALC_THETA', IERUSR )
             CALL PUTVID ( 'MODELE'  , 1, MODELE, IERUSR )
             CALL PUTVID ( 'FOND_FISS', 1, FOND3D(J), IERUSR )
             CALL SMDMCF ( 'THETA_3D', IERUSR )
               CALL PUTVTX ( 'TOUT'   , 1, K8B1, I8, IERUSR)
               MODULE=1.D0
               CALL PUTVR8 ( 'MODULE' , 1, MODULE       , IERUSR )
               CALL PUTVR8 ( 'R_INF'  , 1, ZR(JRINF+I-1), IERUSR )
               CALL PUTVR8 ( 'R_SUP'  , 1, ZR(JRSUP+I-1), IERUSR )
             CALL SMFMCF ( IERUSR )
           CALL SMFCMD ( IERUSR )
C
C          --- COMMANDE CALC_G_THETA_T ---
C
           WRITE(MONTIT,1000) ZR(JRINF+I-1), ZR(JRSUP+I-1)
           ICMD = ICMD + 1
           CALL GCNCON ( '.' , GTHETA(J) )
           CALL SMDCMD ( ICMD, GTHETA(J), 'CALC_G_THETA_T', IERUSR )
             CALL PUTVID ( 'MODELE'    , 1, MODELE, IERUSR )
             CALL PUTVID ( 'CHAM_MATER', 1, AFFMAT, IERUSR )
             CALL PUTVID ( 'THETA'     , 1, THETA(J) , IERUSR )
             CALL PUTVID ( 'RESULTAT'  , 1, NOMRES, IERUSR )
             CALL PUTVTX ( 'TOUT_ORDRE', 1, K8B1, I8, IERUSR)
             IF ((NPRE .NE. 0) .AND. (PIQUAG(11:13) .EQ. 'DEB')) THEN
               CALL PUTVID ( 'CHARGE', 1, CHPRES, IERUSR )
             ENDIF
             IF (THERM)  CALL PUTVID ( 'CHARGE', 1, CHMETH, IERUSR )
             IF (NCOEL.NE.0) THEN
              CALL SMDMCF ( 'COMP_ELAS', IERUSR )
                CALL PUTVTX ( 'TOUT'    , 1, 'OUI',      3, IERUSR )
                CALL PUTVTX ( 'RELATION', 1, RELCO, LRELCO, IERUSR )
              CALL SMFMCF ( IERUSR )
             END IF
             IF ( NCOIN .NE. 0 ) THEN
               CALL SMDMCF ( 'COMP_INCR', IERUSR )
               CALL PUTVTX ( 'RELATION', 1, RELCO, LRELCO, IERUSR )
               CALL SMFMCF (   IERUSR )
             ENDIF
             CALL PUTVTX ( 'TITRE', 1, MONTIT, I57, IERUSR)
           CALL SMFCMD ( IERUSR )
C
           ICMD = ICMD + 1
           CALL SMDCMD ( ICMD, ' ', 'IMPR_TABLE', IERUSR )
             CALL PUTVID ( 'TABLE', 1, GTHETA(J), IERUSR )
           CALL SMFCMD ( IERUSR )
C
C          RECHERCHE DU G MAX
C
           IF (OPTION .EQ. 'CALC_G_MAX') THEN
             IF (NBORN.NE.0) THEN
C
               ICMD = ICMD + 1
               CALL GCNCON ( '.' , GBIL(J) )
               CALL SMDCMD ( ICMD, GBIL(J), 'CALC_G_THETA_T', IERUSR )
                 CALL PUTVID ( 'MODELE'         , 1, MODELE, IERUSR )
                 CALL PUTVID ( 'CHAM_MATER', 1, AFFMAT, IERUSR )
                 CALL PUTVID ( 'THETA'         , 1, THETA(J) , IERUSR)
                 CALL PUTVID ( 'RESULTAT'  , 1, NOMRES, IERUSR )
                 CALL PUTVTX ( 'TOUT_ORDRE', 1, K8B1, I8, IERUSR)
                 CALL SMDMCF ( 'COMP_ELAS', IERUSR )
                   CALL PUTVTX ( 'TOUT'    , 1, 'OUI',      3, IERUSR )
                   CALL PUTVTX ( 'RELATION', 1, RELCO, LRELCO, IERUSR)
                 CALL SMFMCF ( IERUSR )
                 CALL PUTVTX ( 'TITRE' ,1,MONTIT, I57, IERUSR)
                 CALL PUTVTX ( 'OPTION',1,OPTION, I16, IERUSR )
                 DO 340 K= 1, NBORN
                   CALL SMDMCF ( 'BORNES', IERUSR )
                     CALL PUTVIS ( 'NUME_ORDRE',1,ZI(IORD+K-1),IERUSR )
                     CALL PUTVR8 ( 'VALE_MIN' , 1,
     +                             ZR(IBOR+2*(K-1)) , IERUSR )
                     CALL PUTVR8 ( 'VALE_MAX' , 1,
     +                             ZR(IBOR+2*(K-1)+1) , IERUSR )
                   CALL SMFMCF ( IERUSR )
 340             CONTINUE
               CALL SMFCMD ( IERUSR )
C
               ICMD = ICMD + 1
               CALL SMDCMD ( ICMD, ' ', 'IMPR_TABLE', IERUSR )
                 CALL PUTVID ( 'TABLE', 1, GBIL(J), IERUSR )
               CALL SMFCMD ( IERUSR )
C
             END IF
           END IF
C
C          --- COMMANDE CALC_G_LOCAL_T ---
C
           WRITE(MONTIT,1010) ZR(JRINF+I-1), ZR(JRSUP+I-1)
           ICMD = ICMD + 1
           CALL GCNCON ( '.' , GLOCAL(J) )
           CALL SMDCMD ( ICMD, GLOCAL(J), 'CALC_G_LOCAL_T', IERUSR )
             CALL PUTVID ( 'MODELE'    , 1, MODELE, IERUSR )
             CALL PUTVID ( 'CHAM_MATER', 1, AFFMAT, IERUSR )
             CALL PUTVID ( 'FOND_FISS' , 1, FOND3D(J), IERUSR )
             CALL PUTVID ( 'RESULTAT'  , 1, NOMRES, IERUSR )
             CALL PUTVTX ( 'TOUT_ORDRE', 1, K8B1, I8, IERUSR)
             IF ((NPRE .NE. 0) .AND. (PIQUAG(11:13) .EQ. 'DEB')) THEN
               CALL PUTVID ( 'CHARGE', 1, CHPRES, IERUSR )
             ENDIF
             IF (THERM)  CALL PUTVID ( 'CHARGE', 1, CHMETH, IERUSR )
             IF (NCOEL.NE.0) THEN
              CALL SMDMCF ( 'COMP_ELAS', IERUSR )
                CALL PUTVTX ( 'TOUT'    , 1, 'OUI',      3, IERUSR )
                CALL PUTVTX ( 'RELATION', 1, RELCO, LRELCO, IERUSR )
              CALL SMFMCF ( IERUSR )
             END IF
             CALL PUTVR8 ( 'R_INF'  , 1, ZR(JRINF+I-1), IERUSR )
             CALL PUTVR8 ( 'R_SUP'  , 1, ZR(JRSUP+I-1), IERUSR )
             CALL PUTVTX ( 'TITRE', 1, MONTIT, I57, IERUSR)
             IF (FERME) THEN
                NOM8 = 'LAGRANGE'
                CALL PUTVTX ( 'LISSAGE_THETA',1,NOM8,I8,IERUSR)
                CALL PUTVTX ( 'LISSAGE_G',1,NOM8,I8,IERUSR)
             ENDIF
           CALL SMFCMD ( IERUSR )
C
C          RECHERCHE DU G MAX LOCAL
C
           IF (OPTION .EQ. 'CALC_G_MAX_LOCAL') THEN
             OPTIO2 = 'CALC_G_MAX'
             IF (NBORN.NE.0) THEN
C
               ICMD = ICMD + 1
               CALL GCNCON ( '.' , GLBIL(J) )
               CALL SMDCMD ( ICMD, GLBIL(J), 'CALC_G_LOCAL_T', IERUSR )
                 CALL PUTVID ( 'MODELE'         , 1, MODELE, IERUSR )
                 CALL PUTVID ( 'CHAM_MATER', 1, AFFMAT, IERUSR )
                 CALL PUTVID ( 'FOND_FISS' , 1, FOND3D(J), IERUSR )
                 CALL PUTVID ( 'RESULTAT'  , 1, NOMRES, IERUSR )
                 CALL PUTVTX ( 'TOUT_ORDRE', 1, K8B1, I8, IERUSR)
                 CALL SMDMCF ( 'COMP_ELAS', IERUSR )
                   CALL PUTVTX ( 'TOUT'    , 1, 'OUI',      3, IERUSR )
                   CALL PUTVTX ( 'RELATION', 1, RELCO, LRELCO, IERUSR)
                 CALL SMFMCF ( IERUSR )
                 CALL PUTVTX ( 'TITRE' ,1,MONTIT, I57, IERUSR)
                 CALL PUTVTX ( 'OPTION',1,OPTIO2, I16, IERUSR )
                 CALL PUTVR8 ( 'R_INF'  , 1, ZR(JRINF+I-1), IERUSR )
                 CALL PUTVR8 ( 'R_SUP'  , 1, ZR(JRSUP+I-1), IERUSR )
                 CALL PUTVTX ( 'TITRE', 1, MONTIT, I57, IERUSR)
                 IF (FERME) THEN
                   NOM8 = 'LAGRANGE'
                   CALL PUTVTX ( 'LISSAGE_THETA',1,NOM8,I8,IERUSR)
                   CALL PUTVTX ( 'LISSAGE_G',1,NOM8,I8,IERUSR)
                 ENDIF
                 DO 400 K= 1, NBORN
                   CALL SMDMCF ( 'BORNES', IERUSR )
                     CALL PUTVIS ( 'NUME_ORDRE',1,ZI(IORD+K-1),IERUSR )
                     CALL PUTVR8 ( 'VALE_MIN' , 1,
     +                             ZR(IBOR+2*(K-1)) , IERUSR )
                     CALL PUTVR8 ( 'VALE_MAX' , 1,
     +                             ZR(IBOR+2*(K-1)+1) , IERUSR )
                   CALL SMFMCF ( IERUSR )
 400             CONTINUE
               CALL SMFCMD ( IERUSR )
C
               ICMD = ICMD + 1
               CALL SMDCMD ( ICMD, ' ', 'IMPR_TABLE', IERUSR )
                 CALL PUTVID ( 'TABLE', 1, GLBIL(J), IERUSR )
               CALL SMFCMD ( IERUSR )
C
             END IF
           END IF
C
           ICMD = ICMD + 1
           CALL SMDCMD ( ICMD, ' ', 'IMPR_TABLE', IERUSR )
             CALL PUTVID ( 'TABLE', 1, GLOCAL(J), IERUSR )
           CALL SMFCMD ( IERUSR )
C
 330     CONTINUE
 320     CONTINUE
         CALL JEDETR ( '&&OPS017.R_INF' )
         CALL JEDETR ( '&&OPS017.R_SUP' )
         CALL JEDETR ( '&&OPS017.COUPLES_BORNES' )
         CALL JEDETR ( '&&OPS017.NUM_ORDRE' )
      ENDIF
C
C     --- COMMANDE IMPR_RESU  ---
C
      IF ( IMPR ) THEN
C
         K8B = 'OUI'
         ICMD = ICMD + 1
         CALL SMDCMD ( ICMD, ' ', 'IMPR_RESU', IERUSR )
           CALL PUTVID ( 'MODELE'    , 1, MODELE, IERUSR )
             CALL SMDMCF ( 'RESU', IERUSR )
               CALL PUTVID ( 'MAILLAGE', 1, MAILLA, IERUSR )
               CALL PUTVID ( 'RESULTAT', 1, NOMRES, IERUSR )
C               CALL PUTVTX ( 'TOUT_ORDRE', 1, K8B, I8, IERUSR)
               CALL PUTVTX ( 'FORMAT'  , 1, FORMAT, LFORMA, IERUSR)
               IF ((FORMAT(1:5).EQ.'IDEAS') .OR. 
     +             (FORMAT(1:6).EQ.'CASTEM')) THEN
                 IF ( N12 .EQ. 3 ) THEN
                   NOMCHA(1) = 'DEPL'
                   NOMCHA(2) = 'EQUI_ELNO_SIGM'
                   CALL PUTVTX('NOM_CHAM',2, NOMCHA(1), L2I16, IERUSR)
                 ELSEIF ( (N12 .EQ. 1) .AND. 
     +                    (NOMCHA(1)(1:4) .NE. 'TEMP') ) THEN
                   CALL PUTVTX('NOM_CHAM',N12, NOMCHA(1), I16, IERUSR)
                 ELSEIF ( (N12 .EQ. 2) .AND. 
     +                    (NOMCHA(1)(1:4) .NE. 'TEMP') .AND. 
     +                    (NOMCHA(2)(1:4) .NE. 'TEMP') ) THEN
                   CALL PUTVTX('NOM_CHAM',N12,NOMCHA(1),L2I16,IERUSR)
                 ELSEIF ( (N12 .EQ. 2) .AND. 
     +                    (NOMCHA(1)(1:4) .EQ. 'TEMP') ) THEN
                   CALL PUTVTX('NOM_CHAM',1, NOMCHA(2), I16, IERUSR)
                 ELSEIF ( (N12 .EQ. 2) .AND. 
     +                    (NOMCHA(2)(1:4) .EQ. 'TEMP') ) THEN
                   CALL PUTVTX('NOM_CHAM',1, NOMCHA(1), I16, IERUSR)
                 ENDIF
                 IF ( TOUORD(1:3) .EQ. 'OUI' ) THEN
                   CALL PUTVTX('TOUT_ORDRE',1, TOUORD, I3, IERUSR)
                 ELSEIF ( NBORDR .NE. 0 ) THEN
                   CALL PUTVIS('NUME_ORDRE', NBORDR, ZI(JORDR), IERUSR)
                 ELSEIF ( NBINST .NE. 0 ) THEN
                   CALL PUTVR8 ( 'INST', NBINST, ZR(JINST) , IERUSR )
                 ENDIF
               ENDIF
               IF (FORMAT(1:5).EQ.'IDEAS') THEN
                  CALL PUTVIS ( 'VERSION' , 1, VERSIO, IERUSR )
               ELSEIF (FORMAT(1:6).EQ.'CASTEM') THEN
                  CALL PUTVIS ( 'NIVE_GIBI' , 1, NIVGIB, IERUSR )
               ENDIF
 
           CALL SMFMCF ( IERUSR )
           IF (THERM) THEN
             CALL SMDMCF ( 'RESU', IERUSR )
               CALL PUTVID ( 'RESULTAT', 1, RESUTH , IERUSR )
C               CALL PUTVTX ( 'TOUT_ORDRE', 1, K8B, I8, IERUSR)
               CALL PUTVTX ( 'FORMAT'  , 1, FORMAT, LFORMA, IERUSR)
               IF ((FORMAT(1:5).EQ.'IDEAS') .OR. 
     +             (FORMAT(1:6).EQ.'CASTEM')) THEN
                 IF ( N12 .EQ. 3 ) THEN
                   NOMCHA(1) = 'TEMP'
                   CALL PUTVTX('NOM_CHAM',1, NOMCHA(1), I4, IERUSR)
                 ELSEIF ( ( N12 .EQ. 1 ) .AND. 
     +                    ( NOMCHA(1)(1:4) .EQ. 'TEMP') ) THEN
                   CALL PUTVTX('NOM_CHAM',1, NOMCHA(1), I4, IERUSR)
                 ELSEIF ( ( N12 .EQ. 2 ) .AND. 
     +                    ( NOMCHA(1)(1:4) .EQ. 'TEMP') ) THEN
                   CALL PUTVTX('NOM_CHAM',1, NOMCHA(1), I4, IERUSR)
                 ELSEIF ( ( N12 .EQ. 2 ) .AND. 
     +                    ( NOMCHA(2)(1:4) .EQ. 'TEMP') ) THEN
                   CALL PUTVTX('NOM_CHAM',1, NOMCHA(2), I4, IERUSR)
                 ENDIF
                 IF ( TOUORD(1:3) .EQ. 'OUI' ) THEN
                   CALL PUTVTX('TOUT_ORDRE',1, TOUORD, I3, IERUSR)
                 ELSEIF ( NBORDR .NE. 0 ) THEN
                   CALL PUTVIS('NUME_ORDRE', NBORDR, ZI(JORDR), IERUSR)
                 ELSEIF ( NBINST .NE. 0 ) THEN
                   CALL PUTVR8 ( 'INST', NBINST, ZR(JINST) , IERUSR )
                 ENDIF
               ENDIF
               IF (FORMAT(1:5).EQ.'IDEAS') THEN
                  CALL PUTVIS ( 'VERSION' , 1, VERSIO, IERUSR )
               ELSEIF (FORMAT(1:6).EQ.'CASTEM') THEN
                  CALL PUTVIS ( 'NIVE_GIBI' , 1, NIVGIB, IERUSR )
               ENDIF
             CALL SMFMCF ( IERUSR )
           ENDIF
         CALL SMFCMD ( IERUSR )
      ENDIF
C
 9998 CONTINUE
      IF ( IERUSR .GT. 0 ) THEN
         CALL UTMESS('E',NOMCMD,'ERREURS CONSTATEES DANS LA MACRO')
         IER = IER + IERUSR
      ENDIF
C
 1000 FORMAT ( 'G_THETA AVEC R_INF = ',1PE12.5,' ET R_SUP = ',1PE12.5 )
 1010 FORMAT ( 'G_LOCAL AVEC R_INF = ',1PE12.5,' ET R_SUP = ',1PE12.5 )
C
 9999 CONTINUE
      CALL JEDEMA()
      CALL JEDETC('V', '&&OPS017', 1)
      END
