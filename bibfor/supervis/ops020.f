      SUBROUTINE OPS020 ( ICMD , ICOND , IER )
      IMPLICIT   NONE
      INTEGER             ICMD , ICOND , IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 10/02/2004   AUTEUR LEBOUVIE F.LEBOUVIER 
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
C                   MACR_ASCOUF_CALC
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER           ZI
      COMMON / IVARJE / ZI(1)
      REAL*8            ZR
      COMMON / RVARJE / ZR(1)
      COMPLEX*16        ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL           ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8       ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                       ZK24
      CHARACTER*32                                ZK32
      CHARACTER*80                                         ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
      INTEGER      N1, N2, NTORSC, NCFX(6), NCFY(6), NCFZ(6), NCMX(6),
     +             NCMY(6), NCMZ(6), NFM(6), I, NFPR, NRGMX, NRGRE,
     +             NSOLV, NCOEL, NCONV, NCOIN, LRELCO, IGLMAX, IINMAX,
     +             IINPAS, NUMINI, NUMFIN, NTHET, NPRE, LMETHO,
     +             LRENUM, NPREC, LPRECO, LKSTOP, INFO, NMAXIT,
     +             IERUSR, I8, I16, I80, NTIT, NBMATE, NRM,
     +             NRLI,I16V(2),NIREMP, NIVGIB, LSYME, IRTHET,
     +             LRINTE, NCON,LARRET, NIN, NFI, JPARA, NBPARA,
     +             REAINC, LPREDI, LMATRI, REAITE, ILIMAX, NEWT,
     +             NPRED, LFORMA, VERSIO, NIMP, NCLIM, KB1, LB1(3), NAZ
      INTEGER      N3, N4, N5, NMATE, NCARA, NFOFI, NCHTHE, NTHER
      INTEGER      NINSTI, NINSTF, NEVOL, NSP, NSPM, NCSP, SUBPA, LEVOL
      REAL*8       INSINI, INSFIN, SUBPAM, COEFSP
      REAL*8       KTRDN(6), PRES, RINREL,
     +             TCFX(6), TCFY(6), TCFZ(6), TCMX(6), TCMY(6), TCMZ(6),
     +             RESREL, R8B, RGLMAX, RGLREL, TPREF(3), R8PI,
     +             RLIREL, ALPHA, AZIM, ALPHAR, AZIMR, COEFB1(3), PI
      REAL*8       ACOU, ASEP, RCIN, PCLON
      LOGICAL      FISS, SSEP, THERM, IMPR, IMPT, FLAG
      CHARACTER*2  DDLB1(3)
      CHARACTER*3  FAXI
      CHARACTER*8  MAILLA, MODELE, CARAEL, KSTOP, MODTHE, LINST, GEOM,
     +             APPRES(3), NOP1, NOP2, FMULP, FMULT(6), CHMETH,
     +             AFFMAT, CONLIM, CHPRES, CHTHER, RESUTH, CHTOR(6),
     +             NMAT(3), NZON(3), K8B, MACO1, MACO2, COEFH, TPEXT,
     +             EFFOND,  POINT(3), MODSOR, COUDE, K8B2, GRMAIL(7),
     +             RCCMAT, PLEVRE
      CHARACTER*8  SYME, NOFISS
      CHARACTER*16 TYPRES, NOMCMD, METHOD, RENUM, PRECON,
     +             NOMRES, K16B1, K16B2, K16V(2),
     +             K16B3, ARRET, RINTE, TYPCOU, RELCO,
     +             PREDI , MATRI, FORMAT, EVOL
      CHARACTER*80 MONTIT
C     ------------------------------------------------------------------
      DATA I16V    / 2*16 /
      DATA FISS / .FALSE. /
      DATA SSEP / .FALSE. /
      DATA FAXI / 'NON' /
      DATA KTRDN / 0.0D0 , 0.0D0 , 0.0D0 , 0.0D0 , 0.0D0 , 0.0D0 /
      DATA GRMAIL / 'COUDE' , 'PEAUINT' , 'PEAUEXT' , 'EXTUBE' ,
     &              'CLGV' , 'FACE1' , 'FACE2' /
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      IER = 0
      IF ( ICOND .NE. -1 ) GOTO 9999
      PI = R8PI()
C     ------------------------------------------------------------------
C
      CALL GETRES ( NOMRES, TYPRES, NOMCMD )
C
C     --- LE TYPE DU MAILLAGE ---
C
      CALL GETVTX ( ' ','TYPE_MAILLAGE' ,1,1,1, TYPCOU, N1 )
      IF (TYPCOU.EQ.'FISS_COUDE      ') FISS = .TRUE.
      IF (TYPCOU.EQ.'FISS_AXIS_DEB   ') THEN
        FISS = .TRUE.
        FAXI = 'OUI'
      ENDIF
      IF (TYPCOU.EQ.'SOUS_EPAIS_COUDE') SSEP = .TRUE.
C
C     --- LA COND. AUX LIMITES POUR LE BOL CONIQUE ---
C
       NCLIM = 0
C
      CALL GETFAC ( 'CL_BOL_P2_GV', NCLIM )
      IF (SSEP.AND.NCLIM.NE.0) THEN
       CALL UTMESS('A',NOMCMD,'LA CONDITION AUX LIMITES SUR BOL'//
     +              ' A SECTION CONIQUE EST IGNOREE POUR UN COUDE'//
     +              ' AVEC SOUS-EPAISSEURS')
      ELSE IF (NCLIM.NE.0) THEN
        CALL GETVR8 ( 'CL_BOL_P2_GV', 'ANGLE' , 1,1,1, ALPHA , N1 )
        CALL GETVR8 ( 'CL_BOL_P2_GV', 'AZIMUT', 1,1,1, AZIM , NAZ )
        IF (.NOT.FISS.AND.NAZ.NE.0)
     +    CALL UTMESS('F',NOMCMD,'MOT-CLE AZIMUT NON AUTORISE '//
     +                'DANS LE CAS D''UN COUDE SAIN')
      END IF
C
C     --- LE MAILLAGE ---
C
      CALL GETVID ( ' ', 'MAILLAGE'  , 1,1,1, MAILLA, N1 )
C
C     --- LE MODELE ---
C
      CALL GETVID ( ' ', 'MODELE'    , 1,1,1, MODSOR, N1 )
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
C     --- LE NOM DU FOND DE FISSURE ---
C
      CALL GETVID ( ' ', 'FOND_FISS', 1,1,1, NOFISS, NFOFI )
      IF (NFOFI.EQ.0) CALL GCNCON ( '.' , NOFISS )
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
      NOP1   = 'P1      '
      MACO1  = 'EXTUBE  '
      NOP2   = 'P2      '
      MACO2  = 'CLGV    '
      APPRES(1) = 'PEAUINT '
      APPRES(2) = 'FACE1   '
      APPRES(3) = 'FACE2   '
      COUDE  = 'COUDE   '
C
C     --- LE MATERIAU ---
C
      CALL GETFAC ( 'AFFE_MATERIAU', NBMATE )
      DO 20 I = 1 , NBMATE
         CALL GETVTX ( 'AFFE_MATERIAU', 'TOUT' , I,1,0, K8B , N1 )
         IF ( N1 .NE. 0 ) THEN
            NZON(I) = 'TOUT'
         ELSE
            CALL GETVTX ( 'AFFE_MATERIAU','GROUP_MA' ,I,1,1, NZON(I),N1)
         ENDIF
         CALL GETVID ( 'AFFE_MATERIAU', 'MATER', I,1,1, NMAT(I), N1 )
         CALL GETVR8 ( 'AFFE_MATERIAU', 'TEMP_REF', I,1,1, TPREF(I), N1)
 20   CONTINUE
C
C     --- PRESSION QUI S'APPLIQUE SUR LA PEAU INTERNE ET LES LEVRES ---
C         DE LA FISSURE
C
      CALL GETFAC ( 'PRES_REP', NPRE )
      IF (NPRE.NE.0) THEN
        CALL GETVR8 ( 'PRES_REP', 'PRES', 1,1,1, PRES , N1 )
        CALL GETVTX ( 'PRES_REP', 'EFFE_FOND_P1' , 1,1,1, EFFOND, N1)
        CALL GETVTX ( 'PRES_REP', 'PRES_LEVRE' , 1,1,1, PLEVRE, N1)
        CALL GETVID ( 'PRES_REP', 'FONC_MULT', 1,1,1, FMULP, NFPR )
C
      ENDIF
C
C     --- CONDITIONS D'ECHANGE THERMIQUE ---
C
      THERM = .FALSE.
      CALL GETFAC ( 'ECHANGE', N1 )
      IF ( N1 .NE. 0 ) THEN
         THERM = .TRUE.
         CALL GETVID ( 'ECHANGE', 'COEF_H'   , 1,1,1, COEFH, N1 )
         CALL GETVID ( 'ECHANGE', 'TEMP_EXT' , 1,1,1, TPEXT, N1 )
      ENDIF
C
C     --- TORSEUR D'EFFORTS SUR LE POINT P1 ---
C
      CALL GETFAC ( 'TORS_P1', NTORSC )
      IF ( NTORSC .NE. 0 ) THEN
         DO 30 I=1, NTORSC
           CALL GETVR8 ( 'TORS_P1', 'FX' , I,1,1, TCFX(I), NCFX(I) )
           CALL GETVR8 ( 'TORS_P1', 'FY' , I,1,1, TCFY(I), NCFY(I) )
           CALL GETVR8 ( 'TORS_P1', 'FZ' , I,1,1, TCFZ(I), NCFZ(I) )
           CALL GETVR8 ( 'TORS_P1', 'MX' , I,1,1, TCMX(I), NCMX(I) )
           CALL GETVR8 ( 'TORS_P1', 'MY' , I,1,1, TCMY(I), NCMY(I) )
           CALL GETVR8 ( 'TORS_P1', 'MZ' , I,1,1, TCMZ(I), NCMZ(I) )
           CALL GETVID ( 'TORS_P1', 'FONC_MULT', I,1,1,FMULT(I),NFM(I))
 30      CONTINUE
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
         CALL GETVTX ( 'SOLVEUR', 'PRE_COND'      , 1,1,1, PRECON, NCON)
         CALL GETLTX ( 'SOLVEUR', 'PRE_COND'      , 1,1,1, LPRECO, N1 )
         CALL GETVTX ( 'SOLVEUR', 'STOP_SINGULIER', 1,1,1, KSTOP , N1 )
         CALL GETLTX ( 'SOLVEUR', 'STOP_SINGULIER', 1,1,1, LKSTOP, N1 )
         CALL GETVIS(  'SOLVEUR','NIVE_REMPLISSAGE',1,1,1, NIREMP, N1 )
         CALL GETVR8 ( 'SOLVEUR', 'RESI_RELA'     , 1,1,1, RESREL, N1 )
         CALL GETVIS ( 'SOLVEUR', 'NMAX_ITER'     , 1,1,1, NMAXIT, N1 )
         CALL GETVTX ( 'SOLVEUR', 'SYME'          , 1,1,1, SYME,   N1 )
         CALL GETLTX ( 'SOLVEUR', 'SYME'          , 1,1,1, LSYME,  N1 )
      ENDIF
C
C     --- CONVERGENCE ---
C
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
      CALL GETVID ( 'INCREMENT', 'LIST_INST'     , 1,1,1,LINST,N1)
      CALL GETVIS ( 'INCREMENT', 'NUME_INST_INIT', 1,1,0,NUMINI,NIN)
      CALL GETVIS ( 'INCREMENT', 'NUME_INST_INIT', 1,1,1,NUMINI,N1)
      CALL GETVIS ( 'INCREMENT', 'NUME_INST_FIN' , 1,1,0,NUMFIN,NFI)
      CALL GETVIS ( 'INCREMENT', 'NUME_INST_FIN' , 1,1,1,NUMFIN,N1)
      CALL GETVIS ( 'INCREMENT', 'NUME_INST_FIN' , 1,1,1,NUMFIN,N1)
      CALL GETVR8 ( 'INCREMENT', 'INST_INIT'     , 1,1,1,INSINI,NINSTI)
      CALL GETVR8 ( 'INCREMENT', 'INST_FIN'      , 1,1,1,INSFIN,NINSTF)
      CALL GETVTX ( 'INCREMENT', 'EVOLUTION'     , 1,1,1,EVOL,NEVOL)
      CALL GETLTX ( 'INCREMENT', 'EVOLUTION'     , 1,1,1,LEVOL,N1)
      CALL GETVIS ( 'INCREMENT', 'SUBD_PAS'      , 1,1,1,SUBPA, NSP)
      CALL GETVR8 ( 'INCREMENT', 'SUBD_PAS_MINI' , 1,1,1,SUBPAM,NSPM)
      CALL GETVR8 ( 'INCREMENT','COEF_SUBD_PAS_1', 1,1,1,COEFSP,NCSP)
C
C     --- THETA_3D ---
C
      CALL GETFAC ( 'THETA_3D', NTHET )
      IF ( NTHET .NE. 0 ) THEN
        CALL WKVECT('&&OPS020.THETA_3D','V V R8',2*NTHET,IRTHET)
        DO 35 I=1, NTHET
          CALL GETVR8 ('THETA_3D','R_INF',I,1,1,ZR(IRTHET+2*(I-1)),N1)
          CALL GETVR8 ('THETA_3D','R_SUP',I,1,1,ZR(IRTHET+2*(I-1)+1),N1)
 35     CONTINUE          
      ENDIF
C
C     --- IMPR_TABLE ---
C
      IMPT = .FALSE.
      FLAG = .FALSE.
      CALL GETFAC ( 'IMPR_TABLE', NIMP )
      IF ( NIMP .NE. 0 ) THEN
        IMPT = .TRUE.
        CALL GETVTX ('IMPR_TABLE','NOM_PARA'  ,1,1,0,K8B ,N1)
        IF ( N1 .EQ. 0 ) THEN
          CALL GETVTX ('IMPR_TABLE','TRANSFORMEE',1,1,1,GEOM, N2)   
          CALL GETVR8 ('IMPR_TABLE','ANGLE',1,1,1,ACOU, N2)
          CALL GETVR8 ('IMPR_TABLE','R_CINTR',1,1,1,RCIN, N2)
          CALL GETVR8 ('IMPR_TABLE','POSI_ANGUL',1,1,1,
     +                  ASEP, N2)
          IF ( N2 .EQ. 0 ) THEN
            CALL GETVR8 ('IMPR_TABLE','POSI_CURV_LONGI',1,1,1,
     +                    PCLON, N2)
            IF ( N2 .EQ. 0 ) THEN
              CALL UTMESS('F',NOMCMD,'UN PARMI : POSI_ANGUL, '//
     +                     'POSI_CURV_LONGI EST OBLIGATOIRE.')
            ENDIF
            ASEP = (PCLON/RCIN)*(180.0D0/PI)
          END IF
          FLAG = .TRUE.
        ENDIF
C
        IF ( N1 .NE. 0 ) THEN
          NBPARA = -N1
          CALL WKVECT('&&OPS020.NOM_PARA','V V K16',NBPARA,JPARA)
          CALL GETVTX ('IMPR_TABLE', 'NOM_PARA', 1, 1, NBPARA,
     +                  ZK16(JPARA), N1)
          DO 100 I = 1,NBPARA
            IF (ZK16(JPARA+I-1) .EQ. 'SI_LONG'.OR.
     +          ZK16(JPARA+I-1) .EQ. 'SI_CIRC'.OR.
     +          ZK16(JPARA+I-1) .EQ. 'SI_RADI') THEN
             CALL GETVR8 ('IMPR_TABLE','ANGLE',1,1,1,
     +                     ACOU, N2)
             CALL GETVR8 ('IMPR_TABLE','POSI_ANGUL',1,1,1,
     +                     ASEP, N3)
             CALL GETVR8 ('IMPR_TABLE','R_CINTR',1,1,1,
     +                     RCIN, N4)
             CALL GETVR8 ('IMPR_TABLE','POSI_CURV_LONGI',1,1,1,
     +                     PCLON, N5)
              IF ( ((N2 .EQ. 0) .AND. (N3 .EQ. 0) .AND.(N4 .EQ. 0))
     +             .OR. ((N2 .EQ. 0) .AND. (N4 .EQ. 0)
     +                    .AND. (N5 .EQ. 0)) ) THEN
                CALL UTMESS('F',NOMCMD,'IL FAUT RENSEIGNER : ANGLE'//
     +               ', R_CINTR ET POSI_ANGUL OU ANGLE , R_CINTR '//
     +                       'ET POSI_CURV_LONGI.')
              ENDIF
            FLAG = .TRUE.
            ENDIF
 100      CONTINUE
        ENDIF
C
        IF ( .NOT. FLAG ) THEN
            CALL UTMESS('A',NOMCMD,'ANGL_COUDE ET ANGL_SOUS_EPAI '//
     +                   'SONT INUTILES DANS CE CAS.')
        ENDIF
      ENDIF
C
C     --- IMPRESSION ---
C
      IMPR = .FALSE.
      CALL GETFAC ( 'IMPRESSION', N1 )
      IF ( N1 .NE. 0 ) THEN
         IMPR = .TRUE.
         CALL GETVTX ( 'IMPRESSION', 'FORMAT' ,   1,1,1, FORMAT, N1 )
         CALL GETLTX ( 'IMPRESSION', 'FORMAT' ,   1,1,1, LFORMA, N1 )
         CALL GETVIS ( 'IMPRESSION', 'VERSION',   1,1,1, VERSIO, N1 )
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
      I8  = 8
      I16 = 16
      I80 = 80
C
C     --- COMMANDE AFFE_MODELE ---
C
      K16B1 = 'MECANIQUE'
      K16B2 = 'DIS_TR'
      K16B3 = '3D'
      ICMD = ICMD + 1
      MODELE = MODSOR
      CALL SMDCMD ( ICMD, MODELE, 'AFFE_MODELE', IERUSR )
        CALL PUTVID ( 'MAILLAGE', 1, MAILLA, IERUSR )
        CALL SMDMCF ( 'AFFE', IERUSR )
          IF ((PLEVRE .EQ. 'OUI') .AND. (TYPCOU(1:4) .EQ. 'FISS')) THEN
             CALL PUTVID ( 'GROUP_MA', 7, GRMAIL, IERUSR)
          ELSE
             CALL PUTVID ( 'GROUP_MA', 5, GRMAIL, IERUSR)
          ENDIF
          CALL PUTVTX ( 'PHENOMENE'   , 1, K16B1, I16, IERUSR)
          CALL PUTVTX ( 'MODELISATION', 1, K16B3, I16, IERUSR)
        CALL SMFMCF ( IERUSR )
        IF (NTORSC.NE.0) THEN
          CALL SMDMCF ( 'AFFE', IERUSR )
            CALL PUTVID ( 'GROUP_MA'    , 1, NOP1, IERUSR )
            CALL PUTVTX ( 'PHENOMENE'   , 1, K16B1, I16, IERUSR)
            CALL PUTVTX ( 'MODELISATION', 1, K16B2, I16, IERUSR)
          CALL SMFMCF ( IERUSR )
        END IF
        IF (NCLIM.EQ.0) THEN
          CALL SMDMCF ( 'AFFE', IERUSR )
            CALL PUTVID ( 'GROUP_MA'    , 1, NOP2, IERUSR )
            CALL PUTVTX ( 'PHENOMENE'   , 1, K16B1, I16, IERUSR)
            CALL PUTVTX ( 'MODELISATION', 1, K16B2, I16, IERUSR)
          CALL SMFMCF ( IERUSR )
        END IF
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
            CALL PUTVTX ( 'TOUT', 1, K8B, I8, IERUSR)
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
        DO 22 I = 1 , NBMATE
           CALL SMDMCF ( 'AFFE', IERUSR )
             IF ( NZON(I)(1:4) .EQ. 'TOUT' ) THEN
                RCCMAT = NMAT(I)
                CALL PUTVTX ( 'TOUT', 1, K8B, I8, IERUSR)
             ELSE
                CALL PUTVID ( 'GROUP_MA', 1, NZON(I), IERUSR )
             ENDIF
             CALL PUTVID ( 'MATER', 1, NMAT(I), IERUSR )
             CALL PUTVR8 ( 'TEMP_REF', 1, TPREF(I), IERUSR )
           CALL SMFMCF ( IERUSR )
C
           IF (NZON(I)(1:5).EQ.'COUDE') THEN           
C
             RCCMAT = NMAT(I)
             IF (NTORSC.NE.0) THEN
               CALL SMDMCF ( 'AFFE', IERUSR )
                 CALL PUTVID ( 'GROUP_MA'   , 1, NOP1, IERUSR )
                 CALL PUTVID ( 'MATER',        1, NMAT(I), IERUSR )
                 CALL PUTVR8 ( 'TEMP_REF', 1, TPREF(I), IERUSR )
               CALL SMFMCF ( IERUSR )
             END IF
             IF (NBMATE.EQ.1.AND.NCLIM.EQ.0) THEN
               CALL SMDMCF ( 'AFFE', IERUSR )
                 CALL PUTVID ( 'GROUP_MA'    , 1, NOP2, IERUSR )
                 CALL PUTVID ( 'MATER',        1, NMAT(I), IERUSR )
                 CALL PUTVR8 ( 'TEMP_REF', 1, TPREF(I), IERUSR )
               CALL SMFMCF ( IERUSR )
             END IF
C
           ELSE IF (NZON(I)(1:3).EQ.'BOL'.AND.NCLIM.EQ.0) THEN
C
             CALL SMDMCF ( 'AFFE', IERUSR )
               CALL PUTVID ( 'GROUP_MA'    , 1, NOP2, IERUSR )
               CALL PUTVID ( 'MATER',        1, NMAT(I), IERUSR )
               CALL PUTVR8 ( 'TEMP_REF', 1, TPREF(I), IERUSR )
             CALL SMFMCF ( IERUSR )
C
           END IF
C
 22     CONTINUE

      CALL SMFCMD ( IERUSR )
C
C     --- COMMANDE AFFE_CARA_ELEM ---
C
      IF (NTORSC.NE.0.OR.NCLIM.EQ.0) THEN
C
        K8B = 'K_TR_D_N'
        ICMD = ICMD + 1
        CALL SMDCMD ( ICMD, CARAEL, 'AFFE_CARA_ELEM', IERUSR )
          CALL PUTVID ( 'MODELE'  , 1, MODELE, IERUSR )
          IF (NTORSC.NE.0) THEN
           CALL SMDMCF ( 'DISCRET', IERUSR )
             CALL PUTVID ( 'GROUP_MA', 1, NOP1, IERUSR )
             CALL PUTVTX ( 'CARA'    , 1, K8B, I8, IERUSR)
             CALL PUTVR8 ( 'VALE'    , 6, KTRDN  , IERUSR )
           CALL SMFMCF ( IERUSR )
          END IF
          IF (NCLIM.EQ.0) THEN
           CALL SMDMCF ( 'DISCRET', IERUSR )
             CALL PUTVID ( 'GROUP_MA', 1, NOP2, IERUSR )
             CALL PUTVTX ( 'CARA'    , 1, K8B, I8, IERUSR)
             CALL PUTVR8 ( 'VALE'    , 6, KTRDN  , IERUSR )
           CALL SMFMCF ( IERUSR )
          END IF
        CALL SMFCMD ( IERUSR )
C
      END IF
C
C     --- CALCUL THERMIQUE ---
C
      IF (THERM) THEN
C
        ICMD = ICMD + 1
        CALL GCNCON ( '.' , CHTHER )
        CALL SMDCMD ( ICMD, CHTHER, 'AFFE_CHAR_THER_F', IERUSR )
          CALL PUTVID ( 'MODELE'  , 1, MODTHE, IERUSR )
          CALL SMDMCF ( 'ECHANGE', IERUSR )
            CALL PUTVID ( 'GROUP_MA' , 1, APPRES(1), IERUSR)
            CALL PUTVID ( 'COEF_H'   , 1, COEFH, IERUSR)
            CALL PUTVID ( 'TEMP_EXT' , 1, TPEXT, IERUSR )
          CALL SMFMCF ( IERUSR )
        CALL SMFCMD ( IERUSR )
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
C         CONDITION AUX LIMITES DE TYPE RACCORD 3D-POUTRE
C         OU BIEN BLOCAGE DE MOUVEMENTS RIGIDES EN CAS D'EMBOUT
C         A SECTION CONIQUE (BOL DE TYPE GV)
C
      R8B = 0.0D0
      ICMD = ICMD + 1
      CALL GCNCON ( '.' , CONLIM )
      CALL SMDCMD ( ICMD, CONLIM, 'AFFE_CHAR_MECA', IERUSR )
        CALL PUTVID ( 'MODELE'  , 1, MODELE, IERUSR )
        K8B = '3D_POU'
        IF (NTORSC.NE.0) THEN
          CALL SMDMCF ( 'LIAISON_ELEM', IERUSR )
            CALL PUTVTX ( 'OPTION'    , 1, K8B, I8, IERUSR)
            CALL PUTVID ( 'GROUP_MA_1', 1, MACO1  , IERUSR )
            CALL PUTVID ( 'GROUP_NO_2', 1, NOP1   , IERUSR )
          CALL SMFMCF ( IERUSR )
        END IF
        IF (NCLIM.EQ.0) THEN
          CALL SMDMCF ( 'LIAISON_ELEM', IERUSR )
            CALL PUTVTX ( 'OPTION'    , 1, K8B, I8, IERUSR)
            CALL PUTVID ( 'GROUP_MA_1', 1, MACO2   , IERUSR )
            CALL PUTVID ( 'GROUP_NO_2', 1, NOP2    , IERUSR )
          CALL SMFMCF ( IERUSR )
          CALL SMDMCF ( 'DDL_IMPO', IERUSR )
            CALL PUTVID ( 'GROUP_NO', 1, NOP2, IERUSR )
            CALL PUTVR8 ( 'DX' , 1, R8B, IERUSR )
            CALL PUTVR8 ( 'DY' , 1, R8B, IERUSR )
            CALL PUTVR8 ( 'DZ' , 1, R8B, IERUSR )
            CALL PUTVR8 ( 'DRX', 1, R8B, IERUSR )
            CALL PUTVR8 ( 'DRY', 1, R8B, IERUSR )
            CALL PUTVR8 ( 'DRZ', 1, R8B, IERUSR )
          CALL SMFMCF ( IERUSR )
        ELSE
          CALL SMDMCF ( 'FACE_IMPO', IERUSR )
             K8B = 'CLGV'
             CALL PUTVID ( 'GROUP_MA', 1, K8B, IERUSR )
             CALL PUTVR8 ( 'DNOR', 1, R8B, IERUSR )
          CALL SMFMCF ( IERUSR )
C
          ALPHAR = ALPHA*PI/180.D0
          AZIMR  = AZIM*PI/180.D0
          KB1 = 0
          IF (AZIM.NE.0.D0.AND.AZIM.NE.180.D0.AND.ALPHA.NE.90.D0) THEN
            KB1 = KB1+1
            DDLB1(KB1) = 'DX'
            COEFB1(KB1) = SIN(AZIMR)*COS(ALPHAR)
          END IF
          IF (AZIM.NE.90.D0) THEN
            KB1 = KB1+1
            DDLB1(KB1) = 'DY'
            COEFB1(KB1) = COS(AZIMR)
          END IF
          IF (AZIM.NE.0.D0.AND.AZIM.NE.180.D0.AND.ALPHA.NE.0.D0)THEN
            KB1 = KB1+1
            DDLB1(KB1) = 'DZ'
            COEFB1(KB1) = -SIN(AZIMR)*SIN(ALPHAR)
          END IF

          DO 10 I=1,KB1
            POINT(I) = 'BOU1'
            LB1(I) = 2
 10          CONTINUE
          CALL SMDMCF ( 'LIAISON_DDL', IERUSR )
             CALL PUTVID ( 'GROUP_NO', KB1, POINT, IERUSR )
             CALL PUTVTX ( 'DDL', KB1, DDLB1, LB1, IERUSR)
             CALL PUTVR8 ( 'COEF_MULT', KB1, COEFB1, IERUSR)
             CALL PUTVR8 ( 'COEF_IMPO', 1, R8B, IERUSR )
          CALL SMFMCF ( IERUSR )
C
        END IF
      CALL SMFCMD ( IERUSR )
C
C     --- COMMANDE AFFE_CHAR_MECA ---
C         CHARGEMENT MECANIQUE ( PRES_REP, EFFET DE FOND )
C
      IF (NPRE.NE.0) THEN
       ICMD = ICMD + 1
       CALL GCNCON ( '.' , CHPRES )
       CALL SMDCMD ( ICMD, CHPRES, 'AFFE_CHAR_MECA', IERUSR )
         CALL PUTVID ( 'MODELE'  , 1, MODELE, IERUSR )
         CALL SMDMCF ( 'PRES_REP', IERUSR )
           IF ((PLEVRE .EQ. 'OUI') .AND. (TYPCOU(1:4) .EQ. 'FISS')) THEN
              CALL PUTVID ( 'GROUP_MA', 3, APPRES, IERUSR )
           ELSE
              CALL PUTVID ( 'GROUP_MA', 1, APPRES(1), IERUSR )
           ENDIF
           CALL PUTVR8 ( 'PRES'    , 1, PRES  , IERUSR )
         CALL SMFMCF ( IERUSR )
         IF ( EFFOND.EQ.'OUI' ) THEN
            CALL SMDMCF ( 'EFFE_FOND', IERUSR )
             K8B = 'BORDTU'
             CALL PUTVID ( 'GROUP_MA_INT', 1, K8B , IERUSR )
             CALL PUTVID ( 'GROUP_MA', 1, MACO1 , IERUSR )
             CALL PUTVR8 ( 'PRES'    , 1, PRES, IERUSR )
            CALL SMFMCF ( IERUSR )
         ENDIF
        CALL SMFCMD ( IERUSR )
      END IF
C
C     --- COMMANDE AFFE_CHAR_MECA ---
C         CHARGEMENT MECANIQUE ( TORSEUR D'EFFORTS )
C
      IF (NTORSC.NE.0) THEN
       DO 40 I=1,NTORSC
        ICMD = ICMD + 1
        CALL GCNCON ( '.' , CHTOR(I) )
        CALL SMDCMD ( ICMD, CHTOR(I), 'AFFE_CHAR_MECA', IERUSR )
          CALL PUTVID ( 'MODELE'  , 1, MODELE, IERUSR )
          CALL SMDMCF ( 'FORCE_NODALE', IERUSR )
            IF (NCFX(I) .NE. 0) CALL PUTVR8 ( 'FX', 1, TCFX(I),IERUSR)
            IF (NCFY(I) .NE. 0) CALL PUTVR8 ( 'FY', 1, TCFY(I),IERUSR)
            IF (NCFZ(I) .NE. 0) CALL PUTVR8 ( 'FZ', 1, TCFZ(I),IERUSR)
            IF (NCMX(I) .NE. 0) CALL PUTVR8 ( 'MX', 1, TCMX(I),IERUSR)
            IF (NCMY(I) .NE. 0) CALL PUTVR8 ( 'MY', 1, TCMY(I),IERUSR)
            IF (NCMZ(I) .NE. 0) CALL PUTVR8 ( 'MZ', 1, TCMZ(I),IERUSR)
            CALL PUTVID ( 'GROUP_NO', 1, NOP1 , IERUSR )
          CALL SMFMCF ( IERUSR )
         CALL SMFCMD ( IERUSR )
 40    CONTINUE
      END IF
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
        IF (NPRE.NE.0) THEN
         CALL SMDMCF ( 'EXCIT', IERUSR )
           CALL PUTVID ( 'CHARGE', 1, CHPRES, IERUSR )
           IF (NFPR.NE.0) CALL PUTVID ( 'FONC_MULT', 1, FMULP, IERUSR)
         CALL SMFMCF ( IERUSR )
        END IF

        DO 50 I= 1, NTORSC
          CALL SMDMCF ( 'EXCIT', IERUSR )
            CALL PUTVID ( 'CHARGE', 1, CHTOR(I), IERUSR )
            IF (NFM(I).NE.0)
     +        CALL PUTVID ( 'FONC_MULT', 1, FMULT(I), IERUSR)
          CALL SMFMCF ( IERUSR )
 50     CONTINUE

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
           CALL PUTVID ( 'GROUP_MA', 1, COUDE , IERUSR )
         CALL SMFMCF ( IERUSR )
         IF (NTORSC.NE.0) THEN
           CALL SMDMCF ( 'COMP_INCR', IERUSR )
             CALL PUTVTX ( 'RELATION', 1, K8B2, I8, IERUSR )
             CALL PUTVID ( 'GROUP_MA', 1, NOP1 , IERUSR )
           CALL SMFMCF ( IERUSR )
         END IF
         IF (NCLIM.EQ.0) THEN
           CALL SMDMCF ( 'COMP_INCR', IERUSR )
             CALL PUTVTX ( 'RELATION', 1, K8B2, I8, IERUSR )
             CALL PUTVID ( 'GROUP_MA', 1, NOP2 , IERUSR )
           CALL SMFMCF ( IERUSR )
         END IF
        END IF
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

            CALL PUTVTX ( 'SYME'          , 1, SYME , LSYME, IERUSR)
          CALL SMFMCF ( IERUSR )
        ENDIF

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
        IF (NIN.NE.0) CALL PUTVIS ( 'NUME_INST_INIT', 1,NUMINI,IERUSR)
        IF (NFI.NE.0) CALL PUTVIS ( 'NUME_INST_FIN' , 1,NUMFIN,IERUSR)
        IF (NINSTI.NE.0) THEN
          CALL PUTVR8 ( 'INST_INIT'  , 1, INSINI, IERUSR )
        END IF
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
      ICMD = ICMD + 1
      CALL SMDCMD ( ICMD, '&'//NOMRES, 'CALC_ELEM', IERUSR )
        CALL PUTVID ( 'RESULTAT'  , 1, NOMRES, IERUSR )
        CALL PUTVID ( 'MODELE'    , 1, MODELE, IERUSR )
        CALL PUTVID ( 'CHAM_MATER', 1, AFFMAT, IERUSR )
        CALL PUTVID ( 'CARA_ELEM', 1, CARAEL, IERUSR )
        IF (THERM) THEN
          CALL SMDMCF ( 'EXCIT', IERUSR )
            CALL PUTVID ( 'CHARGE', 1, CHMETH, IERUSR )
          CALL SMFMCF ( IERUSR )
        END IF
        K16V(1) = 'SIEF_ELNO_ELGA'
        K16V(2) = 'EQUI_ELNO_SIGM'
        CALL PUTVTX ( 'OPTION'    , 2, K16V, I16V, IERUSR)
        K8B = 'OUI'
        CALL PUTVTX ( 'TOUT_ORDRE', 1, K8B, I8, IERUSR)
      CALL SMFCMD ( IERUSR )
C
C     --- POST-TRAITEMENTS ---
C
      IF (SSEP) THEN
C
C       --- POST TRAITEMENT SOUS-EPAISSEURS:  LIGAMENTS  ---
C
        IF (IMPT) THEN
          CALL ASCLIG(MAILLA, NOMRES, ICMD, ACOU, ASEP, RCIN, FLAG, 
     +                GEOM, RCCMAT, IERUSR)
        END IF
C
      ELSE IF (FISS) THEN
C
C       --- POST TRAITEMENT FISSURE :  CALCUL DE G ----
C
        CALL ASCTHE(MAILLA, NTHET, ZR(IRTHET), THERM, CHMETH, CHPRES,
     +              AFFMAT, MODELE, NOMRES, NOFISS, FAXI, NPRE, NCOIN,
     +              NCOEL, RELCO, LRELCO, ICMD, IERUSR)
      END IF
C
C     --- COMMANDE IMPR_RESU  ---
C
      IF ( IMPR ) THEN
C
         ICMD = ICMD + 1
         CALL SMDCMD ( ICMD, ' ', 'IMPR_RESU', IERUSR )
           CALL PUTVID ( 'MODELE'    , 1, MODELE, IERUSR )
           CALL SMDMCF ( 'RESU', IERUSR )
             CALL PUTVID ( 'MAILLAGE', 1, MAILLA, IERUSR )
             CALL PUTVID ( 'RESULTAT', 1, NOMRES, IERUSR )
             CALL PUTVTX ( 'FORMAT'  , 1, FORMAT, LFORMA, IERUSR)
             IF (FORMAT(1:5).EQ.'IDEAS') THEN
               CALL PUTVIS ( 'VERSION' , 1, VERSIO, IERUSR )
             ELSEIF (FORMAT(1:6).EQ.'CASTEM') THEN
               CALL PUTVIS ( 'NIVE_GIBI' , 1, NIVGIB, IERUSR )
             ENDIF
           CALL SMFMCF ( IERUSR )
           IF (THERM) THEN
             CALL SMDMCF ( 'RESU', IERUSR )
               CALL PUTVID ( 'RESULTAT', 1, RESUTH , IERUSR )
               CALL PUTVTX ( 'FORMAT'  , 1, FORMAT, LFORMA, IERUSR)
               IF (FORMAT(1:5).EQ.'IDEAS') THEN
                  CALL PUTVIS ( 'VERSION' , 1, VERSIO, IERUSR )
               ELSEIF (FORMAT(1:6).EQ.'CASTEM') THEN
                  CALL PUTVIS ( 'NIVE_GIBI' , 1, NIVGIB, IERUSR )
               ENDIF
             CALL SMFMCF ( IERUSR )
           END IF
         CALL SMFCMD ( IERUSR )
      ENDIF
C
 9998 CONTINUE
      IF ( IERUSR .GT. 0 ) THEN
         CALL UTMESS('E',NOMCMD,'ERREURS CONSTATEES DANS LA MACRO')
         IER = IER + IERUSR
      ENDIF
C
 9999 CONTINUE
C
C     --"MENAGE":
C     -----------
      CALL JEDETC('V','&&OPS020',1)
      CALL JEDEMA()
      END
