      SUBROUTINE OPS019 ( ICMD , ICOND , IER )
      IMPLICIT   NONE
      INTEGER             ICMD , ICOND , IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 27/05/2003   AUTEUR D6BHHJP J.P.LEFEBVRE 
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
C                   MACR_ASCOUF_MAIL
C
C     ON UTILISE LES UNITES :
C     POUR GIBI :
C         70 : POUR ECRIRE LES DONNEES GIBI DE LA PROCEDURE
C
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
      REAL*8       R1,R2,E,OR,AZ,DC
      CHARACTER*8  POS
      COMMON / ASCFOR / R1,R2,E,OR,AZ,DC
      COMMON / ASCFOC / POS
C
      INTEGER      N1, UNITD, INFO, IERUSR, LLOGIE, NBOL, I4,
     +             LFORMA, LFICHI, NF, VERSIO, UNITS, UNITEF, I8
      INTEGER      NBEP, NBEL, NBEC, NBSEP, ISEP, NT, NS, NC, NOCC,
     +             NZMAX, NZONEC, NZONEL, IPOS1, IPOS2, NBXS, LRM
      INTEGER      ICIRC, ILONC, IPROC, ISLC, IBETC, ISCC, IPHIC, IPOS,
     +             INBEL, INBEC, IEVID, ITYPE, ICIRP, ILONP, ISCP, ISLP,
     +             INDBG, INDBD, IBG, IBD, INDBI, INDBS, IBI, IBS, IDNX,
     +             IDNY, INDSEX, INDSEY, IDENL, IDENC, IABSC1, IABSC2,
     +             IORDO1, IORDO2, ICORXG, ICORXD, ICORYI, ICORYS,INIVG,
     +             ITAMP, I16, I, J, K, INBXS, IFORM, ILFOR,IVERS,IPCL,
     +             IFICH, ILFIC, IUNIT, NIMP, INF, UNITP, NIVMAG,
     +             NIVGIB, NBTRAN
      REAL*8       RM ,RC, ALPHA, EP1, EP2, EPI, TETA1, TETA2, LTRAN,
     +             LDEFAU, LTCHAR, LTCLIM, SUREP, PI, R8PI, AXEC, AXEL,
     +             PROF, SL, BETA, SC, PHI, ORIEN, AZIM, FLONG, FPROF,
     +             DGAXEC, SF, RC0, RC2, RC3, PAXEP, GAXEP, SFP, EPSI,
     +             DEXT, LAMOR, RM2, PRECIS
      LOGICAL      FISS, SSEP, DSF, SSEPV2, IMPR
      CHARACTER*2  SECT(3)
      CHARACTER*4  TYPELE,CAR6(8)
      CHARACTER*5  CAR,CAR2
      CHARACTER*5  CAR3(8)
      CHARACTER*6  CAR4(8),CAR5(8)
      CHARACTER*8  NOMRES, TYPBOL, SYME ,GEOM, TYPE, EVID, POSIT,
     +             GROUMA(4), K8B1, DONGIB, NOPAR1, NOPAR2,
     +             MODELE, PROGIB, TRANEP, FAXI, GROUNO(2), K8B2,
     +             CRITER, MAPROV, NOP1, NOP2
      CHARACTER*16 TYPRES, NOMCMD, FORMAT, FICHIE, MOTCL,
     +             K16B1, K16B3, K16B4
      CHARACTER*128 REP, LOGIEL

C     ------------------------------------------------------------------
      DATA TYPBOL / '        '/
      DATA    CAR3/'IFDRO','IEXDR','IEXTR','IEXGA',
     &             'IFGAU','IINGA','IINTR','IINDR'/
      DATA    CAR4/'NOFDRO','NOEXDR','NOEXTR','NOEXGA',
     &             'NOFGAU','NOINGA','NOINTR','NOINDR'/
      DATA    CAR5/'NEFDRO','NEEXDR','NEEXTR','NEEXGA',
     &             'NEFGAU','NEINGA','NEINTR','NEINDR'/
      DATA    CAR6/'FDRO','EXDR','EXTR','EXGA',
     &             'FGAU','INGA','INTR','INDR'/
      DATA SECT    / 'MI','TU','GV'/
      DATA CRITER    / 'RELATIF'/
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      IER = 0
      IF ( ICOND .NE. -1 ) GOTO 9999
C     ------------------------------------------------------------------
C
      PI = R8PI()
      PRECIS = 1.0D-2
C
      CALL GETRES ( NOMRES, TYPRES, NOMCMD )
C
C     --- L'EXECUTABLE ---
C
      CALL GETFAC ( 'EXEC_MAILLAGE', N1 )
C
      CALL GETVTX ( 'EXEC_MAILLAGE', 'LOGICIEL'  , 1,1,1, LOGIEL,N1)
      CALL GETVIS ( 'EXEC_MAILLAGE', 'UNITE_DATG', 1,1,1, UNITD, N1)
      CALL GETVIS ( 'EXEC_MAILLAGE', 'UNITE_MGIB', 1,1,1, UNITS, N1)
      CALL GETVIS ( 'EXEC_MAILLAGE', 'NIVE_GIBI' , 1,1,1, NIVMAG, N1)
C
C     --- TYPE DE MAILLE ---
C
      CALL GETVTX ( ' ', 'TYPE_ELEM' , 1,1,1, TYPELE, N1)
C
C     --- CARACTERISTIQUES DU COUDE ---
C
      SUREP = 0.0D0
      CALL GETVTX ( 'COUDE', 'TRANSFORMEE' , 1,1,1, GEOM   , N1 )
      CALL GETVR8 ( 'COUDE', 'ANGLE'  , 1,1,1, ALPHA , N1 )
      CALL GETVR8 ( 'COUDE', 'R_CINTR', 1,1,1, RC    , N1 )
      CALL GETVR8 ( 'COUDE', 'L_TUBE_P1' , 1,1,1, LTCHAR , N1 )
      CALL GETVR8 ( 'COUDE', 'L_TUBE_P2' , 1,1,1, LTCLIM , N1 )
      CALL GETVR8 ( 'COUDE', 'SUR_EPAIS'     , 1,1,1, SUREP   , N1 )
      CALL GETVIS ( 'COUDE', 'NB_ELEM_EPAIS' , 1,1,1, NBEP    , N1 )
      CALL GETVTX ( 'COUDE', 'BOL_P2'        , 1,1,1, TYPBOL  , NBOL )
      CALL GETVTX ( 'COUDE', 'SYME'        , 1,1,1, SYME   , N1 )
C
      IF (SYME(1:5) .EQ. 'QUART') THEN
        MAPROV = NOMRES
      ELSE
        CALL GCNCON ('.', MAPROV)
      ENDIF
C
      CALL GETVTX ( 'COUDE', 'TRAN_EPAIS'  , 1,1,1, TRANEP , N1 )
      IF ((GEOM .EQ. 'COUDE') .OR. (TRANEP .EQ. 'NON')) THEN
        NBTRAN = 0
        CALL GETVR8 ( 'COUDE', 'DEXT'   , 1,1,1, DEXT  , N1 )
        CALL GETVR8 ( 'COUDE', 'EPAIS'  , 1,1,1, EP1   , N1 )
        EP2 = EP1
        EPI = 0.D0
        TETA1 = 0.D0
        TETA2 = 0.D0
        LTRAN = 0.D0
      ELSE
        NBTRAN = 1
C
        IF (GEOM .EQ. 'COUDE') THEN
          CALL UTMESS ('F',NOMCMD,'VOUS NE POUVEZ APPLIQUER UNE '
     +                 //'TRANSITION D''EPAISSEUR QUE SUR UN '
     +                 //'TUBE DROIT')
        ENDIF
C
        IF (SYME(1:6) .NE. 'ENTIER') THEN
          CALL UTMESS ('F',NOMCMD,'LES QUART ET DEMI STRUCTURE '
     +                 //'NE PEUVENT ETRE REALISEES '          
     +                 //'SUR UN MODELE COMPORTANT UNE TRANSITION '
     +                 //'D''EPAISSEUR')
        ENDIF
C
        CALL GETVR8 ( 'COUDE', 'DEXT_T1'     , 1,1,1, DEXT  , N1 )
        CALL GETVR8 ( 'COUDE', 'EPAIS_T1'    , 1,1,1, EP1   , N1 )
        CALL GETVR8 ( 'COUDE', 'EPAIS_T2'    , 1,1,1, EP2   , N1 )
        CALL GETVR8 ( 'COUDE', 'ANGL_TETA1' , 1,1,1, TETA1 , N1 )
C
        EPI = 0.D0
        TETA2 = 0.D0
        CALL GETVR8 ( 'COUDE', 'ANGL_TETA2' , 1,1,0, TETA2 , N1 )
        IF (N1.NE.0) THEN
          NBTRAN = 2
          CALL GETVR8 ( 'COUDE', 'ANGL_TETA2'    , 1,1,1, TETA2 , N1 )
          CALL GETVR8 ( 'COUDE', 'EPAIS_TI'       , 1,1,1, EPI   , N1 )
        END IF
C
        CALL GETVR8 ( 'COUDE', 'ABSC_CURV_TRAN' , 1,1,0, LTRAN , N1 )
        IF (N1.NE.0) THEN
          CALL GETVR8 ( 'COUDE', 'ABSC_CURV_TRAN' , 1,1,1, LTRAN , N1 )
        ELSE
          CALL GETVR8 ( 'COUDE', 'POSI_ANGU_TRAN' , 1,1,1, LTRAN , N1 )
          LTRAN = LTRAN * RC * PI / 180.D0
        END IF
      ENDIF
C
      RM = (DEXT-EP1)/2.D0
      RM2 = RM + (EP2-EP1)/2.D0
      R1 = RC
      R2 = RM
      E  = EP1
C
      IF (SYME(1:6).NE.'ENTIER'.AND.(LTCHAR.NE.LTCLIM)) THEN
        CALL UTMESS('F',NOMCMD,'LES DEUX EMBOUTS DOIVENT ETRE'//
     +              ' DE MEME LONGUEUR POUR LES CAS DE SYMETRIE !')
      END IF
C
      LAMOR = 3.D0/2.D0 * SQRT( RM*RM*RM / EP1)
      IF (LTCHAR.LT.LAMOR) THEN
        CALL UTDEBM('A','OPS019','LONGUEUR D''EMBOUT P1 INFERIEURE'
     +               // ' A LA LONGUEUR D''AMORTISSEMENT')
        CALL UTIMPR('L','LONGUEUR AMORTISSEMENT :',1,LAMOR)
        CALL UTFINM()
      END IF
C
      LAMOR = 3.D0/2.D0 * SQRT( RM2*RM2*RM2 / EP2)
      IF ((LTCLIM.LT.LAMOR).AND.(NBOL.EQ.0)) THEN
        CALL UTDEBM('A','OPS019','LONGUEUR D''EMBOUT P2 INFERIEURE'
     +               // ' A LA LONGUEUR D''AMORTISSEMENT')
        CALL UTIMPR('L','LONGUEUR AMORTISSEMENT :',1,LAMOR)
        CALL UTFINM()
      END IF
C
      IF (TYPBOL(1:2).EQ.'GV') THEN
        CALL UTMESS('A',NOMCMD,'LA CONDITION AUX LIMITES RACCORD'
     +       //' 3D-POUTRE APPLIQUEE AVEC LA MACRO DE CALCUL'
     +       //' ASCOUF N''EST PAS LICITE AVEC UN EMBOUT'
     +       //' DE TYPE CONIQUE')
      END IF
C
C     --- CARACTERISTIQUES DE LA FISSURE ---
C
      FISS = .FALSE.
      CALL GETFAC('FISS_COUDE',NOCC)
      IF (NOCC.NE.0) THEN
C
       IF (NBEP.NE.3) THEN
        CALL UTMESS('A',NOMCMD,'LE NOMBRE D''ELEMENTS DANS L'''
     +       //'EPAISSEUR DU COUDE N''EST PAS PARAMETRABLE POUR'
     +       //' UN COUDE AVEC FISSURE : MOT-CLE "NB_ELEM_EPAIS"'
     +       //' IGNORE')
       END IF
C
       FISS = .TRUE.
       CALL GETVR8 ( 'FISS_COUDE', 'PROFONDEUR' ,1,1,1, FPROF , N1 )
       CALL GETVTX ( 'FISS_COUDE', 'AXIS'       ,1,1,1, FAXI , N1 )
       CALL GETVR8 ( 'FISS_COUDE', 'LONGUEUR'   ,1,1,0, FLONG , N1 )
       IF ((FAXI.EQ.'NON').AND.(N1.EQ.0)) THEN
         CALL UTMESS ('F',NOMCMD,'POUR LES FISSURES NON AXISYMETRIQUES'
     +                //' LA LONGUEUR DOIT ETRE SPECIFIEE')
       ENDIF
       IF ((FAXI.EQ.'OUI').AND.(N1.NE.0)) THEN
         CALL UTMESS ('A',NOMCMD,'LA FISSURE EST AXISYMETRIQUE : ON NE'
     +                //' TIENT PAS COMPTE DE LA LONGUEUR SPECIFIEE')
       ENDIF
       IF (N1.NE.0) THEN
         CALL GETVR8 ( 'FISS_COUDE', 'LONGUEUR'   ,1,1,1, FLONG , N1 )
       ENDIF
       IF (FAXI.EQ.'OUI') THEN
C ON PREND UNE MARGE DE SECURITE A CAUSE DES MODIFS DANS ASCFIS
         FLONG = 2.D0 * PI * (RM + EP1)
       ENDIF
       CALL GETVR8 ( 'FISS_COUDE', 'ABSC_CURV'  ,1,1,0, SF    , N1 )
       IF (N1.NE.0) THEN
         DSF = .TRUE.
         CALL GETVR8 ( 'FISS_COUDE', 'ABSC_CURV' ,1,1,1, SF  , N1 )
         LDEFAU = SF
         BETA = 0.0D0
       ELSE
         SF = 0.0D0
         DSF = .FALSE.
         CALL GETVR8 ( 'FISS_COUDE', 'POSI_ANGUL' ,1,1,1, BETA, N1 )
         LDEFAU = BETA * RC * PI / 180.D0
       END IF
       CALL GETVR8 ( 'FISS_COUDE', 'AZIMUT' ,1,1,1, AZIM  , N1 )
       CALL GETVR8 ( 'FISS_COUDE', 'ORIEN'  ,1,1,1, ORIEN , N1 )
       CALL GETVTX ( 'FISS_COUDE', 'FISSURE'   ,1,1,1, POSIT , N1 )
       CALL GETVIS ( 'FISS_COUDE', 'NB_TRANCHE'  ,1,1,1, NT , N1 )
       CALL GETVIS ( 'FISS_COUDE', 'NB_SECTEUR'  ,1,1,1, NS , N1 )
       CALL GETVIS ( 'FISS_COUDE', 'NB_COURONNE' ,1,1,1, NC , N1 )
       CALL GETVR8 ( 'FISS_COUDE', 'RAYON_TORE'   ,1,1,0, RC0 , N1 )
       IF (N1.NE.0) THEN
        CALL GETVR8 ( 'FISS_COUDE', 'RAYON_TORE'   ,1,1,1, RC0 , N1 )
       ELSE
        RC0 = 0.D0
       END IF
       CALL GETVR8 ( 'FISS_COUDE', 'COEF_MULT_RC2'  ,1,1,0, RC2 , N1 )
       IF (N1.NE.0) THEN
       CALL GETVR8 ( 'FISS_COUDE', 'COEF_MULT_RC2'  ,1,1,1, RC2 , N1 )
       ELSE
        RC2 = 0.D0
       END IF
       CALL GETVR8 ( 'FISS_COUDE', 'COEF_MULT_RC3'  ,1,1,0, RC3 , N1 )
       IF (N1.NE.0) THEN
         CALL GETVR8 ( 'FISS_COUDE', 'COEF_MULT_RC3'  ,1,1,1, RC3 , N1 )
       ELSE
         RC3 = 0.D0
       END IF
       CALL GETVR8 ( 'FISS_COUDE', 'ANGL_OUVERTURE'  ,1,1,1,EPSI , N1 )
       OR  = ORIEN
       AZ  = AZIM
       POS = POSIT
       DGAXEC = FLONG/2.D0
       DC  = DGAXEC
C
       IF ((ORIEN.NE.90.D0).AND.(NBTRAN.NE.0)) THEN
         CALL UTMESS ('F',NOMCMD,'AVEC UNE TRANSITION D''EPAISSEUR '
     +              //'LA FISSURE DOIT OBLIGATOIREMENT ETRE TRANSVERSE')
       END IF
C
       IF ((ORIEN.NE.90.D0).AND.(SYME(1:6).NE.'ENTIER')) THEN
         CALL UTMESS ('F',NOMCMD,'L''ORIENTATION DE LA FISSURE DOIT '
     +              //'ETRE TRANSVERSE (ORIEN : 90.) POUR MODELISER '
     +              //'UN QUART OU UNE DEMI STRUCTURE ')
       END IF
C
       IF ((ORIEN.NE.90.D0).AND.(FAXI.EQ.'OUI')) THEN
         CALL UTMESS ('F',NOMCMD,'LA FISSURE EST AXISYMETRIQUE : SON '
     +              //'ORIENTATION DOIT ETRE TRANSVERSE (ORIEN : 90.)')
       END IF
C
      END IF
C
C     --- CARACTERISTIQUES DES SOUS-EPAISSEURS ---
C
      SSEP = .FALSE.
      SSEPV2 = .FALSE.
      MOTCL = 'SOUS_EPAIS_COUDE'
      CALL GETFAC(MOTCL,NBSEP)
      IF (NBSEP.EQ.0) THEN
        MOTCL = 'SOUS_EPAIS_MULTI'
        CALL GETFAC(MOTCL,NBSEP)
        IF (NBSEP.NE.0) SSEP = .TRUE.
      ELSE
        SSEPV2 = .TRUE.
      END IF

      IF ((SSEP) .AND. (NBTRAN.NE.0)) THEN
        CALL UTMESS ('F',NOMCMD,'IL NE PEUT PAS Y AVOIR PLUSIEURS '
     +                //'SOUS-EPAISSEURS EN MEME TEMPS QU''UNE '
     +                //'TRANSITION D''EPAISSEUR : SI UNE SEULE '
     +                //'SOUS-EPAISSEUR, UTILISER SOUS_EPAIS_COUDE')
      ENDIF
      IF ((.NOT.SSEPV2).AND.(.NOT.FISS).AND.(NBTRAN.NE.0)) THEN
        CALL UTMESS ('F',NOMCMD,'AVEC UNE TRANSITION D''EPAISSEUR '
     +                //'IL DOIT OBLIGATOIREMENT Y AVOIR UN DEFAUT : '
     +                //'SOIT UNE FISSURE, SOIT UNE SOUS-EPAISSEUR')
      ENDIF

      IF (NBSEP.NE.0) THEN
C
         IF (NBSEP.NE.1.AND.SYME(1:6).NE.'ENTIER') THEN
           CALL UTMESS('F',NOMCMD,'NE MODELISER QU''UNE SEULE'//
     +                 ' SOUS-EPAISSEUR POUR UN QUART OU DEMI-COUDE !')
         END IF
         SSEP = .TRUE.
         AZIM = 90.D0
         CALL WKVECT('&&OPS019.TYPE','V V K8',NBSEP,ITYPE)
         CALL WKVECT('&&OPS019.CIRC','V V R' ,NBSEP,ICIRC)
         CALL WKVECT('&&OPS019.LONC','V V R' ,NBSEP,ILONC)
         CALL WKVECT('&&OPS019.PROC','V V R' ,NBSEP,IPROC)
         CALL WKVECT('&&OPS019.SLC' ,'V V R' ,NBSEP,ISLC)
         CALL WKVECT('&&OPS019.IPCL','V V I' ,NBSEP,IPCL)
         CALL WKVECT('&&OPS019.BETC','V V R' ,NBSEP,IBETC)
         CALL WKVECT('&&OPS019.SCC' ,'V V R' ,NBSEP,ISCC)
         CALL WKVECT('&&OPS019.PHIC','V V R' ,NBSEP,IPHIC)
         CALL WKVECT('&&OPS019.POS' ,'V V K8',NBSEP,IPOS)
         CALL WKVECT('&&OPS019.EVID','V V K8',NBSEP,IEVID)
C
         CALL WKVECT('&&OPS019.CIRP','V V R' ,NBSEP,ICIRP)
         CALL WKVECT('&&OPS019.LONP','V V R' ,NBSEP,ILONP)
         CALL WKVECT('&&OPS019.SCP' ,'V V R' ,NBSEP,ISCP)
         CALL WKVECT('&&OPS019.SLP' ,'V V R' ,NBSEP,ISLP)
         CALL WKVECT('&&OPS019.NBEL','V V I' ,NBSEP,INBEL)
         CALL WKVECT('&&OPS019.NBEC','V V I' ,NBSEP,INBEC)
         CALL WKVECT('&&OPS019.NBXS','V V I' ,NBSEP,INBXS)
C
      DO 100 ISEP = 1,NBSEP
C
        CALL GETVTX( MOTCL, 'TYPE'     , ISEP,1,1, TYPE   ,N1 )
        CALL GETVR8( MOTCL, 'AXE_CIRC' , ISEP,1,1, AXEC  ,N1 )
        IF (N1.NE.0.AND.TYPE.EQ.'AXIS') THEN
          CALL UTMESS('F',NOMCMD,'VOUS NE POUVEZ DECLARER LA SOUS-'
     +                 //'EPAISSEUR COMME AXISYMETRIQUE ET DONNER '
     +                 //'UNE TAILLE D''AXE CIRCONFERENTIEL !')
        END IF
        IF (N1.EQ.0.AND.TYPE.EQ.'ELLI') THEN
          CALL UTMESS('F',NOMCMD,'VOUS DEVEZ DONNER UNE TAILLE D''AXE'
     +                 //' CIRCONFERENTIEL POUR UNE SOUS-EPAISSEUR DE'
     +                 //' TYPE ELLIPTIQUE !')
        END IF
        CALL GETVR8( MOTCL, 'AXE_LONGI' , ISEP,1,1, AXEL  ,N1 )
        CALL GETVR8( MOTCL, 'PROFONDEUR',ISEP,1,1, PROF   ,N1 )
C
        CALL GETVR8( MOTCL, 'POSI_CURV_LONGI' ,ISEP,1,0, SL ,N1 )
        ZI(IPCL+ISEP-1) = N1
        IF (N1.NE.0) THEN
         CALL GETVR8(MOTCL, 'POSI_CURV_LONGI' ,ISEP,1,1, SL ,N1 )
          IF (SL.GT.ALPHA*RC*PI/180.D0) THEN
           CALL UTDEBM('F','OPS019','VALEUR HORS DOMAINE DE VALIDITE')
           CALL UTIMPI('L','SOUS-EPAISSEUR NUMERO :',1,ISEP)
           CALL UTIMPR('L','ABSCISSE CURV. LONGIT. :',1,SL)
           CALL UTIMPR('L','VALEUR MAXIMALE AUTORISEE :'
     +                    ,1,ALPHA*RC*PI/180.D0)
           CALL UTFINM()
           GOTO 9999
          END IF
          LDEFAU = SL + AXEL/2.D0
          BETA = 0.0D0
        ELSE
         CALL GETVR8(MOTCL, 'POSI_ANGUL' ,ISEP,1,1, BETA ,N1 )
         IF ((BETA.LT.0.D0).OR.(BETA.GT.ALPHA)) THEN
          CALL UTDEBM('F','ASCCOH','VALEUR HORS DOMAINE DE VALIDITE')
          CALL UTIMPI('L','SOUS-EPAISSEUR NUMERO :',1,ISEP)
          CALL UTIMPR('L','POSITION ANGULAIRE CENTRE SOUS-EP :',1,BETA)
          CALL UTIMPR('L','VALEUR LIMITE AUTORISEE :',1,ALPHA)
          CALL UTFINM()
          GOTO 9999
         END IF
         LDEFAU = (BETA*RC*PI/180.D0) + AXEL/2.D0
        END IF
C
        CALL GETVR8(MOTCL, 'POSI_CURV_CIRC' ,ISEP,1,0, SC ,N1 )
        IF (N1.NE.0) THEN
         CALL GETVR8(MOTCL, 'POSI_CURV_CIRC' ,ISEP,1,1, SC ,N1 )
          IF (SC.GT.(2*PI*RM)) THEN
           CALL UTDEBM('F','OPS019','VALEUR HORS DOMAINE DE VALIDITE')
           CALL UTIMPI('L','SOUS-EPAISSEUR NUMERO :',1,ISEP)
           CALL UTIMPR('L','ABSCISSE CURV. CIRCONF. :',1,SC)
           CALL UTIMPR('L','VALEUR MAXIMALE AUTORISEE :'
     +                    ,1,2*PI*RM)
           CALL UTFINM()
           GOTO 9999
         END IF
         IF (TYPE.EQ.'AXIS'.AND.(SC.NE.PI*RM)) THEN
          CALL UTMESS('F',NOMCMD,'LE CENTRE D''UNE SOUS-EPAISSEUR'
     +               //' AXISYMETRIQUE EST IMPOSE EN INTRADOS (PI*RM)')
         END IF
        ELSE
         CALL GETVR8(MOTCL, 'AZIMUT' ,ISEP,1,1, PHI ,N1)
         IF ((PHI.LT.0.D0).OR.(PHI.GT.360.D0)) THEN
          CALL UTDEBM('F','OPS019','VALEUR HORS DOMAINE DE VALIDITE')
          CALL UTIMPI('L','SOUS-EPAISSEUR NUMERO :',1,ISEP)
          CALL UTIMPR('L','AZIMUT CENTRE SOUS-EP :',1,AZIM)
          CALL UTIMPR('L','VALEUR LIMITE AUTORISEE :',1,360.D0)
          CALL UTFINM()
          GOTO 9999
         END IF
         IF (TYPE.EQ.'AXIS'.AND.(PHI.NE.180.D0)) THEN
          CALL UTMESS('F',NOMCMD,'LE CENTRE D''UNE SOUS-EPAISSEUR'
     +               //' AXISYMETRIQUE EST IMPOSE EN INTRADOS :'
     +               //' L''AZIMUT EST FIXE A 180 DEGRES')
         END IF
        END IF
C
        CALL GETVTX(MOTCL, 'SOUS_EPAIS'    ,ISEP,1,1, POSIT ,N1 )
        CALL GETVIS(MOTCL, 'NB_ELEM_LONGI' ,ISEP,1,1, NBEL , N1 )
        CALL GETVIS(MOTCL, 'NB_ELEM_CIRC'  ,ISEP,1,1, NBEC , N1 )
C
        IF (SSEPV2) THEN
         CALL GETVIS(MOTCL, 'NB_ELEM_RADI'  ,ISEP,1,1, NBXS , N1 )
         IF (NBXS.LT.3) THEN
           CALL UTMESS('F',NOMCMD,'LE NOMBRE D''ELEMENTS DANS L'''
     +               //'EPAISSEUR DU BLOC DOIT ETRE DE 3 AU MINIMUM')
         END IF
         ZI(INBXS  +ISEP-1) = NBXS
        END IF
C
        CALL GETVTX(MOTCL, 'EMPREINTE'     ,ISEP,1,1, EVID , N1 )
C
        ZK8(ITYPE +ISEP-1) = TYPE
        ZR(ICIRC  +ISEP-1) = AXEC
        ZR(ILONC  +ISEP-1) = AXEL
        ZR(IPROC  +ISEP-1) = PROF
        ZR(ISLC   +ISEP-1) = SL
        ZR(IBETC  +ISEP-1) = BETA
        ZR(ISCC   +ISEP-1) = SC
        ZR(IPHIC  +ISEP-1) = PHI
        ZK8(IPOS  +ISEP-1) = POSIT
        ZI(INBEL  +ISEP-1) = NBEL
        ZI(INBEC  +ISEP-1) = NBEC
        ZK8(IEVID +ISEP-1) = EVID
C
100   CONTINUE
C
      IF (SSEPV2.AND.NBEP.NE.3) THEN
        CALL UTMESS('A',NOMCMD,'LE NOMBRE D''ELEMENTS DANS L'''
     +       //'EPAISSEUR DU COUDE N''EST PAS PARAMETRABLE POUR'
     +       //' LA VERSION 2 DE LA PROCEDURE DE PLAQUE AVEC SOUS'
     +       //'-EPAISSEUR : MOT-CLE "NB_ELEM_EPAIS" IGNORE')
      END IF
C
      END IF
C
C     --- VERIFICATION DE COHERENCES
C
      CALL ASCCOH(ALPHA,RM,RC,NBTRAN,EP1,EP2,EPI,TETA1,TETA2,LTRAN,
     +            LDEFAU,SUREP,AZIM,BETA,NC,NT,NS,DSF,SF, FISS )
C
C     --- CALCUL TAILLE INITIALE DES DEFAUTS SUR LA PLAQUE ---
C
      IF (FISS) THEN
         CALL ASCFIS(ALPHA,RM,RC,EP1,SUREP,GEOM,FPROF,DGAXEC,AZIM,
     +                POSIT,SF,DSF,BETA,ORIEN,PAXEP,GAXEP,SFP)
C
      ELSE IF (SSEP) THEN
         CALL ASCSEP(ALPHA,RM,RC,EP1,GEOM,SYME,NBSEP,ITYPE,ICIRC,ILONC,
     +               ISLC,IBETC,ISCC,IPHIC,IPOS,ICIRP,ILONP,ISCP,ISLP,
     +               SSEPV2)
C
         NZMAX = 30
         CALL WKVECT('&&OPS019.INDBG','V V I'   ,NZMAX,INDBG)
         CALL WKVECT('&&OPS019.INDBD','V V I'   ,NZMAX,INDBD)
         CALL WKVECT('&&OPS019.BG','V V R'      ,NZMAX,IBG)
         CALL WKVECT('&&OPS019.BD','V V R'      ,NZMAX,IBD)
         CALL WKVECT('&&OPS019.INDBI','V V I'   ,NZMAX,INDBI)
         CALL WKVECT('&&OPS019.INDBS','V V I'   ,NZMAX,INDBS)
         CALL WKVECT('&&OPS019.BI','V V R'      ,NZMAX,IBI)
         CALL WKVECT('&&OPS019.BS','V V R'      ,NZMAX,IBS)
         CALL WKVECT('&&OPS019.DNX','V V R'     ,2*NZMAX,IDNX)
         CALL WKVECT('&&OPS019.DNY','V V R'     ,2*NZMAX,IDNY)
         CALL WKVECT('&&OPS019.INDSEPX','V V I' ,NZMAX,INDSEX)
         CALL WKVECT('&&OPS019.INDSEPY','V V I' ,NZMAX,INDSEY)
         CALL WKVECT('&&OPS019.IABSC1','V V I'  ,2*NBSEP,IABSC1)
         CALL WKVECT('&&OPS019.IABSC2','V V I'  ,3*NBSEP,IABSC2)
         CALL WKVECT('&&OPS019.COORXG','V V R'  ,NBSEP,ICORXG)
         CALL WKVECT('&&OPS019.COORXD','V V R'  ,NBSEP,ICORXD)
         CALL WKVECT('&&OPS019.TAMPON','V V R'  ,2*NBSEP,ITAMP)
         CALL WKVECT('&&OPS019.IORDO1','V V I'  ,NBSEP,IORDO1)
         CALL WKVECT('&&OPS019.IORDO2','V V I'  ,2*NBSEP,IORDO2)
         CALL WKVECT('&&OPS019.COORYI','V V R'  ,NBSEP,ICORYI)
         CALL WKVECT('&&OPS019.COORYS','V V R'  ,NBSEP,ICORYS)
         CALL WKVECT('&&OPS019.DENL','V V R' ,NBSEP,IDENL)
         CALL WKVECT('&&OPS019.DENC','V V R' ,NBSEP,IDENC)
C
         DO 400 ISEP = 1,NBSEP
           ZR(IDENL+ISEP-1) = ZR(ILONP+ISEP-1)
     +                         /ZI(INBEL+ISEP-1)*180/(PI*RC)
           ZR(IDENC+ISEP-1) = ZR(ICIRP+ISEP-1)
     +                        /ZI(INBEC+ISEP-1)*180/(PI*RM)
  400    CONTINUE
C
         IF (SYME(1:5).EQ.'QUART') THEN
C
C         QUART DE STRUCTURE
C
           CALL ASCSYM(RM,RC,ALPHA,LTCHAR,LTCLIM,NBSEP,ISCP,ILONC,
     +                   ISLP,ICIRP,ILONP,ZR(IDENC),ZR(IDENL),
     +                 ZI(INBEC),ZI(INBEL),
     +                 ZI(INDBG),ZI(INDBD),ZR(IBG),ZR(IBD),ZI(INDBI),
     +                 ZI(INDBS),ZR(IBI),ZR(IBS),ZR(IDNX),ZR(IDNY),
     +                 ZI(INDSEX),ZI(INDSEY),ZI(IABSC1),ZI(IABSC2),
     +                 ZI(IORDO1),ZI(IORDO2),ZR(ICORXG),ZR(ICORXD),
     +                 ZR(ICORYI),ZR(ICORYS),ZR(ITAMP),NZONEC,NZONEL,
     +                 NZMAX)
         ELSE
C
C         DEMI-STRUCTURE OU ENTIERE
C
           CALL ASCPRE(RM,RC,ALPHA,LTCHAR,LTCLIM,NBSEP,ISCP,ILONC,
     +                 ISLP,ICIRP,ILONP,ZR(IDENC),ZR(IDENL),
     +                 ZI(INBEC),ZI(INBEL),
     +                 ZI(INDBG),ZI(INDBD),ZR(IBG),ZR(IBD),ZI(INDBI),
     +                 ZI(INDBS),ZR(IBI),ZR(IBS),ZR(IDNX),ZR(IDNY),
     +                 ZI(INDSEX),ZI(INDSEY),ZI(IABSC1+1),ZI(IABSC2),
     +                 ZI(IORDO1),ZI(IORDO2),ZR(ICORXG),ZR(ICORXD),
     +                 ZR(ICORYI),ZR(ICORYS),ZR(ITAMP),NZONEC,NZONEL,
     +                 NZMAX,SYME)
         END IF
C
      END IF
C
C     --- INFO ---
C
      CALL GETVIS ( ' ', 'INFO' , 1,1,1, INFO  , N1 )
C
C     --- IMPRESSION ---
C
      IMPR = .FALSE.
      CALL GETFAC ( 'IMPRESSION', NIMP )
      IF ( NIMP .NE. 0 ) THEN
         IMPR = .TRUE.
C
         CALL WKVECT('&&OPS019.FORM','V V K16',NIMP,IFORM)
         CALL WKVECT('&&OPS019.ILFO','V V I'  ,NIMP,ILFOR)
         CALL WKVECT('&&OPS019.IVER','V V I'  ,NIMP,IVERS)
         CALL WKVECT('&&OPS019.INIV','V V I'  ,NIMP,INIVG)
         CALL WKVECT('&&OPS019.IFIC','V V K16',NIMP,IFICH)
         CALL WKVECT('&&OPS019.ILFI','V V I'  ,NIMP,ILFIC)
         CALL WKVECT('&&OPS019.IUNI','V V I'  ,NIMP,IUNIT)
         CALL WKVECT('&&OPS019.INF','V V I'  ,NIMP,INF)
C
         DO 10 I=1, NIMP
C
           CALL GETVTX ( 'IMPRESSION', 'FORMAT' , I,1,1, FORMAT, N1 )
           CALL GETLTX ( 'IMPRESSION', 'FORMAT' , I,1,1, LFORMA, N1 )
           CALL GETVIS ( 'IMPRESSION', 'VERSION', I,1,1, VERSIO, N1 )
           CALL GETVIS ( 'IMPRESSION', 'NIVE_GIBI', I,1,1, NIVGIB, N1 )
           CALL GETVTX ( 'IMPRESSION', 'FICHIER', I,1,1, FICHIE, NF )
           CALL GETLTX ( 'IMPRESSION', 'FICHIER', I,1,1, LFICHI, N1 )
           CALL GETVIS ( 'IMPRESSION', 'UNITE'  , I,1,1, UNITEF, N1 )
           ZI(INF+I-1)     = NF
           ZK16(IFORM+I-1) = FORMAT
           ZI(ILFOR+I-1)   = LFORMA
           ZI(IVERS+I-1)   = VERSIO
           ZI(INIVG+I-1)   = NIVGIB
           ZK16(IFICH+I-1) = FICHIE
           ZI(ILFIC+I-1)   = LFICHI
           ZI(IUNIT+I-1)   = UNITEF
C
 10     CONTINUE
C
      ENDIF
C
C     ------------------------------------------------------------------
C
C     --- DESTRUCTION DE LA COMMANDE COURANTE --
      IERUSR = 0
      CALL SMCDEL(ICMD,0,IERUSR)
      ICMD   = ICMD - 1
C
      CALL REPOUT(1,LRM,REP)
      IF ( LOGIEL(1:6) .EQ. 'GIBI98' ) THEN
        LOGIEL = REP(1:LRM)//'gibi98'
        LLOGIE = LRM+6
      ELSEIF ( LOGIEL(1:8) .EQ. 'GIBI2000' ) THEN
        LOGIEL = REP(1:LRM)//'gibi2000'
        LLOGIE = LRM+8
      ELSE
        CALL UTMESS('F',NOMCMD,'SEULS GIBI98 ET GIBI2000 SONT '//
     +                  'APPELABLES.')
      ENDIF
C
C     --- ECRITURE SUR LE FICHIER .DATG  DE LA PROCEDURE ---
C
      DONGIB = 'donngib'
      CALL ULDEFI( UNITD , DONGIB , 'A' , 'N' , 'O')
C
      UNITP = 71
      PROGIB = 'provgib'
      CALL ULDEFI( UNITP , PROGIB , 'A' , 'N' , 'O')
C        
      IF (FISS) THEN
C
C     PROCEDURE COUDE FISSURE (MOT-CLE FISS_COUDE)
C
        CALL ASCFDO (UNITD, RM, RC, ALPHA, NBTRAN, EP1, EP2, EPI, TETA1,
     +               TETA2, LTRAN, SUREP, LTCHAR, LTCLIM,
     +               TYPBOL, PAXEP, GAXEP, NT, NS, NC, SFP, ORIEN, AZIM,
     +               RC0, RC2, RC3, POSIT, EPSI, NIVMAG, SYME)
      ELSE IF (SSEP) THEN

        IF (SSEPV2) THEN
C
C     PROCEDURE COUDE SOUS-EPAISSEURS (MOT-CLE SOUS_EPAIS_COUDE)
C
          CALL ASCSQO (UNITD, TYPELE, RM, RC, ALPHA, NBTRAN, EP1, EP2,
     +                 EPI, TETA1, TETA2, LTRAN, LTCHAR, LTCLIM,
     +                 GEOM, SYME, ZR(ISLP), ZR(ISCP), ZR(ICIRP),
     +                 ZR(ILONP), ZR(IPROC), ZK8(IPOS), ZK8(ITYPE),
     +                 ZR(ISLC), ZR(IBETC), ZR(ILONC), NBEP, ZK8(IEVID),
     +                 ZI(INBXS), ZI(INBEC), ZI(INBEL), ZR(IPHIC),
     +                 NIVMAG,ZI(IPCL) )
          CALL ASCSQ2 (UNITP, NBSEP, ZK8(ITYPE), ZI(INBEC), ZI(INBEL))
C
        ELSE
C
C     PROCEDURE COUDE SOUS-EPAISSEURS (MOT-CLE SOUS_EPAIS_MULTI)
C
          CALL ASCSP1 (UNITD, TYPELE, NIVMAG)
          CALL ASCSDO (UNITP, RM, RC, ALPHA, EP1, LTCHAR, LTCLIM,
     +                 GEOM, SYME, NZONEC, NZONEL ,ZI(INDBG), ZI(INDBD),
     +                 ZR(IBG), ZR(IBD),  ZI(INDBI),ZI(INDBS), ZR(IBI),
     +                 ZR(IBS), ZR(IDNX),  ZR(IDNY), NBSEP,  ZR(ISLP),
     +                 ZR(ISCP),ZR(ICIRP), ZR(ILONP), ZR(IPROC),
     +                 ZK8(IPOS),ZK8(ITYPE), ZR(ISLC), ZR(IBETC),
     +                 ZR(ILONC),NBEP, ZK8(IEVID), TYPELE, ZI(IPCL))
          CALL ASCSP2 (UNITP,NBSEP,ZK8(ITYPE),ZI(INBEC),ZI(INBEL))
C
        END IF
C
      ELSE
C
C     PROCEDURE COUDE REGLE
C
        CALL ASCRDO (UNITD, RM, RC, ALPHA, EP1, SUREP, LTCHAR, LTCLIM,
     +               TYPBOL, NBEP, TYPELE, NIVMAG)
      END IF
      CLOSE( UNIT=UNITD )
      CLOSE( UNIT=UNITP )
C
C        --- COMMANDE EXEC_LOGICIEL ---
C
      CALL CODENT ( UNITD, 'G', K8B1 )
      NOPAR1 = 'fort.'//K8B1
      CALL CODENT ( UNITS, 'G', K8B1 )
      NOPAR2 = 'fort.'//K8B1
      I4 = 4
      I8 = 8
      I16 = 16
      ICMD = ICMD + 1
      CALL SMDCMD ( ICMD, ' ', 'EXEC_LOGICIEL', IERUSR )
        CALL PUTVTX ( 'LOGICIEL', 1, LOGIEL, LLOGIE, IERUSR)
        CALL SMDMCF ( 'ARGUMENT', IERUSR )
          CALL PUTVTX ( 'NOM_PARA', 1, NOPAR1, I8, IERUSR)
        CALL SMFMCF ( IERUSR )
        CALL SMDMCF ( 'ARGUMENT', IERUSR )
          CALL PUTVTX ( 'NOM_PARA', 1, NOPAR2, I8, IERUSR)
        CALL SMFMCF ( IERUSR )
      CALL SMFCMD ( IERUSR )
C
C     --- COMMANDE  PRE_GIBI  ---
C
      ICMD = ICMD + 1
      CALL SMDCMD ( ICMD, ' ', 'PRE_GIBI', IERUSR )
        CALL PUTVIS ( 'UNITE_GIBI', 1, UNITS, IERUSR )
      CALL SMFCMD ( IERUSR )
C
C     --- COMMANDE  LIRE_MAILLAGE  ---
C
      ICMD = ICMD + 1
      CALL SMDCMD ( ICMD, MAPROV, 'LIRE_MAILLAGE', IERUSR )
        CALL PUTVIS ( 'INFO' , 1, INFO , IERUSR )
      CALL SMFCMD ( IERUSR )
C
C     --- COMMANDES  DEFI_GROUP  ---
C
      ICMD = ICMD + 1
      CALL SMDCMD ( ICMD, '&'//MAPROV, 'DEFI_GROUP', IERUSR )
        CALL PUTVID ( 'MAILLAGE', 1, MAPROV, IERUSR )
C
C       CONVERSION DE GROUPE DE MAILLES GENERAUX
C
        K8B1 = 'BORD1'
        CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
          CALL PUTVID ( 'GROUP_MA', 1, K8B1, IERUSR )
        CALL SMFMCF ( IERUSR )
        K8B1 = 'CLGV'
        CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
          CALL PUTVID ( 'GROUP_MA', 1, K8B1, IERUSR )
        CALL SMFMCF ( IERUSR )
        K8B1 = 'BORD2'
        CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
          CALL PUTVID ( 'GROUP_MA', 1, K8B1, IERUSR )
        CALL SMFMCF ( IERUSR )
        K8B1 = 'PEAUINT'
        CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
          CALL PUTVID ( 'GROUP_MA', 1, K8B1, IERUSR )
        CALL SMFMCF ( IERUSR )
        K8B1 = 'PEAUEXT'
        CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
          CALL PUTVID ( 'GROUP_MA', 1, K8B1, IERUSR )
        CALL SMFMCF ( IERUSR )
C
C       CAS DES FISSURES AXISYMETRIQUES
        IF (FAXI.EQ.'OUI') THEN
           K8B1 = 'FONDFISS'
           CALL SMDMCF ( 'CREA_GROUP_MA', IERUSR )
             CALL PUTVID ( 'GROUP_MA' , 1,  K8B1, IERUSR )
             CALL PUTVID ( 'NOM'      , 1, 'MAIL_ORI', IERUSR )
             CALL PUTVTX ( 'POSITION' , 1, 'INIT', I4, IERUSR )
           CALL SMFMCF ( IERUSR )
        ENDIF
C
C       CONVERSION DES GROUPES DE MAILLES DU BLOC FISSURE
C
        IF (FISS) THEN
          IF (SYME(1:6).EQ.'ENTIER') THEN
            K8B1 = 'NOLIG1'
            CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
              CALL PUTVID ( 'GROUP_MA', 1, K8B1, IERUSR )
            CALL SMFMCF ( IERUSR )
            K8B1 = 'FACE1'
            CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
              CALL PUTVID ( 'GROUP_MA', 1, K8B1, IERUSR )
            CALL SMFMCF ( IERUSR )
          END IF
          K8B1 = 'NOLIG2'
          CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
            CALL PUTVID ( 'GROUP_MA', 1, K8B1, IERUSR )
          CALL SMFMCF ( IERUSR )
          K8B1 = 'FACE2'
          CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
            CALL PUTVID ( 'GROUP_MA', 1, K8B1, IERUSR )
          CALL SMFMCF ( IERUSR )
          K8B1 = 'FONDFISS'
          CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
            CALL PUTVID ( 'GROUP_MA', 1, K8B1, IERUSR )
          CALL SMFMCF ( IERUSR )
        END IF
C
C       CONVERSION DES GROUPES DE MAILLES EN GROUPES DE NOEUDS
C       POUR LES LIGAMENTS DES SOUS-EPAISSEURS
C
        IF (SSEP) THEN
          K16B4 = 'SEGM_DROI_ORDO'
          DO 20 I=1, NBSEP
             CALL CODENT(I,'G',CAR)
             IPOS1 = INDEX(CAR,' ')-1
             IF (ZK8(ITYPE+I-1).EQ.'ELLI') THEN
C
               DO 30 K= 1 , 2*ZI(INBEC+I-1)+1
C               
                 CALL CODENT(K,'G',CAR2)
                 IPOS2 = INDEX(CAR2,' ')-1
                 K8B1 = 'CIR'//CAR(1:IPOS1)//'_'//CAR2(1:IPOS2)
                 K8B2 = 'ICI'//CAR(1:IPOS1)//'_'//CAR2(1:IPOS2)
                 CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
                   CALL PUTVID ( 'GROUP_MA', 1, K8B1, IERUSR )
                        CALL PUTVID ( 'NOM', 1, K8B2, IERUSR )
                 CALL SMFMCF ( IERUSR )
 30            CONTINUE
C
                 K8B1 = 'PCENT'//CAR(1:IPOS1)
                 K8B2 = 'IPCEN'//CAR(1:IPOS1)
                 CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
                   CALL PUTVID ( 'GROUP_MA', 1, K8B1, IERUSR )
                        CALL PUTVID ( 'NOM', 1, K8B2, IERUSR )
                 CALL SMFMCF ( IERUSR )
C
               DO 50 K= 1 , 2*ZI(INBEL+I-1)+1
                 CALL CODENT(K,'G',CAR2)
                 IPOS2 = INDEX(CAR2,' ')-1
                 K8B1 = 'LON'//CAR(1:IPOS1)//'_'//CAR2(1:IPOS2)
                 K8B2 = 'ILO'//CAR(1:IPOS1)//'_'//CAR2(1:IPOS2)
                 CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
                   CALL PUTVID ( 'GROUP_MA', 1, K8B1, IERUSR )
                        CALL PUTVID ( 'NOM', 1, K8B2, IERUSR )
                 CALL SMFMCF ( IERUSR )
 50            CONTINUE
C  
               GROUNO(1) = 'PEAUEXT'
               DO 51 K= 1 , 2*ZI(INBEC+I-1)+1
                 CALL CODENT(K,'G',CAR2)
                 IPOS2 = INDEX(CAR2,' ')-1
                 GROUNO(2) = 'ICI'//CAR(1:IPOS1)//'_'//CAR2(1:IPOS2)
                 K8B1 = 'OCI'//CAR(1:IPOS1)//'_'//CAR2(1:IPOS2)
                 CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
                   CALL PUTVID ( 'NOM', 1, K8B1, IERUSR )
                   CALL PUTVID ( 'INTERSEC', 2, GROUNO ,  IERUSR )
                 CALL SMFMCF ( IERUSR )
 51            CONTINUE
               GROUNO(1) = 'PEAUINT'
               DO 52 K= 1 , 2*ZI(INBEC+I-1)+1
                 CALL CODENT(K,'G',CAR2)
                 IPOS2 = INDEX(CAR2,' ')-1
                 GROUNO(2) = 'ICI'//CAR(1:IPOS1)//'_'//CAR2(1:IPOS2)
                 K8B1 = 'ECI'//CAR(1:IPOS1)//'_'//CAR2(1:IPOS2)
                 CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
                   CALL PUTVID ( 'NOM', 1, K8B1, IERUSR )
                   CALL PUTVID ( 'INTERSEC', 2, GROUNO ,  IERUSR )
                 CALL SMFMCF ( IERUSR )
 52            CONTINUE 
C
               GROUNO(1) = 'PEAUEXT'
               GROUNO(2) = 'IPCEN'//CAR(1:IPOS1)
               K8B1 = 'OPCEN'//CAR(1:IPOS1)
               CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
                 CALL PUTVID ( 'NOM', 1, K8B1, IERUSR )
                 CALL PUTVID ( 'INTERSEC', 2, GROUNO ,  IERUSR )
               CALL SMFMCF ( IERUSR )
               GROUNO(1) = 'PEAUINT'
               GROUNO(2) = 'IPCEN'//CAR(1:IPOS1)
               K8B1 = 'EPCEN'//CAR(1:IPOS1)
               CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
                 CALL PUTVID ( 'NOM', 1, K8B1, IERUSR )
                 CALL PUTVID ( 'INTERSEC', 2, GROUNO ,  IERUSR )
               CALL SMFMCF ( IERUSR )
C
               GROUNO(1) = 'PEAUEXT'
               DO 53 K= 1 , 2*ZI(INBEL+I-1)+1
                 CALL CODENT(K,'G',CAR2)
                 IPOS2 = INDEX(CAR2,' ')-1
                 GROUNO(2) = 'ILO'//CAR(1:IPOS1)//'_'//CAR2(1:IPOS2)
                 K8B1 = 'OLO'//CAR(1:IPOS1)//'_'//CAR2(1:IPOS2)
                 CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
                   CALL PUTVID ( 'NOM', 1, K8B1, IERUSR )
                   CALL PUTVID ( 'INTERSEC', 2, GROUNO ,  IERUSR )
                 CALL SMFMCF ( IERUSR )
 53            CONTINUE
               GROUNO(1) = 'PEAUINT'
               DO 54 K= 1 , 2*ZI(INBEL+I-1)+1
                 CALL CODENT(K,'G',CAR2)
                 IPOS2 = INDEX(CAR2,' ')-1
                 GROUNO(2) = 'ILO'//CAR(1:IPOS1)//'_'//CAR2(1:IPOS2)
                 K8B1 = 'ELO'//CAR(1:IPOS1)//'_'//CAR2(1:IPOS2)
                 CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
                   CALL PUTVID ( 'NOM', 1, K8B1, IERUSR )
                   CALL PUTVID ( 'INTERSEC', 2, GROUNO ,  IERUSR )
                 CALL SMFMCF ( IERUSR )
 54            CONTINUE
C
               DO 55 K= 1 , 2*ZI(INBEC+I-1)+1
                 CALL CODENT(K,'G',CAR2)
                 IPOS2 = INDEX(CAR2,' ')-1
                 GROUNO(1) = 'OCI'//CAR(1:IPOS1)//'_'//CAR2(1:IPOS2)
                 GROUNO(2) = 'ECI'//CAR(1:IPOS1)//'_'//CAR2(1:IPOS2)
                 K8B1 = 'CIR'//CAR(1:IPOS1)//'_'//CAR2(1:IPOS2)
                 K8B2 = 'ICI'//CAR(1:IPOS1)//'_'//CAR2(1:IPOS2)
                 CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
                   CALL PUTVTX ( 'OPTION', 1, K16B4, I16, IERUSR )
                   CALL PUTVID ( 'NOM', 1, K8B1 ,  IERUSR )
                   CALL PUTVID ( 'GROUP_NO', 1, K8B2 ,  IERUSR )
                   CALL PUTVID ( 'GROUP_NO_ORIG', 1, GROUNO(1), IERUSR )
                   CALL PUTVID ( 'GROUP_NO_EXTR', 1, GROUNO(2), IERUSR )
                   CALL PUTVR8 ( 'PRECISION', 1, PRECIS, IERUSR )
                   CALL PUTVTX ( 'CRITERE', 1, CRITER, I8, IERUSR )
                 CALL SMFMCF ( IERUSR )
 55            CONTINUE
 
               GROUNO(1) = 'OPCEN'//CAR(1:IPOS1)
               GROUNO(2) = 'EPCEN'//CAR(1:IPOS1)
               K8B1 = 'PCENT'//CAR(1:IPOS1)
               K8B2 = 'IPCEN'//CAR(1:IPOS1) 
               CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
                 CALL PUTVTX ( 'OPTION', 1, K16B4, I16, IERUSR )
                 CALL PUTVID ( 'NOM', 1, K8B1 ,  IERUSR )
                 CALL PUTVID ( 'GROUP_NO', 1, K8B2 ,  IERUSR )
                 CALL PUTVID ( 'GROUP_NO_ORIG', 1, GROUNO(1), IERUSR )
                 CALL PUTVID ( 'GROUP_NO_EXTR', 1, GROUNO(2), IERUSR )
                 CALL PUTVR8 ( 'PRECISION', 1, PRECIS, IERUSR )
                 CALL PUTVTX ( 'CRITERE', 1, CRITER, I8, IERUSR )
               CALL SMFMCF ( IERUSR )
C 
               DO 56 K= 1 , 2*ZI(INBEL+I-1)+1
                 CALL CODENT(K,'G',CAR2)
                 IPOS2 = INDEX(CAR2,' ')-1
                 GROUNO(1) = 'OLO'//CAR(1:IPOS1)//'_'//CAR2(1:IPOS2)
                 GROUNO(2) = 'ELO'//CAR(1:IPOS1)//'_'//CAR2(1:IPOS2)
                 K8B1 = 'LON'//CAR(1:IPOS1)//'_'//CAR2(1:IPOS2)
                 K8B2 = 'ILO'//CAR(1:IPOS1)//'_'//CAR2(1:IPOS2)
                 CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
                   CALL PUTVTX ( 'OPTION', 1, K16B4, I16, IERUSR )
                   CALL PUTVID ( 'NOM', 1, K8B1 ,  IERUSR )
                   CALL PUTVID ( 'GROUP_NO', 1, K8B2 ,  IERUSR )
                   CALL PUTVID ( 'GROUP_NO_ORIG', 1, GROUNO(1), IERUSR )
                   CALL PUTVID ( 'GROUP_NO_EXTR', 1, GROUNO(2), IERUSR )
                   CALL PUTVR8 ( 'PRECISION', 1, PRECIS, IERUSR )
                   CALL PUTVTX ( 'CRITERE', 1, CRITER, I8, IERUSR )
                 CALL SMFMCF ( IERUSR )
 56            CONTINUE 
C
             END IF
C
C   1/ NOMS INTERMEDIAIRES DES GROUPES DE NOEUDS REPRESENTANT LES
C      LIGAMENTS DES SECTIONS : TU, MI, GV ET SOUS-EPAISSEURS (1, ...).
C
             DO 60 K = 1,8
               K8B1 = CAR6(K)//CAR(1:IPOS1)
               K8B2 = CAR3(K)//CAR(1:IPOS1)
               CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
                 CALL PUTVID ( 'GROUP_MA', 1, K8B1, IERUSR )
                 CALL PUTVID ( 'NOM', 1, K8B2, IERUSR )
               CALL SMFMCF ( IERUSR )
 60          CONTINUE
C
 20       CONTINUE
          DO 70 K = 1,3
             IF (SYME(1:6).EQ.'ENTIER' .OR. K.NE.3) THEN
               DO 80 J = 1,8
                 K8B1 = CAR6(J)//SECT(K)
                 K8B2 = CAR3(J)//SECT(K)
                 CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
                   CALL PUTVID ( 'GROUP_MA', 1, K8B1, IERUSR )
                   CALL PUTVID ( 'NOM', 1, K8B2, IERUSR )
                 CALL SMFMCF ( IERUSR )
 80            CONTINUE
             END IF
 70       CONTINUE
C
C   2/ DETERMINATION ET NOMMAGE DES NOEUDS ORIGINE ET EXTREMITE DES
C      GROUPES DE NOEUDS REPRESENTANT LES LIGAMENTS DE LA OU DES
C      SECTIONS : SOUS-EPAISSEUR (1, ...).
C
          DO 110 I=1, NBSEP
             CALL CODENT(I,'G',CAR)
             IPOS1 = INDEX(CAR,' ')-1
C
             GROUNO(1) = 'PEAUEXT'
             DO 120 K = 1,8
               K8B1 = CAR4(K)//CAR(1:IPOS1)
               GROUNO(2) = CAR3(K)//CAR(1:IPOS1)
               CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
                 CALL PUTVID ( 'NOM', 1, K8B1, IERUSR )
                 CALL PUTVID ( 'INTERSEC', 2, GROUNO ,  IERUSR )
               CALL SMFMCF ( IERUSR )
 120         CONTINUE
C
             GROUNO(1) = 'PEAUINT'
             DO 130 K = 1,8
               K8B1 = CAR5(K)//CAR(1:IPOS1)
               GROUNO(2) = CAR3(K)//CAR(1:IPOS1)
               CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
                 CALL PUTVID ( 'NOM', 1, K8B1, IERUSR )
                 CALL PUTVID ( 'INTERSEC', 2, GROUNO ,  IERUSR )
               CALL SMFMCF ( IERUSR )
 130         CONTINUE
C
C   3/ NOMMAGE FINAL DES GROUPES DE NOEUDS REPRESENTANT LES LIGAMENTS
C      DE LA OU DES SECTIONS : SOUS-EPAISSEURS (1, ...).
C
             DO 140 K = 1,8
               K8B1 = CAR6(K)//CAR(1:IPOS1)
               K8B2 = CAR3(K)//CAR(1:IPOS1)
               GROUNO(1) = CAR4(K)//CAR(1:IPOS1)
               GROUNO(2) = CAR5(K)//CAR(1:IPOS1)
               CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
                 CALL PUTVTX ( 'OPTION', 1, K16B4, I16, IERUSR )
                 CALL PUTVID ( 'NOM', 1, K8B1 ,  IERUSR )
                 CALL PUTVID ( 'GROUP_NO', 1, K8B2 ,  IERUSR )
                 CALL PUTVID ( 'GROUP_NO_ORIG', 1, GROUNO(1), IERUSR )
                 CALL PUTVID ( 'GROUP_NO_EXTR', 1, GROUNO(2), IERUSR )
                 CALL PUTVR8 ( 'PRECISION', 1, PRECIS, IERUSR )
                 CALL PUTVTX ( 'CRITERE', 1, CRITER, I8, IERUSR )
               CALL SMFMCF ( IERUSR )
 140         CONTINUE
C
C   4/ DETERMINATION ET NOMMAGE DES NOEUDS ORIGINE ET EXTREMITE DES
C      GROUPES DE NOEUDS REPRESENTANT LES LIGAMENTS DES SECTIONS :
C      TU, MI ET GV.
C
 110      CONTINUE
          DO 200 K = 1,3
             IF (SYME(1:6).EQ.'ENTIER' .OR. K.NE.3) THEN
               GROUNO(1) = 'PEAUEXT'
               DO 210 J = 1,8
                 K8B1 = CAR4(J)//SECT(K)
                 GROUNO(2) = CAR3(J)//SECT(K)
                 CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
                   CALL PUTVID ( 'NOM', 1, K8B1, IERUSR )
                   CALL PUTVID ( 'INTERSEC', 2, GROUNO ,  IERUSR )
                 CALL SMFMCF ( IERUSR )
 210           CONTINUE
               GROUNO(1) = 'PEAUINT'
               DO 220 J = 1,8
                 K8B1 = CAR5(J)//SECT(K)
                 GROUNO(2) = CAR3(J)//SECT(K)
                 CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
                   CALL PUTVID ( 'NOM', 1, K8B1, IERUSR )
                   CALL PUTVID ( 'INTERSEC', 2, GROUNO ,  IERUSR )
                 CALL SMFMCF ( IERUSR )
 220           CONTINUE
C
C   5/ NOMMAGE FINAL DES GROUPES DE NOEUDS REPRESENTANT LES LIGAMENTS
C      DES SECTIONS : TU, MI ET GV.
C
               DO 230 J = 1,8
                 K8B1 = CAR6(J)//SECT(K)
                 K8B2 = CAR3(J)//SECT(K)
                 GROUNO(1) = CAR4(J)//SECT(K)
                 GROUNO(2) = CAR5(J)//SECT(K)
                 CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
                   CALL PUTVTX ('OPTION',1 , K16B4, I16, IERUSR)
                   CALL PUTVID ('NOM', 1, K8B1 ,  IERUSR )
                   CALL PUTVID ('GROUP_NO', 1, K8B2 ,  IERUSR )
                   CALL PUTVID ('GROUP_NO_ORIG', 1, GROUNO(1), IERUSR)
                   CALL PUTVID ('GROUP_NO_EXTR', 1, GROUNO(2), IERUSR)
                   CALL PUTVR8 ('PRECISION', 1, PRECIS, IERUSR )
                   CALL PUTVTX ('CRITERE', 1, CRITER, I8, IERUSR )
                 CALL SMFMCF ( IERUSR )
 230           CONTINUE
             END IF
 200      CONTINUE
C
        END IF
      CALL SMFCMD ( IERUSR )
C
      IF (FISS) THEN
C
C       CREATION DES GROUPES PETIT AXE ET GRAND AXE FISSURE PAR
C       INTERSECTION DE GROUPES EXISTANTS
C
        ICMD = ICMD + 1
        CALL SMDCMD ( ICMD, '&'//MAPROV, 'DEFI_GROUP', IERUSR )
          CALL PUTVID ( 'MAILLAGE', 1, MAPROV, IERUSR )
          IF (SYME(1:6).EQ.'ENTIER') THEN
            GROUMA(1) = 'NOLIG1'
            GROUMA(2) = 'FACE1'
            K8B1 = 'P_AXE_1'
            CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
              CALL PUTVID ( 'NOM', 1, K8B1, IERUSR )
              CALL PUTVID ( 'INTERSEC', 2, GROUMA ,  IERUSR )
            CALL SMFMCF ( IERUSR )
          END IF
          GROUMA(1) = 'NOLIG2'
          GROUMA(2) = 'FACE2'
          K8B1 = 'P_AXE_2'
          CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
            CALL PUTVID ( 'NOM', 1, K8B1, IERUSR )
            CALL PUTVID ( 'INTERSEC', 2, GROUMA ,  IERUSR )
          CALL SMFMCF ( IERUSR )
          IF (POSIT.EQ.'DEB_INT ') THEN
            GROUMA(1) = 'PEAUINT'
          ELSE
            GROUMA(1) = 'PEAUEXT'
          END IF
          IF (SYME(1:6).EQ.'ENTIER') THEN
            GROUMA(2) = 'FACE1'
            K8B1 = 'G_AXE_1'
            CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
              CALL PUTVID ( 'NOM', 1, K8B1, IERUSR )
              CALL PUTVID ( 'INTERSEC', 2, GROUMA ,  IERUSR )
            CALL SMFMCF ( IERUSR )
          END IF
          GROUMA(2) = 'FACE2'
          K8B1 = 'G_AXE_2'
          CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
            CALL PUTVID ( 'NOM', 1, K8B1, IERUSR )
            CALL PUTVID ( 'INTERSEC', 2, GROUMA ,  IERUSR )
          CALL SMFMCF ( IERUSR )
C
        CALL SMFCMD ( IERUSR )
C
      END IF
C
C     --- COMMANDE AFFE_MODELE ---
C
      K16B1 = 'MECANIQUE'
      K16B3 = '3D'
      K8B1 = 'COUDE'
      ICMD = ICMD + 1
      CALL GCNCON ( '.' , MODELE )
      CALL SMDCMD ( ICMD, MODELE, 'AFFE_MODELE', IERUSR )
        CALL PUTVID ( 'MAILLAGE', 1, MAPROV, IERUSR )
        CALL SMDMCF ( 'AFFE', IERUSR )
          CALL PUTVID ( 'GROUP_MA', 1, K8B1, IERUSR)
          CALL PUTVTX ( 'PHENOMENE'   , 1, K16B1, I16, IERUSR)
          CALL PUTVTX ( 'MODELISATION', 1, K16B3, I16, IERUSR)
        CALL SMFMCF ( IERUSR )
      CALL SMFCMD ( IERUSR )
C
C     --- COMMANDE  MODI_MAILLAGE  ---
C
      ICMD = ICMD + 1
      CALL SMDCMD ( ICMD, '&'//MAPROV, 'MODI_MAILLAGE', IERUSR )
        CALL PUTVID ( 'MAILLAGE', 1, MAPROV, IERUSR )
        CALL SMDMCF ( 'PLAQ_TUBE', IERUSR )
          CALL PUTVR8 ( 'DEXT'  , 1, DEXT  , IERUSR )
          CALL PUTVR8 ( 'EPAIS' , 1, EP1   , IERUSR )
          IF (SYME(1:5).EQ.'QUART') THEN
           K8B1 = 'NON'
           CALL PUTVTX ( 'COUTURE'  , 1, K8B1  ,I8,  IERUSR )
          END IF
          IF (FISS) THEN
            CALL PUTVR8 ( 'AZIMUT'  , 1, AZIM , IERUSR )
          ELSE IF (SSEPV2) THEN
            CALL PUTVR8 ( 'AZIMUT'  , 1, ZR(IPHIC) , IERUSR )
          END IF
          CALL PUTVR8 ( 'L_TUBE_P1', 1, LTCHAR, IERUSR )
        CALL SMFMCF ( IERUSR )
        IF (GEOM.EQ.'COUDE') THEN
          CALL SMDMCF ( 'TUBE_COUDE', IERUSR )
            CALL PUTVR8 ( 'ANGLE'    , 1, ALPHA  , IERUSR )
            CALL PUTVR8 ( 'R_CINTR'  , 1, RC     , IERUSR )
            CALL PUTVR8 ( 'L_TUBE_P1', 1, LTCHAR, IERUSR )
            CALL SMFMCF ( IERUSR )
        END IF
      CALL SMFCMD ( IERUSR )
C
      ICMD = ICMD + 1
      CALL SMDCMD ( ICMD, '&'//MAPROV, 'MODI_MAILLAGE', IERUSR )
        CALL PUTVID ( 'MAILLAGE', 1, MAPROV, IERUSR )
        CALL PUTVID ( 'MODELE', 1, MODELE, IERUSR )
        CALL SMDMCF ( 'ORIE_PEAU_3D', IERUSR )
           GROUMA(1) = 'PEAUINT'
           GROUMA(2) = 'EXTUBE'
           IF (POSIT .EQ. 'DEB_INT') THEN
             GROUMA(3) = 'FACE1'
             GROUMA(4) = 'FACE2'
             CALL PUTVID ( 'GROUP_MA',4, GROUMA, IERUSR )
           ELSE
             CALL PUTVID ( 'GROUP_MA',2, GROUMA, IERUSR )
           ENDIF
        CALL SMFMCF ( IERUSR )
      CALL SMFCMD ( IERUSR )
C
C     --- COMMANDE CREA_MAILLAGE ---
C
      IF (SYME(1:5).NE.'QUART') THEN
        NOP1   = 'P1      '
        NOP2   = 'P2      '
C
        ICMD = ICMD + 1
        CALL SMDCMD ( ICMD, NOMRES, 'CREA_MAILLAGE', IERUSR )
          CALL PUTVID ( 'MAILLAGE', 1, MAPROV, IERUSR )
          CALL SMDMCF ( 'CREA_POI1', IERUSR )
            CALL PUTVID ( 'NOM_GROUP_MA', 1, NOP1, IERUSR )
            CALL PUTVID ( 'GROUP_NO'    , 1, NOP1, IERUSR )
          CALL SMFMCF ( IERUSR )
          IF (NBOL.EQ.0) THEN
            CALL SMDMCF ( 'CREA_POI1', IERUSR )
             CALL PUTVID ( 'NOM_GROUP_MA', 1, NOP2, IERUSR )
             CALL PUTVID ( 'GROUP_NO'    , 1, NOP2, IERUSR )
            CALL SMFMCF ( IERUSR )
          END IF
        CALL SMFCMD ( IERUSR )
      ENDIF
C
C     --- COMMANDE  DEFI_FICHIER, IMPR_RESU  ---
C
      IF ( IMPR ) THEN
C
       DO 300 I=1, NIMP
C
         IF ( ZI(INF+I-1) .NE. 0 ) THEN
            ICMD = ICMD + 1
            CALL SMDCMD ( ICMD, ' ', 'DEFI_FICHIER', IERUSR )
            CALL PUTVTX ( 'FICHIER'   , 1, ZK16(IFICH+I-1) ,
     +                        ZI(ILFIC+I-1), IERUSR)
            CALL PUTVIS ( 'UNITE' , 1, ZI(IUNIT+I-1), IERUSR )
            CALL SMFCMD ( IERUSR )
         ENDIF
C
         ICMD = ICMD + 1
         CALL SMDCMD ( ICMD, ' ', 'IMPR_RESU', IERUSR )
           CALL SMDMCF ( 'RESU', IERUSR )
             CALL PUTVID ( 'MAILLAGE', 1, NOMRES, IERUSR )
             CALL PUTVTX ( 'FORMAT'  , 1, ZK16(IFORM+I-1),
     +                     ZI(ILFOR+I-1) , IERUSR)
             IF (ZK16(IFORM+I-1)(1:5).EQ.'IDEAS') THEN
               CALL PUTVIS ( 'VERSION' , 1, ZI(IVERS+I-1), IERUSR )
             ELSEIF (ZK16(IFORM+I-1)(1:6).EQ.'CASTEM') THEN
               CALL PUTVIS ( 'NIVE_GIBI' , 1, ZI(INIVG+I-1), IERUSR )
             ENDIF
             IF ( ZI(INF+I-1) .NE. 0 ) THEN
               CALL PUTVTX ( 'FICHIER'  , 1, ZK16(IFICH+I-1) ,
     +                       ZI(ILFIC+I-1), IERUSR)
             ENDIF
           CALL SMFMCF ( IERUSR )
         CALL SMFCMD ( IERUSR )
C
 300   CONTINUE
C
      ENDIF

C
C     ---------------------------------------------------------------
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
      CALL JEDETC('V','&&OPS019',1)
      CALL JEDEMA()
      END
