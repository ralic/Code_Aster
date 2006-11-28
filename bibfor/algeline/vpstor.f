      SUBROUTINE VPSTOR (INEG, TYPE, MODES, NBMODE, NEQ, VECPR8, VECPC8,
     &                   MXRESF, NBPARI, NBPARR, NBPARK, NOPARA, MOD45,
     &                   RESUFI, RESUFR, RESUFK, IPREC  )
      IMPLICIT   NONE
      INTEGER           INEG, NBMODE, NEQ, MXRESF, NBPARI, NBPARR,NBPARK
      INTEGER           IPREC, RESUFI(MXRESF,*)
      CHARACTER*4       MOD45
      CHARACTER*(*)     TYPE, MODES, RESUFK(MXRESF,*), NOPARA(*)
      REAL*8            VECPR8(NEQ,*), RESUFR(MXRESF,*)
      COMPLEX*16        VECPC8(NEQ,*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 28/11/2006   AUTEUR COURTOIS M.COURTOIS 
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
C     STOCKAGE DES VALEURS PROPRES
C
C     REMARQUE:
C        DANS NOPARA, ON A LES NOMS DE PARAMETRES DE TYPE ENTIER
C                     ENSUITE LES NOMS DE PARAMETRES DE TYPE CHARACTER
C                     ENSUITE LES NOMS DE PARAMETRES DE TYPE REEL
C     ------------------------------------------------------------------
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
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C     ------------------------------------------------------------------
      INTEGER       JREFD, IMODE, JMODE, IER, NMIN, IMIN, NMAX, IMAX,IEQ
      INTEGER       NMIN1, KMODE, NORDR, IBID, I, LADPA, LMODE, LVALE
      INTEGER       INDK24, NBPAST, IRANG,IRET,JMODG,JMACR,JBASM,JRAID
      INTEGER       JMOD2,JMODL,JMATE,JCARA,JLIME,JMERI
      PARAMETER    ( NBPAST = 17 )
      CHARACTER*8   RES ,K8B, RAIDE, MODELE, CHMAT, CARAEL
      CHARACTER*16  TYPCON, NOMCMD, NOSY
      CHARACTER*19  CHAMNO
      CHARACTER*24  REFD,NUME,NOPAST(NBPAST)
      CHARACTER*32  JEXNUM
      LOGICAL       LREFD, LNUME, LBASM, LSTOCK
C     ------------------------------------------------------------------
      DATA  REFD  /'                   .REFD'/
C
C --- PARAMETRES STOCKES DANS LA SD RESULTAT DYNAMIQUE
      DATA  NOPAST /        'NUME_MODE'       , 'NORME'           ,
     &  'FREQ'            , 'OMEGA2'          , 'AMOR_REDUIT'     ,
     &  'MASS_GENE'       , 'RIGI_GENE'       , 'AMOR_GENE'       ,
     &  'MASS_EFFE_DX'    , 'MASS_EFFE_DY'    , 'MASS_EFFE_DZ'    ,
     &  'FACT_PARTICI_DX' , 'FACT_PARTICI_DY' , 'FACT_PARTICI_DZ' ,
     &  'MASS_EFFE_UN_DX' , 'MASS_EFFE_UN_DY' , 'MASS_EFFE_UN_DZ' /
C     ------------------------------------------------------------------
C
      CALL JEMARQ ( )
C
      CALL GETRES (RES, TYPCON, NOMCMD )
C
C     POUR POUVOIR UTILISER VPSTOR DANS STAT_NON_LINE VIA NMOP45
      IF ( TYPCON .EQ. 'EVOL_NOLI' ) THEN
        TYPCON = 'MODE_FLAMB'
        IF ( MOD45 . EQ. 'VIBR' ) TYPCON = 'MODE_MECA'
      ENDIF
      
      IF ( TYPCON .EQ. 'MODE_ACOU' ) THEN
        NOSY = 'PRES'
      ELSE
        NOSY = 'DEPL'
      ENDIF
C
      IF ( TYPCON(1:11) .EQ. 'BASE_MODALE' ) THEN 
        LBASM=.TRUE.
      ELSE
        LBASM=.FALSE.
      ENDIF
C
      LREFD = .TRUE.
      LNUME = .TRUE.
      LSTOCK = .FALSE.
      REFD(1:8) = MODES
C On teste l'existence du REFD
      CALL JEEXIN(REFD,IER)
      IF (IER.EQ.0) THEN
         LREFD = .FALSE.
      ELSE
         IF(LBASM)THEN
           LNUME = .TRUE.
           CALL GETVID(' ','RAIDE',0,1,1,RAIDE,IER)
         ELSE
           CALL JEVEUO (REFD, 'L', JREFD )
C On recupere la matrice du REFD
           RAIDE=ZK24(JREFD)(1:8)
           CALL EXISD('MATR_ASSE',RAIDE,IER)
           IF (IER.EQ.0) THEN
C On recupere la numerotation du REFD si la matrice n'existe pas
             NUME = ZK24(JREFD+3)
             LNUME = .FALSE.
             LSTOCK=.FALSE.
           ELSE
             LNUME = .TRUE.
             LSTOCK=.TRUE.
           ENDIF
        ENDIF
C Si elle existe on prend la numerotation associee
      ENDIF
C
C
C     --- CONTROLE PREALABLE ---
      DO 20 IMODE = 1, NBMODE
         JMODE = RESUFI(IMODE,1)
         IF ( JMODE .LT. 1 .AND. INEG .GT. 0 ) THEN
            CALL U2MESS('A','ALGELINE3_79')
         ENDIF
 20   CONTINUE
C
C     --- STOCKAGE DES MODES ---
      CALL RSEXIS ( MODES , IER )
      IF ( IER .EQ. 0 ) THEN
         CALL U2MESS('F','ALGELINE3_80')
      ENDIF
C
      NMIN = RESUFI(1,1)
      IMIN = 1
      NMAX = RESUFI(1,1)
      IMAX = 1
      DO 30 IMODE = 2, NBMODE
         IF ( RESUFI(IMODE,1) .LT. NMIN ) THEN
            NMIN = RESUFI(IMODE,1)
            IMIN = IMODE
         ENDIF
         IF ( RESUFI(IMODE,1) .GT. NMAX ) THEN
            NMAX = RESUFI(IMODE,1)
            IMAX = IMODE
         ENDIF
 30   CONTINUE
      NMIN1 = NMAX
C
C     ON RECUPERE LE NOM DE LA MATRICE DE RAIDEUR AFIN DE
C     DETERMINER LE NOM DU MODELE, DU MATERIAU ET DES
C     CARACTERISTIQUES ELEMENTAIRES
      IF(LSTOCK)THEN
        IF ( TYPCON(1:9).EQ.'MODE_MECA'.OR.TYPCON.EQ. 'MODE_ACOU'
     &  .OR. TYPCON.EQ.'MODE_FLAMB')THEN
           CALL DISMOI('F','NOM_MODELE',RAIDE,'MATR_ASSE',IBID,
     &          MODELE,IRET)
           CALL DISMOI('F','CHAM_MATER',RAIDE,'MATR_ASSE',IBID,
     &          CHMAT,IRET)
           CALL DISMOI('F','CARA_ELEM',RAIDE,'MATR_ASSE',IBID,
     &          CARAEL,IRET)
        ELSEIF( TYPCON(1:9).EQ.'MODE_GENE')THEN
           CALL JEVEUO(RAIDE//'           .LIME','L',JMODG)
           IF(ZK8(JMODG).EQ.'        ')THEN
C            ON EST PASSE PAR UN PROJ_MATR_BASE
             CALL JEVEUO(RAIDE//'           .REFA','L',JMODG)
             CALL JEVEUO(ZK24(JMODG)(1:8)//'           .REFD','L',JRAID)
             IF(ZK24(JRAID)(1:8).EQ.'        ')THEN
             CALL JEVEUO(JEXNUM(ZK24(JMODG)(1:8)//'           .TACH',1),
     &                   'L',JMOD2)
             CALL JEVEUO(ZK24(JMOD2)(1:8)//'           .MODL','L',JMODL)
             CALL JEVEUO(ZK24(JMOD2)(1:8)//'           .MATE','L',JMATE)
             CALL JEVEUO(ZK24(JMOD2)(1:8)//'           .CARA','L',JCARA)
             MODELE=ZK8(JMODL)
             CHMAT =ZK8(JMATE)
             CARAEL=ZK8(JCARA)
             GOTO 39
             ELSE
             CALL JEVEUO(ZK24(JRAID)(1:8)//'           .LIME','L',JLIME)
             IF(ZK8(JLIME).NE.'        ')THEN
C            ON EST PASSE PAR UN ASSE_MATRICE/CALC_MATR_ELEM
             CALL JEEXIN(ZK8(JLIME)//'      .MODG.SSME',IRET)
             IF(IRET.NE.0)THEN
             CALL JEVEUO(ZK8(JLIME)//'      .MODG.SSME','L',JMACR)
             CALL JEVEUO(ZK8(JMACR)//'.MAEL_INER_REFE','L',JBASM)
             CALL JEVEUO(ZK24(JBASM)(1:8)//'           .REFD','L',JRAID)
             CALL JEVEUO(ZK24(JRAID)(1:8)//'           .LIME','L',JLIME)
             CALL JEVEUO(ZK8(JLIME)//'.REFE_RESU','L',JMERI)
             MODELE=ZK24(JMERI)(1:8)
             CHMAT=ZK24(JMERI+3)(1:8)
             CARAEL=ZK24(JMERI+4)(1:8)
             GOTO 39
             ENDIF
             ENDIF
             ENDIF
           ELSE
C            ON EST PASSE PAR UN DEFI_MODELE_GENE
             CALL JEVEUO(ZK8(JMODG)//'      .MODG.SSME','L',JMACR)
             CALL JEVEUO(ZK8(JMACR)//'.MAEL_INER_REFE','L',JBASM)
             CALL JEVEUO(ZK24(JBASM)(1:8)//'           .REFD','L',JRAID)
             IF(ZK24(JRAID)(1:8).EQ.'        ')THEN
             CALL JEVEUO(JEXNUM(ZK24(JBASM)(1:8)//'           .TACH',1),
     &                   'L',JMOD2)
             CALL JEVEUO(ZK24(JMOD2)(1:8)//'           .MODL','L',JMODL)
             CALL JEVEUO(ZK24(JMOD2)(1:8)//'           .MATE','L',JMATE)
             CALL JEVEUO(ZK24(JMOD2)(1:8)//'           .CARA','L',JCARA)
             MODELE=ZK8(JMODL)
             CHMAT =ZK8(JMATE)
             CARAEL=ZK8(JCARA)
             GOTO 39
             ENDIF
           ENDIF
           CALL DISMOI('F','NOM_MODELE',ZK24(JRAID)(1:8),'MATR_ASSE',
     &        IBID,MODELE,IRET)
           CALL DISMOI('F','CHAM_MATER',ZK24(JRAID)(1:8),'MATR_ASSE',
     &        IBID, CHMAT,IRET)
           CALL DISMOI('F','CARA_ELEM',ZK24(JRAID)(1:8),'MATR_ASSE',
     &        IBID,CARAEL,IRET)
        ENDIF
      ENDIF  
C
 39   CONTINUE

      DO 40 IMODE = 1, NBMODE
C
C       STOCKAGE DES FREQUENCES PAR ORDRE CROISSANT DE NUMERO
        IF (IMODE.EQ.1) THEN
           KMODE = IMIN
        ELSEIF (IMODE.EQ.NBMODE) THEN
           KMODE = IMAX
        ELSE
           DO 42 LMODE = 1, NBMODE
              IF ( RESUFI(LMODE,1) .GT. NMIN  .AND.
     &             RESUFI(LMODE,1) .LT. NMIN1 ) THEN
                 NMIN1 = RESUFI(LMODE,1)
                 KMODE = LMODE
              ENDIF
 42        CONTINUE
           NMIN  = NMIN1
           NMIN1 = NMAX
        ENDIF
C
        JMODE = RESUFI(KMODE,1)
        NORDR = IPREC + IMODE
C
C        --- VECTEUR PROPRE ---
        CALL RSEXCH (MODES, NOSY, NORDR, CHAMNO, IER )
        IF     ( IER .EQ. 0   ) THEN
        ELSEIF ( IER .EQ. 100 .AND. LREFD ) THEN
          IF (LNUME) THEN
            CALL VTCREM (CHAMNO, RAIDE, 'G', TYPE(1:1) )
          ELSE
            CALL VTCREB(CHAMNO,NUME,'G',TYPE(1:1),NEQ)
          ENDIF
        ELSE
          CALL UTDEBM('F','VPSTOR'//'.VPSTOR','APPEL ERRONE')
          CALL UTIMPI('L','MODE NUMERO',1,KMODE)
          CALL UTIMPI('L','POSITION MODALE',1,JMODE)
          CALL UTIMPI('L','CODE RETOUR DE RSEXCH :',1,IER)
          CALL UTIMPK('L','PB CHAM_NO',1,CHAMNO)
          CALL UTFINM()
        ENDIF
        IF (TYPCON.EQ.'MODE_GENE' .OR. TYPCON.EQ.'HARM_GENE') THEN
           CALL JEECRA (CHAMNO//'.DESC','DOCU',IBID,'VGEN')
        ENDIF
        CALL JEVEUO (CHAMNO//'.VALE', 'E', LVALE )
        IF (TYPE(1:1) .EQ. 'R' ) THEN
           DO 44 IEQ = 1, NEQ
              ZR(LVALE+IEQ-1) = VECPR8(IEQ,KMODE)
 44        CONTINUE
        ELSEIF (TYPE(1:1) .EQ. 'C' ) THEN
           DO 46 IEQ = 1, NEQ
              ZC(LVALE+IEQ-1) = VECPC8(IEQ,KMODE)
 46        CONTINUE
        ENDIF
        CALL RSNOCH (MODES, NOSY, NORDR, ' ' )
C
C ----- ON STOCKE 'NUME_MODE'
C
        IRANG = INDK24(NOPARA,NOPAST(1),1,NBPARI)
        IF ( IRANG .GT. 0 ) THEN
           CALL RSADPA(MODES,'E',1,NOPAST(1),NORDR,0,LADPA,K8B)
           ZI(LADPA) = RESUFI(KMODE,IRANG)
        ENDIF
C
C ----- ON STOCKE 'NORME'
C
        IRANG = INDK24(NOPARA(NBPARI+1),NOPAST(2),1,NBPARK)
        IF ( IRANG .GT. 0 ) THEN
           CALL RSADPA(MODES,'E',1,NOPAST(2),NORDR,0,LADPA,K8B)
           ZK24(LADPA) = RESUFK(KMODE,IRANG)
        ENDIF
C
C ----- ON STOCKE : MODELE, CARA_ELEM, CHAM_MATER
C
        IF(LSTOCK)THEN
          CALL RSADPA(MODES,'E',1,'MODELE',NORDR,0,LADPA,K8B)
          ZK8(LADPA)=MODELE
          CALL RSADPA(MODES,'E',1,'CHAMPMAT',NORDR,0,LADPA,K8B)
          ZK8(LADPA)=CHMAT
          CALL RSADPA(MODES,'E',1,'CARAELEM',NORDR,0,LADPA,K8B)
          ZK8(LADPA)=CARAEL
        ENDIF
C
C
C ----- ON STOCKE LES PARAMETRES REELS
C
        IF ( TYPCON .EQ. 'MODE_FLAMB' ) THEN
           CALL RSADPA(MODES,'E',1,'CHAR_CRIT',NORDR,0,LADPA,K8B)
           IF ( NOMCMD.EQ.'NORM_MODE') THEN
             ZR(LADPA) = RESUFR(KMODE,1)
           ELSE
             ZR(LADPA) = RESUFR(KMODE,2)
           ENDIF
        ELSE
          DO 48 I = 3 , NBPAST
            IRANG = INDK24(NOPARA(NBPARI+NBPARK+1),NOPAST(I),1,NBPARR)
            IF ( IRANG .GT. 0 ) THEN
              CALL RSADPA(MODES,'E',1,NOPAST(I),NORDR,0,LADPA,K8B)
              ZR(LADPA) = RESUFR(KMODE,IRANG)
            ENDIF
 48       CONTINUE
        ENDIF
C
 40   CONTINUE
C
      CALL JEDEMA ( )
      END
