      SUBROUTINE OP0037 ( IER )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 15/06/2005   AUTEUR VABHHTS J.PELLET 
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
C     OPERATEUR DE NORMALISATION DES MODES
C     ------------------------------------------------------------------
C
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
C     PARAMETRES "MODE_MECA"
      PARAMETER   ( NBPAMI=8 , NBPAMR=16 , NBPAMK=2, NBPAMT=26 )
C     PARAMETRES "MODE_FLAMB"
      PARAMETER   ( NBPAFI=1 , NBPAFR=2  , NBPAFK=1, NBPAFT=4  )
      INTEGER       LMAT(2), IBID, IFM , NIV
      INTEGER       ADRECG,NRPASS,NBPASS,IRET,IAUX,JAUX,IOCC,LMODS,ISENS
      REAL*8        R8B, PREC
      COMPLEX*16    C16B
      LOGICAL       LMASIN, LREFE
      CHARACTER*1   STATUT, TYPMOD
      CHARACTER*8   MODEOU, MODEIN, NOMCMP(7), MASINE, K8B, CRIT, CMP,
     +              PARAKI(2), VALEKI(2), CTYP, FORMAR, NOMA
      CHARACTER*14  NUME
      CHARACTER*16  TYPCON, NOMCMD, NORM, NOEUD, NOMSY, NOMVAR, OLDNOR
      CHARACTER*19  K19B,NORECG
      CHARACTER*24  MASSE, AMOR, RAIDE, REFE, NOMJV, METHOD,
     +              KVEC, KVALI, KVALR, KVALK,
     +              NOPARM(NBPAMT), NOPARF(NBPAFT), NOPARA(NBPAMT)
C     ------------------------------------------------------------------
      DATA  NOMCMP / 'LAGR', 'DX', 'DY', 'DZ', 'DRX', 'DRY', 'DRZ' /
      DATA  KVEC  / '&&OP0037.VAL_PROPRE'/
      DATA  KVALI / '&&OP0037.GRAN_MODAL_I' /
      DATA  KVALR / '&&OP0037.GRAN_MODAL_R' /
      DATA  KVALK / '&&OP0037.GRAN_MODAL_K' /
      DATA  NOPARM /
     +  'NUME_MODE'       , 'ITER_QR'         , 'ITER_BATHE'      ,
     +  'ITER_ARNO'       , 'ITER_JACOBI'     , 'ITER_SEPARE'     ,
     +  'ITER_AJUSTE'     , 'ITER_INVERSE'    ,
     +  'NORME'           , 'METHODE'         ,
     +  'FREQ'            ,
     +  'OMEGA2'          , 'AMOR_REDUIT'     , 'ERREUR'          ,
     +  'MASS_GENE'       , 'RIGI_GENE'       , 'AMOR_GENE'       ,
     +  'MASS_EFFE_DX'    , 'MASS_EFFE_DY'    , 'MASS_EFFE_DZ'    ,
     +  'FACT_PARTICI_DX' , 'FACT_PARTICI_DY' , 'FACT_PARTICI_DZ' ,
     +  'MASS_EFFE_UN_DX' , 'MASS_EFFE_UN_DY' , 'MASS_EFFE_UN_DZ' /
      DATA  NOPARF /
     +  'NUME_MODE'       ,
     +  'NORME'           ,
     +  'CHAR_CRIT'       , 'ERREUR'          /
C     ------------------------------------------------------------------
C
C     --- RECUPERATION DU RESULTAT ET DU MODE A TRAITER ---
      CALL JEMARQ()
      CALL GETRES( MODEOU, TYPCON, NOMCMD )
C
      CALL GETVID('  ','MODE',1,1,1,MODEIN,L)
      CALL GETCMD( MODEOU, TYPCON, NOMCMD, STATUT,INUMEX )
      IF ( STATUT .EQ. 'M' ) THEN
         IF ( MODEOU .NE. MODEIN ) THEN
            CALL UTDEBM('F',NOMCMD,'LE CONCEPT PRODUIT ETANT '//
     +                  'DEJA EXISTANT, LA NORMALISATION DOIT SE '//
     +                  'FAIRE EN PLACE ET DONC IL EST IMPOSSIBLE D')
            CALL UTIMPK('S','"AVOIR COMME CONCEPT PRODUIT ',1,MODEOU)
            CALL UTIMPK('S','ET ',1,MODEIN)
            CALL UTIMPK('S','COMME CONCEPT D''ENTREE.',0,' ')
            CALL UTIMPK('L','COMME LE DIT LA SAGESSE POPULAIRE, ON'//
     +                  ' NE PEUT AVOIR LE BEURRE ET L''ARGENT DU '//
     +                  'BEURRE (DE CHARENTE POITOU).',0,' ')
            CALL UTFINM()
         ENDIF
      ENDIF
C
      IF ( TYPCON(1:9) .EQ. 'MODE_MECA' ) THEN
         NOMSY = 'DEPL'
         NBPARI = NBPAMI
         NBPARR = NBPAMR
         NBPARK = NBPAMK
         NBPARA = NBPAMT
         DO 1 I = 1 , NBPARA
            NOPARA(I) = NOPARM(I)
 1       CONTINUE
      ELSEIF ( TYPCON(1:11) .EQ. 'MODE_MECA_C' ) THEN
         NOMSY = 'DEPL'
         NBPARI = NBPAMI
         NBPARR = NBPAMR - 9
         NBPARK = NBPAMK
         NBPARA = NBPAMT - 9
         DO 2 I = 1 , NBPARA
            NOPARA(I) = NOPARM(I)
 2       CONTINUE
      ELSEIF ( TYPCON(1:10) .EQ. 'MODE_FLAMB' ) THEN
         NOMSY = 'DEPL'
         NBPARI = NBPAFI
         NBPARR = NBPAFR
         NBPARK = NBPAFK
         NBPARA = NBPAFT
         DO 3 I = 1 , NBPARA
            NOPARA(I) = NOPARF(I)
 3       CONTINUE
      ELSE
         CALL UTMESS('F',NOMCMD,'TYPE DE MODE INCONNU: '//TYPCON)
      ENDIF
C
C
C     ---RECUPERATION DU NIVEAU D'IMPRESSION---
C
      CALL INFMAJ
      CALL INFNIV ( IFM , NIV )
C
      CALL JELIRA ( MODEIN//'           .ORDR', 'LONUTI', IRET, K8B )
C SI LA BANDE DE FREQUENCE EST VIDE, ON NE FAIT RIEN
C  => DIRECT A LA FIN APRES UN PETIT MESSAGE D'INFO
      IF ( IRET .EQ. 0 ) THEN
            IF ( NIV .GE. 1 ) THEN
               WRITE(IFM,1000) MODEIN 
               WRITE(IFM,1030)
            ENDIF
            GOTO 9999
      ENDIF


C     --- PROTECTION DES OBJETS PERES (AYANT GENERE DES OBJETS .PAPA)
C
      CALL RSORAC(MODEIN,'LONUTI',IBID,R8B,K8B,C16B,0.0D0,
     +                                           K8B,NBMOD,1,NBTROU)
      CALL WKVECT('&&OP0037.NUMERO.ORDRE','V V I',NBMOD,LNUMOR)
      CALL RSORAC(MODEIN,'TOUT_ORDRE',IBID,R8B,K8B,C16B,0.0D0,
     +                                   K8B,ZI(LNUMOR),NBMOD,NBTROU)
      DO 77 IM = 1, NBMOD
        CALL RSEXCH(MODEIN,'DEPL',ZI(LNUMOR+IM-1),K19B,IRET)
        CALL JEEXIN(K19B//'.PAPA',IRET)
        IF (IRET.NE.0) THEN
          CALL JELIRA(K19B//'.PAPA','LONUTI',IVAL,K8B)
          IF (IVAL.NE.0) THEN
            CALL UTMESS('F',NOMCMD,
     +                'IL N''EST PAS PERMIS DE MODIFIER UN OBJET PERE')
          ENDIF
        ENDIF
 77   CONTINUE
C
C     --- INITIALISATION ---
      NORM  = ' '
      NOEUD = ' '
      NCMP  = 0
      IDEB  = 0
      IFIN  = 0
      LCMP  = 1
      LMAT(1)  = 0
      LMAT(2)  = 0
      LDDL  = 1
C
C     --- MATRICES DE REFERENCE DES MODES ---
      LREFE = .TRUE.
      REFE = MODEIN//'           .REFD'
      CALL JEEXIN(REFE,IRET)
      IF (IRET.EQ.0) THEN
         LREFE = .FALSE.
         CALL RSEXCH(MODEIN,'DEPL',1,K19B,IRET)
         REFE = K19B//'.REFE'
         CALL JEVEUO(REFE,'L',LMODE)
         NOMA = ZK24(LMODE  )(1:8)
         NUME = ZK24(LMODE+1)(1:14)
         GOTO 100
      ENDIF
      CALL JEVEUO(REFE,'L',LMODE)
      MASSE = ZK24(LMODE  )
      AMOR  = ZK24(LMODE+1)
      RAIDE = ZK24(LMODE+2)
C
C     --- COMPATIBILITE DES MODES ---
      CALL VPCREA(0,MODEOU,MASSE,AMOR,RAIDE,IBID)
C
C     --- NUMEROTATION ASSOCIEE AUX DDL ---
      CALL DISMOI('F','NOM_NUME_DDL',RAIDE,'MATR_ASSE',IBID,NUME,IERD)
      CALL DISMOI('F','NOM_MAILLA'  ,RAIDE,'MATR_ASSE',IBID,NOMA,IERD)
C
  100 CONTINUE
C
C     --- RECUPERATION DE LA MASSE ---
      LMASIN = .FALSE.
      XMASTR = 1.D0
      CALL GETVID('  ','MASS_INER',1,1,1,MASINE,NMI)
      IF ( NMI .NE. 0 ) THEN
         CALL TBLIVA ( MASINE, 1, 'LIEU', IBID, R8B, C16B, NOMA, K8B,
     +              R8B, 'MASSE', K8B, IBID, XMASTR, C16B, K8B, IRET )
         IF ( IRET .EQ. 1 ) THEN
            CALL UTDEBM('F','OP0037', 'ERREUR DANS LES DONNEES' )
            CALL UTIMPK('L','LE PARAMETRE ',1,'LIEU')
            CALL UTIMPK('S','N EXISTE PAS DANS LA TABLE ',1,MASINE)
            CALL UTFINM()
         ELSEIF ( IRET .EQ. 2 ) THEN
            CALL UTDEBM('F','OP0037', 'ERREUR DANS LES DONNEES' )
        CALL UTIMPK('L','LA MASSE N EXISTE PAS DANS LA TABLE ',1,MASINE)
            CALL UTFINM()
         ELSEIF ( IRET .EQ. 3 ) THEN
            PARAKI(1) = 'LIEU'
            PARAKI(2) = 'ENTITE'
            VALEKI(1) = NOMA
            VALEKI(2) = 'TOUT'
            CALL TBLIVA ( MASINE, 2, PARAKI, IBID, R8B, C16B, VALEKI,
     +           K8B, R8B, 'MASSE', K8B, IBID, XMASTR, C16B, K8B, IRET )
            IF ( IRET .NE. 0 ) THEN
               CALL UTDEBM('F','OP0037', 'ERREUR DANS LES DONNEES' )
        CALL UTIMPK('L','LA MASSE N EXISTE PAS DANS LA TABLE ',1,MASINE)
               CALL UTFINM()
            ENDIF
         ENDIF
         LMASIN = .TRUE.
      ENDIF
C
C     --- OPTION DE NORMALISATION  ---
      METHOD = '                        '
      CALL GETVTX(' ','NORME',1,1,1,NORM,L)
      IF ( L .NE. 0 ) THEN
         IF      ( NORM .EQ. 'MASS_GENE'      ) THEN
            IF (.NOT.LREFE) CALL UTMESS('F',NOMCMD,
     +       'MODE NON CALCULE A PARTIR DE MATRICES ASSEMBLEES')
            METHOD(1:9) = 'MASS_GENE'
            CALL MTDSCR(MASSE)
            CALL JEVEUO(MASSE(1:19)//'.&INT','E',LMAT(1))
            IF (AMOR.NE.' ') THEN
               CALL MTDSCR(AMOR)
               CALL JEVEUO(AMOR(1:19)//'.&INT','E',LMAT(2))
            END IF
         ELSE IF ( NORM .EQ. 'RIGI_GENE'  ) THEN
            IF (.NOT.LREFE) CALL UTMESS('F',NOMCMD,
     +       'MODE NON CALCULE A PARTIR DE MATRICES ASSEMBLEES')
            METHOD(1:9) = 'RAID_GENE'
            CALL MTDSCR(RAIDE)
            CALL JEVEUO(RAIDE(1:19)//'.&INT','E',LMAT(1))
            IF (AMOR.NE.' ') THEN
               CALL MTDSCR(MASSE)
               CALL JEVEUO(MASSE(1:19)//'.&INT','E',LMAT(2))
            END IF
         ELSE IF ( NORM .EQ. 'EUCL'      ) THEN
            METHOD(1:4) = 'EUCL'
            NCMP   = 1
            CALL WKVECT('&&OP0037.LISTE.CMP','V V K8',NCMP,LCMP)
            ZK8(LCMP) = NOMCMP(1)
         ELSE IF ( NORM .EQ. 'EUCL_TRAN' ) THEN
            METHOD(1:9) = 'EUCL_TRAN'
            NCMP   = 3
            CALL WKVECT('&&OP0037.LISTE.CMP','V V K8',NCMP,LCMP)
            ZK8(LCMP)   = NOMCMP(2)
            ZK8(LCMP+1) = NOMCMP(3)
            ZK8(LCMP+2) = NOMCMP(4)
         ELSE IF ( NORM .EQ. 'TRAN'      ) THEN
            METHOD(1:4) = 'TRAN'
            NCMP   = 3
            CALL WKVECT('&&OP0037.LISTE.CMP','V V K8',NCMP,LCMP)
            ZK8(LCMP)   = NOMCMP(2)
            ZK8(LCMP+1) = NOMCMP(3)
            ZK8(LCMP+2) = NOMCMP(4)
            NORM   = 'AVEC_CMP'
         ELSE IF ( NORM .EQ. 'ROTA'      ) THEN
            METHOD(1:4) = 'ROTA'
            NCMP   = 3
            CALL WKVECT('&&OP0037.LISTE.CMP','V V K8',NCMP,LCMP)
            ZK8(LCMP)   = NOMCMP(5)
            ZK8(LCMP+1) = NOMCMP(6)
            ZK8(LCMP+2) = NOMCMP(7)
            NORM   = 'AVEC_CMP'
         ELSE IF ( NORM .EQ. 'TRAN_ROTA' ) THEN
            METHOD(1:9) = 'TRAN_ROTA'
            NCMP   = 6
            CALL WKVECT('&&OP0037.LISTE.CMP','V V K8',NCMP,LCMP)
            ZK8(LCMP)   = NOMCMP(2)
            ZK8(LCMP+1) = NOMCMP(3)
            ZK8(LCMP+2) = NOMCMP(4)
            ZK8(LCMP+3) = NOMCMP(5)
            ZK8(LCMP+4) = NOMCMP(6)
            ZK8(LCMP+5) = NOMCMP(7)
            NORM   = 'AVEC_CMP'
         ELSE
            CALL UTDEBM('F',NOMCMD,'L''OPTION DE NORMALISATION')
            CALL UTIMPK('S',' ',1,NORM)
            CALL UTIMPI('S',' N''EST PAS IMPLANTEE.',0,IBID)
            CALL UTFINM()
         ENDIF
      ENDIF
C
      CALL GETVEM(NOMA,'NOEUD',' ','NOEUD',1,1,1,NOEUD,L)
      IF ( L .NE. 0 ) THEN
         NORM   = 'POINT'
         NCMP   = 1
         METHOD(1:6) = 'NOEUD:'
         IDEB = 7
         LG = LXLGUT(NOEUD)
         IFIN = IDEB + LG
         METHOD(IDEB:IFIN) = ' '//NOEUD(1:LG)
         CALL WKVECT('&&OP0037.LISTE.CMP','V V K8',NCMP,LCMP)
         CALL GETVTX(' ','NOM_CMP',1,1,1,ZK8(LCMP),L)
         CALL POSDDL('NUME_DDL', NUME,NOEUD,ZK8(LCMP),NUMNOE,NUMDDL)
         IF (NUMNOE.EQ.0) THEN
            CALL UTMESS('F',NOMCMD//' (ERREUR.1)',
     +                     'NORMALISATION IMPOSSIBLE, LE POINT N''EST'//
     +                     ' PAS PRESENT DANS LE MODELE.')
         ENDIF
         IF (NUMDDL.EQ.0) THEN
            CALL UTMESS('F',NOMCMD//' (ERREUR.1)',
     +                     'NORMALISATION IMPOSSIBLE, LA COMPOSANTE '//
     +                     'N''EST PAS PRESENTE DANS LE MODELE.')
         ENDIF
         IDEB = IFIN + 1
         DO 50 IC = 1,NCMP
            LG = LXLGUT(ZK8(LCMP+IC-1))
            IFIN = IDEB + LG
            IF (IFIN.GT.24) THEN
               METHOD(22:24) = '...'
               GOTO 52
            ENDIF
            METHOD(IDEB:IFIN) = ' '//ZK8(LCMP+IC-1)(1:LG)
            IDEB = IDEB + LG + 1
 50      CONTINUE
 52      CONTINUE
      ENDIF
C
      CALL GETVTX(' ','AVEC_CMP',1,1,0,K8B,L)
      IF ( L .NE. 0 ) THEN
         NORM   = 'AVEC_CMP'
         NCMP   = -L
         METHOD(1:9) = 'AVEC_CMP:'
         CALL WKVECT('&&OP0037.LISTE.CMP','V V K8',NCMP,LCMP)
         CALL GETVTX(' ','AVEC_CMP',1,1,NCMP,ZK8(LCMP),L)
         IDEB = 10
         DO 30 IC = 1,NCMP
            LG = LXLGUT(ZK8(LCMP+IC-1))
            IFIN = IDEB + LG
            IF (IFIN.GT.24) THEN
               METHOD(22:24) = '...'
               GOTO 32
            ENDIF
            METHOD(IDEB:IFIN) = ' '//ZK8(LCMP+IC-1)(1:LG)
            IDEB = IDEB + LG + 1
 30      CONTINUE
 32      CONTINUE
      ENDIF
C
      CALL GETVTX(' ','SANS_CMP',1,1,0,K8B,L)
      IF ( L .NE. 0 ) THEN
         NORM   = 'SANS_CMP'
         NCMP   = -L
         METHOD(1:9) = 'SANS_CMP:'
         CALL WKVECT('&&OP0037.LISTE.CMP','V V K8',NCMP,LCMP)
         CALL GETVTX(' ','SANS_CMP',1,1,NCMP,ZK8(LCMP),L)
         IDEB = 10
         DO 40 IC = 1,NCMP
            LG = LXLGUT(ZK8(LCMP+IC-1))
            IFIN = IDEB + LG
            IF (IFIN.GT.24) THEN
               METHOD(22:24) = '...'
               GOTO 42
            ENDIF
            METHOD(IDEB:IFIN) = ' '//ZK8(LCMP+IC-1)(1:LG)
            IDEB = IDEB + LG + 1
 40      CONTINUE
 42      CONTINUE
      ENDIF
C
      IF ( NIV .GE. 1 ) THEN
         WRITE(IFM,1000) MODEIN
         WRITE(IFM,1010)
         DO 78 IM = 1, NBMOD
           CALL RSADPA(MODEIN,'L',1,'NORME',ZI(LNUMOR+IM-1),0,LADPA,K8B)
           OLDNOR = ZK24(LADPA)
           WRITE(IFM,1020) ZI(LNUMOR+IM-1), OLDNOR, METHOD
 78      CONTINUE
      ENDIF
C
C     --- RECUPERATION DES VECTEURS PROPRES ET DES GRANDEURS MODALES ---
      CALL VPRECU ( MODEIN, NOMSY, NBMOD, ZI(LNUMOR), KVEC,
     +              NBPARA, NOPARA, KVALI, KVALR, KVALK,
     +              NEQ, NBMODE, TYPMOD, NPARI, NPARR, NPARK )
      IF (NPARI.NE.NBPARI) THEN
         CALL UTMESS('F',NOMCMD,'MANQUE DES PARAMETRES ENTIERS')
      ENDIF
      IF (NPARR.NE.NBPARR) THEN
         CALL UTMESS('F',NOMCMD,'MANQUE DES PARAMETRES REELS')
      ENDIF
      IF (NPARK.NE.NBPARK) THEN
         CALL UTMESS('F',NOMCMD,'MANQUE DES PARAMETRES CARACTERES')
      ENDIF
      CALL JEVEUO ( KVEC , 'E', LMOD  )
      CALL JEVEUO ( KVALI, 'E', LVALI )
      CALL JEVEUO ( KVALR, 'E', LVALR )
      CALL JEVEUO ( KVALK, 'E', LVALK )
C
C     --- RECUPERATION DES COMPOSANTES ---
      IF ( NORM.EQ.'AVEC_CMP' .OR. NORM.EQ.'SANS_CMP' .OR.
     +     NORM(1:4).EQ.'EUCL'     ) THEN
         CALL WKVECT('&&OP0037.POSITION.DDL','V V I',NEQ*NCMP,LDDL)
         CALL PTEDDL('NUME_DDL', NUME , NCMP, ZK8(LCMP), NEQ, ZI(LDDL))
         DO 20 IC = 2,NCMP
            IND = (IC-1)*NEQ
            DO 21 IE = 0,NEQ-1
               ZI(LDDL+IE)= MAX(ZI(LDDL+IND+IE),ZI(LDDL+IE))
 21         CONTINUE
 20      CONTINUE
         IF ( NORM.EQ.'SANS_CMP' .OR. NORM.EQ.'EUCL' ) THEN
            DO 22 IE = 0,NEQ-1
               ZI(LDDL+IE)= 1-ZI(LDDL+IE)
 22         CONTINUE
            IF (NORM.EQ.'SANS_CMP') NORM='AVEC_CMP'
         ENDIF
      ELSE IF ( NORM.EQ.'POINT' ) THEN
         CALL WKVECT('&&OP0037.POSITION.DDL','V V I',NEQ,LDDL)
         ZI(LDDL+NUMDDL-1) = 1
         NORM = 'AVEC_CMP'
      ENDIF
C
C     --- CALCUL DU NOMBRE DE COMPOSANTES ACTIVES ---
      IF ( NCMP .GT. 0 ) THEN
         NCMPAC = 0
         DO 120 IEQ =0,NEQ-1
            NCMPAC = NCMPAC + ZI(LDDL+IEQ)
 120     CONTINUE
         IF (NCMPAC .LT. 1 ) THEN
            CALL UTMESS('F',NOMCMD//' (ERREUR.1)',
     +                     'NORMALISATION IMPOSSIBLE,  AUCUNE COMPOSANT'
     +                   //'E N''EST PRESENTE DANS LE MODELE.')
         ENDIF
      ENDIF
C
C     --- SIGNE DES MODES ---
      ISIGN = 0
      CALL GETFAC ( 'MODE_SIGNE' , MOSIGN )
      IF ( MOSIGN .NE. 0 ) THEN
         CALL GETVEM(NOMA,'NOEUD','MODE_SIGNE','NOEUD',
     +             1,1,1,NOEUD,L)
         CALL GETVTX('MODE_SIGNE','NOM_CMP',1,1,1,CMP  ,L)
         CALL POSDDL('NUME_DDL', NUME,NOEUD,CMP,NUMNOE,NUMDDL)
         IF (NUMNOE.EQ.0) THEN
            CALL UTMESS('F',NOMCMD,
     +                     'NORMALISATION IMPOSSIBLE, LE NOEUD N''EST'//
     +                     ' PAS PRESENT DANS LE MODELE.')
         ENDIF
         IF (NUMDDL.EQ.0) THEN
            CALL UTMESS('F',NOMCMD,
     +                     'NORMALISATION IMPOSSIBLE, LA COMPOSANTE '//
     +                     'N''EST PAS PRESENTE DANS LE MODELE.')
         ENDIF
         ISIGN = 1
         CALL GETVTX('MODE_SIGNE','SIGNE',1,1,1,K8B,L)
         IF ( K8B(1:7) .EQ. 'NEGATIF' ) ISIGN = -1
         IF ( TYPMOD .EQ. 'C' ) THEN
            ISIGN = 0
            CALL UTMESS('A',NOMCMD,'ON NE TIENT PAS COMPTE DU MOT '//
     +              'CLE FACTEUR "MODE_SIGNE" POUR DES "MODE_MECA_C"')
         ENDIF
      ENDIF
C
C RECUPERATION INFORMATIONS SENSIBILITE
      CALL GETVID(' ','SENSIBILITE',1,IERD,1,K8B,ISENS)

      NBPASS = 0
      CALL WKVECT('&&OP0037.COEF_MODE','V V R',NBMODE,LCOEF)
      IF (ISENS.GT.0) THEN
        IOCC = 1
        IAUX = 1
        JAUX = 0
        NORECG = '&&OP0037.NORECG'
        CALL PSRESE(' ',IOCC,IAUX,MODEIN,JAUX,NBPASS,NORECG,IRET)
        IF (NBPASS .GT. 0) CALL JEVEUO(NORECG,'L',ADRECG)
      ENDIF

      DO 300 NRPASS = 0,NBPASS 
        IF (NRPASS .GT. 0) THEN
          K19B = ZK24(ADRECG+2*NRPASS-2)(1:19)
          CALL JEDETR(KVEC)
          CALL JEDETR(KVALI)
          CALL JEDETR(KVALR)
          CALL JEDETR(KVALK)
          CALL VPRECU ( K19B, NOMSY, NBMOD, ZI(LNUMOR), KVEC,
     &              NBPARA, NOPARA, KVALI, KVALR, KVALK,
     &              NEQ, NBMODE, TYPMOD, NPARI, NPARR, NPARK )
          CALL JEVEUO ( KVEC , 'E', LMODS )
          CALL JEVEUO ( KVALI, 'E', LVALI )
          CALL JEVEUO ( KVALR, 'E', LVALR )
          CALL JEVEUO ( KVALK, 'E', LVALK )
          MODEOU = K19B
        ENDIF

C     --- NORMALISATION DES MODES ET ARCHIVAGE ---
        ILGCON = LXLGUT(TYPCON)
        IF ( TYPCON(ILGCON-1:ILGCON) .EQ. '_C' ) ILGCON = ILGCON -2
        CALL RSEXIS(MODEOU,IRET)
        IF ( IRET .EQ. 0 ) CALL RSCRSD(MODEOU,TYPCON(:ILGCON),NBMODE)
        IPREC = 0
        IF ( TYPMOD .EQ. 'R' ) THEN
         IF (NRPASS .EQ. 0) THEN
          IF ( TYPCON(1:10) .EQ. 'MODE_FLAMB' ) THEN
             CALL VPNOR1 ( NORM, NEQ, NBMODE, ZI(LDDL), ZR(LMOD),
     +                     ISIGN, NUMDDL, ZR(LCOEF) )
          ELSE
             IF ( LREFE ) THEN
                CALL VPNORM ( NORM,'OUI',LMAT,NEQ,NBMODE,ZI(LDDL),
     +                        ZR(LMOD), ZR(LVALR), LMASIN, XMASTR,
     +                        ISIGN, NUMDDL, ZR(LCOEF) )
             ELSE
                CALL VPNORM ( NORM,'NON',LMAT,NEQ,NBMODE,ZI(LDDL),
     +                        ZR(LMOD), ZR(LVALR), LMASIN, XMASTR,
     +                        ISIGN, NUMDDL, ZR(LCOEF) )
             ENDIF
          ENDIF
          CALL VPSTOR ( -1, TYPMOD, MODEOU, NBMODE, NEQ, ZR(LMOD),
     +                  ZC(1),NBMODE,NBPARI,NBPARR,NBPARK,NOPARA,
     +                  ZI(LVALI),ZR(LVALR),ZK24(LVALK),IPREC)
          CALL VPNOR2 ( MODEOU, NBMODE, ZI(LNUMOR), ZR(LCOEF) )

         ELSE
           DO 200 IM = 1,NBMODE
             DO 210 IEQ = 1,NEQ
               IRET = LMODS-1+NEQ*(IM-1)+IEQ
                     ZR(IRET) = ZR(IRET)*ZR(LCOEF-1+IM)
 210         CONTINUE
 200       CONTINUE
           CALL VPSTOR ( -1,TYPMOD,MODEOU,NBMODE,NEQ,ZR(LMODS),
     +                  ZC(1),NBMODE,NBPARI,NBPARR,NBPARK,NOPARA,
     +                  ZI(LVALI),ZR(LVALR),ZK24(LVALK),IPREC)
           CALL VPNOR2 ( MODEOU,NBMODE,ZI(LNUMOR),ZR(LCOEF))
         ENDIF

        ELSEIF ( TYPMOD .EQ. 'C' ) THEN
         IF (NRPASS .EQ. 0) THEN
          CALL WPNORM ( NORM, 'OUI', LMAT, NEQ, NBMODE, ZI(LDDL),
     +                  ZC(LMOD), ZR(LVALR) ,ZR(LCOEF))
          CALL VPSTOR ( -1,TYPMOD,MODEOU,NBMODE,NEQ,ZR(1),
     +                 ZC(LMOD),NBMODE,NBPARI,NBPARR,NBPARK,NOPARA,
     +                 ZI(LVALI),ZR(LVALR),ZK24(LVALK),IPREC )
         ELSE
           DO 220 IM = 1,NBMODE
             DO 230 IEQ = 1,NEQ
               IRET = LMODS-1+NEQ*(IM-1)+IEQ
               ZC(IRET) = ZC(IRET)*ZR(LCOEF-1+IM)
 230         CONTINUE
 220       CONTINUE
           CALL VPSTOR ( -1,TYPMOD,MODEOU,NBMODE,NEQ,ZR(1),
     +                 ZC(LMODS),NBMODE,NBPARI,NBPARR,NBPARK,NOPARA,
     +                 ZI(LVALI),ZR(LVALR),ZK24(LVALK),IPREC)
         ENDIF
        ELSE
          CALL UTMESS('F',NOMCMD,'"'//TYPMOD//
     +                                '"  TYPE DE MODE NON TRAITE')
        ENDIF
C
        DO 60 IM = 1,NBMODE
         CALL RSADPA(MODEOU,'E',1,'NORME',ZI(LNUMOR+IM-1),0,LNORM,K8B)
         ZK24(LNORM) = METHOD
 60     CONTINUE
C
C FIN BOUCLE SUR LES PARAMETRES SENSIBLES
C
 300  CONTINUE

C     --- ON MET UN TITRE ----
      CALL TITRE
C
C
 1000 FORMAT(/,'NORMALISATION DES MODES : ',A8)
 1010 FORMAT(' NUME_ORDRE         ANCIENNE NORME            ',
     +       'NOUVELLE NORME')
 1020 FORMAT(I12,A24,A24)
 1030 FORMAT('BANDE DE FREQUENCE VIDE !!!')
 9999 CONTINUE
      CALL JEDEMA()
      END
