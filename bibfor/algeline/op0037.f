      SUBROUTINE OP0037()
      IMPLICIT REAL*8 (A-H,O-Z)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 20/06/2012   AUTEUR ABBAS M.ABBAS 
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
C     OPERATEUR DE NORMALISATION DES MODES
C     ------------------------------------------------------------------
C
C     PARAMETRES "MODE_MECA"
      INCLUDE 'jeveux.h'
      PARAMETER   ( NBPAMI=1 , NBPAMR=15 , NBPAMK=1, NBPAMT=17 )
C     PARAMETRES "MODE_FLAMB"
      PARAMETER   ( NBPAFI=1 , NBPAFR=1  , NBPAFK=1, NBPAFT=3  )
      INTEGER       LMAT(2), IBID, IFM , NIV, LDDL2
      INTEGER VALI
      INTEGER       IRET
      INTEGER       L1,L2,L3,LMASSE,LRAIDE,LAMOR,LDDL
      REAL*8        R8B
      COMPLEX*16    C16B
      LOGICAL       LMASIN, LREFE, LBASM, LAMO, LCMPLX
      CHARACTER*1    TYPMOD
      CHARACTER*24 VALK(4)
      CHARACTER*8   MODEOU, MODEIN, NOMCMP(7), K8B, CMP,
     &                NOMA, MAT1,
     &              MAT2, MAT3
      CHARACTER*14  NUME
      CHARACTER*16  TYPCON, NOMCMD, NORM, NOEUD, NOMSY
      CHARACTER*19  K19B,CHAMNO
      CHARACTER*24  MASSE, AMOR, RAIDE, REFE, METHOD,
     &              KVEC, KVALI, KVALR, KVALK,
     &              NOPARM(NBPAMT), NOPARF(NBPAFT), NOPARA(NBPAMT),
     &              MATE, CARA, MODELE,TYPEBA, OLDNOR
      INTEGER      IARG
C     ------------------------------------------------------------------
      DATA  NOMCMP / 'LAGR', 'DX', 'DY', 'DZ', 'DRX', 'DRY', 'DRZ' /
      DATA  KVEC  / '&&OP0037.VAL_PROPRE'/
      DATA  KVALI / '&&OP0037.GRAN_MODAL_I' /
      DATA  KVALR / '&&OP0037.GRAN_MODAL_R' /
      DATA  KVALK / '&&OP0037.GRAN_MODAL_K' /
      DATA  NOPARM /        'NUME_MODE'       , 'NORME'           ,
     &  'FREQ'            , 'OMEGA2'          , 'AMOR_REDUIT'     ,
     &  'MASS_GENE'       , 'RIGI_GENE'       , 'AMOR_GENE'       ,
     &  'MASS_EFFE_DX'    , 'MASS_EFFE_DY'    , 'MASS_EFFE_DZ'    ,
     &  'FACT_PARTICI_DX' , 'FACT_PARTICI_DY' , 'FACT_PARTICI_DZ' ,
     &  'MASS_EFFE_UN_DX' , 'MASS_EFFE_UN_DY' , 'MASS_EFFE_UN_DZ' /
      DATA  NOPARF / 'NUME_MODE'  , 'NORME'   ,'CHAR_CRIT'   /
C     ------------------------------------------------------------------
C
C     --- RECUPERATION DU RESULTAT ET DU MODE A TRAITER ---
      CALL JEMARQ()
      CALL GETRES( MODEOU, TYPCON, NOMCMD )
      CALL GCUCON( MODEOU, TYPCON, IEX    )
C
      LBASM = .FALSE.
      LAMO  = .FALSE.
      LCMPLX= .FALSE.
      CALL GETVID('  ','MODE',1,IARG,1,MODEIN,L)

      IF ( IEX .GT. 0 ) THEN
         IF ( MODEOU .NE. MODEIN ) THEN
            VALK (1) = MODEOU
            VALK (2) = MODEIN
            CALL U2MESG('F', 'ALGELINE4_33',2,VALK,0,0,0,0.D0)
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
         CALL U2MESK('F','ALGELINE2_33',1,TYPCON)
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
     &                                           K8B,NBMOD,1,NBTROU)
      CALL WKVECT('&&OP0037.NUMERO.ORDRE','V V I',NBMOD,LNUMOR)
      CALL RSORAC(MODEIN,'TOUT_ORDRE',IBID,R8B,K8B,C16B,0.0D0,
     &                                   K8B,ZI(LNUMOR),NBMOD,NBTROU)
      DO 77 IM = 1, NBMOD
        CALL RSEXCH(MODEIN,'DEPL',ZI(LNUMOR+IM-1),K19B,IRET)
        CALL JEEXIN(K19B//'.PAPA',IRET)
        IF (IRET.NE.0) THEN
          CALL JELIRA(K19B//'.PAPA','LONUTI',IVAL,K8B)
          IF (IVAL.NE.0) THEN
            CALL U2MESS('F','ALGELINE2_34')
          ENDIF
        ENDIF

C       ------ AU PASSAGE, ON FAIT UN TEST SUR LE TYPE DES MODES
C              (REEL OU COMPLEXE)
        CALL JELIRA(K19B//'.VALE','TYPE',IBID,TYPMOD)
        IF (TYPMOD .EQ. 'C')   LCMPLX = .TRUE.

 77   CONTINUE
C
C     --- INITIALISATION ---
      NORM  = ' '
      NOEUD = ' '
      TYPEBA = ' '
      NCMP  = 0
      IDEB  = 0
      IFIN  = 0
      LCMP  = 1
      LMAT(1)  = 0
      LMAT(2)  = 0
      LDDL  = 1
      LMASIN = .TRUE.
C
C     --- MATRICES DE REFERENCE DES MODES ---
      LREFE = .TRUE.
      REFE = MODEIN//'           .REFD'
      CALL JEVEUO(REFE,'L',LMODE)
      TYPEBA=ZK24(LMODE+6)
      IF (TYPEBA(1:1).NE.' ') LBASM=.TRUE.

      IF (TYPEBA(1:1).NE.' ') THEN      
        CALL GETVID(' ','RAIDE',0,IARG,1,MAT1,L1)
        CALL GETVID(' ','MASSE',0,IARG,1,MAT2,L2)
        CALL GETVID(' ','AMOR',0,IARG,1,MAT3,L3)
        IF ( (L1*L2) .EQ. 0 )  CALL U2MESS('F','ALGELINE_6')
        MASSE = MAT2
        RAIDE = MAT1
        AMOR  = ' '
        LAMO=.FALSE.
        IF(L3.NE.0) THEN
           LAMO=.TRUE.
           AMOR=MAT3
        ENDIF
      ELSE
        CALL JEVEUO(REFE,'L',LMODE)
        RAIDE = ZK24(LMODE)
        MASSE = ZK24(LMODE+1)
        AMOR  = ZK24(LMODE+2)
        IF (RAIDE.EQ.' ') THEN
           LREFE = .FALSE.
           CALL RSEXCH(MODEIN,'DEPL',1,CHAMNO,IRET)
           REFE = K19B//'.REFE'
           CALL JEVEUO(REFE,'L',LMODE)
           NOMA = ZK24(LMODE  )(1:8)
           NUME = ZK24(LMODE+1)(1:14)
           LMASIN=.FALSE.
           GOTO 100
        ENDIF
      ENDIF
C
C
C     --- NUMEROTATION ASSOCIEE AUX DDL ---
      CALL DISMOI('F','NOM_NUME_DDL',RAIDE,'MATR_ASSE',IBID,NUME,IRET)
      CALL DISMOI('F','NOM_MAILLA'  ,RAIDE,'MATR_ASSE',IBID,NOMA,IRET)
      CALL DISMOI('F','CARA_ELEM'   ,RAIDE,'MATR_ASSE',IBID,CARA,IRET)
      CALL DISMOI('F','CHAM_MATER'  ,RAIDE,'MATR_ASSE',IBID,MATE,IRET)
      CALL DISMOI('F','NOM_MODELE'  ,RAIDE,'MATR_ASSE',IBID,MODELE,IRET)
C
C     --- COMPATIBILITE DES MODES ---
      CALL VPCREA(0,MODEOU,MASSE,AMOR,RAIDE,NUME,IBID)
C
C
C     --- POUR LES MODES DE FLAMBAGE PAS DE MASSE UNITAIRE ---
      IF (TYPCON(1:10).EQ.'MODE_FLAMB') THEN
         XMASTR=1.D0
         LMASIN=.FALSE.
         GOTO 100
      ENDIF
      CALL VPMAIN(MODELE,MATE,CARA,XMASTR,NBPARA)
      IF (XMASTR.LE.R8PREM()) THEN
         LMASIN = .FALSE.
         CALL U2MESS('I','ALGELINE5_58')
         XMASTR = 1.D0
      ENDIF

  100 CONTINUE

C     --- OPTION DE NORMALISATION  ---
      METHOD = '                        '
      CALL GETVTX(' ','NORME',1,IARG,1,NORM,L)
      IF ( L .NE. 0 ) THEN
         IF      ( NORM .EQ. 'MASS_GENE'      ) THEN
C        --- CALCUL DE LA MASSE DU MODELE
            IF (.NOT.LREFE) CALL U2MESS('F','ALGELINE2_35')
            IF ( LBASM .AND. LCMPLX .AND. (AMOR.EQ.' ') ) THEN
               CALL U2MESS('F','ALGELINE_8')
            ENDIF
            METHOD(1:9) = 'MASS_GENE'
            CALL MTDSCR(MASSE)
            CALL JEVEUO(MASSE(1:19)//'.&INT','E',LMAT(1))
            IF (AMOR.NE.' ') THEN
               CALL MTDSCR(AMOR)
               CALL JEVEUO(AMOR(1:19)//'.&INT','E',LMAT(2))
            ENDIF
         ELSE IF ( NORM .EQ. 'RIGI_GENE'  ) THEN
            IF (.NOT.LREFE) CALL U2MESS('F','ALGELINE2_35')
            IF ( LBASM .AND. LCMPLX .AND. (AMOR.EQ.' ') ) THEN
               CALL U2MESS('F','ALGELINE_8')
            ENDIF
            METHOD(1:9) = 'RAID_GENE'
            CALL MTDSCR(RAIDE)
            CALL JEVEUO(RAIDE(1:19)//'.&INT','E',LMAT(1))
            IF (AMOR.NE.' ') THEN
               CALL MTDSCR(MASSE)
               CALL JEVEUO(MASSE(1:19)//'.&INT','E',LMAT(2))
            ENDIF
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
            VALK (1) = NORM
            VALI = IBID
            CALL U2MESG('F', 'ALGELINE4_36',1,VALK,1,VALI,0,0.D0)
         ENDIF
      ENDIF
C
      CALL GETVEM(NOMA,'NOEUD',' ','NOEUD',1,IARG,1,NOEUD,L)
      IF ( L .NE. 0 ) THEN
         NORM   = 'POINT'
         NCMP   = 1
         METHOD(1:6) = 'NOEUD:'
         IDEB = 7
         LG = LXLGUT(NOEUD)
         IFIN = IDEB + LG
         METHOD(IDEB:IFIN) = ' '//NOEUD(1:LG)
         CALL WKVECT('&&OP0037.LISTE.CMP','V V K8',NCMP,LCMP)
         CALL GETVTX(' ','NOM_CMP',1,IARG,1,ZK8(LCMP),L)
         IF (LREFE) THEN
           CALL POSDDL('NUME_DDL',NUME,NOEUD,ZK8(LCMP),NUMNOE,NUMDDL)
         ELSE
           CALL POSDDL('CHAM_NO',CHAMNO,NOEUD,ZK8(LCMP),NUMNOE,NUMDDL)
         ENDIF
         IF (NUMNOE.EQ.0) THEN
            CALL U2MESS('F','ALGELINE2_36')
         ENDIF
         IF (NUMDDL.EQ.0) THEN
            CALL U2MESS('F','ALGELINE2_37')
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
      CALL GETVTX(' ','AVEC_CMP',1,IARG,0,K8B,L)
      IF ( L .NE. 0 ) THEN
         NORM   = 'AVEC_CMP'
         NCMP   = -L
         METHOD(1:9) = 'AVEC_CMP:'
         CALL WKVECT('&&OP0037.LISTE.CMP','V V K8',NCMP,LCMP)
         CALL GETVTX(' ','AVEC_CMP',1,IARG,NCMP,ZK8(LCMP),L)
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
      CALL GETVTX(' ','SANS_CMP',1,IARG,0,K8B,L)
      IF ( L .NE. 0 ) THEN
         NORM   = 'SANS_CMP'
         NCMP   = -L
         METHOD(1:9) = 'SANS_CMP:'
         CALL WKVECT('&&OP0037.LISTE.CMP','V V K8',NCMP,LCMP)
         CALL GETVTX(' ','SANS_CMP',1,IARG,NCMP,ZK8(LCMP),L)
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
         IF(LBASM)THEN
           WRITE(IFM,1040)
           DO 79 IM = 1, NBMOD
             WRITE(IFM,1050) ZI(LNUMOR+IM-1), METHOD
 79        CONTINUE
         ELSE
           WRITE(IFM,1010)
           DO 78 IM = 1, NBMOD
             CALL RSADPA(MODEIN,'L',1,'NORME',ZI(LNUMOR+IM-1),0,
     &               LADPA,K8B)
             OLDNOR = ZK24(LADPA)
             WRITE(IFM,1020) ZI(LNUMOR+IM-1), OLDNOR, METHOD
 78        CONTINUE
         ENDIF
      ENDIF
C
C     --- RECUPERATION DES VECTEURS PROPRES ET DES GRANDEURS MODALES ---

      CALL VPRECU ( MODEIN, NOMSY, NBMOD, ZI(LNUMOR), KVEC,
     &              NBPARA, NOPARA, KVALI, KVALR, KVALK,
     &              NEQ, NBMODE, TYPMOD, NPARI, NPARR, NPARK )
      IF(.NOT.LBASM)THEN
         IF (NPARI.NE.NBPARI) THEN
            CALL U2MESS('F','ALGELINE2_38')
          ENDIF
          IF (NPARR.NE.NBPARR) THEN
            CALL U2MESS('F','ALGELINE2_39')
          ENDIF
          IF (NPARK.NE.NBPARK) THEN
            CALL U2MESS('F','ALGELINE2_40')
          ENDIF
      ENDIF
C
C     --- RECUPERATION DES COMPOSANTES ---
      IF ( NORM.EQ.'AVEC_CMP' .OR. NORM.EQ.'SANS_CMP' .OR.
     &     NORM(1:4).EQ.'EUCL'     ) THEN
         CALL WKVECT('&&OP0037.POSITION.DDL','V V I',NEQ*NCMP,LDDL)
         IF (LREFE) THEN
           CALL PTEDDL('NUME_DDL',NUME,NCMP,ZK8(LCMP),NEQ,ZI(LDDL))
         ELSE
           CALL PTEDDL('CHAM_NO',CHAMNO,NCMP,ZK8(LCMP),NEQ,ZI(LDDL))
         ENDIF
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
            CALL U2MESS('F','ALGELINE2_41')
         ENDIF
      ENDIF
C
C     --- SIGNE DES MODES ---
      ISIGN = 0
      CALL GETFAC ( 'MODE_SIGNE' , MOSIGN )
      IF ( MOSIGN .NE. 0 ) THEN
         CALL GETVEM(NOMA,'NOEUD','MODE_SIGNE','NOEUD',
     &             1,IARG,1,NOEUD,L)
         CALL GETVTX('MODE_SIGNE','NOM_CMP',1,IARG,1,CMP  ,L)
         IF (LREFE) THEN
           CALL POSDDL('NUME_DDL', NUME,NOEUD,CMP,NUMNOE,NUMDDL)
         ELSE
           CALL POSDDL('CHAM_NO', CHAMNO,NOEUD,CMP,NUMNOE,NUMDDL)
         ENDIF
         IF (NUMNOE.EQ.0) THEN
            CALL U2MESS('F','ALGELINE2_42')
         ENDIF
         IF (NUMDDL.EQ.0) THEN
            CALL U2MESS('F','ALGELINE2_37')
         ENDIF
         ISIGN = 1
         CALL GETVTX('MODE_SIGNE','SIGNE',1,IARG,1,K8B,L)
         IF ( K8B(1:7) .EQ. 'NEGATIF' ) ISIGN = -1
         IF ( TYPMOD .EQ. 'C' ) THEN
            ISIGN = 0
            CALL U2MESS('A','ALGELINE2_43')
         ENDIF
      ENDIF
C
      IERD = 1
      CALL WKVECT('&&OP0037.COEF_MODE','V V R',NBMODE,LCOEF)

      CALL JEVEUO ( KVEC , 'E', LMOD  )
      CALL JEVEUO ( KVALI, 'E', LVALI )
      CALL JEVEUO ( KVALR, 'E', LVALR )
      CALL JEVEUO ( KVALK, 'E', LVALK )

      IF(LBASM)THEN
        CALL MTDSCR(MASSE)
        CALL JEVEUO(MASSE(1:19)//'.&INT','E',LMASSE)
        CALL MTDSCR(RAIDE)
        CALL JEVEUO(RAIDE(1:19)//'.&INT','E',LRAIDE)
        IF (LAMO) THEN
          CALL MTDSCR(AMOR)
          CALL JEVEUO(AMOR(1:19)//'.&INT','E',LAMOR)
        ELSE
          LAMOR=0
        END IF

        CALL JEVEUO ( KVEC , 'L', LMOD  )

C       CALCUL DES PARAMETRES GENERALISES
        CALL WKVECT('&&OP0037.POSI.DDL','V V I',NEQ,LDDL2)
        CALL WKVECT('&&OP0037.DDL.BLOQ.CINE','V V I',NEQ,LPROD)
        CALL VPDDL(RAIDE(1:19),MASSE(1:19),NEQ,IB,IB,IB,ZI(LDDL2),
     &           ZI(LPROD),IERD)
        CALL VPPGEN(LMASSE,LAMOR,LRAIDE,ZR(LVALR+3*NBMODE),
     &            ZR(LVALR+5*NBMODE), ZR(LVALR+4*NBMODE),
     &            ZR(LMOD),NEQ,NBMODE,ZI(LPROD))

C        CALCUL DES FACTEURS DE PARTICIPATIONS ET DES MASSES EFFECTIVES
         CALL VPPFAC(LMASSE,ZR(LVALR+3*NBMODE),ZR(LMOD),NEQ,
     &            NBMODE,NBMODE,ZR(LVALR+6*NBMODE), ZR(LVALR+9*NBMODE))
      ENDIF


C     --- NORMALISATION DES MODES ET ARCHIVAGE ---
        ILGCON = LXLGUT(TYPCON)
        IF ( TYPCON(ILGCON-1:ILGCON) .EQ. '_C' ) ILGCON = ILGCON -2
        CALL RSEXIS(MODEOU,IRET)
        IF (IRET.EQ.0) CALL RSCRSD('G',MODEOU,TYPCON(:ILGCON),NBMODE)
        IPREC = 0
        IF ( TYPMOD .EQ. 'R' ) THEN
          IF ( TYPCON(1:10) .EQ. 'MODE_FLAMB' ) THEN
             CALL VPNOR1 ( NORM, NEQ, NBMODE, ZI(LDDL), ZR(LMOD),
     &                     ISIGN, NUMDDL, ZR(LCOEF) )
          ELSE
             IF ( LREFE) THEN
                CALL VPNORM ( NORM,'OUI',LMAT,NEQ,NBMODE,ZI(LDDL),
     &                        ZR(LMOD), ZR(LVALR), LMASIN, XMASTR,
     &                        ISIGN, NUMDDL, ZR(LCOEF),LBASM )
             ELSE
                CALL VPNORM ( NORM,'NON',LMAT,NEQ,NBMODE,ZI(LDDL),
     &                        ZR(LMOD), ZR(LVALR), LMASIN, XMASTR,
     &                        ISIGN, NUMDDL, ZR(LCOEF),LBASM )
             ENDIF
          ENDIF
          CALL VPSTOR ( -1, TYPMOD, MODEOU, NBMODE, NEQ, ZR(LMOD),
     &                  ZC(1),NBMODE,NBPARI,NBPARR,NBPARK,NOPARA,'    ',
     &                  ZI(LVALI),ZR(LVALR),ZK24(LVALK),IPREC)
          CALL VPNOR2 ( MODEOU, NBMODE, ZI(LNUMOR), ZR(LCOEF) )



        ELSEIF ( TYPMOD .EQ. 'C' ) THEN
          CALL WPNORM ( NORM, 'OUI', LMAT, NEQ, NBMODE, ZI(LDDL),
     &                  ZC(LMOD), ZR(LVALR) ,ZR(LCOEF))
          CALL VPSTOR ( -1,TYPMOD,MODEOU,NBMODE,NEQ,ZR(1),
     &               ZC(LMOD),NBMODE,NBPARI,NBPARR,NBPARK,NOPARA,'    ',
     &                 ZI(LVALI),ZR(LVALR),ZK24(LVALK),IPREC )
        ELSE
          CALL U2MESK('F','ALGELINE2_44',1,TYPMOD)
        ENDIF
C
        DO 60 IM = 1,NBMODE
         CALL RSADPA(MODEOU,'E',1,'NORME',ZI(LNUMOR+IM-1),0,LNORM,K8B)
         ZK24(LNORM) = METHOD
 60     CONTINUE

C     --- ON MET UN TITRE ----
      CALL TITRE
C
C
 1000 FORMAT(/,'NORMALISATION DES MODES : ',A8)
 1010 FORMAT(' NUME_ORDRE         ANCIENNE NORME            ',
     &       'NOUVELLE NORME')
 1040 FORMAT(' NUME_ORDRE         NORME            ')
 1050 FORMAT(I12,8(' '),A24)
 1020 FORMAT(I12,A24,A24)
 1030 FORMAT('BANDE DE FREQUENCE VIDE !!!')
 9999 CONTINUE
      CALL JEDEMA()
      END
