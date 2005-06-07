      SUBROUTINE OP0093 ( IER )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 05/10/2004   AUTEUR REZETTE C.REZETTE 
C TOLE CRP_20
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
C     ------------------------------------------------------------------
C
C     OPERATEUR :   MODE_STATIQUE
C
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
      INTEGER      IBID, NEQ, LMATR, LMATM, IFM, NIV
      INTEGER      NIVE, VERSIO
      REAL*8       R8B, ZERO, UN, COEF(3)
      CHARACTER*8  K8B, RESU, NOMMA, MONAXE, FORMAR
      CHARACTER*8  NOMNOE, NOMCMP, KNUM, NOMDIR
      CHARACTER*8 NOSIMP, NOPASE
      CHARACTER*14 NUME
      CHARACTER*16 NOMCMD, CONCEP, ACCES(3)
      CHARACTER*19 CHAMNO, RAIDE, RAILDL, MASSE, AMOR
      CHARACTER*24  VALE
      LOGICAL      DEPLIM, FORCIM, ACCUNI, ACCDDL, DIRECT
      LOGICAL      LMOD,LBID
      COMPLEX*16   C16B
C     ------------------------------------------------------------------
      CALL JEMARQ()
C
      MASSE  = ' '
      AMOR   = ' '
      RAIDE  = ' '
      ZERO = 0.D0
      UN = 1.D0
      VERSIO = 0
      LMOD = .FALSE.
      NIVE = 3
      NOSIMP = '        '
      NOPASE = '        '
C
      CALL GETRES(RESU,CONCEP,NOMCMD)
C
      CALL GETVID(' ','MATR_RIGI',1,1,1,RAIDE,NRA)
      CALL GETVID(' ','MATR_MASS',1,1,1,MASSE,NMA)
C
      CALL GETFAC('PSEUDO_MODE',NBPSMO)
      IF (NBPSMO.NE.0) THEN
         IF (NMA.EQ.0) THEN
            CALL UTMESS('F',NOMCMD,'POUR LE MOT CLE FACTEUR '//
     >        ' "PSEUDO_MODE", IL FAUT DONNER LA MATRICE DE MASSE.')
         ENDIF
      ENDIF
C
C     --- COMPATIBILITE DES MODES (DONNEES ALTEREES) ---
      CALL VPCREA(0,RESU,MASSE,AMOR,RAIDE,IBID)
C
      CALL INFMAJ
      CALL INFNIV(IFM,NIV)
C
      CALL DISMOI('F','NOM_MAILLA'  ,RAIDE,'MATR_ASSE',IBID,NOMMA,IERD)
      CALL DISMOI('F','NOM_NUME_DDL',RAIDE,'MATR_ASSE',IBID,NUME ,IERD)
      CALL DISMOI('F','NB_EQUA'     ,RAIDE,'MATR_ASSE',NEQ ,K8B ,IERD)
      RAILDL = '&&OP0093.MATR_FAC'
C
      CALL MTDEFS(RAILDL,RAIDE,'V',' ')
      CALL MTCOPY(RAIDE,RAILDL,IRET)
      CALL MTDSCR(RAILDL)
      CALL JEVEUO(RAILDL(1:19)//'.&INT','E',LMATR)
      CALL TLDLGG(1,LMATR,1,NEQ,0,NDECI,ISINGU,NPVNEG,IRET)
      IF (IRET.EQ.2) THEN
         CALL UTDEBM('F',NOMCMD,'PROBLEME(S) RENCONTRE(S) LORS DE LA')
         CALL UTIMPK('S',' FACTORISATION DE LA MATRICE :',1,RAIDE)
         CALL UTFINM()
      ENDIF
C
C
      NBMODD = 0
      DEPLIM = .FALSE.
      CALL GETFAC ( 'MODE_STAT', NBMOST )
C                    ---------
      IF (NBMOST.NE.0) THEN
         DEPLIM = .TRUE.
         CALL WKVECT('&&OP0093.DDL_STAT_DEPL','V V I',NEQ,LDDLD)
         CALL MSTGET(NOMCMD,RAIDE,'MODE_STAT',NBMOST,ZI(LDDLD))
         DO 10 I = 0,NEQ-1
            NBMODD = NBMODD + ZI(LDDLD+I)
 10      CONTINUE
         CALL WKVECT('&&OP0093.MODE_STAT_DEPL','V V R',NEQ*NBMODD,LMODD)
         CALL MODSTA('DEPL',LMATR,IBID,NUME,ZI(LDDLD),R8B,NEQ,
     >                                                NBMODD,ZR(LMODD))
      ENDIF
C
C
      NBMODF = 0
      FORCIM = .FALSE.
      CALL GETFAC ( 'FORCE_NODALE', NBFONA )
C                    ------------
      IF (NBFONA.NE.0) THEN
         FORCIM = .TRUE.
         CALL WKVECT('&&OP0093.DDL_STAT_FORC','V V I',NEQ,LDDLF)
         CALL MSTGET(NOMCMD,RAIDE,'FORCE_NODALE',NBFONA,ZI(LDDLF))
         DO 20 I = 0,NEQ-1
            NBMODF = NBMODF + ZI(LDDLF+I)
 20      CONTINUE
         CALL WKVECT('&&OP0093.MODE_STAT_FORC','V V R',NEQ*NBMODF,LMODF)
         CALL MODSTA('FORC',LMATR,IBID,NUME,ZI(LDDLF),R8B,NEQ,
     >                                                NBMODF,ZR(LMODF))
      ENDIF
C
C
      NBMOAD = 0
      NBMODA = 0
      ACCUNI = .FALSE.
      ACCDDL = .FALSE.
      CALL GETFAC ( 'PSEUDO_MODE', NBPSMO )
C                    -----------
      IF (NBPSMO.NE.0) THEN
         CALL MTDSCR(MASSE)
         CALL JEVEUO(MASSE(1:19)//'.&INT','E',LMATM)
         DO 30 I = 1,NBPSMO
            CALL GETVTX('PSEUDO_MODE','AXE',I,1,0,K8B,NA)
            IF (NA.NE.0) NBMODA = NBMODA - NA
            CALL GETVR8('PSEUDO_MODE','DIRECTION',I,1,0,R8B,ND)
            IF (ND.NE.0) NBMODA = NBMODA + 1
 30      CONTINUE
C
         IF ( NBMODA .NE. 0 ) THEN
           CALL WKVECT('&&OP0093.COEFFICIENT','V V R',3*NBMODA,JCOEF)
         ENDIF
C
         IMOD = 0
         NBACC = 0
         DO 32 I = 1,NBPSMO
            CALL GETVTX('PSEUDO_MODE','AXE',I,1,0,MONAXE,NA)
            IF (NA.NE.0) THEN
               NBACC = NBACC + 1
               NNAXE = -NA
               ACCUNI = .TRUE.
               CALL WKVECT('&&OP0093.AXE','V V K8',NNAXE,JAXE)
               CALL GETVTX('PSEUDO_MODE','AXE',I,1,NNAXE,ZK8(JAXE),NA)
               DO 34 IA = 1,NNAXE
                  MONAXE = ZK8(JAXE+IA-1)
                  IF (MONAXE(1:1).EQ.'X') THEN
                     IMOD = IMOD + 1
                     IND = 3 * ( IMOD - 1 )
                     ZR(JCOEF+IND+1-1) = UN
                     ZR(JCOEF+IND+2-1) = ZERO
                     ZR(JCOEF+IND+3-1) = ZERO
                  ELSEIF (MONAXE(1:1).EQ.'Y') THEN
                     IMOD = IMOD + 1
                     IND = 3 * ( IMOD - 1 )
                     ZR(JCOEF+IND+1-1) = ZERO
                     ZR(JCOEF+IND+2-1) = UN
                     ZR(JCOEF+IND+3-1) = ZERO
                  ELSEIF (MONAXE(1:1).EQ.'Z') THEN
                     IMOD = IMOD + 1
                     IND = 3 * ( IMOD - 1 )
                     ZR(JCOEF+IND+1-1) = ZERO
                     ZR(JCOEF+IND+2-1) = ZERO
                     ZR(JCOEF+IND+3-1) = UN
                  ENDIF
 34            CONTINUE
               CALL JEDETR('&&OP0093.AXE')
            ENDIF
            CALL GETVR8('PSEUDO_MODE','DIRECTION',I,1,3,COEF,ND)
C              --- ON NORME LA DIRECTION ---
            IF (ND.NE.0) THEN   
               NBACC = NBACC + 1
               ACCUNI = .TRUE.
               XNORM = ZERO
               DO 36 ID = 1,3
                  XNORM = XNORM + COEF(ID)*COEF(ID)
 36            CONTINUE
               IF (XNORM.LE.ZERO) THEN
                  CALL UTMESS('F',NOMCMD,'LA DIRECTION EST NULLE.')
               ENDIF
               XNORM = UN / SQRT(XNORM)
               DO 38 ID = 1,3
                  COEF(ID) = COEF(ID) * XNORM
 38            CONTINUE
               IMOD = IMOD + 1
               IND = 3 * ( IMOD - 1 )
               ZR(JCOEF+IND+1-1) = COEF(1)
               ZR(JCOEF+IND+2-1) = COEF(2)
               ZR(JCOEF+IND+3-1) = COEF(3)
            ENDIF
 32      CONTINUE
C
         IF ( ACCUNI ) THEN
        CALL WKVECT('&&OP0093.MODE_STAT_ACCU','V V R',NEQ*NBMODA,LMODA)
            CALL MODSTA('ACCE',LMATR,LMATM,NUME,IBID,ZR(JCOEF),NEQ,
     >                                                NBMODA,ZR(LMODA))
         ENDIF
C
         IF ( NBACC .NE. NBPSMO ) THEN

            CALL WKVECT('&&OP0093.DDL_ACCE_IMPO','V V I',NEQ,LDDAD)
            ACCDDL = .TRUE.
            CALL MSTGET(NOMCMD,MASSE,'PSEUDO_MODE',NBPSMO,ZI(LDDAD))
            DO 24 II = 0,NEQ-1
               NBMOAD = NBMOAD + ZI(LDDAD+II)
 24         CONTINUE
C
         CALL WKVECT('&&OP0093.MODE_STAT_ACCD','V V R',NEQ*NBMOAD,LMOAD)
           CALL MODSTA('ACCD',LMATR,LMATM,NUME,ZI(LDDAD),R8B,NEQ,
     >                                                NBMOAD,ZR(LMOAD))
         ENDIF
C
      ENDIF
C
      NBMODE = NBMODD + NBMODF + NBMODA + NBMOAD
C
C     --- STOKAGE DES MODES ----
      CALL RSCRSD(RESU,CONCEP,NBMODE)
      IMODE = 0
      IF ( DEPLIM ) THEN
         DO 50 IEQ = 1,NEQ
            IF (ZI(LDDLD+IEQ-1).EQ.1) THEN
               IMODE = IMODE + 1
C
C              --- LE VECTEUR ---
               CALL RSEXCH(RESU,'DEPL',IMODE,CHAMNO,IERD)
               IF ( IERD .EQ. 100 ) THEN
                  CALL VTCREM(CHAMNO,RAIDE,'G','R')
               ELSE
                  CALL UTDEBM('F',NOMCMD,'APPEL ERRONE :')
                  CALL UTIMPI('L','   CODE RETOUR DE RSEXCH :',1,IERD)
                  CALL UTIMPK('L','   PB CHAM_NO',1,CHAMNO)
                  CALL UTFINM()
               ENDIF
               VALE(1:19)  = CHAMNO
               VALE(20:24) = '.VALE'
               CALL JEVEUO(VALE,'E',LVALE)
               IND = NEQ*(IMODE-1)
               DO 52 IE = 0, NEQ-1
                  ZR(LVALE+IE) = ZR(LMODD+IND+IE)
 52            CONTINUE
               CALL JELIBE(VALE)
               CALL RSNOCH(RESU,'DEPL',IMODE,' ')
C
C              --- LES PARAMETRES ---
              CALL RGNDAS('NUME_DDL',NUME,IEQ,NOMNOE,NOMCMP,K8B,K8B,K8B)
               CALL RSADPA(RESU,'E',1,'NOEUD_CMP',IMODE,0,LNOM,K8B)
               ZK16(LNOM) = NOMNOE//NOMCMP
               CALL RSADPA(RESU,'E',1,'NUME_DDL',IMODE,0,LNUME,K8B)
               ZI(LNUME) = IEQ
               CALL RSADPA(RESU,'E',1,'TYPE_DEFO',IMODE,0,LTYPE,K8B)
               ZK16(LTYPE) = 'DEPL_IMPO'
            ENDIF
 50      CONTINUE
      ENDIF
      IF ( FORCIM ) THEN
         IMODF = 0
         DO 60 IEQ = 1,NEQ
            IF (ZI(LDDLF+IEQ-1).EQ.1) THEN
               IMODE = IMODE + 1
               IMODF = IMODF + 1
C
C              --- LE VECTEUR ---
               CALL RSEXCH(RESU,'DEPL',IMODE,CHAMNO,IERD)
               IF ( IERD .EQ. 100 ) THEN
                  CALL VTCREM(CHAMNO,RAIDE,'G','R')
               ELSE
                  CALL UTDEBM('F',NOMCMD,'APPEL ERRONE :')
                  CALL UTIMPI('L','   CODE RETOUR DE RSEXCH :',1,IERD)
                  CALL UTIMPK('L','   PB CHAM_NO',1,CHAMNO)
                  CALL UTFINM()
               ENDIF
               VALE(1:19)  = CHAMNO
               VALE(20:24) = '.VALE'
               CALL JEVEUO(VALE,'E',LVALE)
               IND = NEQ*(IMODF-1)
               DO 62 IE = 0, NEQ-1
                  ZR(LVALE+IE) = ZR(LMODF+IND+IE)
 62            CONTINUE
               CALL JELIBE(VALE)
               CALL RSNOCH(RESU,'DEPL',IMODE,' ')
C
C              --- LES PARAMETRES ---
              CALL RGNDAS('NUME_DDL',NUME,IEQ,NOMNOE,NOMCMP,K8B,K8B,K8B)
               CALL RSADPA(RESU,'E',1,'NOEUD_CMP',IMODE,0,LNOM,K8B)
               ZK16(LNOM) = NOMNOE//NOMCMP
               CALL RSADPA(RESU,'E',1,'NUME_DDL',IMODE,0,LNUME,K8B)
               ZI(LNUME) = IEQ
               CALL RSADPA(RESU,'E',1,'TYPE_DEFO',IMODE,0,LTYPE,K8B)
               ZK16(LTYPE) = 'FORC_IMPO'
            ENDIF
 60      CONTINUE
      ENDIF
      IF ( ACCDDL ) THEN
         IMOAD = 0
         DO 66 IEQ = 1,NEQ
            IF (ZI(LDDAD+IEQ-1).EQ.1) THEN
               IMODE = IMODE + 1
               IMOAD = IMOAD + 1
C
C              --- LE VECTEUR ---
               CALL RSEXCH(RESU,'DEPL',IMODE,CHAMNO,IERD)
               IF ( IERD .EQ. 100 ) THEN
                  CALL VTCREM(CHAMNO,RAIDE,'G','R')
               ELSE
                  CALL UTDEBM('F',NOMCMD,'APPEL ERRONE :')
                  CALL UTIMPI('L','   CODE RETOUR DE RSEXCH :',1,IERD)
                  CALL UTIMPK('L','   PB CHAM_NO',1,CHAMNO)
                  CALL UTFINM()
               ENDIF
               VALE(1:19)  = CHAMNO
               VALE(20:24) = '.VALE'
               CALL JEVEUO(VALE,'E',LVALE)
               IND = NEQ*(IMOAD-1)
               DO 68 IE = 0, NEQ-1
                  ZR(LVALE+IE) = ZR(LMOAD+IND+IE)
 68            CONTINUE
               CALL JELIBE(VALE)
               CALL RSNOCH(RESU,'DEPL',IMODE,' ')
C
C              --- LES PARAMETRES ---
              CALL RGNDAS('NUME_DDL',NUME,IEQ,NOMNOE,NOMCMP,K8B,K8B,K8B)
               CALL RSADPA(RESU,'E',1,'NOEUD_CMP',IMODE,0,LNOM,K8B)
               ZK16(LNOM) = NOMNOE//NOMCMP
               CALL RSADPA(RESU,'E',1,'NUME_DDL',IMODE,0,LNUME,K8B)
               ZI(LNUME) = IEQ
               CALL RSADPA(RESU,'E',1,'TYPE_DEFO',IMODE,0,LTYPE,K8B)
               ZK16(LTYPE) = 'ACCE_DDL_IMPO'
            ENDIF
 66      CONTINUE
      ENDIF
      IF ( ACCUNI ) THEN
         IMODA = 0
         DO 70 I = 1,NBPSMO
            DIRECT = .FALSE.
            CALL GETVTX('PSEUDO_MODE','AXE',I,1,0,MONAXE,NA)
            IF (NA.NE.0) THEN
               NNAXE = -NA
               CALL WKVECT('&&OP0093.AXE','V V K8',NNAXE,JAXE)
               CALL GETVTX('PSEUDO_MODE','AXE',I,1,NNAXE,ZK8(JAXE),NA)
               IFIN = 0
               DO 72 IA = 1,NNAXE
                  MONAXE = ZK8(JAXE+IA-1)
                  IF (MONAXE(1:1).EQ.'X') THEN
                     IFIN = IFIN + 1
                     ACCES(IFIN) = 'ACCE    X       '
                     COEF(1) = UN
                     COEF(2) = ZERO
                     COEF(3) = ZERO
                  ELSEIF (MONAXE(1:1).EQ.'Y') THEN
                     IFIN = IFIN + 1
                     ACCES(IFIN) = 'ACCE    Y       '
                     COEF(1) = ZERO
                     COEF(2) = UN
                     COEF(3) = ZERO
                  ELSEIF (MONAXE(1:1).EQ.'Z') THEN
                     IFIN = IFIN + 1
                     ACCES(IFIN) = 'ACCE    Z       '
                     COEF(1) = ZERO
                     COEF(2) = ZERO
                     COEF(3) = UN
                  ENDIF
 72            CONTINUE
               CALL JEDETR('&&OP0093.AXE')
            ELSE
               CALL GETVR8('PSEUDO_MODE','DIRECTION',I,1,3,COEF,NA)
               IF (NA.NE.0) THEN
C              --- ON NORME LA DIRECTION ---
                 XNORM = ZERO
                 DO 80 ID = 1,3
                   XNORM = XNORM + COEF(ID)*COEF(ID)
 80              CONTINUE
                 XNORM = UN / SQRT(XNORM)
                 DO 82 ID = 1,3
                  COEF(ID) = COEF(ID) * XNORM
 82              CONTINUE
                 CALL GETVTX('PSEUDO_MODE','NOM_DIR'  ,I,1,1,NOMDIR,NND)
                 DIRECT = .TRUE.
                 IFIN = 1
               ELSE
                 GOTO 70
               ENDIF
            ENDIF
            DO 74 IM = 1,IFIN
               IMODE = IMODE + 1
               IMODA = IMODA + 1
C
C              --- LE VECTEUR ---
               CALL RSEXCH(RESU,'DEPL',IMODE,CHAMNO,IERD)
               IF ( IERD .EQ. 100 ) THEN
                  CALL VTCREM(CHAMNO,RAIDE,'G','R')
               ELSE
                  CALL UTDEBM('F',NOMCMD,'APPEL ERRONE :')
                  CALL UTIMPI('L','   CODE RETOUR DE RSEXCH :',1,IERD)
                  CALL UTIMPK('L','   PB CHAM_NO',1,CHAMNO)
                  CALL UTFINM()
               ENDIF
               VALE(1:19)  = CHAMNO
               VALE(20:24) = '.VALE'
               CALL JEVEUO(VALE,'E',LVALE)
               IND = NEQ*(IMODA-1)
               DO 76 IE = 0, NEQ-1
                  ZR(LVALE+IE) = ZR(LMODA+IND+IE)
 76            CONTINUE
               CALL JELIBE(VALE)
               CALL RSNOCH(RESU,'DEPL',IMODE,' ')
C
C              --- LES PARAMETRES ---
               CALL RSADPA(RESU,'E',1,'NOEUD_CMP',IMODE,0,LNOM ,K8B)
               IF ( DIRECT ) THEN
                  IF (NND.EQ.0) THEN
                     CALL CODENT(IMODA,'G',KNUM)
                     NOMDIR = 'DIR_'//KNUM(1:4)
                  ENDIF
                  ZK16(LNOM) = 'ACCE    '//NOMDIR
               ELSE
                  ZK16(LNOM) = ACCES(IM)
                  IF (ACCES(IM).EQ.'ACCE    X       ') THEN
                     COEF(1) = UN
                     COEF(2) = ZERO
                     COEF(3) = ZERO
                  ELSEIF (ACCES(IM).EQ.'ACCE    Y       ') THEN
                     COEF(1) = ZERO
                     COEF(2) = UN
                     COEF(3) = ZERO
                  ELSE
                     COEF(1) = ZERO
                     COEF(2) = ZERO
                     COEF(3) = UN
                  ENDIF
               ENDIF
               CALL RSADPA(RESU,'E',1,'COEF_X',IMODE,0,LCOEF,K8B)
               ZR(LCOEF) = COEF(1)
               CALL RSADPA(RESU,'E',1,'COEF_Y',IMODE,0,LCOEF,K8B)
               ZR(LCOEF) = COEF(2)
               CALL RSADPA(RESU,'E',1,'COEF_Z',IMODE,0,LCOEF,K8B)
               ZR(LCOEF) = COEF(3)
               CALL RSADPA(RESU,'E',1,'TYPE_DEFO',IMODE,0,LTYPE,K8B)
               ZK16(LTYPE) = 'ACCE_IMPO'
 74         CONTINUE
 70      CONTINUE
      ENDIF
C
      CALL TITRE
C
C     --- ECRITURE EVENTUELLE DES VALEURS ET DES VECTEURS PROPRES ---
      FORMAR = '1PE12.5'
      IF ( NIV. GT. 1 ) THEN
         CALL RSORAC ( RESU, 'LONUTI', IBID, R8B, K8B, C16B, R8B, K8B,
     >                                            NBMODE, 1, NBTROU )
         CALL WKVECT ( '&&OP0093.ECRITURE.RES', 'V V I', NBMODE, LRES )
         CALL RSORAC ( RESU, 'TOUT_ORDRE', IBID, R8B, K8B, C16B, R8B,
     >                                  K8B, ZI(LRES), NBMODE, NBTROU )
         CALL IRPARB ( RESU, -1, ' ', '&&OP0093.NOM_PARA', NBPAR )
         CALL JEEXIN ( '&&OP0093.NOM_PARA', IRET )
         IF ( IRET .GT. 0 ) THEN
            CALL JEVEUO ( '&&OP0093.NOM_PARA', 'L', JPARA )
         ELSE
            JPARA = 1
            NBPAR = 0
         END IF
         R8B = 0.D0
         IBID = 0
         K8B = ' '
         IUL = IUNIFI( 'MESSAGE' )
         CALL IRECRI ( RESU,NOSIMP,NOPASE, 'RESULTAT',IUL, K8B,
     >                 LBID,IBID, K8B, NBPAR,
     >                 ZK16(JPARA), NBMODE, ZI(LRES), .TRUE., K8B,
     >                 IBID, K8B, 'T', .FALSE., IBID, IBID, IBID, IBID,
     >                 IBID, K8B, .FALSE., R8B, .FALSE., R8B, .FALSE.,
     >                 .FALSE., FORMAR,LMOD,NIVE,VERSIO)
      ENDIF
C     ------------------------------------------------------------------
      CALL JEDEMA()
      END
