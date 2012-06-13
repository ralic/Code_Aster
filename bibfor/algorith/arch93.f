      SUBROUTINE ARCH93(RESU,CONCEP,NUME,RAIDE,NBMODD,NBMODF,NBMODA,
     &                  NBMOAD,NBMODI,NBPSMO)
      IMPLICIT NONE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C     ------------------------------------------------------------------
C
C     BUT:
C       OPERATEUR MODE_STATIQUE
C
C
C     ARGUMENTS:
C     ----------
C
C      ENTREE :
C-------------
C
C      SORTIE :
C-------------
C
C ......................................................................
C
C
C
C
      INCLUDE 'jeveux.h'
      INTEGER      IBID,NEQ,IFM,NIV,LMOAD,LMODA,
     &             VALI,IRET, NBMODI,LDDLD,LMODD,LDDLF,LMODF,LVALE,
     &             NIVE, VERSIO,IND,IE,I,IA,ID,IEQ,IERD,IFIN,IM,IMOAD,
     &             IMODA,IMODE,IMODF,IUL,IUNIFI,JAXE,JPARA,LCOEF,LDDAD,
     &             LFREQ,LNOM,LNUME,LRES,LTYPE,NA,NBMOAD,NBMODA,LADPA,
     &             NBMODD,NBMODE,NBMODF,NBPAR,NBPSMO,NBTROU,NNAXE,
     &             NND,LNUMM

      REAL*8       R8B, ZERO, UN, COEF(3),XNORM

      CHARACTER*8  K8B, RESU,MONAXE, FORMAR, CHMAT, CARAEL
      CHARACTER*8  NOMNOE, NOMCMP, KNUM, NOMDIR
      CHARACTER*8  NOSIMP, NOPASE
      CHARACTER*14 NUME
      CHARACTER*16 CONCEP,ACCES(3)
      CHARACTER*19 CHAMNO,RAIDE
      CHARACTER*24 VALE,VALK,MOCB,MOATTA,MOAIMP,MOAUNI,MOINTF,
     &             DDLCB,DDLMN,VEFREQ,DDLAC,MODELE

      COMPLEX*16   C16B

      LOGICAL      DIRECT
      LOGICAL      LBID
      INTEGER      IARG
C
C-----------------------------------------------------------------------
C
      CALL JEMARQ()

C             123456789012345678901234
      DDLCB= '&&OP0093.DDL_STAT_DEPL'
      MOCB=  '&&OP0093.MODE_STAT_DEPL'
      DDLMN= '&&OP0093.DDL_STAT_FORC'
      MOATTA='&&OP0093.MODE_STAT_FORC'
      MOAUNI='&&OP0093.MODE_STAT_ACCU'
      MOAIMP='&&OP0093.MODE_ACCE_IMPO'
      DDLAC= '&&OP0093.DDL_ACCE_IMPO'
      MOINTF='&&MOIN93.MODE_INTF_DEPL'
      VEFREQ='&&MOIN93.FREQ_INTF_DEPL'

C---------------------------C
C--                       --C
C-- STOKAGE DES DEFORMEES --C
C--                       --C
C---------------------------C

      NBMODE = NBMODD + NBMODF + NBMODA + NBMOAD + NBMODI

      CALL DISMOI('F','NB_EQUA',RAIDE,'MATR_ASSE',NEQ,K8B,IERD)
      CALL DISMOI('F','NOM_MODELE',RAIDE,'MATR_ASSE',IBID,MODELE,IERD)
      CALL DISMOI('F','CHAM_MATER',RAIDE,'MATR_ASSE',IBID,CHMAT,IERD)
      CALL DISMOI('F','CARA_ELEM',RAIDE,'MATR_ASSE',IBID,CARAEL,IERD)

      CALL RSCRSD('G',RESU,CONCEP,NBMODE)

      IMODE = 0
      NIVE=3
      UN=1.D0
      ZERO=0.D0
      CALL INFNIV(IFM,NIV)
C--
C-- MODES DE CONTRAINTE
C--
      IF ( NBMODD .GT. 0 ) THEN
         CALL JEVEUO(DDLCB,'L',LDDLD)
         CALL JEVEUO(MOCB,'L',LMODD)

         DO 50 IEQ = 1,NEQ
            IF (ZI(LDDLD+IEQ-1).EQ.1) THEN
               IMODE = IMODE + 1
C              --- LE VECTEUR ---
               CALL RSEXCH(RESU,'DEPL',IMODE,CHAMNO,IERD)
               IF ( IERD .EQ. 100 ) THEN
                  CALL VTCREM(CHAMNO,RAIDE,'G','R')
               ELSE
                  VALI = IERD
                  VALK = CHAMNO
                  CALL U2MESG('F', 'ALGELINE4_38',1,VALK,1,VALI,0,0.D0)
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
C              --- LES PARAMETRES ---
              CALL RGNDAS(NUME,IEQ,NOMNOE,NOMCMP,K8B,K8B,K8B)
               CALL RSADPA(RESU,'E',1,'NOEUD_CMP',IMODE,0,LNOM,K8B)
               ZK16(LNOM) = NOMNOE//NOMCMP
               CALL RSADPA(RESU,'E',1,'NUME_DDL',IMODE,0,LNUME,K8B)
               ZI(LNUME) = IEQ
               CALL RSADPA(RESU,'E',1,'NUME_MODE',IMODE,0,LNUMM,K8B)
               ZI(LNUMM) = IMODE
               CALL RSADPA(RESU,'E',1,'TYPE_DEFO',IMODE,0,LTYPE,K8B)
               ZK16(LTYPE) = 'DEPL_IMPO'
               CALL RSADPA(RESU,'E',1,'TYPE_MODE',IMODE,0,LTYPE,K8B)
               ZK16(LTYPE) = 'MODE_STA'
               CALL RSADPA(RESU,'E',1,'MODELE',IMODE,0,LADPA,K8B)
               ZK8(LADPA) = MODELE(1:8)
               CALL RSADPA(RESU,'E',1,'CHAMPMAT',IMODE,0,LADPA,K8B)
               ZK8(LADPA) = CHMAT
               CALL RSADPA(RESU,'E',1,'CARAELEM',IMODE,0,LADPA,K8B)
               ZK8(LADPA) = CARAEL
            ENDIF
 50      CONTINUE
      ENDIF
C--
C-- MODES D'ATTACHE
C--
      IF ( NBMODF .GT. 0 ) THEN
         IMODF = 0
         CALL JEVEUO(DDLMN,'L',LDDLF)
         CALL JEVEUO(MOATTA,'L',LMODF)
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
                  VALI = IERD
                  VALK = CHAMNO
                  CALL U2MESG('F', 'ALGELINE4_38',1,VALK,1,VALI,0,0.D0)
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
              CALL RGNDAS(NUME,IEQ,NOMNOE,NOMCMP,K8B,K8B,K8B)
               CALL RSADPA(RESU,'E',1,'NOEUD_CMP',IMODE,0,LNOM,K8B)
               ZK16(LNOM) = NOMNOE//NOMCMP
               CALL RSADPA(RESU,'E',1,'NUME_DDL',IMODE,0,LNUME,K8B)
               ZI(LNUME) = IEQ
               CALL RSADPA(RESU,'E',1,'NUME_MODE',IMODE,0,LNUMM,K8B)
               ZI(LNUMM) = IMODE
               CALL RSADPA(RESU,'E',1,'TYPE_DEFO',IMODE,0,LTYPE,K8B)
               ZK16(LTYPE) = 'FORC_IMPO'
               CALL RSADPA(RESU,'E',1,'TYPE_MODE',IMODE,0,LTYPE,K8B)
               ZK16(LTYPE) = 'MODE_STA'
               CALL RSADPA(RESU,'E',1,'MODELE',IMODE,0,LADPA,K8B)
               ZK8(LADPA) = MODELE(1:8)
               CALL RSADPA(RESU,'E',1,'CHAMPMAT',IMODE,0,LADPA,K8B)
               ZK8(LADPA) = CHMAT
               CALL RSADPA(RESU,'E',1,'CARAELEM',IMODE,0,LADPA,K8B)
               ZK8(LADPA) = CARAEL
            ENDIF
 60      CONTINUE
      ENDIF
C--
C-- MODES A ACCELERATION UNIFORME
C--
      IF ( NBMOAD .GT. 0 ) THEN
         IMOAD = 0
         CALL JEVEUO(MOAIMP,'L',LMOAD)
         CALL JEVEUO(DDLAC,'L',LDDAD)
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
                  VALI = IERD
                  VALK = CHAMNO
                  CALL U2MESG('F', 'ALGELINE4_38',1,VALK,1,VALI,0,0.D0)
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
              CALL RGNDAS(NUME,IEQ,NOMNOE,NOMCMP,K8B,K8B,K8B)
               CALL RSADPA(RESU,'E',1,'NOEUD_CMP',IMODE,0,LNOM,K8B)
               ZK16(LNOM) = NOMNOE//NOMCMP
               CALL RSADPA(RESU,'E',1,'NUME_DDL',IMODE,0,LNUME,K8B)
               ZI(LNUME) = IEQ
               CALL RSADPA(RESU,'E',1,'NUME_MODE',IMODE,0,LNUMM,K8B)
               ZI(LNUMM) = IMODE
               CALL RSADPA(RESU,'E',1,'TYPE_DEFO',IMODE,0,LTYPE,K8B)
               ZK16(LTYPE) = 'ACCE_DDL_IMPO'
               CALL RSADPA(RESU,'E',1,'TYPE_MODE',IMODE,0,LTYPE,K8B)
               ZK16(LTYPE) = 'MODE_STA'
               CALL RSADPA(RESU,'E',1,'MODELE',IMODE,0,LADPA,K8B)
               ZK8(LADPA) = MODELE(1:8)
               CALL RSADPA(RESU,'E',1,'CHAMPMAT',IMODE,0,LADPA,K8B)
               ZK8(LADPA) = CHMAT
               CALL RSADPA(RESU,'E',1,'CARAELEM',IMODE,0,LADPA,K8B)
               ZK8(LADPA) = CARAEL
            ENDIF
 66      CONTINUE
      ENDIF
C--
C-- MODES A ACCELERATION IMPOSEE
C--
      IF ( NBMODA .GT. 0 ) THEN

         CALL JEVEUO(MOAUNI,'L',LMODA)
         IMODA = 0
         DO 70 I = 1,NBPSMO
            DIRECT = .FALSE.
            CALL GETVTX('PSEUDO_MODE','AXE',I,IARG,0,MONAXE,NA)
            IF (NA.NE.0) THEN
               NNAXE = -NA
               CALL WKVECT('&&OP0093.AXE','V V K8',NNAXE,JAXE)
               CALL GETVTX('PSEUDO_MODE','AXE',I,IARG,NNAXE,
     &                     ZK8(JAXE),NA)
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
               CALL GETVR8('PSEUDO_MODE','DIRECTION',I,IARG,3,COEF,NA)
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
                 CALL GETVTX('PSEUDO_MODE','NOM_DIR',I,IARG,1,
     &                       NOMDIR,NND)
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
                  VALI = IERD
                  VALK = CHAMNO
                  CALL U2MESG('F', 'ALGELINE4_38',1,VALK,1,VALI,0,0.D0)
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
               CALL RSADPA(RESU,'E',1,'NUME_MODE',IMODE,0,LNUMM,K8B)
               ZI(LNUMM) = IMODE
               CALL RSADPA(RESU,'E',1,'TYPE_DEFO',IMODE,0,LTYPE,K8B)
               ZK16(LTYPE) = 'ACCE_IMPO'
               CALL RSADPA(RESU,'E',1,'TYPE_MODE',IMODE,0,LTYPE,K8B)
               ZK16(LTYPE) = 'MODE_STA'
               CALL RSADPA(RESU,'E',1,'MODELE',IMODE,0,LADPA,K8B)
               ZK8(LADPA) = MODELE(1:8)
               CALL RSADPA(RESU,'E',1,'CHAMPMAT',IMODE,0,LADPA,K8B)
               ZK8(LADPA) = CHMAT
               CALL RSADPA(RESU,'E',1,'CARAELEM',IMODE,0,LADPA,K8B)
               ZK8(LADPA) = CARAEL
 74         CONTINUE
 70      CONTINUE
      ENDIF
C--
C-- MODES D'INTERFACE
C--
      IF ( NBMODI .GT. 0 ) THEN
         CALL JEVEUO(MOINTF,'L',LMODD)
         CALL JEVEUO(VEFREQ,'L',LFREQ)

         DO 90 IEQ = 1,NBMODI
               IMODE = IMODE + 1
C
C              --- LE VECTEUR ---
               CALL RSEXCH(RESU,'DEPL',IMODE,CHAMNO,IERD)
               IF ( IERD .EQ. 100 ) THEN
                  CALL VTCREM(CHAMNO,RAIDE,'G','R')
               ELSE
                  VALI = IERD
                  VALK = CHAMNO
                  CALL U2MESG('F', 'ALGELINE4_38',1,VALK,1,VALI,0,0.D0)
               ENDIF
               VALE(1:19)  = CHAMNO
               VALE(20:24) = '.VALE'
               CALL JEVEUO(VALE,'E',LVALE)
               IND = NEQ*(IMODE-1)
               DO 100 IE = 0, NEQ-1
                  ZR(LVALE+IE) = ZR(LMODD+IND+IE)
 100           CONTINUE
               CALL JELIBE(VALE)
               CALL RSNOCH(RESU,'DEPL',IMODE,' ')
C
C              --- LES PARAMETRES ---

               CALL RSADPA(RESU,'E',1,'NOEUD_CMP',IMODE,0,LNOM,K8B)
               ZK16(LNOM) = '  '
               CALL RSADPA(RESU,'E',1,'NUME_DDL',IMODE,0,LNUME,K8B)
               ZI(LNUME) = IEQ
               CALL RSADPA(RESU,'E',1,'NUME_MODE',IMODE,0,LNUMM,K8B)
               ZI(LNUMM) = IMODE
               CALL RSADPA(RESU,'E',1,'TYPE_DEFO',IMODE,0,LTYPE,K8B)
               ZK16(LTYPE) = 'DEPL_IMPO'
               CALL RSADPA(RESU,'E',1,'TYPE_MODE',IMODE,0,LTYPE,K8B)
               ZK16(LTYPE) = 'MODE_INT'
               CALL RSADPA(RESU,'E',1,'FREQ',IMODE,0,LTYPE,K8B)
               ZR(LTYPE) = ZR(LFREQ+IEQ-1)
               CALL RSADPA(RESU,'E',1,'MODELE',IMODE,0,LADPA,K8B)
               ZK8(LADPA) = MODELE(1:8)
               CALL RSADPA(RESU,'E',1,'CHAMPMAT',IMODE,0,LADPA,K8B)
               ZK8(LADPA) = CHMAT
               CALL RSADPA(RESU,'E',1,'CARAELEM',IMODE,0,LADPA,K8B)
               ZK8(LADPA) = CARAEL

 90      CONTINUE
      ENDIF
C
      CALL TITRE
C
C     --- ECRITURE EVENTUELLE DES VALEURS ET DES VECTEURS PROPRES ---
      FORMAR = '1PE12.5'
      IF ( NIV. GT. 1 ) THEN
         CALL RSORAC ( RESU, 'LONUTI', IBID, R8B, K8B, C16B, R8B, K8B,
     &                                            NBMODE, 1, NBTROU )
         CALL WKVECT ( '&&OP0093.ECRITURE.RES', 'V V I', NBMODE, LRES )
         CALL RSORAC ( RESU, 'TOUT_ORDRE', IBID, R8B, K8B, C16B, R8B,
     &                                  K8B, ZI(LRES), NBMODE, NBTROU )
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
     &                 LBID,IBID, K8B,' ', NBPAR,
     &                 ZK16(JPARA), NBMODE, ZI(LRES), .TRUE., K8B,
     &                 IBID, K8B, 'T', K8B,.FALSE., IBID, IBID, IBID,
     &                 IBID, IBID, K8B, .FALSE., R8B, .FALSE., R8B,
     &                 .FALSE.,.FALSE., FORMAR,NIVE,VERSIO)
      ENDIF



C     ------------------------------------------------------------------

      CALL JEDEMA()
      END
