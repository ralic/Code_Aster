      SUBROUTINE CRTYPE ( )
      IMPLICIT  NONE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 17/12/2002   AUTEUR CIBHHGB G.BERTRAND 
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
C
C     COMMANDE:  CREA_RESU
C     CREE UNE STRUCTURE DE DONNEE DE TYPE "EVOL_THER"
C                                          "EVOL_VARC"
C                                          "EVOL_ELAS"
C                                          "MULT_ELAS"
C                                          "FOURIER_ELAS"
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      CHARACTER*32       JEXNUM , JEXNOM , JEXATR
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
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER       MXPARA, IBID, IER, LG, ICOMPT, IRET, NBFAC, IOCC,
     +              NUMINI, NUMFIN, N0, N1, N2, N3, NIS, NBINST, IP, 
     +              NBVAL, NUME, IADESC, IGD, L, J, JC, 
     +              JCOOR, IAD, JINST, JVAL, JNOMF,
     +              JDEEQ, LPROL, NBPF, INO, NBV
      PARAMETER   ( MXPARA = 10 )
      INTEGER       IPAR(MXPARA),RSMXNO, NBTROU,JCPT,NBR,IVMX,K
      REAL*8        VALPU(MXPARA), RBID, TPS, PREC
      COMPLEX*16    CBID
      LOGICAL       LNCAS
      CHARACTER*6   TYPEGD
      CHARACTER*8   K8B, RESU, NOMF, NOMA, TYPMOD, CRITER
      CHARACTER*16  NOMP(MXPARA), TYPE, OPER, ACCES, K16B
      CHARACTER*24  CHAMP, NOMCH, K24, LINST, NSYMB, TYPRES,
     &              LISTR8, TYPCHF, TYPCHR, LCPT
C
      DATA          LINST,LISTR8,LCPT/'&&CRTYPE_LINST','&&CRTYPE_LISR8',
     &                                '&&CPT_CRTYPE'/
C ----------------------------------------------------------------------
      CALL JEMARQ()
C
      CALL GETRES ( RESU, TYPE, OPER )
C
      CALL GETFAC ( 'AFFE', NBFAC )
C
      CALL GETVTX ( ' ', 'NOM_CHAM',  1,1,1, NSYMB , N1 )
C
      CALL GETVTX ( ' ', 'TYPE_RESU', 1,1,1, TYPRES, N1 )
C
      CALL JEEXIN ( RESU//'           .DESC', IRET )
      IF ( IRET .EQ. 0 ) THEN
        CALL RSCRSD ( RESU, TYPRES, 100 )
      ENDIF
C
      IF ( TYPRES .EQ. 'EVOL_THER' ) THEN
C        --- ACCES PAR "INST" OU "LIST_INST" ---
         LNCAS  = .FALSE.
         IF ( NSYMB .EQ. 'TEMP' ) THEN
            TYPEGD = 'TEMP_R'
            TYPCHF = 'CHAM_NO_TEMP_F'
            TYPCHR = 'CHAM_NO_TEMP_R'
         ELSEIF ( NSYMB .EQ. 'HYDR_ELGA' ) THEN
            TYPEGD = 'HYDR_R'
            TYPCHF = 'CHAM_ELEM_????_F'
            TYPCHR = 'CHAM_ELEM_HYDR_R'
         ELSEIF ( NSYMB .EQ. 'META_ELGA_TEMP' ) THEN
            TYPEGD = 'VARI_R'
            TYPCHF = 'CHAM_ELEM_????_F'
            TYPCHR = 'CHAM_ELEM_VARI_R'
         ELSE
            CALL UTMESS('F','CRTYPE','POUR UN "EVOL_THER", ON NE TRAI'//
     &    'TE QUE LES NOM_CHAM "TEMP", "HYDR_ELGA" ET "META_ELGA_TEMP"')
         ENDIF
      ELSEIF ( TYPRES .EQ. 'EVOL_VARC' ) THEN
C        --- ACCES PAR "INST" OU "NUME_ORDRE" ---
         LNCAS  = .FALSE.
         TYPEGD = 'IRRA_R'
         TYPCHF = 'CHAM_NO_????_F'         
         TYPCHR = 'CHAM_NO_IRRA_R'         
      ELSEIF ( TYPRES .EQ. 'MULT_ELAS'    .OR.
     +         TYPRES .EQ. 'FOURIER_ELAS'  ) THEN
         LNCAS  = .TRUE.
         TYPEGD = 'DEPL_R'
         TYPCHF = 'CHAM_NO_DEPL_F'
         TYPCHR = 'CHAM_NO_DEPL_R'
      ELSE
         LNCAS  = .FALSE.
         TYPEGD = 'DEPL_R'
         TYPCHF = 'CHAM_NO_DEPL_F'
         TYPCHR = 'CHAM_NO_DEPL_R'
      ENDIF
C
      NUMINI = -1
      ICOMPT = -1
C
      DO 100 IOCC = 1,NBFAC
C
        CALL GETVID ( 'AFFE', 'CHAM_GD', IOCC,1,1, CHAMP, N1 )
C
        CALL DISMOI ('F','TYPE_SUPERVIS',CHAMP,'CHAMP',IBID,K24 ,IER)
        CALL DISMOI ('F','NOM_MAILLA'   ,CHAMP,'CHAMP',IBID,NOMA,IER)
        CALL JEVEUO ( NOMA//'.COORDO    .VALE', 'L', JCOOR )
C
C ----- MOT CLE "NOM_CAS", "NUME_MODE" PRESENT :
C
        IF ( LNCAS ) THEN
           CALL RSORAC (RESU,'LONUTI',IBID,RBID,K8B,CBID,RBID,K8B,
     &                  NUMINI,1,NBTROU)
          NUMINI = NUMINI + 1
          CALL RSEXCH ( RESU, NSYMB, NUMINI, NOMCH, IRET )
          IF ( IRET .EQ. 110 ) THEN
             CALL RSAGSD ( RESU, 0 )
             CALL RSEXCH ( RESU, NSYMB, NUMINI, NOMCH, IRET )
          ELSEIF ( IRET .EQ. 100 ) THEN
          ELSE
            CALL UTMESS('F','CRTYPE','VRAIMENT DESOLE !')
          ENDIF
          CALL COPISD('CHAMP_GD','G',CHAMP(1:19),NOMCH(1:19))
          CALL RSNOCH ( RESU, NSYMB, NUMINI, ' ' )
C
          CALL GETVTX ( 'AFFE', 'NOM_CAS', IOCC,1,1, ACCES, N0 )
          IF ( N0 .NE. 0 ) THEN
            CALL RSADPA ( RESU,'E',1,'NOM_CAS',NUMINI,0,IAD,K8B)
            ZK16(IAD) = ACCES
          ELSE
            CALL GETVIS ('AFFE', 'NUME_MODE', IOCC,1,1, NUME, N0 )
            CALL RSADPA ( RESU,'E',1,'NUME_MODE',NUMINI,0,IAD,K8B)
            ZI(IAD) = NUME
          ENDIF
          IF ( TYPRES .EQ. 'FOURIER_ELAS' ) THEN
            CALL GETVTX ( 'AFFE', 'TYPE_MODE', IOCC,1,1, TYPMOD, N0 )
            CALL RSADPA (RESU,'E',1,'TYPE_MODE',NUMINI,0,IAD,K8B)
            ZK8(IAD) = TYPMOD
          ENDIF
          GOTO 100
        ENDIF
C
C ----- MOT CLE INST PRESENT :
C
        CALL GETVR8 ( 'AFFE', 'INST', IOCC,1,0, RBID, NIS )
        IF ( NIS .NE. 0 ) THEN
          NBINST = -NIS
          CALL WKVECT ( LCPT, 'V V I', NBINST, JCPT )
          CALL WKVECT ( LINST, 'V V R', NBINST, JINST )
          CALL GETVR8 ( 'AFFE','INST', IOCC,1,NBINST, ZR(JINST), N1)
          CALL GETVR8 ( 'AFFE','PRECISION',1,1,1,PREC,IBID)
          CALL GETVTX ( 'AFFE','CRITERE',1,1,1,CRITER,IBID)
          CALL RSORAC ( RESU,'LONUTI',IBID,RBID,K8B,CBID,RBID,K8B,
     &                  NBV,1,IBID)
          IF (NBV .GT. 0) THEN
            CALL RSORAC ( RESU,'INST',IBID,ZR(JINST),K8B,CBID,PREC,
     &                    CRITER,NUME,1,NBR)
          ELSE 
             NBR = 0
          ENDIF   
          IF (NBR.LT.0) THEN
            CALL UTMESS('F','CRTYPE','PLUSIEURS INSTANTS '//
     &                  'CORRESPONDENT A CELUI SPECIFIE SOUS ''AFFE''')
          ELSE IF (NBR.EQ.0) THEN
            ZI(JCPT) = RSMXNO(RESU) + 1
          ELSE 
            ZI(JCPT) = NUME 
          ENDIF
        ELSE
C
C ----- MOT CLE LIST_INST PRESENT :
C
          CALL GETVID ( 'AFFE', 'LIST_INST', IOCC,1,1, LISTR8, N1)
          CALL GETVIS ( 'AFFE', 'NUME_INIT', IOCC,1,1, NUMINI, N2)
          CALL GETVIS ( 'AFFE', 'NUME_FIN' , IOCC,1,1, NUMFIN, N3)
          CALL GETVR8 ( 'AFFE', 'PRECISION',1,1,1,PREC,IBID)
          CALL GETVTX ( 'AFFE', 'CRITERE',1,1,1,CRITER,IBID)
          CALL JELIRA ( LISTR8(1:19)//'.VALE', 'LONMAX', NBVAL, K8B)
          NBINST = NBVAL
          IF ( N3 .GT. 0 ) THEN
            IF ( NUMFIN .LT. NUMINI ) THEN
             CALL UTMESS('F','CRTYPE','NUME_FIN INFERIEUR A NUME_INIT')
            ENDIF
            NBINST = NUMFIN - NUMINI + 1
            NBINST = MIN( NBINST , NBVAL )
          ELSE
            NBINST = NBVAL
          ENDIF
          CALL WKVECT ( LINST, 'V V R', NBINST, JINST )
          CALL JEVEUO ( LISTR8(1:19)//'.VALE', 'L', JVAL )
          DO 20 J = 1 , NBINST
            ZR(JINST-1+J) = ZR(JVAL-1+J)
 20       CONTINUE
          CALL RSORAC ( RESU,'LONUTI',IBID,RBID,K8B,CBID,RBID,K8B,
     &                  NBV,1,IBID)
          CALL WKVECT ( LCPT, 'V V I', NBINST, JCPT )
          IVMX = RSMXNO(RESU)
          DO 25 K=1,NBINST
            IF (NBV .GT. 0) THEN
              CALL RSORAC ( RESU,'INST',IBID,ZR(JINST+K-1),K8B,CBID,
     &                      PREC,CRITER,NUME,1,NBR)
            ELSE 
              NBR = 0
            ENDIF   
            IF (NBR.LT.0) THEN
               CALL UTMESS('F','CRTYPE','PLUSIEURS INSTANTS '//
     &                     'CORRESPONDENT A CELUI SPECIFIE SOUS AFFE ')
            ELSE IF (NBR.EQ.0) THEN
              ZI(JCPT + K - 1) = IVMX+1
              IVMX = IVMX+1
            ELSE 
              ZI(JCPT + K - 1) = NUME 
            ENDIF
 25       CONTINUE
        ENDIF
C
        DO 3 J = 1 , NBINST
          ICOMPT = ZI(JCPT + J - 1)
          TPS    = ZR(JINST+ J - 1)
          CALL RSEXCH ( RESU, NSYMB, ICOMPT, NOMCH, IRET )
          IF ( IRET .EQ. 110 ) THEN
            CALL RSAGSD ( RESU, 0 )
            CALL RSEXCH ( RESU, NSYMB, ICOMPT, NOMCH, IRET )
          ENDIF
C
          CALL COPISD ( 'CHAMP_GD', 'G', CHAMP(1:19), NOMCH(1:19) )
C
          IF ( K24 .EQ. TYPCHR ) THEN
          ELSE IF ( K24 .EQ. TYPCHF ) THEN
C
C -- CHAM_NO DE FONCTIONS A EVALUER
C  - MODIFICATION DU .DESC : ..._F --> ..._R
C
            CALL JEVEUO ( NOMCH(1:19)//'.DESC', 'E', IADESC )
            CALL JENONU ( JEXNOM('&CATA.GD.NOMGD',TYPEGD), IGD )
            ZI(IADESC-1+1) = IGD
C
            CALL JELIRA ( CHAMP(1:19)//'.VALE', 'LONMAX', LG, K8B )
            CALL JEVEUO ( CHAMP(1:19)//'.VALE', 'E', JNOMF )
            CALL JEDETR ( NOMCH(1:19)//'.VALE' )
            CALL WKVECT ( NOMCH(1:19)//'.VALE', 'G V R', LG, JC )
            CALL JEVEUO ( CHAMP(1:19)//'.DEEQ', 'L', JDEEQ )
            DO 300 L = 1 , LG
              NOMF = ZK8(JNOMF+L-1)
              IF ( NOMF .EQ. ' ' ) GOTO 300
              CALL JEVEUO ( NOMF//'           .PROL', 'L', LPROL )
              CALL FONBPA ( NOMF, ZK16(LPROL), K16B, MXPARA, NBPF, NOMP)
              INO = ZI(JDEEQ+2*(L-1))
              IF ( INO .EQ. 0 ) GOTO 300
              DO 310 IP = 1,NBPF
                IF (NOMP(IP).EQ.'INST') THEN
                  VALPU(IP) = TPS
                ELSEIF (NOMP(IP).EQ.'X') THEN
                  VALPU(IP) = ZR(JCOOR-1+3*(INO-1)+1)
                ELSEIF (NOMP(IP).EQ.'Y') THEN
                  VALPU(IP) = ZR(JCOOR-1+3*(INO-1)+2)
                ELSEIF (NOMP(IP).EQ.'Z') THEN
                  VALPU(IP) = ZR(JCOOR-1+3*(INO-1)+3)
                ELSE
                  CALL UTMESS('F','CRTYPE','CMP NON TRAITEE')
                ENDIF
                IPAR(IP) = IP
 310          CONTINUE
              IF (ZK16(LPROL).EQ.'INTERPRE') THEN
                 CALL FIINTE('F',NOMF,NBPF,IPAR,VALPU,ZR(JC+L-1),IER)
              ELSE
                 CALL FOINTE('F',NOMF,NBPF,NOMP,VALPU,ZR(JC+L-1),IER)
              ENDIF
300         CONTINUE
          ELSE
             CALL UTMESS('F','CRTYPE','ON NE SAIT PAS TRAITER CE '//
     +                       'TYPE DE CHAMP '//K24)
          ENDIF
          CALL RSNOCH ( RESU, NSYMB, ICOMPT, ' ' )
          CALL RSADPA ( RESU,'E',1,'INST',ICOMPT,0,IAD,K8B)
          ZR(IAD) = TPS
3       CONTINUE
        CALL JEDETR ( LINST )
        CALL JEDETR ( LCPT )
C
 100  CONTINUE
C     ------------------------------------------------------------------
      CALL JEDEMA()
      END
