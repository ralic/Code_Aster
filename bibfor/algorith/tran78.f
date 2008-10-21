      SUBROUTINE TRAN78(NOMRES,TYPRES,NOMIN,NOMCMD,BASEMO)
      IMPLICIT REAL*8 (A-H,O-Z)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 21/10/2008   AUTEUR NISTOR I.NISTOR 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================

C TOLE CRP_20
C IN  : NOMRES : NOM UTILISATEUR POUR LA COMMANDE REST_COND_TRAN
C IN  : TYPRES : TYPE DE RESULTAT : 'DYNA_TRANS'
C IN  : NOMIN  : NOM UTILISATEUR DU CONCEPT TRAN_GENE AMONT
C IN  : NOMCMD : NOM DE LA COMMANDE : 'REST_COND_TRAN'
C IN  : BASEMO : NOM UTILISATEUR DU CONCEPT MODE_MECA AMONT
C                (SI CALCUL MODAL PAR SOUS-STRUCTURATION)
C                BLANC SINON
C ----------------------------------------------------------------------
C     ----DEBUT DES COMMUNS JEVEUX--------
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
      CHARACTER*24 VALK(2)
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32  JEXATR, JEXNOM, JEXNUM
C     ----FIN DES COMMUNS JEVEUX----------
C ----------------------------------------------------------------------
      PARAMETER     ( MXPARA = 10 )
      INTEGER       IPAR(MXPARA), I, J, ITRESU(8)
      INTEGER       FOCI, FOCF, FOMI, FOMF, FOMO
      INTEGER VALI
      REAL*8        R8B, EPSI, ALPHA, XNORM, DEPL(6)
      COMPLEX*16    CBID
      CHARACTER*1   COLI, K1BID
      CHARACTER*8   K8B, BASEMO, CRIT, GRAN, INTERP, BASEM2,
     &              MAILLA, NOMRES, NOMIN, NOMCMP(6), MONMOT(2),
     &              MATGEN, NOMGD, MACREL, LINTF, NOMNOL, NOGDSI, MAYA
      CHARACTER*14  NUMDDL, NUMGEN
      CHARACTER*16  TYPRES, NOMCMD, NOMP(MXPARA), TYPE(8), TYPCHA,
     &              TYPBAS(8), TYPREP, CONCEP, CHAMP(8)
      CHARACTER*19  FONCT, KINST, KNUME, KREFE, PRCHNO, TRANGE,
     &              TYPREF(8), CHAM19
      CHARACTER*24  MATRIC, CHAMNO, CREFE(2), NOMCHA, OBJVE1,
     &              OBJVE2, OBJVE3, OBJVE4, NOMNOE, NUMEDD, NPRNO
      LOGICAL       LEFFOR, LRPHYS
C     ------------------------------------------------------------------
C      DATA BLANC    /'        '/
      DATA NOMCMP   /'DX      ','DY      ','DZ      ',
     &               'DRX     ','DRY     ','DRZ     '/
C     ------------------------------------------------------------------
      CALL JEMARQ()
C      MODE = BASEMO
      TRANGE = NOMIN
      CALL GETTCO(NOMIN,CONCEP)

C     --- RECUPERATION DES ENTITES DU MAILLAGE SUR LESQUELLES ---
C     ---                PORTE LA RESTITUTION                 ---
C      TOUSNO = .TRUE.
      CALL GETVID ( ' ', 'GROUP_NO', 1,1,0, K8B, N1 )
      CALL GETVID ( ' ', 'NOEUD'   , 1,1,0, K8B, N2 )
      CALL GETVID ( ' ', 'GROUP_MA', 1,1,0, K8B, N3 )
      CALL GETVID ( ' ', 'MAILLE'  , 1,1,0, K8B, N4 )
C      IF ( N1+N2+N3+N4 .NE. 0 ) TOUSNO = .FALSE.
C   
      IF (CONCEP(1:9).EQ.'EVOL_NOLI') THEN
        CALL NOLI75(NOMRES,TRANGE)
         
      ELSE
      
        CALL GETVID (' ','MACR_ELEM_DYNA',1,1,1,MACREL,NMC)
        IF (NMC.NE.0) THEN
          CALL JEVEUO(MACREL//'.MAEL_REFE','L',IADREF)
          BASEMO = ZK24(IADREF)
        ELSE
          CALL GETVID(' ','BASE_MODALE',1,1,1,BASEMO,IBID)
        ENDIF
        CALL RSORAC(BASEMO,'LONUTI',IBID,RBID,K8B,CBID,RBID,
     &               K8B,NBMODE,1,IBID)
        CALL JEVEUO(BASEMO//'           .REFD','L',IADRIF)
        NUMEDD = ZK24(IADRIF+3)
        IF (NMC.NE.0) THEN
          CALL DISMOI('F','NOM_MAILLA',NUMEDD(1:14),'NUME_DDL',IBID,
     &                 MAILLA,IRET)
          LINTF = ZK24(IADRIF+4)
          CALL JELIRA(JEXNUM(LINTF//'.IDC_LINO',1),'LONMAX',
     &                 NBNOE,K8B)
          CALL BMNBMD(BASEMO,'DEFORMEE',NBMDEF)
          NBMDYN = NBMODE-NBMDEF
          CALL JEVEUO(MACREL//'.DESM','L',IADESM)
          NBNDYN = ZI(IADESM+1)-NBNOE
          IF (NBNDYN.NE.0) THEN
            NEC = NBMDYN/NBNDYN
          ELSE
            NEC = 0
          ENDIF
C       CREATION DU TABLEAU NOEUD-COMPOSANTE ASSOCIES AUX MODES
          CALL WKVECT('&&TRAN78.NOECMP','V V K8',2*NBMODE,JNOCMP)
          CALL JEVEUO(MACREL//'.LINO','L',IACONX)
          DO 21 I=1,NBNDYN
          CALL JENUNO(JEXNUM(MAILLA//'.NOMNOE',ZI(IACONX+I-1)),NOMNOL)
          DO 22 J=1,NEC
             ZK8(JNOCMP+2*NEC*(I-1)+2*J-2) = NOMNOL
             ZK8(JNOCMP+2*NEC*(I-1)+2*J-1) = NOMCMP(J)
  22      CONTINUE
  21      CONTINUE
          DO 23 I=NBMDYN+1,NBMODE
             CALL RSADPA(BASEMO,'L',1,'NOEUD_CMP',I,0,LNOCMP,K8B)
             ZK8(JNOCMP+2*I-2) = ZK16(LNOCMP)(1:8)
             ZK8(JNOCMP+2*I-1) = ZK16(LNOCMP)(9:16)
  23      CONTINUE
        ENDIF
        CALL GETVID(' ','NUME_DDL',1,1,1,K8B,IBID)
        IF (IBID.NE.0) THEN
          CALL GETVID(' ','NUME_DDL',1,1,1,NUMEDD,IBID)
          NUMEDD = NUMEDD(1:14)//'.NUME'
        ENDIF
        NUMDDL = NUMEDD(1:14)
        CALL DISMOI('F','NB_EQUA',NUMDDL,'NUME_DDL',NEQ,K8B,IRET)
        CALL WKVECT('&&TRAN78.BASE','V V R',NBMODE*NEQ,IDBASE)
        CALL COPMO2(BASEMO,NEQ,NUMDDL,NBMODE,ZR(IDBASE))
        CALL GETVTX(' ','TOUT_CHAM',1,1,0,K8B,N0)
        IF (N0.NE.0) THEN
          NBCHAM = 3
          CHAMP(1) = 'DEPL'
          CHAMP(2) = 'VITE'
          CHAMP(3) = 'ACCE'
        ELSE
          CALL GETVTX(' ','NOM_CHAM',1,1,0,CHAMP,N1)
          IF (N1.NE.0) THEN
            NBCHAM = -N1
            CALL GETVTX(' ','NOM_CHAM',1,1,NBCHAM,CHAMP,N1)
          ELSE
            CALL U2MESS('A','ALGORITH10_93')
            GOTO 9999
          ENDIF
        ENDIF
        KNUME = '&&TRAN78.NUM_RANG'
        KINST = '&&TRAN78.INSTANT'
        CALL RSTRAN('NON',TRANGE,' ',1,KINST,KNUME,NBINST,IRETOU)
        IF ( IRETOU .NE. 0 ) THEN
          CALL U2MESS('F','UTILITAI4_24')
        ENDIF
        CALL JEEXIN(KINST,IRET )
        IF ( IRET .GT. 0 ) THEN
          CALL JEVEUO( KINST, 'L', JINST )
          CALL JEVEUO( KNUME, 'L', JNUME )
        END IF
        IF (NMC.EQ.0) THEN
           CALL JELIRA(TRANGE//'.DGEN','LONMAX',NBSTO,K8B)
           NBINS2 = NBSTO/NBMODE
           IF (NBINST.GT.NBINS2) NBINST = NBINS2
        ENDIF
C     --- CREATION DE LA SD RESULTAT ---
        CALL RSCRSD('G',NOMRES, TYPRES, NBINST)
C
        IF (NMC.NE.0) THEN
          CALL WKVECT('&&TRAN78.RESTR','V V R',NBMODE,JRESTR)
          CALL RSEXCH (NOMIN,'DEPL',1,CHAM19,IRET )
          CALL DISMOI('F','NOM_MAILLA',CHAM19,'CHAMP',IBID,MAYA,IE)
          CALL DISMOI('F','NOM_GD',CHAM19,'CHAMP',IBID,NOGDSI,IE)
          CALL DISMOI('F','NB_EC',NOGDSI,'GRANDEUR',NEC,K8B,IERD)

          CALL DISMOI('F','PROF_CHNO',CHAM19,'CHAMP',IBID,NPRNO,IE)
          NPRNO = NPRNO(1:19)//'.PRNO'
          CALL JEVEUO(JEXNUM(NPRNO,1),'L',IAPRNO)
        ENDIF
        DO 300 I = 1 , NBCHAM
          IF (NMC.EQ.0) THEN
            IF ( CHAMP(I) .EQ. 'DEPL' ) THEN
              CALL JEVEUO(TRANGE//'.DGEN','L',JRESTR)
            ELSEIF ( CHAMP(I) .EQ. 'VITE' ) THEN
              CALL JEVEUO(TRANGE//'.VGEN','L',JRESTR)
            ELSEIF ( CHAMP(I) .EQ. 'ACCE' ) THEN
              CALL JEVEUO(TRANGE//'.AGEN','L',JRESTR)
            ELSE
              CALL U2MESS('A','ALGORITH10_94')
              GOTO 300
            ENDIF
          ENDIF
          DO 310 IARCH = 1, NBINST
            INUM = ZI(JNUME+IARCH-1)
            IF (CONCEP(1:10).EQ.'DYNA_TRANS') INUM=INUM-1
            IF (NMC.NE.0) THEN
              CALL RSEXCH(NOMIN,CHAMP(I)(1:4),INUM,NOMCHA,IRET)
              NOMCHA = NOMCHA(1:19)//'.VALE'
              CALL JEVEUO(NOMCHA,'L',IVALE)
              DO 24 IM=1,NBMODE
                NOMNOL = ZK8(JNOCMP+2*IM-2)
                CALL JENONU(JEXNOM(MAYA//'.NOMNOE',NOMNOL),INOE)
                IF (ZK8(JNOCMP+2*IM-1).EQ.'DX') ICMP = 1
                IF (ZK8(JNOCMP+2*IM-1).EQ.'DY') ICMP = 2
                IF (ZK8(JNOCMP+2*IM-1).EQ.'DZ') ICMP = 3
                IF (ZK8(JNOCMP+2*IM-1).EQ.'DRX') ICMP = 4
                IF (ZK8(JNOCMP+2*IM-1).EQ.'DRY') ICMP = 5
                IF (ZK8(JNOCMP+2*IM-1).EQ.'DRZ') ICMP = 6
                IDDL = ZI(IAPRNO-1+(NEC+2)*(INOE-1)+1)
                ZR(JRESTR+IM-1) = ZR(IVALE+IDDL-1+ICMP-1)
  24          CONTINUE
            ENDIF
            CALL RSEXCH(NOMRES,CHAMP(I)(1:4),IARCH,CHAMNO,IRET)
            CALL VTCREB(CHAMNO,NUMEDD,'G','R',NEQ)
            CALL JEVEUO(CHAMNO(1:19)//'.VALE','E',LDNEW)
            IF (NMC.EQ.0) THEN
              CALL MDGEPH(NEQ,NBMODE,ZR(IDBASE),
     &                    ZR(JRESTR+(INUM-1)*NBMODE),ZR(LDNEW))
            ELSE
              CALL MDGEPH(NEQ,NBMODE,ZR(IDBASE),ZR(JRESTR),ZR(LDNEW))
            ENDIF
            CALL RSNOCH(NOMRES,CHAMP(I)(1:4),IARCH,' ')
            IF (I.EQ.1) THEN
              CALL RSADPA(NOMRES,'E',1,'INST',IARCH,0,LINST,K8B)
              ZR(LINST) = ZR(JINST+IARCH-1)
            ENDIF
 310      CONTINUE
 300    CONTINUE
        KREFE  = NOMRES
        CALL WKVECT(KREFE//'.REFD','G V K24',6,LREFE)
        ZK24(LREFE  ) = ZK24(IADRIF)
        ZK24(LREFE+1) = ZK24(IADRIF+1)
        ZK24(LREFE+2) = ZK24(IADRIF+2)
        ZK24(LREFE+3) = NUMEDD
        ZK24(LREFE+4) = ZK24(IADRIF+4)
        ZK24(LREFE+5) = ZK24(IADRIF+5)
        CALL JELIBE(KREFE//'.REFD')
      ENDIF

      CALL JEDETC(' ','&&TRAN78',1)
      CALL TITRE
9999  CONTINUE
C
99999 CONTINUE
      CALL JEDEMA()
      END
