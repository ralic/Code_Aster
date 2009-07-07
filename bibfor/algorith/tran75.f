      SUBROUTINE TRAN75(NOMRES,TYPRES,NOMIN,NOMCMD,BASEMO)
      IMPLICIT REAL*8 (A-H,O-Z)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 06/07/2009   AUTEUR COURTOIS M.COURTOIS 
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
C     ------------------------------------------------------------------
C     OPERATEUR DE RETOUR A LA BASE PHYSIQUE A PARTIR DE DONNEES
C     GENERALISEES DANS LE CAS D'UN CALCUL TRANSITOIRE
C     ------------------------------------------------------------------
C IN  : NOMRES : NOM UTILISATEUR POUR LA COMMANDE REST_BASE_PHYS
C IN  : TYPRES : TYPE DE RESULTAT : 'DYNA_TRANS'
C IN  : NOMIN  : NOM UTILISATEUR DU CONCEPT TRAN_GENE AMONT
C IN  : NOMCMD : NOM DE LA COMMANDE : 'REST_BASE_PHYS'
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
      CHARACTER*8   K8B, BLANC, BASEMO, CRIT, GRAN, INTERP, BASEM2,
     &              MAILLA, NOMRES, NOMIN, NOMCMP(6), MODE, MONMOT(2),
     &              MATGEN, NOMGD, MACREL, LINTF, NOMNOL, NOGDSI, MAYA
      CHARACTER*14  NUMDDL, NUMGEN
      CHARACTER*16  TYPRES, NOMCMD, NOMP(MXPARA), TYPE(8), TYPCHA,
     &              TYPBAS(8), TYPREP, CONCEP, CHAMP(8)
      CHARACTER*19  FONCT, KINST, KNUME, KREFE, PRCHNO, TRANGE
      CHARACTER*19  TYPREF(8), CHAM19, PROF
      CHARACTER*24  MATRIC, CHAMNO, CREFE(2), NOMCHA, CHAMN2, OBJVE1
      CHARACTER*24  OBJVE2,OBJVE3,OBJVE4,NOMNOE,NUMEDD,NPRNO,CHMOD
      LOGICAL       TOUSNO, MULTAP, LEFFOR, LRPHYS, PREMS
C     ------------------------------------------------------------------
      DATA BLANC    /'        '/
      DATA CHAMN2   /'&&TRAN75.CHAMN2'/
      DATA NOMCMP   /'DX      ','DY      ','DZ      ',
     &               'DRX     ','DRY     ','DRZ     '/
C     ------------------------------------------------------------------
      CALL JEMARQ()
      MODE = BASEMO
      TRANGE = NOMIN
      CALL GETTCO(NOMIN,CONCEP)

      NOMCHA=' '
      NUMDDL=' '
      PRCHNO=' '
C
C     --- RECHERCHE SI UNE ACCELERATION D'ENTRAINEMENT EXISTE ---
      FONCT = ' '
      NFONCT = 0
      CALL GETVID(' ','ACCE_MONO_APPUI',1,1,1,FONCT,NFONCT)
      IF ( NFONCT .NE. 0 ) THEN
C
         CALL GETVR8 ( ' ', 'DIRECTION', 1,1,0, DEPL, NBD )
         NBDIR = -NBD
         CALL GETVR8 ( ' ', 'DIRECTION', 1,1,NBDIR, DEPL, NBD )
         XNORM = 0.D0
         DO 10 ID = 1,NBDIR
            XNORM = XNORM + DEPL(ID) * DEPL(ID)
 10      CONTINUE
         XNORM = SQRT(XNORM)
         IF (XNORM.LT.R8PREM()) THEN
            CALL U2MESS('F','ALGORITH9_81')
         ENDIF
         DO 12 ID = 1,NBDIR
            DEPL(ID) = DEPL(ID) / XNORM
 12      CONTINUE
C
      ENDIF
C
C     --- RECUPERATION DES ENTITES DU MAILLAGE SUR LESQUELLES ---
C     ---                PORTE LA RESTITUTION                 ---
      TOUSNO = .TRUE.
      CALL GETVTX ( ' ', 'GROUP_NO', 1,1,0, K8B, N1 )
      CALL GETVTX ( ' ', 'NOEUD'   , 1,1,0, K8B, N2 )
      CALL GETVTX ( ' ', 'GROUP_MA', 1,1,0, K8B, N3 )
      CALL GETVTX ( ' ', 'MAILLE'  , 1,1,0, K8B, N4 )
      IF ( N1+N2+N3+N4 .NE. 0 ) TOUSNO = .FALSE.
C
C     --- RECUPERATION DE LA BASE MODALE ---
C

      CALL JEVEUO ( TRANGE//'.DESC', 'L', IADESC )
      NBMODE = ZI(IADESC+1)

C
      IF ( MODE .EQ. BLANC) THEN
         CALL JEVEUO(TRANGE//'.REFD','L',IAREFE)
         MATGEN = ZK24(IAREFE)(1:8)
         BASEMO = ZK24(IAREFE+5)(1:8)
         CALL JEVEUO(BASEMO//'           .REFD','L',IADRIF)
         IF (MATGEN(1:8) .NE. BLANC) THEN
           CALL JEVEUO(MATGEN//'           .REFA','L',J2REFE)
           NUMGEN = ZK24(J2REFE+1)(1:14)
           CALL JEVEUO(NUMGEN//'.NUME.REFN','L',J3REFE)
           CALL GETTCO(ZK24(J3REFE),TYPREP)
           MATRIC = ZK24(IADRIF)
           IF (MATRIC.NE.BLANC) THEN
            CALL DISMOI('F','NOM_NUME_DDL',MATRIC,'MATR_ASSE',IBID,
     &                 NUMDDL,IRET)
           ELSE
            NUMDDL = ZK24(IADRIF+3)(1:14)
           ENDIF
           PRCHNO=NUMDDL//'.NUME'
           CALL DISMOI('F','NOM_GD',NUMDDL,'NUME_DDL',IBID,NOMGD,IE)
           CALL DISMOI('F','NOM_MAILLA',NUMDDL,'NUME_DDL',IBID,
     &                 MAILLA,IRET)
           IF ( TOUSNO ) CALL DISMOI('F','NB_EQUA',NUMDDL,'NUME_DDL',
     &                               NEQ,K8B,IRET)
         ELSE
C          -- POUR LES CALCULS SANS MATRICE GENERALISEE
C             (PROJ_MESU_MODAL)
           MATRIC = ZK24(IADRIF+3)
           IF (MATRIC(1:8) .EQ. BLANC) THEN
             MATRIC=ZK24(IADRIF)
             CALL DISMOI('F','NOM_NUME_DDL',MATRIC,'MATR_ASSE',
     &                              IBID,NUMDDL,IRET)
           ELSE
             NUMDDL = MATRIC(1:8)
           ENDIF
           PRCHNO=NUMDDL//'.NUME'
           CALL JEVEUO(NUMDDL//'.NUME.REFN','L',J3REFE)
           MATRIC = ZK24(J3REFE)
           MAILLA = MATRIC(1:8)
           MATRIC = ZK24(IADRIF)
           IF ( TOUSNO ) CALL DISMOI('F','NB_EQUA',NUMDDL,'NUME_DDL',
     &                              NEQ,K8B,IRET)
         ENDIF
C
         BASEM2 = BASEMO
C

      ELSE
C         --- BASE MODALE CALCULEE PAR SOUS-STRUCTURATION
C
         CALL RSEXCH ( BASEMO, 'DEPL', 1, CHMOD, IRET )
         CHMOD = CHMOD(1:19)//'.REFE'
         CALL DISMOI('F','NOM_GD',CHMOD,'CHAM_NO',IBID,NOMGD,IE)
         CALL DISMOI('F','PROF_CHNO',CHMOD,'CHAM_NO',IBID,PRCHNO,IE)
         CALL JEVEUO ( CHMOD, 'L', LLCHA )
         MAILLA = ZK24(LLCHA)
         CREFE(1) = ZK24(LLCHA)
         CREFE(2) = ZK24(LLCHA+1)
         IF ( TOUSNO ) CALL JELIRA(CREFE(2)(1:19)//'.NUEQ','LONMAX',
     &                             NEQ,K8B)
         BASEM2 = ' '
      ENDIF
C
C --- MULTI-APPUIS
C
      MULTAP = .FALSE.
      CALL GETVTX ( ' ','MULT_APPUI',1,1,1, MONMOT(1), N1 )
      CALL GETVTX ( ' ','CORR_STAT' ,1,1,1, MONMOT(2), N2 )

      IF ( MONMOT(1).EQ.'OUI' .OR. MONMOT(2).EQ.'OUI' ) MULTAP = .TRUE.
C
C     ---   RECUPERATION DES VECTEURS DEPLACEMENT, VITESSE ET   ---
C     --- ACCELERATION GENERALISES SUIVANT LES CHAMPS SOUHAITES ---
      CALL RBPH01 ( TRANGE, NBCHAM, TYPE, ITRESU, NFONCT, BASEM2,
     &              TYPREF, TYPBAS, TOUSNO, MULTAP )
C
C     --- RECUPERATION DES NUMEROS DES NOEUDS ET DES DDLS ASSOCIES ---
C     ---         DANS LE CAS D'UNE RESTITUTION PARTIELLE          ---
C
      IF ( .NOT. TOUSNO ) THEN
         OBJVE1 = '&&TRAN75.NUME_NOEUD  '
         OBJVE2 = '&&TRAN75.NOM_CMP     '
         OBJVE3 = '&&TRAN75.NB_NEQ      '
         OBJVE4 = '&&TRAN75.NUME_DDL    '
         CALL RBPH02(MAILLA,NUMDDL,CHMOD,NOMGD,NEQ,NBNOEU,OBJVE1,
     &                    NCMP, OBJVE2, OBJVE3, OBJVE4 )
         CALL JEVEUO ( OBJVE1, 'L', INUMNO )
         CALL JEVEUO ( OBJVE2, 'L', INOCMP )
         CALL JEVEUO ( OBJVE3, 'L', INOECP )
         CALL JEVEUO ( OBJVE4, 'L', INUDDL )
      ENDIF
C
C --- MULTI-APPUIS : RECUP DE L EXCITATION ET DE PSI*DELTA
C
      IF ( MULTAP ) THEN
         CALL JEVEUO(TRANGE//'.FDEP','L',JNODEP)
         CALL JEVEUO(TRANGE//'.FVIT','L',JNOVIT)
         CALL JEVEUO(TRANGE//'.FACC','L',JNOACC)
         CALL JEVEUO(TRANGE//'.IPSD','L',IPSDEL)
         CALL JELIRA(TRANGE//'.FDEP','LONMAX',NBEXCI,K8B)
         NBEXCI = NBEXCI/2
         IF ( TOUSNO ) THEN
            CALL VTCREB(CHAMN2,NUMDDL,'V','R',NEQ)
            CHAMN2(20:24) = '.VALE'
            CALL JEVEUO(CHAMN2,'E',LVAL2)
            LPSDEL = IPSDEL
         ELSE
            CALL WKVECT('&&TRAN75.PSI_DELTA','V V R',NEQ,JPSDEL)
            IDEC = 0
            DO 100 I = 1,NBNOEU
               DO 102 J = 1,NCMP
                  IF ( ZI(INOECP-1+(I-1)*NCMP+J) .EQ. 1 ) THEN
                     IDEC = IDEC + 1
                     ZR(JPSDEL+IDEC-1) = ZR(IPSDEL+ZI(INUDDL+IDEC-1)-1)
                  ENDIF
 102           CONTINUE
 100        CONTINUE
            CALL WKVECT('&&TRAN75.VAL2','V V R',NEQ,LVAL2)
            LPSDEL = JPSDEL
         ENDIF
      ENDIF
C
C     --- RECUPERATION DES INSTANTS ---
C
      CALL GETVTX ( ' ', 'CRITERE'  , 0,1,1, CRIT   , N1)
      CALL GETVR8 ( ' ', 'PRECISION', 0,1,1, EPSI   , N1)
      CALL GETVTX ( ' ', 'INTERPOL' , 0,1,1, INTERP , N1)
C
      KNUME = '&&TRAN75.NUM_RANG'
      KINST = '&&TRAN75.INSTANT'
      CALL RSTRAN(INTERP,TRANGE,' ',1,KINST,KNUME,NBINST,IRETOU)
      IF ( IRETOU .NE. 0 ) THEN
         CALL U2MESS('F','UTILITAI4_24')
      ENDIF
      CALL JEEXIN ( KINST, IRET )
      IF ( IRET .GT. 0 ) THEN
        CALL JEVEUO ( KINST, 'L', JINST )
        CALL JEVEUO ( KNUME, 'L', JNUME )
      END IF
C
C     --- CREATION DE LA SD RESULTAT ---
      CALL RSCRSD('G', NOMRES, TYPRES, NBINST )
C
C     --- RESTITUTION SUR LA BASE REELLE ---
C
C VERIFICATION QU'IL Y UN DE CES MOTS CLEFS :
C  'LIST_INST', 'LIST_FREQ', 'INST' ou 'FREQ'
C A MOINS QUE L'ON NE SOIT DANS UN CAS DE DOUBLE RESTITUTION
C APRES UNE DOUBLE PROJECTION (PRESENCE DU MOT CLEF 'MODE_MECA')
       FOCI = 0
       FOCF = 0
       FOMI = 0
       FOMF = 0
       FOMO = 0
       CALL GETVID(' ','LIST_INST',0,1,1,K8B,FOCI)
       CALL GETVID(' ','LIST_FREQ',0,1,1,K8B,FOCF)
       CALL GETVR8(' ','INST',0,1,1,R8B,FOMI)
       CALL GETVR8(' ','FREQ',0,1,1,R8B,FOMF)
       CALL GETVID(' ','MODE_MECA',0,1,1,K8B,FOMO)
       IF ((INTERP(1:3).NE.'NON').AND.(FOCI.EQ.0 .AND. FOCF.EQ.0 .AND.
     &     FOMI.EQ.0 .AND. FOMF.EQ.0 .AND. FOMO.EQ.0 )) THEN
          CALL U2MESS('F','ALGORITH10_95')
       ENDIF

       CALL JEVEUO(TRANGE//'.INST','L',IDINSG)
       CALL JELIRA(TRANGE//'.INST','LONMAX',NBINSG,K8B)
       CALL WKVECT('&&TRAN75.VECTGENE','V V R',NBMODE,IDVECG)
       DO 210 ICH = 1,NBCHAM
          LEFFOR=.TRUE.
          IF (TYPE(ICH).EQ.'DEPL'.OR.TYPE(ICH).EQ.'VITE'.OR.
     &        TYPE(ICH).EQ.'ACCE'.OR.TYPE(ICH).EQ.'ACCE_ABSOLU')
     &      LEFFOR=.FALSE.
C
C            --- RECUPERATION DES DEFORMEES MODALES ---
C
          TYPCHA = TYPBAS(ICH)
          CALL RSEXCH(BASEMO,TYPCHA,1,NOMCHA,IRET)
          NOMCHA = NOMCHA(1:19)//'.VALE'
          CALL JEEXIN(NOMCHA,IBID)
          IF (IBID.GT.0) THEN
            NOMCHA(20:24)='.VALE'
          ELSE
            NOMCHA(20:24)='.CELV'
          END IF

          IF (LEFFOR)
     &     CALL JELIRA(NOMCHA,'LONMAX',NEQ,K1BID)
          CALL WKVECT('&&TRAN75.BASE','V V R',NBMODE*NEQ,IDBASE)
          IF ( TOUSNO ) THEN
            IF (MODE.EQ.BLANC.AND.MATRIC.EQ.BLANC.AND.
     &         TYPCHA.EQ.'DEPL') THEN
             CALL COPMO2(BASEMO,NEQ,NUMDDL,NBMODE,ZR(IDBASE))
            ELSE
             CALL COPMOD(BASEMO,TYPCHA,NEQ,PRCHNO,NBMODE,ZR(IDBASE))
            ENDIF
          ELSE
            DO 110 J = 1,NBMODE
               CALL RSEXCH ( BASEMO, TYPCHA, J, NOMCHA, IRET )
               IF ( IRET .NE. 0 ) THEN
                  VALK (1) = TYPCHA
                  VALK (2) = BASEMO
                  VALI = J
      CALL U2MESG('F', 'ALGORITH12_66',2,VALK,1,VALI,0,0.D0)
               ENDIF
               CALL JEEXIN ( NOMCHA(1:19)//'.VALE', IBID )
               IF (IBID.GT.0) THEN
                  NOMCHA(20:24)='.VALE'
               ELSE
                  NOMCHA(20:24)='.CELV'
               END IF
               CALL JEVEUO(NOMCHA,'L',IDEFM)
               IDEC = 0
               DO 120 I = 1,NBNOEU
                  DO 122 JC = 1,NCMP
                     IF ( ZI(INOECP-1+(I-1)*NCMP+JC) .EQ. 1 ) THEN
                        IDEC = IDEC + 1
                        ZR(IDBASE+(J-1)*NEQ+IDEC-1) =
     &                                    ZR(IDEFM+ZI(INUDDL+IDEC-1)-1)
                     ENDIF
 122              CONTINUE
 120           CONTINUE
 110        CONTINUE
          ENDIF
          IARCHI = 0
          IF (INTERP(1:3).EQ.'NON') THEN
            CALL JEEXIN(TRANGE//'.ORDR',IRET)
            IF (IRET.NE.0 .AND. ZI(JNUME).EQ.1) IARCHI = -1
          ENDIF
          IDRESU = ITRESU(ICH)
          PREMS=.TRUE.
          DO 200 I = 0,NBINST-1
             IARCHI = IARCHI + 1
             CALL RSEXCH(NOMRES,TYPE(ICH),IARCHI,CHAMNO,IRET)
             IF ( IRET .EQ. 0 ) THEN
             CALL U2MESK('A','ALGORITH2_64',1,CHAMNO)
             ELSEIF ( IRET .EQ. 100 ) THEN
               IF ( TOUSNO ) THEN
                 IF (MODE.EQ.BLANC) THEN
                   IF (LEFFOR) THEN
                     CALL VTDEFS(CHAMNO,TYPREF(ICH),'G','R')
                   ELSE
                     CALL VTCREB(CHAMNO,NUMDDL,'G','R',NEQ)
                   ENDIF
                 ELSE
                   CALL VTCREC(CHAMNO,CHMOD,'G','R',NEQ)
                 ENDIF
               ELSE
                  IF(PREMS) THEN
                    PREMS=.FALSE.
                    CALL CNOCRE ( MAILLA, NOMGD, NBNOEU, ZI(INUMNO),
     &                   NCMP, ZK8(INOCMP), ZI(INOECP), 'G',' ',CHAMNO)
                    CALL DISMOI('F','PROF_CHNO',CHAMNO,'CHAM_NO',
     &                           IBID,PROF,IRET)
                  ELSE
                    CALL CNOCRE ( MAILLA, NOMGD, NBNOEU, ZI(INUMNO),
     &                   NCMP,ZK8(INOCMP),ZI(INOECP),'G',PROF,CHAMNO)
                  ENDIF
               ENDIF
             ELSE
                CALL ASSERT(.FALSE.)
             ENDIF
             CHAMNO(20:24) = '.VALE'
             CALL JEEXIN(CHAMNO,IBID)
             IF (IBID.GT.0) THEN
                 CHAMNO(20:24) = '.VALE'
             ELSE
                 CHAMNO(20:24) = '.CELV'
             END IF
             CALL JEVEUO(CHAMNO,'E',LVALE)

             IF (LEFFOR .OR. .NOT.TOUSNO)
     &         CALL JELIRA(CHAMNO,'LONMAX',NEQ,K8B)
             IF (INTERP(1:3).NE.'NON') THEN
               CALL EXTRAC(INTERP,EPSI,CRIT,NBINSG,ZR(IDINSG),
     &               ZR(JINST+I),ZR(IDRESU),NBMODE,ZR(IDVECG), IBID)
               CALL MDGEPH(NEQ,NBMODE,ZR(IDBASE),ZR(IDVECG),ZR(LVALE))
             ELSE
               CALL MDGEPH(NEQ,NBMODE,ZR(IDBASE),
     &                     ZR(IDRESU+(ZI(JNUME+I)-1)*NBMODE),ZR(LVALE))
             ENDIF
             IF ( MULTAP ) THEN
                IF (TYPE(ICH).EQ.'DEPL')
     &           CALL MDGEP3(NEQ,NBEXCI,ZR(LPSDEL),
     &                       ZR(JINST+I),ZK8(JNODEP),ZR(LVAL2))
                IF (TYPE(ICH).EQ.'VITE')
     &           CALL MDGEP3(NEQ,NBEXCI,ZR(LPSDEL),
     &                       ZR(JINST+I),ZK8(JNOVIT),ZR(LVAL2))
                IF (TYPE(ICH).EQ.'ACCE')
     &           CALL MDGEP3(NEQ,NBEXCI,ZR(LPSDEL),
     &                       ZR(JINST+I),ZK8(JNOACC),ZR(LVAL2))
                IF (TYPE(ICH).EQ.'ACCE_ABSOLU')
     &           CALL MDGEP3(NEQ,NBEXCI,ZR(LPSDEL),
     &                       ZR(JINST+I),ZK8(JNOACC),ZR(LVAL2))
                DO 240 IE =1,NEQ
                   ZR(LVALE+IE-1)=ZR(LVALE+IE-1)+ZR(LVAL2+IE-1)
 240            CONTINUE
             ENDIF
C            --- PRISE EN COMPTE D'UNE ACCELERATION D'ENTRAINEMENT
             IF ( TYPE(ICH) .EQ. 'ACCE_ABSOLU'.AND.NFONCT.NE.0 ) THEN
                IRET = 0
                CALL FOINTE('F',FONCT,1,'INST',ZR(JINST+I),ALPHA,IER)
C               --- ACCELERATION ABSOLUE = RELATIVE + ENTRAINEMENT
                CALL WKVECT('&&TRAN75.VECTEUR','V V R',NEQ,JVEC)
                CALL WKVECT('&&TRAN75.DDL','V V I',NEQ*NBDIR,JDDL)
                CALL PTEDDL('NUME_DDL',NUMDDL,NBDIR,NOMCMP,NEQ,ZI(JDDL))
                DO 250 ID = 1 , NBDIR
                   DO 252 IE = 0 , NEQ-1
                      ZR(JVEC+IE) =  ZR(JVEC+IE) +
     &                         ZI(JDDL+NEQ*(ID-1)+IE)*ALPHA*DEPL(ID)
 252               CONTINUE
 250            CONTINUE
                DO 254 IE = 0 , NEQ-1
                   ZR(LVALE+IE) = ZR(LVALE+IE) + ZR(JVEC+IE)
 254            CONTINUE
                CALL JEDETR ('&&TRAN75.VECTEUR')
                CALL JEDETR ('&&TRAN75.DDL')
             ENDIF
             CALL RSNOCH(NOMRES,TYPE(ICH),IARCHI,' ')
             CALL RSADPA(NOMRES,'E',1,'INST',IARCHI,0,LINST,K8B)
             ZR(LINST) = ZR(JINST+I)
 200      CONTINUE
          CALL JEDETR ( '&&TRAN75.BASE' )
 210   CONTINUE
C
C
      KREFE  = NOMRES
      CALL WKVECT(KREFE//'.REFD','G V K24',7,LREFE)
      IF (MODE.EQ.BLANC) THEN
        ZK24(LREFE) = ZK24(IADRIF)
        ZK24(LREFE+1) = ZK24(IADRIF+1)
        ZK24(LREFE+2  ) = ZK24(IADRIF+2)
        ZK24(LREFE+3  ) = ZK24(IADRIF+3)
        ZK24(LREFE+4  ) = ZK24(IADRIF+4)
        ZK24(LREFE+5  ) = ZK24(IADRIF+5)
        ZK24(LREFE+6  ) = ZK24(IADRIF+6)
      ENDIF
      CALL JELIBE(KREFE//'.REFD')
C
 9999 CONTINUE
      CALL JEDETC(' ','&&TRAN75',1)
      CALL TITRE
C
99999 CONTINUE
      CALL JEDEMA()
      END
