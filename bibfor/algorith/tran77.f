      SUBROUTINE TRAN77(NOMRES,TYPRES,NOMIN,BASEMO)
      IMPLICIT NONE
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 27/08/2012   AUTEUR ALARCON A.ALARCON 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C
C IN  : NOMRES : NOM UTILISATEUR POUR LA COMMANDE REST_SOUS_STRUC
C IN  : TYPRES : TYPE DE RESULTAT : 'DYNA_TRANS'
C IN  : NOMIN  : NOM UTILISATEUR DU CONCEPT TRAN_GENE AMONT
C IN  : BASEMO : NOM UTILISATEUR DU CONCEPT MODE_MECA AMONT
C ----------------------------------------------------------------------
      INCLUDE 'jeveux.h'
      CHARACTER*24 VALK(2)
C ----------------------------------------------------------------------
      INTEGER       I, J, ITRESU(8)
      INTEGER       FOCI, FOCF, FOMI, FOMF, FOMO
      REAL*8        R8B, EPSI
      CHARACTER*1   K1BID
      CHARACTER*8   K8B, BLANC, BASEMO, CRIT, INTERP, BASEM2,
     &              MAILLA, NOMRES, NOMIN, MODE, NOMMA,
     &              MATGEN, NOMGD
      CHARACTER*14  NUMDDL, NUMGEN
      CHARACTER*16  TYPRES, TYPE(8), TYPCHA,
     &              TYPBAS(8), TYPREP, CONCEP
      CHARACTER*19  KINST, KNUME, KREFE, TRANGE,
     &              TYPREF(8), PROF
      CHARACTER*24  MATRIC, CHAMNO, CREFE(2), NOMCHA, OBJVE1,K24BID,
     &              OBJVE2, OBJVE3, OBJVE4
      LOGICAL       TOUSNO, MULTAP, LEFFOR, PREMS
      INTEGER      IARG
C     ------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER IADESC ,IADRIF ,IARCHI ,IAREFE ,IBID ,ICH ,IDBASE 
      INTEGER IDEC ,IDEFM ,IDINSG ,IDRESU ,IDVECG ,IE ,INOCMP 
      INTEGER INOECP ,INUDDL ,INUMNO ,IRET ,IRETOU ,ISK ,J2REFE 
      INTEGER J3REFE ,JC ,JINST ,JNUME ,LINST ,LLCHA ,LREFE 
      INTEGER LVALE ,N1 ,N2 ,N3 ,N4 ,NBCHAM ,NBINSG 
      INTEGER NBINST ,NBMODE ,NBNOEU ,NCMP ,NEQ ,NFONCT 
C-----------------------------------------------------------------------
      DATA BLANC    /'        '/
C      DATA CHAMN2   /'&&TRAN77.CHAMN2'/
C      DATA NOMCMP   /'DX      ','DY      ','DZ      ',
C     &               'DRX     ','DRY     ','DRZ     '/
C     ------------------------------------------------------------------
      CALL JEMARQ()
      MODE = BASEMO
      TRANGE = NOMIN
      CALL GETTCO(NOMIN,CONCEP)
      NOMCHA=' '
      NUMDDL=' '

C     --- RECUPERATION DES ENTITES DU MAILLAGE SUR LESQUELLES ---
C     ---                PORTE LA RESTITUTION                 ---
      TOUSNO = .TRUE.
      PREMS  = .TRUE.
      CALL GETVTX ( ' ', 'GROUP_NO', 1,IARG,0, K8B, N1 )
      CALL GETVTX ( ' ', 'NOEUD'   , 1,IARG,0, K8B, N2 )
      CALL GETVTX ( ' ', 'GROUP_MA', 1,IARG,0, K8B, N3 )
      CALL GETVTX ( ' ', 'MAILLE'  , 1,IARG,0, K8B, N4 )
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
        BASEMO = ZK24(IAREFE+4)(1:8)
        CALL JEVEUO(BASEMO//'           .REFD','L',IADRIF)
        IF (MATGEN(1:8) .NE. BLANC) THEN
          CALL JEVEUO(MATGEN//'           .REFA','L',J2REFE)
          NUMGEN = ZK24(J2REFE+1)(1:14)
          CALL JEVEUO(NUMGEN//'.NUME.REFN','L',J3REFE)
          CALL GETTCO(ZK24(J3REFE),TYPREP)
          MATRIC = ZK24(IADRIF)
          IF (MATRIC.NE.BLANC) THEN
           CALL DISMOI('F','NOM_NUME_DDL',MATRIC,'MATR_ASSE',IBID,
     &                NUMDDL,IRET)
           CALL DISMOI('F','NOM_MAILLA',MATRIC,'MATR_ASSE',IBID,
     &                MAILLA,IRET)
           IF ( TOUSNO ) CALL DISMOI('F','NB_EQUA',MATRIC,'MATR_ASSE',
     &                               NEQ,K8B,IRET)
          ELSE
           NUMDDL = ZK24(IADRIF+3)(1:14)
           CALL DISMOI('F','NOM_GD',NUMDDL,'NUME_DDL',IBID,NOMGD,IE)
           CALL DISMOI('F','NOM_MAILLA',NUMDDL,'NUME_DDL',IBID,
     &                MAILLA,IRET)
           IF ( TOUSNO ) CALL DISMOI('F','NB_EQUA',NUMDDL,'NUME_DDL',
     &                              NEQ,K8B,IRET)
          ENDIF
        ELSE
C  POUR LES CALCULS SANS MATRICE GENERALISEE (PROJ_MESU_MODAL)
          MATRIC = ZK24(IADRIF+3)
          IF (MATRIC(1:8) .EQ. BLANC) THEN
            MATRIC=ZK24(IADRIF)
            CALL DISMOI('F','NOM_NUME_DDL',MATRIC,'MATR_ASSE',
     &                              IBID,NUMDDL,IRET)
          ELSE
            NUMDDL = MATRIC(1:8)
          ENDIF
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
         CALL RSEXCH('F',BASEMO, 'DEPL', 1, NOMCHA, IRET )
         NOMCHA = NOMCHA(1:19)//'.REFE'
         CALL DISMOI('F','NOM_GD',NOMCHA,'CHAM_NO',IBID,NOMGD,IE)
         CALL JEVEUO ( NOMCHA, 'L', LLCHA )
         K24BID=ZK24(LLCHA)
         MAILLA = K24BID(1:8)

C------ON VERIFIE QUE L'UTILISATEUR A RENSEIGNE LE MEME SUPPORT DE
C------RESTITUTION DANS LE FICHIER DE COMMANDE
         CALL GETVID(' ','SQUELETTE',1,IARG,1,NOMMA,ISK)
         IF (ISK.NE.0) THEN
           IF (NOMMA .NE. MAILLA) THEN
             VALK (1) = NOMMA
             VALK (2) = MAILLA
             CALL U2MESK('F','SOUSTRUC2_9',2,VALK)
           ENDIF
         ENDIF

         CREFE(1) = ZK24(LLCHA)
         CREFE(2) = ZK24(LLCHA+1)
         IF ( TOUSNO ) CALL JELIRA(CREFE(2)(1:19)//'.NUEQ','LONMAX',
     &                             NEQ,K8B)
         BASEM2 = ' '
         CALL JEVEUO(NOMCHA,'L',IADRIF)
         MATRIC=ZK24(IADRIF+1)
         NUMDDL=MATRIC(1:14)
      ENDIF
C
      MULTAP = .FALSE.

C     ---   RECUPERATION DES VECTEURS DEPLACEMENT, VITESSE ET   ---
C     --- ACCELERATION GENERALISES SUIVANT LES CHAMPS SOUHAITES ---
      NFONCT = 0
      CALL RBPH01 ( TRANGE, NBCHAM, TYPE, ITRESU, NFONCT, BASEM2,
     &              TYPREF, TYPBAS, TOUSNO, MULTAP )
C
C     --- RECUPERATION DES NUMEROS DES NOEUDS ET DES DDLS ASSOCIES ---
C     ---         DANS LE CAS D'UNE RESTITUTION PARTIELLE          ---
C
      IF ( .NOT. TOUSNO ) THEN
         OBJVE1 = '&&TRAN77.NUME_NOEUD  '
         OBJVE2 = '&&TRAN77.NOM_CMP     '
         OBJVE3 = '&&TRAN77.NB_NEQ      '
         OBJVE4 = '&&TRAN77.NUME_DDL    '
         CALL RBPH02(MAILLA,NUMDDL,NOMCHA,NOMGD,NEQ,NBNOEU,OBJVE1,
     &                    NCMP, OBJVE2, OBJVE3, OBJVE4 )
         CALL JEVEUO ( OBJVE1, 'L', INUMNO )
         CALL JEVEUO ( OBJVE2, 'L', INOCMP )
         CALL JEVEUO ( OBJVE3, 'L', INOECP )
         CALL JEVEUO ( OBJVE4, 'L', INUDDL )
      ENDIF

C     --- RECUPERATION DES INSTANTS ---
C
      CALL GETVTX ( ' ', 'CRITERE'  , 0,IARG,1, CRIT   , N1)
      CALL GETVR8 ( ' ', 'PRECISION', 0,IARG,1, EPSI   , N1)
      CALL GETVTX ( ' ', 'INTERPOL' , 0,IARG,1, INTERP , N1)
C
      KNUME = '&&TRAN77.NUM_RANG'
      KINST = '&&TRAN77.INSTANT'
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
       CALL GETVID(' ','LIST_INST',0,IARG,1,K8B,FOCI)
       CALL GETVID(' ','LIST_FREQ',0,IARG,1,K8B,FOCF)
       CALL GETVR8(' ','INST',0,IARG,1,R8B,FOMI)
       CALL GETVR8(' ','FREQ',0,IARG,1,R8B,FOMF)
       CALL GETVID(' ','MODE_MECA',0,IARG,1,K8B,FOMO)
       IF ((INTERP(1:3).NE.'NON').AND.(FOCI.EQ.0 .AND. FOCF.EQ.0 .AND.
     &     FOMI.EQ.0 .AND. FOMF.EQ.0 .AND. FOMO.EQ.0 )) THEN
          CALL U2MESS('F','ALGORITH10_95')
       ENDIF

       CALL JEVEUO(TRANGE//'.DISC','L',IDINSG)
       CALL JELIRA(TRANGE//'.DISC','LONMAX',NBINSG,K8B)
       CALL WKVECT('&&TRAN77.VECTGENE','V V R',NBMODE,IDVECG)
       DO 210 ICH = 1,NBCHAM
          LEFFOR=.TRUE.
          IF (TYPE(ICH).EQ.'DEPL'.OR.TYPE(ICH).EQ.'VITE'.OR.
     &        TYPE(ICH).EQ.'ACCE'.OR.TYPE(ICH).EQ.'ACCE_ABSOLU')
     &      LEFFOR=.FALSE.
C
C            --- RECUPERATION DES DEFORMEES MODALES ---
C
          TYPCHA = TYPBAS(ICH)
          CALL RSEXCH('F',BASEMO,TYPCHA,1,NOMCHA,IRET)
          NOMCHA = NOMCHA(1:19)//'.VALE'
          CALL JEEXIN(NOMCHA,IBID)
          IF (IBID.GT.0) THEN
            NOMCHA(20:24)='.VALE'
          ELSE
            NOMCHA(20:24)='.CELV'
          END IF
          IF (LEFFOR)
     &     CALL JELIRA(NOMCHA,'LONMAX',NEQ,K1BID)
          CALL WKVECT('&&TRAN77.BASE','V V R',NBMODE*NEQ,IDBASE)
          IF ( TOUSNO ) THEN
            IF (MODE.EQ.BLANC.AND.MATRIC.EQ.BLANC.AND.
     &         TYPCHA.EQ.'DEPL') THEN
             CALL COPMO2(BASEMO,NEQ,NUMDDL,NBMODE,ZR(IDBASE))
            ELSE
             CALL COPMOD(BASEMO,TYPCHA,NEQ,NUMDDL,NBMODE,ZR(IDBASE))
            ENDIF
          ELSE
            DO 110 J = 1,NBMODE
               CALL RSEXCH('F',BASEMO, TYPCHA, J, NOMCHA, IRET )
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
          DO 200 I = 0,NBINST-1
             IARCHI = IARCHI + 1
             CALL RSEXCH(' ',NOMRES,TYPE(ICH),IARCHI,CHAMNO,IRET)
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
                   CALL VTCREC(CHAMNO,NOMCHA,'G','R',NEQ)
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
C
             CALL RSNOCH(NOMRES,TYPE(ICH),IARCHI)
             CALL RSADPA(NOMRES,'E',1,'INST',IARCHI,0,LINST,K8B)
             ZR(LINST) = ZR(JINST+I)
 200      CONTINUE
          CALL JEDETR ( '&&TRAN77.BASE' )
 210   CONTINUE
C
C
      KREFE  = NOMRES
      CALL WKVECT(KREFE//'.REFD','G V K24',7,LREFE)
      IF (MODE.EQ.BLANC) THEN

        ZK24(LREFE  ) = ZK24(IADRIF)
        ZK24(LREFE+1) = ZK24(IADRIF+1)
        ZK24(LREFE+2) = ZK24(IADRIF+2)
        ZK24(LREFE+3) = ZK24(IADRIF+3)
        ZK24(LREFE+4) = '        '
        ZK24(LREFE+5) = ZK24(IADRIF+4)
        ZK24(LREFE+6) = '        '
C
      ENDIF
      CALL JELIBE(KREFE//'.REFD')
C
      CALL JEDETC(' ','&&TRAN77',1)
      CALL TITRE
C
      CALL JEDEMA()
      END
