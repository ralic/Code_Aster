      SUBROUTINE TRAN75(NOMRES,TYPRES,NOMIN,NOMCMD,BASEMO)
      IMPLICIT REAL*8 (A-H,O-Z)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/11/2004   AUTEUR BOYERE E.BOYERE 
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
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32  JEXATR, JEXNOM, JEXNUM
C     ----FIN DES COMMUNS JEVEUX----------
C ----------------------------------------------------------------------
      PARAMETER     ( MXPARA = 10 )
      INTEGER       IPAR(MXPARA), I, J, ITRESU(8)
      INTEGER       FOCI, FOCF, FOMI, FOMF, FOMO
      REAL*8        R8B, EPSI, ALPHA, XNORM, DEPL(6)
      CHARACTER*1   COLI, K1BID
      CHARACTER*8   K8B, BLANC, BASEMO, CRIT, GRAN, INTERP, BASEM2,
     +              MAILLA, NOMRES, NOMIN, NOMCMP(6), MODE, MONMOT(2),
     +              MATGEN
      CHARACTER*14  NUMDDL, NUMGEN
      CHARACTER*16  TYPRES, NOMCMD, NOMP(MXPARA), TYPE(8), TYPCHA,
     +              TYPBAS(8), TYPREP
      CHARACTER*19  FONCT, KINST, KNUME, KREFE, PRCHNO, TRANGE,
     +              TYPREF(8)
      CHARACTER*24  MATRIC, CHAMNO, CREFE(2), NOMCHA, CHAMN2, OBJVE1,
     +              OBJVE2, NOMNOE
      LOGICAL       TOUSNO, MULTAP, LEFFOR
C     ------------------------------------------------------------------
      DATA BLANC    /'        '/
      DATA CHAMN2   /'&&TRAN75.CHAMN2'/
      DATA NOMCMP   /'DX      ','DY      ','DZ      ',
     +               'DRX     ','DRY     ','DRZ     '/
C     ------------------------------------------------------------------
      CALL JEMARQ()
      MODE = BASEMO
      TRANGE = NOMIN
      IER = 0
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
            CALL UTMESS('F',NOMCMD,'LE VECTEUR DIRECTEUR EST NUL.')
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
C      CALL GETVEM(MAILLA,'GROUP_NO', ' ','GROUP_NO',1,1,0,K8B,N1)
C      CALL GETVEM(MAILLA,'NOEUD'   , ' ','NOEUD'   ,1,1,0,K8B,N2)
C      IF ( N1+N2 .NE. 0 ) TOUSNO = .FALSE.
C
C     --- RECUPERATION DE LA BASE MODALE ---
C
      CALL JEVEUO ( TRANGE//'.DESC', 'L', IADESC )
      NBMODE = ZI(IADESC+1)
C
      IF ( MODE .EQ. BLANC ) THEN
         CALL JEVEUO(TRANGE//'.REFE','L',IAREFE)
         BASEMO = ZK24(IAREFE)(1:8)
         CALL JEVEUO(BASEMO//'           .REFE','L',IADRIF)
C
         CALL JEVEUO(NOMIN//'           .REFE','L',J1REFE)
         MATGEN = ZK24(J1REFE+1)
         IF (MATGEN(1:8) .NE. BLANC) THEN
           CALL JEVEUO(MATGEN//'           .REFA','L',J2REFE)
           NUMGEN = ZK24(J2REFE+1)(1:14)
           CALL JEVEUO(NUMGEN//'.NUME.REFN','L',J3REFE)
           CALL GETTCO(ZK24(J3REFE),TYPREP)
           IF (TYPREP(1:9).EQ.'MODE_MECA') THEN
               MATRIC = ZK24(IADRIF)
           ELSEIF (TYPREP(1:11).EQ.'BASE_MODALE') THEN
               MATRIC = ZK24(IADRIF+3)
           ELSEIF (TYPREP(1:11).EQ.'MODE_STAT') THEN
               MATRIC = ZK24(IADRIF)
               IF (MATRIC(1:8) .EQ. BLANC) MATRIC = ZK24(IADRIF+2)
           ENDIF
           IF (MATRIC.NE.BLANC) THEN
            CALL DISMOI('F','NOM_NUME_DDL',MATRIC,'MATR_ASSE',IBID,
     +                 NUMDDL,IRET)
            CALL DISMOI('F','NOM_MAILLA',MATRIC,'MATR_ASSE',IBID,
     +                 MAILLA,IRET)
            IF ( TOUSNO ) CALL DISMOI('F','NB_EQUA',MATRIC,'MATR_ASSE',
     +                               NEQ,K8B,IRET)
           ELSE
            NUMDDL = ZK24(IADRIF+1)(1:14)
            CALL DISMOI('F','NOM_MAILLA',NUMDDL,'NUME_DDL',IBID,
     +                 MAILLA,IRET)
            IF ( TOUSNO ) CALL DISMOI('F','NB_EQUA',NUMDDL,'NUME_DDL',
     +                               NEQ,K8B,IRET)
           ENDIF
         ELSE
C  POUR LES CALCULS SANS MATRICE GENERALISEE (PROJ_MESU_MODAL)
           MATRIC = ZK24(IADRIF+1)
           IF(MATRIC(1:8) .EQ. BLANC) THEN
             MATRIC=ZK24(IADRIF+2)
             CALL DISMOI('F','NOM_NUME_DDL',MATRIC,'MATR_ASSE',
     +                              IBID,NUMDDL,IRET)
           ELSE
             NUMDDL = MATRIC(1:8)
           ENDIF
           CALL JEVEUO(NUMDDL//'.NUME.REFN','L',J3REFE)
           MATRIC = ZK24(J3REFE)
           MAILLA = MATRIC(1:8)
           MATRIC = ZK24(IADRIF+2)
           IF ( TOUSNO ) CALL DISMOI('F','NB_EQUA',NUMDDL,'NUME_DDL',
     +                              NEQ,K8B,IRET)
         ENDIF
C
         BASEM2 = BASEMO
C
      ELSE
C         --- BASE MODALE CALCULEE PAR SOUS-STRUCTURATION
C
         CALL RSEXCH ( BASEMO, 'DEPL', 1, NOMCHA, IRET )
         NOMCHA = NOMCHA(1:19)//'.REFE'
         CALL JEVEUO ( NOMCHA, 'L', LLCHA )
         MAILLA = ZK24(LLCHA)
         NUMDDL = ZK24(LLCHA+1)
         CREFE(1) = ZK24(LLCHA)
         CREFE(2) = ZK24(LLCHA+1)
         IF ( TOUSNO ) CALL JELIRA(CREFE(2)(1:19)//'.NUEQ','LONMAX',
     +                             NEQ,K8B)
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
     +              TYPREF, TYPBAS, TOUSNO, MULTAP )
C
C     --- RECUPERATION DES NUMEROS DES NOEUDS ET DES DDLS ASSOCIES ---
C     ---         DANS LE CAS D'UNE RESTITUTION PARTIELLE          ---
C
      IF ( .NOT. TOUSNO ) THEN
         OBJVE1 = '&&TRAN75.NUMDDL_NOEUD'
         OBJVE2 = '&&TRAN75.NUME_NOEUDS'
         CALL RBPH02 ( MAILLA, NUMDDL, NBNOEU, OBJVE1, OBJVE2 )
         NEQ = 6 * NBNOEU
         CALL JEVEUO ( OBJVE1, 'L', INUDDL )
         CALL JEVEUO ( OBJVE2, 'L', INUMNO )
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
C            CALL VTCREM(CHAMN2,MATRIC,'V','R')
            CALL VTCREB(CHAMN2,NUMDDL,'V','R',NEQ)
            CHAMN2(20:24) = '.VALE'
            CALL JEVEUO(CHAMN2,'E',LVAL2)
            LPSDEL = IPSDEL
         ELSE
            CALL WKVECT('&&TRAN75.PSI_DELTA','V V R',NEQ,JPSDEL)
            DO 100 I = 1,NBNOEU
               IDEC = 6 * ( I - 1 )
               ZR(JPSDEL+IDEC  ) = ZR(IPSDEL+ZI(INUDDL+IDEC  )-1)
               ZR(JPSDEL+IDEC+1) = ZR(IPSDEL+ZI(INUDDL+IDEC+1)-1)
               ZR(JPSDEL+IDEC+2) = ZR(IPSDEL+ZI(INUDDL+IDEC+2)-1)
               ZR(JPSDEL+IDEC+3) = ZR(IPSDEL+ZI(INUDDL+IDEC+3)-1)
               ZR(JPSDEL+IDEC+4) = ZR(IPSDEL+ZI(INUDDL+IDEC+4)-1)
               ZR(JPSDEL+IDEC+5) = ZR(IPSDEL+ZI(INUDDL+IDEC+5)-1)
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
         CALL UTMESS('F',NOMCMD,'PROBLEME(S) RENCONTRE(S) LORS'//
     +                          ' DE LA LECTURE DES INSTANTS.' )
      ENDIF
      CALL JEEXIN ( KINST, IRET )
      IF ( IRET .GT. 0 ) THEN
        CALL JEVEUO ( KINST, 'L', JINST )
        CALL JEVEUO ( KNUME, 'L', JNUME )
      END IF
C
C     --- CREATION DE LA SD RESULTAT ---
      CALL RSCRSD ( NOMRES, TYPRES, NBINST )
C
C --- SI RESTITUTION PARTIELLE CONSTITUTION DE DONNEES POUR
C --- CREATION DU PROF_CHNO ADEQUAT
C
      IF ( .NOT. TOUSNO ) THEN
         NOMNOE = MAILLA//'.NOMNOE'
         CALL JELIRA(NOMNOE,'NOMMAX',NBNOMA,K1BID)
         GRAN = 'DEPL_R  '
         CALL JENONU(JEXNOM('&CATA.GD.NOMGD',GRAN),NUMGD)
         CALL JEVEUO(JEXNUM('&CATA.GD.NOMCMP',NUMGD),'L',IACMP)
         CALL JEVEUO(JEXATR('&CATA.GD.NOMCMP','LONCUM'),'L',IAV)
         NBCOMP = ZI(IAV+NUMGD) - ZI(IAV+NUMGD-1)
         CALL DISMOI('F','NB_EC',GRAN,'GRANDEUR',NEC,K8B,IER)
         CALL WKVECT('&&TRAN75.DESC_NOEUD' ,'V V I',NEC*NBNOMA,JDESC)
         CALL WKVECT('&&TRAN75.NBCOMP_AFFE','V V I',NBNOMA,    JNBCA)
         DO 150 I = 1,NBNOEU
            NUNO = ZI(INUMNO+I-1)
            DO 160 ICMP = 1,6
               J = INDIK8(ZK8(IACMP),NOMCMP(ICMP),1,NBCOMP)
               IF ( J.NE.0 ) THEN
                  IEC = (J-1)/30 + 1
                  JJ = J - 30*(IEC-1)
                  ZI(JDESC+(NUNO-1)*NEC+IEC-1) =
     +                    IOR(ZI(JDESC+(NUNO-1)*NEC+IEC-1),2**JJ)
               ENDIF
 160        CONTINUE
            ZI(JNBCA+NUNO-1) = 6
 150     CONTINUE
      ENDIF
      IER = 0
C
C     --- RESTITUTION SUR LA BASE REELLE ---
C
      IF (INTERP(1:3).NE.'NON') THEN

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
         IF (FOCI.EQ.0 .AND. FOCF.EQ.0 .AND.
     &       FOMI.EQ.0 .AND. FOMF.EQ.0 .AND. FOMO.EQ.0 ) THEN
            CALL UTMESS('F',NOMCMD,'POUR INTERPOLER IL FAUT FOURNIR'//
     &                  ' UNE LISTE DE FREQUENCES OU INSTANTS.')
         ENDIF

         CALL JEVEUO(TRANGE//'.INST','L',IDINSG)
         CALL JELIRA(TRANGE//'.INST','LONMAX',NBINSG,K8B)
         CALL WKVECT('&&TRAN75.VECTGENE','V V R',NBMODE,IDVECG)
         DO 210 ICH = 1,NBCHAM
            LEFFOR=.TRUE.
            IF (TYPE(ICH).EQ.'DEPL'.OR.TYPE(ICH).EQ.'VITE'.OR.
     &          TYPE(ICH).EQ.'ACCE'.OR.TYPE(ICH).EQ.'ACCE_ABSOLU')
     &        LEFFOR=.FALSE.
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

            NOMCHA = NOMCHA(1:19)//'.VALE'
            IF (LEFFOR)
     &       CALL JELIRA(NOMCHA,'LONMAX',NEQ,K1BID)
            CALL WKVECT('&&TRAN75.BASE','V V R',NBMODE*NEQ,IDBASE)
            IF ( TOUSNO ) THEN
              IF (MODE.EQ.BLANC.AND.MATRIC.EQ.BLANC.AND.
     &           TYPCHA.EQ.'DEPL') THEN
               CALL COPMO2(BASEMO,NEQ,NUMDDL,NBMODE,ZR(IDBASE))
              ELSE      
               CALL COPMOD(BASEMO,TYPCHA,NEQ,NUMDDL,NBMODE,ZR(IDBASE))
              ENDIF
            ELSE
               DO 110 J = 1,NBMODE
                  CALL RSEXCH(BASEMO,TYPCHA,J,NOMCHA,IRET)
                  CALL JEEXIN(NOMCHA,IBID)
                  IF (IBID.GT.0) THEN
                    NOMCHA(20:24)='.VALE'
                  ELSE
                    NOMCHA(20:24)='.CELV'
                  END IF

                  NOMCHA = NOMCHA(1:19)//'.VALE'
                  CALL JEVEUO(NOMCHA,'L',IDEFM)
                  IDECJ = NEQ * ( J - 1 )
                  DO 120 I = 1,NBNOEU
                     IDECI = 6 * ( I - 1 )
                     IDECJI = IDECJ + IDECI
                    ZR(IDBASE+IDECJI  ) = ZR(IDEFM+ZI(INUDDL+IDECI  )-1)
                    ZR(IDBASE+IDECJI+1) = ZR(IDEFM+ZI(INUDDL+IDECI+1)-1)
                    ZR(IDBASE+IDECJI+2) = ZR(IDEFM+ZI(INUDDL+IDECI+2)-1)
                    ZR(IDBASE+IDECJI+3) = ZR(IDEFM+ZI(INUDDL+IDECI+3)-1)
                    ZR(IDBASE+IDECJI+4) = ZR(IDEFM+ZI(INUDDL+IDECI+4)-1)
                    ZR(IDBASE+IDECJI+5) = ZR(IDEFM+ZI(INUDDL+IDECI+5)-1)
 120              CONTINUE
 110           CONTINUE
               CALL JEDETR ( OBJVE1 )
            ENDIF
            IDRESU = ITRESU(ICH)
            IARCHI = 0
            DO 200 I = 0,NBINST-1
               IARCHI = IARCHI + 1
               CALL RSEXCH(NOMRES,TYPE(ICH),IARCHI,CHAMNO,IRET)
               IF ( IRET .EQ. 0 ) THEN
                 CALL UTMESS('A',NOMCMD,CHAMNO//'CHAM_NO DEJA EXISTANT')
               ELSEIF ( IRET .EQ. 100 ) THEN
                 IF ( TOUSNO ) THEN
                   IF (MODE.EQ.BLANC) THEN
                     IF (LEFFOR) THEN
                       CALL VTDEFS(CHAMNO,TYPREF(ICH),'G','R')
                     ELSE
                       CALL VTCREB(CHAMNO,NUMDDL,'G','R',NEQ)
                     ENDIF
                   ELSE
                     CALL VTCREA(CHAMNO,CREFE,'G','R',NEQ)
                   ENDIF
                 ELSE
                   IF ( (I.EQ.0).AND.(ICH.EQ.1) ) THEN
                      CALL CRCHNO(CHAMNO,CHAMNO,GRAN,MAILLA,'G','R',
     +                            NBNOMA,NEQ)
                      PRCHNO = CHAMNO
                      CALL CRPRNO(PRCHNO,'G',NBNOMA,NEQ)
                      CALL JEVEUO(PRCHNO//'.PRNO','E',JPRNO)
                      II = 0
                      IDEC = 1
                      DO 220 INO = 1,NBNOMA
                         ZI(JPRNO-1+(NEC+2)*(INO-1)+1) = IDEC
                         ZI(JPRNO-1+(NEC+2)*(INO-1)+2) = ZI(JNBCA+INO-1)
                         DO 230 INEC = 1,NEC
                            II = II + 1
                            ZI(JPRNO-1+(NEC+2)*(INO-1)+2+INEC) =
     +                                                    ZI(JDESC+II-1)
 230                     CONTINUE
                         IDEC = IDEC + ZI(JNBCA+INO-1)
 220                  CONTINUE
                      CALL JELIBE(PRCHNO//'.PRNO')
                      CALL PTEEQU(PRCHNO,NEQ,NUMGD)
                      CALL JEDETR( OBJVE2 )
                      CALL JEDETR('&&TRAN75.DESC_NOEUD')
                      CALL JEDETR('&&TRAN75.NBCOMP_AFFE')
                   ELSE
                      CALL CRCHNO(CHAMNO,PRCHNO,GRAN,MAILLA,'G','R',
     +                            NBNOMA,NEQ)
                   ENDIF
                 ENDIF
               ELSE
                  CALL UTMESS('F',NOMCMD,'APPEL ERRONE')
               ENDIF
               CHAMNO(20:24) = '.VALE'
               CALL JEVEUO(CHAMNO,'E',LVALE)
               IF (LEFFOR .OR. .NOT.TOUSNO) 
     +           CALL JELIRA(CHAMNO,'LONMAX',NEQ,K8B)
               CALL EXTRAC(INTERP,EPSI,CRIT,NBINSG,ZR(IDINSG),
     +                 ZR(JINST+I),ZR(IDRESU),NBMODE,ZR(IDVECG), IBID)
               CALL MDGEPH(NEQ,NBMODE,ZR(IDBASE),ZR(IDVECG),ZR(LVALE))
               IF ( MULTAP ) THEN
                  IF (TYPE(ICH).EQ.'DEPL')
     +             CALL MDGEP3(NEQ,NBEXCI,ZR(LPSDEL),
     +                         ZR(JINST+I),ZK8(JNODEP),ZR(LVAL2))
                  IF (TYPE(ICH).EQ.'VITE')
     +             CALL MDGEP3(NEQ,NBEXCI,ZR(LPSDEL),
     +                         ZR(JINST+I),ZK8(JNOVIT),ZR(LVAL2))
                  IF (TYPE(ICH).EQ.'ACCE')
     +             CALL MDGEP3(NEQ,NBEXCI,ZR(LPSDEL),
     +                         ZR(JINST+I),ZK8(JNOACC),ZR(LVAL2))
                  IF (TYPE(ICH).EQ.'ACCE_ABSOLU')
     +             CALL MDGEP3(NEQ,NBEXCI,ZR(LPSDEL),
     +                         ZR(JINST+I),ZK8(JNOACC),ZR(LVAL2))
                  DO 240 IE =1,NEQ
                     ZR(LVALE+IE-1)=ZR(LVALE+IE-1)+ZR(LVAL2+IE-1)
 240              CONTINUE
               ENDIF
C              --- PRISE EN COMPTE D'UNE ACCELERATION D'ENTRAINEMENT
               IF ( TYPE(ICH) .EQ. 'ACCE_ABSOLU'.AND.NFONCT.NE.0 ) THEN
                  IRET = 0
                  CALL FOINTE('F',FONCT,1,'INST',ZR(JINST+I),ALPHA,IER)
C                 --- ACCELERATION ABSOLUE = RELATIVE + ENTRAINEMENT
C
                  CALL WKVECT('&&TRAN75.VECTEUR','V V R',NEQ,JVEC)
                  CALL WKVECT('&&TRAN75.DDL','V V I',NEQ*NBDIR,JDDL)
                CALL PTEDDL('NUME_DDL',NUMDDL,NBDIR,NOMCMP,NEQ,ZI(JDDL))
                  DO 250 ID = 1 , NBDIR
                     DO 252 IE = 0 , NEQ-1
                        ZR(JVEC+IE) =  ZR(JVEC+IE) +
     +                           ZI(JDDL+NEQ*(ID-1)+IE)*ALPHA*DEPL(ID)
 252                 CONTINUE
 250              CONTINUE
                  DO 254 IE = 0 , NEQ-1 
                     ZR(LVALE+IE) = ZR(LVALE+IE) + ZR(JVEC+IE)
 254              CONTINUE
                  CALL JEDETR ('&&TRAN75.VECTEUR')
                  CALL JEDETR ('&&TRAN75.DDL')
               ENDIF
               CALL RSNOCH(NOMRES,TYPE(ICH),IARCHI,' ')
               CALL RSADPA(NOMRES,'E',1,'INST',IARCHI,0,LINST,K8B)
               ZR(LINST) = ZR(JINST+I)
 200        CONTINUE
            CALL JEDETR ( '&&TRAN75.BASE' )
 210     CONTINUE
C
      ELSE
         DO 310 ICH = 1,NBCHAM
            LEFFOR=.TRUE.
            IF (TYPE(ICH).EQ.'DEPL'.OR.TYPE(ICH).EQ.'VITE'.OR.
     &          TYPE(ICH).EQ.'ACCE'.OR.TYPE(ICH).EQ.'ACCE_ABSOLU')
     &        LEFFOR=.FALSE.
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
     &       CALL JELIRA(NOMCHA,'LONMAX',NEQ,K1BID)
            CALL WKVECT('&&TRAN75.BASE','V V R',NBMODE*NEQ,IDBASE)
            IF ( TOUSNO ) THEN
              IF (MODE.EQ.BLANC.AND.MATRIC.EQ.BLANC.AND.
     &           TYPCHA.EQ.'DEPL') THEN
               CALL COPMO2(BASEMO,NEQ,NUMDDL,NBMODE,ZR(IDBASE))
              ELSE      
               CALL COPMOD(BASEMO,TYPCHA,NEQ,NUMDDL,NBMODE,ZR(IDBASE))
              ENDIF
            ELSE
               DO 130 J = 1,NBMODE
                  CALL RSEXCH(BASEMO,TYPCHA,J,NOMCHA,IRET)
                  NOMCHA = NOMCHA(1:19)//'.VALE'
                  CALL JEEXIN(NOMCHA,IBID)
                  IF (IBID.GT.0) THEN
                    NOMCHA(20:24)='.VALE'
                  ELSE
                    NOMCHA(20:24)='.CELV'
                  END IF

                  CALL JEVEUO(NOMCHA,'L',IDEFM)
                  IDECJ = NEQ * ( J - 1 )
                  DO 170 I = 1,NBNOEU
                     IDECI = 6 * ( I - 1 )
                     IDECJI = IDECJ + IDECI
                    ZR(IDBASE+IDECJI  ) = ZR(IDEFM+ZI(INUDDL+IDECI  )-1)
                    ZR(IDBASE+IDECJI+1) = ZR(IDEFM+ZI(INUDDL+IDECI+1)-1)
                    ZR(IDBASE+IDECJI+2) = ZR(IDEFM+ZI(INUDDL+IDECI+2)-1)
                    ZR(IDBASE+IDECJI+3) = ZR(IDEFM+ZI(INUDDL+IDECI+3)-1)
                    ZR(IDBASE+IDECJI+4) = ZR(IDEFM+ZI(INUDDL+IDECI+4)-1)
                    ZR(IDBASE+IDECJI+5) = ZR(IDEFM+ZI(INUDDL+IDECI+5)-1)
 170              CONTINUE
 130           CONTINUE
               CALL JEDETR ( OBJVE1 )
            ENDIF
            IARCH = 0
            CALL JEEXIN(TRANGE//'.ORDR',IRET)
            IF (IRET.NE.0 .AND. ZI(JNUME).EQ.1) IARCH = -1
            IDRESU = ITRESU(ICH)
            DO 300 I = 0,NBINST-1
               IARCH = IARCH + 1
               CALL RSEXCH(NOMRES,TYPE(ICH),IARCH,CHAMNO,IRET)
               IF ( IRET .EQ. 0 ) THEN
                 CALL UTMESS('A',NOMCMD,CHAMNO//'CHAM_NO DEJA EXISTANT')
               ELSEIF ( IRET .EQ. 100 ) THEN
                 IF ( TOUSNO ) THEN
                   IF (MODE.EQ.BLANC) THEN
                     IF (LEFFOR) THEN
                       CALL VTDEFS(CHAMNO,TYPREF(ICH),'G','R')
                     ELSE
                       CALL VTCREB(CHAMNO,NUMDDL,'G','R',NEQ)
                     ENDIF
                   ELSE
                     CALL VTCREA(CHAMNO,CREFE,'G','R',NEQ)
                   ENDIF
                 ELSE
                   IF ( (I.EQ.0).AND.(ICH.EQ.1) ) THEN
                      CALL CRCHNO(CHAMNO,CHAMNO,GRAN,MAILLA,'G','R',
     +                            NBNOMA,NEQ)
                      PRCHNO = CHAMNO
                      CALL CRPRNO(PRCHNO,'G',NBNOMA,NEQ)
                      CALL JEVEUO(PRCHNO//'.PRNO','E',JPRNO)
                      II = 0
                      IDEC = 1
                      DO 320 INO = 1,NBNOMA
                         ZI(JPRNO-1+(NEC+2)*(INO-1)+1) = IDEC
                         ZI(JPRNO-1+(NEC+2)*(INO-1)+2) = ZI(JNBCA+INO-1)
                         DO 330 INEC = 1,NEC
                            II = II + 1
                            ZI(JPRNO-1+(NEC+2)*(INO-1)+2+INEC) =
     +                                                    ZI(JDESC+II-1)
 330                     CONTINUE
                         IDEC = IDEC + ZI(JNBCA+INO-1)
 320                  CONTINUE
                      CALL JELIBE(PRCHNO//'.PRNO')
                      CALL PTEEQU(PRCHNO,NEQ,NUMGD)
                      CALL JEDETR( OBJVE2 )
                      CALL JEDETR('&&TRAN75.DESC_NOEUD')
                      CALL JEDETR('&&TRAN75.NBCOMP_AFFE')
                   ELSE
                      CALL CRCHNO(CHAMNO,PRCHNO,GRAN,MAILLA,'G','R',
     +                            NBNOMA,NEQ)
                   ENDIF
                 ENDIF
               ELSE
                  CALL UTMESS('F',NOMCMD,'APPEL ERRONE')
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
     +           CALL JELIRA(CHAMNO,'LONMAX',NEQ,K8B)
               CALL MDGEPH(NEQ,NBMODE,ZR(IDBASE),
     +                     ZR(IDRESU+(ZI(JNUME+I)-1)*NBMODE),ZR(LVALE))
               IF ( MULTAP ) THEN
                  IF (TYPE(ICH).EQ.'DEPL')
     +             CALL MDGEP3(NEQ,NBEXCI,ZR(LPSDEL),
     +                         ZR(JINST+I),ZK8(JNODEP),ZR(LVAL2))
                  IF (TYPE(ICH).EQ.'VITE')
     +             CALL MDGEP3(NEQ,NBEXCI,ZR(LPSDEL),
     +                         ZR(JINST+I),ZK8(JNOVIT),ZR(LVAL2))
                  IF (TYPE(ICH).EQ.'ACCE')
     +             CALL MDGEP3(NEQ,NBEXCI,ZR(LPSDEL),
     +                         ZR(JINST+I),ZK8(JNOACC),ZR(LVAL2))
                  IF (TYPE(ICH).EQ.'ACCE_ABSOLU')
     +             CALL MDGEP3(NEQ,NBEXCI,ZR(LPSDEL),
     +                         ZR(JINST+I),ZK8(JNOACC),ZR(LVAL2))
                  DO 340 IE =1,NEQ
                     ZR(LVALE+IE-1)=ZR(LVALE+IE-1)+ZR(LVAL2+IE-1)
 340              CONTINUE
               ENDIF
C              --- PRISE EN COMPTE D'UNE ACCELERATION D'ENTRAINEMENT
               IF ( TYPE(ICH) .EQ. 'ACCE_ABSOLU'.AND.NFONCT.NE.0 ) THEN
                  IRET = 0
                  CALL FOINTE('F',FONCT,1,'INST',ZR(JINST+I),ALPHA,IER)
C                 --- ACCELERATION ABSOLUE = RELATIVE + ENTRAINEMENT
                  CALL WKVECT('&&TRAN75.VECTEUR','V V R',NEQ,JVEC)
                  CALL WKVECT('&&TRAN75.DDL','V V I',NEQ*NBDIR,JDDL)
                CALL PTEDDL('NUME_DDL',NUMDDL,NBDIR,NOMCMP,NEQ,ZI(JDDL))
                  DO 350 ID = 1 , NBDIR
                     DO 352 IE = 0 , NEQ-1
                        ZR(JVEC+IE) =  ZR(JVEC+IE) +
     +                           ZI(JDDL+NEQ*(ID-1)+IE)*ALPHA*DEPL(ID)
 352                 CONTINUE
 350              CONTINUE
                  DO 354 IE = 0 , NEQ-1 
                     ZR(LVALE+IE) = ZR(LVALE+IE) + ZR(JVEC+IE)
 354              CONTINUE
                  CALL JEDETR ('&&TRAN75.VECTEUR')
                  CALL JEDETR ('&&TRAN75.DDL')
               ENDIF
               CALL RSNOCH(NOMRES,TYPE(ICH),IARCH,' ')
               CALL RSADPA(NOMRES,'E',1,'INST',IARCH,0,LINST,K8B)
               ZR(LINST) = ZR(JINST+I)
 300        CONTINUE
            CALL JEDETR ( '&&TRAN75.BASE' )
 310     CONTINUE
      ENDIF
C
      KREFE  = NOMRES
      CALL WKVECT(KREFE//'.REFE','G V K24',3,LREFE)
      IF (MODE.EQ.BLANC) THEN
C
         IF (TYPREP(1:9).EQ.'MODE_MECA') THEN
             ZK24(LREFE  ) = ZK24(IADRIF)
         ELSEIF (TYPREP(1:9).EQ.'MODE_STAT') THEN
           ZK24(LREFE) = ZK24(IADRIF)
           IF (ZK24(IADRIF)(1:8).EQ.BLANC) ZK24(LREFE)=ZK24(IADRIF+2)
         ELSEIF (TYPREP(1:11).EQ.'BASE_MODALE') THEN
             ZK24(LREFE  ) = ZK24(IADRIF+3)
         ENDIF
C
         ZK24(LREFE+1) = ZK24(IADRIF+1)
         ZK24(LREFE+2) = ZK24(IADRIF+2)
      ELSE
         ZK24(LREFE  ) = '  '
         ZK24(LREFE+1) = '  '
         ZK24(LREFE+2) = ZK24(LLCHA+1)
      ENDIF
      CALL JELIBE(KREFE//'.REFE')
C
 9999 CONTINUE
      CALL JEDETC(' ','&&TRAN75',1)
      CALL TITRE
C
99999 CONTINUE
      CALL JEDEMA()
      END
