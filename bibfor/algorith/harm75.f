      SUBROUTINE HARM75(NOMRES,TYPRES,NOMIN,NOMCMD,BASEMO)
      IMPLICIT NONE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/11/2012   AUTEUR GREFFET N.GREFFET 
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
C     OPERATEUR DE RETOUR A LA BASE PHYSIQUE A PARTIR DE DONNEES
C     GENERALISEES DANS LE CAS D'UN CALCUL HARMONIQUE
C     ------------------------------------------------------------------
C IN  : NOMRES : NOM UTILISATEUR POUR LA COMMANDE REST_BASE_PHYS
C IN  : TYPRES : TYPE DE RESULTAT : 'DYNA_HARMO'
C IN  : NOMIN  : NOM UTILISATEUR DU CONCEPT HARM_GENE AMONT
C IN  : NOMCMD : NOM DE LA COMMANDE : 'REST_BASE_PHYS'
C IN  : BASEMO : NOM UTILISATEUR DU CONCEPT MODE_MECA AMONT
C                (SI CALCUL MODAL PAR SOUS-STRUCTURATION)
C                ' ' SINON
C ----------------------------------------------------------------------
C
C
      INCLUDE 'jeveux.h'
C
C
      INTEGER       IBID,NBMODE,ITRESU(8)
      REAL*8        EPSI
      CHARACTER*1   K1BID,TYPE1
      CHARACTER*8   K8B,BASEMO,CRIT,CHAMP(8),INTERP,NOMRES,
     &              NOMIN,MODE,TOUCH, MAILLA,MATGEN
      CHARACTER*8   NOMGD,BASEM2,BLANC
      CHARACTER*14  NUMDDL
      CHARACTER*16  NOMCMD,TYPRES,TYPBAS(8),TYPCHA,TYPE(3)
      CHARACTER*19  KREFE,KNUME,KFREQ,HRANGE,PRCHNO,PROF,TYPREF(8)
      CHARACTER*24  MATRIC,CHAMNO,CREFE(2),CHMOD,NOMCHA,
     &              OBJVE1,OBJVE2,OBJVE3,OBJVE4
      LOGICAL       TOUSNO, LEFFOR, PREMS
      INTEGER IARG,INOCMP,INOECP,INUMNO,INUDDL     
      INTEGER J,JC,I,IE,IADESC,IADRIF,IARCHI,IAREFE,ICH 
      INTEGER IDBASE,IDVECG,IRET,IRETOU,JFREQ
      INTEGER JNUME,LFREQ,LLCHA,LREFE,LVALE,NBCHAM,NBINSG 
      INTEGER N1,N2,N3,N4,J3REFE,IDEC,IDEFM,IDINSG,IDRESU
      INTEGER NBFREQ,NEQ,NBNOEU,NCMP 
C ------------------------------------------------------------------
      DATA CHAMNO   /'&&HARM75.CHAMNO'/      
      DATA BLANC    /'        '/
C ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      MATRIC=' '
C      
      NOMCHA=' '
      NUMDDL=' '
      PRCHNO=' '
C      
      MODE = BASEMO
      HRANGE = NOMIN

      CALL GETVTX ( ' ', 'TOUT_CHAM', 1,IARG,1, TOUCH, N1 )
      IF (N1.NE.0) THEN
         CALL ASSERT( TOUCH(1:3).EQ.'OUI' )
         NBCHAM = 3
         TYPE(1) = 'DEPL            '
         TYPE(2) = 'VITE            '
         TYPE(3) = 'ACCE            '
      ELSE
         CALL GETVTX ( ' ', 'NOM_CHAM', 1,IARG,0, CHAMP, N1 )
         NBCHAM = -N1
         CALL GETVTX ( ' ', 'NOM_CHAM', 1,IARG,NBCHAM, CHAMP, N1 )
         DO 11 I = 1 , NBCHAM
         IF (CHAMP(I)(1:4).EQ.'DEPL') THEN
            TYPE(I) = 'DEPL            '
         ELSEIF (CHAMP(I)(1:4).EQ.'VITE') THEN
            TYPE(I) = 'VITE            '
         ELSEIF (CHAMP(I)(1:4).EQ.'ACCE') THEN
            TYPE(I) = 'ACCE            '
         ELSE
C           CHAMP IGNORE
         ENDIF
  11     CONTINUE
      ENDIF
      
C
C     --- RECUPERATION DES ENTITES DU MAILLAGE SUR LESQUELLES ---
C     ---                PORTE LA RESTITUTION                 ---
      TOUSNO = .TRUE.
      CALL GETVTX ( ' ', 'GROUP_NO', 1,IARG,0, K8B, N1 )
      CALL GETVTX ( ' ', 'NOEUD'   , 1,IARG,0, K8B, N2 )
      CALL GETVTX ( ' ', 'GROUP_MA', 1,IARG,0, K8B, N3 )
      CALL GETVTX ( ' ', 'MAILLE'  , 1,IARG,0, K8B, N4 )
      IF ( N1+N2+N3+N4 .NE. 0 ) TOUSNO = .FALSE.
C
C     --- RECUPERATION DE LA BASE MODALE ---
C ON SUPPOSE QU ELLE EST ISSUE D UN MODE_MECA
C
      CALL JEVEUO ( HRANGE//'.DESC', 'L', IADESC )

      NBMODE = ZI(IADESC+1)
C
      IF (MODE.EQ.' ') THEN
C      
         CALL JEVEUO(HRANGE//'.REFD','L',IAREFE)
         MATGEN = ZK24(IAREFE)(1:8)
         BASEMO = ZK24(IAREFE+4)(1:8)
         CALL JEVEUO(BASEMO//'           .REFD','L',IADRIF)
         IF (MATGEN(1:8) .NE. BLANC) THEN
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
      ELSE
C         --- BASE MODALE CALCULEE PAR SOUS-STRUCTURATION
C
         CALL RSEXCH('F',BASEMO, 'DEPL', 1, CHMOD, IRET )
         CHMOD = CHMOD(1:19)//'.REFE'
         CALL DISMOI('F','NOM_GD',CHMOD,'CHAM_NO',IBID,NOMGD,IE)
         CALL DISMOI('F','PROF_CHNO',CHMOD,'CHAM_NO',IBID,PRCHNO,IE)
         CALL JEVEUO ( CHMOD, 'L', LLCHA )
         MAILLA = ZK24(LLCHA)(1:8)
         CREFE(1) = ZK24(LLCHA)
         CREFE(2) = ZK24(LLCHA+1)
         IF ( TOUSNO ) CALL JELIRA(CREFE(2)(1:19)//'.NUEQ','LONMAX',
     &                             NEQ,K8B)
         BASEM2 = ' '
      ENDIF
C
C     ---   RECUPERATION DES VECTEURS DEPLACEMENT, VITESSE ET   ---
C     --- ACCELERATION GENERALISES SUIVANT LES CHAMPS SOUHAITES ---
      CALL RBPH01 ( HRANGE, NBCHAM, TYPE, ITRESU, 0, BASEM2,
     &              TYPREF, TYPBAS, TOUSNO, .FALSE. )
C
C     --- RECUPERATION DES NUMEROS DES NOEUDS ET DES DDLS ASSOCIES ---
C     ---         DANS LE CAS D'UNE RESTITUTION PARTIELLE          ---
C
      IF ( .NOT. TOUSNO ) THEN
         OBJVE1 = '&&HARM75.NUME_NOEUD  '
         OBJVE2 = '&&HARM75.NOM_CMP     '
         OBJVE3 = '&&HARM75.NB_NEQ      '
         OBJVE4 = '&&HARM75.NUME_DDL    '
         CALL RBPH02(MAILLA,NUMDDL,CHMOD,NOMGD,NEQ,NBNOEU,OBJVE1,
     &                    NCMP, OBJVE2, OBJVE3, OBJVE4 )
         CALL JEVEUO ( OBJVE1, 'L', INUMNO )
         CALL JEVEUO ( OBJVE2, 'L', INOCMP )
         CALL JEVEUO ( OBJVE3, 'L', INOECP )
         CALL JEVEUO ( OBJVE4, 'L', INUDDL )
      ENDIF
C
C
C     --- RECUPERATION DES FREQUENCES ---
      CALL GETVTX(' ','CRITERE'  ,0,IARG,1,CRIT,N1)
      CALL GETVR8(' ','PRECISION',0,IARG,1,EPSI,N1)
      CALL GETVTX(' ','INTERPOL' ,0,IARG,1,INTERP,N1)
C ON PLANTE LE CALCUL SI ON DEMANDE D'INTERPOLER EN FREQUENCIEL
      IF (INTERP(1:3).NE.'NON') CALL U2MESS('F','ALGORITH3_86')
C      
      KNUME = '&&HARM75.NUM_RANG'
      KFREQ = '&&HARM75.FREQ'
C
      CALL RSTRAN(INTERP,HRANGE,' ',1,KFREQ,KNUME,NBFREQ,IRETOU)
C
      IF ( IRETOU .NE. 0 ) THEN
         CALL U2MESS('F','UTILITAI4_24')
      ENDIF
      CALL JEEXIN ( KFREQ, IRET )
      IF ( IRET .GT. 0 ) THEN
        CALL JEVEUO ( KFREQ, 'L', JFREQ )
        CALL JEVEUO ( KNUME, 'L', JNUME )
      END IF
C      
C     --- CREATION DE LA SD RESULTAT ---
C
      CALL RSCRSD('G', NOMRES, TYPRES, NBFREQ )
C      
C     --- RESTITUTION SUR LA BASE REELLE ---
C
       CALL JEVEUO(HRANGE//'.DISC','L',IDINSG)
       CALL JELIRA(HRANGE//'.DISC','LONMAX',NBINSG,K8B)
       CALL WKVECT('&&HARM75.VECTGENE','V V C',NBMODE,IDVECG)
       DO 210 ICH = 1,NBCHAM
          LEFFOR=.TRUE.
          IF (TYPE(ICH).EQ.'DEPL'.OR.TYPE(ICH).EQ.'VITE'.OR.
     &        TYPE(ICH).EQ.'ACCE')
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
C
          IF (LEFFOR)
     &     CALL JELIRA(NOMCHA,'LONMAX',NEQ,K1BID)
          CALL WKVECT('&&HARM75.BASE','V V R',NBMODE*NEQ,IDBASE)
C CAS DE LA RESTITUTION SUR TOUTE LA STRUCTURE          
          IF ( TOUSNO ) THEN
            IF (MODE.EQ.BLANC.AND.MATRIC.EQ.BLANC.AND.
     &         TYPCHA.EQ.'DEPL') THEN
             CALL COPMO2(BASEMO,NEQ,NUMDDL,NBMODE,ZR(IDBASE))
            ELSE
             CALL COPMOD(BASEMO,TYPCHA,NEQ,PRCHNO,NBMODE,ZR(IDBASE))
            ENDIF
C CAS DE LA RESTITUTION SUR UNE PARTIE DE LA STRUCTURE SEULEMENT
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
     &                                   ZR(IDEFM+ZI(INUDDL+IDEC-1)-1)
                     ENDIF
 122              CONTINUE
 120           CONTINUE
 110        CONTINUE
          ENDIF
C FIN DE LA RECUPERATION DE LA BASE MODALE
C
C  RESTITUTION PROPREMENT DITE
C
          IARCHI = 0
          IDRESU = ITRESU(ICH)
          PREMS=.TRUE.
          DO 200 I = 0,NBFREQ-1
             IARCHI = IARCHI + 1
             CALL RSEXCH(' ',NOMRES,TYPE(ICH),IARCHI,CHAMNO,IRET)
             IF ( IRET .EQ. 0 ) THEN
             CALL U2MESK('A','ALGORITH2_64',1,CHAMNO)
             ELSEIF ( IRET .EQ. 100 ) THEN
               IF ( TOUSNO ) THEN
                 IF (MODE.EQ.BLANC) THEN
                   IF (LEFFOR) THEN
                     CALL VTDEFS(CHAMNO,TYPREF(ICH),'G','C')
                   ELSE
                     CALL VTCREB(CHAMNO,NUMDDL,'G','C',NEQ)
                   ENDIF
                 ELSE
                   CALL VTCREC(CHAMNO,CHMOD,'G','C',NEQ)
                 ENDIF
               ELSE
                  IF(PREMS) THEN
                    PREMS=.FALSE.     
C                                   
                    IF (NOMGD.EQ.'DEPL_R') THEN
                        NOMGD = 'DEPL_C'
                    ENDIF
C
                    CALL CNOCRE ( MAILLA, NOMGD, NBNOEU, ZI(INUMNO),
     &                  NCMP, ZK8(INOCMP), ZI(INOECP), 'G',' ',CHAMNO)
                    CALL DISMOI('F','PROF_CHNO',CHAMNO,'CHAM_NO',
     &                           IBID,PROF,IRET)
                  ELSE
                    CALL CNOCRE ( MAILLA, NOMGD, NBNOEU, ZI(INUMNO),
     &                  NCMP,ZK8(INOCMP),ZI(INOECP),'G',PROF,CHAMNO)
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
             CALL JELIRA(CHAMNO,'TYPE',IBID,TYPE1)
             CALL ASSERT(TYPE1.EQ.'C')

             IF (LEFFOR .OR. .NOT.TOUSNO)
     &         CALL JELIRA(CHAMNO,'LONMAX',NEQ,K8B)
C             IF (INTERP(1:3).NE.'NON') THEN
C               CALL EXTRAC(INTERP,EPSI,CRIT,NBINSG,ZR(IDINSG),
C     &               ZR(JFREQ+I),ZC(IDRESU),NBMODE,ZR(IDVECG), IBID)
C               CALL MDGPHC(NEQ,NBMODE,ZR(IDBASE),ZC(IDVECG),ZC(LVALE))
C             ELSE
               CALL MDGEPC(NEQ,NBMODE,ZR(IDBASE),
     &                    ZC(IDRESU+(ZI(JNUME+I)-1)*NBMODE),ZC(LVALE))
C             ENDIF
C
             CALL RSNOCH(NOMRES,TYPE(ICH),IARCHI)
             CALL RSADPA(NOMRES,'E',1,'FREQ',IARCHI,0,LFREQ,K8B)
             ZR(LFREQ) = ZR(JFREQ+I)
 200      CONTINUE
          CALL JEDETR ( '&&HARM75.BASE' )
 210   CONTINUE
C
      KREFE  = NOMRES
      CALL WKVECT(KREFE//'.REFD','G V K24',7,LREFE)
      IF (MODE.EQ.' ') THEN
        ZK24(LREFE  ) = ZK24(IADRIF)
        ZK24(LREFE+1) = ZK24(IADRIF+1)
        ZK24(LREFE+2) = ZK24(IADRIF+2)
        ZK24(LREFE+3) = ZK24(IADRIF+3)
        ZK24(LREFE+4) = ZK24(IADRIF+4)
        ZK24(LREFE+5) = ZK24(IADRIF+5)
        ZK24(LREFE+6) = ZK24(IADRIF+6)
      ENDIF
      CALL JELIBE(KREFE//'.REFD')
C
C
C --- MENAGE
C
      CALL JEDETR('&&HARM75.NUME_NOEUD  ')
      CALL JEDETR('&&HARM75.NOM_CMP     ')
      CALL JEDETR('&&HARM75.NB_NEQ      ')
      CALL JEDETR('&&HARM75.NUME_DDL    ')
      CALL JEDETR('&&HARM75.NUM_RANG')
      CALL JEDETR('&&HARM75.FREQ')
      CALL JEDETR('&&HARM75.VECTGENE')
      CALL DETRSD('CHAM_NO','&&HARM75.CHAMNO')

      CALL TITRE
C
      CALL JEDEMA()
      END
