      SUBROUTINE IRCECA(IFI,LIGREL,NBGREL,LONGR,NCMPMX,VALE,NOMGD,
     &   NCMPGD,CELD,NBNOMA,TYPMA,NOMSYM,NBMAT,
     &   LRESU,NBCPUT,NCMPUT,IMODL,NCMPV,NUCMPV,NIVE)
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INTEGER       IFI,LIGREL(*),NBGREL,LONGR(*),NCMPMX,CELD(*)
      INTEGER       NBNOMA(*),TYPMA(*),NBMAT,NBCPUT,IMODL
      INTEGER       NCMPV,NUCMPV(*),NIVE
      CHARACTER*(*) NOMGD,NCMPGD(*),NOMSYM,NCMPUT(*)
      REAL*8                                     VALE(*)
      LOGICAL       LRESU
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 26/04/2011   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C        ECRITURE D'UN CHAMELEM SUR FICHIER CASTEM
C        A VALEURS REELLES
C  ENTREE:
C     IFI   : UNITE LOGIQUE DU FICHIER UNIVERSEL
C     LIGREL: LIGREL COMPLET
C     NBGREL: NOMBRE DE GRELS
C     LONGR : POINTEUR DE LONGUEUR DE LIGREL
C     NCMPMX: NOMBRE MAXI DE CMP DE LA GRANDEUR NOMGD
C     VALE  : VALEURS DU CHAM_ELEM
C     NOMGD : NOM DE LA GRANDEUR: SIEF_R, EPSI_R,...
C     NCMPGD: NOMS DES CMP
C     CELD  : DESCRIPTEUR DU CHAM_ELEM (MODES LOCAUX,ADRESSES->.CELV)
C     NBNOMA: NOMBRE DE NOEUDS DE CHAQUE MAILLE
C     TYPMA : TYPE_MAILLES
C     NBMAT : NOMBRE DE MAILLES A IMPRIMER
C     LRESU : =.TRUE. IMPRESSION D'UN CONCEPT RESULTAT
C     NBCPUT: NOMBRE DE CMP DEMANDE PAR L'UTILISATEUR
C     NCMPUT: NOMS DES CMP DEMANDE PAR L'UTILISATEUR
C     NIVE  : NIVEAU IMPRESSION CASTEM 3 OU 10
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER           ZI
      COMMON / IVARJE / ZI(1)
      REAL*8            ZR
      COMMON / RVARJE / ZR(1)
      COMPLEX*16        ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL           ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8       ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                       ZK24
      CHARACTER*32                                ZK32
      CHARACTER*80                                         ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32      JEXNUM, JEXNOM,JEXATR
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
      INTEGER      NBVAR,IAD,ITYPE,IZERO,IUN
      INTEGER      MODSAV,NBELT,DIGDEL,TABEC(10)
      INTEGER      IMODEL,ILONG
      CHARACTER*3  TOTO
      CHARACTER*24 VALK(2)
      CHARACTER*8  NOMCO,GTYPE,KTYPE,K8B
      CHARACTER*16 CTYPE
      LOGICAL       EXISDG, LMODE, FIRST, LNOCEN
C     ------------------------------------------------------------------
C
C  --- INITIALISATIONS ----
C
      CALL JEMARQ()
C
      IF (.NOT.LRESU) THEN
         CALL JEVEUO ( '&&OP0039.LAST', 'E', JLAST )
         INUM = ZI(JLAST-1+5) + 1
      ELSE
         INUM = 0
      ENDIF
      NBVAR = 0
      IZERO = 0
C
      CALL WKVECT ('&&IRCECA.NBRCMP','V V I',NBGREL  ,JNBR)
      CALL WKVECT ('&&IRCECA.ENTETE','V V I',NBGREL*7,JENT)
      CALL JEVEUO ( JEXNUM('&&OP0039.LIGREL',IMODL), 'E', JLI )
      CALL JEVEUO ('&CATA.TE.MODELOC', 'L', IMODEL )
      CALL JEVEUO (JEXATR('&CATA.TE.MODELOC','LONCUM'),'L',ILONG)
C     ------------------------------------------------------------------
C
C     --- DETERMINATION DU NOMBRE MAXIMUM DE SOUS-POINTS ---
C
      ICOMAX = 0
      NBSOBJ = 0
      NBSMO  = ZI(JLI-1+1)
      CALL WKVECT ( '&&IRCECA.JLOGI', 'V V L', NBSMO*NCMPMX, JLOG)
      CALL WKVECT ( '&&IRCECA.JNBVA', 'V V I', NBSMO+1     , JVA )
      DO 10 ISO = 1 , NBSMO
        LMODE = .FALSE.
        MODSAV = CELD(CELD(4+ZI(JLI+(ISO-1)*(4+NBGREL)+5))+2)
        NBGR = ZI(JLI+(ISO-1)*(4+NBGREL)+3)
        DO 12 IGR = 1 , NBGR
          IGRE  = ZI(JLI+(ISO-1)*(4+NBGREL)+4+IGR)
          ICOEF=MAX(1,CELD(4))
          IF (ICOEF.GT.ICOMAX) ICOMAX = ICOEF
 12     CONTINUE
        IF ( NCMPV .GT. 0 ) THEN
          NCMP = 0
          DO 14 I = 1 , NCMPV
            IF ( NUCMPV(I) .LE. ICOMAX ) THEN
              NCMP = NCMP + 1
            ELSE IF (NOMGD.EQ.'VARI_R') THEN
              NCMP = NCMP + 1
            ELSE
              CALL CODENT ( NUCMPV(I), 'G', K8B )
              NOMCO = 'V'//K8B
              CALL U2MESK('A','PREPOST_74',1,NOMCO)
            ENDIF
 14       CONTINUE
          IF ( NCMP .EQ. 0 ) THEN
             CALL U2MESS('A','PREPOST_75')
             GOTO 9999
          ENDIF
          ICOMAX = NCMP
        ENDIF
        IF (ICOMAX.GT.999) CALL U2MESS('F','PREPOST_76')
        DO 16 IGR = 1 , NBGR
          IGRE  = ZI(JLI+(ISO-1)*(4+NBGREL)+4+IGR)
          ICOEF=MAX(1,CELD(4))
          MODE=CELD(CELD(4+IGRE)+2)
          IF ( MODE .EQ. 0 ) GO TO 16
          MODSAV = MODE
          IF (MODE.NE.MODSAV.AND.MODE.NE.0) THEN
             CALL U2MESS('A','PREPOST_77')
             GOTO 10
          ENDIF
          LMODE = .TRUE.
          CALL JEVEUO(JEXNUM('&CATA.TE.MODELOC',MODE),'L',JMOD)
          NEC = NBEC (ZI(JMOD-1+2))
          CALL ASSERT ( NEC .LE. 10 )
          CALL DGMODE ( MODE, IMODEL, ILONG, NEC, TABEC )
          IF ( NBCPUT .NE. 0 ) THEN
            DO 18 ICM = 1,NBCPUT
               IF (NOMGD.EQ.'VARI_R') THEN
                 CALL LXLIIS(NCMPUT(ICM)(2:8),IVARI,IRET)
                 IF ((NCMPUT(ICM)(1:1).NE.'V').OR.(IRET.NE.0)) THEN
                   VALK (1) = NCMPUT(ICM)
                   VALK (2) = 'VARI_R'
                 CALL U2MESK('F', 'CALCULEL6_49',2,VALK)
                 END IF
                 ZL(JLOG-1+(ISO-1)*NCMPMX+IVARI) = .TRUE.
                 NBVAR = NBVAR + 1
                 GO TO 18
               ELSE
                DO 20 I = 1,NCMPMX
                  IF ( NCMPUT(ICM) .EQ. NCMPGD(I) ) THEN
                     ZL(JLOG-1+(ISO-1)*NCMPMX+I) = .TRUE.
                     NBVAR = NBVAR + 1
                     GO TO 18
                  ENDIF
 20             CONTINUE
              ENDIF
              VALK (1) = NCMPUT(ICM)
              VALK (2) = NOMGD
              CALL U2MESG('A', 'PREPOST5_25',2,VALK,0,0,0,0.D0)
 18         CONTINUE
          ELSE
            DO 22 I=1,NCMPMX
              IF ( EXISDG(TABEC,I) ) THEN
                ZL(JLOG-1+(ISO-1)*NCMPMX+I) = .TRUE.
                NBVAR = NBVAR + 1
              ENDIF
 22         CONTINUE
          ENDIF
          IF ( NBVAR .EQ. 0 ) THEN
             CALL U2MESS('A','PREPOST_75')
             GOTO 9999
          ENDIF
 16     CONTINUE
        IF ( LMODE ) THEN
          NBSOBJ = NBSOBJ + 1
          ZI(JNBR-1+NBSOBJ) = DIGDEL(MODSAV)
          ZI(JENT-1+(NBSOBJ-1)*7+1) = ZI(JLI+(ISO-1)*(4+NBGREL)+2)
        ELSE
          ZI(JLI+(ISO-1)*(4+NBGREL)+3) = 0
        ENDIF
 10   CONTINUE
      ZI(JVA-1+1) = ICOMAX
C     ------------------------------------------------------------------
C
C     --- NOMS DES COMPOSANTES ET POSITIONS DANS LA GRANDEUR ---
C
      CALL WKVECT('&&IRCECA.NOMVAR','V V K8',NCMPMX*ICOMAX*NBSMO,JNOM)
      CALL WKVECT('&&IRCECA.POSVAR','V V I' ,NCMPMX*ICOMAX*NBSMO,JPOS)
      ICOMA2 = 0
      DO 50 ISO = 1 , NBSMO
        NBGR = ZI(JLI+(ISO-1)*(4+NBGREL)+3)
        DO 58 IGR = 1 , NBGR
          IGRE  = ZI(JLI+(ISO-1)*(4+NBGREL)+4+IGR)
          ICOEF=MAX(1,CELD(4))
          IF (ICOEF.GT.ICOMA2) ICOMA2 = ICOEF
 58     CONTINUE
        DO 52 I = 1 , NCMPMX
          IF ( ZL(JLOG-1+(ISO-1)*NCMPMX+I) ) THEN
            NOMCO = NCMPGD(I)
            IF    (NOMCO.EQ.'SIXX    ') THEN
              NOMCO = 'SMXX    '
            ELSEIF(NOMCO.EQ.'SIYY    ') THEN
              NOMCO = 'SMYY    '
            ELSEIF(NOMCO.EQ.'SIZZ    ') THEN
              NOMCO = 'SMZZ    '
            ELSEIF(NOMCO.EQ.'SIXY    ') THEN
              NOMCO = 'SMXY    '
            ELSEIF(NOMCO.EQ.'SIXZ    ') THEN
              NOMCO = 'SMXZ    '
            ELSEIF(NOMCO.EQ.'SIYZ    ') THEN
              NOMCO = 'SMYZ    '
            ENDIF
            ZI(JVA+(ISO-1)+1) = ZI(JVA+(ISO-1)+1) + 1
            NBVA = ZI(JVA+(ISO-1)+1)
            IF ( ICOMA2 .GT. 1 ) THEN
              IF ( NCMPV .GT. 0 ) THEN
                DO 54 ISP = 1 , NCMPV
                  CALL CODENT(NUCMPV(ISP),'G',TOTO)
                  ZK8(JNOM-1+(ISO-1)*NCMPMX*ICOMAX+NBVA-1+ISP) =
     &                                                       'V'//TOTO
                  ZI(JPOS-1+(ISO-1)*NCMPMX*ICOMAX+NBVA-1+ISP) = I
 54             CONTINUE
              ELSE
                DO 56 ISP = 1 , ICOMA2
                  CALL CODENT(ISP,'G',TOTO)
                  ZK8(JNOM-1+(ISO-1)*NCMPMX*ICOMAX+NBVA-1+ISP) =
     &                                                       'V'//TOTO
                  ZI(JPOS-1+(ISO-1)*NCMPMX*ICOMAX+NBVA-1+ISP) = I
 56             CONTINUE
              ENDIF
            ELSE
              IUTIL= LXLGUT(NOMCO)
              IF ( IUTIL .LE. 4 ) THEN
                ZK8(JNOM-1+(ISO-1)*NCMPMX*ICOMAX+NBVA) = NOMCO
              ELSE
                ZK8(JNOM-1+(ISO-1)*NCMPMX*ICOMAX+NBVA) =
     &                              NOMCO(1:2)//NOMCO((IUTIL-1):IUTIL)
              ENDIF
              ZI(JPOS-1+(ISO-1)*NCMPMX*ICOMAX+NBVA) = I
            ENDIF
          ENDIF
 52     CONTINUE
 50   CONTINUE
C     ------------------------------------------------------------------
C
C     --- ECRITURE DE L'EN-TETE ---
C
      ITYPE = 39
      IZERO =  0
      IDEU  =  2
      IUN   = 1
      WRITE (IFI,'(A,I4)')   ' ENREGISTREMENT DE TYPE',IDEU
      IF ( LRESU ) THEN
       IF(NIVE.EQ.3) THEN
        WRITE (IFI,'(A,I4,A,I4,A,I4)')  ' PILE NUMERO',ITYPE,
     &                  'NBRE OBJETS NOMMES ',IZERO,'NBRE OBJETS ',IUN
       ELSEIF (NIVE.EQ.10) THEN
        WRITE (IFI,'(A,I4,A,I8,A,I8)')  ' PILE NUMERO',ITYPE,
     &                  'NBRE OBJETS NOMMES',IZERO,'NBRE OBJET ',IUN
       ENDIF
      ELSE
       IF(NIVE.EQ.3) THEN
        WRITE (IFI,'(A,I4,A,I4,A,I4)')  ' PILE NUMERO',ITYPE,
     &                  'NBRE OBJETS NOMMES ',IUN,'NBRE OBJETS ',IUN
        WRITE(IFI,'(1X,A8)') NOMSYM
        WRITE(IFI,'(I5)') INUM
       ELSEIF (NIVE.EQ.10) THEN
        WRITE (IFI,'(A,I4,A,I8,A,I8)')  ' PILE NUMERO',ITYPE,
     &                  'NBRE OBJETS NOMMES',IUN,'NBRE OBJETS',IUN
        WRITE(IFI,'(1X,A8)') NOMSYM
        WRITE(IFI,'(I8)') INUM
       ENDIF
      ENDIF
C
      LKNAME = 16
      IBID = 4
      IF(NIVE.EQ.3) WRITE(IFI,'(16(I5))') NBSOBJ,IDEU,IBID,LKNAME
      IF(NIVE.EQ.10) WRITE(IFI,'(10(I8))') NBSOBJ,IDEU,IBID,LKNAME
      IF(NOMGD(1:4).EQ.'SIEF'.OR.NOMGD(1:4).EQ.'SIGM') THEN
         CTYPE = 'CONTRAINTES'
      ELSE IF (NOMGD(1:4).EQ.'EPSI') THEN
         CTYPE = 'DEFORMATIONS'
      ELSE
         CTYPE = NOMGD
      ENDIF
      WRITE(IFI,'(1X,A71)')  CTYPE
      IOBJ = 0
      DO 100 ISO=1,NBSMO
        NBVAR = ZI(JVA+(ISO-1)+1)
        ICOMAX= ZI(JVA-1+1)
        IF( NBVAR.NE.0) THEN
          IOBJ = IOBJ + 1
          ZI(JENT-1+(IOBJ-1)*7+2) = IZERO
          ZI(JENT-1+(IOBJ-1)*7+3) = NBVAR*ICOMAX
          ZI(JENT-1+(IOBJ-1)*7+4) = IZERO
          ZI(JENT-1+(IOBJ-1)*7+5) = IZERO
          ZI(JENT-1+(IOBJ-1)*7+6) = IZERO
          ZI(JENT-1+(IOBJ-1)*7+7) = IZERO
        ENDIF
 100  CONTINUE
      IF(NIVE.EQ.3) THEN
       WRITE(IFI,'(16I5)') (ZI(JENT-1+I),I=1,NBSOBJ*7)
      ELSEIF(NIVE.EQ.10) THEN
       WRITE(IFI,'(10I8)') (ZI(JENT-1+I),I=1,NBSOBJ*7)
C      NOMS DES CONSTITUANTS (2A8 COLLES POUR UN K16 PAR SOUS-ZONE)
C      GIBI LIT DONC 2*NBSOBJ A8 SUR UN FORMAT 8(1X,A8), IL FAUT DONC
C      ECRIRE (2*NBSOBJ-1)/8+1 LIGNES
       DO 101 IOBJ=1, (2*NBSOBJ-1)/8+1
          WRITE(IFI,'(A)')
 101   CONTINUE
      ENDIF
C 8001 FORMAT(8(1X,A8))
C     ------------------------------------------------------------------
C
C     --- IMPRESSION ---
C
      CALL WKVECT ( '&&IRCECA.BID', 'V V I', NCMPMX*ICOMAX, JBID )
      NBMAT = LONGR(NBGREL+1)
      CALL GICOOR ()
      NBSMO = ZI(JLI-1+1)
      FIRST = .TRUE.
      ICOMA2 = 0
      LNOCEN=.FALSE.
      DO 200 ISO = 1 , NBSMO
        FIRST  = .TRUE.
        NBGR  = ZI(JLI+(ISO-1)*(4+NBGREL)+3)
        NBVAR = ZI(JVA+(ISO-1)+1)
        IF( NBGR .NE. 0 ) THEN
          NBELT = ZI(JLI+(ISO-1)*(4+NBGREL)+4)
          IELT = 0
          DO 201 IGR = 1 , NBGR
            IGRE  = ZI(JLI+(ISO-1)*(4+NBGREL)+4+IGR)
            ICOEF=MAX(1,CELD(4))
            IF (ICOEF.GT.ICOMA2) ICOMA2 = ICOEF
 201      CONTINUE
          DO 202 IGR = 1 , NBGR
            IGREL  = ZI(JLI+(ISO-1)*(4+NBGREL)+4+IGR)
            MODE=CELD(CELD(4+IGREL)+2)
            IPOIN1 = LONGR(IGREL)
            IPOIN2 = LONGR(IGREL+1)
            NBELGR = IPOIN2-IPOIN1-1
            IF ( MODE .EQ. 0 ) THEN
               IELT = IELT + NBELGR
               GO TO 202
            ENDIF
            JMOD = IMODEL+ZI(ILONG-1+MODE)-1
            NEC = NBEC (ZI(JMOD-1+2))
            CALL ASSERT(NEC .LE. 10)
            CALL DGMODE ( MODE,IMODEL, ILONG, NEC, TABEC )
            IAD=CELD(CELD(4+IGREL)+8)
            NSCAL = DIGDEL(MODE)
            ICOEF=MAX(1,CELD(4))
            NSCA = NSCAL*ICOEF
            NCMPP=0
            DO 204 I=1,NCMPMX
              IF ( EXISDG(TABEC,I) ) THEN
                NCMPP = NCMPP+1
              ENDIF
 204        CONTINUE
C
            IEL = LIGREL(IPOIN1)
            ITYPE = TYPMA(IEL)
            CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ITYPE),KTYPE)
C
            NPCALC = NSCAL / NCMPP
            IF (KTYPE.EQ.'QUAD9'.OR.KTYPE.EQ.'TRIA7') NPCALC = NPCALC-1
            IF (KTYPE.EQ.'PENTA18') NPCALC = NPCALC-3
            IF (KTYPE.EQ.'SEG4') NPCALC = NPCALC-2
C
            IF (FIRST) THEN
              CALL WKVECT('&&IRCECA.VALE','V V R',
     &                                  NBELT*NPCALC*NBVAR*ICOMAX,JVALE)
              FIRST=.FALSE.
            ENDIF
C
C -- ECRITURE DE L'EN-TETE DE CHAQUE SOUS OBJETS ----
C
            CALL IRMAC2 ( KTYPE, ITYCA, GTYPE, IBID )
            CALL JEVEUO(JEXNOM('&&GILIRE.CORR_ASTER_GIBI',GTYPE),'L',
     &                                                           IACORR)
            DO 206 IELG = 1 , NBELGR
             IEL = LIGREL(IPOIN1+IELG-1)
             IF (IEL.LE.0) GO TO 206
             IELT = IELT + 1
C
C --  RECHERCHE DE L'ADRESSE DANS VALE DU DEBUT DES VALEURS --
C
             IACHML = IAD + NSCA * (IELG-1)
C
C    --- CHAMELEM AUX NOEUDS ---
C
             NNOE = NBNOMA(IEL)
             IF (KTYPE.EQ.'QUAD9' .OR. KTYPE.EQ.'TRIA7') THEN
                NNOE = NNOE-1
                LNOCEN=.TRUE.
             ENDIF
             IF (KTYPE.EQ.'SEG4') NNOE = NNOE-2
             IF (KTYPE.EQ.'PENTA18')THEN
                NNOE = NNOE-3
                LNOCEN=.TRUE.
             ENDIF
             IF(NPCALC.NE.NNOE) THEN
               CALL U2MESS('F','PREPOST_79')
             ENDIF
             ITYPE = TYPMA(IEL)
             DO 214 INOS = 1 , NNOE
               IJ   = ZI(IACORR-1+INOS)
               J    = IACHML-1+NCMPP*ICOMA2*(IJ-1)
               JADR = JVALE-1+(IELT-1)*NBVAR*NPCALC*ICOMAX
     &                       +(INOS-1)*NBVAR*ICOMAX
               IC = 0
               DO 208 ICMP = 1 , NCMPMX
                 IF ( EXISDG(TABEC,ICMP) ) THEN
                   IC = IC + 1
                   DO 210 ICMC = 1 , NBVAR
                     ICMCAS = ZI(JPOS-1+(ISO-1)*NCMPMX*ICOMAX+ICMC)
                     IF( ICMP .EQ. ICMCAS ) THEN
                       IF ( NCMPV .GT. 0 ) THEN
                         DO 211 ISPV = 1 , NCMPV
                           IF ( NUCMPV(ISPV) .LE. ICOMA2 )
     &                ZR(JADR+ICMC-1+ISPV)=VALE(J+IC-1+NUCMPV(ISPV))
 211                     CONTINUE
                       ELSE
                         DO 212 ISP = 1 , ICOMA2
                           ZR(JADR+ICMC-1+ISP)=VALE(J+IC-1+ISP)
 212                     CONTINUE
                       ENDIF
                     ENDIF
 210               CONTINUE
                 ENDIF
 208           CONTINUE
 214         CONTINUE
 206       CONTINUE
 202     CONTINUE
         DO 220 I = 1,NBVAR*ICOMAX
           ZI(JBID-1+I) = IZERO
 220     CONTINUE
         IF(NIVE.EQ.3) THEN
         WRITE(IFI,'(16I5)') (ZI(JBID-1+I),I=1,NBVAR*ICOMAX)
         ELSEIF(NIVE.EQ.10) THEN
         WRITE(IFI,'(10I8)') (ZI(JBID-1+I),I=1,NBVAR*ICOMAX)
         ENDIF
         WRITE(IFI,'(8(1X,A8))') (ZK8(JNOM-1+(ISO-1)*NCMPMX*ICOMAX+I),
     &                                                 I=1,NBVAR*ICOMAX)
         WRITE(IFI,'(8(1X,A8))') ('REAL*8  ',' ',I=1,NBVAR*ICOMAX)
         ZI(JBID-1+1) = NPCALC
         ZI(JBID-1+2) = NBELT
         ZI(JBID-1+3) = IZERO
         ZI(JBID-1+4) = IZERO
         DO 222 JV = 1 , NBVAR
           DO 224 ISP = 1 , ICOMAX
             IF(NIVE.EQ.3) THEN
              WRITE(IFI,'(16I5)')    (ZI(JBID-1+I),I=1,4)
             ELSEIF(NIVE.EQ.10) THEN
              WRITE(IFI,'(10I8)')    (ZI(JBID-1+I),I=1,4)
             ENDIF
             WRITE(IFI,'(1P,3E22.13E3)') (ZR(JVALE-1+I),
     &                  I=JV*ISP,NBELT*NPCALC*NBVAR*ICOMAX,NBVAR*ICOMAX)
 224       CONTINUE
 222     CONTINUE
         CALL JEDETR('&&IRCECA.VALE')
        ENDIF
 200  CONTINUE
      IF(LNOCEN)THEN
        CALL U2MESS('A','PREPOST_80')
      ENDIF
C     ------------------------------------------------------------------
 9999 CONTINUE
      IF(.NOT.LRESU)  ZI(JLAST-1+5) = INUM
      CALL JEDETR('&&GILIRE.CORR_ASTER_GIBI')
      CALL JEDETR('&&IRCECA.BID')
      CALL JEDETR('&&IRCECA.ENTETE')
      CALL JEDETR('&&IRCECA.JLOGI')
      CALL JEDETR('&&IRCECA.JNBVA')
      CALL JEDETR('&&IRCECA.NBRCMP')
      CALL JEDETR('&&IRCECA.NOMVAR')
      CALL JEDETR('&&IRCECA.POSVAR')
      CALL JEDETR('&&IRCECA.VALE')
C
      CALL JEDEMA()
      END
