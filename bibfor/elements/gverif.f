      SUBROUTINE GVERIF ( RESU, NOMA, MOTFAC, NOUM )
      IMPLICIT NONE
      CHARACTER*8         RESU, NOMA, NOUM
      CHARACTER*(*)       MOTFAC
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 11/09/2006   AUTEUR GALENNE E.GALENNE 
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
C-----------------------------------------------------------------------
C FONCTION REALISEE:
C
C     VERIFICATION QUE LES NOMS DE GROUPE OU D'ELEMENTS (MAILLE/NOEUD)
C     APPARTIENNENT BIEN AU MAILLAGE
C     -----------------------------------------------------------------
C ENTREE:
C        RESU   : NOM DU CONCEPT RESULTAT DE L'OPERATEUR
C        NOMA   : NOM DU MAILLAGE
C        MOTFAC : MOT-CLE FACTEUR :'FOND_FISS' OU 'FOND_FERME' OU
C                                  'LEVRE_SUP' OU 'LEVRE_INF'
C                 OU MOT-CLE : 'NORMALE' OU 'DTAN_ORIG'
C                                        OU 'DTAN_EXTR'
C        NOUM   : 'NOEUD' OU 'MAILLE'
C        NCNCIN : CONNECTIVITE INVERSE
C     -----------------------------------------------------------------
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
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32     JEXNOM, JEXNUM, JEXATR
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER       KK1,KK2,KK3,JADR,NBOBJ,NENT,NSOM,MM1,MM2,MM3,NCI
      INTEGER       JJJ,NDIM,NGRO,NBGM,NBEM,IER,DIM1,DIM2,DIM3
      INTEGER       IGR,NGR,INO,IRET,LL1,LL2,LL3,ITYP,DIM,JJTYP
      INTEGER       I,J,K,L,NA,NB,JTYPM,IT,JFINF,JFSUP,IRE1,IRE2,IN
      INTEGER       IAA,IAB,IADM,IAGRN,IATYMA,IBID,IGAA,IGAB,K1,K2,K3
      INTEGER       IMA,JEXTR,JNORM,JORIG,JVALE,NCMP,NUMER,J1,J2,J3
      INTEGER       ADRVLC, ACNCIN, NBMA, NBMB, ADRA, ADRB, NUMA, NUMB
      INTEGER       NUFINF, NUFSUP,JSUP, NBMAS
      REAL*8        XPFI,XPFO,YPFI,YPFO,ZPFI,ZPFO,ZRBID,D,PREC
      CHARACTER*4   TYPMA, TYPMP, TYPM
      CHARACTER*8   K8B, MOTCLE, GROUPE, NOEUD, MAILLE, TYPE, NOMGRP(2)
      CHARACTER*24  GRPNOE, COOVAL, NCNCIN
      CHARACTER*24  OBJ1, OBJ2, OBJ4, OBJ5, TRAV
      LOGICAL       LFON
      PARAMETER(PREC=1.D0)
C     -----------------------------------------------------------------
C
      CALL JEMARQ()
      L = LEN(MOTFAC)
      IER = 0
      NCNCIN = '&&OP055.CONNECINVERSE  '
C
      GRPNOE = NOMA//'.GROUPENO       '
      COOVAL = NOMA//'.COORDO    .VALE'
      TYPMP = '    '
      CALL JEVEUO ( COOVAL, 'L', JVALE )
C
C     -----------------------------------------------------------------
      IF ( MOTFAC .EQ. 'NORMALE' ) THEN
C          ---------------------
         CALL GETVR8 (' ','NORMALE',1,1,0,ZRBID,NCMP)
         IF(NCMP.NE.0) THEN
           NCMP = -NCMP
           IF(NCMP.EQ.3) THEN
             CALL WKVECT(RESU//'.NORMALE','G V R8',3,JNORM)
             CALL GETVR8 (' ','NORMALE',1,1,3,ZR(JNORM),NCMP)
           ELSE
             CALL UTMESS('E','GVERIF','LE MOT CLE NORMALE DOIT '
     &                        //  'COMPORTER 3 COMPOSANTES' )
             IER = IER + 1
           ENDIF
         ENDIF
         GO TO 9999
      ENDIF
C
C     -----------------------------------------------------------------
      IF ( MOTFAC .EQ. 'DTAN_ORIG' ) THEN
C          -----------------------
         CALL GETVR8 (' ',MOTFAC,1,1,0,ZRBID,NCMP)
         IF(NCMP.NE.0) THEN
           NCMP = -NCMP
           IF(NCMP.EQ.3) THEN
             CALL WKVECT(RESU//'.DTAN_ORIGINE','G V R8',3,JORIG)
             CALL GETVR8 (' ',MOTFAC,1,1,3,ZR(JORIG),NCMP)
           ELSE
             CALL UTMESS('E','GVERIF','LE MOT CLE DTAN_ORIG DOIT '
     &                         //  'COMPORTER 3 COMPOSANTES' )
             IER = IER + 1
           ENDIF
         ENDIF
         GO TO 9999
      ENDIF
C
C     -----------------------------------------------------------------
      IF ( MOTFAC .EQ. 'DTAN_EXTR' ) THEN
C          -----------------------
         CALL GETVR8 (' ',MOTFAC,1,1,0,ZRBID,NCMP)
         IF(NCMP.NE.0) THEN
           NCMP = -NCMP
           IF(NCMP.EQ.3) THEN
             CALL WKVECT(RESU//'.DTAN_EXTREMITE','G V R8',3,JEXTR)
             CALL GETVR8 (' ',MOTFAC,1,1,3,ZR(JEXTR),NCMP)
           ELSE
             CALL UTMESS('E','GVERIF','LE MOT CLE DTAN_EXTR DOIT '
     &                         //  'COMPORTER 3 COMPOSANTES' )
             IER = IER + 1
           ENDIF
         ENDIF
         GO TO 9999
      ENDIF
C
C     -----------------------------------------------------------------
      IF ( MOTFAC.EQ.'VECT_GRNO_ORIG' ) THEN
C          --------------------------
         CALL GETVEM (NOMA,'GROUP_NO',
     .                   ' ',MOTFAC,1,1,0,NOMGRP,NCMP)
         IF(NCMP.NE.0) THEN
           NCMP = -NCMP
           IF(NCMP.EQ.2) THEN
             CALL WKVECT(RESU//'.DTAN_ORIGINE','G V R8',3,JORIG)
             CALL GETVEM (NOMA,'GROUP_NO',
     .                   ' ',MOTFAC,1,1,2,NOMGRP,NCMP)
C
             CALL JEVEUO (JEXNOM(GRPNOE,NOMGRP(1)),'L',IAGRN)
             NUMER = ZI(IAGRN)
             XPFO = ZR(JVALE-1+3*(NUMER-1)+1)
             YPFO = ZR(JVALE-1+3*(NUMER-1)+2)
             ZPFO = ZR(JVALE-1+3*(NUMER-1)+3)
C
             CALL JEVEUO (JEXNOM(GRPNOE,NOMGRP(2)),'L',IAGRN)
             NUMER = ZI(IAGRN)
             XPFI = ZR(JVALE-1+3*(NUMER-1)+1)
             YPFI = ZR(JVALE-1+3*(NUMER-1)+2)
             ZPFI = ZR(JVALE-1+3*(NUMER-1)+3)
             ZR(JORIG+0)=XPFI-XPFO
             ZR(JORIG+1)=YPFI-YPFO
             ZR(JORIG+2)=ZPFI-ZPFO
C
           ELSE
             CALL UTMESS('E','GVERIF','LE MOT CLE VECT_GRNO_ORIG DOIT '
     &                         //  'COMPORTER 2 GROUPES DE POINTS' )
             IER = IER + 1
           ENDIF
         ENDIF
         GO TO 9999
      ENDIF
C
C     -----------------------------------------------------------------
      IF ( MOTFAC .EQ. 'VECT_GRNO_EXTR' ) THEN
C          ----------------------------
         CALL GETVEM (NOMA,'GROUP_NO',
     .                   ' ',MOTFAC,1,1,0,NOMGRP,NCMP)
         IF(NCMP.NE.0) THEN
           NCMP = -NCMP
           IF(NCMP.EQ.2) THEN
             CALL WKVECT(RESU//'.DTAN_EXTREMITE','G V R8',3,JEXTR)
             CALL GETVEM (NOMA,'GROUP_NO',
     .                   ' ',MOTFAC,1,1,2,NOMGRP,NCMP)
C
             CALL JEVEUO (JEXNOM(GRPNOE,NOMGRP(1)),'L',IAGRN)
             NUMER = ZI(IAGRN)
             XPFO = ZR(JVALE-1+3*(NUMER-1)+1)
             YPFO = ZR(JVALE-1+3*(NUMER-1)+2)
             ZPFO = ZR(JVALE-1+3*(NUMER-1)+3)
C
             CALL JEVEUO (JEXNOM(GRPNOE,NOMGRP(2)),'L',IAGRN)
             NUMER = ZI(IAGRN)
             XPFI = ZR(JVALE-1+3*(NUMER-1)+1)
             YPFI = ZR(JVALE-1+3*(NUMER-1)+2)
             ZPFI = ZR(JVALE-1+3*(NUMER-1)+3)
             ZR(JEXTR+0)=XPFI-XPFO
             ZR(JEXTR+1)=YPFI-YPFO
             ZR(JEXTR+2)=ZPFI-ZPFO
C
           ELSE
             CALL UTMESS('E','GVERIF','LE MOT CLE VECT_GRNO_EXTR DOIT '
     &                         //  'COMPORTER 2 GROUPES DE POINTS' )
             IER = IER + 1
           ENDIF
         ENDIF
         GO TO 9999
      ENDIF
C
C     -----------------------------------------------------------------
C
C OBJETS DE MAILLAGE : OBJ1 A OBJ5
      CALL JEVEUO(NOMA//'.TYPMAIL','L',IATYMA)
      IF (NOUM(1:2) .EQ. 'NO') THEN
         MOTCLE = 'NOEUD'
         GROUPE = 'GROUP_NO'
         OBJ1 = NOMA//'.GROUPENO'
         OBJ2 = NOMA//'.NOMNOE'
C
C ------ SI L'UTILISATEUR FOURNIT UNE LISTE DE NOEUDS OU DE GROUP_NO
C        ON VERIFIERA QUE LES NOEUDS SONT ORDONNES
C
         CALL JEEXIN ( NCNCIN, NCI )
         IF (NCI .EQ. 0) CALL CNCINV (NOMA, IBID, 0, 'V', NCNCIN )
         CALL JEVEUO ( JEXATR(NCNCIN,'LONCUM'), 'L', ADRVLC )
         CALL JEVEUO ( JEXNUM(NCNCIN,1)       , 'L', ACNCIN )
C
      ELSE IF (NOUM(1:2) .EQ. 'MA') THEN
         MOTCLE = 'MAILLE'
         GROUPE = 'GROUP_MA'
         OBJ1 = NOMA//'.GROUPEMA'
         OBJ2 = NOMA//'.NOMMAI'
         OBJ4 = NOMA//'.CONNEX'
         OBJ5 = NOMA//'.NOMNOE'
      ENDIF
C
      NBGM = 0
      NBEM = 0
      CALL GETVID (MOTFAC(1:L),GROUPE,1,1,0,K8B,NGRO)
      CALL GETVID (MOTFAC(1:L),MOTCLE,1,1,0,K8B,NENT)
      NSOM = NGRO + NENT
      IF (NSOM.EQ.NGRO) THEN
         NGRO = -NGRO
         NBGM = MAX(NBGM,NGRO)
      ELSE IF (NSOM.EQ.NENT) THEN
         NENT = -NENT
         NBEM = MAX(NBEM,NENT)
      ENDIF
C
      NDIM = MAX(NBGM,NBEM)
      IF (NDIM .EQ. 0) GOTO 9999
C
C --- ALLOCATION D'UN PREMIER OBJET DE TRAVAIL
C
      TRAV = '&&VERIFE.'//MOTFAC(1:L)//'               '
      CALL WKVECT(TRAV,'V V K8',NDIM,JJJ)
C
      CALL GETVID (MOTFAC(1:L),GROUPE,1,1,NDIM,ZK8(JJJ),NGR)
      DIM1 = 0
      DO 100 I=1,NGR
         CALL JEEXIN (JEXNOM(OBJ1,ZK8(JJJ+I-1)),IRET)
         IF(IRET.NE.0) THEN
            CALL JELIRA (JEXNOM(OBJ1,ZK8(JJJ+I-1)),'LONMAX',NBOBJ,K8B)
            DIM1 = DIM1 + NBOBJ
         ELSE
            IER = IER + 1
            CALL UTMESS('E','GVERIF',ZK8(JJJ+I-1)//' N''EST PAS '//
     &                              'UN GROUP_NO OU UN GROUP_MA' )
         ENDIF
100   CONTINUE
      IF (IER.NE.0) CALL UTMESS('F','GVERIF',
     +                          'ARRET SUR ERREUR(S) UTILISATEUR.')
      DIM2 = MAX(DIM1,NENT)
C
C --- ALLOCATION DE 5 AUTRES OBJETS DE TRAVAIL
C
      IF ( MOTFAC(1:4) .EQ. 'FOND' ) THEN
         CALL WKVECT('&&VERIFE.FOND      .NOEU','V V K8',2*DIM2+1,KK1)
         CALL WKVECT('&&VERIFE.NODER          ','V V I',DIM2,IAA)
         CALL WKVECT('&&VERIFE.NOPRE          ','V V I',DIM2,IAB)
         IF (NGR.GT.0) THEN
            CALL WKVECT('&&VERIFE.GNODER         ','V V I',NGR,IGAA)
            CALL WKVECT('&&VERIFE.GNOPRE         ','V V I',NGR,IGAB)
         ENDIF
         LL1 = KK1
C
      ELSEIF ( MOTFAC .EQ. 'LEVRE_SUP' ) THEN
         CALL WKVECT('&&VERIFE.LEVRESUP  .MAIL','V V K8',DIM2,KK2)
         LL2 = KK2
C
      ELSEIF ( MOTFAC .EQ. 'LEVRE_INF' ) THEN
         CALL WKVECT('&&VERIFE.LEVREINF  .MAIL','V V K8',DIM2,KK3)
         LL3 = KK3
      ENDIF
C
C --- TRAITEMENT DES "GROUP_NO" OU "GROUP_MA"
C     ---------------------------------------
C
      LFON=.FALSE.
      IF(MOTFAC(6:8).EQ.'INF'.OR.MOTFAC(6:8).EQ.'SUP')THEN
            LFON=.TRUE.
      ENDIF
C
      DO 110 IGR = 1, NGR

         CALL JELIRA (JEXNOM(OBJ1,ZK8(JJJ+IGR-1)),'LONMAX',NBOBJ,K8B)
         CALL JEVEUO (JEXNOM(OBJ1,ZK8(JJJ+IGR-1)),'L',JADR)
C
         IF (MOTFAC(1:4).EQ.'FOND') THEN
C
            IF ( NOUM(1:2) .EQ. 'NO' ) THEN
C
C ------------ VERIFICATION QUE CHAQUE PAIRE DE NOEUDS
C              CONSECUTIFS APPARTIENT A UNE MAILLE
C
               IT = 1
               DO 10 I = 1 , NBOBJ-1
                  NA = ZI(JADR+I-1)
                  NB = ZI(JADR+I)
                  NBMA = ZI(ADRVLC+NA+1-1) - ZI(ADRVLC+NA-1)
                  NBMB = ZI(ADRVLC+NB+1-1) - ZI(ADRVLC+NB-1)
                  ADRA = ZI(ADRVLC+NA-1)
                  ADRB = ZI(ADRVLC+NB-1)
                  DO 20 J = 1,NBMA
                    NUMA = ZI(ACNCIN+ADRA-1+J-1)
                    DO 22 K = 1,NBMB
                      NUMB = ZI(ACNCIN+ADRB-1+K-1)
                      ITYP = IATYMA-1+NUMB
                      CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(ITYP)),
     +                             TYPE)
                      IF (TYPE(1:3).EQ.'SEG ') THEN
                        IF ((IT.GT.1) .AND. 
     +                      (TYPE(1:4).NE.TYPMP(1:4))) THEN
                          CALL UTMESS('F','GVERIF',
     +                     'MELANG1 SEG2 ET SEG3 : LES MAILLES DU '//
     +                     'FOND DE FISSURE DOIVENT ETRE DU MEME TYPE')
                        ENDIF
                        TYPMP(1:4) = TYPE(1:4)
                        IT = IT + 1
                        IF(LFON .AND. NUMA.EQ.NUMB) GOTO 24
                      ENDIF
                      IF (.NOT.LFON .AND. NUMA .EQ. NUMB ) GOTO 24
 22                 CONTINUE
 20               CONTINUE
                  CALL UTMESS('F','GVERIF','LE GROUPE DE NOEUDS '//
     &                                      ZK8(JJJ+IGR-1)//
     &                    ' DEFINISSANT LA FISSURE N''EST PAS ORDONNE')
 24               CONTINUE
                  

 10            CONTINUE
C
C ------------ VERIFICATION DES NOEUDS IDENTIQUES POUR 2 GROUP_NO
C              CONSECUTIFS
C
               ZI(IAA + IGR - 1) = ZI(JADR + NBOBJ - 1)
               ZI(IAB + IGR - 1) = ZI(JADR)
               IF ( IGR .GE. 2 ) THEN
                  IF (ZI(IAA + IGR - 2) .NE. ZI(IAB + IGR - 1)) THEN
                     CALL UTMESS('F','GVERIF','ARRET SUR ERREUR UTIL'//
     +                'ISATEUR: DEUX GROUP_NO CONSECUTIFS INCOHERENTS')
                  ELSE
                    DO 102 I=2,NBOBJ
                       CALL JENUNO(JEXNUM(OBJ2,ZI(JADR+I-1)),NOEUD)
                       ZK8(KK1) = NOEUD
                       KK1 = KK1 + 1
102                 CONTINUE
                  ENDIF
               ELSE
                  DO 103 I=1,NBOBJ
                     CALL JENUNO(JEXNUM(OBJ2,ZI(JADR+I-1)),NOEUD)
                     ZK8(KK1) = NOEUD
                     KK1 = KK1 + 1
103               CONTINUE
               ENDIF
C
            ELSEIF ( NOUM(1:2) .EQ. 'MA' ) THEN
C
C --------------- VERIFICATION DU TYPE DE MAILLES
C
               DO 104 IMA=1,NBOBJ
                  CALL JENUNO(JEXNUM(OBJ2,ZI(JADR+IMA-1)),MAILLE)
                  CALL JENONU(JEXNOM(OBJ2,MAILLE),IBID)
                  ITYP = IATYMA-1+IBID
                  CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(ITYP)),TYPE)
                  TYPMA = TYPE(1:3)
                  IF(TYPMA.NE.'SEG ')  CALL UTMESS('F','GVERIF',
     +                      'LES MAILLES DU FOND DE FISSURE DOIVENT '//
     +                      'ETRE DU TYPE SEGMENT')
                  TYPM = TYPE(1:4)
                  IF ((IMA.GT.1).AND.(TYPM(1:4).NE.TYPMP(1:4))) THEN
                   CALL UTMESS('F','GVERIF',
     +                     'MELANG2 SEG2 ET SEG3 : LES MAILLES DU '//
     +                     'FOND DE FISSURE DOIVENT ETRE DU MEME TYPE')
                  ENDIF
                  TYPMP(1:4) = TYPM(1:4)


C
C --------------- VERIFICATION DES NOEUDS IDENTIQUES POUR 2 MAILLES
C                 CONSECUTIVES
C                 
                  CALL JENONU(JEXNOM(OBJ2,MAILLE),IBID)
                  CALL JEVEUO(JEXNUM(OBJ4,IBID),'L',IADM)

                  ZI(IAA + IMA - 1) = ZI(IADM + 1)
                  ZI(IAB + IMA - 1) = ZI(IADM)
                  IF (IMA.EQ.1)  ZI(IGAB + IGR - 1) = ZI(IADM)
                  IF (IMA.EQ.NBOBJ) ZI(IGAA + IGR - 1) = ZI(IADM + 1)
                  IF (IMA.GE.2) THEN
                     IF (ZI(IAA + IMA - 2).NE.ZI(IAB + IMA - 1)) THEN
                        CALL UTMESS('F','GVERIF','ARRET SUR ERREUR U'//
     +                                  'TILISATEUR: DEUX MAILLES DU'//
     +                        'FOND DE FISSURE SONT NON CONSECUTIVES'//
     +                        ' DANS LA NUMEROTATION DES NOEUDS ')
                     ELSE
                        IF (TYPE(1:4) .EQ. 'SEG2' ) THEN
                           CALL JENUNO(JEXNUM(OBJ5,ZI(IADM+1)),NOEUD)
                           ZK8(KK1) = NOEUD
                           KK1 = KK1 + 1
                        ELSE
                           CALL JENUNO(JEXNUM(OBJ5,ZI(IADM+2)),NOEUD)
                           ZK8(KK1) = NOEUD
                           KK1 = KK1 + 1
                           CALL JENUNO(JEXNUM(OBJ5,ZI(IADM+1)),NOEUD)
                           ZK8(KK1) = NOEUD
                           KK1 = KK1 + 1
                        ENDIF
                     ENDIF
                  ELSE
                     CALL JENUNO(JEXNUM(OBJ5,ZI(IADM)),NOEUD)
                     ZK8(KK1) = NOEUD
                     KK1 = KK1 + 1
                     IF (TYPE(1:4).EQ.'SEG2') THEN
                        CALL JENUNO(JEXNUM(OBJ5,ZI(IADM+1)),NOEUD)
                        ZK8(KK1) = NOEUD
                        KK1 = KK1 + 1
                     ELSE
                        CALL JENUNO(JEXNUM(OBJ5,ZI(IADM+2)),NOEUD)
                        ZK8(KK1) = NOEUD
                        KK1 = KK1 + 1
                        CALL JENUNO(JEXNUM(OBJ5,ZI(IADM+1)),NOEUD)
                        ZK8(KK1) = NOEUD
                        KK1 = KK1 + 1
                     ENDIF
                  ENDIF
104            CONTINUE
               DIM3 = KK1 - LL1
C
C ------------ VERIFICATION DES NOEUDS IDENTIQUES POUR 2 GROUP_MA
C              CONSECUTIFS
C
               IF (IGR.GE.2) THEN
                  IF (ZI(IGAA + IGR - 2).NE.ZI(IGAB + IGR - 1)) THEN
                     CALL UTMESS('F','GVERIF','ARRET SUR ERREUR UTIL'//
     +                  'ISATEUR: 2 GROUP_MA DU FOND DE FISSURE SONT'//
     +               'NON CONSECUTIFS DANS LA NUMEROTATION DES NOEUDS')
                  ENDIF
               ENDIF
            ENDIF
C
         ENDIF
C
         DO 105 I = 1, NBOBJ
            IF (MOTFAC.EQ.'LEVRE_SUP') THEN
               CALL JENUNO(JEXNUM(OBJ2,ZI(JADR+I-1)),MAILLE)
               CALL JENONU(JEXNOM(OBJ2,MAILLE),IBID)
               ITYP=IATYMA-1+IBID
               CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(ITYP)),TYPE)
               TYPMA = TYPE(1:4)
               IF ((TYPMA.NE.'QUAD').AND.(TYPMA.NE.'TRIA')) THEN
                  CALL UTMESS('F','GVERIF','LES MAILLES DES LEVRES '//
     +                   'DOIVENT ETRE DU TYPE QUADRANGLE OU TRIANGLE')
               ELSE
                  ZK8(KK2) = MAILLE
                  KK2 = KK2 + 1
               ENDIF
C
            ELSEIF(MOTFAC.EQ.'LEVRE_INF') THEN
               CALL JENUNO(JEXNUM(OBJ2,ZI(JADR+I-1)),MAILLE)
               CALL JENONU(JEXNOM(OBJ2,MAILLE),IBID)
               ITYP=IATYMA-1+IBID
               CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(ITYP)),TYPE)
               TYPMA = TYPE(1:4)
               IF ((TYPMA.NE.'QUAD').AND.(TYPMA.NE.'TRIA')) THEN
                  CALL UTMESS('F','GVERIF','LES MAILLES DES LEVRES '//
     +                   'DOIVENT ETRE DU TYPE QUADRANGLE OU TRIANGLE')
               ELSE
                  ZK8(KK3) = MAILLE
                  KK3 = KK3 + 1
               ENDIF
            ENDIF
105      CONTINUE
C
110   CONTINUE
C
C --- TRAITEMENT DES "NOEUD" OU "MAILLE"
C     ----------------------------------
C
      CALL GETVID (MOTFAC(1:L),MOTCLE,1,1,NDIM,ZK8(JJJ),NBOBJ)
      DO 200 INO = 1, NBOBJ
         CALL JENONU (JEXNOM(OBJ2,ZK8(JJJ+INO-1)),IRET)
         IF(IRET .EQ. 0) THEN
            CALL UTMESS('E','GVERIF',MOTCLE//' '//ZK8(JJJ+INO-1)//
     &                     'NE FAIT PAS PARTIE DU MAILLAGE : '//NOMA )
            IER = IER + 1
         ENDIF
 200  CONTINUE
      IF (IER.NE.0) CALL UTMESS('F','GVERIF',
     +                          'ARRET SUR ERREUR(S) UTILISATEUR.')
C
      IF ( MOTFAC(1:4) .EQ. 'FOND' ) THEN
         IF (NOUM(1:2).EQ.'NO') THEN
C
C --------- VERIFICATION QUE CHAQUE PAIRE DE NOEUDS
C           CONSECUTIFS APPARTIENT A UNE MAILLE
C
            IT = 1
            DO 210 INO = 1 , NBOBJ-1
               CALL JENONU (JEXNOM(OBJ2,ZK8(JJJ+INO-1)),NA)
               CALL JENONU (JEXNOM(OBJ2,ZK8(JJJ+INO  )),NB)
               NBMA = ZI(ADRVLC+NA+1-1) - ZI(ADRVLC+NA-1)
               NBMB = ZI(ADRVLC+NB+1-1) - ZI(ADRVLC+NB-1)
               ADRA = ZI(ADRVLC+NA-1)
               ADRB = ZI(ADRVLC+NB-1)
               DO 212 J = 1,NBMA
                  NUMA = ZI(ACNCIN+ADRA-1+J-1)
                  DO 214 K = 1,NBMB
                     NUMB = ZI(ACNCIN+ADRB-1+K-1)
                     ITYP = IATYMA-1+NUMB
                     CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(ITYP)),
     +                             TYPE)
                     IF (TYPE(1:3).EQ.'SEG ') THEN
                        IF ((IT.GT.1) .AND. 
     +                      (TYPE(1:4).NE.TYPMP(1:4))) THEN
                          CALL UTMESS('F','GVERIF',
     +                     'MELANG3 SEG2 ET SEG3 : LES MAILLES DU '//
     +                     'FOND DE FISSURE DOIVENT ETRE DU MEME TYPE')
                        ENDIF
                        TYPMP(1:4) = TYPE(1:4)
                        IT = IT + 1
                        IF(LFON .AND. NUMA.EQ.NUMB) GOTO 216
                      ENDIF
                      IF (.NOT.LFON .AND. NUMA .EQ. NUMB ) GOTO 216
 214              CONTINUE
 212           CONTINUE
               CALL UTMESS('F','GVERIF','LA LISTE DE NOEUDS DEFINIS'//
     &                           'SANT LA FISSURE N''EST PAS ORDONNEE')
 216           CONTINUE
                  
                  
 210        CONTINUE
            DO 218 INO = 1, NBOBJ
               ZK8(KK1) = ZK8(JJJ + INO - 1)
               KK1 = KK1 + 1
 218        CONTINUE
C
         ELSE IF(NOUM(1:2).EQ.'MA') THEN
            DO 220 INO = 1, NBOBJ
               CALL JENONU(JEXNOM(OBJ2,ZK8(JJJ+INO-1)),IBID)
               ITYP=IATYMA-1+IBID
               CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(ITYP)),TYPE)
               TYPMA = TYPE(1:3)
               IF(TYPMA.NE.'SEG ')  CALL UTMESS('F','GVERIF',
     +                      'LES MAILLES DU FOND DE FISSURE DOIVENT '//
     +                      'ETRE DU TYPE SEGMENT')
               TYPM = TYPE(1:4)
               IF ((INO.GT.1).AND.(TYPM(1:4).NE.TYPMP(1:4))) THEN
                CALL UTMESS('F','GVERIF',
     +                    'MELANG4 SEG2 ET SEG3 : LES MAILLES DU '//
     +                    'FOND DE FISSURE DOIVENT ETRE DU MEME TYPE')
               ENDIF
               TYPMP(1:4) = TYPM(1:4)

               CALL JENONU(JEXNOM(OBJ2,ZK8(JJJ+INO-1)),IBID)
               CALL JEVEUO(JEXNUM(OBJ4,IBID),'L',IADM)
C
C ------------ VERIFICATION DES NOEUDS IDENTIQUES POUR 2 MAILLES
C              CONSECUTIVES
C
               ZI(IAA + INO - 1) = ZI(IADM + 1)
               ZI(IAB + INO - 1) = ZI(IADM)
               IF (INO.GE.2) THEN
                  IF (ZI(IAA + INO - 2).NE.ZI(IAB + INO - 1)) THEN
                     CALL UTMESS('F','GVERIF',
     +                 'ARRET SUR ERREUR(S) UTILISATEUR: DEUX MAILLES'
     +             //  ' DU FOND DE FISSURE SONT NON CONSECUTIVES'
     +             //  ' DANS LA NUMEROTATION DES NOEUDS ')
                  ELSE
                     IF (TYPE(1:4).EQ.'SEG2') THEN
                        CALL JENUNO(JEXNUM(OBJ5,ZI(IADM+1)),NOEUD)
                        ZK8(KK1) = NOEUD
                        KK1 = KK1 + 1
                     ELSE
                        CALL JENUNO(JEXNUM(OBJ5,ZI(IADM+2)),NOEUD)
                        ZK8(KK1) = NOEUD
                        KK1 = KK1 + 1
                        CALL JENUNO(JEXNUM(OBJ5,ZI(IADM+1)),NOEUD)
                        ZK8(KK1) = NOEUD
                        KK1 = KK1 + 1
                     ENDIF
                  ENDIF
               ELSE
                  CALL JENUNO(JEXNUM(OBJ5,ZI(IADM)),NOEUD)
                  ZK8(KK1) = NOEUD
                  KK1 = KK1 + 1
                  IF (TYPE(1:4).EQ.'SEG2') THEN
                     CALL JENUNO(JEXNUM(OBJ5,ZI(IADM+1)),NOEUD)
                     ZK8(KK1) = NOEUD
                     KK1 = KK1 + 1
                  ELSE
                     CALL JENUNO(JEXNUM(OBJ5,ZI(IADM+2)),NOEUD)
                     ZK8(KK1) = NOEUD
                     KK1 = KK1 + 1
                     CALL JENUNO(JEXNUM(OBJ5,ZI(IADM+1)),NOEUD)
                     ZK8(KK1) = NOEUD
                     KK1 = KK1 + 1
                  ENDIF
               ENDIF
 220        CONTINUE
         ENDIF
C
      ELSEIF ( MOTFAC .EQ. 'LEVRE_SUP' ) THEN
         DO 230 INO = 1, NBOBJ
            CALL JENONU(JEXNOM(OBJ2,ZK8(JJJ+INO-1)),IBID)
            ITYP=IATYMA-1+IBID
            CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(ITYP)),TYPE)
            TYPMA = TYPE(1:4)
            IF ((TYPMA.NE.'QUAD').AND.(TYPMA.NE.'TRIA')) THEN
               CALL UTMESS('F','GVERIF','LES MAILLES DES LEVRES '//
     +                   'DOIVENT ETRE DU TYPE QUADRANGLE OU TRIANGLE')
            ELSE
               ZK8(KK2) = ZK8(JJJ + INO - 1)
               KK2 = KK2 + 1
            ENDIF
 230     CONTINUE
C
      ELSEIF ( MOTFAC .EQ. 'LEVRE_INF') THEN
         DO 240 INO = 1, NBOBJ
            CALL JENONU(JEXNOM(OBJ2,ZK8(JJJ+INO-1)),IBID)
            ITYP=IATYMA-1+IBID
            CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(ITYP)),TYPE)
            TYPMA = TYPE(1:4)
            IF ((TYPMA.NE.'QUAD').AND.(TYPMA.NE.'TRIA')) THEN
               CALL UTMESS('F','GVERIF','LES MAILLES DES LEVRES '//
     +                   'DOIVENT ETRE DU TYPE QUADRANGLE OU TRIANGLE')
            ELSE
               ZK8(KK3) = ZK8(JJJ + INO - 1)
               KK3 = KK3 + 1
            ENDIF
 240     CONTINUE
      ENDIF
      DIM3 = KK1 - LL1
C
C --- VERIFICATION QU'IL N Y A PAS DUPLICATION DES ENTITES ET STOCKAGE
C
C --- ALLOCATION DES 3 OBJETS DE STOCKAGE
C
      IF ( MOTFAC(1:4) .EQ. 'FOND' ) THEN
C          -----------------------
         IF(NOUM(1:2).EQ.'NO') DIM = DIM2
         IF(NOUM(1:2).EQ.'MA') DIM = DIM3
         IF (MOTFAC(6:10).EQ.'FERME') THEN
            CALL WKVECT(RESU//'.FOND      .NOEU','G V K8',DIM+1,MM1)
         ELSEIF(MOTFAC(6:8).EQ.'INF') THEN
           CALL WKVECT(RESU//'.FOND_INF  .NOEU','G V K8',DIM,MM1)
         ELSEIF(MOTFAC(6:8).EQ.'SUP') THEN
           CALL WKVECT(RESU//'.FOND_SUP  .NOEU','G V K8',DIM,MM1)
         ELSE
            CALL WKVECT(RESU//'.FOND      .NOEU','G V K8',DIM,MM1)
         ENDIF
         K1 = 1
         DO 500 I=1,DIM-1
            IF ( ZK8(LL1 + I - 1) .NE. '0' ) THEN
               ZK8(MM1 + K1 - 1) = ZK8(LL1 + I - 1)
               K1 = K1 + 1
               DO 505 J=I+1,DIM
                  IF ( ZK8(LL1 + I - 1).EQ.ZK8(LL1 + J - 1) ) THEN
                     ZK8(LL1 + J - 1) = '0'
                     J1 = I
                  ENDIF
505            CONTINUE
           ENDIF
500     CONTINUE
        IF(ZK8(LL1 + DIM - 1).NE.'0') THEN
           ZK8(MM1 + K1 - 1) = ZK8(LL1 + DIM - 1)
           K1 = K1 + 1
        ENDIF
        K1 = K1 - 1
C
        IF (K1.NE.DIM) THEN
           CALL UTMESS('E','GVERIF','ERREUR : LE FOND DE FISSURE '//
     +                 'POSSEDE UN NOEUD REPETE 2 FOIS : NOEUD '//
     +                  ZK8(LL1 + J1 - 1)//'. REVOIR LES DONNEES')
           IER = IER+1
        ENDIF
C
        IF (MOTFAC(6:10).EQ.'FERME')  ZK8(MM1+DIM+1-1) = ZK8(MM1+1- 1)
C
        IF(LFON)THEN
          CALL JEEXIN(RESU//'.FOND      .TYPE',IRET)
          IF(IRET.EQ.0)THEN
             CALL WKVECT(RESU//'.FOND      .TYPE','G V K8',1,JTYPM)
             ZK8(JTYPM) = TYPMP
          ELSE
             CALL JEVEUO(RESU//'.FOND      .TYPE','L',JJTYP)
             IF(ZK8(JJTYP)(1:4).NE.TYPMP)THEN
                CALL UTMESS('F','GVERIF','LES MAILLES DU FOND_INF 
     +                    ET DU FOND_SUP SONT DE TYPE DIFFERENT')
             ENDIF
          ENDIF
        ELSE
          CALL WKVECT(RESU//'.FOND      .TYPE','G V K8',1,JTYPM)
          ZK8(JTYPM) = TYPMP
        ENDIF

C
C       LORSQUE LE FOND DE FISSURE EST DEFINI PAR FOND_INF ET FOND_SUP,
C       ON VERIFIE QUE LES NOEUDS SONT EN VIV A VIS
        CALL JEEXIN(RESU//'.FOND_INF  .NOEU',IRE1)
        CALL JEEXIN(RESU//'.FOND_SUP  .NOEU',IRE2)
        IF(IRE1.NE.0 .AND. IRE2.NE.0)THEN
           CALL JEVEUO(RESU//'.FOND_INF  .NOEU','L',JFINF)
           CALL JEVEUO(RESU//'.FOND_SUP  .NOEU','L',JFSUP)
           DO 555 IN = 1 , DIM
             CALL JENONU(JEXNOM(OBJ2,ZK8(JFINF+IN-1)),NUFINF)
             CALL JENONU(JEXNOM(OBJ2,ZK8(JFSUP+IN-1)),NUFSUP)
             D = ABS(ZR(JVALE+3*(NUFINF-1))-
     +               ZR(JVALE+3*(NUFSUP-1)))
             D = D+ABS(ZR(JVALE+3*(NUFINF-1)+1)-
     +                 ZR(JVALE+3*(NUFSUP-1)+1))
             D = D+ABS(ZR(JVALE+3*(NUFINF-1)+2)-
     +                 ZR(JVALE+3*(NUFSUP-1)+2))
             IF ( SQRT(D) .GT.PREC)THEN
               CALL UTDEBM('F','GVERIF','LES NOEUDS')
               CALL UTIMPK('S',' ',1,ZK8(JFINF+IN-1))
               CALL UTIMPK('S','ET',1,ZK8(JFSUP+IN-1))
               CALL UTIMPK('S','NE SONT PAS EN VIS A VIS.',1,' ')
               CALL UTFINM()
             ENDIF
 555       CONTINUE
        ENDIF
C
C
      ELSEIF ( MOTFAC .EQ. 'LEVRE_SUP' ) THEN
C              -----------------------
         CALL WKVECT(RESU//'.LEVRESUP  .MAIL','G V K8',DIM2,MM2)
         K2 = 1
         DO 600 I=1,DIM2-1
            IF ( ZK8(LL2 + I - 1).NE.'0' ) THEN
               ZK8(MM2 + K2 - 1) = ZK8(LL2 + I - 1)
               K2 = K2 + 1
               DO 605 J=I+1,DIM2
                  IF ( ZK8(LL2 + I - 1).EQ.ZK8(LL2 + J - 1) ) THEN
                     ZK8(LL2 + J - 1) = '0'
                     J2 = I
                  ENDIF
605            CONTINUE
            ENDIF
600      CONTINUE
         IF (ZK8(LL2 + DIM2 - 1).NE.'0') THEN
            ZK8(MM2 + K2 - 1) = ZK8(LL2 + DIM2 - 1)
            K2 = K2 + 1
         ENDIF
         K2 = K2 - 1
C
         IF (K2.NE.DIM2) THEN
            CALL UTMESS('E','GVERIF','ERREUR : LA LEVRE SUPERIEURE '//
     +                  'POSSEDE UNE MAILLE REPETEE 2 FOIS : MAILLE '//
     +                   ZK8(LL2 + J2 - 1)//'. REVOIR LES DONNEES')
            IER = IER+1
         ENDIF
C
      ELSEIF ( MOTFAC .EQ. 'LEVRE_INF' ) THEN
C              -----------------------
         CALL WKVECT(RESU//'.LEVREINF  .MAIL','G V K8',DIM2,MM3)
         K3 = 1
         DO 700 I=1,DIM2-1
            IF ( ZK8(LL3 + I - 1).NE.'0' ) THEN
               ZK8(MM3 + K3 - 1) = ZK8(LL3 + I - 1)
               K3 = K3 + 1
               DO 705 J=I+1,DIM2
                  IF ( ZK8(LL3 + I - 1).EQ.ZK8(LL3 + J - 1) ) THEN
                     ZK8(LL3 + J - 1) = '0'
                     J3 = I
                  ENDIF
705            CONTINUE
            ENDIF
700      CONTINUE
         IF (ZK8(LL3 + DIM2 - 1).NE.'0') THEN
            ZK8(MM3 + K3 - 1) = ZK8(LL3 + DIM2 - 1)
            K3 = K3 + 1
         ENDIF
         K3 = K3 - 1
C
         IF (K3.NE.DIM2) THEN
            CALL UTMESS('E','GVERIF','ERREUR : LA LEVRE INFERIEURE '//
     +                  'POSSEDE UNE MAILLE REPETEE 2 FOIS : MAILLE '//
     +                   ZK8(LL3 + J3 - 1)//'. REVOIR LES DONNEES')
            IER = IER+1
         ENDIF
         
C COMPARAISON LEVRE SUP / LEVRE INF
         CALL JEVEUO ( RESU//'.LEVRESUP  .MAIL', 'L', JSUP )
         CALL JELIRA ( RESU//'.LEVRESUP  .MAIL', 'LONMAX',NBMAS,K8B)
         DO 710 I = 1,NBMAS
           DO 715 J = 1,DIM2
            IF (ZK8(JSUP+I-1) .EQ. ZK8(MM3+J-1) ) THEN
              CALL UTMESS('F','GVERIF','ERREUR : LA LEVRE INFERIEURE'//
     +            ' ET LA LEVRE SUPERIEURE ONT UNE MAILLE SURFACIQUE'//
     +            ' EN COMMUN. REVOIR LES DONNEES')
            END IF
715        CONTINUE
710      CONTINUE

      ENDIF
C
C --- DESTRUCTION DES OBJETS DE TRAVAIL
C
      CALL JEDETR (TRAV)
C
      IF ( MOTFAC(1:4) .EQ. 'FOND' ) THEN
         CALL JEDETR('&&VERIFE.FOND      .NOEU')
         CALL JEDETR('&&VERIFE.NODER          ')
         CALL JEDETR('&&VERIFE.NOPRE          ')
         CALL JEDETR('&&VERIFE.GNODER         ')
         CALL JEDETR('&&VERIFE.GNOPRE         ')
      ELSEIF(MOTFAC.EQ.'LEVRE_SUP') THEN
         CALL JEDETR('&&VERIFE.LEVRESUP  .MAIL')
      ELSEIF(MOTFAC.EQ.'LEVRE_INF') THEN
         CALL JEDETR('&&VERIFE.LEVREINF  .MAIL')
      ENDIF
C
      IF (IER.NE.0) CALL UTMESS('F','GVERIF',
     +                          'ARRET SUR ERREUR(S) UTILISATEUR.')
9999  CONTINUE
      CALL JEDEMA()
      END
