      SUBROUTINE GVERIF(RESU,NOMA,MOTFAC,NOUM,CODE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 20/10/1999   AUTEUR DURAND C.DURAND 
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
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
C
C FONCTION REALISEE:
C
C     VERIFICATION QUE LES NOMS DE GROUPE OU D'ELEMENTS (MAILLE/NOEUD)
C     APPARTIENNENT BIEN AU MAILLAGE
C     ------------------------------------------------------------------
C ENTREE:
C        RESU   : NOM DU CONCEPT RESULTAT DE L'OPERATEUR
C        NOMA   : NOM DU MAILLAGE
C        MOTFAC : MOT-CLE FACTEUR :'FOND' OU 'FOND_FERME' OU 'LEVRE_SUP'
C                                  OU 'LEVRE_INF'
C                 OU MOT-CLE : 'NORMALE' OU 'DTAN_ORIG'
C                                        OU 'DTAN_EXTR'
C        NOUM   : 'NOEUD' OU 'MAILLE'
C
C SORTIE
C        CODE   : CODE RETOUR SUR L'EXISTENCE DU MOT CLE LEVRE_INF
C                 .TRUE.   : LEVRE_INF
C                 .FALSE.  : PAS DE LEVRE_INF
C     ------------------------------------------------------------------
C
      CHARACTER*24      OBJ1,OBJ2,OBJ4,OBJ5,TRAV
      CHARACTER*8       MOTCLE,GROUPE
      CHARACTER*8       RESU,NOMA,NOUM
      CHARACTER*8       NOEUD,MAILLE,TYPE
      CHARACTER*4       TYPMA
      CHARACTER*(*)     MOTFAC
      CHARACTER*8       NOMGRP(2)
C
      INTEGER           KK1,KK2,KK3,IADR,N1,NENT,NSOM,MM1,MM2,MM3
      INTEGER           JJJ,NDIM,NGRO,NBGM,NBEM,IER,DIM1,DIM2,DIM3
      INTEGER           IGR,NGR,INO,NNO,IRET,LL1,LL2,LL3,ITYP,DIM
C
      LOGICAL           CODE
      CHARACTER*24 GRPNOE, COOVAL
      INTEGER I,IAA,IAB,IADM,IAGRN,IATYMA,IBID,IGAA,IGAB,K1,K2,K3
      INTEGER IMA,J,JEXTR,JNORM,JORIG,JVALE,K,L,NCMP,NUMER,J1,J2,J3
      REAL*8 XPFI,XPFO,YPFI,YPFO,ZPFI,ZPFO,ZRBID
C
C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      COMMON/IVARJE/ZI(1)
      COMMON/RVARJE/ZR(1)
      COMMON/CVARJE/ZC(1)
      COMMON/LVARJE/ZL(1)
      COMMON/KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI
      REAL*8 ZR
      COMPLEX*16 ZC
      LOGICAL ZL
      CHARACTER*8  ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32,JEXNOM,JEXNUM
      CHARACTER*80 ZK80
      CHARACTER*8  K8BID
      CHARACTER*1 K1BID
C---------------- FIN COMMUNS NORMALISES  JEVEUX  ----------------------
C
C
      CALL JEMARQ()
      CODE = .FALSE.
      L = LEN(MOTFAC)
      IER = 0
      NBGM = 0
      NBEM = 0
C
      GRPNOE = NOMA//'.GROUPENO       '
      COOVAL = NOMA//'.COORDO    .VALE'
      CALL JEVEUO ( COOVAL, 'E', JVALE )
C
      IF (MOTFAC .EQ. 'NORMALE') THEN
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
      IF (MOTFAC .EQ. 'DTAN_ORIG') THEN
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
      IF (MOTFAC .EQ. 'DTAN_EXTR') THEN
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
      IF (MOTFAC.EQ.'VECT_GRNO_ORIG') THEN
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
      IF (MOTFAC.EQ.'VECT_GRNO_EXTR') THEN
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
C OBJETS DE MAILLAGE : OBJ1 A OBJ5
C
      IF (NOUM(1:2) .EQ. 'NO') THEN
         MOTCLE = 'NOEUD'
         GROUPE = 'GROUP_NO'
         OBJ1 = NOMA//'.GROUPE'//'NO'
         OBJ2 = NOMA//'.NOMNOE'
      ELSE IF (NOUM(1:2) .EQ. 'MA') THEN
         MOTCLE = 'MAILLE'
         GROUPE = 'GROUP_MA'
         OBJ1 = NOMA//'.GROUPE'//'MA'
         OBJ2 = NOMA//'.NOMMAI'
         CALL JEVEUO(NOMA//'.TYPMAIL','L',IATYMA)
         OBJ4 = NOMA//'.CONNEX'
         OBJ5 = NOMA//'.NOMNOE'
      ENDIF
C
C
      CALL GETVID (MOTFAC(1:L),GROUPE,1,1,0,K8BID,NGRO)
      CALL GETVID (MOTFAC(1:L),MOTCLE,1,1,0,K8BID,NENT)
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
C ALLOCATION D'UN PREMIER OBJET DE TRAVAIL
C
      TRAV = '&&VERIFE.'//MOTFAC(1:L)//'               '
      CALL WKVECT(TRAV,'V V K8',NDIM,JJJ)
C
      CALL GETVID (MOTFAC(1:L),GROUPE,1,1,NDIM,ZK8(JJJ),NGR)
      DIM1 = 0
      DO 100 I=1,NGR
         CALL JEEXIN (JEXNOM(OBJ1,ZK8(JJJ+I-1)),IRET)
         IF(IRET.NE.0) THEN
            CALL JELIRA (JEXNOM(OBJ1,ZK8(JJJ+I-1)),'LONMAX',N1,K1BID)
            DIM1 = DIM1 + N1
         ELSE
           CALL UTMESS('F','GVERIF',ZK8(JJJ+I-1)//' N''EST PAS'
     &                 //' UN GROUP_NO OU UN GROUP_MA' )
         ENDIF
100   CONTINUE
      DIM2 = MAX(DIM1,NENT)
C
C ALLOCATION DE 5 AUTRES OBJETS DE TRAVAIL
C
      IF(MOTFAC(1:4).EQ.'FOND') THEN
        CALL WKVECT('&&VERIFE.FOND      .NOEU','V V K8',2*DIM2+1,KK1)
        CALL WKVECT('&&VERIFE.NODER          ','V V I',DIM2,IAA)
        CALL WKVECT('&&VERIFE.NOPRE          ','V V I',DIM2,IAB)
        IF(NGR.GT.0) THEN
          CALL WKVECT('&&VERIFE.GNODER         ','V V I',NGR,IGAA)
          CALL WKVECT('&&VERIFE.GNOPRE         ','V V I',NGR,IGAB)
        ENDIF
        LL1 = KK1
      ENDIF
C
      IF(MOTFAC.EQ.'LEVRE_SUP') THEN
          CALL WKVECT('&&VERIFE.LEVRESUP  .MAIL','V V K8',DIM2,KK2)
          LL2 = KK2
      ENDIF
C
      IF(MOTFAC.EQ.'LEVRE_INF') THEN
          CALL WKVECT('&&VERIFE.LEVREINF  .MAIL','V V K8',DIM2,KK3)
          LL3 = KK3
      ENDIF
C
      DO 110 IGR = 1, NGR
         CALL JEEXIN (JEXNOM(OBJ1,ZK8(JJJ+IGR-1)),IRET)
C
         IF (IRET .EQ. 0) THEN
             CALL UTMESS('E','GVERIF','LE GROUPE '//ZK8(JJJ+IGR-1)//
     &                      'NE FAIT PAS PARTIE DU MAILLAGE : '//NOMA )
             IER = IER + 1
         ELSE
             CALL JELIRA (JEXNOM(OBJ1,ZK8(JJJ+IGR-1)),'LONMAX',N1,K1BID)
             CALL JEVEUO (JEXNOM(OBJ1,ZK8(JJJ+IGR-1)),'L',IADR)
C
             IF(MOTFAC(1:4).EQ.'FOND') THEN
C
               IF(NOUM(1:2).EQ.'NO') THEN
C
C  VERIFICATION DES NOEUDS IDENTIQUES POUR 2 GROUP_NO CONSECUTIFS
C
                 ZI(IAA + IGR - 1) = ZI(IADR + N1 - 1)
                 ZI(IAB + IGR - 1) = ZI(IADR)
                 IF (IGR.GE.2) THEN
                      IF(ZI(IAA + IGR - 2).NE.ZI(IAB + IGR - 1)) THEN
                        CALL UTMESS('F','GVERIF',
     +                       'ARRET SUR ERREUR(S) UTILISATEUR: '
     +                //    'DEUX GROUP_NO CONSECUTIFS INCOHERENTS')
                      ELSE
                        DO 102 I=2,N1
                           CALL JENUNO(JEXNUM(OBJ2,ZI(IADR+I-1)),NOEUD)
                           ZK8(KK1) = NOEUD
                           KK1 = KK1 + 1
102                     CONTINUE
                      ENDIF
                  ELSE
                     DO 103 I=1,N1
                        CALL JENUNO(JEXNUM(OBJ2,ZI(IADR+I-1)),NOEUD)
                        ZK8(KK1) = NOEUD
                        KK1 = KK1 + 1
103                  CONTINUE
C
                  ENDIF
C
                 ELSE IF(NOUM(1:2).EQ.'MA') THEN
C
                   DO 104 IMA=1,N1
                    CALL JENUNO(JEXNUM(OBJ2,ZI(IADR+IMA-1)),MAILLE)
                    CALL JENONU(JEXNOM(NOMA//'.NOMMAI',MAILLE),IBID)
                    ITYP=IATYMA-1+IBID
                    CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(ITYP)),TYPE)
                    TYPMA = TYPE(1:3)
                    IF(TYPMA.NE.'SEG ') THEN
                         CALL UTMESS('F','GVERIF',
     +                   'LES MAILLES DU FOND DE FISSURE DOIVENT ETRE'
     +               //   ' DU TYPE SEGMENT')
                    ENDIF
                    CALL JENONU(JEXNOM(NOMA//'.NOMMAI',MAILLE),IBID)
                    CALL JEVEUO(JEXNUM(OBJ4,IBID),'L',IADM)
C
C  VERIFICATION DES NOEUDS IDENTIQUES POUR 2 MAILLES CONSECUTIVES
C
                 ZI(IAA + IMA - 1) = ZI(IADM + 1)
                 ZI(IAB + IMA - 1) = ZI(IADM)
                 IF (IMA.EQ.1)  ZI(IGAB + IGR - 1) = ZI(IADM)
                 IF (IMA.EQ.N1) ZI(IGAA + IGR - 1) = ZI(IADM + 1)
                 IF (IMA.GE.2) THEN
                     IF(ZI(IAA + IMA - 2).NE.ZI(IAB + IMA - 1)) THEN
                       CALL UTMESS('F','GVERIF',
     +                 'ARRET SUR ERREUR(S) UTILISATEUR: DEUX MAILLES'
     +             //  ' DU FOND DE FISSURE SONT NON CONSECUTIVES'
     +             //  ' DANS LA NUMEROTATION DES NOEUDS ')
                     ELSE
                       IF(TYPE(1:4).EQ.'SEG2') THEN
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
                     IF(TYPE(1:4).EQ.'SEG2') THEN
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
C  VERIFICATION DES NOEUDS IDENTIQUES POUR 2 GROUP_MA CONSECUTIFS
C
                 IF (IGR.GE.2) THEN
                    IF(ZI(IGAA + IGR - 2).NE.ZI(IGAB + IGR - 1)) THEN
                      CALL UTMESS('F','GVERIF',
     +                  'ARRET SUR ERREUR(S) UTILISATEUR: 2 GROUP_MA'
     +             //   ' DU FOND DE FISSURE SONT NON CONSECUTIFS'
     +             //   ' DANS LA NUMEROTATION DES NOEUDS ')
                    ENDIF
                 ENDIF
               ENDIF
             ENDIF
C
               DO 105 I=1,N1
                  IF(MOTFAC.EQ.'LEVRE_SUP') THEN
                    CALL JENUNO(JEXNUM(OBJ2,ZI(IADR+I-1)),MAILLE)
                    CALL JENONU(JEXNOM(NOMA//'.NOMMAI',MAILLE),IBID)
                    ITYP=IATYMA-1+IBID
                    CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(ITYP)),TYPE)
                    TYPMA = TYPE(1:4)
                    IF((TYPMA.NE.'QUAD').AND.(TYPMA.NE.'TRIA')) THEN
                         CALL UTMESS('F','GVERIF',
     +                   'LES MAILLES DES LEVRES DOIVENT ETRE DU TYPE'
     +               //  ' QUADRANGLE OU TRIANGLE')
                     ELSE
                         ZK8(KK2) = MAILLE
                         KK2 = KK2 + 1
                     ENDIF
                  ENDIF
C
                  IF(MOTFAC.EQ.'LEVRE_INF') THEN
                    CALL JENUNO(JEXNUM(OBJ2,ZI(IADR+I-1)),MAILLE)
                    CALL JENONU(JEXNOM(NOMA//'.NOMMAI',MAILLE),IBID)
                    ITYP=IATYMA-1+IBID
                    CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(ITYP)),TYPE)
                    TYPMA = TYPE(1:4)
                    IF((TYPMA.NE.'QUAD').AND.(TYPMA.NE.'TRIA')) THEN
                         CALL UTMESS('F','GVERIF',
     +                   'LES MAILLES DES LEVRES DOIVENT ETRE DU TYPE'
     +              //   ' QUADRANGLE OU TRIANGLE')
                    ELSE
                         ZK8(KK3) = MAILLE
                         KK3 = KK3 + 1
                    ENDIF
                  ENDIF
105            CONTINUE
C
         ENDIF
110   CONTINUE
C
      CALL GETVID (MOTFAC(1:L),MOTCLE,1,1,NDIM,ZK8(JJJ),NNO)
      DO 120 INO = 1, NNO
         CALL JENONU (JEXNOM(OBJ2,ZK8(JJJ+INO-1)),IRET)
         IF(IRET .EQ. 0) THEN
            CALL UTMESS('E','GVERIF',MOTCLE//' '//ZK8(JJJ+INO-1)//
     &                     'NE FAIT PAS PARTIE DU MAILLAGE : '//NOMA )
            IER = IER + 1
         ELSE
            IF(MOTFAC(1:4).EQ.'FOND') THEN
               IF(NOUM(1:2).EQ.'NO') THEN
                 ZK8(KK1) = ZK8(JJJ + INO - 1)
                 KK1 = KK1 + 1
               ELSE IF(NOUM(1:2).EQ.'MA') THEN
                    CALL JENONU(JEXNOM(NOMA//'.NOMMAI',ZK8(JJJ+INO-1)),
     &                          IBID)
                    ITYP=IATYMA-1+IBID
                    CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(ITYP)),TYPE)
                    TYPMA = TYPE(1:3)
                    IF(TYPMA.NE.'SEG ') THEN
                         CALL UTMESS('F','GVERIF',
     +                   'LES MAILLES DU FOND DE FISSURE DOIVENT ETRE'
     +               //   ' DU TYPE SEGMENT')
                    ENDIF
                    CALL JENONU(JEXNOM(NOMA//'.NOMMAI',ZK8(JJJ+INO-1)),
     &                          IBID)
                    CALL JEVEUO(JEXNUM(OBJ4,IBID),'L',IADM)
C
C  VERIFICATION DES NOEUDS IDENTIQUES POUR 2 MAILLES CONSECUTIVES
C
                 ZI(IAA + INO - 1) = ZI(IADM + 1)
                 ZI(IAB + INO - 1) = ZI(IADM)
                 IF (INO.GE.2) THEN
                     IF(ZI(IAA + INO - 2).NE.ZI(IAB + INO - 1)) THEN
                       CALL UTMESS('F','GVERIF',
     +                 'ARRET SUR ERREUR(S) UTILISATEUR: DEUX MAILLES'
     +             //  ' DU FOND DE FISSURE SONT NON CONSECUTIVES'
     +             //  ' DANS LA NUMEROTATION DES NOEUDS ')
                     ELSE
                       IF(TYPE(1:4).EQ.'SEG2') THEN
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
                     IF(TYPE(1:4).EQ.'SEG2') THEN
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
               ENDIF
            ENDIF
C
            IF(MOTFAC.EQ.'LEVRE_SUP') THEN
                CALL JENONU(JEXNOM(NOMA//'.NOMMAI',ZK8(JJJ+INO-1)),
     &                      IBID)
                ITYP=IATYMA-1+IBID
                CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(ITYP)),TYPE)
                TYPMA = TYPE(1:4)
                IF((TYPMA.NE.'QUAD').AND.(TYPMA.NE.'TRIA')) THEN
                    CALL UTMESS('F','GVERIF',
     +                  'LES MAILLES DES LEVRES DOIVENT ETRE DU TYPE'
     +              // ' QUADRANGLE OU TRIANGLE')
                ELSE
                    ZK8(KK2) = ZK8(JJJ + INO - 1)
                    KK2 = KK2 + 1
                ENDIF
            ENDIF
C
            IF(MOTFAC.EQ.'LEVRE_INF') THEN
                CALL JENONU(JEXNOM(NOMA//'.NOMMAI',ZK8(JJJ+INO-1)),
     &                      IBID)
                ITYP=IATYMA-1+IBID
                CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(ITYP)),TYPE)
                TYPMA = TYPE(1:4)
                IF((TYPMA.NE.'QUAD').AND.(TYPMA.NE.'TRIA')) THEN
                     CALL UTMESS('F','GVERIF',
     +               'LES MAILLES DES LEVRES DOIVENT ETRE DU TYPE'
     +            //' QUADRANGLE OU TRIANGLE')
                ELSE
                     ZK8(KK3) = ZK8(JJJ + INO - 1)
                     KK3 = KK3 + 1
                ENDIF
            ENDIF
         ENDIF
120   CONTINUE
      DIM3 = KK1 - LL1
C
C VERIFICATION QU'IL N Y A PAS DUPLICATION DES ENTITES
C ET STOCKAGE
C
C ALLOCATION DES 3 OBJETS DE STOCKAGE
C
      IF(MOTFAC(1:4).EQ.'FOND') THEN
        IF(NOUM(1:2).EQ.'NO') DIM = DIM2
        IF(NOUM(1:2).EQ.'MA') DIM = DIM3
        IF(MOTFAC(6:10).EQ.'FERME') THEN
          CALL WKVECT(RESU//'.FOND      .NOEU','G V K8',DIM+1,MM1)
        ELSE
          CALL WKVECT(RESU//'.FOND      .NOEU','G V K8',DIM,MM1)
        ENDIF
        K1 = 1
        DO 500 I=1,DIM-1
           IF( ZK8(LL1 + I - 1).NE.'0' ) THEN
               ZK8(MM1 + K1 - 1) = ZK8(LL1 + I - 1)
               K1 = K1 + 1
               DO 505 J=I+1,DIM
                  IF( ZK8(LL1 + I - 1).EQ.ZK8(LL1 + J - 1) ) THEN
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
        IF(K1.NE.DIM) THEN
           CALL UTMESS('E','GVERIF',
     +     'ERREUR : LE FOND DE FISSURE POSSEDE UN NOEUD '
     +   //' REPETE 2 FOIS : NOEUD '//ZK8(LL1 + J1 - 1)
     +   //'. REVOIR LES DONNEES')
        IER = IER+1
        ENDIF
C
        IF(MOTFAC(6:10).EQ.'FERME') THEN
           ZK8(MM1+DIM+1-1) = ZK8(MM1+1- 1)
        ENDIF
C
      ENDIF
C
      IF(MOTFAC.EQ.'LEVRE_SUP') THEN
          CALL WKVECT(RESU//'.LEVRESUP  .MAIL','G V K8',DIM2,MM2)
          K2 = 1
          DO 600 I=1,DIM2-1
             IF( ZK8(LL2 + I - 1).NE.'0' ) THEN
                 ZK8(MM2 + K2 - 1) = ZK8(LL2 + I - 1)
                 K2 = K2 + 1
                 DO 605 J=I+1,DIM2
                    IF( ZK8(LL2 + I - 1).EQ.ZK8(LL2 + J - 1) ) THEN
                        ZK8(LL2 + J - 1) = '0'
                        J2 = I
                    ENDIF
605              CONTINUE
             ENDIF
600       CONTINUE
          IF(ZK8(LL2 + DIM2 - 1).NE.'0') THEN
             ZK8(MM2 + K2 - 1) = ZK8(LL2 + DIM2 - 1)
             K2 = K2 + 1
          ENDIF
          K2 = K2 - 1
C
          IF(K2.NE.DIM2) THEN
             CALL UTMESS('E','GVERIF',
     +       'ERREUR : LA LEVRE SUPERIEURE POSSEDE UNE MAILLE '
     +     //' REPETEE 2 FOIS : MAILLE '//ZK8(LL2 + J2 - 1)
     +     //'. REVOIR LES DONNEES')
          IER = IER+1
          ENDIF
      ENDIF
C
      IF(MOTFAC.EQ.'LEVRE_INF') THEN
          CALL WKVECT(RESU//'.LEVREINF  .MAIL','G V K8',DIM2,MM3)
          K3 = 1
          DO 700 I=1,DIM2-1
             IF( ZK8(LL3 + I - 1).NE.'0' ) THEN
                 ZK8(MM3 + K3 - 1) = ZK8(LL3 + I - 1)
                 K3 = K3 + 1
                 DO 705 J=I+1,DIM2
                    IF( ZK8(LL3 + I - 1).EQ.ZK8(LL3 + J - 1) ) THEN
                        ZK8(LL3 + J - 1) = '0'
                        J3 = I
                    ENDIF
705              CONTINUE
             ENDIF
700       CONTINUE
          IF(ZK8(LL3 + DIM2 - 1).NE.'0') THEN
             ZK8(MM3 + K3 - 1) = ZK8(LL3 + DIM2 - 1)
             K3 = K3 + 1
          ENDIF
          K3 = K3 - 1
C
          IF(K3.NE.DIM2) THEN
             CALL UTMESS('E','GVERIF',
     +       'ERREUR : LA LEVRE INFERIEURE POSSEDE UNE MAILLE '
     +     //' REPETEE 2 FOIS : MAILLE '//ZK8(LL3 + J3 - 1)
     +     //'. REVOIR LES DONNEES')
          IER = IER+1
          ENDIF
C
      ENDIF
C
C DESTRUCTION DES OBJETS DE TRAVAIL
C
      CALL JEDETR (TRAV)
C
      IF(MOTFAC(1:4).EQ.'FOND') THEN
         CALL JEDETR('&&VERIFE.FOND      .NOEU')
         CALL JEDETR('&&VERIFE.NODER          ')
         CALL JEDETR('&&VERIFE.NOPRE          ')
         CALL JEDETR('&&VERIFE.GNODER         ')
         CALL JEDETR('&&VERIFE.GNOPRE         ')
      ENDIF
C
      IF(MOTFAC.EQ.'LEVRE_SUP') THEN
         CALL JEDETR('&&VERIFE.LEVRESUP  .MAIL')
      ENDIF
C
      IF(MOTFAC.EQ.'LEVRE_INF') THEN
         CODE = .TRUE.
         CALL JEDETR('&&VERIFE.LEVREINF  .MAIL')
      ENDIF
C
      IF (IER.NE.0) CALL UTMESS('F','GVERIF',
     +                          'ARRET SUR ERREUR(S) UTILISATEUR.')
9999  CONTINUE
      CALL JEDEMA()
      END
