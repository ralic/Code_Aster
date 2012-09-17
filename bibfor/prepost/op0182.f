      SUBROUTINE OP0182()
      IMPLICIT  NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 18/09/2012   AUTEUR LADIER A.LADIER 
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
C TOLE CRP_6
C
C     OPERATEUR  "MODI_OBSTACLE"
C
C     DEFINITION DES VARIABLES
C        ETUBE : EPAISSEUR DU TUBE
C        RTUBE : RAYON EXTERIEURE DU TUBE
C        ROBST : RAYON INTERIEURE DE L'OBSTACLE
C        HOBST : HAUTEUR DE L'OBSTACLE
C
C ----------------------------------------------------------------------

      INCLUDE 'jeveux.h'
      INTEGER       IBID, NS, DIMTUB, DIMOBS, IFM, I,
     +              IDRAY, IDTOB, IDROB, IDTHE, IRETT,
     +              JTUBUS, JOBSUS, N1, NIS, NC, NCO, NCR,
     +              NPU, NR, NIV, NPO, IREUSE, IRET2,
     +              NBPARA, LPRO, LXLGUT
      PARAMETER   ( NBPARA = 10 )
      REAL*8        ARETE, ARETE2, SECT(20), VOLTUB(20), TABR(NBPARA),
     +              VOLOBS(20), RTUBE, ROBST, JEUI, VUST(20), VUSO(20),
     +              RAD, R8DGRD, PI, R8PI, PERCE, R8B, SUSETU, RINT,
     +              ETUBE, DENC, DINST, HOBST, SINIT, SUSEOB, SVOOBS,
     +              SVOTUB
      COMPLEX*16    C16B
      CHARACTER*8   K8B, GUIDAG, GUIDE, OBCRAY,
     +              TYPARA(NBPARA), K8TYP
      CHARACTER*19  TABPUS
      CHARACTER*16  CONCEP, NOMCMD, NOMOBS, NOMOB1,
     +              NOPARA(NBPARA), NOPAR1(NBPARA)
      CHARACTER*19  RESU
      CHARACTER*24  TYPE, TABK(NBPARA), NOMFON, NOMF, TYPINI, NOMFG,
     +              TYPOBC, NOMOBC
      INTEGER      IARG
C
      DATA NOPARA / 'LIEU'    , 'SECTEUR' , 'TYPE'    , 'ANGL_DEBUT',
     &              'ANGL_FIN', 'ANGL_MAX', 'PROF_MAX', 'SURF_INIT' ,
     &              'SURF_USE', 'FONCTION' /
      DATA TYPARA / 'K8'      , 'I'         , 'K8'        , 'R'       ,
     &              'R'       , 'R'         , 'R'         , 'R'       ,
     &              'R'       , 'K24'       /
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      CALL GETRES ( RESU, CONCEP, NOMCMD )
      NOMFON = RESU(1:8)//'_INITIAL'
C
      PI  = R8PI( )
      RAD = R8DGRD()
C
      CALL WKVECT ( '&&OP0182.TUBUSE', 'V V R', 3000, JTUBUS )
      CALL WKVECT ( '&&OP0182.OBSUSE', 'V V R', 3000, JOBSUS )
C
      CALL INFMAJ
      CALL INFNIV ( IFM, NIV )
C
C     ------------------------------------------------------------------
C                       CREATION DE LA TABLE
C     ------------------------------------------------------------------
C
      CALL JEEXIN(RESU//'.TBLP', IREUSE)
      IF (IREUSE.NE.0) THEN
C     SI REENTRANT ON CONSERVE LA DESCRIPTION ET LE TYPE DE L'OBSTACLE
         CALL TBLIVA(RESU,1,'LIEU',
     &               IBID,R8B,C16B,'DEFIOBST',K8B,R8B,'FONCTION',
     &               K8TYP,IBID,R8B,C16B,NOMF,IRETT)
         CALL TBLIVA(RESU,1,'LIEU',
     &               IBID,R8B,C16B,'DEFIOBST',K8B,R8B,'TYPE',
     &               K8TYP,IBID,R8B,C16B,TYPINI,IRET2)
         IF (IRETT.NE.0.OR.IRET2.NE.0)THEN
            CALL U2MESS('F','PREPOST4_96')
         ENDIF
         CALL COPISD('FONCTION', 'V', NOMF, '&&OP0182.REUSE.NOMF')
         CALL DETRSD('TABLE',RESU)
         CALL COPISD('FONCTION', 'G', '&&OP0182.REUSE.NOMF', NOMFON)
      ELSE
C     SI PAS REENTRANT : TYPE DISCRET, INITIALISATION DE NOMFON
         TYPINI = 'DISCRET'
         CALL ASSERT(LXLGUT(NOMFON).LE.24)
         CALL WKVECT(NOMFON(1:19)//'.PROL','G V K24',6,LPRO)
         ZK24(LPRO) = 'FONCTION'
         ZK24(LPRO+1) = 'LINLIN'
         ZK24(LPRO+2) = 'THETA'
         ZK24(LPRO+3) = 'R'
         ZK24(LPRO+4) = 'EE'
         ZK24(LPRO+5) = NOMFON
         NPO = 721
         CALL WKVECT(NOMFON(1:19)//'.VALE','G V R',NPO*2,IDTOB)
         IDROB = IDTOB + NPO
         JEUI = 5.D-4
         DO 20 I = 1,NPO
            ZR(IDROB+I-1) = JEUI
            ZR(IDTOB+I-1) = (I-1)*RAD*360.D0/(NPO-1)
 20      CONTINUE
      ENDIF
      CALL TBCRSD(RESU, 'G')
      CALL TBAJPA(RESU, NBPARA, NOPARA, TYPARA)
C
C --- INSERTION DE LA LIGNE DE DESCRIPTION DANS LA TABLE
      NOPAR1(1) = 'LIEU'
      NOPAR1(2) = 'TYPE'
      NOPAR1(3) = 'FONCTION'
      TABK(1) = 'DEFIOBST'
      TABK(2) = TYPINI
      TABK(3) = NOMFON
      CALL TBAJLI(RESU,3,NOPAR1,IBID,R8B,C16B,TABK,0)

C     utiliser uniquement par CALFIG
      CALL JELIRA (NOMFON(1:19)//'.VALE', 'LONMAX', NPO, K8B)
C
      NS = 12
      DINST = 0.D0
      DO 10 I = 1 , NS
         SECT(I)   = (I-1)*360.D0/NS
         VOLTUB(I) = 0.D0
         VOLOBS(I) = 0.D0
 10   CONTINUE
C
C     ------------------------------------------------------------------
C            LES VOLUMES D'USURE TUBE ET OBST PAR SECTEUR
C     ------------------------------------------------------------------
C
      CALL GETVID ( ' ', 'TABL_USURE', 1,IARG,1, TABPUS, NPU )
      IF ( NPU .NE. 0 ) THEN
         CALL MOREVU ( TABPUS, DINST, NS, SECT, VOLTUB, VOLOBS )
      ENDIF
C
      CALL GETVR8 ( ' ', 'V_USUR_TUBE', 1,IARG,0, VUST, NIS )
      IF ( NIS .NE. 0 ) THEN
         NS = -NIS
         IF ( NS.NE.10 .AND. NS.NE.12 ) THEN
            CALL U2MESS('F','PREPOST3_63')
         ENDIF
         CALL GETVR8 ( ' ', 'V_USUR_TUBE', 1,IARG,NS, VUST, NIS )
         CALL GETVR8 ( ' ', 'V_USUR_OBST', 1,IARG,NS, VUSO, NIS )
         DO 12 I = 1 , NS
            SECT(I)   = (I-1)*360.D0/NS
            VOLTUB(I) = VUST(I)
            VOLOBS(I) = VUSO(I)
 12      CONTINUE
      ENDIF
C
      SECT(NS+1) = 360.D0
C
C     ------------------------------------------------------------------
C            REMPLACEMENT DU TUBE PERCE PAR UN TUBE NEUF
C     ------------------------------------------------------------------
C
      CALL GETVR8 ( ' ', 'PERCEMENT', 1,IARG,1, PERCE, NIS )
C
C     ------------------------------------------------------------------
C          PARAMETRES POUR L'USURE DES OBSTACLES EN FONCTION DE LA
C                    HAUTEUR DE LA CARTE OU DU GUIDAGE
C     ------------------------------------------------------------------
C
      CALL GETVID ( ' ', 'GUIDE'   , 1,IARG,1, GUIDE, N1  )
C
      CALL TBLIVA(GUIDE,1,'LIEU',
     &            IBID,R8B,C16B,'DEFIOBST',K8B,R8B,'TYPE',
     &            K8TYP,IBID,R8B,C16B,TYPE,IRETT)
      CALL TBLIVA(GUIDE,1,'LIEU',
     &            IBID,R8B,C16B,'DEFIOBST',K8B,R8B,'FONCTION',
     &            K8TYP,IBID,R8B,C16B,NOMFG,IRET2)
      IF (IRETT.NE.0.OR.IRET2.NE.0)THEN
         CALL U2MESS('F', 'PREPOST4_96')
      ENDIF
      CALL JELIRA(NOMFG(1:19)//'.VALE', 'LONMAX', NCO, K8B)
      NCO = NCO/2
      NOMOB1 = TYPE(1:7)
C
C --- CAS DES DISCRETS, EN SUPPOSANT QUE LE RAYON EST CONSTANT
C     SI PAS CONSTANT LE CALCUL DE L'USURE EST FAUX
C
      IF ( NOMOB1 .EQ. 'DISCRET') THEN
         CALL JEVEUO ( NOMFG(1:19)//'.VALE', 'L', IDTHE )
         IDRAY = IDTHE + NCO
         HOBST = 0.D0
         ROBST = ZR(IDRAY)
         NOMOBS = NOMOB1
         NOMOBS = NOMOB1
      ELSE
         NOMOBS = TYPE(8:12)
      ENDIF
C
      DENC = 3.05D-3
C
      IF (NOMOBS.EQ.'CARSP') THEN
C         -----------------
         ROBST = 5.59D-3
         HOBST  = 18.D-3
         IF (TYPE(1:6).EQ.'GUID_E') HOBST = 11.D-3
C
      ELSEIF (NOMOBS.EQ.'CARTE') THEN
C             -----------------
         IF (TYPE(14:17).EQ.'1300') ROBST = 5.34D-3
         IF (TYPE(14:16).EQ.'900')  ROBST = 5.325D-3
         HOBST = 18.D-3
         IF (TYPE(1:6).EQ.'GUID_E') HOBST = 11.D-3
C
      ELSEIF (NOMOBS.EQ.'GCONT') THEN
C             -----------------
         IF (TYPE(6:6).EQ.'B'.OR.TYPE(6:6).EQ.'D') DENC = 3.175D-3
         IF (TYPE(14:17).EQ.'1300') THEN
           ROBST = 5.44D-3
           HOBST  = 16.7D-3
         ELSEIF (TYPE(14:16).EQ.'900') THEN
           ROBST = 5.425D-3
           HOBST  = 10.D-3
         ENDIF
      ENDIF
C
      IF ( TYPE(1:6).EQ.'GUID_A' .OR. TYPE(1:6).EQ.'GUID_B' .OR.
     &     TYPE(1:6).EQ.'GUID_C' .OR. TYPE(1:6).EQ.'GUID_D' ) THEN
         GUIDAG = 'ENCO_1'
      ELSEIF (TYPE(1:6).EQ.'GUID_E' .OR. TYPE(1:6).EQ.'GUID_F') THEN
         GUIDAG = 'ENCO_2'
      ENDIF
C
      IF (NOMOBS.EQ.'GCOMB') THEN
         ROBST = 5.49D-3
         HOBST  = 12.D-3
         GUIDAG = 'CERCLE'
      ENDIF
C
      ARETE  = ASIN(DENC/ROBST)*180.D0/PI
      ARETE2 = 180.D0-ARETE
C
      CALL GETVR8 ( ' ', 'R_MOBILE', 1,IARG,1, RTUBE , NR )
      CALL GETVID ( ' ', 'CRAYON'  , 1,IARG,1, OBCRAY, NC )
      IF (NR.EQ.0) THEN
         IF (NC.EQ.0) THEN
           IF (TYPE(14:17).EQ.'1300') RTUBE = 4.84D-3
           IF (TYPE(14:16).EQ.'900')  RTUBE = 4.825D-3
         ELSE
           CALL TBLIVA(OBCRAY,1,'LIEU',
     &                 IBID,R8B,C16B,'DEFIOBST',K8B,R8B,'TYPE',
     &                 K8TYP,IBID,R8B,C16B,TYPOBC,IRETT)
           CALL TBLIVA(OBCRAY,1,'LIEU',
     &                 IBID,R8B,C16B,'DEFIOBST',K8B,R8B,'FONCTION',
     &                 K8TYP,IBID,R8B,C16B,NOMOBC,IRET2)
           CALL ASSERT(IRETT.EQ.0.AND.IRET2.EQ.0)
           IF (TYPOBC(14:17).EQ.'1300') THEN
              RTUBE = 4.84D-3
              GOTO 61
           ENDIF
           IF (TYPOBC(14:16).EQ.'900') THEN 
              RTUBE = 4.825D-3
              GOTO 61
           ENDIF
           CALL JELIRA(NOMOBC(1:19)//'.VALE', 'LONMAX', NCR, K8B)
           NCR = NCR/2
           GUIDAG = ' '
 61        CONTINUE
         ENDIF
      ENDIF
C
C --- CALCUL DE LA SURFACE INITIALE :
C     -----------------------------
C
      IF ( TYPE(14:17).EQ.'1300')  ETUBE = 0.98D-3
      IF ( TYPE(14:16).EQ.'900' )  ETUBE = 0.47D-3
      RINT  = RTUBE-ETUBE
      SINIT = PI*((RTUBE*RTUBE)-(RINT*RINT))
C
      IF ( NIV .GE. 2 ) THEN
         WRITE(IFM,1020) RTUBE
         WRITE(IFM,1021) ETUBE
         WRITE(IFM,1022) ROBST
         WRITE(IFM,1023) HOBST
         WRITE(IFM,*) '  TYPE OBSTACLE: ', GUIDAG
         WRITE(IFM,1000)
         DO 140 I = 1 , NS
            WRITE(IFM,1010) I, SECT(I), SECT(I+1), VOLTUB(I), VOLOBS(I)
 140     CONTINUE
      ENDIF
 1000 FORMAT('==> IMPRESSION DES VOLUMES USES PAR SECTEUR:',/,'  SECT',
     &   '   ANGL_DEBUT      ANGL_FIN      USURE_TUBE      USURE_OBST')
 1010 FORMAT(1P,2X,I2,3X,E12.5,3X,E12.5,3X,E12.5,3X,E12.5)
 1020 FORMAT(1P,'    TUBE RAYON EXT: ', E12.5 )
 1021 FORMAT(1P,'         EPAISSEUR: ', E12.5 )
 1022 FORMAT(1P,'    OBST RAYON INT: ', E12.5 )
 1023 FORMAT(1P,'           HAUTEUR: ', E12.5 )
C
C*********************************************************************
C
C ---  ENCO_1 , ENCO_2 , CERCLE
C
C*********************************************************************
C
      IF ( GUIDAG .EQ. 'ENCO_1'  .OR.
     &     GUIDAG .EQ. 'ENCO_2'  .OR.
     &     GUIDAG .EQ. 'CERCLE'  ) THEN
C
         CALL MOUSTO ( GUIDAG, DIMTUB, VOLTUB, ZR(JTUBUS),
     &                         DIMOBS, VOLOBS, ZR(JOBSUS), RTUBE,
     &                 ROBST, SECT, ARETE, ARETE2, NS, GUIDE,
     &                 HOBST, ETUBE, RESU, DENC, PERCE )
C
C*********************************************************************
C
C --- AUTRE
C
C*********************************************************************
C
      ELSE
         DIMTUB = NCR
         CALL JEVEUO(NOMOBC(1:19)//'.VALE', 'L', IDTHE)
         IDRAY = IDTHE + NCR
         DO 101 I = 1 , DIMTUB
            ZR(JTUBUS+2*I-2) = ZR(IDTHE+I-1)/RAD
            ZR(JTUBUS+2*I-1) = ZR(IDRAY+I-1)
 101     CONTINUE
C
         DIMOBS = NCO
         CALL JEVEUO(NOMFG(1:19)//'.VALE', 'L', IDTHE)
         IDRAY = IDTHE + NCO
         DO 102 I = 1 , DIMOBS
            ZR(JOBSUS+2*I-2) = ZR(IDTHE+I-1)/RAD
            ZR(JOBSUS+2*I-1) = ZR(IDRAY+I-1)
 102     CONTINUE
      ENDIF
C*********************************************************************
C
      CALL CALFIG(GUIDAG, RESU, DIMOBS, DIMTUB, ZR(JOBSUS), ZR(JTUBUS))
C
C
C     CALCUL DES SECTIONS USEES SUR TUBE ET OBSTACLE :
C     ----------------------------------------------
C
      SVOTUB = VOLTUB(1)
      SVOOBS = VOLOBS(1)
      DO 103 I = 2 , NS
        SVOTUB = SVOTUB + VOLTUB(I)
        SVOOBS = SVOOBS + VOLOBS(I)
 103  CONTINUE
      SUSETU = SVOTUB / HOBST
      SUSEOB = SVOOBS / HOBST
C
      IF ( NIV .GE. 1 ) THEN
         WRITE(IFM,*)'SURFACE INITIALE TUBE =', SINIT
         WRITE(IFM,*)'SURFACE USEE     TUBE =', SUSETU
         WRITE(IFM,*)'SURFACE USEE     OBST =', SUSEOB
      ENDIF
C
      NOPAR1(1) = NOPARA(1)
      NOPAR1(2) = NOPARA(8)
      NOPAR1(3) = NOPARA(9)
      TABR(1) = SINIT
      TABR(2) = SUSETU
      TABK(1) = 'TUBE'
      CALL TBAJLI ( RESU, 3, NOPAR1, IBID, TABR, C16B, TABK, 0 )
      NOPAR1(1) = NOPARA(1)
      NOPAR1(2) = NOPARA(9)
      TABR(1) = SUSEOB
      TABK(1) = 'OBST'
      CALL TBAJLI ( RESU, 2, NOPAR1, IBID, TABR, C16B, TABK, 0 )
C
C
      CALL JEDEMA()
      END
