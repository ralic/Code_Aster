      SUBROUTINE OP0182 ( IERR )
      IMPLICIT  REAL*8  ( A-H,O-Z )
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 15/06/2005   AUTEUR VABHHTS J.PELLET 
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
C     ---- DEBUT DES COMMUNS JEVEUX ------------------------------------
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
C     ---- FIN DES COMMUNS JEVEUX --------------------------------------

      INTEGER       IBID, NS, DIMTUB, DIMOBS, NBPARA
      PARAMETER   ( NBPARA = 10 )
      REAL*8        ARETE, ARETE2, SECT(20), VOLTUB(20), TABR(NBPARA)
      REAL*8        VOLOBS(20), RTUBE, ROBST, JEUI, VUST(20), VUSO(20)
      REAL*8        RAD, R8DGRD, PI, R8PI, PERCE
      COMPLEX*16    C16B
      CHARACTER*8   K8B, GUIDAG, OBST, GUIDE, OBCRAY, TYPARA(NBPARA)
      CHARACTER*19  TABPUS
      CHARACTER*16  CONCEP, NOMCMD, NOMOBS, NOMOB1,
     +              NOPARA(NBPARA), NOPAR1(NBPARA)
      CHARACTER*19  RESU, NOMT19
      CHARACTER*24  TYPE, TABK(NBPARA)
C
      DATA NOPARA / 'LIEU'    , 'SECTEUR' , 'TYPE'    , 'ANGL_DEBUT', 
     +              'ANGL_FIN', 'ANGL_MAX', 'PROF_MAX', 'SURF_INIT' , 
     +              'SURF_USE', 'FONCTION' /
      DATA TYPARA / 'K8'      , 'I'         , 'K8'        , 'R'       ,
     +              'R'       , 'R'         , 'R'         , 'R'       ,
     +              'R'       , 'K24'       /
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      CALL GETRES ( RESU, CONCEP, NOMCMD )
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

      CALL JEEXIN ( RESU//'.LTNT', IRET )
      IF (IRET.EQ.0) CALL LTCRSD ( RESU, 'G' )
      NOMT19 = ' '
      CALL LTNOTB ( RESU, 'OBSTACLE', NOMT19 )
      CALL JEEXIN ( NOMT19//'.TBBA', IRET )
      IF (IRET.NE.0)  CALL DETRSD ( 'TABLE', NOMT19 )
      CALL TBCRSD ( NOMT19, 'G' )
      CALL TBAJPA ( NOMT19, NBPARA, NOPARA, TYPARA )
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
      CALL GETVID ( ' ', 'TABL_USURE', 1,1,1, TABPUS, NPU )
      IF ( NPU .NE. 0 ) THEN
         CALL MOREVU ( TABPUS, DINST, NS, SECT, VOLTUB, VOLOBS )
      ENDIF
C
      CALL GETVR8 ( ' ', 'V_USUR_TUBE', 1,1,0, VUST, NIS )
      IF ( NIS .NE. 0 ) THEN
         NS = -NIS
         IF ( NS.NE.10 .AND. NS.NE.12 ) THEN
            CALL UTMESS('F','OP0182','ON ATTEND 10 OU 12 SECTEURS')
         ENDIF
         CALL GETVR8 ( ' ', 'V_USUR_TUBE', 1,1,NS, VUST, NIS )
         CALL GETVR8 ( ' ', 'V_USUR_OBST', 1,1,NS, VUSO, NIS )
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
      CALL GETVR8 ( ' ', 'PERCEMENT', 1,1,1, PERCE, NIS )
C
C     ------------------------------------------------------------------
C          PARAMETRES POUR L'USURE DES OBSTACLES EN FONCTION DE LA 
C                    HAUTEUR DE LA CARTE OU DU GUIDAGE
C     ------------------------------------------------------------------
C
      CALL GETVID ( ' ', 'GUIDE'   , 1,1,1, GUIDE, N1  )
      CALL GETVID ( ' ', 'OBSTACLE', 1,1,1, OBST , NOB )
C
      IF ( NOB .EQ. 0 ) THEN
         OBST = '&&OBSTEM'
         CALL WKVECT(OBST//'           .REFO','V V K24',1,IDREFO)
         ZK24(IDREFO) = 'DISCRET'
         NPO = 721
         CALL WKVECT(OBST//'           .VALR','V V R8',NPO,IDROB)
         CALL WKVECT(OBST//'           .VALT','V V R8',NPO,IDTOB)
         JEUI = 5.D-4
         DO 20 I = 1,NPO
            ZR(IDROB+I-1) = JEUI
            ZR(IDTOB+I-1) = (I-1)*RAD*360.D0/(NPO-1)
 20      CONTINUE   
      ENDIF
      CALL JELIRA ( OBST//'           .VALR', 'LONMAX', NO, K8B )
C
      CALL JEVEUO ( GUIDE//'           .REFO', 'L', IDREFE )
      CALL JELIRA ( GUIDE//'           .VALR', 'LONMAX', NCO, K8B )
      TYPE = ZK24(IDREFE)
      NOMOB1 = TYPE(1:7)
C
C --- CAS DES DISCRETS, EN SUPPOSANT QUE LE RAYON EST CONSTANT
C     SI PAS CONSTANT LE CALCUL DE L'USURE EST FAUX
C
      IF ( NOMOB1 .EQ. 'DISCRET') THEN
         CALL JEVEUO ( GUIDE//'           .VALR', 'L', JVALR )
         HOBST = 0.D0
         ROBST = ZR(JVALR)
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
      CALL GETVR8 ( ' ', 'R_MOBILE', 1,1,1, RTUBE , NR )
      CALL GETVID ( ' ', 'CRAYON'  , 1,1,1, OBCRAY, NC )
      IF (NR.EQ.0) THEN
         IF (NC.EQ.0) THEN
           IF (TYPE(14:17).EQ.'1300') RTUBE = 4.84D-3 
           IF (TYPE(14:16).EQ.'900')  RTUBE = 4.825D-3 
         ELSE
           CALL JEVEUO(OBCRAY//'           .REFO','L',IDREF2)
           IF (ZK24(IDREF2)(14:17).EQ.'1300') THEN
              RTUBE = 4.84D-3
              GOTO 61
           ENDIF
           IF (ZK24(IDREF2)(14:16).EQ.'900') THEN 
              RTUBE = 4.825D-3
              GOTO 61
           ENDIF
           CALL JELIRA(OBCRAY//'           .VALR','LONMAX',NCR,K8B)
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
     +   '   ANGL_DEBUT      ANGL_FIN      USURE_TUBE      USURE_OBST')
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
     &                 HOBST, ETUBE, NOMT19, DENC, PERCE )
C
C*********************************************************************
C
C --- AUTRE
C
C*********************************************************************
C
      ELSE
         DIMTUB = NCR
         CALL JEVEUO ( OBCRAY//'           .VALR', 'L', IDRAY )
         CALL JEVEUO ( OBCRAY//'           .VALT', 'L', IDTHE )
         DO 101 I = 1 , DIMTUB
            ZR(JTUBUS+2*I-2) = ZR(IDTHE+I-1)/RAD
            ZR(JTUBUS+2*I-1) = ZR(IDRAY+I-1)
 101     CONTINUE
C
         DIMOBS = NCO
         CALL JEVEUO ( GUIDE//'           .VALR', 'L', IDRAY )
         CALL JEVEUO ( GUIDE//'           .VALT', 'L', IDTHE )
         DO 102 I = 1 , DIMOBS
            ZR(JOBSUS+2*I-2) = ZR(IDTHE+I-1)/RAD
            ZR(JOBSUS+2*I-1) = ZR(IDRAY+I-1)
 102     CONTINUE
      ENDIF
C*********************************************************************
C
      CALL CALFIG ( GUIDAG, RESU, OBST, NO, DIMOBS, DIMTUB,
     &                      ZR(JOBSUS), ZR(JTUBUS), NOMT19 )
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
      CALL TBAJLI ( NOMT19, 3, NOPAR1, IBID, TABR, C16B, TABK, 0 )
      NOPAR1(1) = NOPARA(1)
      NOPAR1(2) = NOPARA(9)
      TABR(1) = SUSEOB
      TABK(1) = 'OBST'
      CALL TBAJLI ( NOMT19, 2, NOPAR1, IBID, TABR, C16B, TABK, 0 )
C
C
      CALL JEDEMA()
      END
