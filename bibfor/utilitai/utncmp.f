      SUBROUTINE UTNCMP ( CHAM19 , NCMP , NOMOBJ )
      IMPLICIT   NONE
      INTEGER             NCMP
      CHARACTER*(*)       CHAM19 , NOMOBJ
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 15/02/2005   AUTEUR CIBHHPD L.SALMONA 
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
C     ------------------------------------------------------------------
C
C     RECUPERE LE NOMBRE ET LES NOMS DES COMPOSANTES D'UN CHAMP
C
C     ------------------------------------------------------------------
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
      CHARACTER*32     JEXNUM, JEXNOM,JEXATR
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER       IBID, IE, JPRNO, GD, NBEC, NEC, TABEC(10), J,
     +              INO, IEC, ICMP, NCMPMX, JCMP, IAD, KCMP, IGR, MODE,
     +              NNOE, JCELD, NBGREL, IREPE, IMAIL, NBEL, JMOD, 
     +              IMODEL,ILONG,IDESCR
      CHARACTER*4   TYCH
      CHARACTER*8   K8B, NOMA
      CHARACTER*19  CH19, PRNO, NOLIGR
      LOGICAL       EXISDG
C     ------------------------------------------------------------------
      CALL JEMARQ( )
C
      CH19 = CHAM19
      NCMP = 0
C
      CALL DISMOI ( 'F', 'TYPE_CHAMP', CH19, 'CHAMP', IBID, TYCH, IE )
      CALL DISMOI ( 'F', 'NOM_MAILLA', CH19, 'CHAMP', IBID, NOMA, IE )
      CALL DISMOI ( 'F', 'NUM_GD'    , CH19, 'CHAMP', GD  , K8B , IE )
C
      CALL JEVEUO('&CATA.GD.DESCRIGD','L',IDESCR)
      NEC  = NBEC( GD)
      IF ( NEC .GT. 10 ) CALL UTMESS('F','UTNCMP','NEC TROP GRAND')
      CALL JELIRA ( JEXNUM('&CATA.GD.NOMCMP',GD), 'LONMAX', NCMPMX, K8B)
      CALL JEVEUO ( JEXNUM('&CATA.GD.NOMCMP',GD), 'L', IAD )
      CALL WKVECT ( '&&UTNCMP.ICMP', 'V V I', NCMPMX, JCMP )
C
C     ==================================================================
C                            C H A M _ N O
C     ==================================================================
      IF ( TYCH(1:4) .EQ. 'NOEU' ) THEN
         CALL DISMOI('F','NB_NO_MAILLA', NOMA, 'MAILLAGE',NNOE, K8B ,IE)
         CALL DISMOI('F','PROF_CHNO'   , CH19, 'CHAM_NO' ,IBID, PRNO,IE)
         CALL JEVEUO ( JEXNUM(PRNO//'.PRNO',1), 'L', JPRNO )
         DO 10 INO = 1 , NNOE
            DO 12 IEC = 1 , NEC
               TABEC(IEC)= ZI(JPRNO-1+(INO-1)*(NEC+2)+2+IEC )
 12         CONTINUE
            DO 14 ICMP = 1 , NCMPMX
               IF ( EXISDG(TABEC,ICMP) ) THEN
                  DO 16 J = 1 , NCMP
                     IF ( ZI(JCMP+J-1) .EQ. ICMP ) GOTO 14
 16               CONTINUE
                  NCMP = NCMP + 1
                  ZI(JCMP+NCMP-1) = ICMP
               ENDIF
 14         CONTINUE
 10      CONTINUE
C
C     ==================================================================
C                             C H A M _ E L E M
C     ==================================================================
      ELSEIF ( TYCH(1:2) .EQ. 'EL' ) THEN
         CALL DISMOI('F','NB_MA_MAILLA', NOMA, 'MAILLAGE',NBEL, K8B ,IE)
         CALL DISMOI ( 'F','NOM_LIGREL', CH19,'CHAMP', IBID, NOLIGR,IE)
         CALL DISMOI ( 'F', 'NB_GREL', NOLIGR, 'LIGREL', NBGREL,K8B,IE)
         CALL JEVEUO ( CH19//'.CELD', 'L', JCELD )
         CALL JEVEUO ( NOLIGR//'.REPE', 'L', IREPE )
         CALL JEVEUO ('&CATA.TE.MODELOC', 'L', IMODEL )
         CALL JEVEUO (JEXATR('&CATA.TE.MODELOC','LONCUM'),'L',ILONG)
         DO 20 IGR = 1 , NBGREL
            MODE=ZI(JCELD-1+ZI(JCELD-1+4+IGR) +2)
            IF ( MODE .EQ. 0 ) GOTO 20
            JMOD = IMODEL+ZI(ILONG-1+MODE)-1
            NEC = NBEC( ZI(JMOD-1+2))
            CALL DGMODE ( MODE, IMODEL, ILONG, NEC, TABEC )
            DO 22 ICMP = 1, NCMPMX
               IF ( EXISDG( TABEC , ICMP ) ) THEN
                  DO 24 J = 1 , NCMP
                     IF ( ZI(JCMP+J-1) .EQ. ICMP ) GOTO 22
  24              CONTINUE
                  NCMP = NCMP + 1
                  ZI(JCMP+NCMP-1) = ICMP
               ENDIF
  22       CONTINUE
  20     CONTINUE
C
      ELSE
         CALL UTMESS('F','UTNCMP','ON NE SAIT PAS TRAITER'//
     +               ' LE CHAMP DE TYPE: '//TYCH//' CHAMP : '//CH19)
      ENDIF
C
      IF ( NCMP.EQ.0) CALL UTMESS('F','UTNCMP','PAS DE COMPOSANTES')
C
      CALL WKVECT ( NOMOBJ, 'V V K8', NCMP, KCMP )
      DO 30 ICMP = 1 , NCMP
         ZK8(KCMP+ICMP-1) = ZK8(IAD-1+ZI(JCMP+ICMP-1))
 30   CONTINUE
      CALL JEDETR ( '&&UTNCMP.ICMP' )
C
      CALL JEDEMA( )
      END
