      SUBROUTINE OP0073( IER )
      IMPLICIT  REAL*8 (A-H,O-Z)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/06/2004   AUTEUR DURAND C.DURAND 
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
C
C     DEFINITION D UN OBSTACLE DE CHOC DISCRETISE PAR FACETTES
C
C-----------------------------------------------------------------------
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
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
      CHARACTER*8   NOMRES
      CHARACTER*16  TYPRES, NOMCOM
      CHARACTER*24  TYPE
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFMAJ()
      CALL INFNIV ( IFM, NIV )
C
      CALL GETRES(NOMRES,TYPRES,NOMCOM)
C
C     --- VERIFICATIONS DE PREMIER NIVEAU ---
      CALL GETVR8(' ','VALE',0,1,1,VALE,N1)
      NBVAL = -N1
      IF ( (NBVAL/2)*2 .NE. NBVAL ) THEN
         CALL UTMESS('F',NOMCOM,'LE NOMBRE DE VALEURS DOIT ETRE PAIR.')
      ENDIF
C
      CALL WKVECT(NOMRES//'           .REFE','G V K24',1,IDREFE)
      CALL GETVTX(' ','TYPE',0,1,1,ZK24(IDREFE),N1)
      TYPE = ZK24(IDREFE)
C
C     --- DIMENSIONNEMENT DES OBJETS DE STOCKAGE ---
      RAD = R8DGRD()
      NBPAIR = NBVAL / 2
      IF (NBVAL.GT.0) THEN
         CALL WKVECT('&&OP0073.TEMP','V V R',NBVAL,IDTEMP)
         CALL WKVECT(NOMRES//'           .VALR','G V R',NBPAIR,IDRAYO)
         CALL WKVECT(NOMRES//'           .VALT','G V R',NBPAIR,IDTHET)
C
         CALL GETVR8(' ','VALE',0,1,NBVAL,ZR(IDTEMP),N1)
         DO 10 I = 1,NBPAIR
            ZR(IDRAYO+I-1) = ZR(IDTEMP+2*I-1)
            ZR(IDTHET+I-1) = ZR(IDTEMP+2*(I-1)) * RAD
 10     CONTINUE
        CALL JEIMPO(IFM,NOMRES//'           .VALR',' ',' RAYONS ')
        CALL JEIMPO(IFM,NOMRES//'           .VALT',' ',' ANGLES ')
        CALL JEDETR('&&OP0073.TEMP')
      ENDIF
      IF (TYPE(1:5).EQ.'GUID_') THEN
        NBPAIR = 801
        CALL WKVECT(NOMRES//'           .VALR','G V R',NBPAIR,IDRAYO)
        CALL WKVECT(NOMRES//'           .VALT','G V R',NBPAIR,IDTHET)
        IF (TYPE(8:12).EQ.'CARTE') THEN
          IF (TYPE(14:17).EQ.'1300') THEN
            RCARTE = 5.34D-3
            DENC = 3.05D-3
          ELSE IF (TYPE(14:16).EQ.'900') THEN
            RCARTE = 5.325D-3
            DENC = 3.05D-3
          ENDIF
          IF (TYPE(6:6).EQ.'A'.OR.TYPE(6:6).EQ.'B'.OR.
     &        TYPE(6:6).EQ.'C'.OR.TYPE(6:6).EQ.'D') THEN
             CALL GUIDE1(RCARTE,DENC,ZR(IDTHET),ZR(IDRAYO))
          ELSEIF (TYPE(6:6).EQ.'E'.OR.TYPE(6:6).EQ.'F') THEN
             CALL GUIDE2(RCARTE,DENC,ZR(IDTHET),ZR(IDRAYO))
          ENDIF
        ELSEIF (TYPE(8:12).EQ.'CARSP') THEN
          IF (TYPE(14:17).EQ.'1300') THEN
            RCARTE = 5.59D-3
            DENC = 3.05D-3
          ELSE IF (TYPE(14:16).EQ.'900') THEN
            RCARTE = 5.59D-3
            DENC = 3.05D-3
          ENDIF
          IF (TYPE(6:6).EQ.'A'.OR.TYPE(6:6).EQ.'B'.OR.
     &        TYPE(6:6).EQ.'C'.OR.TYPE(6:6).EQ.'D') THEN
             CALL GUIDE1(RCARTE,DENC,ZR(IDTHET),ZR(IDRAYO))
          ELSEIF (TYPE(6:6).EQ.'E'.OR.TYPE(6:6).EQ.'F') THEN
             CALL GUIDE2(RCARTE,DENC,ZR(IDTHET),ZR(IDRAYO))
          ENDIF
        ELSEIF (TYPE(8:12).EQ.'GCONT') THEN
          IF(TYPE(14:17).EQ.'1300') THEN
            RCARTE=5.44D-3
          ELSE IF (TYPE(14:16).EQ.'900') THEN
            RCARTE=5.425D-3
          ENDIF
          IF(TYPE(6:6).EQ.'A'.OR.TYPE(6:6).EQ.'C'.OR.
     &       TYPE(6:6).EQ.'E'.OR.TYPE(6:6).EQ.'F') THEN
            DENC=3.05D-3
            IF(TYPE(6:6).EQ.'A'.OR.TYPE(6:6).EQ.'C') THEN
              CALL GUIDE1(RCARTE,DENC,ZR(IDTHET),ZR(IDRAYO))
            ELSE IF(TYPE(6:6).EQ.'E'.OR.TYPE(6:6).EQ.'F') THEN
              CALL GUIDE2(RCARTE,DENC,ZR(IDTHET),ZR(IDRAYO))
            ENDIF
          ELSE IF(TYPE(6:6).EQ.'B'.OR.TYPE(6:6).EQ.'D') THEN
            DENC=3.175D-3
            CALL GUIDE1(RCARTE,DENC,ZR(IDTHET),ZR(IDRAYO))
          ENDIF
        ELSEIF (TYPE(8:12).EQ.'GCOMB') THEN
          IF (TYPE(14:17).EQ.'1300') THEN
            RCARTE = 5.49D-3
          ELSE IF (TYPE(14:16).EQ.'900') THEN
            RCARTE = 5.665D-3
          ENDIF
          DO 20 I = 1,NBPAIR
            ZR(IDRAYO+I-1) = RCARTE
            ZR(IDTHET+I-1) = (I-1) * RAD * 4.5D-1
 20       CONTINUE   
        ENDIF
      ELSEIF (TYPE(1:7).EQ.'CRAYON_') THEN
        NBPAIR = 801
        CALL WKVECT(NOMRES//'           .VALR','G V R',NBPAIR,IDRAYO)
        CALL WKVECT(NOMRES//'           .VALT','G V R',NBPAIR,IDTHET)
        IF (TYPE(8:11).EQ.'1300') THEN
           RCARTE = 4.84D-3
        ELSEIF (TYPE(8:10).EQ.'900') THEN
           RCARTE = 4.825D-3
        ENDIF
        DO 21 I = 1,NBPAIR
            ZR(IDRAYO+I-1) = RCARTE
            ZR(IDTHET+I-1) = (I-1) * RAD * 4.5D-1
 21     CONTINUE     
      ENDIF      
C
      CALL JEDEMA()
      END
