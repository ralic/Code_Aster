      SUBROUTINE RVTECN ( RELEVE,ABSC,ITCOPT,ITSPPT,COOR,NOMCMP,NOMNOE,
     &                    NBCMP,NBPOIN,DOCU,NOMTAB, IOCC, XNOVAR,
     &                    NCHEFF, I1, IOC, ISD )
      IMPLICIT NONE
      INTEGER           ITCOPT(*),ITSPPT(*),NBCMP,NBPOIN,IOCC,I1,IOC,ISD
      REAL*8            RELEVE(*),ABSC(*),COOR(*)
      CHARACTER*4       DOCU
      CHARACTER*8       NOMCMP(*),NOMNOE(*)
      CHARACTER*16      NCHEFF
      CHARACTER*19      NOMTAB
      CHARACTER*24      XNOVAR
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 21/09/2011   AUTEUR COURTOIS M.COURTOIS 
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
C     ------------------------------------------------------------------
C     MISE EN TABLEAU POUR UN EXTRACTION SUR UN CHAM_NO
C     ------------------------------------------------------------------
C IN  : RELEVE : TABLE DES RELEVE DE VALEURS
C IN  : ABSC   : TABLE DES ABSCISES DES POINTS
C IN  : ITCOPT : TABLE DES NOMBRES DE COUCHES PAR POINT
C IN  : ITSPPT : TABLE DES NOMBRES DE SOUS-PT PAR POINT
C IN  : COOR   : TABLE DES COORDONNEES DES POINTS
C IN  : NOMCMP : NOM DES CMP
C IN  : NOMNOE : TABLE DES EVENTUELS NOMS DE NOEUDS
C IN  : NBCMP  : NOMBRE DE CMP
C IN  : NBPOIN : NOMBRE DE POINT D'EVALUATION
C IN  : DOCU   : 'LSTN'/'CHMM'/'SGTD'/'ARCC'
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
      CHARACTER*32     JEXNUM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
      INTEGER      NBVARI, NBPAR,  JVAL1, JVALK, ILIGN,  IPT, NBSP,
     &             NBCO, LC, LN, IS, IC, I2, VALEI(12), N1, ADRVAL,
     &             ADRACC, JACC, IK, IR, II, LCR, LCK, NC,
     &             NBACC, NBPR, JACES, IAC, IADR
      LOGICAL      EXIST
      CHARACTER*3  TYPPAR
      CHARACTER*8  K8B, ACCES, NOMRES, CTYPE, NOPASE, COURBE
      CHARACTER*16 INTITU
      CHARACTER*24 NOMVAL, NOMACC, NNORES, NOMJV
      COMPLEX*16   C16B
      CHARACTER*80 VALEK(11)
      INTEGER      IARG
C     ------------------------------------------------------------------
C
      CALL JEMARQ( )
      IF ( DOCU.NE.'LSTN' .AND. DOCU.NE.'CHMM' .AND.
     &     DOCU.NE.'SGTD' .AND. DOCU.NE.'ARCC' .AND.
     &                          DOCU.NE.'SGT3' ) GOTO 9999
C
      CALL JELIRA(JEXNUM(XNOVAR,IOCC),'LONUTI',NBVARI,K8B)
      IF ( NBVARI .NE. 0 ) THEN
         CALL RVTEC2 ( RELEVE,ABSC,ITCOPT,ITSPPT,COOR,NOMNOE,
     &                 NBCMP,NBPOIN,DOCU,NOMTAB,IOCC,XNOVAR,
     &                 NCHEFF, I1, IOC, ISD )
         GOTO 9999
      ENDIF
C
      CALL GETVTX ( 'ACTION', 'INTITULE', IOCC,IARG,1, INTITU, N1 )
      CALL GETVID ( 'ACTION', 'CHEMIN'  , IOCC,IARG,1, COURBE, NC )
C
      NOMVAL = NCHEFF//'.VALACCE'
      NOMACC = NCHEFF//'.TYPACCE'
      NNORES = NCHEFF//'.NOMRESU'
      CALL JEVEUO ( NOMACC, 'L', JACC )
C
      LCR  = NBCMP + 10
      CALL WKVECT ( '&&RVTECN.VALE_R', 'V V R', LCR, JVAL1 )
      LCK  = NBCMP + 23
      CALL WKVECT ( '&&RVTECN.PARA', 'V V K24', LCK, JVALK )
C
      II = 0
      IR = 0
      IK = 1
      NBPAR = 1
      VALEK(IK) = INTITU
      ZK24(JVALK-1+NBPAR) = 'INTITULE'
C
      IF ( NC .NE. 0 ) THEN
         NBPAR = NBPAR + 1
         ZK24(JVALK-1+NBPAR) = 'CHEMIN'
         IK = IK + 1
         VALEK(IK) = COURBE
         NBPAR = NBPAR + 1
         ZK24(JVALK-1+NBPAR) = 'SEGMENT'
         II = II + 1
         VALEI(II) = ISD
         NBPAR = NBPAR + 1
         ZK24(JVALK-1+NBPAR) = 'CMP_CNX'
         II = II + 1
         VALEI(II) = IOC
      ENDIF
C
      IF ( ZK8(JACC) .EQ. 'DIRECT  ' ) THEN
         CALL JEVEUO ( JEXNUM(NCHEFF//'.LSCHEFF',1), 'L', JACC )
         NBPAR = NBPAR + 1
         ZK24(JVALK-1+NBPAR) = 'CHAM_GD'
         IK = IK + 1
         VALEK(IK) = ZK24(JACC)(1:8)
      ELSE
         CALL JEVEUO ( NNORES, 'L', JACC )
         NOMRES = ZK16(JACC)(1:8)
         NOPASE = ZK16(JACC+3)(1:8)
         IF ( NOPASE.EQ.'        ' ) THEN
           K8B = NOMRES
         ELSE
           K8B = ZK16(JACC+2)(1:8)
         ENDIF
         NBPAR = NBPAR + 1
         ZK24(JVALK-1+NBPAR) = 'RESU'
         IK = IK + 1
         VALEK(IK) = K8B
         NBPAR = NBPAR + 1
         ZK24(JVALK-1+NBPAR) = 'NOM_CHAM'
         IK = IK + 1
         VALEK(IK) = ZK16(JACC+1)
         CALL JEVEUO ( JEXNUM(NOMACC,IOCC), 'L', ADRACC )
         CALL JEVEUO ( JEXNUM(NOMVAL,IOCC), 'L', ADRVAL )
         ACCES = ZK8(ADRACC)
         IF ( ACCES(1:1) .EQ. 'O' ) THEN
            NBPAR = NBPAR + 1
            ZK24(JVALK-1+NBPAR) = 'NUME_ORDRE'
            II = II + 1
            VALEI(II) = ZI(ADRVAL + I1-1)
            NOMJV = '&&RVTECN.NOMS_ACCES'
            CALL RSNOPA ( NOMRES, 0, NOMJV, NBACC, NBPR )
            IF ( NBACC .NE. 0 ) THEN
               CALL JEVEUO ( NOMJV, 'L', JACES )
               DO 10 IAC = 1 , NBACC
                  CALL RSADPA ( NOMRES, 'L', 1, ZK16(JACES-1+IAC),
     &                          ZI(ADRVAL+I1-1), 1, IADR, CTYPE )
                  CALL TBEXIP ( NOMTAB, ZK16(JACES-1+IAC), EXIST,TYPPAR)
                  IF ( .NOT. EXIST ) THEN
                     CALL TBAJPA ( NOMTAB, 1, ZK16(JACES-1+IAC), CTYPE )
                  ENDIF
                  NBPAR = NBPAR + 1
                  ZK24(JVALK-1+NBPAR) = ZK16(JACES-1+IAC)
                  IF ( CTYPE(1:1) .EQ. 'I' ) THEN
                     II = II + 1
                     VALEI(II) = ZI(IADR)
                  ELSEIF ( CTYPE(1:1) .EQ. 'R'   ) THEN
                     IR = IR + 1
                     ZR(JVAL1+IR-1) = ZR(IADR)
                  ELSEIF ( CTYPE(1:3) .EQ. 'K80' ) THEN
                     IK = IK + 1
                     VALEK(IK) = ZK80(IADR)
                  ELSEIF ( CTYPE(1:3) .EQ. 'K32' ) THEN
                     IK = IK + 1
                     VALEK(IK) = ZK32(IADR)
                  ELSEIF ( CTYPE(1:3) .EQ. 'K24' ) THEN
                     IK = IK + 1
                     VALEK(IK) = ZK24(IADR)
                  ELSEIF ( CTYPE(1:3) .EQ. 'K16' ) THEN
                     IK = IK + 1
                     VALEK(IK) = ZK16(IADR)
                  ELSEIF ( CTYPE(1:2) .EQ. 'K8'  ) THEN
                     IK = IK + 1
                     VALEK(IK) = ZK8(IADR)
                  ENDIF
 10            CONTINUE
               CALL JEDETR ( NOMJV )
            ENDIF
         ELSEIF ( ACCES(1:1) .EQ. 'M' ) THEN
            NBPAR = NBPAR + 1
            ZK24(JVALK-1+NBPAR) = 'NUME_MODE'
            II = II + 1
            VALEI(II) = ZI(ADRVAL + I1-1)
         ELSEIF ( ACCES(1:1) .EQ. 'I' ) THEN
            NBPAR = NBPAR + 1
            ZK24(JVALK-1+NBPAR) = 'INST'
            IR = IR + 1
            ZR(JVAL1+IR-1) = ZR(ADRVAL + I1-1)
         ELSEIF ( ACCES(1:1) .EQ. 'F' ) THEN
            NBPAR = NBPAR + 1
            ZK24(JVALK-1+NBPAR) = 'FREQ'
            IR = IR + 1
            ZR(JVAL1+IR-1) = ZR(ADRVAL + I1-1)
         ENDIF
      ENDIF
      IF ( DOCU.EQ.'LSTN' .OR. DOCU.EQ.'CHMM' ) THEN
         CALL TBEXIP ( NOMTAB, 'NOEUD', EXIST,TYPPAR)
         IF ( .NOT. EXIST ) THEN
            CALL TBAJPA ( NOMTAB, 1, 'NOEUD', 'K8' )
         ENDIF
         NBPAR = NBPAR + 1
         ZK24(JVALK-1+NBPAR) = 'NOEUD'
      ENDIF
      NBPAR = NBPAR + 1
      ZK24(JVALK-1+NBPAR) = 'ABSC_CURV'
      NBPAR = NBPAR + 1
      ZK24(JVALK-1+NBPAR) = 'COOR_X'
      NBPAR = NBPAR + 1
      ZK24(JVALK-1+NBPAR) = 'COOR_Y'
      NBPAR = NBPAR + 1
      ZK24(JVALK-1+NBPAR) = 'COOR_Z'
      IC = 0
      IS = 0
      DO 20, IPT = 1, NBPOIN, 1
         NBSP = ITSPPT(IPT)
         NBCO = ITCOPT(IPT)
         IF ( NBCO .GT. 1  .AND.  IC .EQ. 0 ) THEN
            CALL TBEXIP ( NOMTAB, 'NUME_COUCHE', EXIST,TYPPAR)
            IF ( .NOT. EXIST ) THEN
               CALL TBAJPA ( NOMTAB, 1, 'NUME_COUCHE', 'I' )
            ENDIF
            IC = 1
            NBPAR = NBPAR + 1
            ZK24(JVALK-1+NBPAR) = 'NUME_COUCHE'
         ENDIF
         IF ( NBSP .GT. 1  .AND.  IS .EQ. 0  ) THEN
            CALL TBEXIP ( NOMTAB, 'NUME_GAUSS', EXIST,TYPPAR)
            IF ( .NOT. EXIST ) THEN
               CALL TBAJPA ( NOMTAB, 1, 'NUME_GAUSS', 'I' )
            ENDIF
            IS = 1
            NBPAR = NBPAR + 1
            ZK24(JVALK-1+NBPAR) = 'NUME_GAUSS'
         ENDIF
 20   CONTINUE
      DO 30, I2 = 1, NBCMP, 1
         NBPAR = NBPAR + 1
         ZK24(JVALK-1+NBPAR) = NOMCMP(I2)
 30   CONTINUE
C
      LC = IR+4+NBCMP
      CALL ASSERT( NBPAR .LE. LCK )
      CALL ASSERT( II+2 .LE. 10 )
      CALL ASSERT( IK+1 .LE. 10 )
      CALL ASSERT( LC .LE. LCR )
C
      ILIGN = 0
C
      IK = IK + 1
      DO 100, IPT = 1, NBPOIN, 1
C
         NBSP = ITSPPT(IPT)
         NBCO = ITCOPT(IPT)
         LC   = NBCMP * NBSP
         LN   = LC * NBCO
C
         IF ( DOCU.EQ.'LSTN' .OR. DOCU.EQ.'CHMM' ) THEN
            VALEK(IK) = NOMNOE(IPT)
         ENDIF
C
         ZR(JVAL1+IR  ) = ABSC(IPT)
         ZR(JVAL1+IR+1) = COOR(1+(IPT-1)*3)
         ZR(JVAL1+IR+2) = COOR(2+(IPT-1)*3)
         ZR(JVAL1+IR+3) = COOR(3+(IPT-1)*3)
C
         DO 102, IC = 1, NBCO, 1
C
            VALEI(II+1) = IC
C
            DO 104, IS = 1, NBSP, 1
C
               VALEI(II+2) = IS
C
               DO 106, I2 = 1, NBCMP, 1
C
                  ZR(JVAL1+IR+3+I2) =
     &                      RELEVE((IPT-1)*LN+LC*(IC-1)+NBCMP*(IS-1)+I2)
C
 106           CONTINUE
C
               CALL TBAJLI ( NOMTAB, NBPAR, ZK24(JVALK),
     &                       VALEI, ZR(JVAL1), C16B, VALEK, ILIGN )
C
 104        CONTINUE
C
 102     CONTINUE
C
 100  CONTINUE
C
      CALL JEDETR ( '&&RVTECN.VALE_R' )
      CALL JEDETR ( '&&RVTECN.PARA' )
C
 9999 CONTINUE
      CALL JEDEMA( )
      END
