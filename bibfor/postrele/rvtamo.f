      SUBROUTINE RVTAMO ( T, NOMCMP, NBCP, NBCO, NBSP, NOMTAB,
     &                    IOCC, XNOVAR, NCHEFF, I1, IOC, ISD )
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM
      INTEGER             NBCP, NBCO, NBSP, IOCC, I1, IOC, ISD
      REAL*8              T(*)
      CHARACTER*16        NCHEFF
      CHARACTER*24        XNOVAR
      CHARACTER*(*)       NOMCMP(*), NOMTAB
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C     MISE EN TABLEAU POUR UNE MOYENNE
C     ------------------------------------------------------------------
C IN  : T      : TABLE DES VALEURS
C IN  : NOMCMP : NOM DES COMPOSANTES
C IN  : NBCP   : NOMBRE DE COMPOSANTES
C IN  : NBCO   : NOMBRE DE COUCHES
C IN  : NBSP   : NOMBRE DE SOUS-POINTS
C     ------------------------------------------------------------------
      INTEGER       NBPAR, ILIGN, I, L, M, ICP, ISP, JACC, IK, IR, II,
     &              VALEI(10), N1, ADRACC, ADRVAL, I10, I20, I30, ICO,
     &              NBACC, NBPR, JACES, IAC, IADR, NC, NBVARI, JVARI,
     &              NBCMP2
      REAL*8        VALER(12)
      LOGICAL       EXIST
      COMPLEX*16    C16B
      CHARACTER*3   TYPPAR
      CHARACTER*7  KII
      CHARACTER*8   K8B, ACCES, NOMRES, CTYPE, NOPASE, COURBE
      CHARACTER*16  INTITU, NOMPAR(6)
      CHARACTER*24  NOMVAL, NOMACC, NNORES, NOPARA(18), NOMJV
      CHARACTER*80  VALEK(11)
      INTEGER      IARG
C
      DATA NOMPAR / 'MOMENT_0'  ,  'MOMENT_1'  ,  'MINIMUM'   ,
     &              'MAXIMUM'   ,  'MOYE_INT'  ,  'MOYE_EXT'  /
C     -----------------------------------------------------------------
C
      CALL JEMARQ()
C
      CALL GETVTX ( 'ACTION', 'INTITULE', IOCC,IARG,1, INTITU, N1 )
      CALL GETVID ( 'ACTION', 'CHEMIN'  , IOCC,IARG,1, COURBE, NC )
C
      CALL JELIRA ( JEXNUM(XNOVAR,IOCC),'LONUTI',NBVARI,K8B)
      IF ( NBVARI .EQ. 0 ) THEN
         NBCMP2 = NBCP
      ELSE
         CALL JEVEUO ( JEXNUM(XNOVAR,IOCC), 'L', JVARI )
         IF ( NBVARI .EQ. 1  .AND. ZI(JVARI).EQ. -1 ) THEN
            NBCMP2 = NBCP
         ELSE
            NBCMP2 = NBVARI
         ENDIF
         IF ( NBCMP2.GT.3000) CALL U2MESS('F','POSTRELE_13')
      ENDIF
C
      NOMVAL = NCHEFF//'.VALACCE'
      NOMACC = NCHEFF//'.TYPACCE'
      NNORES = NCHEFF//'.NOMRESU'
      CALL JEVEUO ( NOMACC, 'L', JACC )
C
      IK = 1
      II = 0
      IR = 0
      NBPAR = 1
      NOPARA(NBPAR) = 'INTITULE'
      VALEK(IK) = INTITU
C
      IF ( NC .NE. 0 ) THEN
         NBPAR = NBPAR + 1
         NOPARA(NBPAR) = 'CHEMIN'
         IK = IK + 1
         VALEK(IK) = COURBE
         NBPAR = NBPAR + 1
         NOPARA(NBPAR) = 'SEGMENT'
         II = II + 1
         VALEI(II) = ISD
         NBPAR = NBPAR + 1
         NOPARA(NBPAR) = 'CMP_CNX'
         II = II + 1
         VALEI(II) = IOC
      ENDIF
C
      IF ( ZK8(JACC) .EQ. 'DIRECT  ' ) THEN
         CALL JEVEUO ( JEXNUM(NCHEFF//'.LSCHEFF',1), 'L', JACC )
         NBPAR = NBPAR + 1
         NOPARA(NBPAR) = 'CHAM_GD'
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
         NOPARA(NBPAR) = 'RESU'
         IK = IK + 1
         VALEK(IK) = K8B
         NBPAR = NBPAR + 1
         NOPARA(NBPAR) = 'NOM_CHAM'
         IK = IK + 1
         VALEK(IK) = ZK16(JACC+1)
         CALL JEVEUO ( NOMACC, 'L', ADRACC )
         CALL JEVEUO ( NOMVAL, 'L', ADRVAL )
         ACCES = ZK8(ADRACC)
         IF ( ACCES(1:1) .EQ. 'O' ) THEN
            NBPAR = NBPAR + 1
            NOPARA(NBPAR) = 'NUME_ORDRE'
            II = II + 1
            VALEI(II) = ZI(ADRVAL + I1-1)
            NOMJV = '&&RVTAMO.NOMS_ACCES'
            CALL RSNOPA ( NOMRES, 0, NOMJV, NBACC, NBPR )
            IF ( NBACC .NE. 0 ) THEN
               CALL JEVEUO ( NOMJV, 'L', JACES )
               DO 70 IAC = 1 , NBACC
                  CALL RSADPA ( NOMRES, 'L', 1, ZK16(JACES-1+IAC),
     &                          ZI(ADRVAL+I1-1), 1, IADR, CTYPE )
                  CALL TBEXIP ( NOMTAB, ZK16(JACES-1+IAC), EXIST,TYPPAR)
                  IF ( .NOT. EXIST ) THEN
                     CALL TBAJPA ( NOMTAB, 1, ZK16(JACES-1+IAC), CTYPE )
                  ENDIF
                  NBPAR = NBPAR + 1
                  NOPARA(NBPAR) = ZK16(JACES-1+IAC)
                  IF ( CTYPE(1:1) .EQ. 'I' ) THEN
                     II = II + 1
                     VALEI(II) = ZI(IADR)
                  ELSEIF ( CTYPE(1:1) .EQ. 'R'   ) THEN
                     IR = IR + 1
                     VALER(IR) = ZR(IADR)
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
 70            CONTINUE
               CALL JEDETR ( NOMJV )
            ENDIF
         ELSEIF ( ACCES(1:1) .EQ. 'M' ) THEN
            NBPAR = NBPAR + 1
            NOPARA(NBPAR) = 'NUME_MODE'
            II = II + 1
            VALEI(II) = ZI(ADRVAL + I1-1)
         ELSEIF ( ACCES(1:1) .EQ. 'I' ) THEN
            NBPAR = NBPAR + 1
            NOPARA(NBPAR) = 'INST'
            IR = IR + 1
            VALER(IR) = ZR(ADRVAL + I1-1)
         ELSEIF ( ACCES(1:1) .EQ. 'F' ) THEN
            NBPAR = NBPAR + 1
            NOPARA(NBPAR) = 'FREQ'
            IR = IR + 1
            VALER(IR) = ZR(ADRVAL + I1-1)
         ENDIF
      ENDIF
C
      IF ( NBCO .GT. 1 ) THEN
         CALL TBEXIP ( NOMTAB, 'NUME_COUCHE', EXIST,TYPPAR )
         IF ( .NOT. EXIST ) THEN
            CALL TBAJPA ( NOMTAB, 1, 'NUME_COUCHE', 'I' )
         ENDIF
         NBPAR = NBPAR + 1
         NOPARA(NBPAR) = 'NUME_COUCHE'
      ENDIF
      IF ( NBSP .GT. 1 ) THEN
         CALL TBEXIP ( NOMTAB, 'NUME_GAUSS', EXIST,TYPPAR )
         IF ( .NOT. EXIST ) THEN
            CALL TBAJPA ( NOMTAB, 1, 'NUME_GAUSS', 'I' )
         ENDIF
         NBPAR = NBPAR + 1
         NOPARA(NBPAR) = 'NUME_GAUSS'
      ENDIF
C
      NBPAR = NBPAR + 1
      NOPARA(NBPAR) = 'QUANTITE'
C
      IF ( NBVARI .EQ. 0 ) THEN
         DO 50 ICP = 1, NBCMP2
            NBPAR = NBPAR + 1
            NOPARA(NBPAR) = NOMCMP(ICP)
 50      CONTINUE
      ELSE
         DO 52 ICP = 1, NBCMP2
            CALL CODENT ( ZI(JVARI+ICP-1), 'G', KII )
            NBPAR = NBPAR + 1
            NOPARA(NBPAR) = 'V'//KII
 52      CONTINUE
      ENDIF
C
      CALL ASSERT( NBPAR   .LE. 15 )
      CALL ASSERT( II+2    .LE. 10 )
      CALL ASSERT( IR+NBCMP2 .LE. 10 )
      CALL ASSERT( IK+2    .LE. 10 )
C
      L = 6 * NBSP
      M = L * NBCO
      ILIGN = 0
      IK = IK + 1
C
      DO 20 ICO = 1, NBCO
         I20 = L * ( ICO - 1 )
         VALEI(II+1) = ICO
C
         DO 10 ISP = 1, NBSP
            I10 = 6 * ( ISP - 1 )
            VALEI(II+2) = ISP
C
            DO 40 I = 1, 6
               VALEK(IK) = NOMPAR(I)
C
               DO 30 ICP = 1, NBCMP2
                  I30 = M * ( ICP - 1 )
                  VALER(IR+ICP) = T(I30+I20+I10+I)
C
 30            CONTINUE
C
               CALL TBAJLI ( NOMTAB, NBPAR, NOPARA,
     &                          VALEI, VALER, C16B, VALEK, ILIGN )
 40         CONTINUE
C
 10      CONTINUE
C
 20   CONTINUE
C
      CALL JEDEMA()
      END
