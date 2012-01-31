      SUBROUTINE RVTEC0 ( T, CO, SP, ABSC, X, CMP, ND, SDM, NBPOIN,
     &                    DOCU,  NBCMP, PADR, NOMTAB, IOC, IOCC,
     &                    XNOVAR, NCHEFF, I1, ISD )
      IMPLICIT   NONE
      INTEGER             CO(*),SP(*),NBPOIN,NBCMP,PADR(*),IOC,IOCC,
     &                    I1, ISD
      REAL*8              T(*), ABSC(*), X(*)
      CHARACTER*4         DOCU
      CHARACTER*8         CMP(*), ND(*)
      CHARACTER*16        NCHEFF
      CHARACTER*19        NOMTAB
      CHARACTER*24        SDM, XNOVAR
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 30/01/2012   AUTEUR LEFEBVRE J-P.LEFEBVRE 
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
C     AFFICHAGE CHAM_ELEM DE NBCMP COMPOSANTES
C     COPIE ROUTINE RVIMPK
C     ------------------------------------------------------------------
C IN  T    : R  : VAL DE TOUTES LES CMP
C IN  CO   : I  : TABLE DES NOMBRES DE COUCHES
C IN  SP   : I  : TABLE DES NOMBRES DE SOUS-PT
C IN  S    : R  : TABLE DES ABSCISSES CURVILIGNES
C IN  X    : R  : TABLES DES COORDONNEES
C IN  CMP  : K8 : NOMS DE TOUTES LES CMP
C IN  ND   : K8 : NOMS DES EVENTUELS NOEUDS
C IN  SDM  : K24: NOM OJB SD MAILLES
C IN  NBPOIN : I  : NOMBRE DE POINTS
C IN  DOCU : K4 : TYPE DE LIEU
C IN  NBCMP : I  : NOMBRE TOTAL DE CMP
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
      INTEGER      NBPAR, ILIGN, NBSP, I, IKK, L, JAM,
     &             NBCO, LC, IS, IC, VALEI(1052), N1, ADRVAL, NBMAIL, J,
     &             ADRACC, JACC, IK, IR, II, IVARI(1000), NBCMP2, JVARI,
     &             ICO, LM, IM, NC, JNPAR, JTPAR, NBVARI,
     &             NBACC, NBPR, JACES, IAC, IADR
      REAL*8       VALER(1050)
      COMPLEX*16   C16B
      LOGICAL      EXIST, ERREUR
      CHARACTER*3  TYPPAR
      CHARACTER*7  KII
      CHARACTER*8  K8B, ACCES, NOMRES, CTYPE, NOPASE, COURBE
      CHARACTER*16 INTITU
      CHARACTER*24 NOMVAL, NOMACC, NNORES, NOPARA(1053), NOMJV
      CHARACTER*80 VALEK(1051)
      INTEGER      IARG
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      IF ( NBCMP .LE. 0 ) GOTO 9999
C
      IF ( DOCU.NE.'LSTN' .AND. DOCU.NE.'CHMM' .AND.
     &     DOCU.NE.'SGTD' .AND. DOCU.NE.'ARCC' .AND.
     &                          DOCU.NE.'SGT3' ) GOTO 9999
C
      CALL JELIRA(JEXNUM(XNOVAR,IOCC),'LONUTI',NBVARI,K8B)
      IF ( NBVARI .NE. 0 ) THEN
         CALL JELIRA ( JEXNUM(XNOVAR,IOCC), 'LONUTI', NBVARI, K8B )
         CALL JEVEUO ( JEXNUM(XNOVAR,IOCC), 'L', JVARI )
         IF ( NBVARI .EQ. 1  .AND. ZI(JVARI).EQ. -1 ) THEN
            NBCMP2 = SP(1)
         ELSE
           NBCMP2 = NBVARI
         ENDIF
      ELSE
         NBCMP2 = NBCMP
         DO 2, I = 1, NBCMP2, 1
            IVARI(I) = I
 2       CONTINUE
      ENDIF
      IF ( NBCMP2.GT.1000) CALL U2MESS('F','POSTRELE_13')
C
      CALL GETVTX ( 'ACTION', 'INTITULE', IOCC,IARG,1, INTITU, N1 )
      CALL GETVID ( 'ACTION', 'CHEMIN'  , IOCC,IARG,1, COURBE, NC )
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
      VALEK(IK) = INTITU
      NOPARA(NBPAR) = 'INTITULE'
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
            NOMJV = '&&RVRCCM.NOMS_ACCES'
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
 10            CONTINUE
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
      IF ( DOCU.EQ.'LSTN' .OR. DOCU.EQ.'CHMM' ) THEN
         CALL TBEXIP ( NOMTAB, 'NOEUD', EXIST, TYPPAR )
         IF ( .NOT. EXIST ) THEN
            CALL TBAJPA ( NOMTAB, 1, 'NOEUD', 'K8' )
         ENDIF
         NBPAR = NBPAR + 1
         NOPARA(NBPAR) = 'NOEUD'
      ENDIF
      CALL TBEXIP ( NOMTAB, 'MAILLE', EXIST, TYPPAR )
      IF ( .NOT. EXIST ) THEN
         CALL TBAJPA ( NOMTAB, 1, 'MAILLE', 'K8' )
      ENDIF
      NBPAR = NBPAR + 1
      NOPARA(NBPAR) = 'MAILLE'
      NBPAR = NBPAR + 1
      NOPARA(NBPAR) = 'ABSC_CURV'
      NBPAR = NBPAR + 1
      NOPARA(NBPAR) = 'COOR_X'
      NBPAR = NBPAR + 1
      NOPARA(NBPAR) = 'COOR_Y'
      NBPAR = NBPAR + 1
      NOPARA(NBPAR) = 'COOR_Z'
      IC = 0
      IS = 0
      DO 20, I = 1, NBPOIN, 1
         NBCO = CO(I)
         NBSP = SP(I)
         IF ( NBCO .GT. 1  .AND.  IC .EQ. 0 ) THEN
            CALL TBEXIP ( NOMTAB, 'NUME_COUCHE', EXIST,TYPPAR )
            IF ( .NOT. EXIST ) THEN
               CALL TBAJPA ( NOMTAB, 1, 'NUME_COUCHE', 'I' )
            ENDIF
            IC = 1
            NBPAR = NBPAR + 1
            NOPARA(NBPAR) = 'NUME_COUCHE'
         ENDIF
         IF ( NBSP .GT. 1  .AND.  IS .EQ. 0  ) THEN
            CALL TBEXIP ( NOMTAB, 'NUME_GAUSS', EXIST,TYPPAR )
            IF ( .NOT. EXIST ) THEN
               CALL TBAJPA ( NOMTAB, 1, 'NUME_GAUSS', 'I' )
            ENDIF
            IS = 1
            NBPAR = NBPAR + 1
            NOPARA(NBPAR) = 'NUME_GAUSS'
         ENDIF
 20   CONTINUE
      IF ( NBVARI .EQ. 0 ) THEN
         DO 40, I = 1, NBCMP2, 1
            NBPAR = NBPAR + 1
            NOPARA(NBPAR) = CMP(I)
 40      CONTINUE
      ELSE
         CALL WKVECT ( '&&RVTEC0.NOM_PARA', 'V V K8' , NBCMP2, JNPAR )
         CALL WKVECT ( '&&RVTEC0.TYP_PARA', 'V V K8' , NBCMP2, JTPAR )
         IF ( NBVARI .EQ. 1  .AND. ZI(JVARI).EQ. -1 ) THEN
            DO 12, I = 1, NBCMP2, 1
               IVARI(I) = I
               CALL CODENT ( I, 'G', KII )
               NBPAR = NBPAR + 1
               NOPARA(NBPAR) = 'V'//KII
               ZK8(JNPAR-1+I)  = 'V'//KII
               ZK8(JTPAR-1+I)  = 'R'
 12         CONTINUE
         ELSE
            DO 14, I = 1, NBCMP2, 1
               IVARI(I) = ZI(JVARI+I-1)
               CALL CODENT ( ZI(JVARI+I-1), 'G', KII )
               NBPAR = NBPAR + 1
               NOPARA(NBPAR) = 'V'//KII
               ZK8(JNPAR-1+I)  = 'V'//KII
               ZK8(JTPAR-1+I)  = 'R'
 14         CONTINUE
         ENDIF
         CALL TBAJPA ( NOMTAB, NBCMP2, ZK8(JNPAR), ZK8(JTPAR) )
      ENDIF
C
      ERREUR = .FALSE.
      IF ( NBPAR .GT. 1050 )        ERREUR = .TRUE.
      IF ( II+2  .GT. 1050 )        ERREUR = .TRUE.
      IF ( IR+4+NBCMP2 .GT. 1050 )  ERREUR = .TRUE.
      IF ( IK    .GT. 1050 )        ERREUR = .TRUE.
      IF ( ERREUR ) CALL U2MESS('F','POSTRELE_12')
C
      ILIGN = 0
C
      DO 30, I = 1, NBPOIN, 1
C
         VALER(IR+1) = ABSC(I)
         VALER(IR+2) = X(1+(I-1)*3)
         VALER(IR+3) = X(2+(I-1)*3)
         VALER(IR+4) = X(3+(I-1)*3)
C
         IKK = 0
         IF ( DOCU.EQ.'LSTN' .OR. DOCU.EQ.'CHMM' ) THEN
            IKK = IKK + 1
            VALEK(IK+IKK) = ND(I)
         ENDIF
C
         IF ( DOCU .EQ. 'LSTN' ) THEN
            CALL JEVEUO ( JEXNUM(SDM,I), 'L', JAM )
            CALL JELIRA ( JEXNUM(SDM,I), 'LONMAX', NBMAIL, K8B )
            L    = 0
            NBCO = CO(I)
            NBSP = SP(I)
            J    = PADR(I)
            LM   = NBCMP*NBSP
            LC   = LM*NBMAIL
         ELSE
            IF ( I .EQ. 1 ) THEN
               NBSP = SP(I)
               NBCO = CO(I)
               J    = 1
               LC   = NBSP*NBCMP
               LM   = LC*(2*NBCO-1)
               LC   = 2*LC
               NBMAIL = 1
               L    = 1
            ELSE IF ( I .EQ. NBPOIN ) THEN
               NBSP = SP(I-1)
               NBCO = CO(I-1)
               LC   = NBSP*NBCMP
               LM   = LC*(2*NBCO-1)
               J    = LC*(2*(I-2)*NBCO + 1) + 1
               LC   = 2*LC
               NBMAIL = 1
               L    = 0
            ELSE
               NBSP = SP(I)
               NBCO = CO(I)
               LC   = NBSP*NBCMP
               LM   = LC*(2*NBCO-1)
               J    = LC*(2*(I-2)*NBCO + 1) + 1
               LC   = 2*LC
               NBMAIL = 2
               L    = 0
            ENDIF
         ENDIF
C        POUR UN CHAMP DE TYPE "VARI"
C        LES SOUS-POINTS SONT PRIS EN CHARGE PAR LE NBCMP
         IF (NBVARI.NE.0) NBSP = 1
C
         DO 200, ICO = 1, NBCO, 1
C
            VALEI(II+1) = ICO
C
            DO 220, IS = 1, NBSP, 1
C
               VALEI(II+2) = IS
C
               DO 222, IM = 1, NBMAIL, 1
C
                  IF ( DOCU .EQ. 'LSTN' ) THEN
                     VALEK(IK+IKK+1) = ZK8(JAM+L+IM-1)
                  ELSE
                     VALEK(IK+IKK+1) = '   -    '
                  ENDIF
C
                  DO 224, IC = 1, NBCMP2, 1
                     VALER(IR+4+IC) =
     &           T(J-1+(ICO-1)*LC+(IS-1)*NBCMP+(IM-1)*LM+IVARI(IC))
 224              CONTINUE
C
                  CALL TBAJLI ( NOMTAB, NBPAR, NOPARA,
     &                          VALEI, VALER, C16B, VALEK, ILIGN )
C
 222           CONTINUE
C
 220        CONTINUE
C
 200     CONTINUE
C
 30   CONTINUE
C
      IF ( NBVARI .NE. 0 ) THEN
         CALL JEDETR ( '&&RVTEC0.NOM_PARA' )
         CALL JEDETR ( '&&RVTEC0.TYP_PARA' )
      ENDIF
C
 9999 CONTINUE
      CALL JEDEMA()
C
      END
