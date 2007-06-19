      SUBROUTINE TBEXTB ( TABIN, BASOUT, TABOUT, NPACRI, LIPACR, LCRPA,
     &                    VI, VR, VC, VK, LPREC, LCRIT, IRET )
      IMPLICIT   NONE
      INTEGER             NPACRI, VI(*), IRET
      REAL*8              VR(*), LPREC(*)
      COMPLEX*16          VC(*)
      CHARACTER*(*)       TABIN,BASOUT,TABOUT,LIPACR(*),LCRPA(*),VK(*),
     &                    LCRIT(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 19/06/2007   AUTEUR VIVAN L.VIVAN 
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
C     FILTRAGE ET EXTRACTION D'UNE NOUVELLE TABLE.
C ----------------------------------------------------------------------
C IN  : TABIN  : NOM DE LA TABLE DONT ON VEUT EXTRAIRE DES LIGNES
C IN  : BASOUT : BASE DE CREATION DE "TABOUT"
C IN  : TABOUT : NOM DE LA TABLE QUI CONTIENDRA LES LIGNES EXTRAITES
C IN  : NPACRI : NOMBRE DE PARAMETRES IMPLIQUES DANS LES CRITERES
C IN  : LIPACR : LISTE DES PARAMETRES CRITERES
C IN  : LCRPA  : LISTE DES CRITERES DE COMPARAISON
C IN  : VI     : LISTE DES CRITERES POUR LES PARAMETRES "I"
C IN  : VR     : LISTE DES CRITERES POUR LES PARAMETRES "R"
C IN  : VC     : LISTE DES CRITERES POUR LES PARAMETRES "C"
C IN  : VK     : LISTE DES CRITERES POUR LES PARAMETRES "K"
C IN  : LPREC  : PRECISION POUR LES PARAMETRES "R"
C IN  : LCRIT  : CRITERE POUR LES PARAMETRES "R"
C OUT : IRET   : =  0 , OK
C                = 10 , LE PARAMETRE N'EXISTE PAS
C                = 20 , PAS DE LIGNES POUR LE PARAMETRE DONNE
C ----------------------------------------------------------------------
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
C ----------------------------------------------------------------------
      INTEGER      IRT, NBPARA, NBLIGN, JTBNP, NBPU, JNUMI
      INTEGER      JTBLP, I, J, K, N, JVALE, ITROUV, JTYPE, ITROU2
      INTEGER      KI, KR, KC, KK, JVALL, JPARR, NBP, ISMAEM
      INTEGER      JVALI, JVALR, JVALC, JVALK, IMAX, IMIN
      REAL*8       PREC, REFR, RMAX, RMIN, R8MAEM
      COMPLEX*16   CMIN, CMAX
      CHARACTER*1  BASE
      CHARACTER*4  TYPE, CRIT
      CHARACTER*8  RELA
      CHARACTER*19 NOMTAB
      CHARACTER*24 NOMJV, NOMJVL, INPAR, JNPAR
      LOGICAL      LOK
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      NOMTAB = TABIN
      BASE   = BASOUT(1:1)
      IRET   = 0
C
C     --- VERIFICATION DE LA BASE ---
C
      IF ( BASE.NE.'V' .AND. BASE.NE.'G' ) THEN
         CALL U2MESK('F','UTILITAI2_48',1,BASE)
      ENDIF
C
C     --- VERIFICATION DE LA TABLE ---
C
      CALL JEEXIN ( NOMTAB//'.TBBA', IRT )
      IF ( IRT .EQ. 0 ) THEN
         CALL U2MESS('F','UTILITAI4_64')
      ENDIF
C
      CALL JEVEUO ( NOMTAB//'.TBNP' , 'E', JTBNP )
      NBPARA = ZI(JTBNP  )
      NBLIGN = ZI(JTBNP+1)
      IF ( NBPARA .EQ. 0 ) THEN
         CALL U2MESS('F','UTILITAI4_65')
      ENDIF
      IF ( NBLIGN .EQ. 0 ) THEN
         CALL U2MESS('F','UTILITAI4_66')
      ENDIF
C
C     --- VERIFICATION QUE LES PARAMETRES EXISTENT DANS LA TABLE ---
C
      CALL JEVEUO ( NOMTAB//'.TBLP' , 'L', JTBLP )
      DO 10 I = 1 , NPACRI
         INPAR = LIPACR(I)
         DO 12 J = 1 , NBPARA
            JNPAR = ZK24(JTBLP+4*(J-1))
            IF ( INPAR .EQ. JNPAR ) GOTO 10
 12      CONTINUE
         IRET = 10
         GOTO 9999
 10   CONTINUE
C
      NBPU = NBLIGN
      CALL WKVECT ( '&&TBEXTB.NUMERO', 'V V I', NBPU, JNUMI)
      DO 18 I = 1 , NBPU
         ZI(JNUMI+I-1) = I
 18   CONTINUE
C
      KI = 0
      KR = 0
      KC = 0
      KK = 0
      DO 20 I = 1 , NPACRI
         ITROUV = 0
         INPAR = LIPACR(I)
         RELA  = LCRPA(I)
         DO 22 J = 1 , NBPARA
            JNPAR = ZK24(JTBLP+4*(J-1))
            IF ( INPAR .EQ. JNPAR ) THEN
               TYPE   = ZK24(JTBLP+4*(J-1)+1)
               NOMJV  = ZK24(JTBLP+4*(J-1)+2)
               NOMJVL = ZK24(JTBLP+4*(J-1)+3)
               CALL JEVEUO ( NOMJV, 'L', JVALE )
               CALL JEVEUO ( NOMJVL, 'L', JVALL )
               IF ( RELA .EQ. 'VIDE' ) THEN
                  DO 50 K = 1 , NBPU
                     N = ZI(JNUMI+K-1)
                     ZI(JNUMI+K-1) = 0
                     IF ( ZI(JVALL+N-1).EQ.0 ) THEN
                        ITROUV = ITROUV + 1
                        ZI(JNUMI+ITROUV-1) = N
                     ENDIF
  50              CONTINUE
                  GOTO 24
               ELSEIF ( RELA .EQ. 'NON_VIDE' ) THEN
                  DO 51 K = 1 , NBPU
                     N = ZI(JNUMI+K-1)
                     ZI(JNUMI+K-1) = 0
                     IF ( ZI(JVALL+N-1).EQ.1 ) THEN
                        ITROUV = ITROUV + 1
                        ZI(JNUMI+ITROUV-1) = N
                     ENDIF
  51              CONTINUE
                  GOTO 24
               ELSEIF ( RELA .EQ. 'MAXI' ) THEN
                  ITROU2 = 0
                  IF ( TYPE(1:1) .EQ. 'I' ) THEN
                     IMAX  = -ISMAEM()
                     DO 52 K = 1 , NBPU
                        N = ZI(JNUMI+K-1)
                        ZI(JNUMI+K-1) = 0
                        IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 52
                        IF ( ZI(JVALE+N-1) .GT. IMAX ) THEN
                           IMAX   = ZI(JVALE+N-1)
                           ITROU2 = N
                        ENDIF
  52                 CONTINUE
                  ELSEIF ( TYPE(1:1) .EQ. 'R' ) THEN
                     RMAX  = -R8MAEM()
                     DO 53 K = 1 , NBPU
                        N = ZI(JNUMI+K-1)
                        ZI(JNUMI+K-1) = 0
                        IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 53
                        IF ( ZR(JVALE+N-1) .GT. RMAX ) THEN
                           RMAX   = ZR(JVALE+N-1)
                           ITROU2 = N
                        ENDIF
  53                 CONTINUE
                  ENDIF
                  IF ( ITROU2 .NE. 0 ) THEN
                     ITROUV = ITROUV + 1
                     ZI(JNUMI+ITROUV-1) = ITROU2
                  ENDIF
                  GOTO 24
               ELSEIF ( RELA .EQ. 'ABS_MAXI' ) THEN
                  ITROU2 = 0
                  IF ( TYPE(1:1) .EQ. 'I' ) THEN
                     IMAX  = -ISMAEM()
                     DO 54 K = 1 , NBPU
                        N = ZI(JNUMI+K-1)
                        ZI(JNUMI+K-1) = 0
                        IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 54
                        IF ( ABS(ZI(JVALE+N-1)) .GT. IMAX ) THEN
                           IMAX   = ABS(ZI(JVALE+N-1))
                           ITROU2 = N
                        ENDIF
  54                 CONTINUE
                  ELSEIF ( TYPE(1:1) .EQ. 'R' ) THEN
                     RMAX  = -R8MAEM()
                     DO 55 K = 1 , NBPU
                        N = ZI(JNUMI+K-1)
                        ZI(JNUMI+K-1) = 0
                        IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 55
                        IF ( ABS(ZR(JVALE+N-1)) .GT. RMAX ) THEN
                           RMAX   = ABS(ZR(JVALE+N-1))
                           ITROU2 = N
                        ENDIF
  55                 CONTINUE
                  ELSEIF ( TYPE(1:1) .EQ. 'C' ) THEN
                     CMAX  = DCMPLX( -1.D-50 , -1.D-50 )
                     DO 60 K = 1 , NBPU
                        N = ZI(JNUMI+K-1)
                        ZI(JNUMI+K-1) = 0
                        IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 60
                        IF ( ABS(ZC(JVALE+N-1)) .GT. ABS(CMAX) ) THEN
                           CMAX   = ZC(JVALE+N-1)
                           ITROU2 = N
                        ENDIF
  60                 CONTINUE
                  ENDIF
                  IF ( ITROU2 .NE. 0 ) THEN
                     ITROUV = ITROUV + 1
                     ZI(JNUMI+ITROUV-1) = ITROU2
                  ENDIF
                  GOTO 24
               ELSEIF ( RELA .EQ. 'MINI' ) THEN
                  ITROU2 = 0
                  IF ( TYPE(1:1) .EQ. 'I' ) THEN
                     IMIN  = ISMAEM()
                     DO 56 K = 1 , NBPU
                        N = ZI(JNUMI+K-1)
                        ZI(JNUMI+K-1) = 0
                        IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 56
                        IF ( ZI(JVALE+N-1) .LT. IMIN ) THEN
                           IMIN   = ZI(JVALE+N-1)
                           ITROU2 = N
                        ENDIF
  56                 CONTINUE
                  ELSEIF ( TYPE(1:1) .EQ. 'R' ) THEN
                     RMIN  = R8MAEM()
                     DO 57 K = 1 , NBPU
                        N = ZI(JNUMI+K-1)
                        ZI(JNUMI+K-1) = 0
                        IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 57
                        IF ( ZR(JVALE+N-1) .LT. RMIN ) THEN
                           RMIN   = ZR(JVALE+N-1)
                           ITROU2 = N
                        ENDIF
  57                 CONTINUE
                  ENDIF
                  IF ( ITROU2 .NE. 0 ) THEN
                     ITROUV = ITROUV + 1
                     ZI(JNUMI+ITROUV-1) = ITROU2
                  ENDIF
                  GOTO 24
               ELSEIF ( RELA .EQ. 'ABS_MINI' ) THEN
                  ITROU2 = 0
                  IF ( TYPE(1:1) .EQ. 'I' ) THEN
                     IMIN  = ISMAEM()
                     DO 58 K = 1 , NBPU
                        N = ZI(JNUMI+K-1)
                        ZI(JNUMI+K-1) = 0
                        IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 58
                        IF ( ABS(ZI(JVALE+N-1)) .LT. IMIN ) THEN
                           IMIN   = ABS(ZI(JVALE+N-1))
                           ITROU2 = N
                        ENDIF
  58                 CONTINUE
                  ELSEIF ( TYPE(1:1) .EQ. 'R' ) THEN
                     RMIN  = R8MAEM()
                     DO 59 K = 1 , NBPU
                        N = ZI(JNUMI+K-1)
                        ZI(JNUMI+K-1) = 0
                        IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 59
                        IF ( ABS(ZR(JVALE+N-1)) .LT. RMIN ) THEN
                           RMIN   = ABS(ZR(JVALE+N-1))
                           ITROU2 = N
                        ENDIF
  59                 CONTINUE
                  ELSEIF ( TYPE(1:1) .EQ. 'C' ) THEN
                     CMIN  = DCMPLX ( +1.D+50 , +1.D+50 )
                     DO 61 K = 1 , NBPU
                        N = ZI(JNUMI+K-1)
                        ZI(JNUMI+K-1) = 0
                        IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 61
                        IF ( ABS(ZC(JVALE+N-1)) .LT. ABS(CMIN) ) THEN
                           CMIN   = ZC(JVALE+N-1)
                           ITROU2 = N
                        ENDIF
  61                 CONTINUE
                  ENDIF
                  IF ( ITROU2 .NE. 0 ) THEN
                     ITROUV = ITROUV + 1
                     ZI(JNUMI+ITROUV-1) = ITROU2
                  ENDIF
                  GOTO 24
               ENDIF
               IF ( TYPE(1:1) .EQ. 'I' ) THEN
                  KI = KI + 1
                  DO 30 K = 1 , NBPU
                     N = ZI(JNUMI+K-1)
                     ZI(JNUMI+K-1) = 0
                     IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 30
                     IF ( RELA .EQ. 'EQ' ) THEN
                        IF ( ZI(JVALE+N-1) .EQ. VI(KI) ) THEN
                           ITROUV = ITROUV + 1
                           ZI(JNUMI+ITROUV-1) = N
                        ENDIF
                     ELSEIF ( RELA .EQ. 'LT' ) THEN
                        IF ( ZI(JVALE+N-1) .LT. VI(KI) ) THEN
                           ITROUV = ITROUV + 1
                           ZI(JNUMI+ITROUV-1) = N
                        ENDIF
                     ELSEIF ( RELA .EQ. 'GT' ) THEN
                        IF ( ZI(JVALE+N-1) .GT. VI(KI) ) THEN
                           ITROUV = ITROUV + 1
                           ZI(JNUMI+ITROUV-1) = N
                        ENDIF
                     ELSEIF ( RELA .EQ. 'NE' ) THEN
                        IF ( ZI(JVALE+N-1) .NE. VI(KI) ) THEN
                           ITROUV = ITROUV + 1
                           ZI(JNUMI+ITROUV-1) = N
                        ENDIF
                     ELSEIF ( RELA .EQ. 'LE' ) THEN
                        IF ( ZI(JVALE+N-1) .LE. VI(KI) ) THEN
                           ITROUV = ITROUV + 1
                           ZI(JNUMI+ITROUV-1) = N
                        ENDIF
                     ELSEIF ( RELA .EQ. 'GE' ) THEN
                        IF ( ZI(JVALE+N-1) .GE. VI(KI) ) THEN
                           ITROUV = ITROUV + 1
                           ZI(JNUMI+ITROUV-1) = N
                        ENDIF
                     ENDIF
 30               CONTINUE
                  GOTO 24
               ELSEIF ( TYPE(1:1) .EQ. 'R' ) THEN
                  KR = KR + 1
                  PREC  = LPREC(KR)
                  CRIT  = LCRIT(KR)
                  DO 31 K = 1 , NBPU
                     N = ZI(JNUMI+K-1)
                     ZI(JNUMI+K-1) = 0
                     IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 31
                     IF ( RELA .EQ. 'EQ' ) THEN
                        REFR = ZR(JVALE+N-1)
                        IF ( CRIT .EQ. 'RELA' ) THEN
                           LOK = (ABS(VR(KR)-REFR) .LE. PREC*ABS(REFR))
                        ELSEIF ( CRIT .EQ. 'EGAL' ) THEN
                           LOK = ( VR(KR) .EQ. REFR )
                        ELSE
                           LOK = ( ABS(VR(KR) - REFR) .LE. PREC )
                        ENDIF
                        IF ( LOK ) THEN
                           ITROUV = ITROUV + 1
                           ZI(JNUMI+ITROUV-1) = N
                        ENDIF
                     ELSEIF ( RELA .EQ. 'LT' ) THEN
                        IF ( ZR(JVALE+N-1) .LT. VR(KR) ) THEN
                           ITROUV = ITROUV + 1
                           ZI(JNUMI+ITROUV-1) = N
                        ENDIF
                     ELSEIF ( RELA .EQ. 'GT' ) THEN
                        IF ( ZR(JVALE+N-1) .GT. VR(KR) ) THEN
                           ITROUV = ITROUV + 1
                           ZI(JNUMI+ITROUV-1) = N
                        ENDIF
                     ELSEIF ( RELA .EQ. 'NE' ) THEN
                        IF ( ZR(JVALE+N-1) .NE. VR(KR) ) THEN
                           ITROUV = ITROUV + 1
                           ZI(JNUMI+ITROUV-1) = N
                        ENDIF
                     ELSEIF ( RELA .EQ. 'LE' ) THEN
                        IF ( ZR(JVALE+N-1) .LE. VR(KR) ) THEN
                           ITROUV = ITROUV + 1
                           ZI(JNUMI+ITROUV-1) = N
                        ENDIF
                     ELSEIF ( RELA .EQ. 'GE' ) THEN
                        IF ( ZR(JVALE+N-1) .GE. VR(KR) ) THEN
                           ITROUV = ITROUV + 1
                           ZI(JNUMI+ITROUV-1) = N
                        ENDIF
                     ENDIF
 31               CONTINUE
                  GOTO 24
               ELSEIF ( TYPE(1:1) .EQ. 'C' ) THEN
                  KC = KC + 1
                  DO 32 K = 1 , NBPU
                     N = ZI(JNUMI+K-1)
                     ZI(JNUMI+K-1) = 0
                     IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 32
                     IF ( RELA .EQ. 'EQ' ) THEN
                        IF ( ZC(JVALE+N-1) .EQ. VC(KC) ) THEN
                           ITROUV = ITROUV + 1
                           ZI(JNUMI+ITROUV-1) = N
                        ENDIF
                     ELSEIF ( RELA .EQ. 'NE' ) THEN
                        IF ( ZC(JVALE+N-1) .NE. VC(KC) ) THEN
                           ITROUV = ITROUV + 1
                           ZI(JNUMI+ITROUV-1) = N
                        ENDIF
                     ENDIF
 32               CONTINUE
                  GOTO 24
               ELSEIF ( TYPE(1:3) .EQ. 'K80' ) THEN
                  KK = KK + 1
                  DO 33 K = 1 , NBPU
                     N = ZI(JNUMI+K-1)
                     ZI(JNUMI+K-1) = 0
                     IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 33
                     IF ( RELA .EQ. 'EQ' ) THEN
                        IF ( ZK80(JVALE+N-1) .EQ. VK(KK) ) THEN
                           ITROUV = ITROUV + 1
                           ZI(JNUMI+ITROUV-1) = N
                        ENDIF
                     ELSEIF ( RELA .EQ. 'NE' ) THEN
                        IF ( ZK80(JVALE+N-1) .NE. VK(KK) ) THEN
                           ITROUV = ITROUV + 1
                           ZI(JNUMI+ITROUV-1) = N
                        ENDIF
                     ENDIF
 33               CONTINUE
                  GOTO 24
               ELSEIF ( TYPE(1:3) .EQ. 'K32' ) THEN
                  KK = KK + 1
                  DO 34 K = 1 , NBPU
                     N = ZI(JNUMI+K-1)
                     ZI(JNUMI+K-1) = 0
                     IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 34
                     IF ( RELA .EQ. 'EQ' ) THEN
                        IF ( ZK32(JVALE+N-1) .EQ. VK(KK) ) THEN
                           ITROUV = ITROUV + 1
                           ZI(JNUMI+ITROUV-1) = N
                        ENDIF
                     ELSEIF ( RELA .EQ. 'NE' ) THEN
                        IF ( ZK32(JVALE+N-1) .NE. VK(KK) ) THEN
                           ITROUV = ITROUV + 1
                           ZI(JNUMI+ITROUV-1) = N
                        ENDIF
                     ENDIF
 34               CONTINUE
                  GOTO 24
               ELSEIF ( TYPE(1:3) .EQ. 'K24' ) THEN
                  KK = KK + 1
                  DO 35 K = 1 , NBPU
                     N = ZI(JNUMI+K-1)
                     ZI(JNUMI+K-1) = 0
                     IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 35
                     IF ( RELA .EQ. 'EQ' ) THEN
                        IF ( ZK24(JVALE+N-1) .EQ. VK(KK) ) THEN
                           ITROUV = ITROUV + 1
                           ZI(JNUMI+ITROUV-1) = N
                        ENDIF
                     ELSEIF ( RELA .EQ. 'NE' ) THEN
                        IF ( ZK24(JVALE+N-1) .NE. VK(KK) ) THEN
                           ITROUV = ITROUV + 1
                           ZI(JNUMI+ITROUV-1) = N
                        ENDIF
                     ENDIF
 35               CONTINUE
                  GOTO 24
               ELSEIF ( TYPE(1:3) .EQ. 'K16' ) THEN
                  KK = KK + 1
                  DO 36 K = 1 , NBPU
                     N = ZI(JNUMI+K-1)
                     ZI(JNUMI+K-1) = 0
                     IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 36
                     IF ( RELA .EQ. 'EQ' ) THEN
                        IF ( ZK16(JVALE+N-1) .EQ. VK(KK) ) THEN
                           ITROUV = ITROUV + 1
                           ZI(JNUMI+ITROUV-1) = N
                        ENDIF
                     ELSEIF ( RELA .EQ. 'NE' ) THEN
                        IF ( ZK16(JVALE+N-1) .NE. VK(KK) ) THEN
                           ITROUV = ITROUV + 1
                           ZI(JNUMI+ITROUV-1) = N
                        ENDIF
                     ENDIF
 36               CONTINUE
                  GOTO 24
               ELSEIF ( TYPE(1:2) .EQ. 'K8' ) THEN
                  KK = KK + 1
                  DO 37 K = 1 , NBPU
                     N = ZI(JNUMI+K-1)
                     ZI(JNUMI+K-1) = 0
                     IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 37
                     IF ( RELA .EQ. 'EQ' ) THEN
                        IF ( ZK8(JVALE+N-1) .EQ. VK(KK) ) THEN
                           ITROUV = ITROUV + 1
                           ZI(JNUMI+ITROUV-1) = N
                        ENDIF
                     ELSEIF ( RELA .EQ. 'NE' ) THEN
                        IF ( ZK8(JVALE+N-1) .NE. VK(KK) ) THEN
                           ITROUV = ITROUV + 1
                           ZI(JNUMI+ITROUV-1) = N
                        ENDIF
                     ENDIF
 37               CONTINUE
                  GOTO 24
               ENDIF
            ENDIF
 22      CONTINUE
 24      CONTINUE
         IF ( ITROUV .EQ. 0 ) THEN
            IRET = 20
            GOTO 9999
         ENDIF
         NBPU = ITROUV
 20   CONTINUE
C
C     --- ON RECUPERE LES PARAMETRES ET LEURS TYPES  ---
C
      CALL WKVECT ( '&&TBEXTB.TYPE_R', 'V V K8' , NBPARA, JTYPE)
      CALL WKVECT ( '&&TBEXTB.PARA_R', 'V V K24', NBPARA, JPARR)
      DO 38 I = 1 , NBPARA
         ZK24(JPARR+I-1) = ZK24(JTBLP+4*(I-1))
         ZK8 (JTYPE+I-1) = ZK24(JTBLP+4*(I-1)+1)
 38   CONTINUE
C
C     --- CREATION DE LA TABLE ---
C
      CALL TBCRSD ( TABOUT , BASOUT )
      CALL TBAJPA ( TABOUT, NBPARA, ZK24(JPARR), ZK8(JTYPE) )
      CALL WKVECT ( '&&TBEXTB.VALE_I', 'V V I'  , NBPARA, JVALI)
      CALL WKVECT ( '&&TBEXTB.VALE_R', 'V V R'  , NBPARA, JVALR)
      CALL WKVECT ( '&&TBEXTB.VALE_C', 'V V C'  , NBPARA, JVALC)
      CALL WKVECT ( '&&TBEXTB.VALE_K', 'V V K80', NBPARA, JVALK)
      DO 40 K = 1 , NBPU
         KI = 0
         KR = 0
         KC = 0
         KK = 0
         N = ZI(JNUMI+K-1)
         NBP = 0
         DO 42 J = 1 , NBPARA
            JNPAR  = ZK24(JTBLP+4*(J-1)  )
            TYPE   = ZK24(JTBLP+4*(J-1)+1)
            NOMJV  = ZK24(JTBLP+4*(J-1)+2)
            NOMJVL = ZK24(JTBLP+4*(J-1)+3)
            CALL JEVEUO ( NOMJV , 'L', JVALE )
            CALL JEVEUO ( NOMJVL, 'L', JVALL )
            IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 42
            NBP = NBP + 1
            ZK24(JPARR+NBP-1) = JNPAR
            IF ( TYPE(1:1) .EQ. 'I' ) THEN
               KI = KI + 1
               ZI(JVALI+KI-1) = ZI(JVALE+N-1)
            ELSEIF ( TYPE(1:1) .EQ. 'R' ) THEN
               KR = KR + 1
               ZR(JVALR+KR-1) = ZR(JVALE+N-1)
            ELSEIF ( TYPE(1:1) .EQ. 'C' ) THEN
               KC = KC + 1
               ZC(JVALC+KC-1) = ZC(JVALE+N-1)
            ELSEIF ( TYPE(1:3) .EQ. 'K80' ) THEN
               KK = KK + 1
               ZK80(JVALK+KK-1) = ZK80(JVALE+N-1)
            ELSEIF ( TYPE(1:3) .EQ. 'K32' ) THEN
               KK = KK + 1
               ZK80(JVALK+KK-1) = ZK32(JVALE+N-1)
            ELSEIF ( TYPE(1:3) .EQ. 'K24' ) THEN
               KK = KK + 1
               ZK80(JVALK+KK-1) = ZK24(JVALE+N-1)
            ELSEIF ( TYPE(1:3) .EQ. 'K16' ) THEN
               KK = KK + 1
               ZK80(JVALK+KK-1) = ZK16(JVALE+N-1)
            ELSEIF ( TYPE(1:2) .EQ. 'K8'  ) THEN
               KK = KK + 1
               ZK80(JVALK+KK-1) = ZK8(JVALE+N-1)
            ENDIF
 42      CONTINUE
         IF ( NBP .EQ. 0 ) GOTO 40
         CALL TBAJLI ( TABOUT, NBP, ZK24(JPARR), ZI(JVALI),
     &                 ZR(JVALR), ZC(JVALC), ZK80(JVALK), 0 )
 40   CONTINUE
C
 9999 CONTINUE
      CALL JEDETR ( '&&TBEXTB.NUMERO' )
      CALL JEDETR ( '&&TBEXTB.TYPE_R' )
      CALL JEDETR ( '&&TBEXTB.PARA_R' )
      CALL JEDETR ( '&&TBEXTB.VALE_I' )
      CALL JEDETR ( '&&TBEXTB.VALE_R' )
      CALL JEDETR ( '&&TBEXTB.VALE_C' )
      CALL JEDETR ( '&&TBEXTB.VALE_K' )
C
      CALL JEDEMA()
      END
