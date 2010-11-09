      SUBROUTINE TBLIVA ( NOMTA, NPACRI, LIPACR, VI, VR, VC, VK, CRIT,
     &                  PREC, PARA, CTYPE, VALI, VALR, VALC, VALK, IER )
      IMPLICIT   NONE
      INTEGER             NPACRI, VI(*), VALI, IER
      REAL*8              VR(*), VALR, PREC(*)
      COMPLEX*16          VC(*), VALC
      CHARACTER*(*)       NOMTA,LIPACR(*),VK(*),VALK,CRIT(*),CTYPE,PARA
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 08/11/2010   AUTEUR REZETTE C.REZETTE 
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
C      LECTURE D'UNE VALEUR D'UNE CELLULE DE LA TABLE.
C ----------------------------------------------------------------------
C IN  : NOMTA  : NOM DE LA STRUCTURE "TABLE".
C IN  : NPACRI : NOMBRE DE PARAMETRES IMPLIQUES DANS LES CRITERES
C IN  : LIPACR : LISTE DES PARAMETRES CRITERES
C IN  : VI     : LISTE DES CRITERES POUR LES PARAMETRES "I"
C IN  : VR     : LISTE DES CRITERES POUR LES PARAMETRES "R"
C IN  : VC     : LISTE DES CRITERES POUR LES PARAMETRES "C"
C IN  : VK     : LISTE DES CRITERES POUR LES PARAMETRES "K"
C IN  : CRIT   : CRITERE POUR LES PARAMETRES REELS
C IN  : PREC   : PRECISION POUR LES PARAMETRES REELS
C IN  : PARA   : PARAMETRE A TROUVER
C OUT : CTYPE  : TYPE DE LA VALEUR TROUVEE
C OUT : VALI   : VALEUR TROUVEE SI PARAMETRES "I"
C OUT : VALR   : VALEUR TROUVEE SI PARAMETRES "R"
C OUT : VALC   : VALEUR TROUVEE SI PARAMETRES "C"
C OUT : VALK   : VALEUR TROUVEE SI PARAMETRES "K"
C OUT : IER    : CODE RETOUR 0 : OK
C                            1 : PARA N'EXISTE PAS
C                            2 : PAS DE LIGNE TROUVEE
C                            3 : PLUSIEURS LIGNES TROUVEES
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
      INTEGER      IRET, NBPARA, NBLIGN, JTBNP, NBPU, JNUMI
      INTEGER      JTBLP, I, J, K, N, JVALE, ITROUV
      INTEGER      KI, KR, KC, K8, JVALL
      REAL*8       REFR, XR, EPSI
      COMPLEX*16   REFC, XC
      CHARACTER*4  RELA, TYPE
      CHARACTER*8  K8B
      CHARACTER*19 NOMTAB
      CHARACTER*24 NOMJV, NOMJVL, INPAR, JNPAR
      LOGICAL      LOK
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      IER = 0
      CTYPE = '?'
C
      VALI=0
      VALR=0.D0
      VALC=DCMPLX(0.D0,0.D0)
      VALK=' '
C
      NOMTAB = NOMTA
      CALL JEEXIN ( NOMTAB//'.TBBA', IRET )
      IF ( IRET .EQ. 0 ) THEN
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
      CALL JEVEUO ( NOMTAB//'.TBLP' , 'L', JTBLP )
C
C     --- VERIFICATION QUE LES PARAMETRES EXISTENT DANS LA TABLE ---
C
      DO 10 I = 1 , NPACRI
         INPAR = LIPACR(I)
         DO 12 J = 1 , NBPARA
            JNPAR = ZK24(JTBLP+4*(J-1))
            IF ( INPAR .EQ. JNPAR ) GOTO 10
 12      CONTINUE
         IER = 1
         GOTO 9999
 10   CONTINUE
      INPAR = PARA
      DO 14 J = 1 , NBPARA
         JNPAR = ZK24(JTBLP+4*(J-1))
         IF ( INPAR .EQ. JNPAR ) GOTO 16
 14   CONTINUE
      IER = 1
      GOTO 9999
 16   CONTINUE
C
      NOMJV = ZK24(JTBLP+2)
      CALL JELIRA ( NOMJV, 'LONUTI', NBPU, K8B)
      CALL WKVECT ( '&&TBLIVA.NUMERO', 'V V I', NBPU, JNUMI)
      DO 18 I = 1 , NBPU
         ZI(JNUMI+I-1) = I
 18   CONTINUE
C
      KI = 0
      KR = 0
      KC = 0
      K8 = 0
      DO 20 I = 1 , NPACRI
         ITROUV = 0
         INPAR = LIPACR(I)
         DO 22 J = 1 , NBPARA
            JNPAR = ZK24(JTBLP+4*(J-1))
            IF ( INPAR .EQ. JNPAR ) THEN
               TYPE   = ZK24(JTBLP+4*(J-1)+1)
               NOMJV  = ZK24(JTBLP+4*(J-1)+2)
               NOMJVL = ZK24(JTBLP+4*(J-1)+3)
               CALL JEVEUO ( NOMJV, 'L', JVALE )
               CALL JEVEUO ( NOMJVL, 'L', JVALL )
               IF ( TYPE(1:1) .EQ. 'I' ) THEN
                  KI = KI + 1
                  DO 30 K = 1 , NBPU
                     N = ZI(JNUMI+K-1)
                     IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 30
                     IF ( ZI(JVALE+N-1) .EQ. VI(KI) ) THEN
                        ITROUV = ITROUV + 1
                        ZI(JNUMI+ITROUV-1) = N
                     ENDIF
 30               CONTINUE
               ELSEIF ( TYPE(1:1) .EQ. 'R' ) THEN
                  KR = KR + 1
                  RELA = CRIT(KR)
                  EPSI = PREC(KR)
                  XR = VR(KR)
                  DO 31 K = 1 , NBPU
                     N = ZI(JNUMI+K-1)
                     IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 31
                     REFR = ZR(JVALE+N-1)
                     IF ( RELA .EQ. 'RELA' ) THEN
                        LOK = ( ABS(XR-REFR) .LE. EPSI * ABS(REFR) )
                     ELSEIF ( RELA .EQ. 'EGAL' ) THEN
                        LOK = ( REFR .EQ. XR )
                     ELSE
                        LOK = ( ABS(XR - REFR) .LE. EPSI )
                     ENDIF
                     IF ( LOK ) THEN
                        ITROUV = ITROUV + 1
                        ZI(JNUMI+ITROUV-1) = N
                     ENDIF
 31               CONTINUE
               ELSEIF ( TYPE(1:1) .EQ. 'C' ) THEN
                  KC = KC + 1
                  RELA = CRIT(KC)
                  EPSI = PREC(KC)
                  XC = VC(KC)
                  DO 32 K = 1 , NBPU
                     N = ZI(JNUMI+K-1)
                     IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 32
                     REFC = ZC(JVALE+N-1)
                     IF ( RELA .EQ. 'RELA' ) THEN
                        LOK = ( ABS(XC-REFC) .LE. EPSI * ABS(REFC) )
                     ELSE
                        LOK = ( ABS(XC - REFC) .LE. EPSI )
                     ENDIF
                     IF ( LOK ) THEN
                        ITROUV = ITROUV + 1
                        ZI(JNUMI+ITROUV-1) = N
                     ENDIF
 32               CONTINUE
               ELSEIF ( TYPE(1:3) .EQ. 'K80' ) THEN
                  K8 = K8 + 1
                  DO 33 K = 1 , NBPU
                     N = ZI(JNUMI+K-1)
                     IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 33
                     IF ( ZK80(JVALE+N-1) .EQ. VK(K8) ) THEN
                        ITROUV = ITROUV + 1
                        ZI(JNUMI+ITROUV-1) = N
                     ENDIF
 33               CONTINUE
               ELSEIF ( TYPE(1:3) .EQ. 'K32' ) THEN
                  K8 = K8 + 1
                  DO 34 K = 1 , NBPU
                     N = ZI(JNUMI+K-1)
                     IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 34
                     IF ( ZK32(JVALE+N-1) .EQ. VK(K8) ) THEN
                        ITROUV = ITROUV + 1
                        ZI(JNUMI+ITROUV-1) = N
                     ENDIF
 34               CONTINUE
               ELSEIF ( TYPE(1:3) .EQ. 'K24' ) THEN
                  K8 = K8 + 1
                  DO 35 K = 1 , NBPU
                     N = ZI(JNUMI+K-1)
                     IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 35
                     IF ( ZK24(JVALE+N-1) .EQ. VK(K8) ) THEN
                        ITROUV = ITROUV + 1
                        ZI(JNUMI+ITROUV-1) = N
                     ENDIF
 35               CONTINUE
               ELSEIF ( TYPE(1:3) .EQ. 'K16' ) THEN
                  K8 = K8 + 1
                  DO 36 K = 1 , NBPU
                     N = ZI(JNUMI+K-1)
                     IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 36
                     IF ( ZK16(JVALE+N-1) .EQ. VK(K8) ) THEN
                        ITROUV = ITROUV + 1
                        ZI(JNUMI+ITROUV-1) = N
                     ENDIF
 36               CONTINUE
               ELSEIF ( TYPE(1:2) .EQ. 'K8' ) THEN
                  K8 = K8 + 1
                  DO 37 K = 1 , NBPU
                     N = ZI(JNUMI+K-1)
                     IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 37
                     IF ( ZK8(JVALE+N-1) .EQ. VK(K8) ) THEN
                        ITROUV = ITROUV + 1
                        ZI(JNUMI+ITROUV-1) = N
                     ENDIF
 37               CONTINUE
               ENDIF
            ENDIF
 22      CONTINUE
         IF ( ITROUV .EQ. 0 ) THEN
            IER = 2
            GOTO 9999
         ENDIF
         NBPU = ITROUV
 20   CONTINUE
C
      ITROUV = 0
      INPAR = PARA
      DO 40 J = 1 , NBPARA
         JNPAR = ZK24(JTBLP+4*(J-1))
         IF ( INPAR .EQ. JNPAR ) THEN
            TYPE   = ZK24(JTBLP+4*(J-1)+1)
            NOMJV  = ZK24(JTBLP+4*(J-1)+2)
            NOMJVL = ZK24(JTBLP+4*(J-1)+3)
            CALL JEVEUO ( NOMJV, 'L', JVALE )
            CALL JEVEUO ( NOMJVL, 'L', JVALL )
            IF ( TYPE(1:1) .EQ. 'I' ) THEN
               DO 50 K = 1 , NBPU
                  N = ZI(JNUMI+K-1)
                  IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 50
                  ITROUV = ITROUV + 1
                  VALI = ZI(JVALE+N-1)
                  CTYPE = 'I'
 50            CONTINUE
            ELSEIF ( TYPE(1:1) .EQ. 'R' ) THEN
               DO 51 K = 1 , NBPU
                  N = ZI(JNUMI+K-1)
                  IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 51
                  ITROUV = ITROUV + 1
                  VALR = ZR(JVALE+N-1)
                  CTYPE = 'R'
 51            CONTINUE
            ELSEIF ( TYPE(1:1) .EQ. 'C' ) THEN
               DO 52 K = 1 , NBPU
                  N = ZI(JNUMI+K-1)
                  IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 52
                  ITROUV = ITROUV + 1
                  VALC = ZC(JVALE+N-1)
                  CTYPE = 'C'
 52            CONTINUE
            ELSEIF ( TYPE(1:3) .EQ. 'K80' ) THEN
               K8 = K8 + 1
               DO 53 K = 1 , NBPU
                  N = ZI(JNUMI+K-1)
                  IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 53
                  ITROUV = ITROUV + 1
                  VALK = ZK80(JVALE+N-1)
                  CTYPE = 'K'
 53            CONTINUE
            ELSEIF ( TYPE(1:3) .EQ. 'K32' ) THEN
               DO 54 K = 1 , NBPU
                  N = ZI(JNUMI+K-1)
                  IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 54
                  ITROUV = ITROUV + 1
                  VALK = ZK32(JVALE+N-1)
                  CTYPE = 'K'
 54            CONTINUE
            ELSEIF ( TYPE(1:3) .EQ. 'K24' ) THEN
               DO 55 K = 1 , NBPU
                  N = ZI(JNUMI+K-1)
                  IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 55
                  ITROUV = ITROUV + 1
                  VALK = ZK24(JVALE+N-1)
                  CTYPE = 'K'
 55            CONTINUE
            ELSEIF ( TYPE(1:3) .EQ. 'K16' ) THEN
               DO 56 K = 1 , NBPU
                  N = ZI(JNUMI+K-1)
                  IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 56
                  ITROUV = ITROUV + 1
                  VALK = ZK16(JVALE+N-1)
                  CTYPE = 'K'
 56            CONTINUE
            ELSEIF ( TYPE(1:2) .EQ. 'K8' ) THEN
               DO 57 K = 1 , NBPU
                  N = ZI(JNUMI+K-1)
                  IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 57
                  ITROUV = ITROUV + 1
                  VALK = ZK8(JVALE+N-1)
                  CTYPE = 'K'
 57            CONTINUE
            ENDIF
         ENDIF
 40   CONTINUE
C
      IF ( ITROUV .EQ. 0 ) IER = 2
      IF ( ITROUV .GT. 1 ) IER = 3
C
 9999 CONTINUE
      CALL JEDETR ( '&&TBLIVA.NUMERO' )
C
      CALL JEDEMA()
      END
