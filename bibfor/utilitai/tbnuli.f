      SUBROUTINE TBNULI ( TABIN, NPACRI, LIPACR,
     &                    VI, VR, VC, VK, LPREC, LCRIT, NUME )
      IMPLICIT   NONE
      INTEGER             NPACRI, VI(*), NUME
      REAL*8              VR(*), LPREC(*)
      COMPLEX*16          VC(*)
      CHARACTER*(*)       TABIN, LIPACR(*), VK(*), LCRIT(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 20/02/2007   AUTEUR LEBOUVIER F.LEBOUVIER 
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
C     RECUPERATION D'UN NUMERO DE LIGNE
C ----------------------------------------------------------------------
C IN  : TABIN  : NOM DE LA TABLE DONT ON VEUT RECUPERER UNE LIGNE
C IN  : NPACRI : NOMBRE DE PARAMETRES IMPLIQUES DANS LES CRITERES
C IN  : LIPACR : LISTE DES PARAMETRES CRITERES
C IN  : VI     : LISTE DES CRITERES POUR LES PARAMETRES "I"
C IN  : VR     : LISTE DES CRITERES POUR LES PARAMETRES "R"
C IN  : VC     : LISTE DES CRITERES POUR LES PARAMETRES "C"
C IN  : VK     : LISTE DES CRITERES POUR LES PARAMETRES "K"
C IN  : LPREC  : PRECISION POUR LES PARAMETRES "R"
C IN  : LCRIT  : CRITERE POUR LES PARAMETRES "R"
C OUT : NUME   : = 0 , LA LIGNE N'A PAS PU ETRE RECUPERE
C                = I , ON A RECUPERE LA LIGNE
C                < 0 , PLUSIEURS LIGNES RECUPEREES
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
      INTEGER      IRET, NBPARA, NBLIGN, JTBNP, NBPU, JNUMI, JTBLP, I,
     &             J, K, N, JVALE, ITROUV, KI, KR, KC, KK, JVALL
      REAL*8       PREC, REFR
      CHARACTER*4  TYPE, CRIT
      CHARACTER*8  K8B
      CHARACTER*19 NOMTAB
      CHARACTER*24 NOMJV, NOMJVL, INPAR, JNPAR
      CHARACTER*24 VALK
      LOGICAL      LOK
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      NUME = 0
      NOMTAB = TABIN
C
C     --- VERIFICATION DE LA TABLE ---
C
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
      IF ( NBLIGN .EQ. 0 ) GOTO 9999
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
         VALK = INPAR
         CALL U2MESG('F', 'UTILITAI7_1',1,VALK,0,0,0,0.D0)
 10   CONTINUE
C
      NOMJV = ZK24(JTBLP+2)
      CALL JELIRA ( NOMJV, 'LONUTI', NBPU, K8B)
      CALL WKVECT ( '&&TBNULI.NUMERO', 'V V I', NBPU, JNUMI)
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
                     ZI(JNUMI+K-1) = 0
                     IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 30
                     IF ( ZI(JVALE+N-1) .EQ. VI(KI) ) THEN
                        ITROUV = ITROUV + 1
                        ZI(JNUMI+ITROUV-1) = N
                     ENDIF
 30               CONTINUE
               ELSEIF ( TYPE(1:1) .EQ. 'R' ) THEN
                  KR = KR + 1
                  PREC  = LPREC(KR)
                  CRIT  = LCRIT(KR)
                  DO 31 K = 1 , NBPU
                     N = ZI(JNUMI+K-1)
                     ZI(JNUMI+K-1) = 0
                     IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 31
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
 31               CONTINUE
               ELSEIF ( TYPE(1:1) .EQ. 'C' ) THEN
                  KC = KC + 1
                  DO 32 K = 1 , NBPU
                     N = ZI(JNUMI+K-1)
                     ZI(JNUMI+K-1) = 0
                     IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 32
                     IF ( ZC(JVALE+N-1) .EQ. VC(KC) ) THEN
                        ITROUV = ITROUV + 1
                        ZI(JNUMI+ITROUV-1) = N
                     ENDIF
 32               CONTINUE
               ELSEIF ( TYPE(1:3) .EQ. 'K80' ) THEN
                  KK = KK + 1
                  DO 33 K = 1 , NBPU
                     N = ZI(JNUMI+K-1)
                     ZI(JNUMI+K-1) = 0
                     IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 33
                     IF ( ZK80(JVALE+N-1) .EQ. VK(KK) ) THEN
                        ITROUV = ITROUV + 1
                        ZI(JNUMI+ITROUV-1) = N
                     ENDIF
 33               CONTINUE
               ELSEIF ( TYPE(1:3) .EQ. 'K32' ) THEN
                  KK = KK + 1
                  DO 34 K = 1 , NBPU
                     N = ZI(JNUMI+K-1)
                     ZI(JNUMI+K-1) = 0
                     IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 34
                     IF ( ZK32(JVALE+N-1) .EQ. VK(KK) ) THEN
                        ITROUV = ITROUV + 1
                        ZI(JNUMI+ITROUV-1) = N
                     ENDIF
 34               CONTINUE
               ELSEIF ( TYPE(1:3) .EQ. 'K24' ) THEN
                  KK = KK + 1
                  DO 35 K = 1 , NBPU
                     N = ZI(JNUMI+K-1)
                     ZI(JNUMI+K-1) = 0
                     IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 35
                     IF ( ZK24(JVALE+N-1) .EQ. VK(KK) ) THEN
                        ITROUV = ITROUV + 1
                        ZI(JNUMI+ITROUV-1) = N
                     ENDIF
 35               CONTINUE
               ELSEIF ( TYPE(1:3) .EQ. 'K16' ) THEN
                  KK = KK + 1
                  DO 36 K = 1 , NBPU
                     N = ZI(JNUMI+K-1)
                     ZI(JNUMI+K-1) = 0
                     IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 36
                     IF ( ZK16(JVALE+N-1) .EQ. VK(KK) ) THEN
                        ITROUV = ITROUV + 1
                        ZI(JNUMI+ITROUV-1) = N
                     ENDIF
 36               CONTINUE
               ELSEIF ( TYPE(1:2) .EQ. 'K8' ) THEN
                  KK = KK + 1
                  DO 37 K = 1 , NBPU
                     N = ZI(JNUMI+K-1)
                     ZI(JNUMI+K-1) = 0
                     IF ( ZI(JVALL+N-1).EQ.0 ) GOTO 37
                     IF ( ZK8(JVALE+N-1) .EQ. VK(KK) ) THEN
                        ITROUV = ITROUV + 1
                        ZI(JNUMI+ITROUV-1) = N
                     ENDIF
 37               CONTINUE
               ENDIF
            ENDIF
 22      CONTINUE
         NBPU = ITROUV
 20   CONTINUE
C
      IF ( NBPU .EQ. 1 ) THEN
         NUME = ZI(JNUMI)
      ELSEIF ( NBPU .GT. 1 ) THEN
         NUME = -NBPU
      ENDIF
C
      CALL JEDETR ( '&&TBNULI.NUMERO' )
C
 9999 CONTINUE
      CALL JEDEMA()
      END
