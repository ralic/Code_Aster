      SUBROUTINE TBEXT2 ( TABIN, BASOUT, TABOUT, NPACRI, LIPACR, 
     +                    VI, VR, VC, VK )
      IMPLICIT   NONE
      INTEGER             NPACRI, VI(*)
      REAL*8              VR(*)
      COMPLEX*16          VC(*)
      CHARACTER*(*)       TABIN,BASOUT,TABOUT,LIPACR(*), VK(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 09/12/98   AUTEUR CIBHHLV L.VIVAN 
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
C ----------------------------------------------------------------------
C IN  : TABIN  : NOM DE LA TABLE DONT ON VEUT EXTRAIRE DES LIGNES
C IN  : BASOUT : BASE DE CREATION DE "TABOUT"
C IN  : TABOUT : NOM DE LA TABLE QUI CONTIENDRA LES LIGNES EXTRAITES
C IN  : NPACRI : NOMBRE DE PARAMETRES IMPLIQUES DANS LES CRITERES
C IN  : LIPACR : LISTE DES PARAMETRES CRITERES
C IN  : VI     : LISTE DES CRITERES POUR LES PARAMETRES "I"
C IN  : VR     : LISTE DES CRITERES POUR LES PARAMETRES "R"
C IN  : VC     : LISTE DES CRITERES POUR LES PARAMETRES "C"
C IN  : VK     : LISTE DES CRITERES POUR LES PARAMETRES "K"
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
      INTEGER      IRET, NBPARA, NBLIGN, JTBNP, NBPU, JNUMI, NBPAR2
      INTEGER      JTBLP, I, J, K, N, JVALE, ITROUV, JTYPE
      INTEGER      KI, KR, KC, KK, JVALL, JPARR, NBP
      INTEGER      JVALI, JVALR, JVALC, JVALK
      CHARACTER*1  BASE
      CHARACTER*4  TYPE
      CHARACTER*19 NOMTAB
      CHARACTER*24 NOMJV, NOMJVL, INPAR, JNPAR
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      NOMTAB = TABIN
      BASE   = BASOUT(1:1)
C
C     --- VERIFICATION DE LA BASE ---
C
      IF ( BASE.NE.'V' .AND. BASE.NE.'G' ) THEN
         CALL UTMESS('F','TBEXT2','TYPE BASE INCONNU :'//BASE)
      ENDIF
C
C     --- VERIFICATION DE LA TABLE ---
C
      CALL JEEXIN ( NOMTAB//'.TBBA', IRET )
      IF ( IRET .EQ. 0 ) THEN
         CALL UTMESS('F','TBEXT2','LA TABLE N''EXISTE PAS') 
      ENDIF
C
      CALL JEVEUO ( NOMTAB//'.TBNP' , 'E', JTBNP )
      NBPARA = ZI(JTBNP  )
      NBLIGN = ZI(JTBNP+1)
      IF ( NBPARA .EQ. 0 ) THEN
         CALL UTMESS('F','TBEXT2','PAS DE PARAMETRES DEFINIS') 
      ENDIF
      IF ( NBLIGN .EQ. 0 ) THEN
         CALL UTMESS('F','TBEXT2','PAS DE LIGNES DEFINIS') 
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
         CALL UTDEBM('F','TBEXT2','ERREUR DANS LES DONNEES') 
         CALL UTIMPK('L','PARAMETRE N''EXISTE PAS: ',1,INPAR)
         CALL UTFINM( )
 10   CONTINUE
C
      NBPU = NBLIGN
      CALL WKVECT ( '&&TBEXT2.NUMERO', 'V V I', NBPU, JNUMI)
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
                     IF ( .NOT. ZL(JVALL+N-1) ) GOTO 30
                     IF ( ZI(JVALE+N-1) .EQ. VI(KI) ) THEN
                        ITROUV = ITROUV + 1
                        ZI(JNUMI+ITROUV-1) = N
                     ENDIF
 30               CONTINUE
                  GOTO 24
               ELSEIF ( TYPE(1:1) .EQ. 'R' ) THEN
                  KR = KR + 1
                  DO 31 K = 1 , NBPU
                     N = ZI(JNUMI+K-1)
                     ZI(JNUMI+K-1) = 0
                     IF ( .NOT. ZL(JVALL+N-1) ) GOTO 31
                     IF ( VR(KR) .EQ. ZR(JVALE+N-1) ) THEN
                        ITROUV = ITROUV + 1
                        ZI(JNUMI+ITROUV-1) = N
                     ENDIF
 31               CONTINUE
                  GOTO 24
               ELSEIF ( TYPE(1:1) .EQ. 'C' ) THEN
                  KC = KC + 1
                  DO 32 K = 1 , NBPU
                     N = ZI(JNUMI+K-1)
                     ZI(JNUMI+K-1) = 0
                     IF ( .NOT. ZL(JVALL+N-1) ) GOTO 32
                     IF ( ZC(JVALE+N-1) .EQ. VC(KC) ) THEN
                        ITROUV = ITROUV + 1
                        ZI(JNUMI+ITROUV-1) = N
                     ENDIF
 32               CONTINUE
                  GOTO 24
               ELSEIF ( TYPE(1:3) .EQ. 'K80' ) THEN
                  KK = KK + 1
                  DO 33 K = 1 , NBPU
                     N = ZI(JNUMI+K-1)
                     ZI(JNUMI+K-1) = 0
                     IF ( .NOT. ZL(JVALL+N-1) ) GOTO 33
                     IF ( ZK80(JVALE+N-1) .EQ. VK(KK) ) THEN
                        ITROUV = ITROUV + 1
                        ZI(JNUMI+ITROUV-1) = N
                     ENDIF
 33               CONTINUE
                  GOTO 24
               ELSEIF ( TYPE(1:3) .EQ. 'K32' ) THEN
                  KK = KK + 1
                  DO 34 K = 1 , NBPU
                     N = ZI(JNUMI+K-1)
                     ZI(JNUMI+K-1) = 0
                     IF ( .NOT. ZL(JVALL+N-1) ) GOTO 34
                     IF ( ZK32(JVALE+N-1) .EQ. VK(KK) ) THEN
                        ITROUV = ITROUV + 1
                        ZI(JNUMI+ITROUV-1) = N
                     ENDIF
 34               CONTINUE
                  GOTO 24
               ELSEIF ( TYPE(1:3) .EQ. 'K24' ) THEN
                  KK = KK + 1
                  DO 35 K = 1 , NBPU
                     N = ZI(JNUMI+K-1)
                     ZI(JNUMI+K-1) = 0
                     IF ( .NOT. ZL(JVALL+N-1) ) GOTO 35
                     IF ( ZK24(JVALE+N-1) .EQ. VK(KK) ) THEN
                        ITROUV = ITROUV + 1
                        ZI(JNUMI+ITROUV-1) = N
                     ENDIF
 35               CONTINUE
                  GOTO 24
               ELSEIF ( TYPE(1:3) .EQ. 'K16' ) THEN
                  KK = KK + 1
                  DO 36 K = 1 , NBPU
                     N = ZI(JNUMI+K-1)
                     ZI(JNUMI+K-1) = 0
                     IF ( .NOT. ZL(JVALL+N-1) ) GOTO 36
                     IF ( ZK16(JVALE+N-1) .EQ. VK(KK) ) THEN
                        ITROUV = ITROUV + 1
                        ZI(JNUMI+ITROUV-1) = N
                     ENDIF
 36               CONTINUE
                  GOTO 24
               ELSEIF ( TYPE(1:2) .EQ. 'K8' ) THEN
                  KK = KK + 1
                  DO 37 K = 1 , NBPU
                     N = ZI(JNUMI+K-1)
                     ZI(JNUMI+K-1) = 0
                     IF ( .NOT. ZL(JVALL+N-1) ) GOTO 37
                     IF ( ZK8(JVALE+N-1) .EQ. VK(KK) ) THEN
                        ITROUV = ITROUV + 1
                        ZI(JNUMI+ITROUV-1) = N
                     ENDIF
 37               CONTINUE
                  GOTO 24
               ENDIF
            ENDIF
 22      CONTINUE
 24      CONTINUE
         IF ( ITROUV .EQ. 0 ) THEN
            CALL UTMESS('F','TBEXT2','PAS DE LIGNES TROUVEES') 
         ENDIF
         NBPU = ITROUV
 20   CONTINUE
C
C     --- ON RECUPERE LES PARAMETRES ET LEURS TYPES  ---
C
      CALL WKVECT ( '&&TBEXT2.TYPE_R', 'V V K8' , NBPARA, JTYPE)
      CALL WKVECT ( '&&TBEXT2.PARA_R', 'V V K24', NBPARA, JPARR)
      NBPAR2 = 0
      DO 38 I = 1 , NBPARA
         DO 39 J = 1 , NPACRI
            INPAR = LIPACR(J)
            IF ( ZK24(JTBLP+4*(I-1)) .EQ. INPAR ) GOTO 38
 39      CONTINUE
         NBPAR2 = NBPAR2 + 1
         ZK24(JPARR+NBPAR2-1) = ZK24(JTBLP+4*(I-1))
         ZK8 (JTYPE+NBPAR2-1) = ZK24(JTBLP+4*(I-1)+1)
 38   CONTINUE
C
C     --- CREATION DE LA TABLE ---
C
      CALL TBCRSD ( TABOUT , BASOUT )
      CALL TBAJPA ( TABOUT, NBPAR2, ZK24(JPARR), ZK8(JTYPE) )
      CALL WKVECT ( '&&TBEXT2.VALE_I', 'V V I'  , NBPAR2, JVALI)
      CALL WKVECT ( '&&TBEXT2.VALE_R', 'V V R'  , NBPAR2, JVALR)
      CALL WKVECT ( '&&TBEXT2.VALE_C', 'V V C'  , NBPAR2, JVALC)
      CALL WKVECT ( '&&TBEXT2.VALE_K', 'V V K80', NBPAR2, JVALK)
      DO 40 K = 1 , NBPU
         KI = 0
         KR = 0
         KC = 0
         KK = 0
         N = ZI(JNUMI+K-1)
         NBP = 0
         DO 42 J = 1 , NBPARA
            JNPAR  = ZK24(JTBLP+4*(J-1))
            DO 43 I = 1 , NPACRI
               IF ( JNPAR .EQ. LIPACR(I) ) GOTO 42
 43         CONTINUE
            TYPE   = ZK24(JTBLP+4*(J-1)+1)
            NOMJV  = ZK24(JTBLP+4*(J-1)+2)
            NOMJVL = ZK24(JTBLP+4*(J-1)+3)
            CALL JEVEUO ( NOMJV , 'L', JVALE )
            CALL JEVEUO ( NOMJVL, 'L', JVALL )
            IF ( .NOT. ZL(JVALL+N-1) ) GOTO 42
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
     +                 ZR(JVALR), ZC(JVALC), ZK80(JVALK), 0 )
 40   CONTINUE
C
 9999 CONTINUE
      CALL JEDETR ( '&&TBEXT2.NUMERO' )
      CALL JEDETR ( '&&TBEXT2.TYPE_R' )
      CALL JEDETR ( '&&TBEXT2.PARA_R' )
      CALL JEDETR ( '&&TBEXT2.VALE_I' )
      CALL JEDETR ( '&&TBEXT2.VALE_R' )
      CALL JEDETR ( '&&TBEXT2.VALE_C' )
      CALL JEDETR ( '&&TBEXT2.VALE_K' )
C
      CALL JEDEMA()
      END
