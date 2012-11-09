      SUBROUTINE TBFUTB ( TABOUT, BASOUT, NTAB, LTABIN, PARA, TYPPAR,
     &                    VI, VR, VC, VK )
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'

      INTEGER             NTAB, VI(*)
      REAL*8              VR(*)
      COMPLEX*16          VC(*)
      CHARACTER*(*)       TABOUT, BASOUT, LTABIN(*), PARA, TYPPAR, VK(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C     FUSIONNER PLUSIEURS TABLES EN UNE SEULE TABLE.
C ----------------------------------------------------------------------
C IN  : TABOUT : NOM DE LA TABLE QUE L'ON VEUT OBTENIR
C IN  : BASOUT : BASE DE CREATION DE "TABOUT"
C IN  : NTAB   : NOMBRE DE TABLES QUE L'ON VEUT FUSIONNER
C IN  : LTABIN : NOMS DES TABLES QUE L'ON VEUT FUSIONNER
C IN  : PARA   : NOUVEAU PARAMETRE NECESSAIRE
C IN  : TYPPAR : TYPE DU NOUVEAU PARAMETRE
C IN  : VI     : LISTE DES CRITERES POUR LES PARAMETRES "I"
C IN  : VR     : LISTE DES CRITERES POUR LES PARAMETRES "R"
C IN  : VC     : LISTE DES CRITERES POUR LES PARAMETRES "C"
C IN  : VK     : LISTE DES CRITERES POUR LES PARAMETRES "K"
C ----------------------------------------------------------------------
C ----------------------------------------------------------------------
      INTEGER      IRET, NBPARA, NBLIGN, JTBNP, NBPU, NBPART, IPAR
      INTEGER      JTBLP, I, J, K, JVALE, JTYPE
      INTEGER      KI, KR, KC, KK, JVALL, JPARR
      INTEGER      JVALI, JVALR, JVALC, JVALK
      CHARACTER*1  BASE
      CHARACTER*4  TYPE, KTYPE
      CHARACTER*19 NOMTAB
      CHARACTER*24 NOMJV, NOMJVL, INPAR, JNPAR, KNPAR
      CHARACTER*24 VALK(3)
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      BASE   = BASOUT(1:1)
C
C     --- VERIFICATION DE LA BASE ---
C
      CALL ASSERT ( BASE.EQ.'V' .OR. BASE.EQ.'G' )
C
C     --- VERIFICATION DES TABLES ---
C
      INPAR = PARA
      NBPART = 0
      NBPU = 0
      DO 10 I = 1 , NTAB
         NOMTAB = LTABIN(I)
         CALL JEEXIN ( NOMTAB//'.TBBA', IRET )
         IF ( IRET .EQ. 0 ) THEN
            CALL U2MESS('F','UTILITAI4_64')
         ENDIF
C
         CALL JEVEUO ( NOMTAB//'.TBNP' , 'L', JTBNP )
         NBPARA = ZI(JTBNP  )
         NBLIGN = ZI(JTBNP+1)
         NBPART = NBPART + NBPARA
         NBPU = MAX ( NBPU , NBLIGN )
         IF ( NBPARA .EQ. 0 ) THEN
            CALL U2MESS('F','UTILITAI4_65')
         ENDIF
         IF ( NBLIGN .EQ. 0 ) THEN
            CALL U2MESS('F','UTILITAI4_66')
         ENDIF
C
         CALL JEVEUO ( NOMTAB//'.TBLP' , 'L', JTBLP )
         DO 12 J = 1 , NBPARA
            JNPAR = ZK24(JTBLP+4*(J-1))
            IF ( INPAR .EQ. JNPAR ) THEN
               VALK (1) = JNPAR
               VALK (2) = NOMTAB
               CALL U2MESG('F','UTILITAI8_20',2,VALK,0,0,0,0.D0)
            ENDIF
 12      CONTINUE
C
 10   CONTINUE
C
C     --- ON ELIMINE LES PARAMETRES DOUBLONS ---
C
      NBPART = NBPART + 1
      CALL WKVECT ( '&&TBFUTB.TYPE_R', 'V V K8' , NBPART, JTYPE)
      CALL WKVECT ( '&&TBFUTB.PARA_R', 'V V K24', NBPART, JPARR)
      IPAR = 1
      IF (PARA(1:1).NE.' ') THEN
         ZK24(JPARR) = PARA
         ZK8(JTYPE) = TYPPAR
      ELSE
         ZK24(JPARR) = ZK24(JTBLP)
         ZK8(JTYPE) = ZK24(JTBLP+1)
      END IF
      DO 20 I = 1 , NTAB
         NOMTAB = LTABIN(I)
         CALL JEVEUO ( NOMTAB//'.TBNP' , 'L', JTBNP )
         CALL JEVEUO ( NOMTAB//'.TBLP' , 'L', JTBLP )
         NBPARA = ZI(JTBNP  )
         DO 22 J = 1 , NBPARA
            JNPAR = ZK24(JTBLP+4*(J-1))
            TYPE  = ZK24(JTBLP+4*(J-1)+1)
            DO 24 K = 1 , IPAR
               KNPAR = ZK24(JPARR+K-1)
               KTYPE = ZK8(JTYPE+K-1)
               IF ( KNPAR .EQ. JNPAR ) THEN
                 IF (TYPE .NE. KTYPE ) THEN
                   VALK (1) = JNPAR
                   VALK (2) = JNPAR
                   VALK (3) = KNPAR
                   CALL U2MESG('F','UTILITAI8_21',3,VALK,0,0,0,0.D0)
                 ENDIF
                 GOTO 22
               ENDIF
 24         CONTINUE
            IPAR = IPAR + 1
            ZK24(JPARR+IPAR-1) = JNPAR
            ZK8(JTYPE+IPAR-1) = TYPE
 22      CONTINUE
 20   CONTINUE
      NBPART = IPAR
C
C     --- CREATION DE LA TABLE ---
C
      CALL TBCRSD ( TABOUT , BASOUT )
      CALL TBAJPA ( TABOUT, NBPART, ZK24(JPARR), ZK8(JTYPE) )
      CALL WKVECT ( '&&TBFUTB.VALE_I', 'V V I', NBPU, JVALI)
      CALL WKVECT ( '&&TBFUTB.VALE_R', 'V V R', NBPU, JVALR)
      CALL WKVECT ( '&&TBFUTB.VALE_C', 'V V C', NBPU, JVALC)
      CALL WKVECT ( '&&TBFUTB.VALE_K', 'V V K80', NBPU, JVALK)
      DO 30 I = 1 , NTAB
         NOMTAB = LTABIN(I)
         CALL JEVEUO ( NOMTAB//'.TBNP' , 'L', JTBNP )
         CALL JEVEUO ( NOMTAB//'.TBLP' , 'L', JTBLP )
         NBPARA = ZI(JTBNP  )
         NBLIGN = ZI(JTBNP+1)
         DO 40 K = 1 , NBLIGN
            KI = 0
            KR = 0
            KC = 0
            KK = 0
            IF (PARA.NE.' ') THEN
              IPAR = 1
              ZK24(JPARR) = PARA
            ELSE
              IPAR = 0
            END IF
            IF ( TYPPAR(1:1) .EQ. 'I' ) THEN
               KI = KI + 1
               ZI(JVALI+KI-1) = VI(I)
            ELSEIF ( TYPPAR(1:1) .EQ. 'R' ) THEN
               KR = KR + 1
               ZR(JVALR+KR-1) = VR(I)
            ELSEIF ( TYPPAR(1:1) .EQ. 'C' ) THEN
               KC = KC + 1
               ZC(JVALC+KC-1) = VC(I)
            ELSEIF ( TYPPAR(1:3) .EQ. 'K80' ) THEN
               KK = KK + 1
               ZK80(JVALK+KK-1) = VK(I)
            ELSEIF ( TYPPAR(1:3) .EQ. 'K32' ) THEN
               KK = KK + 1
               ZK80(JVALK+KK-1) = VK(I)
            ELSEIF ( TYPPAR(1:3) .EQ. 'K24' ) THEN
               KK = KK + 1
               ZK80(JVALK+KK-1) = VK(I)
            ELSEIF ( TYPPAR(1:3) .EQ. 'K16' ) THEN
               KK = KK + 1
               ZK80(JVALK+KK-1) = VK(I)
            ELSEIF ( TYPPAR(1:2) .EQ. 'K8'  ) THEN
               KK = KK + 1
               ZK80(JVALK+KK-1) = VK(I)
            ENDIF
            DO 42 J = 1 , NBPARA
               JNPAR  = ZK24(JTBLP+4*(J-1))
               TYPE   = ZK24(JTBLP+4*(J-1)+1)
               NOMJV  = ZK24(JTBLP+4*(J-1)+2)
               NOMJVL = ZK24(JTBLP+4*(J-1)+3)
               CALL JEVEUO ( NOMJV , 'L', JVALE )
               CALL JEVEUO ( NOMJVL, 'L', JVALL )
               IF ( ZI(JVALL+K-1).EQ.0 ) GOTO 42
               IPAR = IPAR + 1
               ZK24(JPARR+IPAR-1) = JNPAR
               IF ( TYPE(1:1) .EQ. 'I' ) THEN
                  KI = KI + 1
                  ZI(JVALI+KI-1) = ZI(JVALE+K-1)
               ELSEIF ( TYPE(1:1) .EQ. 'R' ) THEN
                  KR = KR + 1
                  ZR(JVALR+KR-1) = ZR(JVALE+K-1)
               ELSEIF ( TYPE(1:1) .EQ. 'C' ) THEN
                  KC = KC + 1
                  ZC(JVALC+KC-1) = ZC(JVALE+K-1)
               ELSEIF ( TYPE(1:3) .EQ. 'K80' ) THEN
                  KK = KK + 1
                  ZK80(JVALK+KK-1) = ZK80(JVALE+K-1)
               ELSEIF ( TYPE(1:3) .EQ. 'K32' ) THEN
                  KK = KK + 1
                  ZK80(JVALK+KK-1) = ZK32(JVALE+K-1)
               ELSEIF ( TYPE(1:3) .EQ. 'K24' ) THEN
                  KK = KK + 1
                  ZK80(JVALK+KK-1) = ZK24(JVALE+K-1)
               ELSEIF ( TYPE(1:3) .EQ. 'K16' ) THEN
                  KK = KK + 1
                  ZK80(JVALK+KK-1) = ZK16(JVALE+K-1)
               ELSEIF ( TYPE(1:2) .EQ. 'K8'  ) THEN
                  KK = KK + 1
                  ZK80(JVALK+KK-1) = ZK8(JVALE+K-1)
               ENDIF
 42         CONTINUE
            CALL TBAJLI ( TABOUT, IPAR, ZK24(JPARR), ZI(JVALI),
     &                  ZR(JVALR), ZC(JVALC), ZK80(JVALK), 0 )
 40      CONTINUE
 30   CONTINUE


C
      CALL JEDETR ( '&&TBFUTB.TYPE_R' )
      CALL JEDETR ( '&&TBFUTB.PARA_R' )
      CALL JEDETR ( '&&TBFUTB.VALE_I' )
      CALL JEDETR ( '&&TBFUTB.VALE_R' )
      CALL JEDETR ( '&&TBFUTB.VALE_C' )
      CALL JEDETR ( '&&TBFUTB.VALE_K' )
C
      CALL JEDEMA()
      END
