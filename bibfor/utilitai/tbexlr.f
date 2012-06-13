      SUBROUTINE TBEXLR ( NOMTA, LISTR, BASOUT )
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'
      CHARACTER*(*)       NOMTA, LISTR, BASOUT
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C     TRANSFORMER UNE TABLE EN LISTR8 QUE SI CETTE TABLE EST
C                 "DIAGONALISABLE PAR BLOCS"
C ----------------------------------------------------------------------
C IN  : NOMTA  : NOM DE LA SD "TABLE".
C IN  : LISTR  : NOM DE LA SD "LISTR8" RESULTAT
C IN  : BASOUT : BASE DE CREATION DE "LISTR"
C ----------------------------------------------------------------------
C ----------------------------------------------------------------------
      INTEGER      IRET, NBPARA, NBLIGN, JTBNP, KPARA, NBPR, NBLG, IPAR,
     &             NDIM, JTBLP, I, J, JVALE, JLOGQ, KLIGN, NBVALE, K1,
     &             KCOL, KLIG, IDEB1, IDEB2, IFIN1, IFIN2, NBCL, IVIDE,
     &             ILIG, IBLOC, JPAS, JNBP, JBOR, K, JCOL, JLIG,
     &             KLIS, KCOL1, KCOL2
      CHARACTER*1  BASE
      CHARACTER*3  TYPE
      CHARACTER*19 NOMTAB, LISTR8
      CHARACTER*24 NOMJV, NOMJVL
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      NOMTAB = ' '
      NOMTAB = NOMTA
      CALL JEEXIN ( NOMTAB//'.TBBA', IRET )
      IF ( IRET .EQ. 0 ) THEN
         CALL U2MESS('F','UTILITAI4_64')
      ENDIF
      IF ( NOMTAB(18:19) .NE. '  ' ) THEN
         CALL U2MESS('F','UTILITAI4_68')
      ENDIF
      BASE = BASOUT(1:1)
C
      CALL JEVEUO ( NOMTAB//'.TBNP' , 'E', JTBNP )
      NBPARA = ZI(JTBNP  )
      NBLIGN = ZI(JTBNP+1)
      IF ( NBPARA .EQ. 0 ) THEN
         CALL U2MESS('F','UTILITAI4_65')
      ENDIF
      IF ( NBLIGN .EQ. 0 ) THEN
         CALL U2MESS('F','UTILITAI4_76')
      ENDIF
C
      CALL JEVEUO ( NOMTAB//'.TBLP' , 'L', JTBLP )
C
C     --- ON NE RETIENT QUE LES PARAMETRES DE TYPE "I" ET "R" ---
C
      CALL WKVECT ( '&&TBEXLR.NUME_PARA', 'V V I', NBPARA, KPARA )
      NBPR = 0
      DO 10 I = 1 , NBPARA
         TYPE = ZK24(JTBLP+4*(I-1)+1)
         IF ( TYPE(1:1) .EQ. 'I' ) THEN
            NBPR = NBPR + 1
            ZI(KPARA+NBPR-1) = I
         ELSEIF ( TYPE(1:1) .EQ. 'R' ) THEN
            NBPR = NBPR + 1
            ZI(KPARA+NBPR-1) = I
         ENDIF
 10   CONTINUE
      IF ( NBPR .EQ. 0 ) THEN
         CALL U2MESS('F','UTILITAI4_81')
      ENDIF
C
C     --- ON NE RETIENT QUE LES LIGNES NON VIDES ---
C
      CALL WKVECT ( '&&TBEXLR.NUME_LIGN', 'V V I', NBLIGN, KLIGN )
      NBLG = 0
      DO 20 I = 1 , NBLIGN
         NBCL = 0
         DO 22 J = 1 , NBPR
            IPAR = ZI(KPARA+J-1)
            NOMJVL = ZK24(JTBLP+4*(IPAR-1)+3)
            CALL JEVEUO ( NOMJVL, 'L', JLOGQ )
            IF ( ZI(JLOGQ+I-1).EQ.1 ) NBCL = NBCL + 1
 22      CONTINUE
         IF ( NBCL .NE. 0 ) THEN
            NBLG = NBLG + 1
            ZI(KLIGN+NBLG-1) = I
         ENDIF
 20   CONTINUE
      IF ( NBLG .EQ. 0 ) THEN
         CALL U2MESS('F','UTILITAI4_82')
      ENDIF
C
C     --- RECHERCHE DE BLOCS ---
C
      NBVALE = NBPR * NBLG
      CALL WKVECT ( '&&TBEXLR.VALE_R', 'V V R', NBVALE, KLIS )
      CALL WKVECT ( '&&TBEXLR.COLONN', 'V V I', NBPR  , JCOL )
      CALL WKVECT ( '&&TBEXLR.LIGNES', 'V V I', NBLG  , JLIG )
C
      IBLOC = 1
      K1 = 0
      DO 30 I = 1 , NBLG
         ILIG = ZI(KLIGN+I-1)
         IDEB1 = 0
         IFIN1 = NBPR
         IVIDE = 0
         KCOL1 = 0
         DO 32 J = 1 , NBPR
            IPAR = ZI(KPARA+J-1)
            TYPE   = ZK24(JTBLP+4*(IPAR-1)+1)
            NOMJV  = ZK24(JTBLP+4*(IPAR-1)+2)
            NOMJVL = ZK24(JTBLP+4*(IPAR-1)+3)
            CALL JEVEUO ( NOMJV , 'L', JVALE )
            CALL JEVEUO ( NOMJVL, 'L', JLOGQ )
            IF ( ZI(JLOGQ+ILIG-1).EQ.1 ) THEN
               IF ( IDEB1 .EQ. 0 ) IDEB1 = IPAR
               KCOL1 = KCOL1 + 1
               IF ( IVIDE .EQ. 1 ) THEN
                  CALL U2MESS('F','UTILITAI4_83')
               ENDIF
               IF ( TYPE(1:1) .EQ. 'I' ) THEN
                  K1 = K1 + 1
                  ZR(KLIS+K1-1) = ZI(JVALE+ILIG-1)
               ELSEIF ( TYPE(1:1) .EQ. 'R' ) THEN
                  K1 = K1 + 1
                  ZR(KLIS+K1-1) = ZR(JVALE+ILIG-1)
               ENDIF
            ELSE
               IF ( IDEB1 .NE. 0 ) THEN
                  IVIDE = 1
                  IF ( IFIN1 .EQ. NBPR ) IFIN1 = ZI(KPARA+J-1-1)
               ENDIF
C               IF ( IFIN1 .EQ. 0 ) IFIN1 = ZI(KPARA+J-1-1)
            ENDIF
 32      CONTINUE
         IF ( I .EQ. 1 ) THEN
            KLIG = 1
         ELSE
            IF ( IDEB1.EQ.IDEB2 .AND. IFIN1.EQ.IFIN2) THEN
               KLIG = KLIG + 1
            ELSE
C              --- NOUVEAU BLOC ---
               ZI(JCOL+IBLOC-1) = KCOL2
               ZI(JLIG+IBLOC-1) = KLIG
               IBLOC = IBLOC + 1
               KLIG = 1
            ENDIF
         ENDIF
         IDEB2 = IDEB1
         IFIN2 = IFIN1
         KCOL2 = KCOL1
 30   CONTINUE
      ZI(JCOL+IBLOC-1) = KCOL2
      ZI(JLIG+IBLOC-1) = KLIG
C
C     --- ON STOCKE ---
C
      NBVALE = 1 + ( 2 * IBLOC )
      DO 40 I = 1 , IBLOC
         KCOL = ZI(JCOL+I-1)
         KLIG = ZI(JLIG+I-1)
         NBVALE = NBVALE + ( KCOL * KLIG )
 40   CONTINUE
C
      LISTR8 = LISTR
      NDIM = MAX( 1 , NBVALE-1 )
      CALL WKVECT ( LISTR8//'.LPAS', BASE//' V R', NDIM  , JPAS  )
      CALL WKVECT ( LISTR8//'.NBPA', BASE//' V I', NDIM  , JNBP  )
      CALL WKVECT ( LISTR8//'.BINT', BASE//' V R', NBVALE, JBOR  )
      CALL WKVECT ( LISTR8//'.VALE', BASE//' V R', NBVALE, JVALE )
C
      ZR(JVALE) = IBLOC
      J = 1
      K1 = 0
      DO 50 I = 1 , IBLOC
         KCOL = ZI(JCOL+I-1)
         J = J + 1
         ZR(JVALE+J-1) = KCOL
         KLIG = ZI(JLIG+I-1)
         J = J + 1
         ZR(JVALE+J-1) = KLIG
         NDIM = KCOL * KLIG
         DO 52 K = 1 , NDIM
            K1 = K1 + 1
            J = J + 1
            ZR(JVALE+J-1) = ZR(KLIS+K1-1)
 52      CONTINUE
 50   CONTINUE
C
      DO 4 I = 1 , NBVALE-1
         ZR(JPAS+I-1) = ZR(JVALE+I) - ZR(JVALE+I-1)
         ZI(JNBP+I-1) = 1
         ZR(JBOR+I-1) = ZR(JVALE+I-1)
 4    CONTINUE
      ZR(JBOR+NBVALE-1) = ZR(JVALE+NBVALE-1)
C
      CALL JEDETR ( '&&TBEXLR.NUME_PARA' )
      CALL JEDETR ( '&&TBEXLR.NUME_LIGN' )
      CALL JEDETR ( '&&TBEXLR.VALE_R' )
      CALL JEDETR ( '&&TBEXLR.COLONN' )
      CALL JEDETR ( '&&TBEXLR.LIGNES' )
C
      CALL JEDEMA()
      END
