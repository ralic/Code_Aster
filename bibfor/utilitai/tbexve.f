      SUBROUTINE TBEXVE ( NOMTA, PARA, NOMOBJ, BASOBJ, NBVAL, TYPVAL )
      IMPLICIT   NONE
      INTEGER             NBVAL
      CHARACTER*(*)       NOMTA, PARA, NOMOBJ, BASOBJ, TYPVAL
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C      LECTURE DE TOUTES LES VALEURS D'UNE COLONNE D'UNE TABLE
C ----------------------------------------------------------------------
C IN  : NOMTA  : NOM DE LA STRUCTURE "TABLE".
C IN  : PARA   : PARAMETRE DESIGNANT LA COLONNE A EXTRAIRE
C IN  : NOMOBJ : NOM DE L'OBJET JEVEUX CONTENANT LES VALEURS
C IN  : BASOBJ : BASE SUR LAQUELLE ON CREE LE VECTEUR
C OUT : NBVAL  : NOMBRE DE VALEURS EXTRAITES
C OUT : TYPVAL : TYPE JEVEUX DES VALEURS EXTRAITES
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
      INTEGER      IRET, NBPARA, NBLIGN, JTBNP, JTBLP, IPAR
      INTEGER      I, IV, JVALE, JVALL, KVALE
      CHARACTER*1  BASE
      CHARACTER*4  TYPE
      CHARACTER*19 NOMTAB
      CHARACTER*24 NOMJV, NOMJVL, INPAR, JNPAR
C DEB------------------------------------------------------------------
C
      CALL JEMARQ ( )
C
      NOMTAB = NOMTA
      BASE   = BASOBJ(1:1)
      INPAR  = PARA
C
C     --- VERIFICATION DE LA BASE ---
C
      IF ( BASE.NE.'V' .AND. BASE.NE.'G' ) THEN
         CALL U2MESK('F','UTILITAI2_48',1,BASE)
      ENDIF
C
C     --- VERIFICATION DE LA TABLE ---
C
      CALL JEEXIN ( NOMTAB//'.TBBA', IRET )
      IF ( IRET .EQ. 0 ) THEN
         CALL U2MESS('F','UTILITAI4_64')
      ENDIF
C
      CALL JEVEUO ( NOMTAB//'.TBNP' , 'L', JTBNP )
      NBPARA = ZI(JTBNP  )
      NBLIGN = ZI(JTBNP+1)
      IF ( NBPARA .EQ. 0 ) THEN
         CALL U2MESS('F','UTILITAI4_65')
      ENDIF
      IF ( NBLIGN .EQ. 0 ) THEN
         CALL U2MESS('F','UTILITAI4_76')
      ENDIF
C
C     --- VERIFICATION QUE LE PARAMETRE EXISTE DANS LA TABLE ---
C
      CALL JEVEUO ( NOMTAB//'.TBLP' , 'L', JTBLP )
      DO 10 IPAR = 1 , NBPARA
         JNPAR = ZK24(JTBLP+4*(IPAR-1))
         IF ( INPAR .EQ. JNPAR ) GOTO 12
 10      CONTINUE
         CALL UTDEBM('F','TBEXVE','ERREUR DANS LES DONNEES')
         CALL UTIMPK('L','PARAMETRE N''EXISTE PAS: ',1,INPAR)
         CALL UTFINM( )
 12   CONTINUE
C
      TYPE   = ZK24(JTBLP+4*(IPAR-1)+1)
      NOMJV  = ZK24(JTBLP+4*(IPAR-1)+2)
      NOMJVL = ZK24(JTBLP+4*(IPAR-1)+3)
C
      CALL JEVEUO ( NOMJV , 'L', JVALE )
      CALL JEVEUO ( NOMJVL, 'L', JVALL )
      NBVAL = 0
      DO 20 I = 1 , NBLIGN
         IF ( ZI(JVALL+I-1).EQ.1 ) NBVAL = NBVAL + 1
 20   CONTINUE
C
      IV = 0
      IF     ( TYPE(1:1) .EQ. 'I'   ) THEN
         CALL WKVECT( NOMOBJ, BASE//' V I', NBVAL, KVALE )
         DO 100 I = 1 , NBLIGN
            IF ( ZI(JVALL+I-1).EQ.1 ) THEN
               IV = IV + 1
               ZI(KVALE+IV-1) = ZI(JVALE+I-1)
            ENDIF
 100     CONTINUE
C
      ELSEIF ( TYPE(1:1) .EQ. 'R'   ) THEN
         CALL WKVECT( NOMOBJ, BASE//' V R', NBVAL, KVALE )
         DO 200 I = 1 , NBLIGN
            IF ( ZI(JVALL+I-1).EQ.1 ) THEN
               IV = IV + 1
               ZR(KVALE+IV-1) = ZR(JVALE+I-1)
            ENDIF
 200     CONTINUE
C
      ELSEIF ( TYPE(1:1) .EQ. 'C'   ) THEN
         CALL WKVECT( NOMOBJ, BASE//' V C', NBVAL, KVALE )
         DO 300 I = 1 , NBLIGN
            IF ( ZI(JVALL+I-1).EQ.1 ) THEN
               IV = IV + 1
               ZC(KVALE+IV-1) = ZC(JVALE+I-1)
            ENDIF
 300     CONTINUE
C
      ELSEIF ( TYPE(1:3) .EQ. 'K80' ) THEN
         CALL WKVECT( NOMOBJ, BASE//' V K80', NBVAL, KVALE )
         DO 400 I = 1 , NBLIGN
            IF ( ZI(JVALL+I-1).EQ.1 ) THEN
               IV = IV + 1
               ZK80(KVALE+IV-1) = ZK80(JVALE+I-1)
            ENDIF
 400     CONTINUE
C
      ELSEIF ( TYPE(1:3) .EQ. 'K32' ) THEN
         CALL WKVECT( NOMOBJ, BASE//' V K32', NBVAL, KVALE )
         DO 500 I = 1 , NBLIGN
            IF ( ZI(JVALL+I-1).EQ.1 ) THEN
               IV = IV + 1
               ZK32(KVALE+IV-1) = ZK32(JVALE+I-1)
            ENDIF
 500     CONTINUE
C
      ELSEIF ( TYPE(1:3) .EQ. 'K24' ) THEN
         CALL WKVECT( NOMOBJ, BASE//' V K24', NBVAL, KVALE )
         DO 600 I = 1 , NBLIGN
            IF ( ZI(JVALL+I-1).EQ.1 ) THEN
               IV = IV + 1
               ZK24(KVALE+IV-1) = ZK24(JVALE+I-1)
            ENDIF
 600     CONTINUE
C
      ELSEIF ( TYPE(1:3) .EQ. 'K16' ) THEN
         CALL WKVECT( NOMOBJ, BASE//' V K16', NBVAL, KVALE )
         DO 700 I = 1 , NBLIGN
            IF ( ZI(JVALL+I-1).EQ.1 ) THEN
               IV = IV + 1
               ZK16(KVALE+IV-1) = ZK16(JVALE+I-1)
            ENDIF
 700     CONTINUE
C
      ELSEIF ( TYPE(1:2) .EQ. 'K8'  ) THEN
         CALL WKVECT( NOMOBJ, BASE//' V K8', NBVAL, KVALE )
         DO 800 I = 1 , NBLIGN
            IF ( ZI(JVALL+I-1).EQ.1 ) THEN
               IV = IV + 1
               ZK8(KVALE+IV-1) = ZK8(JVALE+I-1)
            ENDIF
 800     CONTINUE
      ENDIF
C
      TYPVAL = TYPE
      NBVAL  = IV
      CALL JEECRA ( NOMOBJ , 'LONUTI' , NBVAL , ' ' )
C
      CALL JEDEMA()
      END
