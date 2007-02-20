      SUBROUTINE TBTRTB ( TABIN, BASOUT, TABOUT, NPARA, LIPARA, LCRIT,
     &                    PREC, CRIT )
      IMPLICIT   NONE
      INTEGER             NPARA
      REAL*8              PREC
      CHARACTER*8         CRIT
      CHARACTER*(*)       TABIN, BASOUT, TABOUT, LIPARA(*), LCRIT(*)
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
C     TRI DE LA TABLE.
C     LE TRI NE PORTE QUE SUR LES TYPES I , R  ET  K
C     ARRET EN FATAL SUR LES COMPLEXES
C ----------------------------------------------------------------------
C IN  : TABIN  : NOM DE LA TABLE DONT ON VEUT TRIER DES LIGNES
C IN  : BASOUT : BASE DE CREATION DE "TABOUT"
C OUT : TABOUT : NOM DE LA TABLE QUI CONTIENDRA LES LIGNES TRIEES
C IN  : NPARA  : NOMBRE DE PARAMETRES IMPLIQUES DANS LE TRI
C IN  : LIPARA : LISTE DES PARAMETRES A TRIER
C IN  : LCRIT  : TYPES DE CRITERES: CR  CROISSANT
C                                   DE  DECROISSANT
C IN  : PREC   : PRECISION
C IN  : CRIT   : RELATIF / ABSOLU
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
      INTEGER      IRET, NBPARA, NBLIGN, JNUME, JNUM2, II, JJ, KTBLP
      INTEGER      JTBLP, I, J, K, N, M, IDEB, IFIN, NBUTI, NDIM
      INTEGER      JVALL, KVALL, JVALE, KVALE, JTBNP, KTBNP, KTBBA
      CHARACTER*1  BASE
      CHARACTER*4  TYPE, KNUME
      CHARACTER*19 NOMTAB, NOMTA2
      CHARACTER*24 NOMJV, NOJV2, NOMJVL, NOJVL2, INPAR, JNPAR
      CHARACTER*24 VALK
      LOGICAL      LOK
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      NOMTAB = TABIN
      NOMTA2 = TABOUT
      BASE   = BASOUT(1:1)
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
      DO 10 I = 1 , NPARA
         INPAR = LIPARA(I)
         DO 12 J = 1 , NBPARA
            JNPAR = ZK24(JTBLP+4*(J-1))
            IF ( INPAR .EQ. JNPAR ) THEN
               TYPE   = ZK24(JTBLP+4*(J-1)+1)
               IF ( TYPE(1:1) .EQ. 'C' ) THEN
         VALK = INPAR
                  CALL U2MESG('F', 'UTILITAI7_2',1,VALK,0,0,0,0.D0)
               ENDIF
               GOTO 10
            ENDIF
 12      CONTINUE
         VALK = INPAR
         CALL U2MESG('F', 'UTILITAI7_3',1,VALK,0,0,0,0.D0)
 10   CONTINUE
C
      CALL WKVECT ( '&&TBTRTB.TRI' , 'V V I', NBLIGN, JNUME )
      CALL WKVECT ( '&&TBTRTB.TRI2', 'V V I', NBLIGN, JNUM2 )
      DO 20 I = 1 , NBLIGN
         ZI(JNUME+I-1) = I
 20   CONTINUE
C
      CALL TBTR01 ( TABIN, NBPARA, LIPARA(1), NBLIGN, ZI(JNUME) )
C
      IF ( LCRIT(1)(1:2) .EQ. 'DE' ) THEN
         DO 22 I = 1 , NBLIGN
            ZI(JNUM2-1+I) = ZI(JNUME-1+I)
 22      CONTINUE
         DO 24 I = 1 , NBLIGN
            ZI(JNUME-1+I) = ZI(JNUM2-1+NBLIGN-I+1)
 24      CONTINUE
      ENDIF
C
      IF ( NPARA .EQ. 1 ) THEN
         GOTO 104
      ELSEIF ( NPARA .EQ. 2 ) THEN
      ELSEIF ( NPARA .GT. 2 ) THEN
         CALL U2MESS('F','UTILITAI4_87')
      ENDIF
      I = 1
      INPAR = LIPARA(I)
      DO 102 J = 1 , NBPARA
        JNPAR = ZK24(JTBLP+4*(J-1))
        IF ( INPAR .EQ. JNPAR ) THEN
          TYPE   = ZK24(JTBLP+4*(J-1)+1)
          NOMJV  = ZK24(JTBLP+4*(J-1)+2)
          NOMJVL = ZK24(JTBLP+4*(J-1)+3)
          CALL JEVEUO ( NOMJV , 'L', JVALE )
          CALL JEVEUO ( NOMJVL, 'L', JVALL )
          II = 0
 30       CONTINUE
          II = II + 1
          IF ( II .GE. NBLIGN ) GOTO 32
          IDEB = II
          IFIN = II
          N = ZI(JNUME+II-1)
          DO 34 JJ = II+1 , NBLIGN
            IFIN = JJ
            M = ZI(JNUME+JJ-1)
            IF ( TYPE(1:1) .EQ. 'I' ) THEN
              IF ( ZI(JVALE+N-1) .NE. ZI(JVALE+M-1) ) THEN
                 IFIN = IFIN - 1
                 GOTO 36
              ENDIF
            ELSEIF ( TYPE(1:1) .EQ. 'R' ) THEN
C              IF ( ZR(JVALE+N-1) .NE. ZR(JVALE+M-1) ) THEN
C             TEST D'EGALITE A PREC PRES
              IF( CRIT.EQ.'ABSOLU  ') THEN
                LOK = ( ABS(ZR(JVALE+N-1)-ZR(JVALE+M-1)) .LE.
     &                    PREC*ABS(ZR(JVALE+M-1)) )
              ELSE
                LOK = ( ABS(ZR(JVALE+N-1)-ZR(JVALE+M-1)) .LE. PREC )
              ENDIF
              IF ( .NOT.LOK ) THEN
                 IFIN = IFIN - 1
                 GOTO 36
              ENDIF
            ELSEIF ( TYPE(1:1) .EQ. 'C' ) THEN
              IF ( ZC(JVALE+N-1) .NE. ZC(JVALE+M-1) ) THEN
                 IFIN = IFIN - 1
                 GOTO 36
              ENDIF
            ELSEIF ( TYPE(1:3) .EQ. 'K80' ) THEN
              IF ( ZK80(JVALE+N-1) .NE. ZK80(JVALE+M-1) ) THEN
                 IFIN = IFIN - 1
                 GOTO 36
              ENDIF
            ELSEIF ( TYPE(1:3) .EQ. 'K32' ) THEN
              IF ( ZK32(JVALE+N-1) .NE. ZK32(JVALE+M-1) ) THEN
                 IFIN = IFIN - 1
                 GOTO 36
              ENDIF
            ELSEIF ( TYPE(1:3) .EQ. 'K24' ) THEN
              IF ( ZK24(JVALE+N-1) .NE. ZK24(JVALE+M-1) ) THEN
                 IFIN = IFIN - 1
                 GOTO 36
              ENDIF
            ELSEIF ( TYPE(1:3) .EQ. 'K16' ) THEN
              IF ( ZK16(JVALE+N-1) .NE. ZK16(JVALE+M-1) ) THEN
                 IFIN = IFIN - 1
                 GOTO 36
              ENDIF
            ELSEIF ( TYPE(1:2) .EQ. 'K8' ) THEN
              IF ( ZK8(JVALE+N-1) .NE. ZK8(JVALE+M-1) ) THEN
                 IFIN = IFIN - 1
                 GOTO 36
              ENDIF
            ENDIF
 34       CONTINUE
 36       CONTINUE
          NBUTI = IFIN-IDEB+1
          IF ( NBUTI .GT. 1 ) THEN
C           --- ON TESTE AVEC LE PARAMETRE SUIVANT ---
            CALL TBTR01 ( TABIN, NBPARA, LIPARA(I+1), NBUTI,
     &                      ZI(JNUME-1+IDEB) )
            IF ( LCRIT(I+1)(1:2) .EQ. 'DE' ) THEN
              DO 40 K = 1 , NBUTI
                ZI(JNUM2+K-1) = ZI(JNUME-1+IDEB+K-1)
 40           CONTINUE
              DO 42 K = 1 , NBUTI
                ZI(JNUME-1+IDEB+K-1) = ZI(JNUM2-1+NBUTI-K+1)
 42           CONTINUE
            ENDIF
          ENDIF
          II = IFIN
          GOTO 30
 32       CONTINUE
          GOTO 104
        ENDIF
 102  CONTINUE
 104  CONTINUE
C
C     --- ON DUPLIQUE LA TABLE ---
C
C     -- .TBBA :
      CALL WKVECT(NOMTA2//'.TBBA',BASE//' V K8',1,KTBBA)
      ZK8(KTBBA) = BASE
C
C     -- .TBNP :
      CALL WKVECT(NOMTA2//'.TBNP',BASE//' V I',2,KTBNP)
      ZI(KTBNP  ) = NBPARA
      ZI(KTBNP+1) = NBLIGN
C
C     -- .TBLP :
      NDIM = 4 * NBPARA
      CALL JECREO ( NOMTA2//'.TBLP', BASE//' V K24')
      CALL JEECRA ( NOMTA2//'.TBLP', 'LONMAX', NDIM, ' ')
      CALL JEECRA ( NOMTA2//'.TBLP', 'LONUTI', NDIM, ' ')
      CALL JEVEUO ( NOMTA2//'.TBLP' , 'E', KTBLP )
      DO 300 I = 1 , NBPARA
         ZK24(KTBLP+4*(I-1)  ) = ZK24(JTBLP+4*(I-1)  )
         ZK24(KTBLP+4*(I-1)+1) = ZK24(JTBLP+4*(I-1)+1)
C
         CALL CODENT ( I ,'D0',KNUME)
         NOMJV = NOMTA2//'.'//KNUME
         ZK24(KTBLP+4*(I-1)+2) = NOMJV
         TYPE = ZK24(JTBLP+4*(I-1)+1)
         CALL JECREO ( NOMJV , BASE//' V '//TYPE )
         CALL JEECRA ( NOMJV , 'LONMAX' , NBLIGN , ' ' )
         CALL JEECRA ( NOMJV , 'LONUTI' , NBLIGN , ' ' )
         CALL JEVEUO ( NOMJV , 'E', KVALE )
C
         NOMJV = NOMTA2(1:17)//'LG.'//KNUME
         ZK24(KTBLP+4*(I-1)+3) = NOMJV
         CALL JECREO ( NOMJV ,BASE//' V I' )
         CALL JEECRA ( NOMJV , 'LONMAX' , NBLIGN , ' ' )
         CALL JEVEUO ( NOMJV , 'E', KVALL )
C
         NOJV2  = ZK24(JTBLP+4*(I-1)+2)
         NOJVL2 = ZK24(JTBLP+4*(I-1)+3)
         CALL JEVEUO ( NOJV2 , 'L', JVALE )
         CALL JEVEUO ( NOJVL2, 'L', JVALL )
C
         DO 302 J = 1 , NBLIGN
            ZI(KVALL+J-1) = ZI(JVALL+ZI(JNUME+J-1)-1)
            IF ( TYPE(1:1) .EQ. 'I' ) THEN
               ZI(KVALE+J-1) = ZI(JVALE+ZI(JNUME+J-1)-1)
            ELSEIF ( TYPE(1:1) .EQ. 'R' ) THEN
               ZR(KVALE+J-1) = ZR(JVALE+ZI(JNUME+J-1)-1)
            ELSEIF ( TYPE(1:1) .EQ. 'C' ) THEN
               ZC(KVALE+J-1) = ZC(JVALE+ZI(JNUME+J-1)-1)
            ELSEIF ( TYPE(1:3) .EQ. 'K80' ) THEN
               ZK80(KVALE+J-1) = ZK80(JVALE+ZI(JNUME+J-1)-1)
            ELSEIF ( TYPE(1:3) .EQ. 'K32' ) THEN
               ZK32(KVALE+J-1) = ZK32(JVALE+ZI(JNUME+J-1)-1)
            ELSEIF ( TYPE(1:3) .EQ. 'K24' ) THEN
               ZK24(KVALE+J-1) = ZK24(JVALE+ZI(JNUME+J-1)-1)
            ELSEIF ( TYPE(1:3) .EQ. 'K16' ) THEN
               ZK16(KVALE+J-1) = ZK16(JVALE+ZI(JNUME+J-1)-1)
            ELSEIF ( TYPE(1:3) .EQ. 'K8' ) THEN
               ZK8(KVALE+J-1) = ZK8(JVALE+ZI(JNUME+J-1)-1)
            ENDIF
 302     CONTINUE
C
 300  CONTINUE
C
      CALL JEDETR ( '&&TBTRTB.TRI'  )
      CALL JEDETR ( '&&TBTRTB.TRI2' )
C
      CALL JEDEMA()
      END
