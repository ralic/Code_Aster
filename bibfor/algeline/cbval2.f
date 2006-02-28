      SUBROUTINE CBVAL2(NBCOMB,CONST,TYPMAT,LMAT,TYPRES,LRES,
     +                  DDLEXC)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           NBCOMB,                    LMAT(*),      LRES
      CHARACTER*(*)    TYPMAT(*),    TYPRES,  DDLEXC
      REAL*8                          CONST(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 28/02/2006   AUTEUR VABHHTS J.PELLET 
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
C     COMBINAISON LINEAIRE DES .VALM DES MATRICES
C       *  LES MATRICES SONT SUPPOSEES AVOIR LE MEME TYPE DE STOCKAGE
C          (MORSE) MAIS ELLES ONT DES PROFILS DIFFERENTS
C       *  POUR L'INSTANT ON NE TRAITE QUE LE CAS DE MATRICES
C          SYMETRIQUES, REELLES.
C       *  LES SCALAIRES SONT PRIS REELS POUR L'INSTANT
C     -----------------------------------------------------------------
C IN  I  NBCOMB = NOMBRE DE MATRICES A COMBINER
C IN  R  CONST  = TABLEAU DE R*8    DES COEFICIENTS
C IN  K* TYPMAT = TYPE DES MATRICES   (R)
C IN  I  LMAT = TABLEAU DES POINTEURS DES MATRICES
C IN  K* TYPRES = TYPE DES MATRICES   (R)
C IN  I  LRES = POINTEUR DE MATRICE RESULTAT
C IN  K* DDLEXC = NOM DES DDLS A EXCLURE (CONCRETEMENT IL S'AGIT
C                                         DES LAGRANGE)
C
C     -----------------------------------------------------------------
C
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
      CHARACTER*16              ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                  ZK32
      CHARACTER*80                                            ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32  JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
C     -----------------------------------------------------------------
C     LGBLOC = LONGUEUR DES BLOCS
      INTEGER               LGBLOC,TYPSYM,MATSYM,HCOL
C     -----------------------------------------------------------------
      CHARACTER*1   CH1,CLAS
      CHARACTER*4   CBID
      CHARACTER*8   NOMDDL
      CHARACTER*14  NUME, NUMI
      CHARACTER*19  MATRES,MATI,MATSTO
      CHARACTER*19  NOMA
      CHARACTER*24  VALM, VALMR
      CHARACTER*24  MAT1
C
      REAL*8      ZERO
C     -----------------------------------------------------------------
      CALL JEMARQ()
      ZERO   = 0.D0
C
      NOMDDL = DDLEXC
      MATRES = ZK24(ZI(LRES+1))
      NEQ    = ZI(LRES+2)
      VALMR  = MATRES//'.VALM'
      LGBLOC = ZI(LRES+14)
      CALL JELIRA(MATRES//'.REFA','CLAS',IBID,CLAS)
C
      MAT1 = ZK24(ZI(LMAT(1)+1))
      NOMA = MAT1
      MXDDL = 1
C
C --- RECUPERATION DU NOM DE LA NUMEROTATION ASSOCIEE AUX MATRICES :
C     ------------------------------------------------------------
      CALL DISMOI('F','NOM_NUME_DDL',NOMA,'MATR_ASSE',IBID,NUME,IERD)
C
C --- RECUPERATION DES POSITIONS DES DDLS :
C     -----------------------------------
      CALL WKVECT('&&CBVAL2','V V I',NEQ*MXDDL,LDDL)
      CALL PTEDDL( 'NUME_DDL', NUME  , MXDDL , NOMDDL , NEQ , ZI(LDDL))
C
      TYPSYM = ZI(LRES+4)
      CALL ASSERT(TYPSYM.EQ.1.OR.TYPSYM.EQ.0)
C
C --- AFFECTATION DE L'ETAT DE LA PREMIERE MATRICE COMBINER
C --- A LA MATRICE RESULTAT :
C     ---------------------

      CALL JEVEUO(MATRES//'.REFA','L',JREFA)
      CALL JEVEUO(ZK24(JREFA-1+2)(1:14)//'.SMOS.SMHC','L',JSMHC)
      CALL JEVEUO(ZK24(JREFA-1+2)(1:14)//'.SMOS.SMDI','L',JSMDI)
C
C --- BOUCLE SUR LES BLOCS DE LA MATRICE RESULTAT :
C     -------------------------------------------
C
C
C ---  RECUPERATION DU NUMERO DE LA PREMIERE LIGNE DU BLOC :
C      ---------------------------------------------------
C
C ---  RECUPERATION DU TABLEAU DES VALEURS DU BLOC DE LA MATRICE
C ---  RESULTANTE :
C      ----------
        CALL JEVEUO( JEXNUM(VALMR,1),'E',IATRES)
        IF (TYPSYM.EQ.0) THEN
           CALL UTMESS('F','CBVAL2','POUR L''INSTANT, ON NE TRAITE '
     +              //'QUE LE CAS DES MATRICES SYMETRIQUES.')
        ENDIF
        IF (TYPRES(1:1) .EQ.'R') THEN
          DO 20 IVAL = IATRES, IATRES+LGBLOC-1
            ZR(IVAL) = ZERO
 20       CONTINUE
        ELSE
          CALL UTMESS('F','CBVAL2','POUR L''INSTANT, ON NE TRAITE'//
     +                'QUE LE CAS DES MATRICES REELLES.')
        ENDIF
C
C --- BOUCLE SUR LES MATRICES A COMBINER :
C     ----------------------------------
        ICONST = 1
C
        DO 30 IMAT = 1, NBCOMB
C
C ---     NOM DE LA MATRICE A COMBINER :
C         ----------------------------
          MATI = ZK24(ZI(LMAT(IMAT)+1))
C
          VALM = MATI//'.VALM'
C
          MATSYM = ZI(LMAT(IMAT)+4)
C
          IF (MATSYM.EQ.0) THEN
           CALL UTMESS('F','CBVAL2','POUR L''INSTANT, ON NE FAIT '
     +              //' LA COMBINAISON LINEAIRE QUE DE '
     +              //' MATRICES SYMETRIQUES.')
          ENDIF
C
C ---     RECUPERATION DU NOM DE LA NUMEROTATION ASSOCIEE A LA MATRICE :
C         ------------------------------------------------------------
          CALL DISMOI('F','NOM_NUME_DDL',MATI,'MATR_ASSE',IBID,NUMI,IER)
CC
C ---     LA MATRICE RESULTAT EST REELLE :
C         ------------------------------
          IF ( TYPRES(1:1) .EQ. 'R' ) THEN
C
C ---       LA MATRICE A COMBINER EST REELLE :
C           --------------------------------
            IF ( TYPMAT(IMAT)(1:1) .EQ. 'R' ) THEN
            CALL JEVEUO( NUMI//'.SMOS.SMHC','L',JSMHC1)
C
C ---       RECUPERATION DU TABLEAU .SMDI DE LA MATRICE A COMBINER
C ---       DONNANT L'INDICE CUMMULE DES TERMES DIAGONAUX :
C           ---------------------------------------------
            CALL JEVEUO( NUMI//'.SMOS.SMDI','L',JSMDI1)
C
            CALL JEVEUO( JEXNUM(VALM,1),'L',IATMAT)
C
C ---       CREATION DU TABLEAU D'INDIRECTION DES INDICES DES
C ---       TERMES  D'UNE LIGNE DE LA MATRICE A COMBINER
C ---       VERS LES INDICES DES TERMES DE LA MEME LIGNE
C ---       DE LA MATRICE RESULTANTE :
C           ------------------------
            CALL JEDETR('&&CBVAL2.IND_LIG')
            CALL WKVECT('&&CBVAL2.IND_LIG','V V I',NEQ,IDINDI)
C
C ---       CAS D'UNE MATRICE SYMETRIQUE :
C           ---------------------------
            IF (TYPSYM.EQ.1) THEN
              CALL RRSSM2(NEQ, ZI(JSMHC),ZI(JSMHC1),ZI(JSMDI),
     +                        ZI(JSMDI1),ZI(LDDL), CONST(ICONST),
     +                        ZI(IDINDI),ZR(IATMAT), ZR(IATRES))
            ELSE
              CALL UTMESS('F','CBVAL2','POUR L''INSTANT, '
     +          //' ON NE FAIT LA COMBINAISON LINEAIRE QUE DE '
     +          //' MATRICES SYMETRIQUES.')
            ENDIF
C
C ---      CAS OU LA MATRICE RESULTANTE EST REELLE ET LA MATRICE
C ---      A COMBINER EST COMPLEXE :
C           ----------------------
            ELSEIF ( TYPMAT(IMAT)(1:1) .EQ. 'C' ) THEN
              CALL UTMESS('F','CBVAL2','POUR L''INSTANT,  '
     +              //'ON NE FAIT LA COMBINAISON LINEAIRE QUE DE '
     +              //' MATRICES REELLES ET '
     +              //' QUI DONC NE SONT PAS COMPLEXES.')
            ENDIF
          ENDIF
          ICONST = ICONST + 1
          CALL JELIBE( JEXNUM(VALM,1))
C
C ---   FIN DE LA BOUCLE SUR LES MATRICES A COMBINER :
C       --------------------------------------------
  30    CONTINUE
C
C
C --- FIN DE LA BOUCLE SUR LES BLOCS DE LA MATRICE RESULTAT :
C     -----------------------------------------------------
C
      CALL JEDETR('&&CBVAL2')
C
      CALL JEDEMA()
C
      END
