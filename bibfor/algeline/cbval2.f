      SUBROUTINE CBVAL2(NBCOMB,TYPCST,CONST,TYPMAT,LMAT,TYPRES,LRES,
     +                  DDLEXC)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           NBCOMB,                    LMAT(*),      LRES
      CHARACTER*(*)    TYPCST(*),   TYPMAT(*),    TYPRES,  DDLEXC
      REAL*8                          CONST(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 21/06/2000   AUTEUR CIBHHLV L.VIVAN 
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
C     COMBINAISON LINEAIRE DES .VALE DES MATRICES
C       *  LES MATRICES SONT SUPPOSEES AVOIR LE MEME TYPE DE STOCKAGE
C          (EN LIGNE DE CIEL OU MORSE) MAIS ELLES ONT DES PROFILS
C          DIFFERENTS (I.E. LES LONGUEURS DE LIGNE SONT DIFFERENTES) . 
C       *  POUR L'INSTANT ON NE TRAITE QUE LE CAS DE MATRICES 
C          SYMETRIQUES, REELLES.
C       *  LES SCALAIRES SONT PRIS REELS POUR L'INSTANT
C     -----------------------------------------------------------------
C IN  I  NBCOMB = NOMBRE DE MATRICES A COMBINER
C IN  K* TYPCST = TYPE DES CONSTANTES (R)
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
C     NBBLOC = NOMBRE DE BLOCS POUR CONSTITUER UNE MATRICE (.VALE)
C     LGBLOC = LONGUEUR DES BLOCS
      INTEGER               NBBLOC,LGBLOC,TYPSYM,MATSYM,HCOL
C     -----------------------------------------------------------------
      CHARACTER*1   CH1,CLAS
      CHARACTER*4   CBID
      CHARACTER*4   ETAMAT
      CHARACTER*8   NOMDDL
      CHARACTER*14  NUME, NUMI
      CHARACTER*19  MATRES,MATI,MATSTO
      CHARACTER*19  NOMA, NOMRES
      CHARACTER*24  VALE, VALER
      CHARACTER*24  MAT1
C
      REAL*8      ZERO
C     -----------------------------------------------------------------
      CALL JEMARQ()
      ZERO   = 0.D0
C
      NOMDDL = DDLEXC
      ISTOC  = ZI(LRES+6)
      MATRES = ZK24(ZI(LRES+1))
      NOMRES = MATRES
      NEQ    = ZI(LRES+2)
      VALER  = MATRES//'.VALE'
      NBBLOC = ZI(LRES+13)
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
C
C --- AFFECTATION DE L'ETAT DE LA PREMIERE MATRICE COMBINER
C --- A LA MATRICE RESULTAT :
C     ---------------------
      CALL JELIRA(NOMA//'.REFA','DOCU',IBID,ETAMAT)
      CALL JEECRA(NOMRES//'.REFA','DOCU',IBID,ETAMAT)
      IDABLO = ZI(LRES+12)
      CALL JEVEUO(ZK24(ZI(LRES+1))(1:19)//'.REFA','L',IDREF2)
      CALL JEVEUO(ZK24(IDREF2+2)(1:19)//'.HCOL','L',IDHCOL)
      CALL JEVEUO(ZK24(IDREF2+2)(1:19)//'.ABLO','L',IDABLO)
      CALL JEVEUO(ZK24(IDREF2+2)(1:19)//'.ADIA','L',IDADIA)      
C
C --- BOUCLE SUR LES BLOCS DE LA MATRICE RESULTAT :
C     -------------------------------------------
      DO 10 IBLOC = 1, NBBLOC
C
C
C ---  RECUPERATION DU NUMERO DE LA PREMIERE LIGNE DU BLOC :
C      ---------------------------------------------------
        IL1 = ZI(IDABLO+IBLOC-1) + 1
C
C ---  RECUPERATION DU TABLEAU DES VALEURS DU BLOC DE LA MATRICE
C ---  RESULTANTE :
C      ----------
        CALL JEVEUO( JEXNUM(VALER,IBLOC),'E',IATRES)
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
          VALE = MATI//'.VALE'
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
C
C ---         CAS D'UN STOCKAGE EN LIGNE DE CIEL :
C             ----------------------------------
              IF (ISTOC.EQ.1) THEN
                  
C ---           RECUPERATION DU TABLEAU .HCOL DES LONGUEURS DE LIGNE
C ---           DE LA MATRICE A COMBINER :
C               ------------------------
                CALL JEVEUO( NUMI//'.SLCS.HCOL','L',IDHCO1)
C
C ---           RECUPERATION DU TABLEAU .IABL DE LA MATRICE A COMBINER
C ---           DONNANT POUR CHAQUE EQUATION LE NUMERO DE BLOC AUQUEL
C ---           ELLE APPARTIENT :
C               ---------------
                 CALL JEVEUO( NUMI//'.SLCS.IABL','L',IDIABL)
C
C ---           NUMERO DE BLOC DE LA MATRICE A COMBINER AUQUEL 
C ---           APPARTIENT LA PREMIERE LIGNE DU BLOC DE LA MATRICE 
C ---           RESULTANTE :
C               ----------
                JBLOC = ZI(IDIABL+IL1-1)
C
C ---           RECUPERATION DU TABLEAU .ABLO DE LA MATRICE A COMBINER
C ---           DONNANT POUR CHAQUE BLOC LES NUMEROS DE LA PREMIERE
C ---           ET DE LA DERNIERE EQUATION DU BLOC :
C               ----------------------------------
                CALL JEVEUO( NUMI//'.SLCS.ABLO','L',IDABL1)
C
                CALL JEVEUO( JEXNUM(VALE,JBLOC),'L',IATMAT)
C
C ---           CAS D'UNE MATRICE SYMETRIQUE :
C               ---------------------------
                IF (TYPSYM.EQ.1) THEN
                  CALL RRRSS2(ZI(IDABLO+IBLOC-1), ZI(IDHCOL),
     +                            ZI(IDHCO1), ZI(IDIABL), ZI(IDABL1),
     +                            ZI(LDDL), CONST(ICONST),
     +                            ZR(IATMAT), ZR(IATRES))
                ELSE
                  CALL UTMESS('F','CBVAL2','POUR L''INSTANT, '
     +              //' ON NE FAIT LA COMBINAISON LINEAIRE QUE DE '
     +              //' MATRICES SYMETRIQUES.')                
                ENDIF
C
C ---         CAS D'UN STOCAGE MORSE
C             ---------------------
              ELSEIF (ISTOC.EQ.2) THEN
C                  
C ---           RECUPERATION DU TABLEAU .HCOL DES LONGUEURS DE LIGNE
C ---           DE LA MATRICE A COMBINER :
C               ------------------------
                CALL JEVEUO( NUMI//'.SMOS.HCOL','L',IDHCO1)
C
C ---           RECUPERATION DU TABLEAU .ADIA DE LA MATRICE A COMBINER
C ---           DONNANT L'INDICE CUMMULE DES TERMES DIAGONAUX :
C               ---------------------------------------------
                CALL JEVEUO( NUMI//'.SMOS.ADIA','L',IDADI1)
C
                JBLOC = 1
                CALL JEVEUO( JEXNUM(VALE,JBLOC),'L',IATMAT)
C
C ---           CREATION DU TABLEAU D'INDIRECTION DES INDICES DES 
C ---           TERMES  D'UNE LIGNE DE LA MATRICE A COMBINER 
C ---           VERS LES INDICES DES TERMES DE LA MEME LIGNE
C ---           DE LA MATRICE RESULTANTE :
C               ------------------------
                CALL JEDETR('&&CBVAL2.IND_LIG')
                CALL WKVECT('&&CBVAL2.IND_LIG','V V I',NEQ,IDINDI)
C
C ---           CAS D'UNE MATRICE SYMETRIQUE :
C               ---------------------------
                IF (TYPSYM.EQ.1) THEN                  
                  CALL RRSSM2(NEQ, ZI(IDHCOL),ZI(IDHCO1),ZI(IDADIA),
     +                            ZI(IDADI1),ZI(LDDL), CONST(ICONST),
     +                            ZI(IDINDI),ZR(IATMAT), ZR(IATRES))
                ELSE
                  CALL UTMESS('F','CBVAL2','POUR L''INSTANT, '
     +              //' ON NE FAIT LA COMBINAISON LINEAIRE QUE DE '
     +              //' MATRICES SYMETRIQUES.')                
                ENDIF
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
          CALL JELIBE( JEXNUM(VALE,JBLOC))
C
C ---   FIN DE LA BOUCLE SUR LES MATRICES A COMBINER :
C       --------------------------------------------
  30    CONTINUE
C
        CALL JELIBE( JEXNUM(VALER,IBLOC))
C
C --- FIN DE LA BOUCLE SUR LES BLOCS DE LA MATRICE RESULTAT :
C     -----------------------------------------------------
  10  CONTINUE
C
      CALL JEDETR('&&CBVAL2')
C
      CALL JEDEMA()
C
      END
