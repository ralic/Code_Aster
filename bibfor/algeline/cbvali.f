      SUBROUTINE CBVALI(NBCOMB,TYPCST,CONST,TYPMAT,LMAT,TYPRES,LRES)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           NBCOMB,                    LMAT(*),    LRES
      CHARACTER*(*)    TYPCST(*),   TYPMAT(*),    TYPRES
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
C     COMBINAISON LINEAIRE DES .VALI DES MATRICES
C     CETTE OPERATION N'EST FAITE QUE LORSQUE LES MATRICES
C     ONT DES DDLS ELIMINES
C     LES DDLS ELIMINES DOIVENT ETRE LES MEMES POUR
C     TOUTES LES MATRICES A COMBINER
C       *  LES MATRICES SONT SUPPOSEES ETRE DE MEME STOCKAGE
C          MAIS PEUVENT ETRE A ELEMENTS REELS OU COMPLEXES
C       *  LES SCALAIRES SONT REELS OU COMPLEXES
C     -----------------------------------------------------------------
C IN  I  NBCOMB = NOMBRE DE MATRICES A COMBINER
C IN  K* TYPCST = TYPE DES CONSTANTES (R OU C OU I)
C IN  R  CONST  = TABLEAU DE R*8    DES COEFICIENTS
C IN  K* TYPMAT = TYPE DES MATRICES   (R OU C)
C IN  I  LMAT = TABLEAU DES POINTEURS DES MATRICES
C IN  K* TYPRES = TYPE DES MATRICES   (R OU C)
C IN  I  LRES = POINTEUR DE MATRICE RESULTAT
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
C     NBLIC  = NOMBRE DE BLOCS POUR .VALI DE LA MATRICE
C     LGBLOC = LONGUEUR DES BLOCS
      INTEGER               LGBLOC,NBLIC
C     -----------------------------------------------------------------
      CHARACTER*1   CH1
      CHARACTER*19  MATRES,MATI
      CHARACTER*24  VALI
C     -----------------------------------------------------------------
      CALL JEMARQ()
C
      MATRES = ZK24(ZI(LRES+1))
C
C --- NOM DE LA PREMIERE MATRICE A COMBINER
C
         MATI = ZK24(ZI(LMAT(1)+1))
C
C --- NOMBRE DE BLOCS
C
         NBLIC = ZI(LMAT(1)+18)
C
C --- LONGUEUR D'UN BLOC
C
        CALL JEVEUO(MATRES//'.ABLI','L',IDABLI)
        CALL JEVEUO(MATRES//'.ALIG','L',IDALIG)
        CALL JEVEUO(MATRES//'.LLIG','L',IDLLIG)
        IMPFIN = ZI(IDABLI+NBLIC)
        ILOC = ZI(IDALIG+IMPFIN-1)
        IND = 2+3*(IMPFIN-1)
        JDEB = ZI(IDLLIG+IND+1-1)
        JFIN = ZI(IDLLIG+IND+2-1)
        LGBLOC = ILOC+JFIN-JDEB
        CALL JELIBE(MATRES//'.ABLI')
        CALL JELIBE(MATRES//'.ALIG')
        CALL JELIBE(MATRES//'.LLIG')
C
C --- BOUCLE SUR LES BLOCS DE LA MATRICE RESULTAT
C
      DO 10 IBLIC = 1, NBLIC
          CALL JEVEUO(JEXNUM(MATRES//'.VALI',IBLIC),'E',IVALI)
          ICONST = 1
C
C ---    BOUCLE SUR LES MATRICES A COMBINER
C
          DO 20 IMAT  = 1, NBCOMB
C
C ---         NOM DE LA MATRICE A COMBINER
C
             MATI = ZK24(ZI(LMAT(IMAT)+1))
             VALI = MATI//'.VALI'
             CALL JEVEUO( JEXNUM(VALI,IBLIC),'L',IATMAT)
C
C ---       LA MATRICE RESULTAT EST REELLE
C
             IF ( TYPRES(1:1) .EQ. 'R' ) THEN
C
C ---          LA MATRICE A COMBINER EST REELLE
C
                  IF ( TYPMAT(IMAT)(1:1) .EQ. 'R' ) THEN
                     DO 30 I= 1, LGBLOC
                        ZR(IVALI+I-1) = ZR(IVALI+I-1)
     +                                  +CONST(ICONST)*ZR(IATMAT-1+I)
 30                  CONTINUE
C
C ---          LA MATRICE A COMBINER EST COMPLEXE
C
                  ELSEIF ( TYPMAT(IMAT)(1:1) .EQ. 'C' ) THEN
C
C ---                LE COEFFICIENT MULTIPLICATEUR EST REEL
C ---                OU COMPLEXE
C
                        IF( TYPCST(IMAT)(1:1) .EQ. 'R'.OR.
     +                      TYPCST(IMAT)(1:1) .EQ. 'C') THEN
                           DO 40 I= 1, LGBLOC
                              ZR(IVALI+I-1) = ZR(IVALI+I-1)
     +                                    +CONST(ICONST)*ZC(IATMAT-1+I)
 40                       CONTINUE
C
C ---                LE COEFFICIENT MULTIPLICATEUR EST IMAGINAIRE PUR
C
                        ELSEIF ( TYPCST(IMAT)(1:1) .EQ. 'I' ) THEN
                           DO 50 I= 1, LGBLOC
                              ZR(IVALI+I-1) = ZR(IVALI+I-1)
     +                             +CONST(ICONST)*DIMAG(ZC(IATMAT-1+I))
 50                       CONTINUE
C
C ---                LE COEFFICIENT MULTIPLICATEUR EST DE TYPE INCONNU
C
                      ELSE
                         CH1 = TYPCST(IMAT)(1:1)
                         CALL UTMESS('F','CBVALI_1','TYPE INCONNU: '
     +                                //CH1)
                        ENDIF
C
C ---          LA MATRICE A COMBINER EST DE TYPE INCONNU
C
                  ELSE
                    CH1 = TYPMAT(IMAT)(1:1)
                    CALL UTMESS('F','CBVALI_2','TYPE INCONNU: '//CH1)
                  ENDIF
C
C ---       LA MATRICE RESULTAT EST COMPLEXE
C
             ELSEIF ( TYPRES(1:1) .EQ. 'C' ) THEN
C
C ---          LA MATRICE A COMBINER EST REELLE
C
                  IF ( TYPMAT(IMAT)(1:1) .EQ. 'R' ) THEN
C
C ---                LE COEFFICIENT MULTIPLICATEUR EST REEL
C
                        IF( TYPCST(IMAT)(1:1) .EQ. 'R') THEN
                          DO 60 I= 1, LGBLOC
                             ZC(IVALI+I-1) = ZC(IVALI+I-1)
     +                                    +CONST(ICONST)*ZR(IATMAT-1+I)
 60                       CONTINUE
C
C ---                LE COEFFICIENT MULTIPLICATEUR EST COMPLEXE
C
                        ELSEIF( TYPCST(IMAT)(1:1) .EQ. 'C') THEN
                          C8CST = DCMPLX(CONST(ICONST),CONST(ICONST+1))
                          DO 70 I= 1, LGBLOC
                             ZC(IVALI+I-1) = ZC(IVALI+I-1)
     +                                    +C8CST*ZR(IATMAT-1+I)
 70                      CONTINUE
                         ENDIF
C
C ---          LA MATRICE A COMBINER EST COMPLEXE
C
                  ELSEIF ( TYPMAT(IMAT)(1:1) .EQ. 'C' ) THEN
C
C ---                LE COEFFICIENT MULTIPLICATEUR EST REEL
C
                      IF( TYPCST(IMAT)(1:1) .EQ. 'R') THEN
                           DO 80 I= 1, LGBLOC
                              ZC(IVALI+I-1) = ZC(IVALI+I-1)
     +                                    +CONST(ICONST)*ZC(IATMAT-1+I)
 80                       CONTINUE
C
C ---                LE COEFFICIENT MULTIPLICATEUR EST COMPLEXE
C
                      ELSEIF ( TYPCST(IMAT)(1:1) .EQ. 'C' ) THEN
                          C8CST = DCMPLX(CONST(ICONST),CONST(ICONST+1))
                           DO 90 I= 1, LGBLOC
                              ZC(IVALI+I-1) = ZC(IVALI+I-1)
     +                                    +C8CST*ZC(IATMAT-1+I)
 90                      CONTINUE
C
C ---                LE COEFFICIENT MULTIPLICATEUR EST DE TYPE INCONNU
C
                      ELSE
                         CH1 = TYPCST(IMAT)(1:1)
                         CALL UTMESS('F','CBVALI_3','TYPE INCONNU: '
     +                                //CH1)
                      ENDIF
C
C ---          LA MATRICE A COMBINER EST DE TYPE INCONNU
C
                  ELSE
                    CH1 = TYPMAT(IMAT)(1:1)
                    CALL UTMESS('F','CBVALI_4','TYPE INCONNU: '//CH1)
                  ENDIF
            ENDIF
            ICONST = ICONST + 1
            IF (TYPCST(IMAT)(1:1) .EQ. 'C') ICONST = ICONST + 1
            CALL JELIBE( JEXNUM(VALI,IBLIC))
C ---   FIN DE LA BOUCLE SUR LES MATRICES A COMBINER
  20      CONTINUE
          CALL JELIBE(JEXNUM(MATRES//'.VALI',IBLIC))
C --- FIN DE LA BOUCLE SUR LES BLOCS DE LA MATRICE RESULTAT
  10  CONTINUE
C
C
      CALL JEDEMA()
      END
