      SUBROUTINE CBVALE(NBCOMB,TYPCST,CONST,TYPMAT,LMAT,TYPRES,LRES,
     &                  DDLEXC)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER NBCOMB,LMAT(*),LRES
      CHARACTER*(*) TYPCST(*),TYPMAT(*),TYPRES,DDLEXC
      REAL*8 CONST(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 01/02/2000   AUTEUR VABHHTS J.PELLET 
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
C IN  K* DDLEXC = NOM DES DDLS A EXCLURE (CONCRETEMENT IL S'AGIT
C                                         DES LAGRANGE)

C     -----------------------------------------------------------------

C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32 JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------

C     -----------------------------------------------------------------
C     NBBLOC = NOMBRE DE BLOCS POUR CONSTITUER UNE MATRICE (.VALE)
C     LGBLOC = LONGUEUR DES BLOCS
      INTEGER NBBLOC,LGBLOC,TYPSYM,MATSYM,HCOL
C     -----------------------------------------------------------------
      CHARACTER*1 CH1,CLAS
      CHARACTER*4 CBID
      CHARACTER*4 ETAMAT
      CHARACTER*19 MATRES,MATI,MATSTO
      CHARACTER*24 VALE,VALER
      CHARACTER*8 NOMDDL
      CHARACTER*14 NUME
      CHARACTER*19 NOMA,NOMRES
      CHARACTER*24 MAT1

      REAL*8 ZERO
      COMPLEX*16 CZERO,C8CST
C     -----------------------------------------------------------------
      CALL JEMARQ()
      ZERO = 0.D0
      CZERO = DCMPLX(ZERO,ZERO)


      NOMDDL = DDLEXC
      ISTOC = ZI(LRES+6)
      MATRES = ZK24(ZI(LRES+1))
      NOMRES = MATRES
      NEQ = ZI(LRES+2)
      CALL JELIRA(MATRES//'.REFA','CLAS',IBID,CLAS)
      VALER = MATRES//'.VALE'
      NBBLOC = ZI(LRES+13)
      LGBLOC = ZI(LRES+14)


      MAT1 = ZK24(ZI(LMAT(1)+1))
      NOMA = MAT1
      MXDDL = 1

C     I) RECUPERATION DU NOM DE LA NUMEROTATION ASSOCIEE AUX MATRICES
      CALL DISMOI('F','NOM_NUME_DDL',NOMA,'MATR_ASSE',IBID,NUME,IERD)

C     II) RECUPERATION DES POSITIONS DES DDL
      CALL WKVECT('&&CBVALE','V V I',NEQ*MXDDL,LDDL)
      CALL PTEDDL('NUME_DDL',NUME,MXDDL,NOMDDL,NEQ,ZI(LDDL))

      TYPSYM = ZI(LRES+4)

C --- AFFECTATION DE L'ETAT DE LA PREMIERE MATRICE COMBINER
C --- A LA MATRICE RESULTAT

      CALL JELIRA(NOMA//'.REFA','DOCU',IBID,ETAMAT)
      CALL JEECRA(NOMRES//'.REFA','DOCU',IBID,ETAMAT)
      CALL MTDSC2(ZK24(ZI(LRES+1)),'ABLO','L',IDABLO)
      CALL MTDSC2(ZK24(ZI(LRES+1)),'ADIA','L',IDADIA)
      CALL JEVEUO(ZK24(ZI(LRES+1)) (1:19)//'.REFA','L',IDREF2)
      CALL JEVEUO(ZK24(IDREF2+2) (1:19)//'.HCOL','L',IDHCOL)

C --- BOUCLE SUR LES BLOCS DE LA MATRICE RESULTAT

      DO 50 IBLOC = 1,NBBLOC

        CALL JEVEUO(JEXNUM(VALER,IBLOC),'E',IATRES)
        IF (TYPSYM.EQ.0) THEN
          CALL JEVEUO(JEXNUM(VALER,IBLOC+NBBLOC),'E',IATREI)
        END IF
        IF (TYPRES(1:1).EQ.'R') THEN
          DO 10 IVAL = IATRES,IATRES + LGBLOC - 1
            ZR(IVAL) = ZERO
   10     CONTINUE

          IF (TYPSYM.EQ.0) THEN
            DO 20 IVAL = IATREI,IATREI + LGBLOC - 1
              ZR(IVAL) = ZERO
   20       CONTINUE
          END IF
        ELSE IF (TYPRES(1:1).EQ.'C') THEN
          DO 30 IVAL = IATRES,IATRES + LGBLOC - 1
            ZC(IVAL) = CZERO
   30     CONTINUE

        ELSE
          CH1 = TYPRES(1:1)
          CALL UTMESS('F','CBVALE_1','TYPE DE MATRICE RESULTAT "'//CH1//
     &                '" INCONNU.')
        END IF

C --- BOUCLE SUR LES MATRICES A COMBINER ---
        ICONST = 1
        DO 40 IMAT = 1,NBCOMB

C ---    NOM DE LA MATRICE A COMBINER

          MATI = ZK24(ZI(LMAT(IMAT)+1))
          VALE = MATI//'.VALE'
          CALL JEVEUO(JEXNUM(VALE,IBLOC),'L',IATMAT)
          MATSYM = ZI(LMAT(IMAT)+4)
          IF (MATSYM.EQ.0) THEN
            CALL JEVEUO(JEXNUM(VALE,IBLOC+NBBLOC),'E',IATMAI)
          END IF

C --- LA MATRICE RESULTAT EST REELLE

          IF (TYPRES(1:1).EQ.'R') THEN

C ---          LA MATRICE A COMBINER EST REELLE

            IF (TYPMAT(IMAT) (1:1).EQ.'R') THEN

C ---            CAS D'UN STOCKAGE EN LIGNE DE CIEL

              IF (ISTOC.EQ.1) THEN

C ---              CAS D'UN MATRICE SYMETRIQUE

                IF (TYPSYM.EQ.1) THEN
                  CALL RRRSSL(ZI(IDABLO+IBLOC-1),ZI(IDHCOL),ZI(LDDL),
     &                        CONST(ICONST),ZR(IATMAT),ZR(IATRES))

C ----             CAS D'UN MATRICE NON-SYMETRIQUE

                ELSE IF (TYPSYM.EQ.0) THEN
                  IF (MATSYM.EQ.1) THEN
                    CALL RRRNSL(ZI(IDABLO+IBLOC-1),ZI(IDHCOL),ZI(LDDL),
     &                          CONST(ICONST),ZR(IATMAT),ZR(IATRES),
     &                          ZR(IATREI))
                  ELSE IF (MATSYM.EQ.0) THEN
                    CALL RRRNNL(ZI(IDABLO+IBLOC-1),ZI(IDHCOL),ZI(LDDL),
     &                          CONST(ICONST),ZR(IATMAT),ZR(IATMAI),
     &                          ZR(IATRES),ZR(IATREI))
                  END IF
                END IF

C ---          CAS D'UN STOCAGE MORSE

              ELSE IF (ISTOC.EQ.2) THEN

C ---              CAS D'UN MATRICE SYMETRIQUE

                IF (TYPSYM.EQ.1) THEN
                  CALL RRRSSM(ZI(IDABLO+IBLOC-1),ZI(IDHCOL),ZI(IDADIA),
     &                        ZI(LDDL),CONST(ICONST),ZR(IATMAT),
     &                        ZR(IATRES))

C ----             CAS D'UN MATRICE NON-SYMETRIQUE

                ELSE IF (TYPSYM.EQ.0) THEN
                  IF (MATSYM.EQ.1) THEN
                    CALL RRRNSM(ZI(IDABLO+IBLOC-1),ZI(IDHCOL),
     &                          ZI(IDADIA),ZI(LDDL),CONST(ICONST),
     &                          ZR(IATMAT),ZR(IATRES),ZR(IATREI))
                  ELSE IF (MATSYM.EQ.0) THEN
                    CALL RRRNNM(ZI(IDABLO+IBLOC-1),ZI(IDHCOL),
     &                          ZI(IDADIA),ZI(LDDL),CONST(ICONST),
     &                          ZR(IATMAT),ZR(IATMAI),ZR(IATRES),
     &                          ZR(IATREI))
                  END IF
                END IF

              END IF

C ---      CAS OU LA MATRICE RESULTANTE EST REELLE ET LA MATRICE
C ---      A COMBINER EST COMPLEXE

            ELSE IF (TYPMAT(IMAT) (1:1).EQ.'C') THEN

C ---        LE COEFFICIENT MULTIPLICATEUR EST REEL

              IF (TYPCST(IMAT) (1:1).EQ.'R') THEN

C ---          CAS D'UN STOCKAGE EN LIGNE DE CIEL

                IF (ISTOC.EQ.1) THEN
                  CALL RCRSSL(ZI(IDABLO+IBLOC-1),ZI(IDHCOL),ZI(LDDL),
     &                        CONST(ICONST),ZC(IATMAT),ZR(IATRES))

C ---          CAS D'UN STOCAGE MORSE

                ELSE IF (ISTOC.EQ.2) THEN
                  CALL RCRSSM(ZI(IDABLO+IBLOC-1),ZI(IDHCOL),ZI(IDADIA),
     &                        ZI(LDDL),CONST(ICONST),ZC(IATMAT),
     &                        ZR(IATRES))
                END IF

C ---        LE COEFFICIENT MULTIPLICATEUR EST IMAGINAIRE PUR

              ELSE IF (TYPCST(IMAT) (1:1).EQ.'I') THEN

C ---          CAS D'UN STOCKAGE EN LIGNE DE CIEL

                IF (ISTOC.EQ.1) THEN
                  CALL RCISSL(ZI(IDABLO+IBLOC-1),ZI(IDHCOL),ZI(LDDL),
     &                        CONST(ICONST),ZC(IATMAT),ZR(IATRES))

C ---         CAS D'UN STOCAGE MORSE

                ELSE IF (ISTOC.EQ.2) THEN
                  CALL RCISSM(ZI(IDABLO+IBLOC-1),ZI(IDHCOL),ZI(IDADIA),
     &                        ZI(LDDL),CONST(ICONST),ZC(IATMAT),
     &                        ZR(IATRES))
                END IF

C ---          LE COEFFICIENT MULTIPLICATEUR EST DE TYPE INCONNU

              ELSE
                CH1 = TYPCST(IMAT) (1:1)
                CALL UTMESS('F','CBVALE_2','TYPE INCONNU: '//CH1)
              END IF

C ---       LA MATRICE A COMBINER EST DE TYPE INCONNU

            ELSE
              CH1 = TYPMAT(IMAT) (1:1)
              CALL UTMESS('F','CBVALE_3','TYPE INCONNU: '//CH1)
            END IF

C --- LA MATRICE RESULTAT EST COMPLEXE

          ELSE IF (TYPRES(1:1).EQ.'C') THEN

C ---        LA MATRICE A COMBINER EST REELLE

            IF (TYPMAT(IMAT) (1:1).EQ.'R') THEN

C ---          LE COEFFICIENT MULTIPLICATEUR EST REEL

              IF (TYPCST(IMAT) (1:1).EQ.'R') THEN

C ---            CAS D'UN STOCKAGE EN LIGNE DE CIEL

                IF (ISTOC.EQ.1) THEN
                  CALL CRRSSL(ZI(IDABLO+IBLOC-1),ZI(IDHCOL),ZI(LDDL),
     &                        CONST(ICONST),ZR(IATMAT),ZC(IATRES))

C ---            CAS D'UN STOCKAGE MORSE

                ELSE IF (ISTOC.EQ.2) THEN
                  CALL CRRSSM(ZI(IDABLO+IBLOC-1),ZI(IDHCOL),ZI(IDADIA),
     &                        ZI(LDDL),CONST(ICONST),ZR(IATMAT),
     &                        ZC(IATRES))
                END IF

C ---          LE COEFFICIENT MULTIPLICATEUR EST COMPLEXE

              ELSE IF (TYPCST(IMAT) (1:1).EQ.'C') THEN
                C8CST = DCMPLX(CONST(ICONST),CONST(ICONST+1))

C ---            CAS D'UN STOCKAGE EN LIGNE DE CIEL

                IF (ISTOC.EQ.1) THEN
                  CALL CRCSSL(ZI(IDABLO+IBLOC-1),ZI(IDHCOL),ZI(LDDL),
     &                        C8CST,ZR(IATMAT),ZC(IATRES))

C ---            CAS D'UN STOCKAGE MORSE

                ELSE IF (ISTOC.EQ.2) THEN
                  CALL CRCSSM(ZI(IDABLO+IBLOC-1),ZI(IDHCOL),ZI(IDADIA),
     &                        ZI(LDDL),C8CST,ZR(IATMAT),ZC(IATRES))
                END IF

C ---          LE COEFFICIENT MULTIPLICATEUR EST DE TYPE INCONNU

              ELSE
                CH1 = TYPCST(IMAT) (1:1)
                CALL UTMESS('F','CBVALE_4','TYPE INCONNU: '//CH1)
              END IF

C ---        LA MATRICE A COMBINER EST COMPLEXE

            ELSE IF (TYPMAT(IMAT) (1:1).EQ.'C') THEN

C ---          LE COEFFICIENT MULTIPLICATEUR EST REEL

              IF (TYPCST(IMAT) (1:1).EQ.'R') THEN

C ---            CAS D'UN STOCKAGE EN LIGNE DE CIEL

                IF (ISTOC.EQ.1) THEN
                  CALL CCRSSL(ZI(IDABLO+IBLOC-1),ZI(IDHCOL),ZI(LDDL),
     &                        CONST(ICONST),ZC(IATMAT),ZC(IATRES))

C ---            CAS D'UN STOCKAGE MORSE

                ELSE IF (ISTOC.EQ.2) THEN
                  CALL CCRSSM(ZI(IDABLO+IBLOC-1),ZI(IDHCOL),ZI(IDADIA),
     &                        ZI(LDDL),CONST(ICONST),ZC(IATMAT),
     &                        ZC(IATRES))
                END IF

C ---          LE COEFFICIENT MULTIPLICATEUR EST REEL

              ELSE IF (TYPCST(IMAT) (:1).EQ.'C') THEN
                C8CST = DCMPLX(CONST(ICONST),CONST(ICONST+1))

C ---            CAS D'UN STOCKAGE EN LIGNE DE CIEL

                IF (ISTOC.EQ.1) THEN
                  CALL CCCSSL(ZI(IDABLO+IBLOC-1),ZI(IDHCOL),ZI(LDDL),
     &                        C8CST,ZC(IATMAT),ZC(IATRES))

C ---            CAS D'UN STOCKAGE MORSE

                ELSE IF (ISTOC.EQ.2) THEN
                  CALL CCCSSM(ZI(IDABLO+IBLOC-1),ZI(IDHCOL),ZI(IDADIA),
     &                        ZI(LDDL),C8CST,ZC(IATMAT),ZC(IATRES))
                END IF

C ---          LE COEFFICIENT MULTIPLICATEUR EST DE TYPE INCONNU

              ELSE
                CH1 = TYPCST(IMAT) (1:1)
                CALL UTMESS('F','CBVALE_5','TYPE INCONNU: '//CH1)
              END IF

C ---        LA MATRICE A COMBINER EST DE TYPE INCONNU

            ELSE
              CH1 = TYPMAT(IMAT) (1:1)
              CALL UTMESS('F','CBVALE_6','TYPE INCONNU: '//CH1)
            END IF
          END IF
          ICONST = ICONST + 1
          IF (TYPCST(IMAT) (1:1).EQ.'C') ICONST = ICONST + 1
          CALL JELIBE(JEXNUM(VALE,IBLOC))
          IF (MATSYM.EQ.0) THEN
            CALL JELIBE(JEXNUM(VALE,IBLOC+NBBLOC))
          END IF
C ---   FIN DE LA BOUCLE SUR LES MATRICES A COMBINER
   40   CONTINUE
        CALL JELIBE(JEXNUM(VALER,IBLOC))
        IF (TYPSYM.EQ.0) THEN
          CALL JELIBE(JEXNUM(VALER,IBLOC+NBBLOC))
        END IF
C --- FIN DE LA BOUCLE SUR LES BLOCS DE LA MATRICE RESULTAT
   50 CONTINUE

      CALL JELIBE(ZK24(IDREF2+2) (1:19)//'.HCOL')

      CALL JEDETR('&&CBVALE')


      CALL JEDEMA()
      END
