      SUBROUTINE MTCMBI(TYPMAT,LMAT,COEF,CCOEF,LRES)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER LMAT,LRES
      CHARACTER*(*) TYPMAT
      COMPLEX*16 CCOEF
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
C     DUPLIQUE LA MATRICE EN METTANT TOUTES LES TERMES A ZERO SAUF
C     LES "LAGRANGE" EN LEUR APPLIQUANT UN COEFFICIENT.
C     -----------------------------------------------------------------
C IN  K* TYPMAT = TYPE DE MATRICE   (R OU C)
C IN  I  LMAT   = POINTEUR DE MATRICE
C IN  I  LRES   = POINTEUR DE MATRICE RESULTAT
C     -----------------------------------------------------------------
C     NBBLOC = NOMBRE DE BLOCS POUR CONSTITUER UNE MATRICE (.VALE)
C     NBBLIC = NOMBRE DE BLOCS POUR .VALI DE LA MATRICE
C     LGBLOC = LONGUEUR DES BLOCS
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
      INTEGER NBBLOC,LGBLOC,NBBLIC,HCOL
      REAL*8 CONST(2)
      CHARACTER*1 CH1,TYPCST
      CHARACTER*4 CBID,ETAMAT
      CHARACTER*8 NOMDDL
      CHARACTER*14 NUME
      CHARACTER*19 MATRES,NOMA
      CHARACTER*24 VALE,VALER
      COMPLEX*16 CZERO
      LOGICAL MATSYM
C     -----------------------------------------------------------------
      CALL JEMARQ()

      IF (TYPMAT(1:1).NE.'R' .AND. TYPMAT(1:1).NE.'C') THEN
        CH1 = TYPMAT(1:1)
        CALL UTMESS('F','MTCMBI','TYPE DE MATRICE "'//CH1//'" INCONNU.')
      END IF

C     --- AFFE_CHAR_CINE ? ---

      IF (ZI(LMAT+7).NE.0) THEN
        CALL UTMESS('F','MTCOMBI','ON NE TRAITE PAS CETTE OPTION.')
      END IF

      ZERO = 0.D0
      CZERO = DCMPLX(ZERO,ZERO)
      MATSYM = .TRUE.

      IF (ZI(LMAT+4).NE.1) MATSYM = .FALSE.
      NOMA = ZK24(ZI(LMAT+1)) (1:19)
      VALE = NOMA//'.VALE'

      NEQ = ZI(LRES+2)
      ISTOC = ZI(LRES+6)
      CALL MTDSC2(ZK24(ZI(LRES+1)),'ADIA','L',IDADIA)
      CALL MTDSC2(ZK24(ZI(LRES+1)),'ABLO','L',IDABLO)
      NBBLOC = ZI(LRES+13)
      LGBLOC = ZI(LRES+14)
      MATRES = ZK24(ZI(LRES+1)) (1:19)
      VALER = MATRES//'.VALE'

C     --- NOM DE LA NUMEROTATION ASSOCIEE A LA MATRICE ---
      CALL DISMOI('F','NOM_NUME_DDL',NOMA,'MATR_ASSE',IBID,NUME,IERD)

      CALL JELIRA(NOMA//'.REFA','DOCU',IBID,ETAMAT)
      CALL JEECRA(MATRES//'.REFA','DOCU',IBID,ETAMAT)

C     --- TOUTES COMPOSANTES A ZERO SAUF LES LAGRANGES ---
      NOMDDL = 'LAGR    '
      MXDDL = 1
      CALL WKVECT('&&MTCMBI','V V I',NEQ*MXDDL,LDDL)
      CALL PTEDDL('NUME_DDL',NUME,MXDDL,NOMDDL,NEQ,ZI(LDDL))
      DO 10 I = 0,NEQ - 1
        ZI(LDDL+I) = 1 - ZI(LDDL+I)
   10 CONTINUE

      CALL JEVEUO(ZK24(ZI(LRES+1)) (1:19)//'.REFA','L',IDREF2)
      CALL JEVEUO(ZK24(IDREF2+2) (1:19)//'.HCOL','L',IDHCOL)

      DO 170 IBLOC = 1,NBBLOC

        CALL JEVEUO(JEXNUM(VALER,IBLOC),'E',IATRES)
        IF (.NOT.MATSYM) THEN
          CALL JEVEUO(JEXNUM(VALER,IBLOC+NBBLOC),'E',IATREI)
        END IF

        IF (TYPMAT(1:1).EQ.'R') THEN
          DO 20 IVAL = IATRES,IATRES + LGBLOC - 1
            ZR(IVAL) = ZERO
   20     CONTINUE
          IF (.NOT.MATSYM) THEN
            DO 30 IVAL = IATREI,IATREI + LGBLOC - 1
              ZR(IVAL) = ZERO
   30       CONTINUE
          END IF
        ELSE
          DO 40 IVAL = IATRES,IATRES + LGBLOC - 1
            ZC(IVAL) = CZERO
   40     CONTINUE
        END IF

        CALL JEVEUO(JEXNUM(VALE,IBLOC),'L',IATMAT)
        IF (.NOT.MATSYM) THEN
          CALL JEVEUO(JEXNUM(VALE,IBLOC+NBBLOC),'E',IATMAI)
        END IF


        IF (TYPMAT(1:1).EQ.'R') THEN

C          --- STOCKAGE EN LIGNE DE CIEL ---

          IF (ISTOC.EQ.1) THEN

C             --- MATRICE SYMETRIQUE ---

            IF (MATSYM) THEN

C               --- NUMERO DE LA PREMIERE EQUATION DU BLOC COURANT
              IL1 = ZI(IDABLO+IBLOC-1) + 1

C               --- NUMERO DE LA DERNIERE EQUATION DU BLOC COURANT
              IL2 = ZI(IDABLO+IBLOC-1+1)
              KIN = 0
              DO 60 IEQUA = IL1,IL2

C                  --- LONGUEUR DE LA LIGNE COURANTE
                ILONG = ZI(IDHCOL+IEQUA-1)

C                  --- ADRESSE DU TERME DIAGONAL COURANT DANS LE BLOC
                DO 50 IND = 1,ILONG
                  KIN = KIN + 1
                  ICOEF = MIN((2-ZI(LDDL+IEQUA-ILONG+IND-1)-ZI(LDDL+
     &                    IEQUA-1)),1)
                  ZR(IATRES+KIN-1) = ZR(IATRES+KIN-1) +
     &                               ZR(IATMAT+KIN-1)*ICOEF*COEF
   50           CONTINUE
   60         CONTINUE

C             --- MATRICE NON-SYMETRIQUE

            ELSE
C               --- NUMERO DE LA PREMIERE EQUATION DU BLOC COURANT
              IL1 = ZI(IDABLO+IBLOC-1) + 1
C               --- NUMERO DE LA DERNIERE EQUATION DU BLOC COURANT
              IL2 = ZI(IDABLO+IBLOC-1+1)
              KIN1 = 0
              KIN2 = 0
              DO 100 IEQUA = IL1,IL2
C                  --- LONGUEUR DE LA LIGNE COURANTE
                ILONG = ZI(IDHCOL+IEQUA-1)
C                  --- ADRESSE DU TERME DIAGONAL COURANT DANS LE BLOC
                DO 70 IND = 1,ILONG
                  KIN1 = KIN1 + 1
                  ICOEF = MIN((2-ZI(LDDL+IEQUA-ILONG+IND-1)-ZI(LDDL+
     &                    IEQUA-1)),1)
                  ZR(IATRES+KIN1-1) = ZR(IATRES+KIN1-1) +
     &                                ZR(IATMAT+KIN1-1)*ICOEF*COEF
   70           CONTINUE
                IF (MATSYM) THEN
                  DO 80 IND = 1,ILONG
                    KIN2 = KIN2 + 1
                    ZR(IATREI+KIN2-1) = ZR(IATRES+KIN2-1)
   80             CONTINUE
                ELSE
                  DO 90 IND = 1,ILONG
                    KIN2 = KIN2 + 1
                    ICOEF = MIN((2-ZI(LDDL+IEQUA-ILONG+IND-1)-ZI(LDDL+
     &                      IEQUA-1)),1)
                    ZR(IATREI+KIN2-1) = ZR(IATREI+KIN2-1) +
     &                                  ZR(IATMAI+KIN2-1)*ICOEF*COEF
   90             CONTINUE
                END IF
  100         CONTINUE
            END IF

C          --- STOCAGE MORSE ---

          ELSE IF (ISTOC.EQ.2) THEN
            KIN = 0
            IDEBLI = 1
            DO 120 IEQUA = 1,NEQ
              IFINLI = ZI(IDADIA+IEQUA-1)
              DO 110 IND = IDEBLI,IFINLI
                KIN = KIN + 1
                ICOEF = MIN((2-ZI(LDDL+IEQUA-IFINLI+IND-1)-ZI(LDDL+
     &                  IEQUA-1)),1)
                ZR(IATRES+KIN-1) = ZR(IATRES+KIN-1) +
     &                             ZR(IATMAT+KIN-1)*ICOEF*COEF
  110         CONTINUE
              IDEBLI = ZI(IDADIA+IEQUA-1) + 1
  120       CONTINUE
          END IF

C         --- MATRICE COMPLEXE ---

        ELSE IF (TYPMAT(1:1).EQ.'C') THEN

C           --- STOCKAGE EN LIGNE DE CIEL ---

          IF (ISTOC.EQ.1) THEN

C            --- NUMERO DE LA PREMIERE EQUATION DU BLOC COURANT
            IL1 = ZI(IDABLO+IBLOC-1) + 1

C            --- NUMERO DE LA DERNIERE EQUATION DU BLOC COURANT
            IL2 = ZI(IDABLO+IBLOC-1+1)
            KIN = 0
            DO 140 IEQUA = IL1,IL2

C               --- LONGUEUR DE LA LIGNE COURANTE
              ILONG = ZI(IDHCOL+IEQUA-1)

C               --- ADRESSE DU TERME DIAGONAL COURANT DANS LE BLOC
              DO 130 IND = 1,ILONG
                KIN = KIN + 1
                ICOEF = MIN((2-ZI(LDDL+IEQUA-ILONG+IND-1)-ZI(LDDL+IEQUA-
     &                  1)),1)
                ZC(IATRES+KIN-1) = ZC(IATRES+KIN-1) +
     &                             ZC(IATMAT+KIN-1)*ICOEF*CCOEF
  130         CONTINUE
  140       CONTINUE

C         --- STOCAGE MORSE ---

          ELSE IF (ISTOC.EQ.2) THEN
            KIN = 0
            IDEBLI = 1
            DO 160 IEQUA = 1,NEQ
              IFINLI = ZI(IDADIA+IEQUA-1)
              DO 150 IND = IDEBLI,IFINLI
                KIN = KIN + 1
                ICOEF = MIN((2-ZI(LDDL+IEQUA-IFINLI+IND-1)-ZI(LDDL+
     &                  IEQUA-1)),1)
                ZC(IATRES+KIN-1) = ZC(IATRES+KIN-1) +
     &                             ZC(IATMAT+KIN-1)*ICOEF*CCOEF
  150         CONTINUE
              IDEBLI = ZI(IDADIA+IEQUA-1) + 1
  160       CONTINUE
          END IF
        END IF

        CALL JELIBE(JEXNUM(VALE,IBLOC))
        IF (.NOT.MATSYM) THEN
          CALL JELIBE(JEXNUM(VALE,IBLOC+NBBLOC))
        END IF
        CALL JELIBE(JEXNUM(VALER,IBLOC))
        IF (.NOT.MATSYM) THEN
          CALL JELIBE(JEXNUM(VALER,IBLOC+NBBLOC))
        END IF

  170 CONTINUE

C     --- ACTUALISATION DU .CONL ----
      NBCOMB = 1
      IF (TYPMAT(1:1).EQ.'R') THEN
        TYPCST = 'R'
        CONST(1) = 1.D0
      ELSE
        TYPCST = 'C'
        CONST(1) = 1.D0
        CONST(2) = 1.D0
      END IF
      CALL MTCONL(NBCOMB,TYPCST,CONST,TYPMAT,LMAT,TYPMAT,LRES)

      CALL JEDETR('&&MTCMBI')
      CALL JELIBE(ZK24(IDREF2+2) (1:19)//'.HCOL')


      CALL JEDEMA()
      END
