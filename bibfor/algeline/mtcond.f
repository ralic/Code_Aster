      SUBROUTINE MTCOND(LMAT,LABASE)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER LMAT
      CHARACTER*(*) LABASE
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
C     ------------------------------------------------------------------
C     CONDITIONNEMENT (DIAGONAL) DE LA MATRICE POINTEE PAR LMAT
C     CREATION D'UN CHAMP "NOMMAT(K19).&CDI"
C     ------------------------------------------------------------------
C LABASE : BASE SUR LAQUELLE EST CREE LE TABLEAU CONTENANT LE CONDITION.

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
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------

C     ------------------------------------------------------------------
      INTEGER LVECOU,LVECIN
      REAL*8 R8VAL,UN,ZERO
      COMPLEX*16 C8VAL,CUN
      CHARACTER*1 BASE,TYPE(2),CBID
      CHARACTER*24 COND,VALE
      CHARACTER*32 JEXNUM
C     ------------------------------------------------------------------
      DATA TYPE/'R','C'/
C     ------------------------------------------------------------------
      CALL JEMARQ()
      BASE = LABASE(1:1)
      UN = 1.D0
      ZERO = 0.D0
      CUN = DCMPLX(UN,ZERO)

C     --- (ADRESSE-1) DES TABLEAUX  ADIA, HCOL, ABLO  ---
      CALL MTDSC2(ZK24(ZI(LMAT+1)),'ADIA','L',IADIA)
      IADIA = IADIA - 1
      CALL JEVEUO(ZK24(ZI(LMAT+1)) (1:19)//'.REFA','L',IDREFE)
      CALL JEVEUO(ZK24(IDREFE+2) (1:19)//'.HCOL','L',IHCOL)
      IHCOL = IHCOL - 1
      CALL MTDSC2(ZK24(ZI(LMAT+1)),'ABLO','L',IABLO)
      IABLO = IABLO - 1

C     --- NOMBRE D'EQUATION ET DE BLOC ---
      NEQ = ZI(LMAT+2)
      NBBLOC = ZI(LMAT+13)

C     --- TYPE DE STOCKAGE ---
      ISTOC = ZI(LMAT+6)

C     CREATION DU TABLEAU DIAGONALE DU CONDITIONNEMENT
      COND = ZK24(ZI(LMAT+1)) (1:19)//'.COND'
      CALL JECREO(COND,BASE//' V '//TYPE(ZI(LMAT+3)))
      CALL JEECRA(COND,'LONMAX',NEQ,CBID)
      CALL JEVEUO(COND,'E',LCOND)

      VALE = ZK24(ZI(LMAT+1)) (1:19)//'.VALE'
      IF (ISTOC.EQ.1) THEN
        IF (TYPE(ZI(LMAT+3)).EQ.'R') THEN
          DO 30 IBLOC = 1,NBBLOC
            CALL JEVEUO(JEXNUM(VALE,IBLOC),'E',LVALE)
            LVALE = LVALE - 1
            DO 20 IEQUA = ZI(IABLO+IBLOC) + 1,ZI(IABLO+IBLOC+1)
              IPOS = LVALE + ZI(IADIA+IEQUA)
              IF (ZR(IPOS).NE.0.D0) THEN
                R8VAL = UN/SQRT(ABS(ZR(IPOS)))
              ELSE
                R8VAL = UN
              END IF
              ZR(LCOND+IEQUA-1) = R8VAL
              ILONG = ZI(IHCOL+IEQUA)
              IDE = LVALE + ZI(IADIA+IEQUA) - ILONG + 1
              JEQUA = LCOND + IEQUA - ILONG
              DO 10 I = 0,ILONG - 1
                ZR(IDE+I) = ZR(IDE+I)*ZR(JEQUA+I)*R8VAL
   10         CONTINUE
   20       CONTINUE
            CALL JELIBE(JEXNUM(VALE,IBLOC))
   30     CONTINUE

        ELSE IF (TYPE(ZI(LMAT+3)).EQ.'C') THEN

          DO 60 IBLOC = 1,NBBLOC
            CALL JEVEUO(JEXNUM(VALE,IBLOC),'E',LVALE)
            LVALE = LVALE - 1
            DO 50 IEQUA = ZI(IABLO+IBLOC) + 1,ZI(IABLO+IBLOC+1)
              IPOS = LVALE + ZI(IADIA+IEQUA)
              IF (ZC(IPOS).NE.0.D0) THEN
                C8VAL = DCMPLX(UN/SQRT(ABS(ZC(IPOS))),ZERO)
              ELSE
                C8VAL = CUN
              END IF
              ZC(LCOND+IEQUA-1) = C8VAL
              ILONG = ZI(IHCOL+IEQUA)
              IDE = LVALE + ZI(IADIA+IEQUA) - ILONG + 1
              JEQUA = LCOND + IEQUA - ILONG
              DO 40 I = 0,ILONG - 1
                ZC(IDE+I) = ZC(IDE+I)*ZC(JEQUA+I)*C8VAL
   40         CONTINUE
   50       CONTINUE
            CALL JELIBE(JEXNUM(VALE,IBLOC))
   60     CONTINUE
        END IF
      ELSE IF (ISTOC.EQ.2) THEN
        IF (TYPE(ZI(LMAT+3)).EQ.'R') THEN
          DO 90 IBLOC = 1,NBBLOC
            CALL JEVEUO(JEXNUM(VALE,IBLOC),'E',LVALE)
            LVALE = LVALE - 1
            KIN = 0
            IDEBLI = 1
            DO 80 IEQUA = 1,NEQ
              IPOS = LVALE + ZI(IADIA+IEQUA)
              IF (ZR(IPOS).NE.0.D0) THEN
                R8VAL = UN/SQRT(ABS(ZR(IPOS)))
              ELSE
                R8VAL = UN
              END IF
              ZR(LCOND+IEQUA-1) = R8VAL
              IFINLI = ZI(IADIA+IEQUA)
              DO 70 IND = IDEBLI,IFINLI
                KIN = KIN + 1
                INDCOL = ZI(IHCOL+IND)
                ZR(LVALE+KIN) = ZR(LVALE+KIN)*ZR(LCOND+INDCOL-1)*R8VAL
   70         CONTINUE
              IDEBLI = ZI(IADIA+IEQUA) + 1
   80       CONTINUE
            CALL JELIBE(JEXNUM(VALE,IBLOC))
   90     CONTINUE

        ELSE IF (TYPE(ZI(LMAT+3)).EQ.'C') THEN

          DO 120 IBLOC = 1,NBBLOC
            CALL JEVEUO(JEXNUM(VALE,IBLOC),'E',LVALE)
            LVALE = LVALE - 1
            KIN = 0
            IDEBLI = 1
            DO 110 IEQUA = 1,NEQ
              IPOS = LVALE + ZI(IADIA+IEQUA)
              IF (ZC(IPOS).NE.0.D0) THEN
                C8VAL = DCMPLX(UN/SQRT(ABS(ZC(IPOS))),ZERO)
              ELSE
                C8VAL = CUN
              END IF
              ZC(LCOND+IEQUA-1) = C8VAL
              IFINLI = ZI(IADIA+IEQUA)
              DO 100 IND = IDEBLI,IFINLI
                KIN = KIN + 1
                INDCOL = ZI(IHCOL+IND)
                ZC(LVALE+KIN) = ZC(LVALE+KIN)*ZC(LCOND+INDCOL-1)*C8VAL
  100         CONTINUE
              IDEBLI = ZI(IADIA+IEQUA) + 1
  110       CONTINUE
            CALL JELIBE(JEXNUM(VALE,IBLOC))
  120     CONTINUE
        END IF
      END IF
      CALL JELIBE(COND)
      CALL JELIBE(ZK24(IDREFE+2) (1:19)//'.HCOL')
      CALL JEDEMA()
      END
