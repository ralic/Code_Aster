      SUBROUTINE VTCMBL(NBCMB,TYPCST,CONST,TYPECH,NOMCH,TYPRES,CHPRES)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           NBCMB
      REAL*8                         CONST(*)
      CHARACTER*(*)        TYPCST(*),TYPECH(*),NOMCH(*),TYPRES,CHPRES
C     ------------------------------------------------------------------
C     COMBINAISON LINEAIRE DE CHAM_NO OU DE CHAM_ELEM
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 11/09/2002   AUTEUR VABHHTS J.PELLET 
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
C     -----------------------------------------------------------------
C     *  LES CHAM_NOS OU CHAM_ELEMS SONT REELS OU COMPLEXES
C     *  LES SCALAIRES SONT REELS OU COMPLEXES
C     -----------------------------------------------------------------
C IN  : NBCOMB : IS  : NOMBRE DE CHAM_GDS A COMBINER
C IN  : TYPCST : K1  : TYPE DES CONSTANTES (R OU C, OU I )
C IN  : CONST  : R8  : TABLEAU DES COEFFICIENTS
C IN  : TYPECH : K1  : TYPE DES CHAM_GDS   (R OU C)
C IN  : NOMCH  : K19 : NOMS DES CHAM_GDS
C IN  : TYPRES : K1  : TYPE DU CHAMP RESULTAT (R OU C)
C IN  : CHPRES : K19 : NOM DU CHAMP RESULTAT
C     -----------------------------------------------------------------
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
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
      INTEGER      IBID
      COMPLEX*16   C8CST
      CHARACTER*4  DOCU, TYPE
      CHARACTER*5  REFE ,DESC,VALE
      CHARACTER*8  K8B
      CHARACTER*19 CH19
C     ------------------------------------------------------------------
      CALL JEMARQ()
      TYPE = TYPRES
      CH19 = NOMCH(1)

      CALL JEEXIN(CH19//'.DESC',IBID)
      IF (IBID.GT.0) THEN
        CALL JELIRA(CH19//'.DESC','DOCU',IBID,DOCU)
      ELSE
        CALL JELIRA(CH19//'.CELD','DOCU',IBID,DOCU)
      END IF

      IF (DOCU.EQ.'CHNO') THEN
         REFE = '.REFE'
         DESC = '.DESC'
         VALE = '.VALE'
      ELSEIF (DOCU.EQ.'CHML') THEN
         REFE = '.CELK'
         DESC = '.CELD'
         VALE = '.CELV'
      ELSE
         CALL UTMESS('F','VTCMBL','ON NE TRAITE QUE DES '//
     +                                  '"CHAM_NO" OU DES "CHAM_ELEM".')
      ENDIF
      CALL JELIRA(CH19//DESC  ,'LONMAX',NBDESC,K8B)
      CALL JELIRA(CH19//VALE  ,'LONMAX',NBVALE,K8B)
      CALL JELIRA(CH19//REFE,'LONMAX',NBREFE,K8B)
      CALL JEVEUO(CH19//DESC  ,'L',JDESC)
      CALL JEVEUO(CH19//REFE,'L',JREFE)
C
C --- CONSTRUCTION D'UN CHAM_GD RESULTAT SUR LE MODELE DE NOMCH(1)
C
      CH19 = CHPRES
      CALL JEEXIN(CH19//VALE,IRET)
      IF ( IRET .EQ. 0 ) THEN
        CALL WKVECT(CH19//DESC  ,'V V I'     ,NBDESC,KDESC)
        CALL WKVECT(CH19//VALE  ,'V V '//TYPE,NBVALE,KVALE)
        CALL WKVECT(CH19//REFE,'V V K24'   ,NBREFE,KREFE)
      ELSE
        CALL JEVEUO(CH19//DESC  ,'E',KDESC)
        CALL JEVEUO(CH19//VALE  ,'E',KVALE)
        CALL JEVEUO(CH19//REFE,'E',KREFE)
      ENDIF
      CALL JEECRA(CH19//DESC,'DOCU',IBID,DOCU)
      DO 2 I = 0,NBDESC-1
        ZI(KDESC+I) = ZI(JDESC+I)
 2    CONTINUE
      DO 4 I = 0,NBREFE-1
        ZK24(KREFE+I) = ZK24(JREFE+I)
 4    CONTINUE
C
C     --- CHANGER LA GRANDEUR ---
      CALL SDCHGD(CHPRES,TYPRES)
C
      CALL WKVECT('&&VTCMBL.VALE','V V '//TYPE,NBVALE,LVALE)
C
C --- BOUCLE SUR LES CHAM_GDS A COMBINER
C
      ICONST = 1
      DO 100 ICMB = 1,NBCMB
         CH19 = NOMCH(ICMB)
         CALL JEVEUO(CH19//VALE,'L',JVALE)
         IF (TYPRES(1:1).EQ.'R') THEN
            IF ( TYPECH(ICMB)(1:1) .EQ. 'R' ) THEN
               DO 110 IVAL = 0,NBVALE-1
                  ZR(LVALE+IVAL) = ZR(LVALE+IVAL) +
     +                            CONST(ICONST)*ZR(JVALE+IVAL)
  110          CONTINUE
            ELSE
               IF ( TYPCST(ICMB)(1:1) .EQ. 'R') THEN
                  DO 120 IVAL = 0,NBVALE-1
                     ZR(LVALE+IVAL) = ZR(LVALE+IVAL) +
     +                                CONST(ICONST)*ZC(JVALE+IVAL)
 120              CONTINUE
               ELSEIF ( TYPCST(ICMB)(1:1) .EQ. 'I') THEN
                  DO 130 IVAL = 0,NBVALE-1
                     ZR(LVALE+IVAL) = ZR(LVALE+IVAL) +
     +                              CONST(ICONST)*DIMAG(ZC(JVALE+IVAL))
 130              CONTINUE
               ELSE
                  TYPE = TYPCST(ICMB)(1:1)
                  CALL UTMESS('F','VTCMBL','TYPE INCONNU: '//TYPE)
               ENDIF
            ENDIF
         ELSE
            IF (TYPECH(ICMB) (1:1).EQ.'R') THEN
               IF (TYPCST(ICMB) (1:1).EQ.'R') THEN
                  DO 210 IVAL = 0,NBVALE - 1
                     ZC(LVALE+IVAL) = ZC(LVALE+IVAL) +
     +                                CONST(ICONST)*ZR(JVALE+IVAL)
  210             CONTINUE
               ELSEIF (TYPCST(ICMB) (1:1).EQ.'C') THEN
                  C8CST = DCMPLX(CONST(ICONST),CONST(ICONST+1))
                  DO 220 IVAL = 0,NBVALE - 1
                     ZC(LVALE+IVAL) = ZC(LVALE+IVAL) +
     +                                C8CST*ZR(JVALE+IVAL)
  220             CONTINUE
               ENDIF
            ELSE
               IF (TYPCST(ICMB) (1:1).EQ.'R') THEN
                  DO 310 IVAL = 0,NBVALE - 1
                     ZC(LVALE+IVAL) = ZC(LVALE+IVAL) +
     +                                CONST(ICONST)*ZC(JVALE+IVAL)
  310             CONTINUE
               ELSEIF (TYPCST(ICMB) (:1).EQ.'C') THEN
                  C8CST = DCMPLX(CONST(ICONST),CONST(ICONST+1))
                  DO 320 IVAL = 0,NBVALE - 1
                     ZC(LVALE+IVAL) = ZC(LVALE+IVAL) +
     +                                C8CST*ZC(JVALE+IVAL)
  320             CONTINUE
               ENDIF
            ENDIF
         ENDIF
         CALL JELIBE(CH19//VALE)
         ICONST = ICONST + 1
         IF (TYPCST(ICMB)(1:1).EQ.'C') ICONST = ICONST + 1
  100 CONTINUE
      IF (TYPE(1:1).EQ.'R') THEN
         DO 500 IVAL = 0,NBVALE-1
            ZR(KVALE+IVAL) = ZR(LVALE+IVAL)
 500     CONTINUE
      ELSEIF (TYPE(1:1).EQ.'C') THEN
         DO 510 IVAL = 0,NBVALE-1
            ZC(KVALE+IVAL) = ZC(LVALE+IVAL)
 510     CONTINUE
      ENDIF
      CH19 = CHPRES
      CALL JELIBE(CH19//VALE)
      CALL JEDETR('&&VTCMBL.VALE')
C
      CALL JEDEMA()
      END
