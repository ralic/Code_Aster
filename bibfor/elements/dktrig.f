      SUBROUTINE DKTRIG(NOMTE,XYZL,OPTION,PGL,RIG,ENER,MULTIC,GRILLE)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
      IMPLICIT  NONE
      REAL*8        XYZL(3,*), PGL(*), RIG(*), ENER(*)
      LOGICAL       GRILLE
      CHARACTER*16  OPTION , NOMTE
C     ------------------------------------------------------------------
C MODIF ELEMENTS  DATE 16/12/2004   AUTEUR VABHHTS J.PELLET 
C
C     MATRICE DE RIGIDITE DE L'ELEMENT DE PLAQUE DKT
C     ------------------------------------------------------------------
C     IN  XYZL   : COORDONNEES LOCALES DES TROIS NOEUDS
C     IN  OPTION : OPTION RIGI_MECA OU EPOT_ELEM_DEPL
C     IN  PGL    : MATRICE DE PASSAGE GLOBAL/LOCAL
C     IN  GRILLE : .TRUE. => ELEMENT DE GRILLE (MEGRDKT)
C     OUT RIG    : MATRICE DE RIGIDITE
C     OUT ENER   : TERMES POUR ENER_POT (EPOT_ELEM_DEPL)
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER      MULTIC, I, INT, JCOQU, JDEPG, LZR
      REAL*8       WGT,AIRE,DISTN,WGT0
      REAL*8       DM(9),DF(9),DMF(9),DF2(9),DMF2(9),DC(4),DCI(4)
      REAL*8       DMC(3,2),DFC(3,2)
      REAL*8       BF(3,9),BM(3,6)
      REAL*8       XAB1(3,9),DEPL(18)
      REAL*8       FLEX(81),MEMB(36),MEFL(54)
      REAL*8       BSIGTH(24), ENERTH, CTOR
      LOGICAL      ELASCO, INDITH
C     ------------------ PARAMETRAGE TRIANGLE --------------------------
      INTEGER   NPG,NC,NNO
      INTEGER   LDETJ,LJACO,LTOR,LQSI,LETA,LWGT,LXYC,LCOTE,LCOS,LSIN
      INTEGER   LAIRE
      PARAMETER (NPG=3)
      PARAMETER (NNO=3)
      PARAMETER (NC=3)
      PARAMETER (LDETJ=1)
      PARAMETER (LJACO=2)
      PARAMETER (LTOR=LJACO+4)
      PARAMETER (LQSI=LTOR+1)
      PARAMETER (LETA=LQSI+NPG+NNO)
      PARAMETER (LWGT=LETA+NPG+NNO)
      PARAMETER (LXYC=LWGT+NPG)
      PARAMETER (LCOTE=LXYC+2*NC)
      PARAMETER (LCOS=LCOTE+NC)
      PARAMETER (LSIN=LCOS+NC)
      PARAMETER (LAIRE=LSIN+NC)
C     ------------------------------------------------------------------
      ENERTH = 0.0D0
C
      CALL JEVETE('&INEL.'//NOMTE(1:8)//'.DESR','E',LZR)
C
      CALL JEVECH('PCACOQU','L',JCOQU)
C
      IF ( GRILLE ) THEN
         DISTN = ZR(JCOQU+3)
         CTOR = ZR(JCOQU+4)
      ELSE
         CTOR = ZR(JCOQU+3)
      ENDIF
C     ------ MISE A ZERO DES MATRICES : FLEX ET MEFL -------------------
      CALL R8INIR(81,0.D0,FLEX,1)
      CALL R8INIR(36,0.D0,MEMB,1)
      CALL R8INIR(54,0.D0,MEFL,1)

C     ----- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE TRIANGLE ----------
      CALL GTRIA3(XYZL,ZR(LZR))
C
C     CALCUL DES MATRICES DE RIGIDITE DU MATERIAU EN FLEXION
C     MEMBRANE ET CISAILLEMENT INVERSEE
      CALL DXMATE(DF,DM,DMF,DC,DCI,DMC,DFC,NNO,PGL,ZR(LZR),MULTIC,
     +            GRILLE,ELASCO)
C     ------------------------------------------------------------------
C     CALCUL DE LA MATRICE DE RIGIDITE DE L'ELEMENT EN MEMBRANE
C     ------------------------------------------------------------------
C
C     ------ CALCUL DE LA MATRICE BM -----------------------------------
      CALL DXTBM(ZR(LZR),BM)
      AIRE = ZR(LZR-1+LAIRE)
C
C     ------ CALCUL DU PRODUIT BMT.DM.BM -------------------------------
      CALL DCOPY(9,DM,1,DMF2,1)
      CALL DSCAL(9,AIRE,DMF2,1)
      CALL UTBTAB('ZERO',3,6,DMF2,BM,XAB1,MEMB)
C
C     ------------------------------------------------------------------
C     CALCUL DES MATRICES DE RIGIDITE DE L'ELEMENT EN FLEXION ET
C     COUPLAGE MEMBRANE/FLEXION
C     ------------------------------------------------------------------
      DO 10 INT = 1,NPG
        WGT = ZR(LZR-1+LWGT+INT-1)*ZR(LZR-1+LDETJ)
C        ----- CALCUL DE LA MATRICE BF AU POINT QSI ETA ------------
        CALL DKTBF(INT,ZR(LZR),BF)
C        ----- CALCUL DU PRODUIT BFT.DF.BF -------------------------
        CALL DCOPY(9,DF,1,DF2,1)
        CALL DSCAL(9,WGT,DF2,1)
        CALL UTBTAB('CUMU',3,9,DF2,BF,XAB1,FLEX)
        IF (MULTIC.EQ.2) THEN
C        ----- CALCUL DU PRODUIT BMT.DMF.BF ------------------------
          CALL DCOPY(9,DMF,1,DMF2,1)
          CALL DSCAL(9,WGT,DMF2,1)
          CALL UTCTAB('CUMU',3,9,6,DMF2,BF,BM,XAB1,MEFL)
        END IF
C
        IF ( GRILLE ) THEN
          WGT0 = DISTN*DISTN*WGT
          CALL DCOPY(9,DM,1,DF2,1)
          CALL DSCAL(9,WGT0,DF2,1)
          CALL UTBTAB('CUMU',3,9,DF2,BF,XAB1,FLEX)
          WGT0 = DISTN*WGT
          CALL DCOPY(9,DM,1,DMF2,1)
          CALL DSCAL(9,WGT0,DMF2,1)
          CALL UTCTAB('CUMU',3,9,6,DMF2,BF,BM,XAB1,MEFL)
        END IF
   10 CONTINUE
C
      IF ( OPTION .EQ. 'RIGI_MECA' ) THEN
         CALL DXTLOC(FLEX,MEMB,MEFL,CTOR,RIG)
      ELSEIF ( OPTION .EQ. 'EPOT_ELEM_DEPL' ) THEN
         CALL JEVECH('PDEPLAR','L',JDEPG)
         CALL UTPVGL(3,6,PGL,ZR(JDEPG),DEPL)
         CALL DXTLOE(FLEX,MEMB,MEFL,CTOR,MULTIC,DEPL,ENER)
         CALL BSTHPL(NOMTE(1:8),BSIGTH,INDITH)
         IF (INDITH) THEN
           DO 20 I = 1, 18
            ENERTH = ENERTH + DEPL(I)*BSIGTH(I)
  20       CONTINUE
           ENER(1) = ENER(1) - ENERTH
         ENDIF
      ENDIF
C
      END
