      SUBROUTINE REGELE(OPTION,TYPMOD,NPI,NDIM,DIMUEL,NDDLS,NDDLM,NNO,
     +                  NNOS,NNOM,AXI,REGULA,DIMCON,IPOIDS,IPOID2,IVF,
     +                  IVF2,IDFDE,IDFDE2,COMPOR,GEOM,DEPLP,CONTP,IMATE,
     +                  DIMDEF,MATUU,VECTU)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/12/2010   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C TOLE CRP_21 CRS_1404
C ======================================================================
      IMPLICIT      NONE
      LOGICAL       AXI
      INTEGER       NPI,IPOIDS,IPOID2,IVF,IVF2,IDFDE,IDFDE2,NDDLS,NDDLM
      INTEGER       IMATE,DIMDEF,NDIM,NNO,NNOM,NNOS,DIMUEL,DIMCON
      INTEGER       REGULA(6)
      REAL*8        VECTU(DIMUEL),MATUU(DIMUEL*DIMUEL),CONTP(DIMCON*NPI)
      REAL*8        GEOM(NDIM,*),DEPLP(DIMUEL)
      CHARACTER*8   TYPMOD(2)
      CHARACTER*16  OPTION,COMPOR(*)
C ======================================================================
C --- BUT : CALCUL ELEMENTAIRE AUX POINTS D'INTEGRATION ----------------
C ======================================================================
C --- VARIABLES LOCALES ------------------------------------------------
C ======================================================================
      INTEGER       I,J,N,KPI
      REAL*8        B(DIMDEF,DIMUEL),POIDS,POIDS2,R(DIMDEF)
      REAL*8        DEFGEP(DIMDEF),DRDE(DIMDEF,DIMDEF)
C ======================================================================
C --- INITIALISATION DE VECTU, MATUU A 0 SUIVANT OPTION ----------------
C ======================================================================
      IF (OPTION(1:9).EQ.'RIGI_MECA') THEN
         DO 30 I=1,DIMUEL*DIMUEL
            MATUU(I)=0.0D0
 30      CONTINUE
      ELSE IF (OPTION(1:9).EQ.'RAPH_MECA') THEN
         DO 40 I=1,DIMUEL
            VECTU(I)=0.0D0
 40      CONTINUE
      ELSE IF (OPTION(1:9).EQ.'FULL_MECA') THEN
         DO 50 I=1,DIMUEL*DIMUEL
            MATUU(I)=0.0D0
 50      CONTINUE
         DO 60 I=1,DIMUEL
            VECTU(I)=0.0D0
 60      CONTINUE
      ENDIF
C ======================================================================
C --- BOUCLE SUR LES POINTS D'INTEGRATION ------------------------------
C ======================================================================
      DO 100 KPI=1,NPI
C ======================================================================
C --- DEFINITION DE L'OPERATEUR B (DEFINI PAR E=B.U) -------------------
C ======================================================================
         CALL CABR2G(KPI,IPOIDS,IPOID2,IVF,IVF2,IDFDE,IDFDE2,GEOM,
     +               DIMDEF,DIMUEL,NDIM,NDDLS,NDDLM,NNO,NNOS,NNOM,AXI,
     +               REGULA,B,POIDS,POIDS2)
C ======================================================================
C --- CALCUL DES DEFORMATIONS GENERALISEES E=B.U -----------------------
C ======================================================================
         DO 10 I=1,DIMDEF
            DEFGEP(I)=0.0D0
            DO 20 N=1,DIMUEL
               DEFGEP(I)=DEFGEP(I)+B(I,N)*DEPLP(N)
 20         CONTINUE
 10      CONTINUE
C ======================================================================
C --- CALCUL DES CONTRAINTES VIRTUELLES ET GENERALISEES ----------------
C --- ET DE LEURS DERIVEES ---------------------------------------------
C ======================================================================
         CALL EQUREG(IMATE,OPTION,COMPOR,REGULA,DIMDEF,DIMCON,DEFGEP,
     +               NDIM,CONTP((KPI-1)*DIMCON+1),R,DRDE)
C ======================================================================
      IF (OPTION(1:9).EQ.'RIGI_MECA' .OR.
     +    OPTION(1:9).EQ.'FULL_MECA' ) THEN
C ======================================================================
C --- CALCUL DE SOM_PG(POIDS_PG*BT_PG*DRDE_PG*B_PG) --------------------
C ======================================================================
         CALL DILOPT(DIMDEF,DIMUEL,POIDS,POIDS2,B,DRDE,MATUU)
      ENDIF
C ======================================================================
      IF (OPTION(1:9).EQ.'RAPH_MECA' .OR.
     +    OPTION(1:9).EQ.'FULL_MECA' ) THEN
C ======================================================================
C --- CALCUL DE SOM_PG(POIDS_PG*BT_PG*R_PG) ----------------------------
C ======================================================================
         CALL DILSGA(DIMDEF,DIMUEL,POIDS,POIDS2,B,R,VECTU)
      ENDIF
C ======================================================================
 100  CONTINUE
C ======================================================================
      END
