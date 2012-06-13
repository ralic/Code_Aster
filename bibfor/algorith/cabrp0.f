      SUBROUTINE CABRP0(KPI,IPOIDS,IPOID2,IVF,IVF2,IDFDE,IDFDE2,GEOM,
     +                  DIMDEF,DIMUEL,NDIM,NDDLS,NDDLM,NNO,NNOS,NNOM,
     +                  AXI,REGULA,B,POIDS,POIDS2)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT  NONE
      INCLUDE 'jeveux.h'
      LOGICAL   AXI
      INTEGER   KPI,IPOIDS,IPOID2,IDFDE,IDFDE2,NDIM,REGULA(6),DIMDEF,IVF
      INTEGER   IVF2,NNO,NNOS,NNOM,NDDLS,NDDLM,DIMUEL
      REAL*8    GEOM(NDIM,*),POIDS,POIDS2,B(DIMDEF,DIMUEL)
C ======================================================================
C --- BUT : CALCUL DE L'OPERATEUR B POUR LA MODELISATION SECOND GRADIENT
C ---       A MICRO DILATATION AVEC MULTIPLICATEURS DE LAGRANGE --------
C ======================================================================
C --- IN ---------------------------------------------------------------
C --- NBDDL  : VECTEUR DIMENSION DU NOMBRE DE DDLS ---------------------
C --- NBNO   : VECTEUR DIMENSION DU NOMBRE DE NOEUDS -------------------
C --- KPI    : INDICE DU POINT D'INTEGRATION ---------------------------
C --- IPOIDS : ADRESSE DES FONCTIONS POIDS D'ORDRE 2 -------------------
C --- IPOID2 : ADRESSE DES FONCTIONS POIDS D'ORDRE 1 -------------------
C --- IVF2   : ADRESSE DES FONCTIONS DE FORME D'ORDRE 1 ----------------
C --- IDFDE  : ADRESSE DES DERIVEES DES FONCTIONS DE FORME D'ORDRE 2 ---
C --- IDFDE2 : ADRESSE DES DERIVEES DES FONCTIONS DE FORME D'ORDRE 1 ---
C --- GEOM   : CARACTERISTIQUES GEOMETRIQUES DE L'ELEMENT REEL ---------
C --- DIMDEF : DIMENSION DU VECTEUR DES DEFORMATIONS GENERALISEES ------
C --- NDIM   : DIMENSION DU PROBLEME -----------------------------------
C --- OUT --------------------------------------------------------------
C --- B      : OPERATEUR B DEFINI TEL QUE E=B.U ------------------------
C --- POIDS  : POIDS ASSOCIE AUX FONCTIONS DE FORME D'ORDRE 2 ----------
C --- POIDS2 : POIDS ASSOCIE AUX FONCTIONS DE FORME D'ORDRE 1 ----------
C ======================================================================
C ======================================================================
C --- VARIABLES LOCALES ------------------------------------------------
C ======================================================================
      INTEGER      I,N,ADDER1,ADDER2,ADDER3
      REAL*8       DFDI(NNO,3),DFDI2(NNOS,3)
C ======================================================================
C --- INITIALISATION DE LA MATRICE B -----------------------------------
C ======================================================================
      CALL MATINI(DIMDEF,DIMUEL,0.D0,B)
      ADDER1 = REGULA(1)
      ADDER2 = REGULA(2)
      ADDER3 = REGULA(3)
C ======================================================================
C --- CAS 2D -----------------------------------------------------------
C ======================================================================
      IF (NDIM.EQ.2) THEN
C ======================================================================
C --- CAS QUADRATIQUES -------------------------------------------------
C ======================================================================
         CALL DFDM2D(NNO,KPI,IPOIDS,IDFDE,GEOM,DFDI(1,1),DFDI(1,2),
     +                                                            POIDS)
C ======================================================================
C --- CAS LINEAIRES ----------------------------------------------------
C ======================================================================
         CALL DFDM2D(NNOS,KPI,IPOID2,IDFDE2,GEOM,DFDI2(1,1),DFDI2(1,2),
     +                                                           POIDS2)
      ELSE IF (NDIM.EQ.3) THEN
C ======================================================================
C --- CAS QUADRATIQUES -------------------------------------------------
C ======================================================================
         CALL DFDM3D (NNO,KPI,IPOIDS,IDFDE,
     +                         GEOM,DFDI(1,1),DFDI(1,2),DFDI(1,3),POIDS)
C ======================================================================
C --- CAS LINEAIRES ----------------------------------------------------
C ======================================================================
         CALL DFDM3D (NNOS,KPI,IPOID2,IDFDE2,GEOM,DFDI2(1,1),
     +                                     DFDI2(1,2),DFDI2(1,3),POIDS2)
      ELSE
         CALL U2MESS('F','ALGORITH6_13')
      ENDIF
C ======================================================================
C --- REMPLISSAGE DE L OPERATEUR B -------------------------------------
C ======================================================================
C --- SUR LES NOEUDS SOMMETS -------------------------------------------
C ======================================================================
      DO 10 N=1,NNOS
         DO 20 I=1,NDIM
            B(ADDER1,(N-1)*NDDLS+I) = B(ADDER1,(N-1)*NDDLS+I)-DFDI(N,I)
 20      CONTINUE
         B(ADDER1,(N-1)*NDDLS+NDIM+1) = B(ADDER1,(N-1)*NDDLS+NDIM+1) +
     +                                  ZR(IVF2+N+(KPI-1)*NNOS-1)
 10   CONTINUE
C ======================================================================
C --- SUR LES NOEUDS MILIEUX -------------------------------------------
C ======================================================================
      DO 30 N=1,NNOM
         DO 40 I=1,NDIM
               B(ADDER1,NNOS*NDDLS+(N-1)*NDDLM+I)=
     +         B(ADDER1,NNOS*NDDLS+(N-1)*NDDLM+I)-DFDI(N+NNOS,I)
 40      CONTINUE
 30   CONTINUE
C ======================================================================
C --- POUR LES GRADIENTS DE VARIATIONS VOLUMIQUE ET LES VAR VOL --------
C --- ON UTILISE LES FONCTIONS DE FORME D'ORDRE 1 ----------------------
C ======================================================================
C --- SUR LES NOEUDS SOMMETS -------------------------------------------
C ======================================================================
      DO 50 N=1,NNOS
         DO 60 I=1,NDIM
            B(ADDER2-1+I,(N-1)*NDDLS+NDIM+1)=
     +      B(ADDER2-1+I,(N-1)*NDDLS+NDIM+1)+DFDI2(N,I)
 60      CONTINUE
 50   CONTINUE
C ======================================================================
C --- POUR LE MULTIPLICATEUR DE LAGRANGE -------------------------------
C --- (PRES) -----------------------------------------------------------
C --- ON UTILISE LES FONCTIONS DE FORME D'ORDRE 0 ----------------------
C ======================================================================
C --- SUR LES NOEUDS CENTRAUX ------------------------------------------
C ======================================================================
      B(ADDER3,DIMUEL)=1.0D0
C ======================================================================
      END
