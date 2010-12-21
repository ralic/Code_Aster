      SUBROUTINE CABR2G(KPI,IPOIDS,IPOID2,IVF,IVF2,IDFDE,IDFDE2,GEOM,
     +                  DIMDEF,DIMUEL,NDIM,NDDLS,NDDLM,NNO,NNOS,NNOM,
     +                  AXI,REGULA,B,POIDS,POIDS2)
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
      IMPLICIT  NONE
      LOGICAL   AXI
      INTEGER   KPI,IPOIDS,IPOID2,IDFDE,IDFDE2,NDIM,REGULA(6),DIMDEF,IVF
      INTEGER   IVF2,NNO,NNOS,NNOM,NDDLS,NDDLM,DIMUEL
      REAL*8    GEOM(NDIM,*),POIDS,POIDS2,B(DIMDEF,DIMUEL)
C ======================================================================
C --- BUT : CALCUL DE L'OPERATEUR B ------------------------------------
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
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER  ZI
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C ======================================================================
C --- VARIABLES LOCALES ------------------------------------------------
C ======================================================================
      INTEGER      I,J,K,N,ADDER1,ADDER2,ADDER3
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
C --- TERMES -(DUJ/DXI-VJI) --------------------------------------------
C ======================================================================
C --- SUR LES NOEUDS SOMMETS -------------------------------------------
C ======================================================================
      DO 10 N=1,NNOS
         DO 20 J=1,NDIM
            DO 30 I=1,NDIM
               B(ADDER1-1+(J-1)*NDIM+I,(N-1)*NDDLS+J) =
     +         B(ADDER1-1+(J-1)*NDIM+I,(N-1)*NDDLS+J) - DFDI(N,I)
               B(ADDER1-1+(J-1)*NDIM+I,(N-1)*NDDLS+NDIM+(J-1)*NDIM+I) =
     +         B(ADDER1-1+(J-1)*NDIM+I,(N-1)*NDDLS+NDIM+(J-1)*NDIM+I) +
     +                                        ZR(IVF2+N+(KPI-1)*NNOS-1)
 30         CONTINUE
 20      CONTINUE
 10   CONTINUE
      DO 40 N=1,NNOM
         DO 50 J=1,NDIM
            DO 60 I=1,NDIM
               B(ADDER1-1+(J-1)*NDIM+I,NNOS*NDDLS+(N-1)*NDDLM+J) =
     +         B(ADDER1-1+(J-1)*NDIM+I,NNOS*NDDLS+(N-1)*NDDLM+J) -
     +                                                   DFDI(N+NNOS,I)
 60         CONTINUE
 50      CONTINUE
 40   CONTINUE
C ======================================================================
C --- POUR LES GRADIENTS DE VARIATIONS VOLUMIQUE -----------------------
C --- ON UTILISE LES FONCTIONS DE FORME D'ORDRE 1 ----------------------
C ======================================================================
C --- SUR LES NOEUDS SOMMETS -------------------------------------------
C ======================================================================
      DO 120 N=1,NNOS
         DO 130 K=1,NDIM
            DO 140 J=1,NDIM
               DO 150 I=1,NDIM
                  B(ADDER2-1+(K-1)*NDIM*NDIM+(J-1)*NDIM+I,
     +                                   (N-1)*NDDLS+NDIM+(K-1)*NDIM+J)=
     +            B(ADDER2-1+(K-1)*NDIM*NDIM+(J-1)*NDIM+I,
     +                                   (N-1)*NDDLS+NDIM+(K-1)*NDIM+J)+
     +            DFDI2(N,I)
 150           CONTINUE
 140        CONTINUE
 130     CONTINUE
 120  CONTINUE
C ======================================================================
C --- POUR LE MULTIPLICATEUR DE LAGRANGE -------------------------------
C --- (PRES) -----------------------------------------------------------
C --- ON UTILISE LES FONCTIONS DE FORME D'ORDRE 0 ----------------------
C ======================================================================
C --- SUR LES NOEUDS CENTRAUX ------------------------------------------
C ======================================================================
      DO 190 I=1,NDIM
         DO 210 J=1,NDIM
            B(ADDER3-1+(I-1)*NDIM+J,NNOS*NDDLS+NNOM*NDDLM+(I-1)*NDIM+J)=
     +      1.0D0
 210     CONTINUE
 190  CONTINUE
C ======================================================================
      END
