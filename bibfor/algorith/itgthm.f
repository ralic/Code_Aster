      SUBROUTINE ITGTHM(MODINT,MECANI,PRESS1,PRESS2,TEMPE,NDIM,NNO,
     &                  NNOS,NNOM,NPI,NPG,NDDLS,NDDLM,DIMUEL,
     &                  IPOIDS,IVF,IDFDE,IPOID2,IVF2,IDFDE2,JGANO)
      IMPLICIT     NONE
      INTEGER      MECANI(5),PRESS1(7),PRESS2(7),TEMPE(5)
      INTEGER      NDIM,NNO,NNO2,NNOS,NNOM,NPI,NPG,NDDLS,NDDLM
      INTEGER      DIMUEL,IPOIDS,IVF,IDFDE,IPOID2,IVF2,IDFDE2,JGANO
      CHARACTER*3  MODINT
      CHARACTER*8  ELREFE,ELREF2
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C TOLE CRP_21
C ======================================================================
C --- ADAPTATION AU MODE D'INTEGRATION ---------------------------------
C --- DEFINITION DE L'ELEMENT (NOEUDS, SOMMETS, POINTS DE GAUSS) -------
C ======================================================================
C AXI       AXISYMETRIQUE?
C TYPMOD    MODELISATION (D_PLAN, AXI, 3D ?)
C MODINT    METHODE D'INTEGRATION (CLASSIQUE,LUMPEE(D),REDUITE(R) ?)
C NNO       NB DE NOEUDS DE L'ELEMENT
C NNOS      NB DE NOEUDS SOMMETS DE L'ELEMENT
C NNOM      NB DE NOEUDS MILIEUX DE L'ELEMENT
C NDDLS     NB DE DDL SUR LES SOMMETS
C NDDLM     NB DE DDL SUR LES MILIEUX
C NPI       NB DE POINTS D'INTEGRATION DE L'ELEMENT
C NPG       NB DE POINTS DE GAUSS     POUR CLASSIQUE(=NPI)
C                 SOMMETS             POUR LUMPEE   (=NPI=NNOS)
C                 POINTS DE GAUSS     POUR REDUITE  (<NPI)
C NDIM      DIMENSION DE L'ESPACE
C DIMUEL    NB DE DDL TOTAL DE L'ELEMENT
C DIMCON    DIMENSION DES CONTRAINTES GENERALISEES ELEMENTAIRES
C DIMDEF    DIMENSION DES DEFORMATIONS GENERALISEES ELEMENTAIRES
C IVF       FONCTIONS DE FORMES QUADRATIQUES
C IVF2      FONCTIONS DE FORMES LINEAIRES
C ======================================================================
      INTEGER      NNOS2,NPI2,IBID
C ======================================================================
      CALL ELREF1(ELREFE)
      IF ( ELREFE.EQ.'TR6') THEN
         ELREF2 = 'TR3'
      ELSEIF ( ELREFE.EQ.'QU8') THEN
         ELREF2 = 'QU4'
      ELSEIF ( ELREFE.EQ.'H20') THEN
         ELREF2 = 'HE8'
      ELSEIF ( ELREFE.EQ.'P15') THEN
         ELREF2 = 'PE6'
      ELSEIF ( ELREFE.EQ.'T10') THEN
         ELREF2 = 'TE4'
      ELSE
         CALL U2MESK('F','ALGORITH4_46',1,ELREFE)
      ENDIF
C ======================================================================
C --- FONCTIONS DE FORME P2 POUR L'INTEGRATION MECANIQUE ---------------
C ======================================================================
      CALL ELREF4(ELREFE,'RIGI',NDIM,NNO,NNOS,NPI,IPOIDS,IVF,IDFDE,
     &                                                            JGANO)
C ======================================================================
C --- FONCTIONS DE FORME P1 POUR L'HYDRAULIQUE - THERMIQUE -------------
C ======================================================================
      CALL ELREF4(ELREF2,'RIGI',NDIM,NNO2,NNOS2,NPI2,IPOID2,IVF2,
     &                                                      IDFDE2,IBID)
C ======================================================================
C --- POUR METHODES CLASSIQUE ET LUMPEE NPG=NPI
C ======================================================================
      NPG    = NPI
      NDDLS  = MECANI(1)*NDIM + PRESS1(1) + PRESS2(1) + TEMPE(1)
      NDDLM  = MECANI(1)*NDIM
      NNOM   = NNO - NNOS
      DIMUEL = NNOS*NDDLS + NNOM*NDDLM
C ======================================================================
C --- POUR METHODE REDUITE NPI = NPG+NNOS ------------------------------
C ======================================================================
      IF (MODINT .EQ. 'RED') NPG= NPI-NNOS
C ======================================================================
C --- ON VERIFIE LA COHERENCE ENTRE NNOS ET NNO2 -----------------------
C ======================================================================
       CALL ASSERT(NNOS.EQ.NNO2)
C ======================================================================
      END
