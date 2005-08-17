      SUBROUTINE CAETHM(NOMTE,AXI,TYPMOD,MODINT,MECANI,PRESS1,PRESS2,
     +                  TEMPE,DIMDEF,DIMCON,NMEC,NP1,NP2,NDIM,NNO,
     +                  NNOS,NNOM,NPI,NPG,NDDLS,NDDLM,DIMUEL,
     +                  IPOIDS,IVF,IDFDE,IPOID2,IVF2,IDFDE2,JGANO)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C ======================================================================
C MODIF ELEMENTS  DATE 16/08/2005   AUTEUR ROMEO R.FERNANDES 
C RESPONSABLE UFBHHLL C.CHAVANT
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
C TOLE CRP_21
C ======================================================================
      IMPLICIT      NONE
      LOGICAL       AXI
      INTEGER       MECANI(5),PRESS1(7),PRESS2(7),TEMPE(5),DIMUEL
      INTEGER       NDIM,NNO,NNOS,NNOM,DIMDEF,DIMCON,NMEC,NP1,NP2
      INTEGER       NPG,NPI,NDDLS,NDDLM,IPOIDS,IVF,IDFDE
      INTEGER       IPOID2,IVF2,IDFDE2,JGANO
      CHARACTER*3   MODINT
      CHARACTER*8   TYPMOD(2)
      CHARACTER*16  NOMTE
C ======================================================================
C --- BUT: PREPARATION DU CALCUL SUR UN ELEMENT THM --------------------
C ======================================================================
C --- INITIALISATIONS --------------------------------------------------
C ======================================================================
      AXI       = .FALSE.
      TYPMOD(2) = '        '
C ======================================================================
C --- TYPE DE MODELISATION? AXI DPLAN OU 3D ----------------------------
C ======================================================================
      CALL TYPTHM(NOMTE,AXI,TYPMOD,NDIM)
C ======================================================================
C --- SELECTION DU TYPE D'INTEGRATION ----------------------------------
C ======================================================================
      CALL MODTHM(NOMTE,MODINT)
C ======================================================================
C --- INITIALISATION DES GRANDEURS GENERALISEES SELON MODELISATION -----
C ======================================================================
      CALL GRDTHM(NOMTE,NDIM,MECANI,PRESS1,PRESS2,TEMPE,DIMDEF,DIMCON,
     +            NMEC,NP1,NP2)
C ======================================================================
C --- ADAPTATION AU MODE D'INTEGRATION ---------------------------------
C --- DEFINITION DE L'ELEMENT (NOEUDS, SOMMETS, POINTS DE GAUSS) -------
C ======================================================================
      CALL ITGTHM(MODINT,MECANI,PRESS1,PRESS2,TEMPE,NDIM,NNO,
     +                  NNOS,NNOM,NPI,NPG,NDDLS,NDDLM,DIMUEL,
     +                  IPOIDS,IVF,IDFDE,IPOID2,IVF2,IDFDE2,JGANO)
C ======================================================================
      END
