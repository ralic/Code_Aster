      SUBROUTINE DIMSL(NDIM,NNO,NNOS,DIMDEF,DIMCON,NNOM,NNOC,NDDLS,
     +                 NDDLM,NDDLC,DIMUEL,REGULA)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT      NONE
      INTEGER       NDIM,NNO,NNOS,DIMDEF,DIMCON,NNOM,NNOC,NDDLS,NDDLM
      INTEGER       NDDLC,DIMUEL,REGULA(6)
C ======================================================================
C --- BUT : INITIALISATION DES GRANDEURS NECESSAIRES POUR LA GESTION ---
C ---       DU CALCUL AVEC REGULARISATION A PARTIR DU MODELE SECOND ----
C ---       GRADIENT A MICRO-DILATATION --------------------------------
C ======================================================================
      INTEGER DEF1,DEF2,CONT1,CONT2
C ======================================================================
      NNOC   = 0
      DEF1   = 1
      DEF2   = NDIM
      DIMDEF = DEF1+DEF2
      CONT1  = 1
      CONT2  = NDIM
      DIMCON = CONT1+CONT2
C ======================================================================
C --- DIMENSION DU VECTEUR DES DEFORMATIONS GENERALISEES ---------------
C ======================================================================
C --- [E] = [DEPV,DGONFDX,DGONFDY,DGONFDZ] ------------------------
C ======================================================================
      NDDLS = NDIM + 1
      NDDLM = NDIM
      NDDLC = 0
C ======================================================================
      NNOM   = NNO - NNOS
      DIMUEL = NNOS*NDDLS + NNOM*NDDLM
C ======================================================================
C --- POSITIONS DU POINTEUR REGULA : -----------------------------------
C --- (1) : ADRESSE DES DEFORMATIONS DEP*** ----------------------------
C --- (2) : ADRESSE DES DEFORMATIONS DGONFX* ---------------------------
C --- (3) : ADRESSE DES DEFORMATIONS PRES** ----------------------------
C --- (4) : ADRESSE DES CONTRAINTES GENERALISEES PRES** ----------------
C --- (5) : ADRESSE DES CONTRAINTES GENERALISEES SIG*** ----------------
C --- (6) : ADRESSE DES CONTRAINTES GENERALISEES DEP*** ----------------
C ======================================================================
      REGULA(1)=1
      REGULA(2)=REGULA(1)+DEF1
      REGULA(3)=REGULA(2)+DEF2
      REGULA(4)=1
      REGULA(5)=REGULA(4)+CONT1
      REGULA(6)=REGULA(5)+CONT2
C ======================================================================
      END
