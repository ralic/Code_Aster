      SUBROUTINE REGINI(OPTION,NOMTE,IVF,IVF2,IDFDE,IDFDE2,JGANO,NDIM,
     +                  IPOIDS,IPOID2,NPI,DIMDEF,NDDLS,NDDLM,DIMCON,
     +                  TYPMOD,DIMUEL,NNO,NNOM,NNOS,REGULA,AXI)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 05/07/2011   AUTEUR FERNANDES R.FERNANDES 
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
C TOLE CRP_21
C ======================================================================
      IMPLICIT      NONE
      LOGICAL       AXI
      INTEGER       IVF,IVF2,IDFDE,IDFDE2,JGANO,NDIM,IPOIDS,NPI,NNOM
      INTEGER       IPOID2,DIMDEF,DIMUEL,DIMCON,NNO,NNOS,NDDLS,NDDLM
      INTEGER       REGULA(6)
      CHARACTER*8   TYPMOD(2)
      CHARACTER*16  OPTION,NOMTE
C ======================================================================
C --- BUT : INITIALISATION DES GRANDEURS NECESSAIRES POUR LA GESTION ---
C ---       DU CALCUL AVEC REGULARISATION A PARTIR DU MODELE SECOND ----
C ---       GRADIENT ---------------------------------------------------
C ======================================================================
C --- VARIABLES LOCALES ------------------------------------------------
C ======================================================================
      INTEGER       NNO2,NNOS2,NPI2,IBID,DEF1,DEF2,DEF3,CONT1,CONT2,NNOC
      INTEGER       CONT3,ADDER1,ADDER2,ADDER3,ADCOR1,ADCOR2,ADCOR3
      CHARACTER*8   ELREFE,ELRF1,ELRF2
C ======================================================================
C --- INITIALISATION ---------------------------------------------------
C ======================================================================
      TYPMOD(2) = '        '
      ELRF1     = '        '
      ELRF2     = '        '
      AXI       = .FALSE.
      DEF1      = 0
      DEF2      = 0
      DEF3      = 0
      CONT1     = 0
      CONT2     = 0
      CONT3     = 0
      DIMDEF    = 0
      DIMCON    = 0
      ADDER1    = 0
      ADDER2    = 0
      ADDER3    = 0
      ADCOR1    = 0
      ADCOR2    = 0
      ADCOR3    = 0
C ======================================================================
C --- TYPE D'ELEMENT ---------------------------------------------------
C ======================================================================
      CALL ELREF1(ELREFE)
      IF ( ELREFE.EQ.'TR7' ) THEN
         ELRF1 = 'TR6'
         ELRF2 = 'TR3'
      ELSEIF ( ELREFE.EQ.'QU9' ) THEN
         ELRF1 = 'QU8'
         ELRF2 = 'QU4'
      ELSEIF ( ELREFE.EQ.'H27' ) THEN
         ELRF1 = 'H20'
         ELRF2 = 'HE8'
      ELSE
         CALL U2MESK('F','DVP_4',1,ELREFE)
      ENDIF
C ======================================================================
C --- FONCTIONS DE FORME P2 --------------------------------------------
C ======================================================================
      CALL ELREF4(ELRF1,'RIGI',NDIM,NNO,NNOS,NPI,IPOIDS,IVF,IDFDE,JGANO)
C ======================================================================
C --- FONCTIONS DE FORME P1 --------------------------------------------
C ======================================================================
      CALL ELREF4(ELRF2,'RIGI',NDIM,NNO2,NNOS2,NPI2,IPOID2,IVF2,
     +                                                      IDFDE2,IBID)
C ======================================================================
C --- NNOC DESIGNE LE NOMBRE DE NOEUD AU CENTRE DES ELEMENTS -----------
C ======================================================================
C --- [E] = [DEPVIJ,DVIJDX,DVIJDY,LAMBIJ] ------------------------------
C ======================================================================
      NNOC   = 1
      DEF1   = NDIM*NDIM
      DEF2   = NDIM*NDIM*NDIM
      DEF3   = NDIM*NDIM
      DIMDEF = DEF1+DEF2+DEF3
      CONT1  = NDIM*NDIM
      CONT2  = NDIM*NDIM*NDIM
      CONT3  = NDIM*NDIM
      DIMCON = CONT1+CONT2+CONT3
C ======================================================================
C --- RECUPERATION DU TYPE DE LA MODELISATION --------------------------
C ======================================================================
      TYPMOD(1) = 'D_PLAN  '
C ======================================================================
C --- DIMENSION DES COMPOSANTES NODALES --------------------------------
C ======================================================================
      NDDLS = NDIM + NDIM*NDIM
      NDDLM = NDIM
C ======================================================================
      ADDER1 = 1
      ADDER2 = ADDER1+DEF1
      ADDER3 = ADDER2+DEF2
      ADCOR1 = 1
      ADCOR2 = ADCOR1+CONT1
      ADCOR3 = ADCOR2+CONT2
      NNOM   = NNO - NNOS
      DIMUEL = NNOS*NDDLS + NNOM*NDDLM + NNOC*NDIM*NDIM
C ======================================================================
C --- POSITIONS DU POINTEUR REGULA : -----------------------------------
C --- (1) : ADRESSE DES DEFORMATIONS DEPV** ----------------------------
C --- (2) : ADRESSE DES DEFORMATIONS DGONFX* ---------------------------
C --- (3) : ADRESSE DES DEFORMATIONS PRES** ----------------------------
C --- (4) : ADRESSE DES CONTRAINTES GENERALISEES PRES** ----------------
C --- (5) : ADRESSE DES CONTRAINTES GENERALISEES SIG*** ----------------
C --- (6) : ADRESSE DES CONTRAINTES GENERALISEES DEPV** ----------------
C ======================================================================
      REGULA(1)=ADDER1
      REGULA(2)=ADDER2
      REGULA(3)=ADDER3
      REGULA(4)=ADCOR1
      REGULA(5)=ADCOR2
      REGULA(6)=ADCOR3
C ======================================================================
      END
