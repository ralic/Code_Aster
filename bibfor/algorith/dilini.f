      SUBROUTINE DILINI(OPTION,NOMTE,IVF,IVF2,IDFDE,IDFDE2,JGANO,NDIM,
     +                  IPOIDS,IPOID2,ICOMPO,NPI,DIMDEF,NDDLS,NDDLM,
     +                  DIMCON,TYPMOD,DIMUEL,NNO,NNOM,NNOS,REGULA,AXI,
     +                  INTERP)
C ======================================================================
C RESPONSABLE FERNANDES R.FERNANDES
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
      INTEGER       REGULA(6),ICOMPO,NDDLC
      CHARACTER*2   INTERP
      CHARACTER*8   TYPMOD(2)
      CHARACTER*16  OPTION,NOMTE
C ======================================================================
C --- BUT : INITIALISATION DES GRANDEURS NECESSAIRES POUR LA GESTION ---
C ---       DU CALCUL AVEC REGULARISATION A PARTIR DU MODELE SECOND ----
C ---       GRADIENT A MICRO-DILATATION --------------------------------
C ======================================================================
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER        ZI
      COMMON /IVARJE/ZI(1)
      REAL*8         ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16     ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL        ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8    ZK8
      CHARACTER*16          ZK16
      CHARACTER*24                  ZK24
      CHARACTER*32                          ZK32
      CHARACTER*80                                  ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C =====================================================================
C --- VARIABLES LOCALES ------------------------------------------------
C ======================================================================
      INTEGER       NNO2,NNOS2,NPI2,IBID,NNOC
      CHARACTER*8   ELREFE,ELRF1,ELRF2
C ======================================================================
C --- INITIALISATION ---------------------------------------------------
C ======================================================================
      INTERP    = '  '
      TYPMOD(2) = '        '
      ELRF1     = '        '
      ELRF2     = '        '
      AXI       = .FALSE.
      DIMDEF    = 0
      DIMCON    = 0
C ======================================================================
C --- TYPE D'ELEMENT ---------------------------------------------------
C ======================================================================
      CALL ELREF1(ELREFE)
      IF ( ELREFE.EQ.'TR7' ) THEN
         INTERP = 'P0'
         ELRF1  = 'TR6'
         ELRF2  = 'TR3'
      ELSEIF ( ELREFE.EQ.'QU9' ) THEN
         INTERP = 'P0'
         ELRF1  = 'QU8'
         ELRF2  = 'QU4'
      ELSEIF ( ELREFE.EQ.'TR6' ) THEN
         INTERP = 'SL'
         ELRF1  = 'TR6'
         ELRF2  = 'TR3'
      ELSEIF ( ELREFE.EQ.'QU8' ) THEN
         INTERP = 'SL'
         ELRF1  = 'QU8'
         ELRF2  = 'QU4'
      ELSEIF ( ELREFE.EQ.'T10' ) THEN
         INTERP = 'P1'
         ELRF1  = 'T10'
         ELRF2  = 'TE4'
      ELSEIF ( ELREFE.EQ.'P15' ) THEN
         INTERP = 'P1'
         ELRF1  = 'P15'
         ELRF2  = 'PE6'
      ELSEIF ( ELREFE.EQ.'H20' ) THEN
         INTERP = 'P1'
         ELRF1  = 'H20'
         ELRF2  = 'HE8'
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
C --- RECUPERATION DU TYPE DE LA MODELISATION --------------------------
C ======================================================================
      IF ( NOMTE(5:6).EQ.'DP' ) THEN
         TYPMOD(1) = 'D_PLAN  '
      ELSE IF ( NOMTE(5:6).EQ.'3D' ) THEN
         TYPMOD(1) = '3D  '
      ELSE
C       NOM D'ELEMENT ILLICITE
        CALL ASSERT(NOMTE(5:6).EQ.'DP' .OR. NOMTE(5:6).EQ.'3D' )
      ENDIF
C ======================================================================
      IF (INTERP.EQ.'P0') THEN
         CALL DIMP0(NDIM,NNO,NNOS,DIMDEF,DIMCON,NNOM,NNOC,NDDLS,NDDLM,
     +              NDDLC,DIMUEL,REGULA)
      ELSE IF (INTERP.EQ.'SL') THEN
         CALL DIMSL(NDIM,NNO,NNOS,DIMDEF,DIMCON,NNOM,NNOC,NDDLS,NDDLM,
     +              NDDLC,DIMUEL,REGULA)
      ELSE IF (INTERP.EQ.'P1') THEN
         CALL DIMP1(NDIM,NNO,NNOS,DIMDEF,DIMCON,NNOM,NNOC,NDDLS,NDDLM,
     +              NDDLC,DIMUEL,REGULA)
      ENDIF
C ======================================================================
      END
