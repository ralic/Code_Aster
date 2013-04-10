      SUBROUTINE ITGTHM(VF,TYPVF,MODINT,MECANI,
     >                  PRESS1,PRESS2,TEMPE,NDIM,NNO,
     >                  NNOS,NNOM,NFACE,NPI,NPG,
     >                  NDDLS,NDDLK,NDDLM,NDDLFA,DIMUEL,
     >                  IPOIDS,IVF,IDFDE,IPOID2,IVF2,IDFDE2,NPI2,JGANO)
      IMPLICIT     NONE
      LOGICAL      VF
      INTEGER      TYPVF
      INTEGER      MECANI(5),PRESS1(7),PRESS2(7),TEMPE(5)
      INTEGER      NDIM,NNO,NNO2,NNOS,NNOM,NFACE
      INTEGER      NPI,NPG,NDDLS,NDDLFA,NDDLM,NDDLK
      INTEGER      DIMUEL,IPOIDS,IVF,IDFDE,IPOID2,IVF2,IDFDE2,JGANO
      CHARACTER*3  MODINT
      CHARACTER*8  ELREFE,ELREF2
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/04/2013   AUTEUR GRANET S.GRANET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C VF        .TRUE. SI VF
C TYPVF  TYPE DE VF : 1  = TPFA (FLUX A DEUX POINTS - SUPPRIME)
C                 2  = SUSHI AVEC VOISIN DECENTRE MAILLE (SUDM)
C                 3  = SUSHI AVEC VOISIN DECENTRE ARETE (SUDA)
C                 4  = SUSHI AVEC VOISIN CENTRE  (SUC)
C MODINT    METHODE D'INTEGRATION (CLASSIQUE,LUMPEE(D),REDUITE(R) ?)
C NNO       NB DE NOEUDS DE L'ELEMENT
C NNOS      NB DE NOEUDS SOMMETS DE L'ELEMENT
C NFACE     NB DE FACES AU SENS BRD DE DIM DIM-1 NE SERT QU EN VF
C NNOM      NB DE NOEUDS MILIEUX DE FACE OU D ARRETE NE SERT QU EN EF
C NDDLS     NB DE DDL SUR LES SOMMETS
C NDDLM     NB DE DDL SUR LES MILIEUX DE FACE OU D ARETE - QU EN EF
C NDDLFA    NB DE DDL SUR LES FACE DE DIMENSION DIM-1 NE SERT QU EN VF
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
C =====================================================================
      INTEGER      NNOS2,NPI2,IBID
C =====================================================================
      CALL ELREF1(ELREFE)
      IF ( .NOT.VF) THEN
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
         CALL U2MESK('F','DVP_9',1,ELREFE)
       ENDIF
      ELSE
       IF ( ELREFE.EQ.'TR7') THEN
         ELREF2 = 'TR3'
       ELSEIF ( ELREFE.EQ.'QU9') THEN
         ELREF2 = 'QU4'
       ELSEIF ( ELREFE.EQ.'H27') THEN
         ELREF2 = 'HE8'
       ELSE
         CALL U2MESK('F','DVP_9',1,ELREFE)
       ENDIF
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
       CALL ASSERT(NNOS.EQ.NNO2)
C ======================================================================
C --- NFACE EN VF -------------
C ======================================================================
      IF ( VF) THEN
       IF(NDIM.EQ.2) THEN
         NFACE=NNOS
       ELSE
        IF(ELREFE.EQ.'H27') THEN
         NFACE = 6
        ELSE IF(ELREFE.EQ.'T9') THEN
         NFACE = 4
        ELSE
          CALL U2MESK('F','VOLUFINI_12', 1 ,ELREFE)
        ENDIF
       ENDIF
      ENDIF
C ======================================================================
C --- POUR METHODES CLASSIQUE ET LUMPEE NPG=NPI
C ======================================================================
      IF (.NOT.VF) THEN
       NPG    = NPI
       NDDLS  = MECANI(1)*NDIM + PRESS1(1) + PRESS2(1) + TEMPE(1)
       NDDLM  = MECANI(1)*NDIM
       NDDLK  = 0
       NNOM   = NNO - NNOS
       DIMUEL = NNOS*NDDLS + NNOM*NDDLM + NDDLK
      ELSE
       IF (( TYPVF.EQ.2).OR.( TYPVF.EQ.3)
     >          .OR.( TYPVF.EQ.4)) THEN
        NPG    = NPI
        NDDLS  = 0
        NDDLFA = PRESS1(1) + PRESS2(1) + TEMPE(1)
        NDDLK  = PRESS1(1) + PRESS2(1) + TEMPE(1)
       ELSE
        CALL U2MESG('F','VOLUFINI_9',0,' ',1,TYPVF,0,0.D0)
C--      POUR UN SCHEMA A DEUX POINTS  ( TYPVF.EQ.1) ON AURAIT EU
C        NPG    = NPI
C        NDDLS  = 0
C        NDDLFA = 0
C--      NDDLK  = PRESS1(1) + PRESS2(1) + TEMPE(1)
       ENDIF
       DIMUEL = NNOS*NDDLS + NFACE*NDDLFA + NDDLK
      ENDIF
C ======================================================================
C --- POUR METHODE REDUITE NPI = NPG+NNOS ------------------------------
C ======================================================================
      IF (MODINT .EQ. 'RED') NPG= NPI-NNOS
      END
