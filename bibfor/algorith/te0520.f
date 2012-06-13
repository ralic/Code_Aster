      SUBROUTINE TE0520(OPTION,NOMTE)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*16 OPTION,NOMTE
C =====================================================================
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
C =====================================================================
C    - FONCTION REALISEE:  CALCUL DES OPTIONS NON-LINEAIRES MECANIQUES
C                          ELEMENTS HH2_SUDM, (SUSHI DECENTRE MAILLE)
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C =====================================================================
      INTEGER JGANO,NNO,IMATUU,NDIM,IMATE,IINSTM,JCRET
      INTEGER IPOID2,IVF2
      INTEGER IDFDE2,NPI,NPG,NPI2
C
      INTEGER RETLOI
      INTEGER IPOIDS,IVF,IDFDE,IGEOM
      INTEGER IINSTP,IDEPLM,IDEPLP,ICOMPO,ICARCR
      INTEGER ICONTM,IVARIP,IVARIM,IVECTU,ICONTP
C =====================================================================
      INTEGER EVFINI,CALVOI,JREPE,JPTVOI,JELVOI
      COMMON /CAII19/EVFINI,CALVOI,JREPE,JPTVOI,JELVOI
C =====================================================================
      INTEGER MECANI(5),PRESS1(7),PRESS2(7),TEMPE(5),DIMUEL
      INTEGER DIMDEP,DIMDEF,DIMCON,NBVARI,NDDLS,NDDLM,NDDLFA,NDDLK
      INTEGER NMEC,NP1,NP2,NNOS
      INTEGER NNOM,NFACE
C     REMARQUE : CES DIMENSIONS DOIVENT ETRE LES MEMES QUE DANS TE0492
      REAL*8 DEFGEP(21),DEFGEM(21)
      INTEGER NCONMA,NDEFMA,ICON,IDEF
      PARAMETER (NCONMA=31,NDEFMA=21)
      REAL*8 DSDE(NCONMA,NDEFMA)
      CHARACTER*3 MODINT
      CHARACTER*8 TYPMOD(2)
C =====================================================================
      INTEGER     ISMAEM
      LOGICAL     AXI, PERMAN,VF
      INTEGER     TYPVF
C =====================================================================
C  CETTE ROUTINE FAIT UN CALCUL EN HH2SUDM , (HYDRO NON SATURE SUSHI
C   DECENTRE MAILLE
C =====================================================================
C  POUR LES TABLEAUX DEFGEP ET DEFGEM ON A DANS L'ORDRE :
C                                      PRE1 P1DX P1DY P1DZ
C                                      PRE2 P2DX P2DY P2DZ
C            EPSXY = RAC2/2*(DU/DY+DV/DX)
C =====================================================================
C    POUR LES CHAMPS DE CONTRAINTE
C                                      M11 FH11X FH11Y FH11Z
C                                      ENT11
C                                      M12 FH12X FH12Y FH12Z
C                                      ENT12
C                                      M21 FH21X FH21Y FH21Z
C                                      ENT21
C                                      M22 FH22X FH22Y FH22Z
C                                      ENT22
C TYPMOD    MODELISATION (D_PLAN,  3D )
C MODINT    METHODE D'INTEGRATION (CLASSIQUE,LUMPEE(D),REDUITE(R) ?)
C NNO       NB DE NOEUDS DE L'ELEMENT
C NNOS      NB DE NOEUDS SOMMETS DE L'ELEMENT
C OUT
C    NFACE     NB DE FACES AU SENS BORD DE DIMENSION DIM-1
C              NE SERT QU EN VF
C    NNOM      NB DE NOEUDS MILIEUX DE FACE OU D ARRETE
C              NE SERT QU EN EF
C NDDLS     NB DE DDL SUR LES SOMMETS
C OUT
C    NDDLM     NB DE DDL SUR LES MILIEUX DE FACE OU D ARRETE
C              NE SERT QU EN EF
C   NDDLFA    NB DE DDL SUR LES FACE DE DIMENSION DIM-1
C             NE SERT QU EN VF
C NDDLK     NB DDL AU CENTRE ELEMENT
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
C TYPVF     : 2 (SUDM)
C =====================================================================
      INTEGER IADZI,IAZK24
      CHARACTER*16 CODVOI
      INTEGER NVOIMA,NSCOMA,NBVOIS
      PARAMETER(NVOIMA=100,NSCOMA=4)
      INTEGER LIVOIS(NVOIMA),TYVOIS(NVOIMA),NBNOVO(NVOIMA)
      INTEGER NBSOCO(NVOIMA),LISOCO(NVOIMA,NSCOMA,2)
      INTEGER NUMA
C
C
C =====================================================================
C --- 1. INITIALISATIONS ----------------------------------------------
C --- SUIVANT ELEMENT, DEFINITION DES CARACTERISTIQUES : --------------
C --- CHOIX DU TYPE D'INTEGRATION -------------------------------------
C --- RECUPERATION DE LA GEOMETRIE ET POIDS DES POINTS D'INTEGRATION --
C --- RECUPERATION DES FONCTIONS DE FORME -----------------------------
C =====================================================================
C
C
      DO 10 ICON = 1 , NCONMA
       DO 11 IDEF = 1 , NDEFMA
        DSDE(ICON,IDEF)=0.D0
   11  CONTINUE
   10 CONTINUE
      CALL CAETHM(NOMTE,AXI,PERMAN,VF,TYPVF,
     >            TYPMOD,MODINT,MECANI,PRESS1,PRESS2,TEMPE,
     >            DIMDEP,DIMDEF,DIMCON,NMEC,NP1,NP2,NDIM,NNO,
     >                  NNOS,NNOM,NFACE,
     >                  NPI,NPG,NDDLS,NDDLM,NDDLFA,NDDLK,DIMUEL,
     >                  IPOIDS,IVF,IDFDE,IPOID2,IVF2,IDFDE2,NPI2,JGANO)
      CALL ASSERT(VF)
C
C   RECHERCHE DE VOISINAGES
C
      CALL TECAEL(IADZI,IAZK24)
      NUMA=ZI(IADZI-1+1)
      IF((TYPVF.EQ.1).OR.(TYPVF.EQ.2)) THEN
       CODVOI='A2'
      ELSE
        CALL U2MESG('F','VOLUFINI_9',0,' ',1,TYPVF,0,0.D0)
      ENDIF
      CALL VOIUTI(NUMA,CODVOI,NVOIMA,NSCOMA,JREPE,JPTVOI,JELVOI,
     &                  NBVOIS,LIVOIS,TYVOIS,NBNOVO,NBSOCO,LISOCO)
C =====================================================================
C --- DEBUT DES DIFFERENTES OPTIONS -----------------------------------
C =====================================================================
C --- 2. OPTIONS : RIGI_MECA_TANG , FULL_MECA , RAPH_MECA -------------
C =====================================================================
      IF ((OPTION(1:14).EQ.'RIGI_MECA_TANG' ) .OR.
     +    (OPTION(1:9).EQ.'RAPH_MECA' ) .OR.
     +    (OPTION(1:9).EQ.'FULL_MECA' )) THEN
C =====================================================================
C --- PARAMETRES EN ENTREE --------------------------------------------
C =====================================================================
         CALL JEVECH('PGEOMER','L',IGEOM )
         CALL JEVECH('PMATERC','L',IMATE )
         CALL JEVECH('PINSTMR','L',IINSTM)
         CALL JEVECH('PINSTPR','L',IINSTP)
         CALL JEVECH('PDEPLMR','L',IDEPLM)
         CALL JEVECH('PDEPLPR','L',IDEPLP)
         CALL JEVECH('PCOMPOR','L',ICOMPO)
         CALL JEVECH('PCARCRI','L',ICARCR)
         CALL JEVECH('PVARIMR','L',IVARIM)
         CALL JEVECH('PCONTMR','L',ICONTM)
         READ (ZK16(ICOMPO-1+2),'(I16)') NBVARI
C =====================================================================
C --- PARAMETRES EN SORTIE ISMAEM? ------------------------------------
C =====================================================================
         IF ((OPTION(1:14).EQ.'RIGI_MECA_TANG' ) .OR.
     +       OPTION(1:9).EQ.'FULL_MECA') THEN
            CALL JEVECH('PMATUNS','E',IMATUU)
         ELSE
            IMATUU = ISMAEM()
         END IF
         IF (OPTION(1:9).EQ.'RAPH_MECA' .OR.
     +       OPTION(1:9).EQ.'FULL_MECA') THEN
            CALL JEVECH('PVECTUR','E',IVECTU)
            CALL JEVECH('PCONTPR','E',ICONTP)
            CALL JEVECH('PVARIPR','E',IVARIP)
            CALL JEVECH('PCODRET','E',JCRET)
            ZI(JCRET) = 0
         ELSE
            IVECTU = ISMAEM()
            ICONTP = ISMAEM()
            IVARIP = ISMAEM()
         END IF
         RETLOI = 0
         IF (OPTION(1:14).EQ.'RIGI_MECA_TANG' ) THEN
            CALL ASSVSU(NNO,NNOS,NFACE,
     +                ZR(IGEOM),ZR(ICARCR),
     +                ZR(IDEPLM),ZR(IDEPLM),ZR(ICONTM),
     +                ZR(ICONTM),ZR(IVARIM),ZR(IVARIM),DEFGEM,DEFGEM,
     +                DSDE,ZR(IMATUU),ZR(IVECTU),
     +                ZR(IINSTM),ZR(IINSTP),OPTION,ZI(IMATE),MECANI,
     +                PRESS1,PRESS2,TEMPE,DIMDEF,DIMCON,DIMUEL,
     +                NBVARI,
     +                NDIM,ZK16(ICOMPO),
     >                TYPMOD,TYPVF,AXI,PERMAN,
     >                NVOIMA,NSCOMA,NBVOIS,
     >                LIVOIS,NBNOVO,NBSOCO,LISOCO)
         ELSE
C
C   DU FAIT DE L UTIISATION DES VOISINS CETTE BOUCLE NE PEUT
C   PLUS ETRE FAITECONTRAIREMENT A LA SITUATION EF
C  ASSVSU UTILISE DELTAP ET PM
C            DO 30 LI = 1,DIMUEL
C               ZR(IDEPLP+LI-1) = ZR(IDEPLM+LI-1) + ZR(IDEPLP+LI-1)
C   30       CONTINUE
C
            CALL ASSVSU(NNO,NNOS,NFACE,
     +                ZR(IGEOM),ZR(ICARCR),
     +                ZR(IDEPLM),ZR(IDEPLP),ZR(ICONTM),
     +                ZR(ICONTP),ZR(IVARIM),ZR(IVARIP),DEFGEM,DEFGEP,
     +                DSDE,ZR(IMATUU),ZR(IVECTU),
     +                ZR(IINSTM),ZR(IINSTP),OPTION,ZI(IMATE),MECANI,
     +                PRESS1,PRESS2,TEMPE,DIMDEF,DIMCON,DIMUEL,
     +                NBVARI,
     +                NDIM,ZK16(ICOMPO),
     >                TYPMOD,TYPVF,AXI,PERMAN,
     >                NVOIMA,NSCOMA,NBVOIS,
     >                LIVOIS,NBNOVO,NBSOCO,LISOCO)
C
            ZI(JCRET) = RETLOI
         END IF
      END IF
C ======================================================================
C --- 6. OPTION : FORC_NODA --------------------------------------------
C ======================================================================
      IF (OPTION.EQ.'FORC_NODA') THEN
C
C   EN VF L OPTION FORC NODA NE PEUT PAS ETRE ATTEINTE SEULE
C   ELLE DOIT ETRE PRECEDE PAR UNE OPTION QU A CALCULEE
C   LES VARIABLES INTERNES MOINS ET LE TERMES ELEMENTAIRES VF
C
C ======================================================================
C --- PARAMETRES EN ENTREE ---------------------------------------------
C ======================================================================
        CALL JEVECH('PCONTMR','L',ICONTM)
C=============================
C --- PARAMETRES EN SORTIE ---------------------------------------------
C ======================================================================
        CALL JEVECH('PVECTUR','E',IVECTU)
C
        CALL FNOVSU(OPTION,NNO,NNOS,NNOM,NFACE,
     +                ZR(ICONTM),ZR(IVECTU),
     +                MECANI,PRESS1,PRESS2,TEMPE,
     >                DIMCON,DIMUEL,
     >                TYPVF,AXI,
     >                NVOIMA,NSCOMA,NBVOIS,
     >                LIVOIS,TYVOIS,NBNOVO,NBSOCO,LISOCO,
     >                IPOIDS,IVF,IDFDE,IPOID2,IVF2,IDFDE2,NPI2,JGANO,
     +                RETLOI)
      END IF
C ======================================================================
      END
