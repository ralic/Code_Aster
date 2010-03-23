      SUBROUTINE TE0515(OPTION,NOMTE)
      IMPLICIT NONE
      CHARACTER*16 OPTION,NOMTE
C =====================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/03/2010   AUTEUR ANGELINI O.ANGELINI 
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
C =====================================================================
C    - FONCTION REALISEE:  CALCUL DES OPTIONS NON-LINEAIRES MECANIQUES
C                          ELEMENTS HH2_SUDA, (SUSHI DECENTRE ARRETE)
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C =====================================================================
      INTEGER JGANO,NNO,IMATUU,NDIM,IMATE,IINSTM,IFORC,JCRET
      INTEGER IPOID2,IVF2
      INTEGER IDFDE2,NPI,NPG,NVIM,NPI2
C
      INTEGER RETLOI,IRET,IRETP,IRETM
      INTEGER IPOIDS,IVF,IDFDE,IGEOM,IDEFO
      INTEGER IINSTP,IDEPLM,IDEPLP,IDEPLA,ICOMPO,ICARCR,IPESA
      INTEGER ICONTM,IVARIP,IVARIM,IVECTU,ICONTP
C =====================================================================
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX --------------------
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
      INTEGER EVFINI,CALVOI,JREPE,JPTVOI,JELVOI
      COMMON /CAII19/EVFINI,CALVOI,JREPE,JPTVOI,JELVOI
C =====================================================================
      INTEGER MECANI(5),PRESS1(7),PRESS2(7),TEMPE(5),DIMUEL
      INTEGER DIMDEP,DIMDEF,DIMCON,NBVARI,NDDLS,NDDLM,NDDLFA,NDDLK
      INTEGER II,INO,NMEC,NP1,NP2,I,NCMP,NNOS,ICHG,ICHN
      INTEGER JTAB(7),IGAU,ISIG,NNOM,NFACE
C     REMARQUE : CES DIMENSIONS DOIVENT ETRE LES MEMES QUE DANS TE0492
      REAL*8 DEFGEP(21),DEFGEM(21)
      REAL*8 POIDS
      INTEGER NCONMA,NDEFMA,ICON,IDEF
      PARAMETER (NCONMA=31,NDEFMA=21)
      REAL*8 DSDE(NCONMA,NDEFMA)
      CHARACTER*3 MODINT
      CHARACTER*8 TYPMOD(2)
      CHARACTER*16 PHENOM
C =====================================================================
      INTEGER     ISMAEM,LI,KP,J,L,K
      REAL*8      R8BID,RHO,COEF,RX
      CHARACTER*2 CODRET(1)
      LOGICAL     AXI, PERMAN,VF
      INTEGER     TYPVF
C =====================================================================
C  CETTE ROUTINE FAIT UN CALCUL EN HH2SUDA OU HH2SUC, (HYDRO NON SATURE
C   SUSHI DECENTRE ARETE OU CENTRE)
C =====================================================================
C  POUR LES TABLEAUX DEFGEP ET DEFGEM ON A DANS L'ORDRE :
C                                      PRE1 P1DX P1DY P1DZ
C                                      PRE2 P2DX P2DY P2DZ
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
C TYPMOD    MODELISATION (D_PLAN, 3D )
C MODINT    METHODE D'INTEGRATION (CLASSIQUE,LUMPEE(D),REDUITE(R) ?)
C NNO       NB DE NOEUDS DE L'ELEMENT
C NNOS      NB DE NOEUDS SOMMETS DE L'ELEMENT
C OUT 
C     NFACE  NB DE FACES AU SENS BORD DE DIM DIM-1 NE SERT QU EN VF
C     NNOM   NB DE NOEUDS MILIEUX DE FACE OU D ARRETE NE SERT QU EN EF
C
C NDDLS     NB DE DDL SUR LES SOMMETS
C OUT 
C    NDDLM     NB DDL SUR LES MILIEUX DE FACE OU D ARRETE 
C              NE SERT QU EN EF
C    NDDLFA    NB DDL SUR LES FACE DE DIMENSION DIM-1 NE SERT QU EN VF
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
C TYPVF     : 3 OU 4 SCHEMA SUSHI DECENTRE ARETE OU CENTRE
C =====================================================================
      REAL*8  DT
C =====================================================================
C --- 1. INITIALISATIONS ----------------------------------------------
C --- SUIVANT ELEMENT, DEFINITION DES CARACTERISTIQUES : --------------
C --- CHOIX DU TYPE D'INTEGRATION -------------------------------------
C --- RECUPERATION DE LA GEOMETRIE ET POIDS DES POINTS D'INTEGRATION --
C --- RECUPERATION DES FONCTIONS DE FORME -----------------------------
C =====================================================================
      CALL CAETHM(NOMTE,AXI,PERMAN,VF,TYPVF,
     >            TYPMOD,MODINT,MECANI,PRESS1,PRESS2,TEMPE,
     >            DIMDEP,DIMDEF,DIMCON,NMEC,NP1,NP2,NDIM,NNO,
     >            NNOS,NNOM,NFACE,
     >            NPI,NPG,NDDLS,NDDLM,NDDLFA,NDDLK,DIMUEL,
     >            IPOIDS,IVF,IDFDE,IPOID2,IVF2,IDFDE2,NPI2,JGANO)
      CALL ASSERT(VF)        
C =====================================================================
C --- DEBUT DES DIFFERENTES OPTIONS -----------------------------------
C =====================================================================
C --- 2. OPTIONS : RIGI_MECA_TANG , FULL_MECA , RAPH_MECA -------------
C =====================================================================
      IF ((OPTION(1:9).EQ.'RIGI_MECA' ) .OR.
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
         IF (OPTION(1:9).EQ.'RIGI_MECA' .OR.
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
         IF (OPTION(1:9).EQ.'RIGI_MECA') THEN
            CALL ASSESU(NNO,NNOS,NNOM,NFACE,
     +                ZR(IGEOM),ZR(ICARCR),
     +                ZR(IDEPLM),ZR(IDEPLM),ZR(ICONTM),
     +                ZR(ICONTM),ZR(IVARIM),ZR(IVARIM),DEFGEM,DEFGEM,
     +                DSDE,ZR(IMATUU),ZR(IVECTU),
     +                ZR(IINSTM),ZR(IINSTP),OPTION,ZI(IMATE),MECANI,
     +                PRESS1,PRESS2,TEMPE,DIMDEF,DIMCON,DIMUEL,
     +                NBVARI,NDDLS,NDDLM,NDDLK,NDDLFA,
     +                NMEC,NP1,NP2,NDIM,ZK16(ICOMPO),
     >                TYPMOD,TYPVF,AXI,PERMAN,MODINT,
     >                IPOIDS,IVF,IDFDE,IPOID2,IVF2,IDFDE2,NPI2,JGANO,
     +                RETLOI)
         ELSE
C
C   DU FAIT DE L UTIISATION DES VOISINS CETTE BOUCLE 
C  NE PEUT PLUS ETRE FAITECONTRAIREMENT A LA SITUATION EF
C  ASSESU UTILISE DELTAP ET PM 
            DO 30 LI = 1,DIMUEL
               ZR(IDEPLP+LI-1) = ZR(IDEPLM+LI-1) + ZR(IDEPLP+LI-1)
   30       CONTINUE
            CALL ASSESU(NNO,NNOS,NNOM,NFACE,
     +                ZR(IGEOM),ZR(ICARCR),
     +                ZR(IDEPLM),ZR(IDEPLP),ZR(ICONTM),
     +                ZR(ICONTP),ZR(IVARIM),ZR(IVARIP),DEFGEM,DEFGEP,
     +                DSDE,ZR(IMATUU),ZR(IVECTU),
     +                ZR(IINSTM),ZR(IINSTP),OPTION,ZI(IMATE),MECANI,
     +                PRESS1,PRESS2,TEMPE,DIMDEF,DIMCON,DIMUEL,
     +                NBVARI,NDDLS,NDDLM,NDDLK,NDDLFA,
     +                NMEC,NP1,NP2,NDIM,ZK16(ICOMPO),
     >                TYPMOD,TYPVF,AXI,PERMAN,MODINT,
     >                IPOIDS,IVF,IDFDE,IPOID2,IVF2,IDFDE2,NPI2,JGANO,
     +                RETLOI)
            ZI(JCRET) = RETLOI
         END IF
      END IF
C ======================================================================
C --- 6. OPTION : FORC_NODA --------------------------------------------
C ======================================================================
      IF (OPTION.EQ.'FORC_NODA') THEN
C ======================================================================
C --- PARAMETRES EN ENTREE ---------------------------------------------
C ======================================================================
        CALL JEVECH('PCONTMR','L',ICONTM)
C ======================================================================
C --- PARAMETRES EN SORTIE ---------------------------------------------
C ======================================================================
        CALL JEVECH('PVECTUR','E',IVECTU)
        CALL FNOESU(OPTION,NNO,NNOS,NNOM,NFACE,
     +                ZR(ICONTM),ZR(IVECTU),
     +                MECANI,PRESS1,PRESS2,TEMPE,
     >                DIMCON,DIMUEL,
     >                TYPVF,AXI,
     >                IPOIDS,IVF,IDFDE,IPOID2,IVF2,IDFDE2,NPI2,JGANO,
     +                RETLOI)
      END IF
C ======================================================================
      END
