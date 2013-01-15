      SUBROUTINE TE0313(OPTION,NOMTE)

      IMPLICIT NONE

      INCLUDE 'jeveux.h'
      CHARACTER*16 OPTION,NOMTE

C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 15/01/2013   AUTEUR DELMAS J.DELMAS 
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

C =====================================================================
C    - FONCTION REALISEE: FULL_MECA, RIGI_MECA, RAPH_MECA, FORC_NODA
C                         VARI_ELNO,SIEF_ELNO
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C =====================================================================
      INTEGER JGANO,IMATUU,NDIM,IMATE,IINSTM,JCRET,NCMP,NVIM
      INTEGER IRET,ICHG,ICHN,ITABIN(7),ITABOU(7)
      INTEGER IVF2
      INTEGER IDF2,NPI,NPG
      INTEGER RETLOI,IRETP,IRETM
      INTEGER IPOIDS,IVF1,IDF1,IGEOM
      INTEGER IINSTP,IDEPLM,IDEPLP,ICOMPO,ICARCR,ICAMAS
      INTEGER ICONTM,IVARIP,IVARIM,IVECTU,ICONTP

C =====================================================================
C =====================================================================

      INTEGER MECANI(8),PRESS1(9),PRESS2(9),TEMPE(5),DIMUEL
      INTEGER DIMDEF,DIMCON,NBVARI
      INTEGER NNO1,NNO2
      INTEGER IADZI,IAZK24
      INTEGER IU(3,18),IP(2,9),IPF(2,2,9),IQ(2,2,9)
      REAL*8 R(22)
      REAL*8 ANG(24)
      CHARACTER*3 MODINT
      CHARACTER*8 NOMAIL

C =====================================================================
      INTEGER     ISMAEM,LI
      LOGICAL     AXI, PERMAN

C =====================================================================
C AXI       AXISYMETRIQUE?
C PERMAN    REGIME PERMANENT ?
C NNO1      NB DE NOEUDS DES BORDS INF ET SUP DE L'ELEMENT
C NNO2      NB DE NOEUDS DU SEGEMENT CENTRAL DE L'ELEMENT
C NPI       NB DE POINTS D'INTEGRATION DE L'ELEMENT
C NPG       NB DE POINTS DE GAUSS     POUR CLASSIQUE(=NPI)
C NDIM      DIMENSION DE L'ESPACE
C DIMUEL    NB DE DDL TOTAL DE L'ELEMENT
C DIMCON    DIMENSION DES CONTRAINTES GENERALISEES ELEMENTAIRES
C DIMDEF    DIMENSION DES DEFORMATIONS GENERALISEES ELEMENTAIRES
C NBVARI    NB DE VARIABLES INTERNES
C IU        DECALAGE D'INDICE POUR ACCEDER AUX DDL DE DEPLACEMENT
C IP        DECALAGE D'INDICE POUR ACCEDER AUX DDL DE PRESSION MILIEU
C IPF       DECALAGE D'INDICE POUR ACCEDER AUX DDL DE PRESSION FACES
C IQ        DECALAGE D'INDICE POUR ACCEDER AUX DDL DE LAGRANGE HYDRO
C ANG       ANGLES NAUTIQUES POUR ORIENTATION ELEMENT
C MODINT    MODE D'INTEGRATION
C NOMAIL    NUMERO DE MAILLE
C IVF       FONCTIONS DE FORMES QUADRATIQUES
C IVF2      FONCTIONS DE FORMES LINEAIRES
C =====================================================================
      LOGICAL FNOEVO
      REAL*8  DT

C =====================================================================
C --- 1. INITIALISATIONS ----------------------------------------------
C --- SUIVANT ELEMENT, DEFINITION DES CARACTERISTIQUES : --------------
C --- CHOIX DU TYPE D'INTEGRATION -------------------------------------
C --- RECUPERATION DE LA GEOMETRIE ET POIDS DES POINTS D'INTEGRATION --
C --- RECUPERATION DES FONCTIONS DE FORME -----------------------------
C =====================================================================

      CALL CAEIHM(NOMTE,AXI,PERMAN,
     >                  MECANI,PRESS1,PRESS2,TEMPE,
     >                  DIMDEF,DIMCON,NDIM,NNO1,
     >                  NNO2,NPI,NPG,DIMUEL,IPOIDS,IVF1,IDF1,
     >                  IVF2,IDF2,JGANO,IU,IP,IPF,IQ,MODINT)

       CALL TECAEL(IADZI,IAZK24)
       NOMAIL = ZK24(IAZK24-1+3) (1:8)

C RECUPERATION DES ANGLES NAUTIQUES DEFINIS PAR AFFE_CARA_ELEM
      IF ((OPTION .EQ. 'FORC_NODA') .OR.
     +    (OPTION(1:9).EQ.'RIGI_MECA' ) .OR.
     +    (OPTION(1:9).EQ.'RAPH_MECA' ) .OR.
     +    (OPTION(1:9).EQ.'FULL_MECA' )) THEN

        CALL JEVECH('PCAMASS','L',ICAMAS)
        IF (ZR(ICAMAS).EQ.-1.D0) CALL U2MESS('F','ELEMENTS5_48')

C DEFINITION DES ANGLES NAUTIQUES AUX NOEUDS SOMMETS
        CALL EIANGL(NDIM,NNO2,ZR(ICAMAS+1),ANG)
      END IF
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

            CALL ASEIHM(OPTION,AXI,NDIM,NNO1,NNO2,NPI,NPG,
     &                    DIMUEL,DIMDEF,DIMCON,NBVARI,ZI(IMATE),
     &                    IU,IP,IPF,IQ,
     &                    MECANI,PRESS1,PRESS2,TEMPE,ZR(IVF1),
     &                    ZR(IVF2),ZR(IDF2),ZR(IINSTM),ZR(IINSTP),
     &                    ZR(IDEPLM),ZR(IDEPLM),ZR(ICONTM),ZR(ICONTM),
     &                    ZR(IVARIM),ZR(IVARIM),NOMAIL,
     &                    ZR(IPOIDS),ZR(IGEOM),ANG,ZK16(ICOMPO),PERMAN,
     &                    ZR(ICARCR),ZR(IVECTU),ZR(IMATUU),RETLOI)
         ELSE
            DO 30 LI = 1,DIMUEL
               ZR(IDEPLP+LI-1) = ZR(IDEPLM+LI-1) + ZR(IDEPLP+LI-1)
 30         CONTINUE

            CALL ASEIHM(OPTION,AXI,NDIM,NNO1,NNO2,NPI,NPG,
     &                    DIMUEL,DIMDEF,DIMCON,NBVARI,ZI(IMATE),
     &                    IU,IP,IPF,IQ,
     &                    MECANI,PRESS1,PRESS2,TEMPE,ZR(IVF1),
     &                    ZR(IVF2),ZR(IDF2),ZR(IINSTM),ZR(IINSTP),
     &                    ZR(IDEPLM),ZR(IDEPLP),ZR(ICONTM),ZR(ICONTP),
     &                    ZR(IVARIM),ZR(IVARIP),NOMAIL,
     &                    ZR(IPOIDS),ZR(IGEOM),ANG,ZK16(ICOMPO),PERMAN,
     &                    ZR(ICARCR),ZR(IVECTU),ZR(IMATUU),RETLOI)

            ZI(JCRET) = RETLOI
         END IF

      END IF

C ======================================================================
C --- 3. OPTION : FORC_NODA --------------------------------------------
C ======================================================================
      IF (OPTION.EQ.'FORC_NODA') THEN
C ======================================================================
C --- PARAMETRES EN ENTREE ---------------------------------------------
C ======================================================================
        CALL JEVECH('PGEOMER','L',IGEOM)
        CALL JEVECH('PCONTMR','L',ICONTM)
        CALL JEVECH('PMATERC','L',IMATE)
C ======================================================================
C --- SI LES TEMPS PLUS ET MOINS SONT PRESENTS -------------------------
C --- C EST QUE L ON APPELLE DEPUIS STAT NON LINE ET -------------------
C --- ALORS LES TERMES DEPENDANT DE DT SONT EVALUES --------------------
C ======================================================================
        CALL TECACH('ONN','PINSTMR','L',1,IINSTM,IRETM)
        CALL TECACH('ONN','PINSTPR','L',1,IINSTP,IRETP)
        IF (IRETM.EQ.0 .AND. IRETP.EQ.0) THEN
           DT = ZR(IINSTP) - ZR(IINSTM)
           FNOEVO = .TRUE.
        ELSE
           FNOEVO = .FALSE.
           DT = 0.D0
        ENDIF

C ======================================================================
C --- PARAMETRES EN SORTIE ---------------------------------------------
C ======================================================================
        CALL JEVECH('PVECTUR','E',IVECTU)

        CALL  FNEIHM(FNOEVO,DT,PERMAN,NNO1,NNO2,NPI,NPG,
     +                   ZR(IPOIDS),IU,IP,IPF,IQ,ZR(IVF1),
     +                   ZR(IVF2),ZR(IDF2),ZR(IGEOM),ANG,
     +                   ZR(ICONTM),R,ZR(IVECTU),MECANI,PRESS1,PRESS2,
     +                   TEMPE,DIMDEF,DIMCON,DIMUEL,
     +                   NDIM,AXI)

      END IF

C ======================================================================
C --- 4. OPTION : SIEF_ELNO ---------------------------------------
C ======================================================================
      IF (OPTION .EQ. 'SIEF_ELNO') THEN
        CALL JEVECH('PCONTRR','L',ICHG)
        CALL JEVECH('PSIEFNOR','E',ICHN)


        NVIM=MECANI(6)

        CALL POEIHM(NOMTE,OPTION,MODINT,JGANO,NNO1,NNO2,DIMCON,NVIM,
     &              ZR(ICHG),ZR(ICHN))
      END IF

C ======================================================================
C --- 5. OPTION : VARI_ELNO ---------------------------------------
C ======================================================================
      IF (OPTION .EQ. 'VARI_ELNO') THEN
        CALL TECACH('OOO','PVARIGR','L',7,ITABIN,IRET)
        CALL TECACH('OOO','PVARINR','E',7,ITABOU,IRET)
        ICHG=ITABIN(1)
        ICHN=ITABOU(1)

        CALL JEVECH('PCOMPOR','L',ICOMPO)
        READ (ZK16(ICOMPO+1),'(I16)') NCMP
        READ (ZK16(ICOMPO-1+7+9+4),'(I16)') NVIM

        CALL POEIHM(NOMTE,OPTION,MODINT,JGANO,NNO1,NNO2,NCMP,NVIM,
     &              ZR(ICHG),ZR(ICHN))
      END IF

C ======================================================================
      END
