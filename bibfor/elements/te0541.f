      SUBROUTINE TE0541(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 08/12/2009   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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

      IMPLICIT NONE
      CHARACTER*16 OPTION,NOMTE
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES OPTIONS NON-LINEAIRES MECANIQUES
C                          ELEMENTS 3D
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXR8,JEXATR
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------


      CHARACTER*24 BLAN24
      PARAMETER ( BLAN24 = '                        ' )
      CHARACTER*8 TYPMOD(2),ELREFE
      INTEGER NNO,NPG1,LGPG,LGPG1,IRET,NDIM
      INTEGER NNOS,JGANO
      INTEGER IPOIDS,IVF,IDFDE,IGEOM,IMATE,IMATSE
      INTEGER ICONTM,IVARIM
      INTEGER IDEPLM,IDEPLP,ICOMPO
      INTEGER IVECTU,ICONTP,IVARIP
      INTEGER JTAB(7),ICONSM,IVARSM,IPSENS,IDEPSM,IDEPSP
      INTEGER ICONSP,IVARSP,ICPARS
      REAL*8 DEF(6*27*3),DFDI(3*27)

      CALL ELREF1(ELREFE)

C -- FONCTIONS DE FORMES ET POINTS DE GAUSS
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG1,IPOIDS,IVF,IDFDE,JGANO)

C     MATNS MAL DIMENSIONNEE
      CALL ASSERT(NNO.LE.27) 


C - TYPE DE MODELISATION
      TYPMOD(1) = '3D      '
      TYPMOD(2) = '        '



C -- PARAMETRES EN ENTREE

      IF ((OPTION.EQ.'MECA_SENS_MATE').OR.(OPTION.EQ.'MECA_SENS_CHAR')
     &    .OR.(OPTION.EQ.'MECA_SENS_RAPH')) THEN
        CALL JEVECH('PGEOMER','L',IGEOM)
        CALL JEVECH('PMATERC','L',IMATE)
        CALL JEVECH('PCOMPOR','L',ICOMPO)
        CALL JEVECH('PDEPLMR','L',IDEPLM)
        CALL JEVECH('PDEPLPR','L',IDEPLP)
        CALL JEVECH('PCONTMS','L',ICONSM)
        CALL JEVECH('PVARIMS','L',IVARSM)
        CALL JEVECH('PVARIMR','L',IVARIM)
        CALL JEVECH('PCONTMR','L',ICONTM)
        CALL JEVECH('PARSENS','L',IPSENS)
        CALL JEVECH('PVARIPR','L',IVARIP)
        CALL JEVECH('PCONTPR','L',ICONTP)
        IF (ZK24(IPSENS).NE.BLAN24) THEN
          CALL JEVECH('PMATSEN','L',IMATSE)
        ELSE
          IMATSE = IMATE
        ENDIF
        CALL TECACH('OON','PVARIMR',7,JTAB,IRET)
        LGPG1 = MAX(JTAB(6),1)*JTAB(7)
        LGPG = LGPG1
        IF (OPTION.EQ.'MECA_SENS_RAPH') THEN
          CALL JEVECH('PDEPLMS','L',IDEPSM)
          CALL JEVECH('PDEPLPS','L',IDEPSP)
        ENDIF
      ELSE
        CALL U2MESK('F','ALGORITH7_61',1,OPTION)
      ENDIF



C -- PARAMETRES EN SORTIE

      IF(OPTION.EQ.'MECA_SENS_MATE'.OR.OPTION.EQ.'MECA_SENS_CHAR')THEN
        CALL JEVECH('PVECTUR','E',IVECTU)
        CALL JEVECH('PCONTPS','E',ICONSP)
      END IF

      IF (OPTION.EQ.'MECA_SENS_RAPH') THEN
        CALL JEVECH('PVARIPS','E',IVARSP)
        CALL JEVECH('PCONTPS','E',ICONSP)
        CALL JEVECH('PCOPARS','L',ICPARS)
C      ESTIMATION VARIABLES INTERNES A L'ITERATION PRECEDENTE
C       CALL JEVECH('PVARIMR','L',IVARIX)
C       CALL R8COPY(NPG1*LGPG,ZR(IVARIX),1,ZR(IVARSP),1)
      END IF


C -- HYPER-ELASTICITE

      IF (ZK16(ICOMPO+3) (1:9).EQ.'COMP_ELAS') THEN
        CALL U2MESK('F','SENSIBILITE_35', 1 ,ZK16(ICOMPO+3))

      ELSE

C -- HYPO-ELASTICITE

        IF (ZK16(ICOMPO+2) (6:10).EQ.'_REAC') THEN
          CALL U2MESK('F','SENSIBILITE_35', 1 ,ZK16(ICOMPO+2))
        END IF

        IF (ZK16(ICOMPO+2) (1:5).EQ.'PETIT') THEN

          CALL NSPL3D(NNO,NPG1,IPOIDS,IVF,IDFDE,
     &                ZR(IGEOM),TYPMOD,OPTION,ZI(IMATE),
     &                ZI(IMATSE),ZK16(ICOMPO),LGPG,
     &                ZR(IDEPLM),ZR(IDEPLP),ZR(ICONSM),ZR(IVARSM),
     &                ZR(IVARIM),ZR(ICONTM),ZK24(IPSENS),
     &                ZR(IDEPSM),ZR(IDEPSP),ZR(IVARIP),ZR(ICONTP),
     &                ZR(IVECTU),ZR(ICONSP),ZR(IVARSP),ZR(ICPARS),
     &                DFDI,DEF)


C -- GRANDES DEFORMATIONS : FORMULATION SIMO - MIEHE

        ELSE IF (ZK16(ICOMPO+2)(1:10).EQ.'SIMO_MIEHE') THEN
          CALL U2MESK('F','SENSIBILITE_35', 1 ,ZK16(ICOMPO+2))

C -- GRANDES ROTATIONS ET PETITES DEFORMATIONS
        ELSE IF (ZK16(ICOMPO+2) .EQ.'GROT_GDEP') THEN
          CALL U2MESK('F','SENSIBILITE_35', 1 ,ZK16(ICOMPO+2))

        ELSE
          CALL U2MESK('F','ELEMENTS3_16',1,ZK16(ICOMPO+2))
        END IF

      END IF

      END
