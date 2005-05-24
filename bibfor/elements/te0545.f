      SUBROUTINE TE0545(OPTION,NOMTE)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 23/05/2005   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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

      IMPLICIT NONE
      CHARACTER*16 OPTION,NOMTE
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES OPTIONS NON-LINEAIRES MECANIQUES
C                          EN 2D (CPLAN ET DPLAN) ET AXI
C                          POUR LES ELEMNTS GRAD_VARI
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................

      CHARACTER*8 TYPMOD(2),NOMAIL

      INTEGER NNO,NPG1,I,IMATUU,LGPG,LGPG1,LGPG2,NDIM
      INTEGER IPOIDS,IVF,IDFDE,IGEOM,IMATE,ICAMAS
      INTEGER ITREF,ICONTM,IVARIM,ITEMPM,ITEMPP,IPHASM,IPHASP
      INTEGER IINSTM,IINSTP,IDEPLM,IDEPLP,ICOMPO,ICARCR
      INTEGER IVECTU,ICONTP,IVARIP,LI,IDEFAM,IDEFAP,NNOS,JGANO
      INTEGER IHYDRM,IHYDRP,ISECHM,ISECHP,ISREF,IVARIX,IRET
      INTEGER IIRRAM,IIRRAP
      INTEGER NDDL,KK,NI,MJ,JTAB(7),IADZI,IAZK24,NZ,JCRET,CODRET
      REAL*8  MATNS(2*9*2*9)
      REAL*8  VECT1(54), VECT2(4*27*27), VECT3(4*27*2)
      REAL*8  R8VIDE,ANGMAS(3),R8DGRD
      LOGICAL DEFANE

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
C
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG1,IPOIDS,IVF,IDFDE,JGANO)
C
C - TYPE DE MODELISATION

      IF (NOMTE(3:4).EQ.'AX') THEN
        TYPMOD(1) = 'AXIS    '
      ELSE IF (NOMTE(3:4).EQ.'CP') THEN
        TYPMOD(1) = 'C_PLAN  '
      ELSE IF (NOMTE(3:4).EQ.'DP') THEN
        TYPMOD(1) = 'D_PLAN  '
      ELSE
        CALL UTMESS('F','TE0100','NOM D''ELEMENT ILLICITE')
      END IF

      TYPMOD(2) = 'GRADVARI'
      CODRET = 0

C - PARAMETRES EN ENTREE

      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PCONTMR','L',ICONTM)
      CALL JEVECH('PVARIMR','L',IVARIM)
      CALL JEVECH('PDEPLMR','L',IDEPLM)
      CALL JEVECH('PDEPLPR','L',IDEPLP)
      CALL JEVECH('PCOMPOR','L',ICOMPO)
      CALL JEVECH('PCARCRI','L',ICARCR)

      CALL TECACH('OON','PVARIMR',7,JTAB,IRET)
      LGPG1 = MAX(JTAB(6),1)*JTAB(7)
      LGPG = LGPG1

C     ORIENTATION DU MASSIF     
      CALL RCANGM ( NDIM, ANGMAS )

C - VARIABLES DE COMMANDE

      CALL JEVECH('PTEREF','L',ITREF)
      CALL JEVECH('PTEMPMR','L',ITEMPM)
      CALL JEVECH('PTEMPPR','L',ITEMPP)
      CALL JEVECH('PINSTMR','L',IINSTM)
      CALL JEVECH('PINSTPR','L',IINSTP)
      CALL TECACH('ONN','PDEFAMR',1,IDEFAM,IRET)
      DEFANE = IRET .EQ. 0
      CALL TECACH('ONN','PDEFAPR',1,IDEFAP,IRET)
      CALL TECACH('NNN','PPHASMR',1,IPHASM,IRET)
      CALL TECACH('NNN','PPHASPR',1,IPHASP,IRET)
      IF (IRET.EQ.0) THEN
        CALL TECACH('OON','PPHASPR',7,JTAB,IRET)
        NZ = JTAB(6)
      END IF

      CALL JEVECH('PHYDRMR','L',IHYDRM)
      CALL JEVECH('PHYDRPR','L',IHYDRP)
      CALL JEVECH('PSECHMR','L',ISECHM)
      CALL JEVECH('PSECHPR','L',ISECHP)
      CALL JEVECH('PSECREF','L',ISREF)
      CALL JEVECH('PIRRAMR','L',IIRRAM)
      CALL JEVECH('PIRRAPR','L',IIRRAP)

C PARAMETRES EN SORTIE

      IF (OPTION(1:10).EQ.'RIGI_MECA_' .OR.
     &    OPTION(1:9).EQ.'FULL_MECA') THEN
        CALL JEVECH('PMATUUR','E',IMATUU)
      END IF

      IF (OPTION(1:9).EQ.'RAPH_MECA' .OR.
     &    OPTION(1:9).EQ.'FULL_MECA') THEN
        CALL JEVECH('PVECTUR','E',IVECTU)
        CALL JEVECH('PCONTPR','E',ICONTP)
        CALL JEVECH('PVARIPR','E',IVARIP)

C      ESTIMATION VARIABLES INTERNES A L'ITERATION PRECEDENTE
        CALL JEVECH('PVARIMP','L',IVARIX)
        CALL DCOPY(NPG1*LGPG,ZR(IVARIX),1,ZR(IVARIP),1)
      END IF



C - HYPER-ELASTICITE

      IF (ZK16(ICOMPO+3) (1:9).EQ.'COMP_ELAS') THEN

        IF (OPTION(1:10).EQ.'RIGI_MECA_') THEN

C        OPTION RIGI_MECA_TANG :         ARGUMENTS EN T-
          CALL NMEL2D(NNO,NPG1,IPOIDS,IVF,IDFDE,
     &                ZR(IGEOM),TYPMOD,OPTION,ZI(IMATE),ZK16(ICOMPO),
     &                LGPG,ZR(ICARCR),ZR(ITEMPM),ZR(IHYDRM),ZR(ISECHM),
     &                ZR(ITREF),ZR(IDEPLM),VECT1,VECT2,
     &                VECT3,ZR(ICONTM),ZR(IVARIM),
     &                ZR(IMATUU),ZR(IVECTU),CODRET)

        ELSE

C        OPTION FULL_MECA OU RAPH_MECA : ARGUMENTS EN T+
          DO 10 LI = 1,2*NNO
            ZR(IDEPLP+LI-1) = ZR(IDEPLM+LI-1) + ZR(IDEPLP+LI-1)
   10     CONTINUE

          CALL NMEL2D(NNO,NPG1,IPOIDS,IVF,IDFDE,
     &                ZR(IGEOM),TYPMOD,OPTION,ZI(IMATE),ZK16(ICOMPO),
     &                LGPG,ZR(ICARCR),ZR(ITEMPP),ZR(IHYDRP),ZR(ISECHP),
     &                ZR(ITREF),ZR(IDEPLP),VECT1,VECT2,
     &                VECT3,ZR(ICONTP),ZR(IVARIP),
     &                ZR(IMATUU),ZR(IVECTU),CODRET)
        END IF

      ELSE


C - HYPO-ELASTICITE

        IF (ZK16(ICOMPO+2) (6:10).EQ.'_REAC') THEN
CCDIR$ IVDEP
          DO 20 I = 1,2*NNO
            ZR(IGEOM+I-1) = ZR(IGEOM+I-1) + ZR(IDEPLM+I-1) +
     &                      ZR(IDEPLP+I-1)
   20     CONTINUE
        END IF

        IF (ZK16(ICOMPO+2) (1:5).EQ.'PETIT') THEN

          CALL NMPL2D(NNO,NPG1,IPOIDS,IVF,IDFDE,
     &                ZR(IGEOM),TYPMOD,OPTION,ZI(IMATE),ZK16(ICOMPO),
     &                LGPG,ZR(ICARCR),
     &                ZR(IINSTM),ZR(IINSTP),
     &                ZR(ITEMPM),ZR(ITEMPP),
     &                ZR(IHYDRM),ZR(IHYDRP),ZR(ITREF),
     &                ZR(ISECHM),ZR(ISECHP),ZR(ISREF),
     &                ZR(IIRRAM),ZR(IIRRAP),
     &                NZ,ZR(IPHASM),ZR(IPHASP),
     &                ZR(IDEPLM),ZR(IDEPLP),ZR(IDEFAM),ZR(IDEFAP),
     &                DEFANE,
     &                ANGMAS,
     &                ZR(ICONTM),ZR(IVARIM),.TRUE.,VECT1,
     &                VECT3,ZR(ICONTP),ZR(IVARIP),
     &                ZR(IMATUU),ZR(IVECTU),CODRET)


C      GRANDES DEFORMATIONS : FORMULATION SIMO - MIEHE

        ELSE IF (ZK16(ICOMPO+2) (1:10).EQ.'SIMO_MIEHE') THEN
          CALL NMGP2D(NNO,NPG1,IPOIDS,IVF,IDFDE,
     &                ZR(IGEOM),TYPMOD,OPTION,ZI(IMATE),ZK16(ICOMPO),
     &                LGPG,ZR(ICARCR),
     &                ZR(IINSTM),ZR(IINSTP),
     &                ZR(ITEMPM),ZR(ITEMPP),ZR(ITREF),
     &                ZR(IHYDRM),ZR(IHYDRP),
     &                ZR(ISECHM),ZR(ISECHP),ZR(ISREF),
     &                ZR(IIRRAM),ZR(IIRRAP),
     &                NZ,ZR(IPHASM),ZR(IPHASP),
     &                ZR(IDEPLM),ZR(IDEPLP),
     &                ANGMAS,
     &                ZR(ICONTM),ZR(IVARIM),
     &                VECT1,VECT2,ZR(ICONTP),ZR(IVARIP),
     &                MATNS,ZR(IVECTU),CODRET)

C        SYMETRISATION DE MATNS DANS MATUU
          IF (OPTION(1:4).EQ.'RIGI' .OR. OPTION(1:4).EQ.'FULL') THEN
            NDDL = 2*NNO
            KK = 0
            DO 40 NI = 1,NDDL
              DO 30 MJ = 1,NI
                ZR(IMATUU+KK) = (MATNS((NI-1)*NDDL+MJ)+
     &                          MATNS((MJ-1)*NDDL+NI))/2.D0
                KK = KK + 1
   30         CONTINUE
   40       CONTINUE
          END IF

C 7.3 - GRANDES ROTATIONS ET PETITES DEFORMATIONS
        ELSE IF (ZK16(ICOMPO+2) .EQ.'GREEN') THEN

          DO 45 LI = 1,2*NNO
            ZR(IDEPLP+LI-1) = ZR(IDEPLM+LI-1) + ZR(IDEPLP+LI-1)
   45     CONTINUE

          CALL NMGR2D(NNO,NPG1,IPOIDS,IVF,IDFDE,
     &                ZR(IGEOM),TYPMOD,OPTION,ZI(IMATE),ZK16(ICOMPO),
     &                LGPG,ZR(ICARCR),
     &                ZR(IINSTM),ZR(IINSTP),
     &                ZR(ITEMPM),ZR(ITEMPP),ZR(ITREF),
     &                ZR(IHYDRM),ZR(IHYDRP),
     &                ZR(ISECHM),ZR(ISECHP),ZR(ISREF),
     &                ZR(IIRRAM),ZR(IIRRAP),
     &                NZ,ZR(IPHASM),ZR(IPHASP),
     &                ZR(IDEPLM),ZR(IDEPLP),ZR(IDEFAM),ZR(IDEFAP),
     &                DEFANE,
     &                ANGMAS,
     &                ZR(ICONTM),ZR(IVARIM),
     &                VECT1,VECT2,VECT3,
     &                ZR(ICONTP),ZR(IVARIP),
     &                ZR(IMATUU),ZR(IVECTU),CODRET)
        ELSE
          CALL UTMESS('F','TE0100','COMPORTEMENT:'//ZK16(ICOMPO+2)//
     &                'NON IMPLANTE')
        END IF

      END IF
      IF (OPTION(1:9).EQ.'FULL_MECA' .OR.
     &    OPTION(1:9).EQ.'RAPH_MECA') THEN
        CALL JEVECH('PCODRET','E',JCRET)
        ZI(JCRET) = CODRET
      END IF
      END
