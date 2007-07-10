      SUBROUTINE TE0539(OPTION,NOMTE)
      IMPLICIT NONE
      CHARACTER*16 OPTION,NOMTE
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 10/07/2007   AUTEUR MARKOVIC D.MARKOVIC 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE GENIAUT S.GENIAUT
C
C    - FONCTION REALISEE:  CALCUL DES OPTIONS NON-LINEAIRES MECANIQUES
C                          ELEMENTS 3D AVEC X-FEM
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
      CHARACTER*8 TYPMOD(2),ELREFP
C      CHARACTER*4 FAMI
      INTEGER JGANO,NNO,NPG,I,KP,K,L,IMATUU,LGPG,NDIM,LGPG1,IRET,IJ
      INTEGER IPOIDS,IVF,IDFDE,IGEOM,IMATE
      INTEGER ITREF,ICONTM,IVARIM,ITEMPM,ITEMPP
      INTEGER IINSTM,IINSTP,IDEPLM,IDEPLP,ICOMPO,ICARCR
      INTEGER IVECTU,ICONTP,IVARIP,LI,JCRET,CODRET
      INTEGER IVARIX
      INTEGER JPINTT,JCNSET,JHEAVT,JLONCH,JBASLO,JLSN,JLST
      INTEGER KK,NI,MJ,JTAB(7),NNOS,ICAMAS,IDIM
      INTEGER DDLH,DDLC,NDDL,INO,NNOM,NFE,IBID
      LOGICAL MATSYM,LTEATT
      REAL*8  MATNS(3*27*3*27)
      REAL*8  VECT1(54), VECT2(4*27*27), VECT3(4*27*2)
      REAL*8  PFF(6*27*27),DEF(6*27*3),DFDI(3*27),DFDI2(3*27)
      REAL*8  ANGMAS(7),R8VIDE,R8DGRD,BARY(3)

C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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


C - FONCTIONS DE FORMES ET POINTS DE GAUSS
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
C      FAMI='RIGI'
      IF (NNO.GT.27) CALL U2MESS('F','ELEMENTS3_24')

C     INITIALISATION DES DIMENSIONS DES DDLS X-FEM
      CALL XTEINI(NOMTE,DDLH,NFE,IBID,DDLC,NNOM,IBID,NDDL)

C - TYPE DE MODELISATION
      IF (NDIM .EQ. 3) THEN
        TYPMOD(1) = '3D      '
        TYPMOD(2) = '        '
      ELSE
         IF (LTEATT(' ','AXIS','OUI')) THEN
           TYPMOD(1) = 'AXIS    '
         ELSE IF (NOMTE(3:4).EQ.'CP') THEN
           TYPMOD(1) = 'C_PLAN  '
         ELSE IF (NOMTE(3:4).EQ.'DP') THEN
           TYPMOD(1) = 'D_PLAN  '
         ELSE
           CALL U2MESS('F','ELEMENTS3_15')
         END IF
         IF (NOMTE(1:2).EQ.'MD') THEN
           TYPMOD(2) = 'ELEMDISC'
         ELSE IF (NOMTE(1:2).EQ.'MI') THEN
           TYPMOD(2) = 'INCO    '
         ELSE
           TYPMOD(2) = '        '
         END IF
         CODRET=0
      ENDIF

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

C     PARAM�TRES PROPRES � X-FEM
      CALL JEVECH('PPINTTO','L',JPINTT)
      CALL JEVECH('PCNSETO','L',JCNSET)
      CALL JEVECH('PHEAVTO','L',JHEAVT)
      CALL JEVECH('PLONCHA','L',JLONCH)
      CALL JEVECH('PBASLOR','L',JBASLO)
      CALL JEVECH('PLSN'   ,'L',JLSN)
      CALL JEVECH('PLST'   ,'L',JLST)

C --- ORIENTATION DU MASSIF
C     COORDONNEES DU BARYCENTRE ( POUR LE REPRE CYLINDRIQUE )

      BARY(1) = 0.D0
      BARY(2) = 0.D0
      BARY(3) = 0.D0
      DO 150 I = 1,NNO
        DO 140 IDIM = 1,NDIM
          BARY(IDIM) = BARY(IDIM)+ZR(IGEOM+IDIM+NDIM*(I-1)-1)/NNO
 140    CONTINUE
 150  CONTINUE
      CALL RCANGM ( NDIM, BARY, ANGMAS )

C - VARIABLES DE COMMANDE

      CALL JEVECH('PINSTMR','L',IINSTM)
      CALL JEVECH('PINSTPR','L',IINSTP)

C - PARAMETRES EN SORTIE

      IF (OPTION(1:10).EQ.'RIGI_MECA_' .OR.
     &    OPTION(1:9).EQ.'FULL_MECA') THEN
          CALL NMTSTM(ZK16(ICOMPO),IMATUU,MATSYM)
      ENDIF


      IF (OPTION(1:9).EQ.'RAPH_MECA' .OR.
     &    OPTION(1:9).EQ.'FULL_MECA') THEN
        CALL JEVECH('PVECTUR','E',IVECTU)
        CALL JEVECH('PCONTPR','E',ICONTP)
        CALL JEVECH('PVARIPR','E',IVARIP)

C ATTENTION : ICONTM ET ICONTP : SIGMA AUX PTS DE GAUSS DES SOUS-T�TRAS

C      ESTIMATION VARIABLES INTERNES A L'ITERATION PRECEDENTE
        CALL JEVECH('PVARIMP','L',IVARIX)
        NPG = JTAB(2)
        CALL DCOPY(NPG*LGPG,ZR(IVARIX),1,ZR(IVARIP),1)

      END IF


      IF (ZK16(ICOMPO+3) (1:9).EQ.'COMP_ELAS') THEN

C - LOIS DE COMPORTEMENT ECRITES EN CONFIGURATION DE REFERENCE
C                          COMP_ELAS

        IF (OPTION(1:10).EQ.'RIGI_MECA_') THEN

C         OPTION RIGI_MECA_TANG :         ARGUMENTS EN T-
          CALL XMEL3D('-',NNO,IPOIDS,IVF,DDLH,NFE,DDLC,IGEOM,TYPMOD,
     &                OPTION,NOMTE,ZI(IMATE),ZK16(ICOMPO),LGPG,
     &                ZR(ICARCR),ZR(JPINTT),ZI(JCNSET),ZI(JHEAVT),
     &                ZI(JLONCH),ZR(JBASLO),
     &                ZR(IDEPLM),ZR(JLSN),ZR(JLST),ZR(ICONTM),
     &                ZR(IVARIM),ZR(IMATUU),ZR(IVECTU),CODRET)

        ELSE

C        OPTION FULL_MECA OU RAPH_MECA : ARGUMENTS EN T+
          DO 200 LI = 1,NDDL
            ZR(IDEPLP+LI-1) = ZR(IDEPLM+LI-1) + ZR(IDEPLP+LI-1)
 200      CONTINUE

          CALL XMEL3D('+',NNO,IPOIDS,IVF,DDLH,NFE,DDLC,IGEOM,TYPMOD,
     &                OPTION,NOMTE,ZI(IMATE),ZK16(ICOMPO),LGPG,
     &                ZR(ICARCR),ZR(JPINTT),ZI(JCNSET),ZI(JHEAVT),
     &                ZI(JLONCH),ZR(JBASLO),
     &                ZR(IDEPLP),ZR(JLSN),ZR(JLST),ZR(ICONTP),
     &                ZR(IVARIP),ZR(IMATUU),ZR(IVECTU),CODRET)

        END IF

C      ELSEIF (NDIM .EQ. 3) THEN
      ELSE 

C - LOIS DE COMPORTEMENT ECRITE EN CONFIGURATION ACTUELLE
C                          COMP_INCR

C      PETITES DEFORMATIONS (AVEC EVENTUELLEMENT REACTUALISATION)
        IF (ZK16(ICOMPO+2) (1:5).EQ.'PETIT') THEN
          IF (ZK16(ICOMPO+2) (6:10).EQ.'_REAC') THEN
            DO 20 I = 1,3*NNO
              ZR(IGEOM+I-1) = ZR(IGEOM+I-1) + ZR(IDEPLM+I-1) +
     &                        ZR(IDEPLP+I-1)
   20       CONTINUE
          END IF

          CALL XINC3D('+',NNO,IPOIDS,IVF,DDLH,NFE,DDLC,IGEOM,
     &                ZR(IINSTM),ZR(IINSTP),ZR(IDEPLP),ZR(ICONTM),     
     &                ZR(IVARIP),TYPMOD,OPTION,NOMTE,ZI(IMATE),
     &                ZK16(ICOMPO),LGPG,ZR(ICARCR),ZR(JPINTT),
     &                ZI(JCNSET),ZI(JHEAVT),ZI(JLONCH),ZR(JBASLO),
     &                ZR(IDEPLM),ZR(JLSN),ZR(JLST),ZR(ICONTP),
     &                ZR(IVARIM),ZR(IMATUU),ZR(IVECTU),CODRET)

C 7.3 - GRANDES ROTATIONS ET PETITES DEFORMATIONS
C         ELSE IF (ZK16(ICOMPO+2) (1:5).EQ.'GREEN') THEN
C 
C           DO 50 LI = 1,3*NNO
C             ZR(IDEPLP+LI-1) = ZR(IDEPLM+LI-1) + ZR(IDEPLP+LI-1)
C    50     CONTINUE
C 
C           CALL NMGR3D(FAMI,NNO,NPG,IPOIDS,IVF,IDFDE,
C      &                ZR(IGEOM),TYPMOD,OPTION,ZI(IMATE),
C      &                ZK16(ICOMPO),LGPG,ZR(ICARCR),
C      &                ZR(IINSTM),ZR(IINSTP),
C      &                ZR(IDEPLM),ZR(IDEPLP),
C      &                ANGMAS,
C      &                ZR(ICONTM),ZR(IVARIM),MATSYM,
C      &                DFDI,PFF,DEF,ZR(ICONTP),ZR(IVARIP),
C      &                ZR(IMATUU),ZR(IVECTU),CODRET)
        ELSE
          CALL U2MESK('F','ELEMENTS3_16',1,ZK16(ICOMPO+2))
        END IF

C       ELSE
C        write(6,*) 'te0539 l 256, ZK16(ICOMPO+2) = ', ZK16(ICOMPO+2)
C        CALL U2MESS('F','ELEMENTS4_23')


C PARTIE 2D
C - HYPO-ELASTICITE

C         IF (ZK16(ICOMPO+2) (6:10).EQ.'_REAC') THEN
C CCDIR$ IVDEP
C           DO 25 I = 1,2*NNO
C             ZR(IGEOM+I-1) = ZR(IGEOM+I-1) + ZR(IDEPLM+I-1) +
C      &                      ZR(IDEPLP+I-1)
C   25     CONTINUE
C         END IF
C 
C         IF (ZK16(ICOMPO+2) (1:5).EQ.'PETIT') THEN
C 
C C -       ELEMENT A DISCONTINUITE INTERNE
C           IF (TYPMOD(2).EQ.'ELEMDISC') THEN
C 
C             CALL NMED2D(NNO,NPG,IPOIDS,IVF,IDFDE,
C      &              ZR(IGEOM),TYPMOD,OPTION,ZI(IMATE),ZK16(ICOMPO),
C      &              LGPG,ZR(ICARCR),
C      &              ZR(IDEPLM),ZR(IDEPLP),
C      &              ZR(ICONTM),ZR(IVARIM),VECT1,
C      &              VECT3,ZR(ICONTP),ZR(IVARIP),
C      &              ZR(IMATUU),ZR(IVECTU),CODRET)
C 
C           ELSE
C 
C             CALL NMPL2D(FAMI,NNO,NPG,IPOIDS,IVF,IDFDE,
C      &              ZR(IGEOM),TYPMOD,OPTION,ZI(IMATE),ZK16(ICOMPO),
C      &              LGPG,ZR(ICARCR),
C      &              ZR(IINSTM),ZR(IINSTP),
C      &              ZR(IDEPLM),ZR(IDEPLP),ANGMAS,
C      &              ZR(ICONTM),ZR(IVARIM),MATSYM,VECT1,
C      &              VECT3,ZR(ICONTP),ZR(IVARIP),
C      &              ZR(IMATUU),ZR(IVECTU),CODRET)
C 
C           ENDIF
C 
C 
C C 7.3 - GRANDES ROTATIONS ET PETITES DEFORMATIONS
C         ELSE IF (ZK16(ICOMPO+2) .EQ.'GREEN') THEN
C 
C           DO 45 LI = 1,2*NNO
C             ZR(IDEPLP+LI-1) = ZR(IDEPLM+LI-1) + ZR(IDEPLP+LI-1)
C    45     CONTINUE
C 
C           CALL NMGR2D(FAMI,NNO,NPG,IPOIDS,IVF,IDFDE,
C      &                ZR(IGEOM),TYPMOD,OPTION,ZI(IMATE),ZK16(ICOMPO),
C      &                LGPG,ZR(ICARCR),
C      &                ZR(IINSTM),ZR(IINSTP),
C      &                ZR(IDEPLM),ZR(IDEPLP),ANGMAS,
C      &                ZR(ICONTM),ZR(IVARIM),MATSYM,
C      &                VECT1,VECT2,VECT3,
C      &                ZR(ICONTP),ZR(IVARIP),
C      &                ZR(IMATUU),ZR(IVECTU),CODRET)
C         ELSE
C           CALL U2MESK('F','ELEMENTS3_16',1,ZK16(ICOMPO+2))
C         END IF
C 
       END IF

      IF (OPTION(1:9).EQ.'RAPH_MECA' .OR.
     &    OPTION(1:9).EQ.'FULL_MECA') THEN
        CALL JEVECH('PCODRET','E',JCRET)
        ZI(JCRET) = CODRET
      END IF

      END
