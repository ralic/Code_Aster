      SUBROUTINE TE0600(OPTION,NOMTE)
      IMPLICIT NONE
      CHARACTER*16 OPTION,NOMTE,PHENOM
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 30/03/2004   AUTEUR CIBHHLV L.VIVAN 
C RESPONSABLE UFBHHLL C.CHAVANT
C =====================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C =====================================================================
C TOLE CRP_20
C TOLE CRP_21
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES OPTIONS NON-LINEAIRES MECANIQUES
C                          ELEMENTS THHM  ET HM
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................

      INTEGER JGANO,NNO,IMATUU,NDIM,IMATE,IINSTM,IFORC,JCRET
      INTEGER RETLOI,IRET
      INTEGER IPOIDS,IVF,IDFDE,IGEOM
      INTEGER IINSTP,IDEPLM,IDEPLP,ICOMPO,ICARCR,IPESA
      INTEGER ICONTM,IVARIP,IVARIM,ITREF,IVECTU,ICONTP
      CHARACTER*8 ALIAS
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

      INTEGER MECANI(5),PRESS1(7),PRESS2(7),TEMPE(5)
      INTEGER DIMDEF,DIMCON,NBVARI,NDDL,II
      INTEGER NMEC,NP1,NP2,NT,I,NPGU,NCMP,NNOS,ICHG,ICHN
      INTEGER JTAB(7)
      REAL*8 DEFGEP(21),DEFGEM(21)
      REAL*8 DFDX(27),DFDY(27),DFDZ(27),POIDS
      REAL*8 DFDI(20,3),B(21,120)
      REAL*8 DRDE(21,21),DRDS(21,31),DSDE(31,21),R(21)
      CHARACTER*8 TYPMOD(2)
C
      INTEGER NNOMAX,NVOMAX,NSOMAX
      PARAMETER (NNOMAX=20,NVOMAX=4,NSOMAX=8)
      INTEGER VOISIN(NVOMAX,NNOMAX)
      INTEGER NBVOS(NSOMAX)
      INTEGER     ISMAEM,LI,KP,J,L,K,NSOM,JSIGTM,JFTEMP,JBSIGM,INDICE
      REAL*8      R8BID,RHO,COEF,RX,R8VIDE,R8MIEM
      CHARACTER*2 CODRET
      LOGICAL     AXI,DPLAN,TRAITE,P2P1,LUMPED
      LOGICAL     VOL2,BORD2,VOL3,BORD3
      LOGICAL     ISTHT3,ISTHQ4,ISTHT6,ISTHQ8,ISTHS2,ISTHS3
      LOGICAL     ISTHF8,ISTHF6,ISTH10,ISTH13,ISTH15,ISTH20
C     -----------------------------------------------------------------
C
C  CETTE ROUTINE FAIT UN CALCUL EN THHM , HM , HHM , THH
C  21 = 9 DEF MECA + 4 POUR P1 + 4 POUR P2 + 4 POUR T
C  31 = 7 MECA + 2*5 POUR P1 + 2*5 POUR P2 + 4 POUR T

C  POUR LES TABLEAUX DEFGEP ET DEFGEM ON A DANS L ORDRE :
C                                      DX DY DZ
C                                      EPXX EPYY EPZZ EPXY EPXZ EPYZ
C                                      PRE1 P1DX P1DY P1DZ
C                                      PRE2 P2DX P2DY P2DZ
C                                      TEMP TEDX TEDY TEDZ
C            EPSXY = RAC2/2*(DU/DY+DV/DX)

C    POUR LES CHAMPS DE CONTRAINTE
C                                      SIXX SIYY SIZZ SIXY SIXZ SIYZ
C                                      SIP
C                                      M11 FH11X FH11Y FH11Z
C                                      ENT11
C                                      M12 FH12X FH12Y FH12Z
C                                      ENT12
C                                      M21 FH21X FH21Y FH21Z
C                                      ENT21
C                                      M22 FH22X FH22Y FH22Z
C                                      ENT22
C                                      QPRIM FHTX FHTY FHTZ
C        SIXY EST LE VRAI DE LA MECANIQUE DES MILIEUX CONTINUS
C        DANS EQUTHM ON LE MULITPLIER PAR RAC2

C   POUR L OPTION FORCNODA
C  SI LES TEMPS PLUS ET MOINS SONT PRESENTS
C  C EST QUE L ON APPELLE DEPUIS STAT NON LINE  : FNOEVO = VRAI
C  ET ALORS LES TERMES DEPENDANT DE DT SONT EVALUES

C  SI LES TEMPS PLUS ET MOINS NE SONT PAS PRESENTS
C  C EST QUE L ON APPELLE DEPUIS CALCNO  : FNOEVO = FAUX
C  ET ALORS LES TERMES DEPENDANT DE DT SONT PAS EVALUES

      LOGICAL FNOEVO
      REAL*8 DT
C *********************************************************************

C  SUIVANT ELEMENT, DEFINITION DE CARACTERISTIQUES
C 
C

C  ON APPELLE CAETHM AVEC POUR LES ELEMENTS VOLUMIQUES
C  2D EN METTANT VOL2 SACHANT QUE ALORS POUR LES ELEMENTS
C  3D IL NE FERA RIEN CE DONT ON S APERCEVRA PAR L ARGUMENT
C     TRAITE

      BORD2 = .FALSE.
      BORD3 = .FALSE.
      VOL2 = .TRUE.
      VOL3 = .TRUE.
       
      CALL CAETHM(NOMTE,VOL2,BORD2,VOL3,BORD3,ALIAS,AXI,DPLAN,TRAITE,
     &            ISTHS2,ISTHS3,ISTHF8,ISTHF6,ISTH10,ISTH13,ISTH15,
     &            ISTH20,ISTHT3,ISTHQ4,ISTHT6,ISTHQ8,NNOMAX,NVOMAX,
     &            NSOMAX,NSOM,VOISIN,NBVOS,P2P1,LUMPED)

      IF ( TRAITE ) THEN
        IF (AXI) THEN
          TYPMOD(1) = 'AXIS    '
        ELSE IF (DPLAN) THEN
          TYPMOD(1) = 'D_PLAN  '
        ELSE
          TYPMOD(1) = '3D      '
        END IF
      ELSE
        CALL UTMESS('F','TE0600','ELEM INCONNU DE CAETHM')
      END IF
      TYPMOD(2) = '        '

      IF ( LUMPED ) THEN
         CALL ELREF4(' ','NOEU_S',NDIM,NNO,NNOS,NPGU,IPOIDS,IVF,IDFDE,
     &               JGANO)
      ELSE
         CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPGU,IPOIDS,IVF,IDFDE,
     &               JGANO)
      ENDIF
C
C SI MODELISATION = THHM

      IF (NOMTE(1:4).EQ.'THHM') THEN
        MECANI(1) = 1
        PRESS1(1) = 1
        PRESS2(1) = 1
        TEMPE(1) = 1
        PRESS1(2) = 2
        PRESS2(2) = 1
      END IF

C SI MODELISATION = THH2M

      IF (NOMTE(1:5).EQ.'THH2M') THEN
        MECANI(1) = 1
        PRESS1(1) = 1
        PRESS2(1) = 1
        TEMPE(1) = 1
        PRESS1(2) = 2
        PRESS2(2) = 2
      END IF

C SI MODELISATION = HM

      IF (NOMTE(1:2).EQ.'HM') THEN
        MECANI(1) = 1
        PRESS1(1) = 1
        PRESS2(1) = 0
        TEMPE(1) = 0
        PRESS1(2) = 1
        PRESS2(2) = 0
      END IF

C SI MODELISATION = HHM
C ON RESERVE DE LA PLACE POUR LES TROIS CONSTITUANTS. MAIS IL EST
C POSSIBLE DE N'EN REMPLIR QUE DEUX EN LAISSANT LE DERNIER A ZERO

      IF (NOMTE(1:3).EQ.'HHM') THEN
        MECANI(1) = 1
        PRESS1(1) = 1
        PRESS2(1) = 1
        TEMPE(1) = 0
        PRESS1(2) = 2
        PRESS2(2) = 1
      END IF
C SI MODELISATION = HHM
C ON RESERVE DE LA PLACE POUR LES TROIS CONSTITUANTS. MAIS IL EST
C POSSIBLE DE N'EN REMPLIR QUE DEUX EN LAISSANT LE DERNIER A ZERO

      IF (NOMTE(1:4).EQ.'HH2M') THEN
        MECANI(1) = 1
        PRESS1(1) = 1
        PRESS2(1) = 1
        TEMPE(1) = 0
        PRESS1(2) = 2
        PRESS2(2) = 2
      END IF


C SI MODELISATION = THH

      IF (NOMTE(1:4).EQ.'THH_') THEN
        MECANI(1) = 0
        PRESS1(1) = 1
        PRESS2(1) = 1
        TEMPE(1) = 1
        PRESS1(2) = 2
        PRESS2(2) = 1
      END IF
C SI MODELISATION = THH2

      IF (NOMTE(1:5).EQ.'THH2_') THEN
        MECANI(1) = 0
        PRESS1(1) = 1
        PRESS2(1) = 1
        TEMPE(1) = 1
        PRESS1(2) = 2
        PRESS2(2) = 2
      END IF
C SI MODELISATION = THV

      IF (NOMTE(1:4).EQ.'THV_') THEN
        MECANI(1) = 0
        PRESS1(1) = 1
        PRESS2(1) = 0
        TEMPE(1) = 1
        PRESS1(2) = 2
        PRESS2(2) = 0
      END IF


C SI MODELISATION = THM

      IF (NOMTE(1:4).EQ.'THM_') THEN
        MECANI(1) = 1
        PRESS1(1) = 1
        PRESS2(1) = 0
        TEMPE(1) = 1
        PRESS1(2) = 1
        PRESS2(2) = 0
      END IF

C **********************************************************************


C   LES AUTRES VALEURS DES TABLEAUX MECA,PRESS1,PRESS2,TEMPE
C   SE DEFINISSENT AUTOMATIQUEMENT :

C NOMBRE DE DEFORMATIONS ET DE CONTRAINTES DE CHAQUE PROBLEME
      IF (MECANI(1).EQ.1) THEN
        MECANI(4) = NDIM + 6
        MECANI(5) = 7
        NMEC = NDIM
      ELSE
        MECANI(4) = 0
        MECANI(5) = 0
        NMEC = 0
      END IF

      IF (PRESS1(1).EQ.1) THEN
        PRESS1(6) = 1 + NDIM
        PRESS1(7) = 1 + NDIM
        NP1 = 1
        IF (TEMPE(1).EQ.1) PRESS1(7) = PRESS1(7) + 1
      ELSE
        PRESS1(6) = 0
        PRESS1(7) = 0
        NP1 = 0
      END IF

      IF (PRESS2(1).EQ.1) THEN
        PRESS2(6) = 1 + NDIM
        PRESS2(7) = 1 + NDIM
        NP2 = 1
        IF (TEMPE(1).EQ.1) PRESS2(7) = PRESS2(7) + 1
      ELSE
        PRESS2(6) = 0
        PRESS2(7) = 0
        NP2 = 0
      END IF

      IF (TEMPE(1).EQ.1) THEN
        TEMPE(4) = 1 + NDIM
        TEMPE(5) = 1 + NDIM
        NT = 1
      ELSE
        TEMPE(4) = 0
        TEMPE(5) = 0
        NT = 0
      END IF
C NOMBRE DE DEGRES DE LIBERTE DE CHAQUE NOEUD

      NDDL = NMEC + NP1 + NP2 + NT

C ADRESSE DES DEFORMATIONS ET DES CONTRAINTES

      IF (MECANI(1).EQ.1) THEN
        MECANI(2) = 1
        MECANI(3) = 1
      ELSE
        MECANI(2) = 0
        MECANI(3) = 0
      END IF

      IF (PRESS1(1).EQ.1) THEN
        PRESS1(3) = MECANI(4) + 1
        PRESS1(4) = MECANI(5) + 1
        IF (PRESS1(2).EQ.2) PRESS1(5) = PRESS1(4) + PRESS1(7)
      END IF

      IF (PRESS2(1).EQ.1) THEN
        PRESS2(3) = PRESS1(3) + PRESS1(6)
        PRESS2(4) = PRESS1(4) + PRESS1(2)*PRESS1(7)
        IF (PRESS2(2).EQ.2) PRESS2(5) = PRESS2(4) + PRESS2(7)
      END IF

      IF (TEMPE(1).EQ.1) THEN
        TEMPE(2) = MECANI(4) + PRESS1(6) + PRESS2(6) + 1
        TEMPE(3) = MECANI(5) + PRESS1(2)*PRESS1(7) +
     &             PRESS2(2)*PRESS2(7) + 1
      END IF

C AUTRES GRANDEURS A METTRE DANS ASSTHM

      NDDL = MECANI(1)*NDIM + PRESS1(1) + PRESS2(1) + TEMPE(1)
      DIMDEF = MECANI(4) + PRESS1(6) + PRESS2(6) + TEMPE(4)
      DIMCON = MECANI(5) + PRESS1(2)*PRESS1(7) + PRESS2(2)*PRESS2(7) +
     &         TEMPE(5)
C
C***********************************************************************
C  DEBUT DES DIFFERENTES OPTIONS


C***********************************************************************

C  OPTIONS : RIGI_MECA_TANG , FULL_MECA , RAPH_MECA


      IF (((OPTION.EQ.'RIGI_MECA_TANG').OR.
     &    (OPTION.EQ.'RAPH_MECA')) .OR. (OPTION.EQ.'FULL_MECA')) THEN

C - PARAMETRES EN ENTREE

        CALL JEVECH('PGEOMER','L',IGEOM)
        CALL JEVECH('PMATERC','L',IMATE)
        CALL JEVECH('PINSTMR','L',IINSTM)
        CALL JEVECH('PINSTPR','L',IINSTP)
        CALL JEVECH('PDEPLMR','L',IDEPLM)
        CALL JEVECH('PDEPLPR','L',IDEPLP)
        CALL JEVECH('PCOMPOR','L',ICOMPO)
        CALL JEVECH('PCARCRI','L',ICARCR)
        CALL JEVECH('PVARIMR','L',IVARIM)
        CALL JEVECH('PCONTMR','L',ICONTM)
        CALL JEVECH('PTEREF','L',ITREF)

        READ (ZK16(ICOMPO-1+2),'(I16)') NBVARI

C - PARAMETRES EN SORTIE

        IF (OPTION(1:16).EQ.'RIGI_MECA_TANG' .OR.
     &      OPTION(1:9).EQ.'FULL_MECA') THEN
          CALL JEVECH('PMATUNS','E',IMATUU)
        ELSE
          IMATUU = ISMAEM()
        END IF

        IF (OPTION(1:9).EQ.'RAPH_MECA' .OR.
     &      OPTION(1:9).EQ.'FULL_MECA') THEN
          CALL JEVECH('PVECTUR','E',IVECTU)
        ELSE
          IVECTU = ISMAEM()
        END IF

        IF (OPTION(1:9).EQ.'RAPH_MECA' .OR.
     &      OPTION(1:9).EQ.'FULL_MECA') THEN
          CALL JEVECH('PCONTPR','E',ICONTP)
          CALL JEVECH('PVARIPR','E',IVARIP)
        ELSE
          ICONTP = ISMAEM()
          IVARIP = ISMAEM()
        END IF

        IF (OPTION(1:9).EQ.'RAPH_MECA' .OR.
     &      OPTION(1:9).EQ.'FULL_MECA') THEN
          CALL JEVECH('PCODRET','E',JCRET)
          ZI(JCRET) = 0
        END IF
        RETLOI = 0

        IF (OPTION(1:16).EQ.'RIGI_MECA_TANG') THEN
          CALL ASSTHM(NNO,NNOS,NPGU,IPOIDS,IVF,IDFDE,
     &                ZR(IGEOM),NOMTE,ZR(ICARCR),
     &                ZR(ITREF),ZR(IDEPLM),ZR(IDEPLM),ZR(ICONTM),
     &                ZR(ICONTM),ZR(IVARIM),ZR(IVARIM),DEFGEM,DEFGEP,
     &                DRDS,DRDE,DSDE,B,DFDI,R,ZR(IMATUU),ZR(IVECTU),
     &                ZR(IINSTM),ZR(IINSTP),OPTION,ZI(IMATE),MECANI,
     &                PRESS1,PRESS2,TEMPE,DIMDEF,DIMCON,NBVARI,NDDL,
     &                NMEC,NP1,NP2,NDIM,ZK16(ICOMPO),TYPMOD,AXI,NVOMAX,
     &                NNOMAX,NSOMAX,NBVOS,VOISIN,P2P1,RETLOI)
        ELSE
          DO 30 LI = 1,NDDL*NNO
            ZR(IDEPLP+LI-1) = ZR(IDEPLM+LI-1) + ZR(IDEPLP+LI-1)

   30     CONTINUE

          CALL ASSTHM(NNO,NNOS,NPGU,IPOIDS,IVF,IDFDE,
     &                ZR(IGEOM),NOMTE,ZR(ICARCR),
     &                ZR(ITREF),ZR(IDEPLM),ZR(IDEPLP),ZR(ICONTM),
     &                ZR(ICONTP),ZR(IVARIM),ZR(IVARIP),DEFGEM,DEFGEP,
     &                DRDS,DRDE,DSDE,B,DFDI,R,ZR(IMATUU),ZR(IVECTU),
     &                ZR(IINSTM),ZR(IINSTP),OPTION,ZI(IMATE),MECANI,
     &                PRESS1,PRESS2,TEMPE,DIMDEF,DIMCON,NBVARI,NDDL,
     &                NMEC,NP1,NP2,NDIM,ZK16(ICOMPO),TYPMOD,AXI,NVOMAX,
     &                NNOMAX,NSOMAX,NBVOS,VOISIN,P2P1,RETLOI)
          IF (OPTION(1:9).EQ.'RAPH_MECA' .OR.
     &        OPTION(1:9).EQ.'FULL_MECA') THEN
            ZI(JCRET) = RETLOI
          END IF


        END IF
      END IF

C***********************************************************************
C***********************************************************************

C   OPTION : 'CHAR_MECA_PESA_R '

C***********************************************************************
      IF (OPTION.EQ.'CHAR_MECA_PESA_R') THEN

        CALL JEVECH('PGEOMER','L',IGEOM)
        CALL JEVECH('PMATERC','L',IMATE)
        CALL JEVECH('PPESANR','L',IPESA)
        CALL JEVECH('PVECTUR','E',IVECTU)

        CALL RCCOMA(ZI(IMATE),'THM_DIFFU',PHENOM,CODRET)
        CALL RCVALA(ZI(IMATE),PHENOM,1,' ',R8BID,1,'RHO',RHO,CODRET,
     &              'FM')
        IF (NDIM.EQ.3) THEN
C  CAS 3D
          DO 40 I = 1,NDDL*NNO
            ZR(IVECTU+I-1) = 0.0D0
   40     CONTINUE

C    BOUCLE SUR LES POINTS DE GAUSS

          DO 70 KP = 1,NPGU
            L = (KP-1)*NNO
            CALL DFDM3D ( NNO, KP, IPOIDS, IDFDE,
     &                    ZR(IGEOM), DFDX, DFDY, DFDZ, POIDS )
            COEF = RHO*POIDS*ZR(IPESA)
            DO 60 I = 1,NNO
              II = NDDL* (I-1)
              DO 50 J = 1,3
                ZR(IVECTU+II+J-1) = ZR(IVECTU+II+J-1) +
     &                           COEF*ZR(IVF+L+I-1)*ZR(IPESA+J)
   50         CONTINUE
   60       CONTINUE
   70     CONTINUE
        ELSE
C  CAS 2D
          DO 110 KP = 1,NPGU
            K = (KP-1)*NNO
            CALL DFDM2D(NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,
     &                  POIDS)
            POIDS = POIDS*RHO*ZR(IPESA)
            IF (TYPMOD(1).EQ.'AXIS    ') THEN
              RX = 0.D0
              DO 80 I = 1,NNO
                RX = RX + ZR(IGEOM+2*I-2)*ZR(IVF+K+I-1)
   80         CONTINUE
              POIDS = POIDS*RX
              DO 90 I = 1,NNO
                K = (KP-1)*NNO
                ZR(IVECTU+NDDL* (I-1)+1) = ZR(IVECTU+NDDL* (I-1)+1) +
     &                                     POIDS*ZR(IPESA+2)*
     &                                     ZR(IVF+K+I-1)
   90         CONTINUE
            ELSE
              DO 100 I = 1,NNO
                K = (KP-1)*NNO
                ZR(IVECTU+NDDL* (I-1)) = ZR(IVECTU+NDDL* (I-1)) +
     &                                   POIDS*ZR(IPESA+1)*ZR(IVF+K+I-1)
                ZR(IVECTU+NDDL* (I-1)+1) = ZR(IVECTU+NDDL* (I-1)+1) +
     &                                     POIDS*ZR(IPESA+2)*
     &                                     ZR(IVF+K+I-1)
  100         CONTINUE
            END IF
  110     CONTINUE
        END IF


      END IF

C***********************************************************************
C***********************************************************************
C  OPTION : CHAR_MECA_FR3D3D
C***********************************************************************

      IF (OPTION.EQ.'CHAR_MECA_FR3D3D') THEN
        CALL JEVECH('PGEOMER','L',IGEOM)
        CALL JEVECH('PVECTUR','E',IVECTU)
        CALL JEVECH('PFR3D3D','L',IFORC)

        DO 120 I = 1,NDDL*NNO
          ZR(IVECTU+I-1) = 0.0D0
  120   CONTINUE

C    BOUCLE SUR LES POINTS DE GAUSS

        DO 150 KP = 1,NPGU

          L = (KP-1)*NNO
          CALL DFDM3D ( NNO, KP, IPOIDS, IDFDE,
     &                  ZR(IGEOM), DFDX, DFDY, DFDZ, POIDS )

          DO 140 I = 1,NNO
            II = NDDL* (I-1)
            DO 130 J = 1,3
              ZR(IVECTU+II+J-1) = ZR(IVECTU+II+J-1) +
     &                            POIDS*ZR(IVF+L+I-1)*ZR(IFORC+J-1)
  130       CONTINUE
  140     CONTINUE
  150   CONTINUE

      END IF

C***********************************************************************
C  OPTION : CHAR_MECA_FR2D2D
C***********************************************************************

      IF (OPTION.EQ.'CHAR_MECA_FR2D2D') THEN

        CALL JEVECH('PGEOMER','L',IGEOM)
        CALL JEVECH('PFR2D2D','L',IFORC)
        CALL JEVECH('PVECTUR','E',IVECTU)

        DO 180 KP = 1,NPGU
          K = (KP-1)*NNO
          CALL DFDM2D(NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,
     &                POIDS)
          IF (TYPMOD(1).EQ.'AXIS    ') THEN
            RX = 0.D0
            DO 160 I = 1,NNO
              RX = RX + ZR(IGEOM+2* (I-1))*ZR(IVF+K+I-1)
  160       CONTINUE
            POIDS = POIDS*RX
          END IF
          DO 170 I = 1,NNO
            K = (KP-1)*NNO
            L = (KP-1)*2
            ZR(IVECTU+NDDL* (I-1)) = ZR(IVECTU+NDDL* (I-1)) +
     &                               POIDS*ZR(IFORC+L)*ZR(IVF+K+I-1)
            ZR(IVECTU+NDDL* (I-1)+1) = ZR(IVECTU+NDDL* (I-1)+1) +
     &                                 POIDS*ZR(IFORC+L+1)*ZR(IVF+K+I-1)
  170     CONTINUE
  180   CONTINUE
      END IF

C***********************************************************************
C  OPTION : FORC_NODA
C***********************************************************************


      IF (OPTION.EQ.'FORC_NODA') THEN


C - PARAMETRES EN ENTREE
        CALL JEVECH('PGEOMER','L',IGEOM)
        CALL JEVECH('PCONTMR','L',ICONTM)
        CALL JEVECH('PMATERC','L',IMATE)

C  SI LES TEMPS PLUS ET MOINS SONT PRESENTS
C  C EST QUE L ON APPELLE DEPUIS STAT NON LINE ET
C  ALORS LES TERMES DEPENDANT DE DT SONT EVALUES

        CALL TECACH('ONN','PINSTMR ',1,IINSTM,IRET)
        CALL TECACH('ONN','PINSTPR ',1,IINSTP,IRET)
        IF (IINSTM.GT.0 .AND. IINSTP.GT.0) THEN
          DT = ZR(IINSTP) - ZR(IINSTM)
          FNOEVO = .TRUE.
        ELSE
          FNOEVO = .FALSE.
          DT = 0.D0
        END IF


C - PARAMETRES EN SORTIE
        CALL JEVECH('PVECTUR','E',IVECTU)


        CALL FNOTHM(FNOEVO,DT,NNO,NNOS,NPGU,IPOIDS,IVF,IDFDE,
     &              ZR(IGEOM),ZR(ICONTM),
     &              B,DFDI,R,ZR(IVECTU),ZI(IMATE),MECANI,PRESS1,PRESS2,
     &              TEMPE,DIMDEF,DIMCON,NDDL,NMEC,NP1,NP2,NDIM,
     &              AXI,NVOMAX,NNOMAX,NSOMAX,NBVOS,VOISIN,P2P1)
      END IF

C ======================================================================
C --- OPTION : REFE_FORC_NODA ------------------------------------------
C ======================================================================
      IF (OPTION.EQ.'REFE_FORC_NODA') THEN
C ======================================================================
C --- PARAMETRES EN ENTREE ---------------------------------------------
C ======================================================================
        CALL JEVECH('PGEOMER','L',IGEOM)
        CALL JEVECH('PMATERC','L',IMATE)
        CALL JEVECH('PREFCO','L',ICONTM)
C ======================================================================
C --- ON RAPPELLE QUE LES PARAMETRES DU CRITERE DE CONVERGENCE SONT ----
C --- STOCKES DE LA FACON SUIVANTE : (1) : SIGM_REFE -------------------
C ---------------------------------- (3) : FLUX_THER_REFE --------------
C ---------------------------------- (4) : FLUX_HYD1_REFE --------------
C ---------------------------------- (5) : FLUX_HYD2_REFE --------------
C ======================================================================
        DT = 1.0D0
        FNOEVO = .TRUE.
C ======================================================================
C --- PARAMETRES EN SORTIE ---------------------------------------------
C ======================================================================
        CALL JEVECH('PVECTUR','E',IVECTU)
C ======================================================================
C --- CREATION DES VECTEURS DE TRAVAIL ---------------------------------
C ======================================================================
        CALL WKVECT ('&&_TE0600.SIGTMP','V V R',DIMCON*NPGU,JSIGTM)
        CALL WKVECT ('&&_TE0600.FTEMP','V V R',NDDL*NNO,JFTEMP)
        CALL WKVECT ('&&_TE0600.BSIGM','V V R',NDDL*NNO,JBSIGM)
C ======================================================================
C --- INITIALISATIONS --------------------------------------------------
C ======================================================================
        CALL R8INIR(DIMCON*NPGU,0.D0,ZR(JSIGTM),1)
        CALL R8INIR(NDDL*NNO,0.D0,ZR(JFTEMP),1)
C ======================================================================
C --- TESTS DE COHERENCE -----------------------------------------------
C ======================================================================
        IF ( MECANI(1).NE.0 ) THEN
           INDICE = 1
           IF ( ZR( ICONTM-1+INDICE ).EQ.R8VIDE() ) THEN
              CALL UTMESS('F','TE0600','IL MANQUE SIGM_REFE')
           ENDIF
        ENDIF
        IF ( PRESS1(1).NE.0 ) THEN
           INDICE = 2
           IF ( ZR( ICONTM-1+INDICE ).EQ.R8VIDE() ) THEN
              CALL UTMESS('F','TE0600','IL MANQUE RESI_HYD1_REFE')
           ENDIF
        ENDIF
        IF ( PRESS2(1).NE.0 ) THEN
           INDICE = 3
           IF ( ZR( ICONTM-1+INDICE ).EQ.R8VIDE() ) THEN
              CALL UTMESS('F','TE0600','IL MANQUE RESI_HYD2_REFE')
           ENDIF
        ENDIF
        IF ( TEMPE(1).NE.0 ) THEN
           INDICE = 4
           IF ( ZR( ICONTM-1+INDICE ).EQ.R8VIDE() ) THEN
              CALL UTMESS('F','TE0600','IL MANQUE RESI_THER_REFE')
           ENDIF
        ENDIF
C ======================================================================
C --- TESTS DE COHERENCE -----------------------------------------------
C ======================================================================
        DO 200 I = 1, NPGU
           DO 210 J = 1, DIMCON
              IF ( J.LE.MECANI(5) ) THEN
                 INDICE = 1
              ELSE IF ( J.LE.
     +                  (MECANI(5)+PRESS1(2)*PRESS1(7)) ) THEN
                 INDICE = 2
                 IF ( TEMPE(5).GT.0 ) THEN
C ======================================================================
C --- ON NE FAIT RIEN DANS LE CAS DE L'ENTHALPIE -----------------------
C ======================================================================
                    IF ( J.EQ.(MECANI(5)+PRESS1(7)) .OR.
     +                   J.EQ.(MECANI(5)+PRESS1(2)*PRESS1(7)) ) THEN
                       GO TO 210
                    ENDIF
                 ENDIF
              ELSE IF ( J.LE.
     +        (MECANI(5)+PRESS1(2)*PRESS1(7)+PRESS2(2)*PRESS2(7)) ) THEN
                 INDICE = 3
                 IF ( TEMPE(5).GT.0 ) THEN
C ======================================================================
C --- ON NE FAIT RIEN DANS LE CAS DE L'ENTHALPIE -----------------------
C ======================================================================
                    IF (
     +             J.EQ.(MECANI(5)+
     +                  PRESS1(2)*PRESS1(7)+PRESS2(7)) .OR.
     +             J.EQ.(MECANI(5)+
     +                  PRESS1(2)*PRESS1(7)+PRESS2(2)*PRESS2(7)) ) THEN
                       GO TO 210
                    ENDIF                 
                 ENDIF
              ELSE IF ( J.LE.(MECANI(5)+TEMPE(5)) ) THEN
                 INDICE = 4
              ENDIF

              IF ( ZR( ICONTM-1+INDICE ).EQ.R8VIDE() ) GOTO 210

              ZR( JSIGTM-1+J+DIMCON*(I-1) ) = ZR( ICONTM-1+INDICE )
              CALL FNOTHM(FNOEVO,DT,NNO,NNOS,NPGU,IPOIDS,IVF,IDFDE,
     &                    ZR(IGEOM),ZR(JSIGTM),
     &              B,DFDI,R,ZR(JBSIGM),ZI(IMATE),MECANI,PRESS1,PRESS2,
     &              TEMPE,DIMDEF,DIMCON,NDDL,NMEC,NP1,NP2,NDIM,
     &              AXI,NVOMAX,NNOMAX,NSOMAX,NBVOS,VOISIN,P2P1)
     
              DO 220 K = 1,NDDL*NNOS
                 ZR(JFTEMP-1+K) = ZR(JFTEMP-1+K) + ABS(ZR(JBSIGM-1+K))
 220          CONTINUE
 
              DO 221 K = NNOS,NNO-1
                 DO 222 L = 1,NMEC
                    ZR(JFTEMP-1+K*NDDL+L) = ZR(JFTEMP-1+K*NDDL+L) + 
     &                     ABS(ZR(JBSIGM-1+K*NDDL+L))
 222            CONTINUE
C POUR TENIR COMPTE DU P2P1 (ON EST SUR LES NOEUDS MILIEUX P1) 
                IF (P2P1) THEN
                   DO 223 L = NMEC+1,NDDL
                      ZR(JFTEMP-1+K*NDDL+L) =
     +                                   ZR(JFTEMP-1+K*NDDL+L) + 1.D0
 223               CONTINUE
                 ELSE
                    DO 224 L = NMEC+1,NDDL
                      ZR(JFTEMP-1+K*NDDL+L) =ZR(JFTEMP-1+K*NDDL+L) + 
     &                     ABS(ZR(JBSIGM-1+K*NDDL+L))
 224               CONTINUE
                 ENDIF
 221          CONTINUE

              ZR( JSIGTM-1+J+DIMCON*(I-1) ) = 0.0D0

 210       CONTINUE
 200    CONTINUE
 
        CALL R8AXPY(NDDL*NNO,1.D0/NPGU,ZR(JFTEMP),1,ZR(IVECTU),1)

        DO 230 K = 1,NDDL*NNO
           IF ( ABS(ZR(IVECTU-1+K)).LT.R8MIEM() ) THEN
              CALL UTMESS('F','TE0600','VECTEUR NUL ENTRAINANT '//
     +                           'UNE DIVISION PAR ZERO DANS NMCONV')
           ENDIF
 230    CONTINUE
        CALL JEDETR('&&_TE0600.SIGTMP')
        CALL JEDETR('&&_TE0600.FTEMP')
        CALL JEDETR('&&_TE0600.BSIGM')
      END IF

C***********************************************************************

C  OPTION : SIEF_ELNO_ELGA ET VARI_ELNO_ELGA

C***********************************************************************


      IF ((OPTION.EQ.'SIEF_ELNO_ELGA') .OR.
     &    (OPTION.EQ.'VARI_ELNO_ELGA')) THEN
        NCMP = DIMCON

        IF (OPTION.EQ.'SIEF_ELNO_ELGA  ') THEN
          CALL JEVECH('PCONTRR','L',ICHG)
          CALL JEVECH('PSIEFNOR','E',ICHN)
        END IF
        IF (OPTION.EQ.'VARI_ELNO_ELGA  ') THEN
          CALL JEVECH('PVARIGR','L',ICHG)
          CALL JEVECH('PCOMPOR','L',ICOMPO)
          CALL JEVECH('PVARINR','E',ICHN)
          READ (ZK16(ICOMPO+1),'(I16)') NCMP
          CALL TECACH('OON','PVARIGR',7,JTAB,IRET)
        END IF

        CALL PPGAN2(JGANO,NCMP,ZR(ICHG),ZR(ICHN))
      END IF

      END
