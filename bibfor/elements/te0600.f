      SUBROUTINE TE0600(OPTION,NOMTE)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 08/10/2002   AUTEUR UFBHHLL C.CHAVANT 
C =====================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C =====================================================================
C TOLE CRP_20
C TOLE CRP_21
      IMPLICIT NONE
      CHARACTER*16 OPTION,NOMTE,PHENOM,ELREFL

C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES OPTIONS NON-LINEAIRES MECANIQUES
C                          ELEMENTS THHM  ET HM
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
      CHARACTER*24 CARAC,FF,TRAV,CHMAT
      INTEGER NNO,IMATUU,NDIM,IFAM,IMATE,IINSTM,IFORC,JCRET
      INTEGER RETLOI
      INTEGER ICARAC,IFF,IPOIDS,IVF,IDFDE,IDFDN,IDFDK,IGEOM
      INTEGER ITRAV,IINSTP,IDEPLM,IDEPLP,ICOMPO,ICARCR,IPESA
      INTEGER ICONTM,IVARIP,IVARIM,ITREF,IVECTU,ICONTP,ITEMPE
      CHARACTER*8 ALIAS,MATER,ELREFE
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

      INTEGER MECANI(5),PRESS1(7),PRESS2(7),TEMPE(5)
      INTEGER DIMDEF,DIMCON,NBVARI,NDDL
      INTEGER NMEC,NP1,NP2,NT,I,NPGU,NCMP,NNOS,LGPG,ICHG,ICHN
      INTEGER IC,JMAT,JTAB(7)
      REAL*8 DEFGEP(21),DEFGEM(21)
      REAL*8 DFDX(27),DFDY(27),DFDZ(27),TPG,POIDS
      REAL*8 DFDI(20,3),B(21,120)
      REAL*8 DRDE(21,21),DRDS(21,31),DSDE(31,21),R(21)
      CHARACTER*8 TYPMOD(2)
C  VARIABLES INTERMEDIAIRES
      REAL*8 DEPL(6,20),TREF,R8BID,RHO,COEF,XI,XIJ,S
      INTEGER NBFMAX
      PARAMETER (NBFMAX=10)
      INTEGER ISMAEM,LI,KP,JJ,J,NBPG(NBFMAX),JIN,NBFPG,JVAL,L,K
      INTEGER NDIMLU
      REAL*8 RX
      CHARACTER*2 CODRET
      CHARACTER*16 COMP
      INTEGER IADBID
      LOGICAL AXI,DPLAN,TRAITE
      LOGICAL VOL2,BORD2,VOL3,BORD3
      LOGICAL ISTHT3
      LOGICAL ISTHQ4
      LOGICAL ISTHT6
      LOGICAL ISTHQ8
      LOGICAL ISTHS2
      LOGICAL ISTHS3
      LOGICAL ISTHF8,ISTHF6
C
      LOGICAL ISTH10,ISTH13,ISTH15,ISTH20
      INTEGER NSOM
C
      INTEGER NNOMAX,NVOMAX,NSOMAX
      PARAMETER(NNOMAX=20,NVOMAX=4,NSOMAX=8)
      INTEGER VOISIN(NVOMAX,NNOMAX)
      INTEGER NBVOS(NSOMAX)
C
      LOGICAL P2P1,LUMPED
      INTEGER II,NPGPRE

      LOGICAL THLU
      INTEGER FAMLUM

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
C
C   POUR L OPTION FORCNODA
C  SI LES TEMPS PLUS ET MOINS SONT PRESENTS
C  C EST QUE L ON APPELLE DEPUIS STAT NON LINE  : FNOEVO = VRAI
C  ET ALORS LES TERMES DEPENDANT DE DT SONT EVALUES
C
C  SI LES TEMPS PLUS ET MOINS NE SONT PAS PRESENTS
C  C EST QUE L ON APPELLE DEPUIS CALCNO  : FNOEVO = FAUX
C  ET ALORS LES TERMES DEPENDANT DE DT SONT PAS EVALUES
C
      LOGICAL FNOEVO
      REAL*8  DT
C **********************************************************************

C  SUIVANT ELEMENT, DEFINITION DE CARACTERISTIQUES

      CALL ELREF1(ELREFE)
      TYPMOD(2) = '        '
      NDIM = 0
C
C  ON APPELLE CAETHM AVEC POUR LES ELEMENTS VOLUMIQUES
C  2D EN METTANT VOL2 SACHANT QUE ALORS POUR LES ELEMENTS
C  3D IL NE FERA RIEN CE DONT ON S APERCEVRA PAR L ARGUMENT
C     TRAITE
C
C
      BORD2 = .FALSE.
      BORD3 = .FALSE.
      VOL2  = .TRUE.
      VOL3  = .TRUE.
C
      CALL CAETHM(NOMTE,VOL2,BORD2,VOL3,BORD3,
     > ALIAS,AXI,DPLAN,TRAITE,
     > ISTHS2,ISTHS3,ISTHF8,ISTHF6,ISTH10,ISTH13,ISTH15,ISTH20,
     > ISTHT3,ISTHQ4,ISTHT6,ISTHQ8,
     > NNOMAX,NVOMAX,NSOMAX,
     > NSOM,VOISIN,NBVOS,P2P1,LUMPED)
C
C
C L APPEL SUIVANT SER A RETOURNER FAMLUM
C LA FAMILLE DE GAUSS UTILISEE POUR LES LUMPES
C
      CALL ALTHLU(ELREFE,THLU,FAMLUM)
C
       IF(TRAITE) THEN
        IF ( AXI) THEN
          NDIM = 2
          TYPMOD(1) = 'AXIS    '
        ELSE IF ( DPLAN) THEN
          NDIM = 2
          TYPMOD(1) = 'D_PLAN  '
        ELSE
          NDIM = 3
          TYPMOD(1) = '3D      '
        ENDIF
       ELSE
        CALL UTMESS('F','TE0600','ELEM INCONNU DE CAETHM')
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
C
      IF (NOMTE(1:3).EQ.'HHM') THEN
        MECANI(1) = 1
        PRESS1(1) = 1
        PRESS2(1) = 1
        TEMPE(1) = 0
        PRESS1(2) = 2
        PRESS2(2) = 1
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
C
C
      IF (NDIM.EQ.3) THEN
        CARAC = '&INEL.'//ELREFE//'.CARACTE'
        FF = '&INEL.'//ELREFE//'.FFORMES'
      ELSE
        CARAC = '&INEL.'//ELREFE//'.CARAC '
        FF = '&INEL.'//ELREFE//'.FF'
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
     +             PRESS2(2)*PRESS2(7) + 1
      END IF

C AUTRES GRANDEURS A METTRE DANS ASSTHM

      NDDL = MECANI(1)*NDIM + PRESS1(1) + PRESS2(1) + TEMPE(1)
      DIMDEF = MECANI(4) + PRESS1(6) + PRESS2(6) + TEMPE(4)
      DIMCON = MECANI(5) + PRESS1(2)*PRESS1(7) + PRESS2(2)*PRESS2(7) +
     +         TEMPE(5)

C
C - FONCTIONS DE FORMES, POINTS DE GAUSS
C  ELREFE DEFINI PRECEDEMMENT
C
        CALL JEVETE(CARAC,'L',ICARAC)
C
        CALL JEVETE(FF,'L',IFF)
C
        IF (NDIM.EQ.3) THEN
C
C  CAS 3D
C
          NDIMLU= ZI(ICARAC)
          IF ( NDIMLU.NE.NDIM) THEN
            CALL UTMESS('F','TE0600','NDIMLU DIFF NDIM')
          ENDIF
          NBFPG = ZI(ICARAC+3-1)
          IF ( NBFPG.GT.NBFMAX) THEN
            CALL UTMESS('F','TE0600','TROP DE FAMILLES DE GAUSS')
          ENDIF
          NNO = ZI(ICARAC+2-1)
          DO 210 I = 1,NBFPG
            NBPG(I) = ZI(ICARAC+3-1+I)
  210     CONTINUE
           NNOS = ZI(ICARAC+3-1+NBFPG+1)
           IF ( LUMPED) THEN
            NPGU=NNOS
            IFAM = FAMLUM
           ELSE
            NPGU= NBPG(1)
            IFAM = 1
            IF (ELREFE.EQ.'PENTA15') THEN
              NPGU = NBPG(2)
              IFAM = 2
            END IF
           ENDIF
C
           CALL ADFF3D(NDIM,NNO,NBFPG,NBPG,IFAM,
     >     IPOIDS,IVF,IDFDE,IDFDN,IDFDK)
C
           IPOIDS = IFF + IPOIDS
           IVF    = IFF + IVF
           IDFDE  = IFF + IDFDE
           IDFDN  = IFF + IDFDN
           IDFDK  = IFF + IDFDK
        ELSE
C
C  CAS 2D
C
          NBFPG = ZI(ICARAC+2-1)
          IF ( NBFPG.GT.NBFMAX) THEN
            CALL UTMESS('F','TE0600','TROP DE FAMILLES DE GAUSS')
          ENDIF
          NNO  = ZI(ICARAC)
          IF ( LUMPED) THEN
            IFAM = FAMLUM
          ELSE
            IFAM = 2
          ENDIF
          DO 211 I = 1,NBFPG
            NBPG(I) = ZI(ICARAC+2-1+I)
  211     CONTINUE
          IF ( ISTHT3.OR.ISTHT6) THEN
           NNOS = 3
          ELSE IF ( ISTHQ4.OR.ISTHQ8) THEN
           NNOS = 4
          ELSE
            CALL UTMESS('F','TE0600','NI TR NI QUAD')
          ENDIF
          NPGU = NBPG(IFAM)
          IPOIDS=IFF
          IF (IFAM.GT.1) THEN
            NPGPRE=0
            DO 212 II = 1,IFAM-1
             NPGPRE=NPGPRE+NBPG(II)
  212       CONTINUE
            IPOIDS=IPOIDS + NPGPRE*(1+3*NNO)
          ENDIF
          IVF   =IPOIDS+NPGU
          IDFDE =IVF   +NPGU*NNO
          IDFDK =IDFDE +NPGU*NNO
        ENDIF
C
C
C***********************************************************************
C  DEBUT DES DIFFERENTES OPTIONS


C***********************************************************************

C  OPTIONS : RIGI_MECA_TANG , FULL_MECA , RAPH_MECA


      IF (((OPTION.EQ.'RIGI_MECA_TANG').OR.
     +    (OPTION.EQ.'RAPH_MECA')) .OR. (OPTION.EQ.'FULL_MECA')) THEN

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
     +      OPTION(1:9).EQ.'FULL_MECA') THEN
          CALL JEVECH('PMATUNS','E',IMATUU)
        ELSE
          IMATUU = ISMAEM()
        END IF

        IF (OPTION(1:9).EQ.'RAPH_MECA' .OR.
     +      OPTION(1:9).EQ.'FULL_MECA') THEN
          CALL JEVECH('PVECTUR','E',IVECTU)
        ELSE
          IVECTU = ISMAEM()
        END IF

        IF (OPTION(1:9).EQ.'RAPH_MECA' .OR.
     +      OPTION(1:9).EQ.'FULL_MECA') THEN
          CALL JEVECH('PCONTPR','E',ICONTP)
          CALL JEVECH('PVARIPR','E',IVARIP)
        ELSE
          ICONTP = ISMAEM()
          IVARIP = ISMAEM()
        END IF
C
        IF ( OPTION(1:9) .EQ. 'RAPH_MECA'  .OR.
     &       OPTION(1:9) .EQ. 'FULL_MECA'  ) THEN
           CALL JEVECH ( 'PCODRET', 'E', JCRET )
           ZI(JCRET) = 0
        ENDIF
        RETLOI = 0
C
        IF (OPTION(1:16).EQ.'RIGI_MECA_TANG') THEN
        CALL ASSTHM(NNO,NNOS,NPGU,
     >                ZR(IPOIDS),ZR(IVF),ZR(IDFDE),ZR(IDFDN),
     +                ZR(IDFDK),ZR(IGEOM),NOMTE,ZR(ICARCR),ZR(ITREF),
     +                ZR(IDEPLM),ZR(IDEPLM),ZR(ICONTM),ZR(ICONTM),
     +                ZR(IVARIM),ZR(IVARIM),DEFGEM,DEFGEP,DRDS,DRDE,
     +                DSDE,B,DFDI,R,ZR(IMATUU),ZR(IVECTU),ZR(IINSTM),
     +                ZR(IINSTP),OPTION,ZI(IMATE),MECANI,PRESS1,PRESS2,
     +                TEMPE,DIMDEF,DIMCON,NBVARI,NDDL,NMEC,NP1,NP2,
     +                NDIM,ZK16(ICOMPO),TYPMOD,AXI,
     >                NVOMAX,NNOMAX,NSOMAX,NBVOS,VOISIN,P2P1,RETLOI)

        ELSE
          DO 10 LI = 1,NDDL*NNO
            ZR(IDEPLP+LI-1) = ZR(IDEPLM+LI-1) + ZR(IDEPLP+LI-1)

   10     CONTINUE

          CALL ASSTHM(NNO,NNOS,NPGU,
     >                ZR(IPOIDS),ZR(IVF),ZR(IDFDE),ZR(IDFDN),
     +                ZR(IDFDK),ZR(IGEOM),NOMTE,ZR(ICARCR),ZR(ITREF),
     +                ZR(IDEPLM),ZR(IDEPLP),ZR(ICONTM),ZR(ICONTP),
     +                ZR(IVARIM),ZR(IVARIP),DEFGEM,DEFGEP,DRDS,DRDE,
     +                DSDE,B,DFDI,R,ZR(IMATUU),ZR(IVECTU),ZR(IINSTM),
     +                ZR(IINSTP),OPTION,ZI(IMATE),MECANI,PRESS1,PRESS2,
     +                TEMPE,DIMDEF,DIMCON,NBVARI,NDDL,NMEC,NP1,NP2,
     +                NDIM,ZK16(ICOMPO),TYPMOD,AXI,
     >                NVOMAX,NNOMAX,NSOMAX,NBVOS,VOISIN,P2P1,RETLOI)
          IF ( OPTION(1:9) .EQ. 'RAPH_MECA'  .OR.
     &       OPTION(1:9) .EQ. 'FULL_MECA'  ) THEN
           ZI(JCRET) = RETLOI
        ENDIF
C

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
     +              'FM')
        IF (NDIM.EQ.3) THEN
C  CAS 3D
          DO 30 I = 1,NDDL*NNO
            ZR(IVECTU+I-1) = 0.0D0
   30     CONTINUE

C    BOUCLE SUR LES POINTS DE GAUSS

          DO 60 KP = 1,NPGU

            L = (KP-1)*NNO
            K = (KP-1)*NNO*3
            CALL DFDM3D(NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDN+K),
     +                  ZR(IDFDK+K),ZR(IGEOM),DFDX,DFDY,DFDZ,POIDS)

            COEF = RHO*POIDS*ZR(IPESA)

            DO 50 I = 1,NNO
              II = NDDL* (I-1)

              DO 40 J = 1,3
                ZR(IVECTU+II+J-1) = ZR(IVECTU+II+J-1) +
     +                              COEF*ZR(IVF+L+I-1)*ZR(IPESA+J)
   40         CONTINUE

   50       CONTINUE

   60     CONTINUE
        ELSE
C  CAS 2D
          DO 100 KP = 1,NPGU
            K = (KP-1)*NNO
            CALL DFDM2D(NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDK+K),
     +                  ZR(IGEOM),DFDX,DFDY,POIDS)
            POIDS = POIDS*RHO*ZR(IPESA)
            IF (TYPMOD(1).EQ.'AXIS    ') THEN
              RX = 0.D0
              DO 70 I = 1,NNO
                RX = RX + ZR(IGEOM+2*I-2)*ZR(IVF+K+I-1)
   70         CONTINUE
              POIDS = POIDS*RX
              DO 80 I = 1,NNO
                K = (KP-1)*NNO
                ZR(IVECTU+NDDL* (I-1)+1) = ZR(IVECTU+NDDL* (I-1)+1) +
     +                                     POIDS*ZR(IPESA+2)*
     +                                     ZR(IVF+K+I-1)
   80         CONTINUE
            ELSE
              DO 90 I = 1,NNO
                K = (KP-1)*NNO
                ZR(IVECTU+NDDL* (I-1)) = ZR(IVECTU+NDDL* (I-1)) +
     +                                   POIDS*ZR(IPESA+1)*ZR(IVF+K+I-1)
                ZR(IVECTU+NDDL* (I-1)+1) = ZR(IVECTU+NDDL* (I-1)+1) +
     +                                     POIDS*ZR(IPESA+2)*
     +                                     ZR(IVF+K+I-1)
   90         CONTINUE
            END IF
  100     CONTINUE
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
          K = (KP-1)*NNO*3
          CALL DFDM3D(NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDN+K),
     +                ZR(IDFDK+K),ZR(IGEOM),DFDX,DFDY,DFDZ,POIDS)

          DO 140 I = 1,NNO
            II = NDDL* (I-1)
            DO 130 J = 1,3
              ZR(IVECTU+II+J-1) = ZR(IVECTU+II+J-1) +
     +                            POIDS*ZR(IVF+L+I-1)*ZR(IFORC+J-1)
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
          CALL DFDM2D(NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDK+K),
     +                ZR(IGEOM),DFDX,DFDY,POIDS)
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
     +                               POIDS*ZR(IFORC+L)*ZR(IVF+K+I-1)
            ZR(IVECTU+NDDL* (I-1)+1) = ZR(IVECTU+NDDL* (I-1)+1) +
     +                                 POIDS*ZR(IFORC+L+1)*ZR(IVF+K+I-1)
  170     CONTINUE
  180   CONTINUE
      END IF

C***********************************************************************
C  OPTION : FORC_NODA
C***********************************************************************


      IF (OPTION.EQ.'FORC_NODA') THEN

        TRAV = '&INEL.'//ELREFE//'.TRAVAIL'
        CALL JEVETE(TRAV,'L',ITRAV)

C - PARAMETRES EN ENTREE
        CALL JEVECH('PGEOMER','L',IGEOM)
        CALL JEVECH('PCONTMR','L',ICONTM)
        CALL JEVECH('PMATERC','L',IMATE)
C
C  SI LES TEMPS PLUS ET MOINS SONT PRESENTS
C  C EST QUE L ON APPELLE DEPUIS STAT NON LINE ET
C  ALORS LES TERMES DEPENDANT DE DT SONT EVALUES
C
      CALL TECACH(.TRUE.,.FALSE.,'PINSTMR ',1,IINSTM)
      CALL TECACH(.TRUE.,.FALSE.,'PINSTPR ',1,IINSTP)
      IF ( IINSTM.GT.0.AND.IINSTP.GT.0) THEN
       DT = ZR(IINSTP)-ZR(IINSTM)
       FNOEVO=.TRUE.
      ELSE
       FNOEVO=.FALSE.
       DT = 0.D0
      ENDIF
C
C
C - PARAMETRES EN SORTIE
        CALL JEVECH('PVECTUR','E',IVECTU)
C
C
        CALL FNOTHM(FNOEVO,DT,NNO,NNOS,NPGU,
     >              ZR(IPOIDS),ZR(IVF),ZR(IDFDE),ZR(IDFDN),
     +              ZR(IDFDK),ZR(IGEOM),ZR(ICONTM),
     +              B,DFDI,R,ZR(IVECTU),ZI(IMATE),
     +              MECANI,PRESS1,PRESS2,TEMPE,DIMDEF,DIMCON,
     +              NDDL,NMEC,NP1,NP2,NDIM,TYPMOD,AXI,
     >              NVOMAX,NNOMAX,NSOMAX,NBVOS,VOISIN,P2P1)
C
      END IF
C
C
C***********************************************************************

C  OPTION : SIEF_ELNO_ELGA ET VARI_ELNO_ELGA

C***********************************************************************


      IF ((OPTION.EQ.'SIEF_ELNO_ELGA') .OR.
     +    (OPTION.EQ.'VARI_ELNO_ELGA')) THEN
        NCMP = DIMCON
C
        IF (OPTION.EQ.'SIEF_ELNO_ELGA  ') THEN
          LGPG = NCMP
          CALL JEVECH('PCONTRR','L',ICHG)
          CALL JEVECH('PSIEFNOR','E',ICHN)
        END IF
        IF (OPTION.EQ.'VARI_ELNO_ELGA  ') THEN
          CALL JEVECH('PVARIGR','L',ICHG)
          CALL JEVECH('PCOMPOR','L',ICOMPO)
          CALL JEVECH('PVARINR','E',ICHN)
          READ (ZK16(ICOMPO+1),'(I16)') NCMP
          CALL TECACH(.TRUE.,.TRUE.,'PVARIGR',7,JTAB)
          LGPG = MAX(JTAB(6),1)*JTAB(7)
        END IF

C --- CAS 3D

        IF (NDIM .EQ. 3) THEN
          ELREFL =ELREFE

C --- RECUPERATION DE LA MATRICE DE PASSAGE GAUSS -----> SOMMETS
          IF ( LUMPED) THEN
             CHMAT = '&INEL.'//ELREFL//'.A'
          ELSE
           IF (ELREFE.EQ.'HEXA20' .OR. ELREFE.EQ.'TETRA10' .OR.
     +         ELREFE.EQ.'HEXA27' .OR. ELREFE.EQ.'PENTA15' )  THEN
            CHMAT = '&INEL.'//ELREFL//'.B'
           ELSE
            CHMAT = '&INEL.'//ELREFL//'.A'
           END IF
          ENDIF
          CALL JEVETE(CHMAT,'L',JMAT)
          JMAT = JMAT+2

C --- CHAMELEM(NOEUD) = P * CHAMELEM(GAUSS)

          DO 240 IC = 1,NCMP
            DO 230 I = 1,NNOS
              S = 0.D0
              DO 220 J = 1,NPGU
                S = S + ZR(JMAT-1+ (I-1)*NPGU+J)*
     +              ZR(ICHG+LGPG* (J-1)+IC-1)
  220         CONTINUE
              ZR(ICHN+LGPG* (I-1)+IC-1) = S
  230       CONTINUE
  240     CONTINUE

C --- PASSAGE DES SOMMETS AUX NOEUDS MILIEUX PAR VALEUR MOYENNE

          IF (ELREFE.EQ.'HEXA20' .OR. ELREFE.EQ.'HEXA27' .OR.
     +        ELREFE.EQ.'HEXS20') THEN
            DO 250 J = 1,NCMP

C ------- NOEUDS 9 A 12

              ZR(ICHN+LGPG*8+J-1) = 0.5D0*
     +                              (ZR(ICHN+J-1)+ZR(ICHN+LGPG+J-1))
              ZR(ICHN+LGPG*9+J-1) = 0.5D0*
     +                              (ZR(ICHN+LGPG+J-1)+ZR(ICHN+LGPG*2+J-
     +                              1))
              ZR(ICHN+LGPG*10+J-1) = 0.5D0*
     +                               (ZR(ICHN+LGPG*2+J-1)+ZR(ICHN+
     +                               LGPG*3+J-1))
              ZR(ICHN+LGPG*11+J-1) = 0.5D0*
     +                               (ZR(ICHN+LGPG*3+J-1)+ZR(ICHN+J-1))

C ------- NOEUDS 13 A 16

              ZR(ICHN+LGPG*12+J-1) = 0.5D0*
     +                               (ZR(ICHN+J-1)+ZR(ICHN+LGPG*4+J-1))
              ZR(ICHN+LGPG*13+J-1) = 0.5D0*
     +                               (ZR(ICHN+LGPG+J-1)+ZR(ICHN+LGPG*5+
     +                               J-1))
              ZR(ICHN+LGPG*14+J-1) = 0.5D0*
     +                               (ZR(ICHN+LGPG*2+J-1)+ZR(ICHN+
     +                               LGPG*6+J-1))
              ZR(ICHN+LGPG*15+J-1) = 0.5D0*
     +                               (ZR(ICHN+LGPG*3+J-1)+ZR(ICHN+
     +                               LGPG*7+J-1))

C ------- NOEUDS 17 A 20

              ZR(ICHN+LGPG*16+J-1) = 0.5D0*
     +                               (ZR(ICHN+LGPG*4+J-1)+ZR(ICHN+
     +                               LGPG*5+J-1))
              ZR(ICHN+LGPG*17+J-1) = 0.5D0*
     +                               (ZR(ICHN+LGPG*6+J-1)+ZR(ICHN+
     +                               LGPG*5+J-1))
              ZR(ICHN+LGPG*18+J-1) = 0.5D0*
     +                               (ZR(ICHN+LGPG*7+J-1)+ZR(ICHN+
     +                               LGPG*6+J-1))
              ZR(ICHN+LGPG*19+J-1) = 0.5D0*
     +                               (ZR(ICHN+LGPG*4+J-1)+ZR(ICHN+
     +                               LGPG*7+J-1))
  250       CONTINUE

          ELSE IF (ELREFE.EQ.'PENTA15') THEN
            DO 260 J = 1,NCMP

C ------- NOEUDS 7 A 9

              ZR(ICHN+LGPG*6+J-1) = 0.5D0*
     +                              (ZR(ICHN+J-1)+ZR(ICHN+LGPG+J-1))
              ZR(ICHN+LGPG*7+J-1) = 0.5D0*
     +                              (ZR(ICHN+LGPG+J-1)+ZR(ICHN+LGPG*2+J-
     +                              1))
              ZR(ICHN+LGPG*8+J-1) = 0.5D0*
     +                              (ZR(ICHN+LGPG*2+J-1)+ZR(ICHN+J-1))

C ------- NOEUDS 10 A 12

              ZR(ICHN+LGPG*9+J-1) = 0.5D0*
     +                              (ZR(ICHN+J-1)+ZR(ICHN+LGPG*3+J-1))
              ZR(ICHN+LGPG*10+J-1) = 0.5D0*
     +                               (ZR(ICHN+LGPG+J-1)+ZR(ICHN+LGPG*4+
     +                               J-1))
              ZR(ICHN+LGPG*11+J-1) = 0.5D0*
     +                               (ZR(ICHN+LGPG*2+J-1)+ZR(ICHN+
     +                               LGPG*5+J-1))

C ------- NOEUDS 13 A 15

              ZR(ICHN+LGPG*12+J-1) = 0.5D0*
     +                               (ZR(ICHN+LGPG*3+J-1)+ZR(ICHN+
     +                               LGPG*4+J-1))
              ZR(ICHN+LGPG*13+J-1) = 0.5D0*
     +                               (ZR(ICHN+LGPG*4+J-1)+ZR(ICHN+
     +                               LGPG*5+J-1))
              ZR(ICHN+LGPG*14+J-1) = 0.5D0*
     +                               (ZR(ICHN+LGPG*5+J-1)+ZR(ICHN+
     +                               LGPG*3+J-1))
  260       CONTINUE

          ELSE IF (ELREFE.EQ.'TETRA10') THEN

            DO 270 J = 1,NCMP

C ------- NOEUDS 5 A 7

              ZR(ICHN+LGPG*4+J-1) = 0.5D0*
     +                              (ZR(ICHN+J-1)+ZR(ICHN+LGPG+J-1))
              ZR(ICHN+LGPG*5+J-1) = 0.5D0*
     +                              (ZR(ICHN+LGPG+J-1)+ZR(ICHN+LGPG*2+J-
     +                              1))
              ZR(ICHN+LGPG*6+J-1) = 0.5D0*
     +                              (ZR(ICHN+LGPG*2+J-1)+ZR(ICHN+J-1))

C ------- NOEUDS 8 A 10

              ZR(ICHN+LGPG*7+J-1) = 0.5D0*
     +                              (ZR(ICHN+J-1)+ZR(ICHN+LGPG*3+J-1))
              ZR(ICHN+LGPG*8+J-1) = 0.5D0*
     +                              (ZR(ICHN+LGPG+J-1)+ZR(ICHN+LGPG*3+J-
     +                              1))
              ZR(ICHN+LGPG*9+J-1) = 0.5D0*
     +                              (ZR(ICHN+LGPG*2+J-1)+ZR(ICHN+LGPG*3+
     +                              J-1))

  270       CONTINUE
          END IF

          IF (ELREFE.EQ.'HEXA27') THEN
            DO 280 J = 1,NCMP

C ------- NOEUDS 21 A 27

              ZR(ICHN+LGPG*20+J-1) = 0.25D0*
     +                               (ZR(ICHN+J-1)+ZR(ICHN+LGPG+J-1)+
     +                               ZR(ICHN+LGPG*2+J-1)+
     +                               ZR(ICHN+LGPG*3+J-1))
              ZR(ICHN+LGPG*21+J-1) = 0.25D0*
     +                               (ZR(ICHN+J-1)+ZR(ICHN+LGPG+J-1)+
     +                               ZR(ICHN+LGPG*5+J-1)+
     +                               ZR(ICHN+LGPG*4+J-1))
              ZR(ICHN+LGPG*22+J-1) = 0.25D0*
     +                               (ZR(ICHN+LGPG+J-1)+ZR(ICHN+LGPG*2+
     +                               J-1)+ZR(ICHN+LGPG*6+J-1)+
     +                               ZR(ICHN+LGPG*5+J-1))
              ZR(ICHN+LGPG*23+J-1) = 0.25D0*
     +                               (ZR(ICHN+LGPG*2+J-1)+ZR(ICHN+
     +                               LGPG*3+J-1)+ZR(ICHN+LGPG*7+J-1)+
     +                               ZR(ICHN+LGPG*6+J-1))
              ZR(ICHN+LGPG*24+J-1) = 0.25D0*
     +                               (ZR(ICHN+J-1)+ZR(ICHN+LGPG*3+J-1)+
     +                               ZR(ICHN+LGPG*7+J-1)+
     +                               ZR(ICHN+LGPG*4+J-1))
              ZR(ICHN+LGPG*25+J-1) = 0.25D0*
     +                               (ZR(ICHN+LGPG*4+J-1)+ZR(ICHN+
     +                               LGPG*5+J-1)+ZR(ICHN+LGPG*6+J-1)+
     +                               ZR(ICHN+LGPG*7+J-1))
              ZR(ICHN+LGPG*26+J-1) = 0.125D0*
     +                               (ZR(ICHN+J-1)+ZR(ICHN+LGPG+J-1)+
     +                               ZR(ICHN+LGPG*2+J-1)+
     +                               ZR(ICHN+LGPG*3+J-1)+
     +                               ZR(ICHN+LGPG*4+J-1)+
     +                               ZR(ICHN+LGPG*5+J-1)+
     +                               ZR(ICHN+LGPG*6+J-1)+
     +                               ZR(ICHN+LGPG*7+J-1))
  280       CONTINUE
          END IF

          IF (ELREFE.EQ.'PYRAM13') THEN

            DO 290 J = 1,NCMP

C   NOEUDS 6 A 9
              ZR(ICHN+LGPG*5+J-1) = 0.5D0*
     +                              (ZR(ICHN+J-1)+ZR(ICHN+LGPG+J-1))
              ZR(ICHN+LGPG*6+J-1) = 0.5D0*
     +                              (ZR(ICHN+LGPG+J-1)+ZR(ICHN+LGPG*2+J-
     +                              1))
              ZR(ICHN+LGPG*7+J-1) = 0.5D0*
     +                              (ZR(ICHN+LGPG*2+J-1)+ZR(ICHN+LGPG*3+
     +                              J-1))
              ZR(ICHN+LGPG*8+J-1) = 0.5D0*
     +                              (ZR(ICHN+LGPG*3+J-1)+ZR(ICHN+J-1))

C   NOEUDS 10 A 13
              ZR(ICHN+LGPG*9+J-1) = 0.5D0*
     +                              (ZR(ICHN+J-1)+ZR(ICHN+LGPG*4+J-1))
              ZR(ICHN+LGPG*10+J-1) = 0.5D0*
     +                               (ZR(ICHN+LGPG+J-1)+ZR(ICHN+LGPG*4+
     +                               J-1))
              ZR(ICHN+LGPG*11+J-1) = 0.5D0*
     +                               (ZR(ICHN+LGPG*2+J-1)+ZR(ICHN+
     +                               LGPG*4+J-1))
              ZR(ICHN+LGPG*12+J-1) = 0.5D0*
     +                               (ZR(ICHN+LGPG*3+J-1)+ZR(ICHN+
     +                               LGPG*4+J-1))
  290       CONTINUE

          END IF


C --- CHAMELEM(GAUSS) = P * CHAMELEM(NOEUD)
C --- INUTILISE DONC SUPPRIME


C --- CAS 2D
      ELSE

C --- CHAMELEM(NOEUD) = P * CHAMELEM(GAUSS)
          CALL PPGANO(NNOS,NPGU,NCMP,ZR(ICHG),ZR(ICHN))

C --- CHAMELEM(GAUSS) = P * CHAMELEM(NOEUD)

      END IF
      END IF
      END
