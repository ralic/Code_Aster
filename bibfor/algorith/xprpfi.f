      SUBROUTINE XPRPFI(P,LSNP,LCMIN,POIFIS,TRIFIS,FISS,NDIM,LSN,LST)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      REAL*8         P(3),LSNP,LCMIN,LSN,LST
      CHARACTER*19   POIFIS,TRIFIS
      CHARACTER*8    FISS
      INTEGER        NDIM

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
C RESPONSABLE COLOMBO D.COLOMBO
C TOLE CRP_20

C     ------------------------------------------------------------------
C
C       XPRPFI   : X-FEM PROPAGATION : CALCUL DE LA PROJECTION D'UN
C       ------     -     --                         -
C                  POINT SUR LA SURFACE DE LA FISSURE
C                                             --
C    DANS LE CADRE DE LA PROPAGATION X-FEM, ON CALCULE LA VALEUR DES
C    LEVEL SETS NORMALE ET TANGENTIELLE POUR UN POINT EN UTILISANT LEUR
C    DEFINITION. ON CALCULE DONC LA PROJECTION DU POINT SUR LA SURFACE
C    DE LA FISSURE.
C
C    ENTREE
C        P      = COORDONNEES DU POINT A PROJETER
C        LSNP   = VALEUR ACTUELLE DE LSN (OU LST SI ON REINITIALISE LA
C                 LEVEL SET TANGENTIELLE)
C        LCMIN  = LONGUEUR DE LA PLUS PETITE ARETE DU MAILLAGE OU DE LA
C                 GRILLE AUXILIAIRE
C        POIFIS = NOM DE L'OBJET JEVEUX OU LA LISTE DES COORDONNEES DES
C                 POINTS D'INTERSECTION ENTRE LES ELEMENTS ET LSN=0
C                 EST STOCKEE (VOIR XPRLS0.F)
C        TRIFIS = NOM DE L'OBJET JEVEUX OU LE NUMERO ET LA LISTE DES
C                 POINTS D'INTERSECTION ENTRE LES ELEMENTS ET LSN=0 SONT
C                 STOCKES (VOIR XPRLS0.F)
C        FISS   = NOM DU CONCEPT FISSURE X-FEM
C        NDIM   = DIMENSION DU MODELE (2=2D, 3=3D)
C
C    SORTIE
C        LSN    = VALEUR DE LA LEVEL SET NORMALE AU POINT P
C        LST    = VALEUR DE LA LEVEL SET TANGENTIELLE AU POINT P
C
C    DANS LE CAS OU ON CALCULE LA REINITIALISATION DE LST, ON DONNE EN
C    ENTRE LA LST (PARAMETRE LSNP) ET ON OBTIENT LA VALEUR DE LST
C    RECALCULEE EN SORTIE (PARAMETRE LSN) ET LA VALEUR DE LSN (PARAMETRE
C    LST).
C
C     ------------------------------------------------------------------


C     GENERAL PURPOSE
      INTEGER        IFM,NIV,I
      REAL*8         R8MAEM
      CHARACTER*1    K1BID

C     UPWIND INTEGRATION
      INTEGER        JTRI,JPOI,ELCUT,NTRI,ITRI,IA,IB,IC,NPTINT,PA,PB,PC,
     &               PSX,PDX,PTER,NELCOU,NP,JFONF,JFMULT,NUMPON
      REAL*8         A(3),B(3),C(3),LSTA,LSTB,LSTC,M(3),D,VN(3),EPS(3),
     &               BESTD,MP(3),
     &               V(3),
     &               R8PREM,VNELE(3),VIN(3),PM(3),PMP(3),TOLL,D1
      LOGICAL        IN,EPS1Z,EPS2Z,EPS3Z,EPS1U,
     &               EPS2U,MVERT,LIBRE,MPIN,KINK

C  TRIANGLES ABC QUE L'ON PEUT FORMER A PARTIR DE N POINTS (N=3 A 6)
      INTEGER        IATRI(20),IBTRI(20),ICTRI(20)
C        ---------------------
C        |  I | TRIANGLE | N |
C        --------------------
C        |  1 |   1 2 3  | 3 |
C        --------------------
C        |  2 |   1 2 4  |   |
C        |  3 |   1 3 4  | 4 |
C        |  4 |   2 3 4  |   |
C        --------------------
C        |  5 |   1 2 5  |   |
C        |  6 |   1 3 5  |   |
C        |  7 |   1 4 5  | 5 |
C        |  8 |   2 3 5  |   |
C        |  9 |   2 4 5  |   |
C        | 10 |   3 4 5  |   |
C        --------------------
C        | 11 |   1 2 6  |   |
C        | 12 |   1 3 6  |   |
C        | 13 |   1 4 6  |   |
C        | 14 |   1 5 6  |   |
C        | 15 |   2 3 6  | 6 |
C        | 16 |   2 4 6  |   |
C        | 17 |   2 5 6  |   |
C        | 18 |   3 4 6  |   |
C        | 19 |   3 5 6  |   |
C        | 20 |   4 5 6  |   |
C        --------------------
      DATA   IATRI/1,1,1,2,1,1,1,2,2,3,1,1,1,1,2,2,2,3,3,4/
      DATA   IBTRI/2,2,3,3,2,3,4,3,4,4,2,3,4,5,3,4,5,4,5,5/
      DATA   ICTRI/3,4,4,4,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,6/

C-----------------------------------------------------------------------
C     DEBUT
C-----------------------------------------------------------------------
      CALL JEMARQ()
      CALL INFMAJ()
      CALL INFNIV(IFM,NIV)

C     INITIALISE THE VALUE OF THE DISTANCE
      BESTD  = R8MAEM()

C     INITIALISE THE FLAG USED TO ASSESS IF THE POINT IS INSIDE THE
C     TRIANGLE AND CALCULATE THE TOLERANCE
      MVERT  = .FALSE.
      TOLL = 1.D-2*LCMIN

C     RETRIEVE THE VECTORS CONTAINING THE TRIANGULATION OF LSN=0
      CALL JEVEUO(POIFIS,'L',JPOI)
      CALL JEVEUO(TRIFIS,'L',JTRI)

C     RETRIEVE THE NUMBER OF ELEMENTS CUT BY THE LSN=0
      CALL JELIRA(TRIFIS,'LONMAX',ELCUT,K1BID)
      ELCUT = ELCUT/7

C     ******************************************************************
C     CALCULATE THE (INNER OR OUTER) PROJECTION OF THE POINT P ON THE
C     TRIANGULATION DEFINING THE LSN=0
C     ******************************************************************

C     LOOP ON EACH ELEMENT CUT BY THE LSN=0. THE SAME ALGORITHM CODED
C     IN XPRLS0.F IS USED HERE.
      DO 1000 I=1,ELCUT

C        RETREIVE THE NUMBER OF INTERSECTION POINTS FOR THE ELEMENT
         NPTINT = ZI(JTRI-1+7*(I-1)+1)

C        DETERMINE THE NUMBER OF TRIANGLES THAT CAN BE DEFINED
         IF (NPTINT.EQ.3)  NTRI=1
         IF (NPTINT.EQ.4)  NTRI=4
         IF (NPTINT.EQ.5)  NTRI=10
         IF (NPTINT.EQ.6)  NTRI=20

C        LOOP ON EACH TRIANGLE TO CALCULATE THE PROJECTION POINT AND
C        THE NORMAL DISTANCE OF THE POINT TO THE LSN=0
         DO 350 ITRI=1,NTRI
            IA = IATRI(ITRI)
            IB = IBTRI(ITRI)
            IC = ICTRI(ITRI)

C           RETREIVE THE POSITION OF THE THREE POINTS A,B,C OF THE
C           TRIANGLE IN THE COORDINATES TABLE
            PA = ZI(JTRI-1+7*(I-1)+IA+1)
            PB = ZI(JTRI-1+7*(I-1)+IB+1)
            PC = ZI(JTRI-1+7*(I-1)+IC+1)

C           RETREIVE THEIR COORDINATES
            A(1) = ZR(JPOI-1+4*(PA-1)+1)
            A(2) = ZR(JPOI-1+4*(PA-1)+2)
            A(3) = ZR(JPOI-1+4*(PA-1)+3)

            B(1) = ZR(JPOI-1+4*(PB-1)+1)
            B(2) = ZR(JPOI-1+4*(PB-1)+2)
            B(3) = ZR(JPOI-1+4*(PB-1)+3)

            C(1) = ZR(JPOI-1+4*(PC-1)+1)
            C(2) = ZR(JPOI-1+4*(PC-1)+2)
            C(3) = ZR(JPOI-1+4*(PC-1)+3)

C           RETREIVE THEIR LST
            LSTA = ZR(JPOI-1+4*(PA-1)+4)
            LSTB = ZR(JPOI-1+4*(PB-1)+4)
            LSTC = ZR(JPOI-1+4*(PC-1)+4)

C           CALCULATE THE (CORRECTED AND NOT) PROJECTION AND DISTANCE
            CALL XPROJ(P,A,B,C,M,MP,D,VN,EPS,IN)

C           SEARCH FOR THE TRIANGLE THAT MINIMISE THE DISTANCE
            IF (D.LT.BESTD) THEN

C              STORE THE DISTANCE, THE LST AND THE "IN/OUT" FLAG
               BESTD = D
               LST = EPS(1)*LSTB + EPS(2)*LSTC + EPS(3)*LSTA

C              STORE THE POINTS DEFINING THE EDGE TO WHICH THE
C              PROJECTED POINT M BELONGS (IF A FURTHER CORRECTION WILL
C              BE NECESSARY)
               IF (.NOT.IN) THEN

C                 ANALYZE THE PARAMETRIC COORDINATES EPS1,EPS2 AND EPS3
                  IF (EPS(1).LT.R8PREM()) THEN
                     EPS1Z = .TRUE.
                  ELSE
                     EPS1Z = .FALSE.
                  ENDIF

                  IF (EPS(2).LT.R8PREM()) THEN
                     EPS2Z = .TRUE.
                  ELSE
                     EPS2Z = .FALSE.
                  ENDIF

                  IF (EPS(3).LT.R8PREM()) THEN
                     EPS3Z = .TRUE.
                  ELSE
                     EPS3Z = .FALSE.
                  ENDIF

                  IF (ABS(EPS(1)-1).LT.R8PREM()) THEN
                     EPS1U = .TRUE.
                  ELSE
                     EPS1U = .FALSE.
                  ENDIF

                  IF (ABS(EPS(2)-1).LT.R8PREM()) THEN
                     EPS2U = .TRUE.
                  ELSE
                     EPS2U = .FALSE.
                  ENDIF

C                 LOCATE THE EDGE USING THE PARAMETRIC COORDINATES AND
C                 STORE ALL THE DATA THAT WILL BE EVENTUALLY USED FOR
C                 FURTHER CALCULATIONS
                  IF (EPS1Z.AND.(.NOT.EPS2Z).AND.(.NOT.EPS2U)) THEN
C                    THE POINT M IS ON EDGE A-C OF THE TRIANGLE
                     PSX    = PA
                     PDX    = PC
                     PTER   = PB
                     NELCOU = I
                     PM(1)  = M(1)
                     PM(2)  = M(2)
                     PM(3)  = M(3)
                     PMP(1) = MP(1)
                     PMP(2) = MP(2)
                     PMP(3) = MP(3)
                     VNELE(1) = VN(1)
                     VNELE(2) = VN(2)
                     VNELE(3) = VN(3)
                     MVERT = .FALSE.

                  ELSE IF (EPS2Z.AND.(.NOT.EPS1Z).AND.(.NOT.EPS1U)) THEN
C                    THE POINT M IS ON EDGE A-B OF THE TRIANGLE
                     PSX    = PA
                     PDX    = PB
                     PTER   = PC
                     NELCOU = I
                     PM(1)  = M(1)
                     PM(2)  = M(2)
                     PM(3)  = M(3)
                     PMP(1) = MP(1)
                     PMP(2) = MP(2)
                     PMP(3) = MP(3)
                     VNELE(1) = VN(1)
                     VNELE(2) = VN(2)
                     VNELE(3) = VN(3)
                     MVERT = .FALSE.

                  ELSE IF (EPS3Z.AND.(.NOT.EPS1Z).AND.(.NOT.EPS1U).AND.
     &                    (.NOT.EPS2Z).AND.(.NOT.EPS2U)) THEN
C                    THE POINT M IS ON EDGE B-C OF THE TRIANGLE
                     PSX    = PB
                     PDX    = PC
                     PTER   = PA
                     NELCOU = I
                     PM(1)  = M(1)
                     PM(2)  = M(2)
                     PM(3)  = M(3)
                     PMP(1) = MP(1)
                     PMP(2) = MP(2)
                     PMP(3) = MP(3)
                     VNELE(1) = VN(1)
                     VNELE(2) = VN(2)
                     VNELE(3) = VN(3)
                     MVERT = .FALSE.

                  ELSE
C                    THE POINT M IS COINCIDENT WITH ONE OF THE THREE
C                    NODES. THIS IS THE MOST COMPLICATED CASE AND IT
C                    WILL SUBJECTED TO FURTHER CALCULATIONS IF NECESSARY
                     IF (EPS1Z.AND.EPS2Z) THEN
C                       POINT M = POINT A
                        PSX    = PA
                        PDX    = PB
                        PTER   = PC
                        NELCOU = I
                        PM(1)  = M(1)
                        PM(2)  = M(2)
                        PM(3)  = M(3)
                        PMP(1) = MP(1)
                        PMP(2) = MP(2)
                        PMP(3) = MP(3)
                        VNELE(1) = VN(1)
                        VNELE(2) = VN(2)
                        VNELE(3) = VN(3)
                        MVERT = .TRUE.
                     ELSE IF (EPS1Z.AND.EPS2U) THEN
C                       POINT M = POINT C
                        PSX    = PC
                        PDX    = PA
                        PTER   = PB
                        NELCOU = I
                        PM(1)  = M(1)
                        PM(2)  = M(2)
                        PM(3)  = M(3)
                        PMP(1) = MP(1)
                        PMP(2) = MP(2)
                        PMP(3) = MP(3)
                        VNELE(1) = VN(1)
                        VNELE(2) = VN(2)
                        VNELE(3) = VN(3)
                        MVERT = .TRUE.
                     ELSE IF (EPS1U.AND.EPS2Z) THEN
C                       POINT M = POINT B
                        PSX    = PB
                        PDX    = PC
                        PTER   = PA
                        NELCOU = I
                        PM(1)  = M(1)
                        PM(2)  = M(2)
                        PM(3)  = M(3)
                        PMP(1) = MP(1)
                        PMP(2) = MP(2)
                        PMP(3) = MP(3)
                        VNELE(1) = VN(1)
                        VNELE(2) = VN(2)
                        VNELE(3) = VN(3)
                        MVERT = .TRUE.
                     ELSE

C                       THIS CASE DOESN'T EXIST. FOR SAFETY IT'S BETTER
C                       TO STOP THE CODE IF WE WILL ARRIVE IN THIS POINT
C                       OF THE SUBROUTINE
                        CALL ASSERT(1.GT.2)

                     ENDIF

                  ENDIF

               ELSE

C                 IF THE PROJECTION IS INSIDE THE TRIANGLE, THERE'S NO
C                 NEED TO STORE THE POINTS DEFINING THE EDGE OF THE
C                 TRIANGLE TO WHICH THE PROJECTION BELONGS
                  MVERT = .FALSE.
                  PSX = 0

               ENDIF

            ENDIF

350      CONTINUE

1000  CONTINUE

C     ******************************************************************
C     IN ORDER TO CORRECTLY CALCULATE LSN, THE NORMAL DISTANCE TO THE
C     PLANE OF THE SELECTED TRIANGLE IS CALCULATED IF THE POINT
C     PROJECTION IS OUTSIDE THE DOMAIN OF THE CALCULUS.
C     ******************************************************************

C     CASE 1: THE NORMAL PROJECTION IS OUTSIDE THE TRIANGLE AND
C             THE CORRECTED PROJECTION BELONGS TO AN EDGE OF THE
C             TRIANGLE
      IF ((.NOT.MVERT).AND.
     &    (PSX.GT.0).AND.(PDX.GT.0).AND.(PTER.GT.0)) THEN

C         CALCULATE THE DISTANCE BETWEEN THE PROJECTION POINT (OUTSIDE)
C         AND THE CORRECTED PROJECTION POINT (ON THE EDGE)
          D=SQRT((PM(1)-PMP(1))**2+(PM(2)-PMP(2))**2+(PM(3)-PMP(3))**2)

C         IF THE DISTANCE IS LOWER THAN THE TOLERANCE, THE TWO POINTS
C         ARE CONSIDERED COINCIDENT AND THE CALCULATED NORMAL DISTANCE
C         IS ACCEPTED
          IF (D.GT.TOLL) THEN

C            YES, THE PROJECTION IS EFFECTIVELY OUTSIDE THE TRIANGLE.
C            VERIFY IF THE EDGE OF THE TRIANGLE IS SHARED BY ANY OTHER
C            TRIANGLE BELONGING TO ANOTHER ELEMENT TO ASSESS IF THE
C            PROJECTION IS OUTSIDE THE DOMAIN.
             CALL XPRALI(PSX,PDX,VNELE,NELCOU,POIFIS,TRIFIS,LIBRE,VIN)

             IF (LIBRE) THEN
C               YES, THE EDGE IS A FREE EDGE AND THEREFORE THE
C               PROJECTION IS ALSO OUTSIDE THE DOMAIN.
C               THE NORMAL DISTANCE MUST BE CALCULATED.
                BESTD = SQRT((P(1)-PMP(1))**2+(P(2)-PMP(2))**2+
     &                       (P(3)-PMP(3))**2)
             ENDIF

          ENDIF

C     CASE 2: THE NORMAL PROJECTION IS OUTSIDE THE TRIANGLE AND
C             THE CORRECTED PROJECTION IS COINCIDENT WITH ONE OF THE
C             THREE POINTS DEFINING THE TRIANGLE. THIS IS THE MOST
C             COMPLICATED CASE.
      ELSE IF (MVERT.AND.(NDIM.EQ.3)) THEN

C          RETREIVE THE COORDINATES OF THE POINT OF THE TRIANGLE
           B(1) = ZR(JPOI-1+4*(PSX-1)+1)
           B(2) = ZR(JPOI-1+4*(PSX-1)+2)
           B(3) = ZR(JPOI-1+4*(PSX-1)+3)

C          CALCULATE THE DISTANCE BETWEEN THE PROJECTED POINT AND
C          THE POINT OF THE TRIANGLE ABOVE
           D = SQRT((B(1)-PMP(1))**2+(B(2)-PMP(2))**2+(B(3)-PMP(3))**2)

C          IF THEY ARE NOT COINCIDENT, WE CAN PROCEED WITH THE
C          CALCULATIONS
           IF (D.GT.R8PREM()) THEN

C             IN THE CASE THE PROJECTED POINT PSX IS COINCIDENT WITH
C             ONE OF THE END OF THE CRACK FRONT, A CORRECTION MUST BE
C             ADOPTED IN ORDER TO PREVENT A DEFORMATION OF THE LEVEL
C             SET IN PRESENCE OF KINKS.
              KINK = .FALSE.

C             RETREIVE THE COORDINATES OF THE POINT OF THE CRACK FRONT
              CALL JEVEUO(FISS//'.FONDFISS','L',JFONF)

C             RETREIVE THE END POINTS OF EACH PART OF THE CRACK FRONT
              CALL JEVEUO(FISS//'.FONDMULT','L',JFMULT)
              CALL JELIRA(FISS//'.FONDMULT','LONMAX',NUMPON,K1BID)

C             LOOP ON THE END POINTS OF THE FRONT TO CHECK THE "KINK
C             CONDITION"
              DO 1500 I=1,NUMPON

C                RETREIVE THE COORDINATES OF THE POINT
                 A(1) = ZR(JFONF-1+4*(I-1)+1)
                 A(2) = ZR(JFONF-1+4*(I-1)+2)
                 A(3) = ZR(JFONF-1+4*(I-1)+3)

C                CALCULATE THE DISTANCE
                 D = SQRT((A(1)-B(1))**2+(A(2)-B(2))**2+(A(3)-B(3))**2)

                 IF (D.LT.R8PREM()) THEN
                    KINK = .TRUE.
                    GOTO 1600
                 ENDIF

1500          CONTINUE

C             CONTINUE WITH THE CALCULATIONS. THE KINK CONDITION WILL
C             BE CONSIDERED LATER.
1600          CONTINUE

C             RETREIVE THE NUMBER OF INTERSECTION POINTS FOR THE
C             ELEMENT
              NPTINT = ZI(JTRI-1+7*(NELCOU-1)+1)

C             FLAG=PROJECTION INSIDE THE TRIANGLE
              MPIN = .TRUE.

C             SEARCH FOR THE FREE EDGES (OF ANY TRIANGLE) STARTING FROM
C             THE PROJECTION NODE. NO MORE THAN TWO FREE EDGES ARE
C             EXPECTED
              DO 2000 I=1,NPTINT

C                RETREIVE THE I-TH INTERSECTION POINT OF THE ELEMENT
                 NP = ZI(JTRI-1+7*(NELCOU-1)+I+1)

                 IF (NP.NE.PSX) THEN

C                   CHECK IF THE EDGE IS ON THE FREE SURFACE
                    CALL XPRALI(PSX,NP,VNELE,NELCOU,POIFIS,TRIFIS,LIBRE,
     &                          VIN)

C                   YES...
                    IF (LIBRE) THEN
C                      RETREIVE THE COORDINATES OF THE PROJECTED POINT
                       A(1) = PMP(1)
                       A(2) = PMP(2)
                       A(3) = PMP(3)
C                      CALCULATE THE VECTOR CONNECTING THIS LAST POINT
C                      AND THE POINT OF THE TRIANGLE
                       V(1) = A(1)-B(1)
                       V(2) = A(2)-B(2)
                       V(3) = A(3)-B(3)
C                      CALCULATE THE SCALAR PRODUCT BETWEEN THIS VECTOR
C                      AND THE NORMAL TO THE FREE SURFACE OF THE DOMAIN,
C                      THAT IS THE DISTANCE BETWEEN THE PROJECTED POINT
C                      AND THE FREE SURFACE
                       D = V(1)*VIN(1)+V(2)*VIN(2)+V(3)*VIN(3)
C                      A NEGATIVE VALUE MEANS THAT THE POINT IS OUTSIDE
C                      THE DOMAIN (A TOLERANCE IS USED AS USUAL)
                       IF ((D.LT.0.D0).AND.(ABS(D).GT.TOLL))THEN
                          MPIN=.FALSE.
                          GOTO 2100
                       ENDIF
                    ENDIF

                 ENDIF

2000          CONTINUE

2100          CONTINUE

C             IF THE POINT IS EFFECTIVELY OUTSIDE THE DOMAIN, CALCULATE
C             THE DISTANCE BETWEEN POINT P AND THE PROJECTION POINT
              IF (.NOT.MPIN) THEN

                    D1 = SQRT((P(1)-PMP(1))**2+(P(2)-PMP(2))**2+
     &                           (P(3)-PMP(3))**2)

C                IN THE CASE OF KINK, A MEAN VALUE IS CALCULATED
                 IF (KINK) THEN
                    BESTD = (BESTD+D1)/2.D0
                 ELSE
                    BESTD = D1
                 ENDIF

              ENDIF

           ENDIF

C     CASE 3: AS CASE 2 BUT WE ARE WORKING ON A 2D MODEL. EVERYTHING IS
C             SIMPLER!
      ELSE IF (MVERT.AND.(NDIM.EQ.2)) THEN

C          FLAG TO INDICATE IF THE PROJECTION POINT IS INSIDE THE CRACK
C          SURFACE
           MPIN = .FALSE.

C          WE SHOULD CHECK IF PSX POINT IS SHARED BY ANY OTHER SEGMENT
C          DEFINING THE CRACK SURFACE
           DO 3000 I=1,ELCUT

C             DO NOT CHECK THE ELEMENT TO WHICH PSX BELONGS
              IF (I.NE.NELCOU) THEN

C                CHECK IF ANY OF THE TWO INTERSECTION POINTS IS
C                COINCIDENT WITH PSX
                 IF ((ZI(JTRI-1+7*(I-1)+2).EQ.PSX).OR.
     &               (ZI(JTRI-1+7*(I-1)+3).EQ.PSX)) THEN
                     MPIN = .TRUE.
                     GOTO 3100
                 ENDIF

              ENDIF

3000       CONTINUE

3100       CONTINUE

C          IF THE POINT IS EFFECTIVELY OUTSIDE THE DOMAIN, CALCULATE
C          THE DISTANCE BETWEEN POINT P AND THE PROJECTION POINT
           IF (.NOT.MPIN) THEN
                    BESTD = SQRT((P(1)-PMP(1))**2+(P(2)-PMP(2))**2+
     &                           (P(3)-PMP(3))**2)
           ENDIF

      ENDIF

C     CALCULATED THE CORRECT NORMAL DISTANCE WITH THE CORRECT SIGN
      LSN = BESTD * SIGN(1.D0,LSNP)

C-----------------------------------------------------------------------
C     FIN
C-----------------------------------------------------------------------
      CALL JEDEMA()
      END
