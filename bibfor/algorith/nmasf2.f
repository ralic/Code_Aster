      SUBROUTINE NMASF2(NNO,NPG,IPOIDS,IVF,IDFDE,GEOM,TYPMOD,
     &                  SIGM,DFDI,VECTU)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 05/04/2004   AUTEUR JMBHH01 J.M.PROIX 
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

      IMPLICIT NONE
      INTEGER NNO,NPG
      CHARACTER*8 TYPMOD(*)
      INTEGER IPOIDS,IVF,IDFDE
      REAL*8 GEOM(2,NNO)
      REAL*8 DFDI(NNO,2)
      REAL*8 SIGM(10,NPG)
      REAL*8 VECTU(2,NNO)

C.......................................................................
C
C     BUT:  CALCUL  DES OPTIONS FORC_NODA
C           EN HYPO-ELASTICITE EN 2D POUR LE QUAD4 SOUS INTEGRE
C           STABILITE PAR ASSUMED STRAIN
C.......................................................................
C IN  NNO     : NOMBRE DE NOEUDS DE L'ELEMENT
C IN  NPG     : NOMBRE DE POINTS DE GAUSS
C IN  IPOIDS  : INDICE POIDS DES POINTS DE GAUSS
C IN  IVFF     :INDICE VALEUR  DES FONCTIONS DE FORME
C IN  IDFDE  :INDICE DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
C IN  GEOM    : COORDONEES DES NOEUDS
C IN  TYPMOD  : TYPE DE MODEELISATION
C IN  SIGM    : CONTRAINTES A L'INSTANT PRECEDENT
C OUT DFDI    : DERIVEE DES FONCTIONS DE FORME  AU DERNIER PT DE GAUSS
C OUT VECTU   : FORCES NODALES
C.......................................................................


      LOGICAL GRAND,AXI
      INTEGER KPG,N,I,M,J,KL,KPGS,PROJ,NPGS
      REAL*8 F(3,3),EPS(6),R,SIGMA(6)
      REAL*8 POIDS,DUM
      REAL*8 RAC2

C     AJ. VARIABLESPOIDSG
      REAL*8 JAC,SIGAS(4,4),DEFC(4,4,2)
      REAL*8 DH(8),GAMMA(8),COOPG(8)
      REAL*8 SDKDX(4),SDKDY(4),SDEDX(4),SDEDY(4),POI2SG(4)
      REAL*8 SDFDY(4,4),SDFDX(4,4),SDFDE(4,4),SDFDK(4,4)
      REAL*8 QPLUS(6),DEFN(4,4,2),KRON(3,3),DEPBID(2,4)

      DATA KRON/1.D0,0.D0,0.D0,0.D0,1.D0,0.D0,0.D0,0.D0,1.D0/


C - INITIALISATION
C   ==============

C    PROJ : INDICATEUR DE LA PROJECTION
C           0 AUCUNE
C           1 OPTIMAL BENDING
C           2 INCOMPRESSIBLE
      PROJ = 2
      RAC2 = SQRT(2.D0)
      CALL R8INIR(8,0.D0,DEPBID,1)
      GRAND = .FALSE.
      AXI = TYPMOD(1) .EQ. 'AXIS'

      DO 20 I = 1,3
        DO 10 J = 1,3
          F(I,J) = KRON(I,J)
   10   CONTINUE
   20 CONTINUE


C - INITIALISATION QUAS4
      CALL INIQS4(NNO,SDFDE,SDFDK,POI2SG,COOPG)

C - CALCUL DU VECTEUR GAMMA
      GAMMA(1) = (GEOM(1,4)* (GEOM(2,2)-GEOM(2,3))+
     &           GEOM(1,2)* (GEOM(2,3)-GEOM(2,4))+
     &           GEOM(1,3)* (GEOM(2,4)-GEOM(2,2)))/
     &           (2* (((GEOM(1,4)-GEOM(1,2))* (GEOM(2,1)-GEOM(2,
     &           3)))+ (GEOM(1,1)-GEOM(1,3))* (GEOM(2,2)-GEOM(2,4))))

      GAMMA(2) = (GEOM(1,4)* (GEOM(2,3)-GEOM(2,1))+
     &           GEOM(1,3)* (GEOM(2,1)-GEOM(2,4))+
     &           GEOM(1,1)* (GEOM(2,4)-GEOM(2,3)))/
     &           (2* (((GEOM(1,4)-GEOM(1,2))* (GEOM(2,1)-GEOM(2,
     &           3)))+ (GEOM(1,1)-GEOM(1,3))* (GEOM(2,2)-GEOM(2,4))))

      GAMMA(3) = (GEOM(1,4)* (GEOM(2,1)-GEOM(2,2))+
     &           GEOM(1,1)* (GEOM(2,2)-GEOM(2,4))+
     &           GEOM(1,2)* (GEOM(2,4)-GEOM(2,1)))/
     &           (2* (((GEOM(1,4)-GEOM(1,2))* (GEOM(2,1)-GEOM(2,
     &           3)))+ (GEOM(1,1)-GEOM(1,3))* (GEOM(2,2)-GEOM(2,4))))

      GAMMA(4) = (GEOM(1,3)* (GEOM(2,1)-GEOM(2,2))+
     &           GEOM(1,1)* (GEOM(2,2)-GEOM(2,3))+
     &           GEOM(1,2)* (GEOM(2,3)-GEOM(2,1)))/
     &           (2* (((GEOM(1,2)-GEOM(1,4))* (GEOM(2,1)-GEOM(2,
     &           3)))- (GEOM(1,1)-GEOM(1,3))* (GEOM(2,2)-GEOM(2,4))))


C - CALCUL POUR LE POINT DE GAUSS CENTRAL
      KPG = 1



C - CALCUL DES ELEMENTS GEOMETRIQUES

C   CALCUL DE DFDI,R(EN AXI) ET POIDS

C      CALL NMGEOM(2,NNO,AXI,GRAND,GEOM,KPG,POIDSG(KPG),VFF(1,KPG),DFDE,
C     &            DUM,DFDK,DEPBID,POIDS,DFDI,F,EPS,R)
      CALL NMGEOM(2, NNO, AXI, GRAND, GEOM, KPG, IPOIDS, IVF,
     &                  IDFDE, DUM, POIDS, DFDI, F, EPS, R)


C    OPERATEUR DE GRADIENT AU CENTRE
      DO 90 N = 1,NNO
        DO 80 I = 1,2
          DEFC(1,N,I) = F(I,1)*DFDI(N,1)
          DEFC(2,N,I) = F(I,2)*DFDI(N,2)
          DEFC(3,N,I) = 0.D0
          DEFC(4,N,I) = (F(I,1)*DFDI(N,2)+F(I,2)*DFDI(N,1))/RAC2
   80   CONTINUE
   90 CONTINUE



C - CALCUL DE LA FORCE INTERIEURE ET DES CONTRAINTES DE CAUCHY

C    INITIALISATION
      NPGS = 4

C    CONTRAINTES GENERALISEES
      DO 180 I = 1,6
        QPLUS(I) = SIGM(I+4,KPG)
 180  CONTINUE



C    OPERATEUR DE STABILISATION DU GRADIENT AU 4 POINTS DE GAUSS
      DO 290 KPGS = 1,NPGS


        CALL DFDA2D(KPGS,NNO,POI2SG(KPGS),SDFDE,SDFDK,SDEDX,SDEDY,
     &              SDKDX,SDKDY,SDFDX,SDFDY,GEOM,JAC)

        DH(2*KPGS-1) = COOPG(2*KPGS-1)*SDKDX(KPGS) +
     &                 COOPG(2*KPGS)*SDEDX(KPGS)
        DH(2*KPGS) = COOPG(2*KPGS-1)*SDKDY(KPGS) +
     &               COOPG(2*KPGS)*SDEDY(KPGS)


        DO 220 N = 1,NNO
          DO 210 I = 1,2

C         QUAS4 SANS PROJECTION
C         ---------------------
            IF (PROJ.EQ.0) THEN
              DEFN(1,N,I) = F(I,1)*GAMMA(N)*DH(2*KPGS-1)
              DEFN(2,N,I) = F(I,2)*GAMMA(N)*DH(2*KPGS)
              DEFN(3,N,I) = 0.D0
              DEFN(4,N,I) = (F(I,1)*GAMMA(N)*DH(2*KPGS)+
     &                      F(I,2)*GAMMA(N)*DH(2*KPGS-1))

C       OPTIMAL BENDING
C       ---------------
            ELSE IF (PROJ.EQ.1) THEN
              DEFN(1,N,I) = F(I,1)*GAMMA(N)*DH(2*KPGS-1)
              DEFN(2,N,I) = F(I,2)*GAMMA(N)*DH(2*KPGS)
              DEFN(3,N,I) = 0.D0
              DEFN(4,N,I) = 0.D0

C       INCOMPRESSIBLE
C       --------------
            ELSE IF (PROJ.EQ.2) THEN
              DEFN(1,N,I) = F(I,1)*GAMMA(N)*DH(2*KPGS-1)* (0.5D0) +
     &                      F(I,2)*GAMMA(N)*DH(2*KPGS)* (-0.5D0)
              DEFN(2,N,I) = F(I,2)*GAMMA(N)*DH(2*KPGS)*0.5D0 +
     &                      F(I,1)*GAMMA(N)*DH(2*KPGS-1)* (-0.5D0)
              DEFN(3,N,I) = 0.D0
              DEFN(4,N,I) = 0.D0
            END IF

  210     CONTINUE
  220   CONTINUE


C    CONTRAINTES DE HOURGLASS

C       QUAS4 SANS PROJECTION
C       ---------------------
        IF (PROJ.EQ.0) THEN
          SIGAS(1,KPGS) = QPLUS(1)*DH(2*KPGS-1) + QPLUS(2)*DH(2*KPGS)
          SIGAS(2,KPGS) = QPLUS(3)*DH(2*KPGS-1) + QPLUS(4)*DH(2*KPGS)
          SIGAS(3,KPGS) = 0.D0
          SIGAS(4,KPGS) = (QPLUS(5)*DH(2*KPGS)+QPLUS(6)*DH(2*KPGS-1))/2

C       OPTIMAL BENDING
C       ---------------
        ELSE IF (PROJ.EQ.1) THEN
          SIGAS(1,KPGS) = QPLUS(1)*DH(2*KPGS-1) + QPLUS(2)*DH(2*KPGS)
          SIGAS(2,KPGS) = QPLUS(3)*DH(2*KPGS-1) + QPLUS(4)*DH(2*KPGS)
          SIGAS(3,KPGS) = 0.D0
          SIGAS(4,KPGS) = 0.D0

C       INCOMPRESSIBLE
C       --------------
        ELSE IF (PROJ.EQ.2) THEN
          SIGAS(1,KPGS) = (QPLUS(1)*DH(2*KPGS-1)+QPLUS(2)*DH(2*KPGS))
          SIGAS(2,KPGS) = (QPLUS(3)*DH(2*KPGS-1)+QPLUS(4)*DH(2*KPGS))
          SIGAS(3,KPGS) = 0.D0
          SIGAS(4,KPGS) = 0.D0
        END IF



C   CALCUL DES FORCES INTERNES

        DO 250 N = 1,NNO
          DO 240 I = 1,2
            DO 230 KL = 1,3
              VECTU(I,N) = VECTU(I,N) + DEFC(KL,N,I)*SIGAS(KL,KPGS)*
     &                     JAC + DEFN(KL,N,I)*SIGAS(KL,KPGS)*JAC
  230       CONTINUE
            VECTU(I,N) = VECTU(I,N) + DEFC(4,N,I)*SIGAS(4,KPGS)*JAC*
     &                   RAC2 + DEFN(4,N,I)*SIGAS(4,KPGS)*JAC
  240     CONTINUE
  250   CONTINUE

        DO 280 N = 1,NNO
          DO 270 I = 1,2
            DO 260 KL = 1,3
              VECTU(I,N) = VECTU(I,N) + DEFC(KL,N,I)*SIGM(KL,KPG)*JAC +
     &                     DEFN(KL,N,I)*SIGM(KL,KPG)*JAC
  260       CONTINUE
            VECTU(I,N) = VECTU(I,N) + DEFC(4,N,I)*SIGM(4,KPG)*RAC2*JAC +
     &                   DEFN(4,N,I)*SIGM(4,KPG)*JAC
  270     CONTINUE
  280   CONTINUE
  290 CONTINUE


      END
