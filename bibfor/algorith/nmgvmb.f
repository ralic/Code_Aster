      SUBROUTINE NMGVMB(NDIM,NNO1,NNO2,NPG,AXI,GEOM,VFF1,VFF2,IDFDE1,
     &                  IDFDE2,IW,NDDL,NEPS,B,W,NI2LDC)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/03/2012   AUTEUR PROIX J-M.PROIX 
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

      IMPLICIT NONE
      LOGICAL  AXI
      INTEGER  NDIM,NNO1,NNO2,NPG,IDFDE1,IDFDE2,IW
      REAL*8   GEOM(NDIM,NNO1),VFF1(NNO1,NPG),VFF2(NNO2,NPG)
      INTEGER  NDDL,NEPS
      REAL*8   B(3*NDIM+2,NPG,*)       
      REAL*8   W(NPG),NI2LDC(3*NDIM+2)
C ----------------------------------------------------------------------
C  CALCUL DES ELEMENTS CINEMATIQUES POUR LA MODELISATION GRAD_VARI
C ----------------------------------------------------------------------
C IN  NDIM   DIMENSION DE L'ESPACE
C IN  NNO1   NOMBRE DE NOEUDS TOTAL (SUPPORT DES DEPLACEMENTS)
C IN  NNO2   NOMBRE DE NOEUDS SOMMETS (SUPPORT DE VI ET LAGRANGE)
C IN  NPG    NOMBRE DE POINTS DE GAUSS
C IN  AXI    .TRUE. SI MODELISATION AXIS
C IN  GEOM   COORDONNEES DES NOEUDS
C IN  VFF1   VALEUR DE LA FAMILLE DE FONCTIONS DE FORME NO 1
C IN  VFF2   VALEUR DE LA FAMILLE DE FONCTIONS DE FORME NO 2
C IN  IDFDE1 POINTEUR SUR LES DER. REFERENCE FAMILLE FCT FORME NO 1
C IN  IDFDE2 POINTEUR SUR LES DER. REFERENCE FAMILLE FCT FORME NO 2
C IN  IW     POINTEUR SUR LES POIDS DES PTS DE GAUSS DE REFERENCE
C OUT NDDL   NOMBRE DE DDL / ELEMENT
C OUT NEPS   NBR DE COMPOSANTE DE DEFORMATION (GENERALISEE)
C OUT B      MATRICE CINEMATIQUE EPS = B.U
C OUT W      POIDS DES POINTS DE GAUSS REELS
C OUT NI2LDC CONVERSION CONTRAINTE STOCKEE -> CONTRAINTE LDC (AVEC RAC2)
C ----------------------------------------------------------------------
      INTEGER G
      REAL*8  RAC2,R2,R,DFDI1(27*3),DFDI2(8*3),UNSURR
C ----------------------------------------------------------------------
      INTEGER IU1,IU2,IA,IL,N,I
      REAL*8  DFF1,DFF2
      IU1(N,I)  = (N-1)*(NDIM+2) + I
      IU2(N,I)  = NNO2*2 + (N-1)*NDIM + I
      IA(N)     = (N-1)*(NDIM+2) + NDIM + 1
      IL(N)     = (N-1)*(NDIM+2) + NDIM + 2
      DFF1(N,I) = DFDI1(NNO1*(I-1) + N)
      DFF2(N,I) = DFDI2(NNO2*(I-1) + N)
C ----------------------------------------------------------------------
      CALL ASSERT(NNO1.LE.27)
      CALL ASSERT(NNO2.LE.8)
      RAC2 = SQRT(2.D0)
      R2   = SQRT(2.D0)/2
      NDDL  = NNO1*NDIM + NNO2*2
      NEPS  = 3*NDIM + 2 
      CALL R8INIR(NEPS*NPG*NDDL,0.D0,B,1)

C - AFFECTATION DE LA FONCTION DE TRANSFERT SIGMA NICE --> SIGMA LDC

      CALL R8INIR(NEPS,1.D0,NI2LDC,1)
      DO 5 I = 4,2*NDIM
        NI2LDC(I) = RAC2
 5    CONTINUE
      

C - AFFECTATION DE LA MATRICE CINEMATIQUE B

      DO 1000 G=1,NPG
        CALL DFDMIP(NDIM,NNO2,AXI,GEOM,G,IW,VFF2(1,G),IDFDE2,R,W(G),
     &    DFDI2)
        CALL DFDMIP(NDIM,NNO1,AXI,GEOM,G,IW,VFF1(1,G),IDFDE1,R,W(G),
     &    DFDI1)
        IF (NDIM.EQ.2) THEN
          IF (AXI) THEN
            UNSURR = 1/R
          ELSE
            UNSURR = 0
          END IF

          DO 10 N = 1,NNO2
            B(1,G,IU1(N,1)) = DFF1(N,1)
            B(2,G,IU1(N,2)) = DFF1(N,2)
            B(3,G,IU1(N,1)) = VFF1(N,G)*UNSURR
            B(4,G,IU1(N,1)) = R2*DFF1(N,2)
            B(4,G,IU1(N,2)) = R2*DFF1(N,1)
            B(5,G,IA(N))    = VFF2(N,G)
            B(6,G,IL(N))    = VFF2(N,G)
            B(7,G,IA(N))    = DFF2(N,1)
            B(8,G,IA(N))    = DFF2(N,2)
 10       CONTINUE

          DO 20 N = NNO2+1,NNO1
            B(1,G,IU2(N,1)) = DFF1(N,1)
            B(2,G,IU2(N,2)) = DFF1(N,2)
            B(3,G,IU2(N,1)) = VFF1(N,G)*UNSURR
            B(4,G,IU2(N,1)) = R2*DFF1(N,2)
            B(4,G,IU2(N,2)) = R2*DFF1(N,1)
 20       CONTINUE

        ELSE IF (NDIM.EQ.3) THEN
          DO 30 N = 1,NNO2
            B(1,G,IU1(N,1)) = DFF1(N,1)
            B(2,G,IU1(N,2)) = DFF1(N,2)
            B(3,G,IU1(N,3)) = DFF1(N,3)
            B(4,G,IU1(N,1)) = R2*DFF1(N,2)
            B(4,G,IU1(N,2)) = R2*DFF1(N,1)
            B(5,G,IU1(N,1)) = R2*DFF1(N,3)
            B(5,G,IU1(N,3)) = R2*DFF1(N,1)
            B(6,G,IU1(N,2)) = R2*DFF1(N,3)
            B(6,G,IU1(N,3)) = R2*DFF1(N,2)
            B(7,G,IA(N))    = VFF2(N,G)
            B(8,G,IL(N))    = VFF2(N,G)
            B(9,G,IA(N))    = DFF2(N,1)
            B(10,G,IA(N))   = DFF2(N,2)
            B(11,G,IA(N))   = DFF2(N,3)
 30       CONTINUE

          DO 40 N = NNO2+1,NNO1
            B(1,G,IU2(N,1)) = DFF1(N,1)
            B(2,G,IU2(N,2)) = DFF1(N,2)
            B(3,G,IU2(N,3)) = DFF1(N,3)
            B(4,G,IU2(N,1)) = R2*DFF1(N,2)
            B(4,G,IU2(N,2)) = R2*DFF1(N,1)
            B(5,G,IU2(N,1)) = R2*DFF1(N,3)
            B(5,G,IU2(N,3)) = R2*DFF1(N,1)
            B(6,G,IU2(N,2)) = R2*DFF1(N,3)
            B(6,G,IU2(N,3)) = R2*DFF1(N,2)
 40       CONTINUE
        END IF
 1000 CONTINUE
      END
