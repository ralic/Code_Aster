        SUBROUTINE  MMNEWT(ALIAS,NNO,NDIM,GEOM,
     &                     ITEMAX,EPSMAX,
     &                     XI,YI,TAU1,TAU2,NIVERR)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 05/09/2006   AUTEUR MABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER*8  ALIAS      
      INTEGER      NNO
      INTEGER      NDIM
      REAL*8       GEOM(30)
      REAL*8       XI
      REAL*8       YI
      REAL*8       TAU1(3)
      REAL*8       TAU2(3)
      INTEGER      NIVERR
      INTEGER      ITEMAX
      REAL*8       EPSMAX
C
C ----------------------------------------------------------------------
C ROUTINE APPELLEE PAR : 
C ----------------------------------------------------------------------
C
C ALGORITHME DE NEWTON POUR CALCULER LA PROJECTION D'UN POINT SUR UNE
C MAILLE - VERSION GENERALE
C
C IN  NOMA   : NOM DU MAILLAGE
C IN  NUMA   : NUMERO ABSOLU DE LA MAILLE 
C IN  ALIAS  : TYPE DE MAILLE
C IN  NNO    : NOMBRE DE NOEUD SUR LA MAILLE
C IN  NDIM   : DIMENSION DE LA MAILLE (2 OU 3)
C IN  GEOM   : COORDONNEES DU POINT ET DE L'ELEMENT
C IN  ITEMAX : NOMBRE MAXI D'ITERATIONS DE NEWTON POUR LA PROJECTION
C IN  EPSMAX : RESIDU POUR CONVERGENCE DE NEWTON POUR LA PROJECTION 
C OUT XI     : PREMIERE COORDONNEE PARAMETRIQUE DU POINT PROJETE
C OUT YI     : SECONDE COORDONNEE PARAMETRIQUE DU POINT PROJETE
C OUT TAU1   : PREMIER VECTEUR TANGENT EN XI,YI
C OUT TAU2   : SECOND VECTEUR TANGENT EN XI,YI
C OUT NIVERR : RETOURNE UN CODE ERREUR
C                0  TOUT VA BIEN
C                1  ELEMENT INCONNU
C                2  MATRICE SINGULIERE (VECTEURS TANGENTS COLINEAIRES)
C                3  DEPASSEMENT NOMBRE ITERATIONS MAX
C
C ----------------------------------------------------------------------
C
      REAL*8 TN(9),VEC1(3),DR(2,9)
      REAL*8 DDR(3,9),TANG(2,2),PAR11(3),PAR12(3),PAR22(3)
      REAL*8 RESIDU(2),EPS
      REAL*8 DER,DES,DET,TEST
      INTEGER K,I,ITER
C
C ----------------------------------------------------------------------
C
C --- POINT DE DEPART
C
      NIVERR = 0
      XI     = 0.D0
      YI     = 0.D0
      ITER   = 0
C
C --- DEBUT DE LA BOUCLE
C      
 20   CONTINUE 
C
C --- INITIALISATIONS
C 
        DO 10 I  = 1,3
          VEC1(I)  = 0.D00
          TAU1(I)  = 0.D00
          TAU2(I)  = 0.D00
          PAR11(I) = 0.D00
          PAR12(I) = 0.D00
          PAR22(I) = 0.D00
 10     CONTINUE

        RESIDU(1) = 0.D00
        RESIDU(2) = 0.D00

        TANG(1,1) = 0.D0
        TANG(1,2) = 0.D0
        TANG(2,1) = 0.D0
        TANG(2,2) = 0.D0

C  CALCUL DES FONCTIONS DE FORME ET LEURS DERIVEES

        CALL CALFFD(ALIAS,XI,YI,TN,DR,DDR,NIVERR)             
        IF (NIVERR.NE.0) THEN
          GOTO 999
        ENDIF


C  CALCUL DU RESIDU

        DO 40 K = 1,NDIM
          DO 30 I = 1,NNO
            VEC1(K)  = GEOM(3*I+K)*TN(I) + VEC1(K)
            TAU1(K)  = GEOM(3*I+K)*DR(1,I) + TAU1(K)
            PAR11(K) = GEOM(3*I+K)*DDR(1,I) + PAR11(K)
            IF (NDIM.EQ.3) THEN
              TAU2(K)  = GEOM(3*I+K)*DR(2,I) + TAU2(K)
              PAR22(K) = GEOM(3*I+K)*DDR(2,I) + PAR22(K)
              PAR12(K) = GEOM(3*I+K)*DDR(3,I) + PAR12(K)
            ENDIF  
 30      CONTINUE
 40     CONTINUE

        DO 35 K = 1,NDIM
          VEC1(K) = GEOM(K) - VEC1(K)
 35     CONTINUE

        DO 50 I = 1,3
          RESIDU(1) = VEC1(I)*TAU1(I) + RESIDU(1)
          IF (NDIM.EQ.3) THEN
            RESIDU(2) = VEC1(I)*TAU2(I) + RESIDU(2)
          ENDIF  
   50   CONTINUE

C  CALCUL DE LA MATRICE

        DO 60 K = 1,NDIM
          TANG(1,1) = -TAU1(K)*TAU1(K) + PAR11(K)*VEC1(K) + TANG(1,1)
          IF (NDIM.EQ.3) THEN
            TANG(1,2) = -TAU2(K)*TAU1(K) + PAR12(K)*VEC1(K) + TANG(1,2)
            TANG(2,1) = -TAU1(K)*TAU2(K) + PAR12(K)*VEC1(K) + TANG(2,1)
            TANG(2,2) = -TAU2(K)*TAU2(K) + PAR22(K)*VEC1(K) + TANG(2,2)
          ENDIF  
   60   CONTINUE

C   L'ALGORITHME DE NEWTON

        IF (NDIM.EQ.2) THEN
          DET = TANG(1,1)
        ELSE IF (NDIM.EQ.3) THEN
          DET = TANG(1,1)*TANG(2,2) - TANG(1,2)*TANG(2,1) 
        ENDIF

        IF (DET.EQ.0.D0) THEN
          NIVERR = 2
          GOTO 999
        END IF

        IF (NDIM.EQ.3) THEN
          DER = (TANG(2,2)* (-RESIDU(1))-TANG(1,2)* (-RESIDU(2)))/DET
          DES = (TANG(1,1)* (-RESIDU(2))-TANG(2,1)* (-RESIDU(1)))/DET
        ELSE IF (NDIM.EQ.2) THEN
          DER = -RESIDU(1)/TANG(1,1)
          DES = 0.D00
        END IF
  
C   ACTUALISATION

        XI   = XI + DER
        YI   = YI + DES         
        ITER = ITER + 1

C   TEST DE CONVERGENCE

        IF ((XI*XI+YI*YI).EQ.0.D0) THEN        
          TEST = SQRT(DER*DER+DES*DES)
          EPS  = EPSMAX
        ELSE     
          TEST = SQRT(DER*DER+DES*DES)/SQRT(XI*XI+YI*YI)
          EPS  = 100*EPSMAX
        ENDIF
        
        IF ((TEST.GT.EPS) .AND. (ITER.LT.ITEMAX)) THEN
          GOTO 20
        ELSEIF ((ITER.GT.ITEMAX).AND.(TEST.GT.EPS)) THEN
          NIVERR = 3
        ENDIF  
C
C --- FIN DE LA BOUCLE
C 

      
  999 CONTINUE    
      
      END
