        SUBROUTINE  MMNEWD(ALIAS,NNO,NDIM,GEOM,
     &                     ITEMAX,EPSMAX,DIR,
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
      REAL*8       DIR(3)
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
C MAILLE - VERSION AVEC DIRECTION DE RECHERCHE IMPOSEE
C
C IN  ALIAS  : TYPE DE MAILLE
C IN  NNO    : NOMBRE DE NOEUD SUR LA MAILLE
C IN  NDIM   : DIMENSION DE LA MAILLE (2 OU 3)
C IN  GEOM   : COORDONNEES DU POINT ET DE L'ELEMENT
C IN  ITEMAX : NOMBRE MAXI D'ITERATIONS DE NEWTON POUR LA PROJECTION
C IN  EPSMAX : RESIDU POUR CONVERGENCE DE NEWTON POUR LA PROJECTION 
C IN  DIR    : DIRECTION D'APPARIEMENT
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
      REAL*8       TN(9),DR(2,9),ALPHA
      REAL*8       DDR(3,9),TANG(3,3),TANG2(2,2)
      REAL*8       RESIDU(3),VEC(3),DX(3)
      REAL*8       IBID,TEST
      INTEGER      I,J,ITER,IRET
C
C ----------------------------------------------------------------------
C
C
C --- POINT DE DEPART
C
      NIVERR = 0
      XI     = 0.D0
      YI     = 0.D0
      ITER   = 0
      ALPHA  = 1.D0      
C
C --- DEBUT DE LA BOUCLE
C      
 20   CONTINUE
 
C  INITIALISATIONS

        DO 10 I = 1,3
          DX(I)   = 0.D0
          VEC(I)  = 0.D0
          TAU1(I) = 0.D0
          TAU2(I) = 0.D0
   10   CONTINUE
        RESIDU(1) = 0.D0
        RESIDU(2) = 0.D0
        RESIDU(3) = 0.D0
        DO 41 I=1,3
          DO 42 J=1,3
            TANG(I,J)=0.D0
 42       CONTINUE
 41     CONTINUE

C  CALCUL DES FONCTIONS DE FORME ET LEURS DERIVEES

        CALL CALFFD(ALIAS,XI,YI,TN,DR,DDR,NIVERR)
        IF (NIVERR.NE.0) THEN
          GOTO 999
        ENDIF
        
C  CALCUL DU RESIDU

        DO 91 I = 1,3
          VEC(I)  = 0.D0
          TAU1(I) = 0.D0
          TAU2(I) = 0.D0
          DO 81 J = 1,NNO
            VEC(I)  = GEOM(3*J+I)*TN(J)   + VEC(I)
            TAU1(I) = GEOM(3*J+I)*DR(1,J) + TAU1(I)
            IF (NDIM.EQ.3) THEN
              TAU2(I) = GEOM(3*J+I)*DR(2,J) + TAU2(I)
            ENDIF
 81       CONTINUE
 91     CONTINUE
        IF (NDIM.EQ.3) THEN
          DO 25 I=1,3
            RESIDU(I)= ALPHA*DIR(I)-GEOM(I)+VEC(I)
            DX(I) = -1.D0*RESIDU(I)
 25       CONTINUE
        ELSE
          RESIDU(1)= ALPHA*DIR(1)-GEOM(1)+VEC(1)
          RESIDU(2)= ALPHA*DIR(2)-GEOM(2)+VEC(2)
          DX(1) = -1.D0*RESIDU(1)
          DX(2) = -1.D0*RESIDU(2)
        ENDIF

C  CALCUL DE LA MATRICE

        IF (NDIM.EQ.2) THEN
          DO 23 I=1,2
            TANG2(I,1)= TAU1(I)
            TANG2(I,2)= DIR(I)
 23       CONTINUE
          CALL MGAUSS('NCVP',TANG2,DX,2,2,1,IBID,IRET)        
          IF (IRET.GT.0) THEN
            NIVERR = 2
            GOTO 999            
          ENDIF  
          XI    = XI + DX(1)
          YI    = 0.D0
          ALPHA = ALPHA +DX(2)
          TEST  = SQRT(DX(1)**2+DX(2)**2)
        ELSEIF (NDIM.EQ.3) THEN
          DO 21 I=1,3
            TANG(I,1)= TAU1(I)
            TANG(I,2)= TAU2(I)
            TANG(I,3)= DIR(I)
 21       CONTINUE
          CALL MGAUSS('NCVP',TANG,DX,3,3,1,IBID,IRET)        
          IF (IRET.GT.0) THEN
            NIVERR = 2
            GOTO 999            
          ENDIF      
          XI    = XI + DX(1)
          YI    = YI + DX(2)
          ALPHA = ALPHA +DX(3)
          TEST  = SQRT(DX(1)**2+DX(2)**2+DX(3)**2)
        ENDIF

C   ACTUALISATION

        ITER = ITER + 1

C   TEST DE CONVERGENCE

        IF ((TEST.GT.EPSMAX) .AND. (ITER.LT.ITEMAX)) THEN
          GOTO 20
        ELSEIF ((ITER.GT.ITEMAX).AND.(TEST.GT.EPSMAX)) THEN
          NIVERR = 3
        ENDIF 
C
C --- FIN DE LA BOUCLE
C 

      
  999 CONTINUE    
      
      END
