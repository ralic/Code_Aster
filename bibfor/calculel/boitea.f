      SUBROUTINE BOITEA(CNOEUD,NOAR  ,PAAR  ,NAREQ ,NPAN  ,
     &                  DIME  ,MINMAX,PAN)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 09/01/2007   AUTEUR ABBAS M.ABBAS 
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
C                                                                       
C                                                                       
C ======================================================================
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE    
      INTEGER DIME
      REAL*8  CNOEUD(DIME,*)      
      INTEGER NOAR(3,*)
      INTEGER PAAR(DIME-1,*)
      INTEGER NAREQ
      INTEGER NPAN
      REAL*8  MINMAX(2,*)
      REAL*8  PAN(DIME+2,*)      
C      
C ----------------------------------------------------------------------
C
C CONSTRUCTION DE BOITES ENGLOBANTES POUR UN GROUPE DE MAILLES
C
C CORRECTION DES BOITES PRENANT EN COMPTE LES ARETES QUADRATIQUES
C 
C
C ----------------------------------------------------------------------
C
C
C IN  CNOEUD : COORD. NOEUDS DE LA MAILLE 
C               DIM: (DIME,*)   (X1, [Y1, Z1], X2, ...)
C IN  NOAR   : INDICES DES NOEUDS DES ARETES QUADRATIQUES (CF NOAREQ)
C               DIM: (3,*)
C IN  PAAR   : INDICES DES PANS TOUCHANT LES ARETES QUADR. (CF PANARQ)
C               DIM: (DIME-1,*)
C IN  NAREQ  : NOMBRE D'ARETES QUADRATIQUES
C IN  NPAN   : NOMBRE DE PANS DE LA MAILLE
C IN  DIME   : DIMENSION DE L'ESPACE
C I/O MINMAX : BOITE ENGLOBANT LA MAILLE SUIVANT X,Y,[Z]
C               DIM: (2,DIME) 
C I/O PAN    : EQUATION DES PANS DU CONVEXE ENGLOBANT
C              ET INSCRIT DE LA MAILLE (CF BOITE)
C               DIM: (DIME+2,*) 
C
C ---------------------------------------------------------------------
C
      REAL*8  DDOT
      INTEGER K1,K2,K3
      INTEGER IARE,IDIME,IPAN
      REAL*8  R,R1,R2,R3,X1(3),X2(3),X3(3)
C
C ----------------------------------------------------------------------
C
      DO 10 IARE = 1, NAREQ
        K1 = NOAR(1,IARE)
        K2 = NOAR(2,IARE)
        K3 = NOAR(3,IARE)  
        DO 20 IDIME = 1, DIME
          R1 = CNOEUD(IDIME,K1)
          R2 = CNOEUD(IDIME,K2)
          R3 = CNOEUD(IDIME,K3)
          X1(IDIME) = R3
          X2(IDIME) = R2 - R1
          X3(IDIME) = R1 + R2 - 2*R3
 20     CONTINUE
C
C --- BOITE MINMAX
C
        DO 30 IDIME = 1, DIME
          IF (ABS(X2(IDIME)).LT.(2.D0*ABS(X3(IDIME)))) THEN
            R = X1(IDIME) - 0.125D0*X2(IDIME)*X2(IDIME)/X3(IDIME)
            IF (R.LT.MINMAX(1,IDIME)) THEN
              MINMAX(1,IDIME) = R
            ELSEIF (R.GT.MINMAX(2,IDIME)) THEN
              MINMAX(2,IDIME) = R
            ENDIF
          ENDIF
 30     CONTINUE
C
C --- PANS CONVEXE ENGLOBANT
C
        K2 = 1 + DIME
        DO 40 IPAN = 1, NPAN
          R2 = DDOT(DIME,PAN(1,IPAN),1,X2,1)
          R3 = DDOT(DIME,PAN(1,IPAN),1,X3,1)
          IF (ABS(R2).LT.(2.D0*ABS(R3))) THEN
            R = 0.125D0*R2*R2/R3 - DDOT(DIME,PAN(1,IPAN),1,X1,1)
            IF (R.LT.PAN(K2,IPAN)) THEN
              PAN(K2,IPAN) = R
            ENDIF  
          ENDIF
 40     CONTINUE
C
C --- PANS CONVEXE INSCRIT
C
        K2 = 2 + DIME
        DO 50 IDIME = 1, DIME-1
          K3 = PAAR(IDIME,IARE)
          R2 = DDOT(DIME,PAN(1,K3),1,X2,1)
          R3 = DDOT(DIME,PAN(1,K3),1,X3,1)
          IF (ABS(R2).LT.(2.D0*ABS(R3))) THEN
            R = 0.125D0*R2*R2/R3 - DDOT(DIME,PAN(1,K3),1,X1,1)
            IF (R.GT.PAN(K2,K3)) PAN(K2,K3) = R
          ENDIF
 50     CONTINUE         
 10   CONTINUE
        
      END
