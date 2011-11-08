      SUBROUTINE DEFLOG(NDIM,F,EPSL,GN,LAMB,LOGL)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 07/11/2011   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ----------------------------------------------------------------------
      IMPLICIT NONE
C     CALCUL DES DEFORMATIONS LOGARITHMIQUES ET DES TERMES NECESSAIRES
C     AU POST TRAITEMENT DES CONTRAINTES ET A LA RIGIDITE TANGENTE
C     SUIVANT ARTICLE MIEHE APEL LAMBRECHT CMAME 2002
C ----------------------------------------------------------------------
C     IN    NDIM : dimension 2 ou 3
C     IN    F gradient de la transformation calcule sur config initiale
C     OUT   EPSL  deformation logarithmique + GN,LAMB,LOGL pour POSLOG
C     OUT   GN    directions propres du tenseur F
C     OUT   LAMB  valeurs propres du tenseur F
C     OUT   LOGL  log des valeurs propres du tenseur F
C ----------------------------------------------------------------------
      REAL*8  TR(6),GN(3,3),EPSL33(3,3),TR2(3),FT(3,3),GN2(2,2)
      REAL*8  F(3,3),EPSL(6),LAMB(3),LOGL(3),F33(3,3)
      INTEGER NBVEC,I,J,K,NDIM
C ----------------------------------------------------------------------

      NBVEC = 3

C     LE CALCUL DES VALEURS PROPRES N'A PAS ENCORE ETE FAIT  
      CALL LCTR2M(3,F,FT)
      CALL PMAT(3,FT,F,F33)
C --- VALEURS PRINCIPALES = VECTEURS PROPRES
C  VECP : DIM1=I=COMPOSANTE DIM2=J=NUM VECTEUR ASSOCIE A LAMBP(J)
      CALL TNSVEC(3,3,F33,TR,1.D0)
      
      IF(NDIM.EQ.3) THEN
      
C         CALL DIAGO3(TR,GN,LAMB)
C     --------------------------------
C     pour gagner du temps
C     --------------------------------
C --- MATRICE TR = (XX XY XZ YY YZ ZZ) (POUR DIAGP3)
         TR(1) = F33(1,1)
         TR(2) = F33(1,2)
         TR(3) = F33(1,3)
         TR(4) = F33(2,2)
         TR(5) = F33(2,3)
         TR(6) = F33(3,3)
         CALL DIAGP3(TR,GN,LAMB) 

      ELSEIF(NDIM.EQ.2) THEN
      
         TR2(1)=TR(1)
         TR2(2)=TR(2)
         TR2(3)=TR(4)
         CALL DIAGO2(TR2,GN2,LAMB)
         LAMB(3)=TR(3)
         CALL R8INIR(9,0.D0,GN,1)
         DO 1 I=1,2
         DO 1 J=1,2
            GN(I,J)=GN2(I,J)
 1       CONTINUE
         GN(3,3)=1.D0
         
      ENDIF

     
      DO 10 I=1,NBVEC
         LOGL(I)=LOG(LAMB(I))*0.5D0
 10   CONTINUE
      
C   EPSL = DEFORMATION LOGARITHMIQUE
      CALL R8INIR(9,0.D0,EPSL33,1)
      CALL R8INIR(6,0.D0,EPSL,1)
      DO 11 I=1,3
         DO 12 J=1,3
            DO 13 K=1,NBVEC
C              Calcul de EPSL dans le repere general
               EPSL33(I,J)=EPSL33(I,J)+LOGL(K)*GN(I,K)*GN(J,K)
 13         CONTINUE
 12      CONTINUE
 11   CONTINUE
      CALL TNSVEC(3,3,EPSL33,EPSL,SQRT(2.D0))
      

      END
