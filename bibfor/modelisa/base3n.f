      SUBROUTINE BASE3N(X1,MAT33)
      IMPLICIT NONE
      REAL*8 X1(3),MAT33(3,3)
      REAL*8 NORME
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 13/04/2010   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE PELLET
C ======================================================================
C     BUT : CALCULE UNE MATRICE ORTHORMEE MAT33 DONT LE 1ER VECTEUR
C           EST COLINEAIRE A X1
C     REMARQUE : IL EXISTE DE NOMBREUSES BASES AYANT CETTE PROPRIETE.
C ======================================================================
      REAL*8 V1(3),V2(3),V3(3)
      INTEGER K,K1


C     -- CALCUL DE V1 :
      DO 10,K=1,3
        V1(K)=X1(K)
   10 CONTINUE
      CALL NORMEV(V1,NORME)


C     -- CALCUL DE V2 :
C     -- ON CHERCHE UNE COMPOSANTE (K1) PAS TROP PETITE DANS V1 :
      DO 20,K=1,3
        IF (ABS(V1(K)).GE.0.5D0)K1=K
   20 CONTINUE

      IF (K1.EQ.1) THEN
        V2(2)=1.D0
        V2(3)=0.D0
        V2(1)=-V1(2)
      ELSEIF (K1.EQ.2) THEN
        V2(3)=1.D0
        V2(1)=0.D0
        V2(2)=-V1(3)
      ELSEIF (K1.EQ.3) THEN
        V2(1)=1.D0
        V2(2)=0.D0
        V2(3)=-V1(1)
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
      CALL NORMEV(V2,NORME)


C     -- CALCUL DE V3 :
      CALL PROVEC(V1,V2,V3)
      CALL NORMEV(V3,NORME)


C     -- RECOPIE DE V1, V2, V3 DANS MAT33 :
      DO 30,K=1,3
        MAT33(K,1)=V1(K)
        MAT33(K,2)=V2(K)
        MAT33(K,3)=V3(K)
   30 CONTINUE


      END
