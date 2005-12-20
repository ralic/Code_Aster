      SUBROUTINE MATINV(NDIM,MAT,INV)
      IMPLICIT NONE
      INTEGER    NDIM
      REAL*8     MAT(NDIM,NDIM),INV(NDIM,NDIM)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 30/03/2004   AUTEUR CIBHHLV L.VIVAN 
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
C ----------------------------------------------------------------------
C FONCTION REALISEE:  CALCUL DE L'INVERSE D'UNE MATRICE
C                    
C    ENTREE:
C         NDIM : DIMENSION DE LA MATRICE
C         MAT  : MATRICE A INVERSER
C    SORTIE:
C         INV  : MATRICE INVERSE
C.......................................................................
      INTEGER       I,J
      REAL*8        M(3,3),DET,R8GAEM

      IF (NDIM.EQ.3) THEN
C       CALCUL DES (-1)^(I+J)*MINEURS(J,I)
        M(1,1) = MAT(2,2) * MAT(3,3) - MAT(2,3) * MAT(3,2)
        M(2,1) = MAT(3,1) * MAT(2,3) - MAT(2,1) * MAT(3,3)
        M(3,1) = MAT(2,1) * MAT(3,2) - MAT(3,1) * MAT(2,2)
        M(1,2) = MAT(1,3) * MAT(3,2) - MAT(1,2) * MAT(3,3)
        M(2,2) = MAT(1,1) * MAT(3,3) - MAT(1,3) * MAT(3,1)
        M(3,2) = MAT(1,2) * MAT(3,1) - MAT(3,2) * MAT(1,1)
        M(1,3) = MAT(1,2) * MAT(2,3) - MAT(1,3) * MAT(2,2)
        M(2,3) = MAT(2,1) * MAT(1,3) - MAT(2,3) * MAT(1,1)
        M(3,3) = MAT(1,1) * MAT(2,2) - MAT(1,2) * MAT(2,1)
C       CALCUL DU DETERMINANT
        DET = MAT(1,1)*M(1,1) + MAT(1,2)*M(2,1) + MAT(1,3)*M(3,1)
        IF(ABS(DET).LE.1.D0/R8GAEM()) THEN
          CALL UTMESS('F','MATINV','MATRICE SINGULIERE')
        ENDIF
C       CALCUL DE L'INVERSE
        DO 10 I=1,NDIM
          DO 20 J=1,NDIM
            INV(I,J)=M(I,J)/DET
 20       CONTINUE
 10     CONTINUE
      ELSE
        CALL UTMESS('F','MATINV','INVERSION SEULEMENT EN DIMENSION 3')
      ENDIF
       
      END
