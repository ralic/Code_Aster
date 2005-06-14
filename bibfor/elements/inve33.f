      SUBROUTINE INVE33(AA,USA,DET,LRET)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/09/2003   AUTEUR JMBHH01 J.M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C--------------------------------------------------------
C ELEMENT SHB8-PS A.COMBESCURE, S.BAGUET INSA LYON 2003 /   
C-------------------------------------------------------

C    CALCUL DE L'INVERSE D'UNE MATRICE 3*3  (METHODE DES MINEURS)

C   ENTREE :  AA(3,3)
C   SORTIE :  USA(3,3)  ET DET , LRET (=0 OK, =1 ERREUR)
C
      IMPLICIT NONE

      REAL *8 AA(3,3),USA(3,3),DET
      INTEGER LRET
      REAL *8 PRECIS,GRAND,CC1,CC2,CC3,UNSDET
      INTEGER I

      DATA PRECIS/1D-40/,GRAND/1D+40/

C---    CALCUL DU DETERMINANT

      CC1 =   AA(2,2)*AA(3,3) - AA(3,2)*AA(2,3)
      CC2 = - AA(1,2)*AA(3,3) + AA(3,2)*AA(1,3)
      CC3 =   AA(1,2)*AA(2,3) - AA(2,2)*AA(1,3)
      DET = AA(1,1)*CC1 + AA(2,1)*CC2 + AA(3,1)*CC3

      IF(ABS(DET).GT.PRECIS) THEN
         LRET=0
      ELSE
         LRET=1
         DET=0
         DO 10 I=1,9
            USA(I,1) = GRAND
10       CONTINUE
         GOTO 9
      ENDIF
      UNSDET=1D0/DET

      USA(1,1)=   CC1 * UNSDET
      USA(1,2)=   CC2 * UNSDET
      USA(1,3)=   CC3 * UNSDET
      USA(2,1)= ( - AA(2,1)*AA(3,3) + AA(3,1)*AA(2,3) ) *  UNSDET
      USA(2,2)= (   AA(1,1)*AA(3,3) - AA(3,1)*AA(1,3) ) *  UNSDET
      USA(2,3)= ( - AA(1,1)*AA(2,3) + AA(2,1)*AA(1,3) ) *  UNSDET
      USA(3,1)= (   AA(2,1)*AA(3,2) - AA(3,1)*AA(2,2) ) *  UNSDET
      USA(3,2)= ( - AA(1,1)*AA(3,2) + AA(3,1)*AA(1,2) ) *  UNSDET
      USA(3,3)= (   AA(1,1)*AA(2,2) - AA(2,1)*AA(1,2) ) *  UNSDET

  9   CONTINUE

      END
