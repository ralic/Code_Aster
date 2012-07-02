      SUBROUTINE GCAX ( M , IN , IP , AC , X , Y )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C TOLE CRP_4
      IMPLICIT NONE
      INTEGER*4         IP(*)
      INTEGER           M,  IN(M)
      REAL*8                           AC(*),X(M),Y(M)
      REAL*8 DTEMP
C     ------------------------------------------------------------------
C     MULTIPLICATION D'UNE MATRICE SYMETRIQUE COMPACTE PAR
C                UN VECTEUR :  Y = AC*X
C     ------------------------------------------------------------------
C IN . M             -->   NOMBRE DE COLONNES DE LA MATRICE
C IN . IN(I=1,M)     -->   POINTEUR DE FIN DE COLONNE DE LA MATRICE
C IN . IP(J)         -->   TABLEAU DES NUMEROS DE LIGNE
C IN . AC(J)         -->   TABLEAU DES COEFFICIENTS DE LA MATRICE
C IN . X(I=1,M)      -->   VECTEUR D'ENTREE
C OUT. Y(I=1,M)     <--    VECTEUR DE SORTIE
C     _____________ ____ ______________________________________________
C-----------------------------------------------------------------------
      INTEGER I ,J ,KDEB ,KFIN ,KI ,KLONG 
C-----------------------------------------------------------------------
      Y(1) = AC(1)*X(1)
      DO 10 I = 2 , M
         KDEB = IN(I-1)+1
         KFIN = IN(I)-1
         KLONG = IN(I)
         DTEMP = 0.0D0
         DO 30 J = KDEB,KLONG
            DTEMP = DTEMP + X(IP(J))*AC(J)
30       CONTINUE
         Y(I) = DTEMP
         DTEMP = X(I)
CDIR$ IVDEP
CDIR$ NOPREFETCH Y
         DO 20 KI = KDEB , KFIN
            Y(IP(KI)) = Y(IP(KI)) + AC(KI)*DTEMP
  20     CONTINUE
 10   CONTINUE
C
      END
