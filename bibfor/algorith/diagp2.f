      SUBROUTINE DIAGP2(TENS,VECP,VALP)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 12/04/2005   AUTEUR PBADEL P.BADEL 
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

      IMPLICIT NONE
      REAL*8   TENS(3),VALP(2),VECP(2,2)
      
C ----------------------------------------------------------------------
C  DIAGONALISATION MATRICE 3x3 SYMETRIQUE PAR UNE METHODE DIRECTE
C    IN    TENS   : TENSEUR SOUS LA FORME
C                     (XX XY YY)
C ----------------------------------------------------------------------
      
      INTEGER I,J,NRAC,IND
      LOGICAL INVVP
      REAL*8  TRACE,X(3),Y(3),DET(4),D12,D13,D23,NORM(3)
      REAL*8  A,B,C,R8PI,THETA,R8MIEM
      REAL*8  F,G
      REAL*8  TPS(6)
      
      CALL R8INIR(4,0.D0,VECP,1)

C -- PASSAGE AU DEVIATEUR
      TRACE=(TENS(1)+TENS(3))/2.D0
      
      TENS(1)=TENS(1)-TRACE
      TENS(3)=TENS(3)-TRACE

C -- CALCUL DES COEFFICIENTS DU POLYNOME P3   
      
      DET(1)=TENS(1)+TENS(3)
      DET(2)=TENS(1)*TENS(3)-TENS(2)**2
      
      CALL ZEROP2(-DET(1),DET(2),VALP,NRAC)
            
C -- VECP DE LA 1ERE VALEUR PROPRE
C -- ON MULTIPLIE LES 2 VECT DE BASE PAR (A-LAMBDA_2.ID)
C      ON PRENDRA CELUI DONT LA NORME EST LA PLUS GRANDE
      DO 4000 IND=1,2
        IF (IND.EQ.1) THEN
          VECP(1,IND)= TENS(1)-VALP(2)      
          VECP(2,IND)= TENS(2)      
        ELSE IF (IND.EQ.2) THEN
          VECP(1,IND)= TENS(2)
          VECP(2,IND)= TENS(3)-VALP(2)
        ENDIF
        Y(IND)=(VECP(1,IND))**2+(VECP(2,IND))**2
4000  CONTINUE
      IND=1
      IF (Y(2).GT.Y(1)) THEN
        IND=2
      ENDIF
      A=SQRT(Y(IND))
C -- CAS DE 2 VALEURS PROPRES EGALES      
      IF (A.LT.R8MIEM()) THEN
        CALL R8INIR(4,0.D0,VECP,1)
        VECP(1,1)=1.D0
        VECP(2,2)=1.D0
        GOTO 9999
      ENDIF
      DO 4010 I=1,2
        VECP(I,1)=VECP(I,IND)/A
4010  CONTINUE

      VECP(1,2)=-VECP(2,1)
      VECP(2,2)=VECP(1,1)
      
      DO 200 I=1,2
        VALP(I)=VALP(I)+TRACE
200   CONTINUE
       
      
9999  CONTINUE

      END
