       SUBROUTINE DIAGO2(TENS,VECP,VALP)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 04/10/2004   AUTEUR GODARD V.GODARD 
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

       IMPLICIT NONE
       REAL*8            TENS(3),VECP(2,2),VALP(2)

C ----------------------------------------------------------------------
C  DIAGONALISATION MATRICE 2x2 SYMETRIQUE PAR UNE METHODE DIRECTE
C    IN    TENS   : TENSEUR SOUS LA FORME
C                     (XX YY XY)
C ----------------------------------------------------------------------


       REAL*8            A1(3),DELTA
       REAL*8            TOTO,TITI,TUTU,TATA


         IF (TENS(3).EQ.0.D0) THEN
           VALP(1)=TENS(1)
           VALP(2)=TENS(2)

           VECP(1,1)=1.D0
           VECP(2,1)=0.D0
           VECP(1,2)=0.D0
           VECP(2,2)=1.D0

         ELSE

         DELTA=(TENS(1)+TENS(2))**2-4.D0*(TENS(1)*TENS(2)-TENS(3)**2)
         VALP(1)=((TENS(1)+TENS(2))+SQRT(DELTA))/2.D0
         VALP(2)=((TENS(1)+TENS(2))-SQRT(DELTA))/2.D0

         A1(1)=TENS(1)-VALP(1)      
         A1(2)=TENS(2)-VALP(1)
         A1(3)=TENS(3)

         TATA=SQRT(A1(3)**2+A1(1)**2)

         VECP(1,1)=-A1(3)/TATA
         VECP(2,1)=A1(1)/TATA
         VECP(1,2)=-A1(1)/TATA
         VECP(2,2)=-A1(3)/TATA


         ENDIF
      END
