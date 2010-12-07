      SUBROUTINE VFIMAT(MAXDIM,NDIM,
     >                  A,AM1)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/03/2010   AUTEUR ANGELINI O.ANGELINI 
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
C ======================================================================
C ======================================================================
C TOLE CRP_20
C TOLE CRP_21
C ======================================================================
     
C VARIABES IN MAXDIM:DIMENSION MAXIMALE
C         NDIM DIMENSION REELLE
C             
C         A MATRICE A INVERSER DE DIMENSION MAXDIM*MAXDIM
C VARIABLE OUT LA MATRICE INVERSE DE A 
      IMPLICIT NONE
C
      INTEGER I,J,MAXDIM,NDIM
      REAL*8 A(MAXDIM,MAXDIM)
      REAL*8 AM1(MAXDIM,MAXDIM)
      REAL*8 DETA
      CALL ASSERT(NDIM.EQ.2.OR.NDIM.EQ.3)
      CALL VFCDET(MAXDIM,NDIM,A,DETA)
      IF (NDIM.EQ.2) THEN
          AM1(1,1)=(1.D0/DETA)*(A(2,2))
          AM1(1,2)=(1.D0/DETA)*(-A(1,2))
          AM1(2,1)=(1.D0/DETA)*(-A(2,1))
          AM1(2,2)=(1.D0/DETA)*(A(1,1))
      END IF 
      IF (NDIM.EQ.3) THEN
       AM1(1,1) =(1.D0/DETA)*(A(2,2)*A(3,3)-A(2,3)*A(3,2))
       AM1(2,1) =(1.D0/DETA)*(-(A(2,1)*A(3,3)-A(2,3)*A(3,1)))
       AM1(3,1) =(1.D0/DETA)*(A(2,1)*A(3,2)-A(2,2)*A(3,1))
       AM1(1,2) =(1.D0/DETA)*(-(A(1,2)*A(3,3)-A(1,3)*A(3,2)))
       AM1(2,2) =(1.D0/DETA)*(A(1,1)*A(3,3)-A(1,3)*A(3,1))
       AM1(3,2) =(1.D0/DETA)*(-(A(1,1)*A(3,2)-A(1,2)*A(3,1)))
       AM1(1,3) =(1.D0/DETA)*(A(1,2)*A(2,3)-A(1,3)*A(2,2))
       AM1(2,3) =(1.D0/DETA)*(-(A(1,1)*A(2,3)-A(1,3)*A(2,1)))
       AM1(3,3) =(1.D0/DETA)*(A(1,1)*A(2,2)-A(1,2)*A(2,1))    
      END IF       
      END
