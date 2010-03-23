      SUBROUTINE VFCDET(MAXDIM,NDIM,A,DETA)
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
       IMPLICIT NONE
       INTEGER MAXDIM,NDIM
       REAL*8 A(1:MAXDIM,1:MAXDIM),DETA
       
C    DETERMINANT CALCULÉ PAR LA REGLE DE SARRUS

       IF (NDIM.EQ.2) THEN
          DETA=A(1,1)*A(2,2)-A(1,2)*A(2,1)
        ELSE IF (NDIM.EQ.3) THEN 
       


          DETA =  ( A(1,1)*A(2,2)*A(3,3)
     +              + A(2,1)*A(3,2)*A(1,3)
     +              + A(3,1)*A(1,2)*A(2,3)
     +              - A(3,1)*A(2,2)*A(1,3)
     +              - A(2,1)*A(1,2)*A(3,3)
     +              - A(1,1)*A(3,2)*A(2,3) )
       ELSE 
       
        CALL ASSERT(.FALSE.)
        END IF
      END
