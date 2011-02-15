      SUBROUTINE COEFDG (COMPOR, MAT, DPIDA2)

C            
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 15/02/2011   AUTEUR FLEJOU J-L.FLEJOU 
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
C 
      IMPLICIT NONE
      CHARACTER*16 COMPOR
      INTEGER      MAT
      REAL*8       DPIDA2, DBDPHI

C ---------------------------------------------------------------------
C     LOIS A GRADIENTS : COEFFICIENT DIAGONAL MATRICE GVNO
C ---------------------------------------------------------------------
C
C ---------------------------------------------------------------------
      
      REAL*8  VAL(1)
      CHARACTER*8 NOM(2)
      CHARACTER*2 K2(5)
C ---------------------------------------------------------------------
        
      IF (COMPOR.EQ.'ENDO_CARRE') THEN
  
        NOM(1) = 'E'
        NOM(2) = 'NU'
        CALL RCVALA(MAT,' ','ELAS'     ,0,' ',0.D0,1,NOM(1),VAL(1),
     &    K2,'F ')

        DPIDA2 = VAL(1)       
        
      ENDIF  

      END
