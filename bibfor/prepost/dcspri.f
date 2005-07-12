      SUBROUTINE DCSPRI(COORP,COORI,SPRIM)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 11/07/2005   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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

C*******************************************************************
C              BUT DE CETTE ROUTINE :                              *
C CALCUL DE LA SURFACE D UN TRIANGLE                               *
C*******************************************************************

C IN   COOR*  : COORDONNEES DES NOEUDS DU TRIANGLE
C OUT  SPRIM  : AIRE DU TRIANGLE

      IMPLICIT NONE 

C DECLARATION GLOBALE

      REAL*8  COORP(2),COORI(2,2),SPRIM

C DECLARATION LOCALE

      REAL*8  XC,YC,XI,YI,XJ,YJ

      XC = COORP(1)
      YC = COORP(2)
      XI = COORI(1,1)
      YI = COORI(2,1)
      XJ = COORI(1,2)
      YJ = COORI(2,2)

      SPRIM=ABS((XJ-XI)*(YC-YI)-(YJ-YI)*(XC-XI))
      SPRIM=SPRIM/2.D+0
      
      END
