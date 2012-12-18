      SUBROUTINE TBAJVR(TABLE,NBPARA,NOMPAR,VR,LIVR)
      IMPLICIT NONE
      INTEGER NBPARA
      REAL*8 VR,LIVR(*)
      CHARACTER*(*) TABLE,NOMPAR
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 17/12/2012   AUTEUR DELMAS J.DELMAS 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C
C     BUT:
C       ROUTINE CHAPEAU D'APPEL A TBAJVA POUR NE PASSER QUE DES REELS
C
C ----------------------------------------------------------------------
C
      INTEGER     VI,LIVI(1)
C
      CHARACTER*8 VK,LIVK(1)
C
      COMPLEX*16  VC,LIVC(1)
C
C ----------------------------------------------------------------------
C
      VI      = 0
      LIVI(1) = 0
      VC      = DCMPLX(0.D0,0.D0)
      LIVC(1) = DCMPLX(0.D0,0.D0)
      VK      = ' '
      LIVK(1) = ' '
C
      CALL TBAJVA(TABLE,NBPARA,NOMPAR,VI,LIVI,VR,LIVR,VC,LIVC,VK,LIVK)
C
      END
