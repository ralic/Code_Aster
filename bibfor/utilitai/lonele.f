      SUBROUTINE LONELE (VLX,DIME,XL)
      IMPLICIT NONE
      REAL*8    VLX(7),XL
      INTEGER   DIME
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 27/06/2001   AUTEUR CIBHHPD D.NUNEZ 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C        CALCULE LA LONGEUR D'UN ELEMENT    
C     ------------------------------------------------------------------
C IN           R8 : COORDONEES DE L'ELEMENT 
C              I  : DIMENSION
C
C OUT          R8 : XL LONGUEUR DE L'ELEMENT 


C     ------------------------------------------------------------------
C

      IF (DIME.EQ.3) THEN
        XL = SQRT( (VLX(5)-VLX(2))**2
     +  + (VLX(6)-VLX(3))**2 + (VLX(7)-VLX(4))**2 )
C
      ELSE IF (DIME.EQ.2) THEN
        XL = SQRT( (VLX(4)-VLX(2))**2 + (VLX(5)-VLX(3))**2 )

C
      ENDIF
      END
