        SUBROUTINE LCDEVI ( A , D )
        IMPLICIT REAL*8 (A-H,O-Z)
C       ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 27/03/2002   AUTEUR VABHHTS J.PELLET 
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
C       ----------------------------------------------------------------
C       DEVIATEUR D UN TENSEUR (3X3) SOUS FORME VECTEUR  (6X1)
C       IN  A      :  TENSEUR
C       OUT D      :  DEVIATEUR DE A = A - 1/3 TR(A) I
C       ----------------------------------------------------------------
        INTEGER         N , ND
        REAL*8          A(6) , D(6) , TA , D13
        COMMON /TDIM/   N , ND
C
        DATA   D13      /.33333333333333D0 /
C
        TA = 0.D0
        DO 1 I = 1,ND
        TA   = TA + A(I)
 1      CONTINUE
        TA   = TA * D13
        DO 2 I = 1,ND
        D(I) = A(I) - TA
 2      CONTINUE
        DO 3 I = ND + 1 , N
        D(I) = A(I)
 3      CONTINUE
        END
