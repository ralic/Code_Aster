      SUBROUTINE UPLETR(NDIM, MPLE,MCOL)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 27/07/2009   AUTEUR BOYERE E.BOYERE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
      INTEGER NDIM
      REAL*8 MPLE(NDIM,NDIM), MCOL(*)
C    CE SOUS PROGRAMME STOCKE SOUS FORME PLEINE UNE MATRICE
C    TRIANGULAIRE SUPERIEURE
C
C
C    -------------------------------------------------------------------
C
C IN TYPE ! NOM    ! TABLEAU !             SIGNIFICATION
C IN -------------------------------------------------------------------
C IN  I   ! NDIM   !     -   ! TAILLE DE LA MATRICE
C IN  R*8 !  MCOL  !    -    ! MATRICE UNICOLONNE TRIANGULAIRE
C SUPERIEURE DE TAILLE NDIM*(NDIM+1)/2
C IN
C IN (+) REMARQUES :
C
C OUT TYPE ! NOM   ! TABLEAU !             SIGNIFICATION
C OUT ------------------------------------------------------------------
C OUT R*8 ! MPLE   !NDIM*NDIM! MATRICE STOCKEE SOUS FORME PLEINE
C

C     ------------------------------------------------------------------
        INTEGER I, J
C

C ---------------------------------------------------------------------
C PASSAGE DE MATRICE COLONNE VERS MAT PLEINE (COMPLETEE PAR ANTI
C SYMETRIE)
        DO 1,I = 1,NDIM
               DO 2,J = 1,I
                        MPLE(J,I)=MCOL(INT(I*(I-1)/2)+J)
                        MPLE(I,J) = - MPLE(J,I)
    2          CONTINUE
    1   CONTINUE
C
      END
