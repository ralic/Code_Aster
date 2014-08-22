subroutine xfem_calc_mult(tab_mat, jadr, nm, scal, transpos)
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!-----------------------------------------------------------------------
! BUT : MIESE A L ECHELLE D UNE MATRICE A PLAT
!-----------------------------------------------------------------------
!
! ARGUMENTS :
!------------
!
!-----------------------------------------------------------------------
    implicit none
!
!  REMARQUE SUR LE STOCKAGE DES MATRICES LOCALES:
!  * EN ENTREE : LES MATRICES LOCALES SONT STOCKEES A PLAT PAR LIGNE DOMINANTE
!                SEULE LA PARTIE SUPERIEURE EST PRISE EN COMPTE (COMME LA MATRICE 
!                LOCALE EST A PRIORI SYMETRIQUE)
!                M(I,J)=TAB_LOC(DECA*(INO-1)+I*NM+J) AVEC I=<J
!  * EN SORTIE : LES MATRICES LOCALES SONT STOCKEES A PLAT ET PAR LIGNE DOMINANTE
!                M(I,J)=TAB_LOC(DECA*(INO-1)+I*NM+J)
#include "jeveux.h"
!
    integer :: nm, jadr
    real(kind=8) :: tab_mat(*), scal
    aster_logical :: transpos
!-----------------------------------------------------------------------
    real(kind=8) :: ab(nm,nm)
    integer :: j, i
!-----------------------------------------------------------------------
!
    if ( transpos ) then
      do j=1,nm
       do i=1,nm
         ab(i,j)=tab_mat(jadr+nm*(i-1)+j)
       enddo
      enddo
!
      do j=1,nm
       do i=1,nm
         tab_mat(jadr+nm*(i-1)+j)=scal*ab(j,i)
       enddo
      enddo
    else
      do j=1,nm
       do i=1,nm
         tab_mat(jadr+nm*(i-1)+j)=scal*tab_mat(jadr+nm*(i-1)+j)
       enddo
      enddo
    endif
!

!
end subroutine
