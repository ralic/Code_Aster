subroutine cntdif(ivect, dimen, diff, valdif, maxdim)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
!
#include "jeveux.h"
#include "asterfort/ordis.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    integer,intent(in) :: maxdim
    integer,intent(in) :: dimen
    integer,intent(in) :: ivect
    integer,intent(out) :: diff
    integer,intent(out) :: valdif(maxdim)
    integer :: ii
    integer, pointer :: vaux(:) => null()

! ----------------------------------------------------------------------
!
!  ROUTINE LIEE A L'OPERATEUR CALC_ERC_DYN
!
!  COMPTE LE NOMBRE D'ELEMENTS DIFFERENTS D'UN ARRAY D'ENTIERS
! ----------------------------------------------------------------------
! IN : IVECT  : POINTEUR DU PREMIER ELEMENT DE L'ARRAY D'ENTIERS
! IN : DIMEN  : DIMENSION DE L'ARRAY SUR LEQUEL ON EFFECTUE LE COMPTAGE
! OUT: DIFF   : NOMBRE D'ELEMENTS DIFFERENTS DANS L'ARRAY
! OUT: VALDIF : ARRAY CONTENANT LES VALEURS DIFFERENTES DANS L'ARRAY 
! IN : MAXDIM : DIMENSION MAXI DE L'ARRAY DE SORTIE VALDIF
! ----------------------------------------------------------------------
    AS_ALLOCATE(vi=vaux, size=dimen)
!
!     ON COPIE DANS UN CONCEPT DE TRAVAIL
    do   ii = 1, dimen
        vaux(ii)=zi(ivect+ii-1)
    end do
!     ON TRIE LE VECTEUR PAR ORDRE CROISSANT
    call ordis(vaux, dimen)
    diff=1
!     ON COMPTE LES ELEMENTS DIFFERENTS
    do   ii = 1, dimen-1
        if (vaux(ii+1) .gt. vaux(ii)) then
            valdif(diff)=vaux(ii)
            diff=diff+1
        endif
    end do
    valdif(diff)=vaux(dimen)
    AS_DEALLOCATE(vi=vaux)
end subroutine
