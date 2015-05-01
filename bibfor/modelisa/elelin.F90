subroutine elelin(nconta, elref1, elref2, nnop, nnops)
    implicit none
!
#include "asterfort/elraca.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    character(len=8) :: elref1, elref2
    integer :: nnop, nnops, nconta
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!                      RETOURNE LE TYPE DE L'ELEMENT "LINEARISE"
!                      ET LE NOMBRE DE NOEUDS DE CHAQUE ELEMENT
!
!     ENTREE
!       NCONTA  : TYPE DE CONTACT
!       ELREF1  : TYPE DE L'ELEMENT PARENT
!
!     SORTIE
!       ELREF2  : TYPE DE L'ELEMENT LINEAIRE A L'ELEMENT PARENT
!       NNOP    : NOMBRE DE NOEUDS DE L'ELEMENT PARENT
!       NNOPS   : NOMBRE DE NOEUDS DE L'ELEMENT LINEAIRE
!......................................................................
!
    integer :: ndim, nbfpg, nbpg(20)
    real(kind=8) :: x(3*27), vol
    character(len=8) :: fapg(20)
!
    call jemarq()
!
    if (nconta .eq. 3) then
        if (elref1 .eq. 'QU8') then
            elref2='QU4'
            nnop = 8
            nnops= 4
        else if (elref1.eq.'TR6') then
            elref2='TR3'
            nnop = 6
            nnops= 3
        else if (elref1.eq.'SE3') then
            elref2='SE2'
            nnop = 3
            nnops= 2
        else if (elref1.eq.'H20') then
            elref2='HE8'
            nnop = 20
            nnops= 8
        else if (elref1.eq.'P15') then
            elref2='PE6'
            nnop = 15
            nnops= 6
        else if (elref1.eq.'P13') then
            elref2='PY5'
            nnop = 13
            nnops= 5
        else if (elref1.eq.'T10') then
            elref2='TE4'
            nnop = 10
            nnops= 4
        else
            elref2=elref1
            call elraca(elref1, ndim, nnop, nnops, nbfpg,&
                        fapg, nbpg, x, vol)
        endif
    else
        elref2=elref1
        call elraca(elref1, ndim, nnop, nnops, nbfpg,&
                    fapg, nbpg, x, vol)
    endif
!
    call jedema()
end subroutine
