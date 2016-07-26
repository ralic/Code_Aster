!
! COPYRIGHT (C) 1991 - 2016  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
#include "asterf_types.h"
!
interface
    subroutine xcrvol(nse, ndim, jcnse, nnose, jpint,&
                      igeom, elrefp, inoloc, nbnoma, jcesd3,&
                      jcesl3, jcesv3, numa2, iheav, nfiss, vhea,&
                      jcesd8, jcesl8, jcesv8, lfiss, vtot)
        integer :: nbnoma
        integer :: ndim
        integer :: nse
        integer :: jcnse
        integer :: nnose
        integer :: jpint
        integer :: igeom
        character(len=8) :: elrefp
        integer :: inoloc
        integer :: jcesd3
        integer :: jcesl3
        integer :: jcesv3
        integer :: numa2
        aster_logical :: lfiss
        integer :: iheav
        integer :: nfiss
        real(kind=8) :: vhea
        integer :: jcesd8
        integer :: jcesl8
        integer :: jcesv8
        real(kind=8) :: vtot
    end subroutine xcrvol
end interface
