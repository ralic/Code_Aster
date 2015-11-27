subroutine massdir(massmat, dir, dmass)
    implicit none
!-----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
! person_in_charge: hassan.berro at edf.fr
!
!    Calculates the structural (total) mass along a given direction
!    "dir" defined as a vector of 3 real coordinates
!
!    Example : call massdir(massm, numddl, [1.d0, 0.d0, 0.d0] , masx)
!-----------------------------------------------------------------------
!    Note : massdir automatically normalizes the input directional 
!           vector such that sqrt(x**2+y**2+z**2) = 1
!
!              call massdir(massm, [0.d0, 5.8d0, 0.d0], masy)
!                                    ^
!                                   | | is equivalent to
!                                    v
!              call massdir(massm, [0.d0, 1.0d0, 0.d0], masy)
!-----------------------------------------------------------------------
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mrmult.h"
#include "asterfort/mtdscr.h"
#include "asterfort/pteddl.h"
#include "blas/ddot.h"
!
!   -0.1- Input/output arguments
    character(len=*), intent(in)   :: massmat
    real(kind=8)    , intent(in)   :: dir(3)
    real(kind=8)    , intent(out)  :: dmass
!
!   -0.2- Local variables
    integer           :: lmatm, neq, ieq, ic, dec, iret
    real(kind=8)      :: magn, normdir(3)
    character(len=8)  :: nomcmp(3)
    character(len=14) :: nume
    character(len=19) :: masse
    integer     , pointer  :: posdof(:)  => null()
    real(kind=8), pointer  :: unitv (:)  => null()
    real(kind=8), pointer  :: mass_utv (:)  => null()
!
!   0 - Initializations
    call jemarq()
    masse = massmat
    magn = 0.d0
    nomcmp(1) = 'DX'
    nomcmp(2) = 'DY'
    nomcmp(3) = 'DZ'

!   1 - Normalisation of the input directional vector
    do ic = 1, 3
        magn = magn + dir(ic)*dir(ic)
        normdir(ic) = dir(ic)
    end do
    magn = sqrt(magn)
    do ic = 1, 3
        normdir(ic) = normdir(ic) / magn
    end do

!   2 - Preparation of the mass matrix for the mrmult call
    call jeexin(masse(1:19)//'.&INT', iret)
    if (iret.eq.0) then
        call mtdscr(masse)
    end if
    call jeveuo(masse(1:19)//'.&INT', 'E', lmatm)


!   3 - Filling up a directional dof unitary field
    call dismoi('NB_EQUA'     , masse, 'MATR_ASSE', repi=neq)
    call dismoi('NOM_NUME_DDL', masse, 'MATR_ASSE', repk=nume)
    AS_ALLOCATE(vi=posdof, size=3*neq)
    call pteddl('NUME_DDL', nume, 3, nomcmp, neq,&
                tabl_equa = posdof)

    AS_ALLOCATE(vr=unitv, size=neq)
    do ieq = 1, neq
        unitv(ieq) = 0.d0
    end do
    do ic = 1, 3
        dec = neq * ( ic - 1 )
        do ieq = 1, neq
            unitv(ieq) = unitv(ieq) + posdof(dec+ieq) * normdir(ic)
        end do
    end do

!
!   4 - Calculate  dmass = Ut*M*U
    AS_ALLOCATE(vr=mass_utv, size=neq)
    call mrmult('ZERO', lmatm, unitv, mass_utv, 1,&
                .true._1)
    dmass = ddot(neq, unitv, 1, mass_utv, 1)
!
!   5 - Cleanup
    AS_DEALLOCATE(vi=posdof)
    AS_DEALLOCATE(vr=unitv)
    AS_DEALLOCATE(vr=mass_utv)
!
    call jedema()

end subroutine