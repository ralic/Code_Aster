subroutine dtmeigen_fsi(sd_dtm_, buffdtm)
    implicit none
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: hassan.berro at edf.fr
!
! dtmeigen_fsi : Update the damping C-matrix after a change in state is detected
!                as to accomodate to the change in modal frequencies and shapes
!
#include "jeveux.h"
#include "blas/dcopy.h"
#include "asterfort/coefmo.h"
#include "asterfort/dtmcase_coder.h"
#include "asterfort/dtmget.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
!
!   -0.1- Input/output arguments
    character(len=*), intent(in)  :: sd_dtm_
    integer, pointer              :: buffdtm(:)
!
!   -0.2- Local variables
    integer          :: i, j, k, nbmode, nlcase, nbno
    real(kind=8)     :: puls, xcf, vgap, x(2), tmp
    character(len=8) :: sd_dtm, tpfl
    character(len=7) :: casek7
!
    real(kind=8), pointer :: mgen0(:)      => null()
    real(kind=8), pointer :: mgen(:)       => null()
    real(kind=8), pointer :: kgen(:)       => null()
    real(kind=8), pointer :: agen(:)       => null()
    real(kind=8), pointer :: aful(:)       => null()

    real(kind=8), pointer :: c_flu(:)      => null()
    real(kind=8), pointer :: phi_v(:)      => null()
    real(kind=8), pointer :: base(:)       => null()

    integer     , pointer :: icoupled(:)   => null()
    integer     , pointer :: ires(:)       => null()
    integer     , pointer :: itypfl(:)     => null()
    real(kind=8), pointer :: profv(:)      => null()
    real(kind=8), pointer :: rhoe(:)       => null()
    real(kind=8), pointer :: phie(:)       => null()
    real(kind=8), pointer :: basf(:)       => null()
    real(kind=8), pointer :: absc(:)       => null()
    real(kind=8), pointer :: codim(:)      => null()
    real(kind=8), pointer :: poids(:)      => null()

#define a(row,col) aful((row-1)*nbmode+col)

!
!   0 - Initializations
!
    call jemarq()
    sd_dtm = sd_dtm_

    call dtmget(sd_dtm, _NB_MODES, iscal=nbmode, buffer=buffdtm)

    call dtmget(sd_dtm, _NL_CASE, iscal=nlcase, buffer=buffdtm)
    call dtmcase_coder (nlcase, casek7)

    call dtmget(sd_dtm, _MASS_DIA, vr=mgen0, buffer=buffdtm)
    call jeveuo(sd_dtm // '.PRJ_MAS.'// casek7, 'E', vr=mgen)
    call jeveuo(sd_dtm // '.PRJ_RIG.'// casek7, 'E', vr=kgen)
    call jeveuo(sd_dtm // '.PRJ_AMO.'// casek7, 'E', vr=agen)
    call jeveuo(sd_dtm // '.PRJ_AM2.'// casek7, 'E', vr=aful)

    call dtmget(sd_dtm, _FSI_TYPF, kscal=tpfl , buffer=buffdtm)
    call dtmget(sd_dtm, _FSI_VGAP, rscal=vgap , buffer=buffdtm)
    call dtmget(sd_dtm, _FSI_CPLD, vi=icoupled, buffer=buffdtm)
    call dtmget(sd_dtm, _FSI_ITYP, vi=itypfl  , buffer=buffdtm)
    call dtmget(sd_dtm, _FSI_ZET0, vr=c_flu   , buffer=buffdtm)           
    if (itypfl(1).eq.1) then
        call jeveuo(sd_dtm // '.PRJ_BAS.'// casek7, 'L', vr=phi_v)
        call dtmget(sd_dtm, _FSI_IRES, vi=ires , buffer=buffdtm)
        call dtmget(sd_dtm, _FSI_PRVI, vr=profv, buffer=buffdtm)
        call dtmget(sd_dtm, _FSI_RHOE, vr=rhoe , buffer=buffdtm)
        call dtmget(sd_dtm, _FSI_BASF, vr=basf , buffer=buffdtm)
        call dtmget(sd_dtm, _FSI_PHIE, vr=phie , buffer=buffdtm)
        call dtmget(sd_dtm, _FSI_ABSC, vr=absc , buffer=buffdtm)

!       --- Determine the new modal basis in physical coordinates
        nbno = itypfl(2)
        AS_ALLOCATE(vr=base, size=nbmode*nbno)
        do j = 1, nbno
            do i = 1, nbmode
                tmp = 0.d0
                do k = 1, nbmode
                    tmp = tmp + phi_v((i-1)*nbmode+k) * basf((k-1)*nbno+j)
                end do
                base((i-1)*nbno+j) = tmp
            end do
        end do

        do i = 1, nbmode
            puls = sqrt(kgen(i)/mgen(i))
            call coefmo(tpfl, .false._1, nbmode, i, nbno,&
                        x, puls, vgap, 0.d0, ires,&
                        profv, rhoe, base, phie, absc,&
                        zr(1), zc(1), xcf)
            c_flu(i) = xcf/mgen(i)
            agen (i) = agen(i) + c_flu(i)
            a(i,i)   = agen(i)
        end do
        AS_DEALLOCATE(vr=base)
    else if (itypfl(1).eq.2) then
        call dtmget(sd_dtm, _FSI_CODM, vr=codim, buffer=buffdtm)
        call dtmget(sd_dtm, _FSI_POID, vr=poids, buffer=buffdtm)
        call dtmget(sd_dtm, _FSI_PHIE, vr=phie , buffer=buffdtm)
        do i = 1, nbmode

            puls = sqrt(kgen(i)/mgen(i))
            call coefmo(tpfl, .false._1, nbmode, i, itypfl(2),&
                        x, puls, vgap, 0.d0, zi(1),&
                        mgen, codim, poids, phie, zr(1),&
                        zr(1), zc(1), xcf)
            c_flu(i) = xcf/mgen(i)
            agen (i) = agen(i) + c_flu(i)
            a(i,i)   = agen(i)
        end do
    end if

    call jedema()
end subroutine
