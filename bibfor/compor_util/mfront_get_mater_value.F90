subroutine mfront_get_mater_value(fami, kpg, ksp, imate, ifm, &
                                  niv, idbg, rela_comp, nprops, props)
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
    character(len=*), intent(in) :: fami
    integer, intent(in) :: kpg
    integer, intent(in) :: ksp
    integer, intent(in) :: imate
    integer, intent(in) :: ifm
    integer, intent(in) :: niv
    integer, intent(in) :: idbg
    character(len=16), intent(in) :: rela_comp
    integer, intent(out) :: nprops
    real(kind=8), intent(out) :: props(*)
!
! Retourne les valeurs des coefficients materiau pour l'interface MFront
!       in   fami    famille de point de gauss (rigi,mass,...)
!            kpg,ksp numero du (sous)point de gauss
!            imate   adresse du materiau code
!       out  nprops  nb coef
!            props   coef materiau
!
#include "asterc/r8nnem.h"
#include "asterc/mfront_get_mater_prop.h"
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/mat_proto.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
!
    integer, parameter :: npropmax=197
    integer :: codrel(npropmax), nbcoef, i
    character(len=16) :: nomres(npropmax)
    real(kind=8) :: propl(npropmax)
!
    real(kind=8) :: phase(5), zalpha
    character(len=8) :: acier(4)
    integer     :: ire2,k,nz
!
    data acier /'PFERRITE','PPERLITE','PBAINITE','PMARTENS'/
!
    if (rela_comp .eq. 'MFRONT') then
!       Usermaterial: parameters are defined using UMAT in DEFI_MATERIAU
        call mat_proto(fami, kpg, ksp, '+', imate, rela_comp, nprops, props)
    else
!       Get the number and the names of the material properties
        call mfront_get_mater_prop(rela_comp, nbcoef, nomres)
        ASSERT(nbcoef <= npropmax)
!       Get the properties values (enter under 'rela_comp' in DEFI_MATERIAU)
        call r8inir(nbcoef, r8nnem(), props, 1)
        if (rela_comp(1:4) .eq. 'Meta') then
!           Compute zalpha 
            nz = 5
            do k = 1, nz-1
                call rcvarc(' ', acier(k), '+', fami, kpg,ksp, phase(k), ire2)
                if (ire2 .eq. 1) phase(k)=0.d0
            end do
            zalpha=phase(1)+phase(2)+phase(3)+phase(4)
!
            do i = 1, nbcoef
                if (nomres(i)(1:4) .eq. 'meta') then
                    call rcvalb(fami, 1, 1, '+', imate,&
                                ' ', rela_comp, 1, 'META', [zalpha],&
                                1, nomres(i), propl(i), codrel(i), 2)
                else
                    call rcvalb(fami, kpg, ksp, '+', imate, &
                                ' ', rela_comp, 0, ' ', [0.d0], &
                    &           1, nomres(i), propl(i), codrel(i), 1)
                endif
            enddo
        else
            call rcvalb(fami, kpg, ksp, '+', imate, &
                        ' ', rela_comp, 0, ' ', [0.d0], &
            &           nbcoef, nomres, propl, codrel, 1)
        endif
!
!       Count the number of properties (but there are all compulsory)
!       codrel(I)=0 if the parameter exists, else 1
        if ((niv.ge.2) .and. (idbg.eq.1)) then
            write(ifm,*)' '
            write(ifm,*)'COEFFICIENTS MATERIAU'
        endif
        nprops = 0
        do i = 1, nbcoef
            if (codrel(i) .eq. 0) then
                nprops = nprops+1
                props(nprops) = propl(i)
                if ((niv.ge.2) .and. (idbg.eq.1)) then
                    write(ifm,*) nomres(i), props(nprops)
                endif
            endif
        end do
    endif
end
