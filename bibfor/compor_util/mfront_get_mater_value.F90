subroutine mfront_get_mater_value(fami, kpg, ksp, imate, ifm, &
                                  niv, idbg, pmatprop, pnbprop, nprops, &
                                  props)
! ======================================================================
! COPYRIGHT (C) 1991 - 2014  EDF R&D                  WWW.CODE-ASTER.ORG
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
    integer, intent(in) :: pmatprop
    integer, intent(in) :: pnbprop
    integer, intent(out) :: nprops
    real(kind=8), intent(out) :: props(*)
!
! Retourne les valeurs des coefficients materiau pour l'interface MFront
!       in   fami    famille de point de gauss (rigi,mass,...)
!            kpg,ksp numero du (sous)point de gauss
!            imate   adresse du materiau code
!            pmatprop, pnbprop adresse des datas MFront
!       out  nprops  nb coef
!            props   coef materiau
!
! aslint: disable=W1504,W0104
#include "asterc/r8nnem.h"
#include "asterc/mfront_get_mater_prop.h"
#include "asterc/mfront_get_mater_prop_size.h"
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
!
    integer, parameter :: npropmax=197
    integer :: codrel(npropmax), nbcoef, i
    character(len=16) :: nomres(npropmax)
    real(kind=8) :: propl(npropmax)
!
!   Get the number of material proporties
    print *, '#DEBUG: pointer: ', pnbprop
    call mfront_get_mater_prop_size(pnbprop, nprops)
    print *, '#DEBUG: nb properties: ', nprops

!   Get the names of the material properties
    ASSERT(nprops <= npropmax)
    call mfront_get_mater_prop(pmatprop, nprops, nomres)

!     LECTURE DES PROPRIETES MATERIAU (MOT-CLE UMAT DE DEFI_MATERIAU)
    call r8inir(nprops, r8nnem(), props, 1)
!
!     LECTURE DU PREMIER PARAMETRE NB, FACULTATIF
    ! call rcvalb(fami, kpg, ksp, '+', imate, ' ', 'UMAT', 0, ' ', [0.d0], &
    !             1, 'NB_VALE', propl(1), codrel, 0)
    ! if (codrel(1) .eq. 0) then
    !     nbcoef=nint(propl(1))
    ! else
    !     nbcoef=nprops
    ! endif
    nbcoef = nprops
!     lecture des autres parametres
    call rcvalb(fami, kpg, ksp, '+', imate, ' ', 'UMAT', 0, ' ', [0.d0], &
    &           nbcoef, nomres, propl, codrel, 0)
!     COMPTAGE DU NOMBRE DE PROPRIETES
!     CODREL(I)=0 SI LE PARAMETRE EXISTE, 1 SINON
    if ((niv.ge.2) .and. (idbg.eq.1)) then
        write(ifm,*)' '
        write(ifm,*)'COEFFICIENTS MATERIAU'
    endif
    ! nprops=0
    do i = 1, nbcoef
        if (codrel(i) .eq. 0) then
            ! nprops=nprops+1
            props(nprops)=propl(i)
            if ((niv.ge.2) .and. (idbg.eq.1)) then
                write(ifm,*) nomres(i),props(nprops)
            endif
        endif
    end do
end
