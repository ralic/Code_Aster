subroutine te0333(option, nomte)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/calcgr.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/get_elas_type.h"
#include "asterfort/get_elas_para.h"
#include "asterfort/epsvmc.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/nbsigm.h"
#include "asterfort/rcvarc.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
#include "asterfort/granvi.h"
#include "asterfort/ortrep.h"
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
! aslint: disable=W0104
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=16), intent(in) :: option
    character(len=16), intent(in) :: nomte
!
! --------------------------------------------------------------------------------------------------
!
! Elementary computation
!
! Elements: 3D
! Option: EPSP_ELGA
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: mxcmel=162
    integer, parameter :: nbsgm=6
    real(kind=8) :: epsi_meca(mxcmel), epsi_plas(mxcmel)
    real(kind=8) :: sigma(nbsgm)
    real(kind=8) :: epsi_creep(nbsgm)
    integer :: i, ndim, nno, nbsig, idsig
    integer :: npg, ipoids, ivf, idfde, igau, isig, igeom, idepl, itemps, imate
    integer :: idefp, icompo, nbvari, ivari, nvi, nvif, ibid, jtab(7), iret
    integer :: idim
    real(kind=8) :: c1, c2, trsig, xyz(3)
    real(kind=8) :: repere(7), nharm, e, nu, zero, un, tempg, time
    character(len=8) :: mod3d
    integer :: elas_type
    character(len=16) :: optio2, kit_comp_1, kit_comp_2, rela_comp, elas_keyword
    aster_logical :: l_creep, l_temp
!
! --------------------------------------------------------------------------------------------------
!
    zero  = 0.d0
    un    = 1.d0
    nharm = zero
    mod3d = '3D'
!
! - Finite element informations
!
    call elrefe_info(fami='RIGI', ndim=ndim, nno=nno, npg=npg,&
                     jpoids=ipoids, jvf=ivf, jdfde=idfde)
!
! - Number of stress components
!
    nbsig = nbsigm()
    ASSERT(nbsig.eq.nbsgm)
!
! - Geometry
!
    call jevech('PGEOMER', 'L', igeom)
!
! - Material parameters
!
    call jevech('PMATERC', 'L', imate)
!
! - Orthotropic parameters
!
    xyz(1) = 0.d0
    xyz(2) = 0.d0
    xyz(3) = 0.d0
    do i = 1, nno
        do idim = 1, ndim
            xyz(idim) = xyz(idim)+zr(igeom+idim+ndim*(i-1)-1)/nno
        end do
    end do
    call ortrep(zi(imate), ndim, xyz, repere)
!
! - Current time
!
    call jevech('PTEMPSR', 'L', itemps)
    time = zr(itemps)
!
! - Current displacements (nodes)
!
    call jevech('PDEPLAR', 'L', idepl)
!
! - Current stresses (gauss points)
!
    call jevech('PCONTRR', 'L', idsig)
!
! - Comportment
!
    call jevech('PCOMPOR', 'L', icompo)
    rela_comp  = zk16(icompo)
    kit_comp_1 = zk16(icompo+7)
    kit_comp_2 = zk16(icompo+8)
!
! - Internal variables
!
    call jevech('PVARIGR', 'L', ivari)
    call tecach('OON', 'PVARIGR', 'L', iret, nval=7,&
                itab=jtab)
    nbvari = max(jtab(6),1)*jtab(7)
!
! - Elasticity: only isotropic and not metallurgy !
!
    call get_elas_type(zi(imate), elas_type, elas_keyword)
    if (elas_type.eq.1) then
        if (elas_keyword.eq.'ELAS_META') then
            call utmess('F', 'ELEMENTS6_1')
        endif
    else
        call utmess('F', 'ELEMENTS6_2')
    endif
!
! - Compute mechanical strains epsi_meca = epsi_tota - epsi_varc
! -- Command variables strains: epsi_varc (contains thermics, drying, ...)
! -- Total strains: epsi_tota
!
    optio2 = 'EPME_ELGA'
    call epsvmc('RIGI', nno, ndim, nbsig, npg,&
                ipoids, ivf, idfde, zr(igeom), zr(idepl),&
                zr(itemps), zi(imate), repere, nharm, optio2,&
                epsi_meca)
!
! - Creep strains: epsi_creep
!
    if (rela_comp(1:10) .ne. 'GRANGER_FP' .and.&
       (rela_comp(1:7).ne.'KIT_DDI'.or.kit_comp_1(1:10).ne.'GRANGER_FP')) then
        l_creep = .false.
        do i = 1, mxcmel
            epsi_plas(i) = zero
        end do
        do i = 1, nbsig
            epsi_creep(i) = zero
        end do
    else
        call granvi(mod3d, ibid, ibid, nvif)
        l_creep = .true.
    endif
!
! - Materials parameters depend on temperature ?
!
    l_temp = .false.
    if (rela_comp(1:15) .eq. 'BETON_DOUBLE_DP') then
        nvi = 3
        l_temp = .true.
    else if (rela_comp(1:7).eq.'KIT_DDI') then
        if (kit_comp_2(1:15) .eq. 'BETON_DOUBLE_DP') then
            if (kit_comp_1(1:10) .eq. 'GRANGER_FP') then
                nvi = nvif + 3
                l_temp = .true.
            else
                call utmess('F', 'ELEMENTS3_76')
            endif
        endif
    endif
!
! - Loop on Gauss points
!
    do igau = 1, npg
!
! ----- Get current temperature
!
        call rcvarc(' ', 'TEMP', '+', 'RIGI', igau,&
                    1, tempg, iret)
!
! ----- Change temperature from internal variable (maximum) for BETON_DOUBLE_DP/GRANGER_FP
!
        if (l_temp) then
            if (tempg .lt. zr(ivari+ (igau-1)*nbvari+nvi- 1)) then
                tempg = zr(ivari+(igau-1)*nbvari+nvi-1)
            endif
        endif
!
! ----- Get elastic parameters (only isotropic elasticity)
!
        call get_elas_para('RIGI', zi(imate), '+', igau, 1,&
                           elas_type, time = time, temp = tempg, e = e, nu = nu)
        ASSERT(elas_type.eq.1)
!
! ----- Compute creep strains (current Gauss point)
!
        if (l_creep) then
            call calcgr(igau      , nbsig, nbvari, zr(ivari), nu,&
                        epsi_creep)
        endif
!
! ----- Compute stresses (current Gauss point)
!
        do i = 1, nbsig
            sigma(i) = zr(idsig+(igau-1)*nbsig+i-1)
        end do
        trsig = sigma(1) + sigma(2) + sigma(3)
!
! ----- Compute plastic strains (current Gauss point) epsi_plas = epsi_tota - epsi_elas - epsi_creep
! -- Creep strains: epsi_creep
! -- Elastic strains: epsi_elas
!
        c1 = (un+nu)/e
        c2 = nu/e
        epsi_plas(nbsig*(igau-1)+1) = epsi_meca(nbsig*(igau-1)+1) - (c1* sigma(1)-c2*trsig) -&
                                      epsi_creep(1)
        epsi_plas(nbsig*(igau-1)+2) = epsi_meca(nbsig*(igau-1)+2) - (c1* sigma(2)-c2*trsig) -&
                                      epsi_creep(2)
        epsi_plas(nbsig*(igau-1)+3) = epsi_meca(nbsig*(igau-1)+3) - (c1* sigma(3)-c2*trsig) -&
                                      epsi_creep(3)
        epsi_plas(nbsig*(igau-1)+4) = epsi_meca(nbsig*(igau-1)+4) - c1* sigma(4) - epsi_creep(4 )
        epsi_plas(nbsig*(igau-1)+5) = epsi_meca(nbsig*(igau-1)+5) - c1* sigma(5) - epsi_creep(5 )
        epsi_plas(nbsig*(igau-1)+6) = epsi_meca(nbsig*(igau-1)+6) - c1* sigma(6) - epsi_creep(6 )
    end do
!
! - Plastic strain output
!
    call jevech('PDEFOPG', 'E', idefp)
    do igau = 1, npg
        do isig = 1, nbsig
            zr(idefp+nbsig*(igau-1)+isig-1) = epsi_plas(nbsig*(igau-1)+ isig)
        end do
    end do
!
end subroutine
