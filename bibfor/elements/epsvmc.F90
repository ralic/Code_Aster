subroutine epsvmc(fami   , nno    , ndim  , nbsig, npg   ,&
                  j_poids, j_vf   , j_dfde, xyz  , disp  ,&
                  time   , j_mater, repere, nharm, option,&
                  epsi   )
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dmatmc.h"
#include "asterfort/eps1mc.h"
#include "asterfort/eps2mc.h"
#include "asterfort/epthmc.h"
#include "asterfort/get_elas_id.h"
#include "asterfort/lteatt.h"
#include "asterfort/jevech.h"
#include "asterfort/rccoma.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
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
!
    character(len=*), intent(in) :: fami
    integer, intent(in) :: nno
    integer, intent(in) :: ndim
    integer, intent(in) :: nbsig
    integer, intent(in) :: npg
    integer, intent(in) :: j_poids
    integer, intent(in) :: j_vf
    integer, intent(in) :: j_dfde
    real(kind=8), intent(in) :: xyz(1)
    real(kind=8), intent(in) :: disp(1)
    real(kind=8), intent(in) :: time
    integer, intent(in) :: j_mater
    real(kind=8), intent(in) :: repere(7)
    real(kind=8), intent(in) :: nharm
    character(len=16), intent(in) :: option
    real(kind=8), intent(out) :: epsi(1)
!
! --------------------------------------------------------------------------------------------------
!
! Compute mechanical strains or total strains (depend on option)
!
! Mechanical strains = total strains - command variables strains
!
! --------------------------------------------------------------------------------------------------
!
! In  fami         : Gauss family for integration point rule
! In  nno          : number of nodes
! In  ndim         : dimension of space
! In  nbsig        : number of stress tensor components
! In  npg          : number of Gauss points
! In  j_poids      : JEVEUX adress to weight of Gauss points
! In  j_vf         : JEVEUX adress to shape functions
! In  j_dfde       : JEVEUX adress to derivatives of shape functions
! In  xyz          : coordinates of element
! In  disp         : displacements of element
! In  time         : current time
! In  j_mater      : coded material address
! In  repere       : definition of basis (for non-isotropic materials)
! In  nharm        : Fourier mode
! In  option       : name of option to compute
! Out epsi         : mechanical strains or total strains
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: elas_keyword, rela_comp
    integer :: elas_id
    real(kind=8) :: epsi_varc(162), epsi_tota_g(162), epsi_tota(162)
    real(kind=8) :: xyzgau(3), d(4, 4)
    real(kind=8) :: zero, un, deux
    integer :: i, iret, kpg, j_compo
    aster_logical :: l_modi_cp
!
! --------------------------------------------------------------------------------------------------
!
    zero = 0.d0
    un   = 1.d0
    deux = 2.d0
    ASSERT(nbsig*npg.le.162)
    epsi     (1:nbsig*npg)   = zero
    epsi_tota(1:nbsig*npg)   = zero
    epsi_tota_g(1:nbsig*npg) = zero
    epsi_varc(1:nbsig*npg)   = zero
!
! - Get comportment
!
    if (option(1:4).eq.'EPME'.or.option(1:4).eq.'EPMG') then
        call tecach('NNO', 'PCOMPOR', 'L', iret, iad=j_compo)
        if (iret.eq.0) then
            rela_comp  = zk16(j_compo)
        else
            rela_comp = 'Unknown'
        endif
    endif
!
! - Total strains: first order (small strains)
!
    call eps1mc(nno, ndim, nbsig, npg, j_poids,&
                j_vf, j_dfde, xyz, disp, nharm,&
                epsi_tota)
!
! - Total strains: second order (large strains)
!
    if (option(4:4) .eq. 'G') then
        call eps2mc(nno, ndim, nbsig, npg, j_poids,&
                    j_vf, j_dfde, xyz, disp, epsi_tota_g)
    endif
!
! - Total strains
!
    do i = 1, nbsig*npg
        epsi(i) = epsi_tota(i) + epsi_tota_g(i)
    end do
!
! - Elasticity type
!
    call get_elas_id(j_mater, elas_id, elas_keyword)
!
! - Mechanical strains: not metallurgy except META_LEMA_ANI !
!
    if (option(1:4).eq.'EPME'.or.option(1:4).eq.'EPMG') then
        if (elas_keyword.eq.'ELAS_META'.and.rela_comp.ne.'META_LEMA_ANI') then
            call utmess('F', 'ELEMENTS6_4')
        endif
    endif
!
! - Compute variable commands strains (thermics, drying, etc.)
!
    if (option(1:4).eq.'EPME'.or.option(1:4).eq.'EPMG'.or.lteatt('C_PLAN','OUI')) then
        call epthmc(fami, nno, ndim, nbsig, npg,&
                    zr(j_vf), xyz, repere, time, j_mater,&
                    option, epsi_varc)
    endif
!
! - Mechanical strains
!
    if (option(1:4) .eq. 'EPME' .or. option(1:4) .eq. 'EPMG') then
        do i = 1, nbsig*npg
            epsi(i) = epsi_tota(i) + epsi_tota_g(i) - epsi_varc(i)
        end do
    endif
!
! - 2D model
!
    if (lteatt('C_PLAN','OUI')) then
!
! ----- Plane stress
!
        do kpg = 1, npg
!
! --------- Real coordinates of Gauss point
!
            xyzgau(1) = zero
            xyzgau(2) = zero
            xyzgau(3) = zero
!
! --------- il s'agit de calculer EPS33 : pour cela il faut donner la
! --------- condition SIG33=0 dans l'expression complete de la loi de
! --------- Hooke c'est à dire avec la loi 3D :
! --------- Eps33= -1/D33 (D13.Eps11 +D12.Eps22), ce qui donne (en
! --------- isotrope) l'expression classique :
! --------- Eps33 = -Nu / (1-Nu) * (Eps11 + Eps22).
! --------- voir issue12540
!
            l_modi_cp = .true.
!
! --------- Hooke matrix for iso-parametric elements
!
            call dmatmc(fami, j_mater, time, '+', kpg,&
                        1, repere, xyzgau, nbsig, d,&
                        l_modi_cp)
!
            if (option(1:4) .eq. 'EPME' .or. option(1:4) .eq. 'EPMG') then
                epsi(nbsig*(kpg-1)+3) = -un/d(3,3)*&
                                         (d(3,1)*epsi(nbsig*(kpg-1)+1)+&
                                          d(3,2)*epsi(nbsig*(kpg-1)+2)+&
                                          d(3,4)*epsi(nbsig*(kpg-1)+4)*deux)
            else
                epsi(nbsig*(kpg-1)+3) = -un/d(3,3)*&
                                         (d(3,1)*(epsi(nbsig*(kpg-1)+1)-&
                                                  epsi_varc(nbsig*(kpg-1)+1))+&
                                          d(3,2)*(epsi(nbsig*(kpg-1)+2)-&
                                                  epsi_varc(nbsig*(kpg-1)+2))+&
                                          d(3,4)*(epsi(nbsig*(kpg-1)+4)-&
                                                  epsi_varc(nbsig*(kpg-1)+4))*deux)+&
                                         epsi_varc(nbsig*(kpg-1)+3)
            endif
        end do
    else if (lteatt('D_PLAN','OUI')) then
!
! ----- Plane strain: EPZZ = 0
!
        do kpg = 1, npg
            epsi(nbsig*(kpg-1)+3) = zero
        end do
    endif
!
end subroutine
