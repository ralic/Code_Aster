subroutine te0253(option, nomte)
!
implicit none
!
#include "jeveux.h"
#include "asterf_types.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
!
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! aslint: disable=W0104
!
    character(len=16), intent(in) :: option
    character(len=16), intent(in) :: nomte
!
! --------------------------------------------------------------------------------------------------
!
! Elementary computation
!
! Elements: AXIS_FLUIDE/2D_FLUIDE
! Options: RIGI_MECA/FORC_NODA/FULL_MECA/RAPH_MECA/RIGI_MECA_HYST/RIGI_MECA_TANG
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: nbres=2
    character(len=16), parameter :: nomres(nbres) = (/'RHO   ', 'CELE_R'/)
    real(kind=8) :: valres(nbres)
    integer :: icodre(nbres)
    integer :: jv_compo, jv_deplm, jv_deplp, k, l, n1, n2
    integer :: nn, nno2, nt2
    real(kind=8) :: r
    character(len=8) :: fami, poum
    character(len=16) :: rela_comp
    integer :: kpg, spt
    real(kind=8) :: a(2, 2, 9, 9)
    real(kind=8) :: b(18, 18), ul(18), c(171)
    real(kind=8) :: poids, rho, celer
    integer :: ipoids, ivf, idfde, jv_geom, jv_mate
    integer :: nno, kp, npg, ik, ijkl, i, j, jv_matr
    integer :: jv_vect, jv_codret
    aster_logical :: l_axis
!
! --------------------------------------------------------------------------------------------------
!
    fami       = 'FPG1'
    kpg        = 1
    spt        = 1
    poum       = '+'
    a(:,:,:,:) = 0.d0
    l_axis     = lteatt('AXIS', 'OUI')
!
! - Get input fields
!
    if (option(1:9) .eq. 'FULL_MECA' .or.&
        option .eq. 'RAPH_MECA' .or.&
        option .eq. 'RIGI_MECA_TANG') then
        call jevech('PCOMPOR', 'L', jv_compo)
        rela_comp = zk16(jv_compo)
        if (rela_comp .ne. 'ELAS') then
            call utmess('F', 'FLUID1_1')
        endif
    endif
    call jevech('PGEOMER', 'L', jv_geom)
    call jevech('PMATERC', 'L', jv_mate)
!
! - Get element parameters
!
    call elrefe_info(fami='RIGI', nno=nno, npg=npg, jpoids=ipoids, jvf=ivf, jdfde=idfde)
!
! - Get material properties
!
    call rcvalb(fami , kpg     , spt   , poum  , zi(jv_mate),&
                ' '  , 'FLUIDE', 0     , ' '   , [0.d0]     ,&
                nbres, nomres  , valres, icodre, 1)
    rho   = valres(1)
    celer = valres(2)
!
! - Loop on Gauss points (compute (P**2)/ (RHO*(CEL**2)) )
!
    do kp = 1, npg
        k = (kp-1)*nno
        call dfdm2d(nno, kp, ipoids, idfde, zr(jv_geom),&
                    poids)
        if (l_axis) then
            r = 0.d0
            do i = 1, nno
                r = r + zr(jv_geom+2*(i-1))*zr(ivf+k+i-1)
            end do
            poids = poids*r
        endif
        do i = 1, nno
            do j = 1, i
                if (celer .eq. 0.d0 .or. rho .eq. 0.d0) then
                    a(1,1,i,j) = 0.d0
                else
                    a(1,1,i,j) = a(1,1,i,j) +&
                                poids * zr(ivf+k+i-1) * zr( ivf+k+j-1) / rho / celer/celer
                endif
            end do
        end do
    end do
!
! - Compute result
!
    do k = 1, 2
        do l = 1, 2
            do i = 1, nno
                ik = ((2*i+k-3) * (2*i+k-2)) / 2
                do j = 1, i
                    ijkl = ik + 2 * (j-1) + l
                    c(ijkl) = a(k,l,i,j)
                end do
            end do
        end do
    end do
    nno2 = nno*2
    nt2 = nno*(nno2+1)
!
! - Save matrix
!
    if (option(1:9) .eq. 'FULL_MECA' .or. option(1:9) .eq. 'RIGI_MECA') then
        if (option .eq. 'RIGI_MECA_HYST') then
            call jevech('PMATUUC', 'E', jv_matr)
            do i = 1, nt2
                zc(jv_matr+i-1)=dcmplx(c(i),0.d0)
            end do
        else
            call jevech('PMATUUR', 'E', jv_matr)
            do i = 1, nt2
                zr(jv_matr+i-1)=c(i)
            end do
        endif
    endif
!
! - Save vector
!
    if (option .eq. 'FULL_MECA' .or.&
        option .eq. 'RAPH_MECA' .or.&
        option .eq. 'FORC_NODA') then
        call jevech('PVECTUR', 'E', jv_vect)
        call jevech('PDEPLMR', 'L', jv_deplm)
        call jevech('PDEPLPR', 'L', jv_deplp)
        do i = 1, nno2
            zr(jv_vect+i-1) = 0.d0
            ul(i)=zr(jv_deplm+i-1)+zr(jv_deplp+i-1)
        end do
        nn = 0
        do n1 = 1, nno2
            do n2 = 1, n1
                nn = nn + 1
                b(n1,n2) = c(nn)
                b(n2,n1) = c(nn)
            end do
        end do
        do n1 = 1, nno2
            do n2 = 1, nno2
                zr(jv_vect+n1-1) = zr(jv_vect+n1-1)+b(n1,n2)*ul(n2)
            end do
        end do
    endif
!
! - Save return code
!
    if (option(1:9) .eq. 'FULL_MECA' .or. option .eq. 'RAPH_MECA') then
        call jevech('PCODRET', 'E', jv_codret)
        zi(jv_codret) = 0
    endif
!
end subroutine
