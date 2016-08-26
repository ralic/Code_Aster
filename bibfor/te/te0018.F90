subroutine te0018(option, nomte)
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevecd.h"
#include "asterfort/jevech.h"
#include "asterfort/fointe.h"
#include "asterfort/nmpr3d_vect.h"
#include "asterfort/rccoma.h"
#include "asterfort/tecach.h"
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
! aslint: disable=W0413
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=16), intent(in) :: option
    character(len=16), intent(in) :: nomte
!
! --------------------------------------------------------------------------------------------------
!
! Elementary computation
!
! Elements: 3D and membrane (only CHAR_MECA_PRES_R)
! Option: CHAR_MECA_PRES_R
!         CHAR_MECA_EFON_R
!
! --------------------------------------------------------------------------------------------------
!
    character(len=32) :: phenom
    character(len=8) :: param
    integer :: ndim, nno, npg, nnos, jgano, kpg, kdec, n
    integer :: ipoids, ivf, idf
    integer :: j_geom, j_pres, j_vect, j_effe
    integer :: imate, icodre, itab(8), iret, jad, nbv, ier
    integer :: k
    real(kind=8) :: pres, pres_point(27), coef_mult
    real(kind=8) :: pr
!
! --------------------------------------------------------------------------------------------------
!
    ASSERT(option.eq.'CHAR_MECA_PRES_R'.or.option.eq.'CHAR_MECA_EFON_R')
!
! - For membrane in small strain, we allow pressure only if it is null
    if(nomte(1:4).eq.'MEMB') then
        call jevech('PMATERC', 'L', imate)
        call rccoma(zi(imate), 'ELAS_MEMBRANE', 0, phenom, icodre)
! -     Only small strains work with ELAS_MEMBRANE behavior
        if (icodre .eq. 0) then
            param='PPRESSR'
            call tecach('NNO', param, 'L', iret, nval=8, itab=itab)
            if (iret.eq.0) then
                jad=itab(1)
                nbv=itab(2)
                ASSERT(itab(5).eq.1 .or. itab(5).eq.4)
                if (itab(5).eq.1) then
                    do k=1,nbv
                        if (zr(jad-1+k).ne.0.d0) then
                            call utmess('F', 'CALCUL_48')
                        endif
                    enddo
                else
                    do k=1,nbv
                        if (zk8(jad-1+k).ne.'&FOZERO') then
                            call fointe(' ', zk8(jad-1+k), 0, ' ', [0.d0], pr, ier)
                            if (ier.eq.0 .and. pr.eq.0.d0) then
                                ! tout va bien ...
                            else
                                call utmess('F', 'CALCUL_48')
                            endif
                        endif
                    enddo
                endif
            endif
        endif  
    endif
    
!
! - Finite element parameters
!
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idf,jgano=jgano)
!
! - IN fields
!
    call jevech('PGEOMER', 'L', j_geom)
!
! - OUT fields
!
    call jevech('PVECTUR', 'E', j_vect)
!
! - For pressure, no node affected -> 0
!
    if (option.eq.'CHAR_MECA_PRES_R') then
        call jevecd('PPRESSR', j_pres, 0.d0)
    elseif (option.eq.'CHAR_MECA_EFON_R') then
        call jevecd('PPREFFR', j_pres, 0.d0)
    endif
!
! - Multiplicative ratio for pressure (EFFE_FOND)
!
    coef_mult = 1.d0
    if (option.eq.'CHAR_MECA_EFON_R') then
        call jevech('PEFOND', 'L', j_effe)
        coef_mult = zr(j_effe-1+1)
    endif
!
! - Evaluation of pressure at Gauss points (from nodes)
!
    do kpg = 0, npg-1
        kdec = kpg*nno
        pres = 0.d0
        do n = 0, nno-1
            pres = pres + zr(j_pres+n) * zr(ivf+kdec+n)
        end do
        pres_point(kpg+1) = coef_mult * pres
    end do
!
! - Second member
!
    call nmpr3d_vect(nno, npg, zr(ipoids), zr(ivf), zr(idf), &
                     zr(j_geom), pres_point, zr(j_vect))
!
end subroutine
