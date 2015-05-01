subroutine pipeei(ndim, axi, nno1, nno2, npg,&
                  wref, vff1, vff2, dffr2, geom,&
                  ang, mat, compor, lgpg, ddlm,&
                  ddld, ddl0, ddl1, dtau, vim,&
                  iu, im, copilo)
!
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
!
! aslint: disable=W1504
    implicit none
#include "asterf_types.h"
#include "asterc/r8vide.h"
#include "asterfort/eicine.h"
#include "asterfort/pipeou.h"
#include "asterfort/pipeex.h"
#include "asterfort/pipetc.h"
#include "asterfort/r8inir.h"
#include "asterfort/utmess.h"
    aster_logical :: axi
    integer :: ndim, nno1, nno2, npg, mat, lgpg, iu(3, 18), im(3, 9)
    real(kind=8) :: vff1(nno1, npg), vff2(nno2, npg), geom(ndim, nno2)
    real(kind=8) :: wref(npg)
    real(kind=8) :: ddlm(2*nno1*ndim+nno2*ndim), ddld(2*nno1*ndim+nno2*ndim)
    real(kind=8) :: ddl0(2*nno1*ndim+nno2*ndim), ddl1(2*nno1*ndim+nno2*ndim)
    real(kind=8) :: vim(lgpg, npg), dffr2(ndim-1, nno2, npg), ang(*)
    real(kind=8) :: dtau, copilo(5, npg)
    character(len=16) :: compor
!
!-----------------------------------------------------------------------
!
!  PILOTAGE PRED_ELAS POUR LES ELEMENTS D'INTERFACE
!
!-----------------------------------------------------------------------
    integer :: g, n, i, j, kk
    real(kind=8) :: mup(3), sup(3), mud(3), sud(3), wg, b(3, 3, 18)
!-----------------------------------------------------------------------
!
!
    call r8inir(3, 0.d0, sup, 1)
    call r8inir(3, 0.d0, sud, 1)
    call r8inir(3, 0.d0, mup, 1)
    call r8inir(3, 0.d0, mud, 1)
    call r8inir(5*npg, 0.d0, copilo, 1)
!
    do 10 g = 1, npg
!
! -- INITIALISATION DES ELEMENTS CINEMATIQUES
!
        call eicine(ndim, axi, nno1, nno2, vff1(1, g),&
                    vff2(1, g), wref(g), dffr2(1, 1, g), geom, ang,&
                    wg, b)
!
        do 150 i = 1, ndim
            sup(i) = 0.d0
            sud(i) = 0.d0
            do 160 j = 1, ndim
                do 161 n = 1, 2*nno1
                    kk = iu(j,n)
                    sup(i) = sup(i) + b(i,j,n) * (ddlm(kk)+ddld(kk)+ ddl0(kk))
                    sud(i) = sud(i) + b(i,j,n) * ddl1(kk)
161             continue
160         continue
150     continue
!
        do 170 i = 1, ndim
            mup(i) = 0.d0
            mud(i) = 0.d0
            do 180 n = 1, nno2
                kk = im(i,n)
                mup(i) = mup(i) + vff2(n,g) * (ddlm(kk)+ddld(kk)+ddl0( kk))
                mud(i) = mud(i) + vff2(n,g) * ddl1(kk)
180         continue
170     continue
!
!
! -- APPEL DU PILOTAGE PRED_ELAS SPECIFIQUE A LA LOI DE COMPORTEMENT
!
        copilo(5,g) = r8vide()
        if (compor .eq. 'CZM_TAC_MIX') then
            call pipetc(mat, sup, sud, mup, mud,&
                        vim(1, g), dtau, copilo(1, g))
        else if (compor.eq.'CZM_OUV_MIX') then
            call pipeou(mat, sup, sud, mup, mud,&
                        vim(1, g), dtau, copilo(1, g))
        else if (compor.eq.'CZM_EXP_MIX') then
            call pipeex(mat, sup, sud, mup, mud,&
                        vim(1, g), dtau, copilo(1, g))
        else
            call utmess('F', 'MECANONLINE_59')
        endif
!
 10 end do
!
end subroutine
