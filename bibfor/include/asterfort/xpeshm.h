!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine xpeshm(nno, nnop, nnops, ndim, nddls,&
                      nddlm, npg, igeom, jpintt, jpmilt, jlsn,&
                      ivf, ipoids, idfde, ivectu, ipesa,&
                      heavt, lonch, cnset, rho, axi,&
                      yaenrm)
        integer :: ndim
        integer :: nnop
        integer :: nno
        integer :: nnops
        integer :: nddls
        integer :: nddlm
        integer :: npg
        integer :: igeom
        integer :: jpintt
        integer :: jpmilt
        integer :: jlsn
        integer :: ivf
        integer :: ipoids
        integer :: idfde
        integer :: ivectu
        integer :: ipesa
        integer :: heavt(36)
        integer :: lonch(10)
        integer :: cnset(128)
        real(kind=8) :: rho
        aster_logical :: axi
        integer :: yaenrm
    end subroutine xpeshm
end interface 
