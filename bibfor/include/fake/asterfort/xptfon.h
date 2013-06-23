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
interface
    subroutine xptfon(noma, ndim, nmafon, cnslt, cnsln,&
                      cnxinv, jmafon, nxptff, jfon, nfon,&
                      jbas, jtail, fiss, goinop, listpt,&
                      orient)
        character(len=8) :: noma
        integer :: ndim
        integer :: nmafon
        character(len=19) :: cnslt
        character(len=19) :: cnsln
        character(len=19) :: cnxinv
        integer :: jmafon
        integer :: nxptff
        integer :: jfon
        integer :: nfon
        integer :: jbas
        integer :: jtail
        character(len=8) :: fiss
        logical :: goinop
        character(len=19) :: listpt
        logical :: orient
    end subroutine xptfon
end interface
