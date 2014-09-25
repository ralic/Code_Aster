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
    subroutine xfocoh(jbas, jconx1, jconx2, jcoor, jfon,&
                      cnsln, chgrn, chgrt, noma, listpt, ndim,&
                      nfon, nxptff, orient, nbmai)
        integer :: jbas
        integer :: jconx1
        integer :: jconx2
        integer :: jcoor
        integer :: jfon
        character(len=19) :: cnsln
        character(len=19) :: chgrn
        character(len=19) :: chgrt
        character(len=8) :: noma
        character(len=19) :: listpt
        integer :: ndim
        integer :: nfon
        integer :: nxptff
        aster_logical :: orient
        integer :: nbmai
    end subroutine xfocoh
end interface 
