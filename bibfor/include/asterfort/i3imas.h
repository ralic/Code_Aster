!
! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine i3imas(epsi, nil, tete, queue, succ,&
                      prec, desc, desctm, sgt, conex,&
                      vlc, coordo, sdrp1d, sdrpom, nbsgte)
        real(kind=8) :: epsi
        integer :: nil
        integer :: tete
        integer :: queue
        integer :: succ(*)
        integer :: prec(*)
        integer :: desc(*)
        integer :: desctm(*)
        real(kind=8) :: sgt(*)
        integer :: conex(*)
        integer :: vlc(*)
        real(kind=8) :: coordo(*)
        character(len=24) :: sdrp1d
        character(len=24) :: sdrpom
        integer :: nbsgte
    end subroutine i3imas
end interface
