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
    subroutine nmevel(sddisc, numins, defico, resoco, vale,&
                      nombcl, lsvimx, ldvres, linsta, lerrcv,&
                      lerror, conver)
        character(len=19) :: sddisc
        integer :: numins
        character(len=24) :: defico
        character(len=24) :: resoco
        character(len=19) :: vale(*)
        character(len=4) :: nombcl
        logical(kind=1) :: lsvimx
        logical(kind=1) :: ldvres
        logical(kind=1) :: linsta
        logical(kind=1) :: lerrcv
        logical(kind=1) :: lerror
        logical(kind=1) :: conver
    end subroutine nmevel
end interface
