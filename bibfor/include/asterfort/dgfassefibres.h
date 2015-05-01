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
    subroutine dgfassefibres(nboccasf, iinbgf, tousgroupesnom, tousgroupesnbf, maxmailgrp, &
                             ulnbnoeuds, ulnbmailles, nbfibres2, maxfibre2, ncarfi2, nbocctype1)
        integer :: nboccasf
        integer :: iinbgf
        integer :: maxmailgrp
        integer :: ulnbnoeuds
        integer :: ulnbmailles
        integer :: nbfibres2
        integer :: tousgroupesnbf(*)
        integer :: maxfibre2
        integer :: ncarfi2
        integer :: nbocctype1
        character(len=24) :: tousgroupesnom(*)
    end subroutine dgfassefibres
end interface
