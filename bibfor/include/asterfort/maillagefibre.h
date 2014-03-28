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
    subroutine maillagefibre(nogfma, ulnbnoeuds, maxmailgrp, nbgf, vcoord, nbnoeuds, &
                             vigroup, vngroup, vmailgrp, vimailles, ulnbmailles, ncarma )
        character(len=8), intent(in) :: nogfma
        integer, intent(in) :: ulnbnoeuds
        integer, intent(in) :: maxmailgrp
        integer, intent(in) :: nbgf
        integer, intent(in) :: ulnbmailles
        real(kind=8), intent(in) :: vcoord(2*ulnbnoeuds)
        integer, intent(in) :: nbnoeuds
        integer, intent(in) :: vmailgrp(nbgf)
        integer, intent(in) :: vigroup(nbgf*maxmailgrp)
        character(len=24), intent(in) :: vngroup(nbgf)
        integer, intent(in) :: ncarma
        integer, intent(in) :: vimailles(ulnbmailles*ncarma)
    end subroutine maillagefibre
end interface
