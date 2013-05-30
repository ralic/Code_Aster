subroutine op0057()
    implicit none
!
! person_in_charge: sebastien.fayolle at edf.fr
!.......................................................................
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!-----------------------------------------------------------------------
!       OPERATEUR DEFI_GLRC
!       DETERMINE LES PARAMETRES HOMOGENEISEES DES LOIS GLRC_DAMAGE
!       ET GLRC_DM A PARTIR DES PROPRIETES DU BETON ET DES COUCHES
!       D ACIER
!-----------------------------------------------------------------------
    include 'asterc/getvtx.h'
    include 'asterfort/assert.h'
    include 'asterfort/dglrda.h'
    include 'asterfort/dglrdm.h'
    integer :: ibid
    character(len=16) :: relat
    integer :: iarg
!
    call getvtx(' ', 'RELATION', 1, iarg, 1,&
                relat, ibid)
!
    if (relat(1:11) .eq. 'GLRC_DAMAGE') then
        call dglrda()
    else if (relat(1:7) .eq. 'GLRC_DM') then
        call dglrdm()
    else
        call assert(.false.)
    endif
!
end subroutine
