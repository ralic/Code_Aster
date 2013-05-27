subroutine op0022()
    implicit none
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ------------------------------------------------------------------
!
!     COMMANDE : DEFI_LIST_ENTI
!
!
!     ------------------------------------------------------------------
    include 'jeveux.h'
    include 'asterc/getres.h'
    include 'asterc/getvtx.h'
    include 'asterfort/assert.h'
    include 'asterfort/dlidef.h'
    include 'asterfort/dliext.h'
    include 'asterfort/infmaj.h'
    include 'asterfort/infniv.h'
    include 'asterfort/liimpr.h'
    include 'asterfort/titre.h'
    integer :: nv, niv, ifm
    character(len=8) :: resu
    character(len=16) :: concep, nomcmd, opera
    integer :: iarg
!     ------------------------------------------------------------------
!
    call infmaj()
    call infniv(ifm, niv)
!
    call getvtx(' ', 'OPERATION', 0, iarg, 1,&
                opera, nv)
    call getres(resu, concep, nomcmd)
!
    if (opera .eq. 'DEFI') then
        call dlidef()
!
    else if (opera.eq.'NUME_ORDRE') then
        call dliext()
!
    else
        call assert(.false.)
    endif
!
!
!     --- TITRE ---
    call titre()
!
!     --- IMPRESSION ---
    if (niv .gt. 1) call liimpr(resu, niv, 'MESSAGE')
!
end subroutine
