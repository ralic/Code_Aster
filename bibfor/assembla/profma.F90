subroutine profma(nuz, solvez, base)
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
    implicit none
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/promor.h'
    character(len=14) :: nu
    character(len=19) :: solveu
    character(len=1) :: base
    character(len=*) :: nuz, solvez
! ----------------------------------------------------------------------
!     CONSTRUCTION DU PROFIL LIGNE DE CIEL OU MORSE DE LA MATRICE
!     A PARTIR DE LA NUMEROTATION
!
! IN K24 NUZ     : NOM DE LA NUMEROTATION
! IN K19 SOLVEZ  : NOM DE L'OBJET DE TYPE SOLVEUR
! IN K1  BASE    : BASE DE CREATION DU PROFIL DE STOCKAGE DE LA MATRICE
!   -------------------------------------------------------------------
!     ASTER INFORMATIONS:
!       20/11/03 (OB): MODIF POUR SOLVEUR FETI.
!----------------------------------------------------------------------
!
!
!
    character(len=24) :: metres
    integer :: islvk, islvr
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    nu = nuz
    solveu = solvez
!
    call jemarq()
    call jeveuo(solveu//'.SLVK', 'L', islvk)
    metres = zk24(islvk)
    call jeveuo(solveu//'.SLVR', 'L', islvr)
!
    if (metres .ne. 'FETI') then
        call promor(nu, base)
    else
!       RIEN, PAS DE STRUCTURE DE STOCKAGE.
!       LE .FETN A DEJA ETE CONSTITUE DANS NUMERO.F
    endif
!
    call jedema()
end subroutine
