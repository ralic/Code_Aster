subroutine tbtrik(ndim, tabcha, tabint)
    implicit   none
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/wkvect.h'
    integer :: ndim, tabint(*)
    character(len=*) :: tabcha(*)
! ----------------------------------------------------------------------
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
!     FONCTION:
!     RANGEMENT DES CHAINES DE CARACTERES DU TABLEAU "TABCHA"
!     DANS L'ORDRE CROISSANT.
!-----------------------------------------------------------------------
! IN  NDIM   : I  : DIMENSION DU TABNLEAU TABCHA.
! IN  TABCHA : K  : TABLEAU CONTENANT DES CHAINES DE CARACTERES A RANGER
!                   DANS L'ORDRE CROISSANT.
! OUT TABINT : I  : TABLEAU D'ENTIERS CONTENANT LES POSITIONS
!                   DANS LE TABLEAU  TABCHA DANS L'ORDRE CROISSANT.
!-----------------------------------------------------------------------
! ----------------------------------------------------------------------
    integer :: imin, j0, j1, i, j, lmasq
!-----------------------------------------------------------------------
    call jemarq()
!
!     --- ON DEMASQUE TOUS LES ELEMENTS DU TABLAEU A TRIER ---
!
    call wkvect('&&TBTRIK.MASQ', 'V V I', ndim, lmasq)
!
    j0 = 1
    do 10 i = 1, ndim
!        --- RECHERCHE DU PREMIER ELEMENT NON MASQUE ---
        do 20 j = j0, ndim
            if (zi(lmasq+j-1) .eq. 0) then
                j1 = j
                goto 22
            endif
20      continue
!
22      continue
!
!        -- RECHERCHE DU PLUS PETIT ELEMENT NON MASQUE --
        j0 = j1
        imin = j1
        do 30 j = j0+1, ndim
            if (zi(lmasq+j-1) .eq. 0 .and. tabcha(j) .lt. tabcha(imin)) imin = j
30      continue
!
!        -- RANGEMENT DU IEME ELEMENT ET MISE A JOUR DU MASQUE --
        tabint(i) = imin
        zi(lmasq+imin-1) = 1
!
10  end do
!
    call jedetr('&&TBTRIK.MASQ')
!
    call jedema()
end subroutine
