subroutine jedisp(n, tab)
! person_in_charge: j-pierre.lefebvre at edf.fr
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
    include 'jeveux_private.h'
    integer :: n, tab(*)
! ----------------------------------------------------------------------
! RENVOIE DANS LE TABLEAU TAB LES LONGUEURS MAX DISPONIBLES DANS LA
! PARTITION MEMOIRE NUMERO 1
!
! IN  N      : TAILLE DU TABLEAU TAB
! IN  TAB    : TAILLE DE SEGMENT DE VALEURS DISPONIBLE
!
! SI L'ALLOCATION DYNAMIQUE EST UTILISEE LA ROUTINE RENVOIE L'ENTIER
! MAXIMUM DANS LES N VALEURS DU TABLEAU TAB
!
!
! ----------------------------------------------------------------------
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
! ----------------------------------------------------------------------
    integer :: lbis, lois, lols, lor8, loc8
    common /ienvje/  lbis , lois , lols , lor8 , loc8
    integer :: istat
    common /istaje/  istat(4)
    integer :: ldyn, lgdyn, nbdyn, nbfree
    common /idynje/  ldyn , lgdyn , nbdyn , nbfree
    real(kind=8) :: mxdyn, mcdyn, mldyn, vmxdyn, vmet, lgio
    common /r8dyje/ mxdyn, mcdyn, mldyn, vmxdyn, vmet, lgio(2)
! ----------------------------------------------------------------------
    integer :: k
!
! DEB ------------------------------------------------------------------
    do 1 k = 1, n
        tab(k) = 0
 1  end do
!
! --- ON DONNE LA VALEUR ASSOCIEE A LA MEMOIRE DYNAMIQUE DISPONIBLE
!
    if (ldyn .eq. 1) then
        tab(1) = nint((vmxdyn-mcdyn)/lois)
    endif
! FIN-------------------------------------------------------------------
end subroutine
