function in_liste_entier(val,liste,indx)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! --------------------------------------------------------------------------------------------------
!
!     si 'val' est dans 'liste' ==> retourne true
!
!     si indx présent ==> index de la valeur dans la liste
!
! --------------------------------------------------------------------------------------------------
! person_in_charge: jean-luc.flejou at edf.fr
!
    implicit none
    integer :: val, liste(:)
    integer,intent(out),optional :: indx
    logical :: in_liste_entier
! --------------------------------------------------------------------------------------------------
    integer :: ii,indx0
! --------------------------------------------------------------------------------------------------
    in_liste_entier = .false.
    indx0 = 0
!
    if ( present(indx) ) then
        indx=indx0
    endif
    do ii = lbound(liste,1), ubound(liste,1)
        indx0 = indx0 + 1
        if ( val.eq.liste(ii) ) then
            in_liste_entier = .true.
            if ( present(indx) ) then
                indx=indx0
            endif
            exit
        endif
    enddo
end
