subroutine inslri(nbx, nbn, lister, listei, valr,&
                  vali)
    implicit none
    integer :: nbx, nbn
    integer :: listei(nbx), vali
    real(kind=8) :: lister(nbx), valr
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!  Insere VALR dans LISTER
!     LISTER est classe du plus grand au plus petit
!     LISTEI contient les VALI
!
!  IN
!     NBX    : nombre maximum de valeur dans les listes
!     VALR   : valeur reelle a inserer dans LISTER
!     VALI   : valeur entiere
!
!  OUT/IN
!     NBN    : nombre actualise de valeur dans les listes
!              Doit etre initialise a 0 avant l'appel
!     LISTER : liste actualisee des reels
!     LISTEI : liste actualisee des entiers
!
    integer :: ii, indx
!
    if (nbn .eq. 0) then
        nbn = 1
        listei(1) = vali
        lister(1) = valr
    else
        indx = nbn+1
        do 71,ii = nbn,1,-1
        if (valr .gt. lister(ii)) indx = ii
71      continue
        if (nbn .lt. nbx) nbn = nbn + 1
        do 72,ii = nbn,indx+1,-1
        listei(ii) = listei(ii-1)
        lister(ii) = lister(ii-1)
72      continue
        if (indx .le. nbx) then
            listei(indx) = vali
            lister(indx) = valr
        endif
    endif
end subroutine
