subroutine inschn(andi, ndi, xadj, adjncy, chaine,&
                  nouv, place, debut)
! person_in_charge: olivier.boiteau at edf.fr
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
    integer :: andi, xadj(*), ndi, chaine(*), nouv(*), adjncy(*)
    integer :: place(*)
!     INSERTION DANS LA CHAINE,
!     DES VOISINS DE NDI,(DE NUMERO SUPERIEUR)
!     QUI NE SONT PAS ENCORE DANS LA CHAINE (PLACE(NDJ)=0)
    integer :: j, suiv, cour, ndj
    integer :: debut
!
    do 150 j = xadj(andi), xadj(andi+1) - 1
        ndj = nouv(adjncy(j))
        if (ndj .gt. ndi) then
            if (place(ndj) .eq. 0) then
                suiv= debut
145              continue
                if (suiv .lt. ndj) then
                    cour = suiv
                    suiv = chaine(cour)
                    goto 145
                endif
                if (suiv .gt. ndj) then
                    chaine(cour) = ndj
                    chaine(ndj) = suiv
                    place(ndj)=1
                endif
            endif
        endif
150  continue
end subroutine
