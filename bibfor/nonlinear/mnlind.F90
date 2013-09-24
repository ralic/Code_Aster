subroutine mnlind(n, deb, cle, vect, ind)
    implicit none
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------------------
!
!     MODE_NON_LINE -- ROUTINE UTILITAIRE
!     -    -                -            -   -
! ----------------------------------------------------------------------
!
! RENVOIE L'INDICE OU SE TROUVE L'ENTIER RECHERCHER DANS UNE LISTE
! ----------------------------------------------------------------------
! IN      N      : I    : TAILLE DU VECTEUR
! IN      DEB    : I    : INDICE DE DEBUT DU VECTEUR NON TRONQUEE
! IN      CLE    : R    : CLE QUE L'ON RECHERCHE
! IN      VECT   : R8(N): LISTE D'INDICE
! IN      IND    : I    : INDICE OU SE TROUVE LA CLEF
! ----------------------------------------------------------------------
!
! ----------------------------------------------------------------------
! --- DECLARATION DES ARGUMENTS DE LA ROUTINE
! ----------------------------------------------------------------------
    integer :: n, deb, ind
    real(kind=8) :: cle, vect(n)
! ----------------------------------------------------------------------
! --- DECLARATION DES VARIABLES LOCALES
! ----------------------------------------------------------------------
    integer :: indt
    logical :: lstp
!
    if (n .lt. 0) then
        ind=-999
    else
        lstp=.true.
        indt=1
10      continue
        if (lstp) then
            if (abs(cle-vect(indt)) .lt. 1.d-8) then
                lstp=.false.
                ind=deb+indt
            else
                indt=indt+1
                if (indt .gt. n) then
                    lstp=.false.
                    ind=-999
                endif
            endif
            goto 10
        endif
    endif
!
end subroutine
