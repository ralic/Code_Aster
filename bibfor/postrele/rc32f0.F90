subroutine rc32f0(nbsigr, nocc, saltij, saltm, trouve,&
                  isk, isl, nk, nl)
    implicit   none
    integer :: nbsigr, nocc(*), isk, isl, nl, nk
    real(kind=8) :: saltij(*), saltm
    logical :: trouve
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2007  EDF R&D               WWW.CODE-ASTER.ORG
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
!     CALCUL DU FACTEUR D'USAGE POUR LES SITUATIONS DE PASSAGE
!     RECHERCHE DU SALT MAXI SUPERIEUR A 0.
! OUT : SALTM : VALEUR MAXIMUM DANS LA MATRICE DES SALT
! OUT : ISK   : SITATION K DONNANT LE SALT MAXI
! OUT : ISL   : SITATION L DONNANT LE SALT MAXI
! OUT : NK    : NB D'OCCURENCE DE LA SITATION K
! OUT : NL    : NB D'OCCURENCE DE LA SITATION L
!
!     ------------------------------------------------------------------
    integer :: k, l
    real(kind=8) :: salt
!     ------------------------------------------------------------------
!
! --- RECHERCHE DU SALT MAXI
!
    do 20 k = 1, nbsigr
!
        if (nocc(k) .eq. 0) goto 20
!
        do 22 l = 1, nbsigr
!
            if (nocc(l) .eq. 0) goto 22
!
            salt = saltij(nbsigr*(k-1)+l)
!
            if (salt .gt. saltm) then
                trouve = .true.
                saltm = salt
                isk = k
                isl = l
                nl = nocc(isl)
                nk = nocc(isk)
            endif
!
22      continue
!
20  end do
!
end subroutine
