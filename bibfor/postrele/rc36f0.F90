subroutine rc36f0(nbsigr, nocc, saltij, saltm, trouve,&
                  isk, isl, i1a4, nk, nl)
    implicit   none
    integer :: nbsigr, nocc(*), isk, isl, i1a4, nl, nk
    real(kind=8) :: saltij(*), saltm
    logical :: trouve
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
! OUT : I1A4  : INDICE 1,2, 3 OU 4 CORESPONDANT AUX INDICES A ET B
!
!     ------------------------------------------------------------------
    integer :: i, k, l, i1, i2
    real(kind=8) :: salt
!     ------------------------------------------------------------------
!
! --- RECHERCHE DU SALT MAXI
!
    do 20 k = 1, nbsigr
!
        if (( nocc(2*(k-1)+1) .eq. 0 ) .and. ( nocc(2*(k-1)+2) .eq. 0 )) goto 20
        i1 = 4*nbsigr*(k-1)
!
        do 22 l = 1, nbsigr
!
            if (( nocc(2*(l-1)+1) .eq. 0 ) .and. ( nocc(2*(l-1)+2) .eq. 0 )) goto 22
            i2 = 4*(l-1)
!
            do 24 i = 1, 4
                salt = saltij(i1+i2+i)
                if (salt .gt. saltm) then
                    trouve = .true.
                    saltm = salt
                    i1a4 = i
                    isk = k
                    isl = l
                    if (i1a4 .eq. 1 .or. i1a4 .eq. 2) then
                        nl = nocc(2*(isl-1)+1)
                    else if (i1a4.eq.3 .or. i1a4.eq.4) then
                        nl = nocc(2*(isl-1)+2)
                    endif
                    if (i1a4 .eq. 1 .or. i1a4 .eq. 3) then
                        nk = nocc(2*(isk-1)+1)
                    else if (i1a4.eq.2 .or. i1a4.eq.4) then
                        nk = nocc(2*(isk-1)+2)
                    endif
                endif
24          continue
!
22      continue
!
20  end do
!
end subroutine
