subroutine dgelas(eb, nub, h, b, a, em, num, ef, nuf, icisai)
!
    implicit   none
!
    integer :: icisai
!
    real(kind=8) :: eb, nub, b, a, h
    real(kind=8) :: em, num, ef, nuf
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
! person_in_charge: sebastien.fayolle at edf.fr
! ----------------------------------------------------------------------
!
! BUT : DETERMINATION DES PARAMETRES ELASTIQUES
!
! IN:
!       EB     : MODULE D YOUNG DU BETON
!       NUB    : COEFF DE POISSON DU BETON
!       H      : EPAISSEUR DE LA PLAQUE
!       B      : SECTIONS DES ACIERS
!       A      :
! OUT:
!       EM     : MODULE D YOUNG EN MEMBRANE
!       EF     : MODULE D YOUNG EN FLEXION
!       EMC    : MODULE D YOUNG EN MEMBRANE POUR LE CISAILLEMENT
!       NUM    : COEFF DE POISSON EN MEMBRANE
!       NUF    : COEFF DE POISSON EN FLEXION
!       NUMC   : COEFF DE POISSON EN MEMBRANE POUR LE CISAILLEMENT
! ----------------------------------------------------------------------
!
! - DETERMINATION DES PARAMETRES ELASTIQUES EN MEMBRANE
    if (icisai .eq. 0) then
! - PAR ESSAI DE TRACTION
        em = b/h + eb*(b+eb*h)/((1.d0-nub**2)*b+eb*h)
        num = nub*eb*h/((1.d0-nub**2)*b+eb*h)
    else
! - PAR ESSAI DE CISAILLEMENT PUR DANS LE PLAN
        em = eb + b*(1.d0-nub)/h
        num = nub + b*(1.d0-nub**2)/eb/h
    endif
!
! - DETERMINATION DES PARAMETRES ELASTIQUES EN FLEXION
    ef = eb*(eb*h+12.d0*a)/(eb*h+12.d0*a*(1.d0-nub**2))+12.d0*a/h
    nuf = nub*eb*h/(eb*h+12.d0*a*(1.d0-nub**2))
!
end subroutine
