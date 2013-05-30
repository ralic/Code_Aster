subroutine dgseui(em, num, ef, nuf, eb,&
                  nub, sytb, h, icisai, syt,&
                  syc, dxd, syf, drd, pelast,&
                  pelasf, icompr)
!
    implicit none
!
! PARAMETRES ENTRANTS
    include 'asterfort/u2mesr.h'
    integer :: icompr, icisai
    real(kind=8) :: em, num, ef, nuf, h
    real(kind=8) :: eb, nub, sytb, syc
!
! PARAMETRES SORTANTS
    real(kind=8) :: syt, dxd, syf, drd, pelast, pelasf
!
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
! person_in_charge: sebastien.fayolle at edf.fr
! ----------------------------------------------------------------------
!
! BUT : DETERMINATION DES SEUIL D ENDOMMAGEMENT
!
! IN:
!       ICOMPR : METHODE DE COMPRESSION
!       EM     : MODULE D YOUNG EN MEMBRANE
!       EF     : MODULE D YOUNG EN FLEXION
!       NUM    : COEFF DE POISSON EN MEMBRANE
!       NUF    : COEFF DE POISSON EN FLEXION
!       H      : EPAISSEUR DE LA PLAQUE
!       EB     : MODULE D YOUNG DU BETON
!       NUB    : COEFF DE POISSON DU BETON
!       SYTB   : LIMITE A LA TRACTION DU BETON
!       SYC    : SEUIL D'ENDOMMAGEMENT EN COMPRESSION
! OUT:
!       SYT    : SEUIL D'ENDOMMAGEMENT EN TRACTION
!       DXD    : DEPLACEMENT A L'APPARITION DE L'ENDOMMAGEMENT
!       SYF    : SEUIL D'ENDOMMAGEMENT EN FLEXION
!       DRD    : ROTATION A L'APPARITION DE L'ENDOMMAGEMENT
!       PELAST : PENTE ELASTIQUE EN TRACTION
!       PELASF : PENTE ELASTIQUE EN FLEXION
! ----------------------------------------------------------------------
    real(kind=8) :: rmesg(2), sycmax
!
! - DETERMINATION DES PARAMETRES EN MEMBRANE
! - SEUILS D'ENDOMMAGEMENT EN TRACTION PURE
    syt=sytb*em*h*(1.d0-nub**2)/(eb*(1.d0-nub*num))
    sycmax=sqrt((1.d0-num)*(1.d0+2.d0*num))/num*syt
!
    if ((abs(syc) .gt. sycmax) .and. (icompr .eq. 1)) then
        rmesg(1) = syc
        rmesg(2) = sycmax
        call u2mesr('F', 'ALGORITH6_2', 2, rmesg)
    endif
!
! - DEPLACEMENT A L'APPARITION DE L'ENDOMMAGEMENT
    dxd=syt/(h*em)
!
    if (icisai .eq. 1) then
! - CALCUL DE LA PENTE ELASTIQUE EN CISAILLEMENT PUR DANS LE PLAN
        pelast=em/(1.d0+num)/2.d0*h
    else
! - PENTE ELASTIQUE EN TRACTION
        pelast=em*h
    endif
!
! - DETERMINATION DES PARAMETRES EN FLEXION
! - SEUIL D'ENDOMMAGEMENT EN FLEXION PURE
    syf=sytb*ef*h**2*(1.d0-nub**2)/(6.d0*eb*(1.d0-nub*nuf))
! - ROTATION A L'APPARITION DE L'ENDOMMAGEMENT
    drd=12.d0*syf/(h**3*ef)
    pelasf=ef*h**3/12.d0
!
end subroutine
