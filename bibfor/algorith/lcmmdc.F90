subroutine lcmmdc(coeft, ifa, nmat, nbcomm, alphap,&
                  is, ceff, dcdals)
    implicit none
    integer :: ifa, nmat, nbcomm(nmat, 3), is
    real(kind=8) :: coeft(*), alphap(12), ceff, dcdals
! ----------------------------------------------------------------------
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
! person_in_charge: jean-michel.proix at edf.fr
! ======================================================================
!  CALCUL DE LA FONCTION Ceffectif POUR LA LOI D'ECOULEMENT  DD-CFC
!       IN  COEFT   :  PARAMETRES MATERIAU
!           IFA     :  NUMERO DE FAMILLE
!           NBCOMM  :  NOMBRE DE COEF MATERIAU PAR FAMILLE
!           NMAT    :  NOMBRE DE MATERIAUX
!           ALPHAP  :  ALPHA =RHO*B**2 (TOTAL) A T+DT
!     OUT:
!           CEFF    :  Coefficeint pour passer de A_ij a Aeff_ij
!     ----------------------------------------------------------------
    real(kind=8) :: alpha, beta, rhoref, omegat
    integer :: iei, i
!     ----------------------------------------------------------------
!
    iei=nbcomm(ifa,3)
    alpha =coeft(iei+1)
    beta  =coeft(iei+2)
    rhoref=coeft(iei+3)
    ceff=1.d0
    dcdals=0.d0
    if (alpha .gt. 0.d0) then
        omegat=0.d0
        do 10 i = 1, 12
            if (alphap(i) .gt. 0.d0) then
                omegat=omegat+alphap(i)
            endif
10      continue
!        PARTIE POSITIVE
        if (omegat .gt. 0.d0) then
            ceff=0.2d0+0.8d0*log(alpha*sqrt(omegat))/ log(alpha*beta*&
            sqrt(rhoref))
            if (alphap(is) .gt. 0.d0) then
                dcdals=0.8d0/2.d0/log(alpha*beta*sqrt(rhoref))/omegat
            endif
        endif
    endif
!
end subroutine
