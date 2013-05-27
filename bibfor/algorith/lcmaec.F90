subroutine lcmaec(fami, kpg, ksp, poum, nmater,&
                  imat, necoul, nbval, valres, nmat)
    implicit none
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! TOLE CRS_1404
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
!     ----------------------------------------------------------------
!     MONOCRISTAL : RECUPERATION DU MATERIAU A T(TEMPD) ET T+DT(TEMPF)
!                  MATER(*,2) = COEF ECRO CINE
!     ----------------------------------------------------------------
!     IN  IMAT   :  ADRESSE DU MATERIAU CODE
!         NMATER :  NOM DU MATERIAU
!         NMAT   :  DIMENSION  DE MATER
!         NECOUL :  NOM DE LA LOI D'ECOULEMENT
!         VALPAR :  VALEUR DES PARAMETRES
!         NOMPAR :  NOM DES PARAMETRES
!     OUT VALRES :  COEFFICIENTS MATERIAU A T
!         NBVAL  :  NOMBRE DE COEF MATERIAU LUS
!     ----------------------------------------------------------------
    include 'asterfort/lceqvn.h'
    include 'asterfort/rcvalb.h'
    integer :: kpg, ksp, nmat, nbval, imat
    real(kind=8) :: valres(nmat), vallue(nmat)
    character(len=8) :: nomres(nmat)
    character(len=*) :: fami, poum
    integer :: icodre(nmat)
    character(len=16) :: nmater, necoul
!     ----------------------------------------------------------------
!
    if (necoul .eq. 'MONO_CINE1') then
        nbval=1
        nomres(1)='D'
        call rcvalb(fami, kpg, ksp, poum, imat,&
                    nmater, necoul, 0, ' ', 0.d0,&
                    nbval, nomres, vallue, icodre, 1)
        call lceqvn(nbval, vallue, valres(2))
        nbval=nbval+1
!         PAR CONVENTION ECRO_CINE1 A LE NUMERO 1
        valres(1)=1
!
    endif
    if (necoul .eq. 'MONO_CINE2') then
        nbval=4
        nomres(1)='D'
        nomres(2)='GM'
        nomres(3)='PM'
        nomres(4)='C'
        call rcvalb(fami, kpg, ksp, poum, imat,&
                    nmater, necoul, 0, ' ', 0.d0,&
                    nbval, nomres, vallue, icodre, 1)
        call lceqvn(nbval, vallue, valres(2))
        nbval=nbval+1
!         PAR CONVENTION ECRO_CINE2 A LE NUMERO 2
        valres(1)=2
!
    endif
end subroutine
