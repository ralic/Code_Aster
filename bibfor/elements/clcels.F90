subroutine clcels(cequi, effm, effn, ht, enrobg,&
                  sigaci, sigbet, dnsinf, dnssup, sigmab,&
                  ierr)
!______________________________________________________________________
!
!     CC_ELS
!
!      DETERMINATION DES ARMATURES EN FLEXION COMPOSEE, CONDITIONS ELS
!
!      I CEQUI       COEFFICIENT D'EQUIVALENCE ACIER/BETON
!      I EFFM        MOMENT DE FLEXION
!      I EFFN        EFFORT NORMAL
!      I HT          EPAISSEUR DE LA COQUE
!      I SIGACI      CONTRAINTE ADMISSIBLE DANS L'ACIER
!      I SIGBET      CONTRAINTE ADMISSIBLE DANS LE BETON
!      O DNSINF      DENSITE DE L'ACIER INFERIEUR
!      O DNSSUP      DENSITE DE L'ACIER SUPERIEUR
!      O SIGMAB      CONTRAINTE DANS LE BETON
!      O IERR        CODE RETOUR (0 = OK)
!______________________________________________________________________
! ======================================================================
! COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
!
    real(kind=8) :: cequi
    real(kind=8) :: effm
    real(kind=8) :: effn
    real(kind=8) :: ht
    real(kind=8) :: enrobg
    real(kind=8) :: sigaci
    real(kind=8) :: sigbet
    real(kind=8) :: dnsinf
    real(kind=8) :: dnssup
    real(kind=8) :: sigmab
    integer :: ierr
!
!       EPAISSEUR UTILE DEPUIS L'ACIER JUSQU'A LA FIBRE SUP COMPRIMEE
    real(kind=8) :: hs
!       EPAISSEUR DEPUIS L'ACIER JUSQU'AU MILIEU DE LA SECTION
    real(kind=8) :: hu
!       EPAISSEUR RELATIVE DU BETON COMPRIME
    real(kind=8) :: alpha
!       MOMENT MESURE A PARTIR DE L'ACIER
    real(kind=8) :: ms
!       MOMENT REDUIT
    real(kind=8) :: mub
!
    real(kind=8) :: moment
    real(kind=8) :: tmp
    real(kind=8) :: tmp1
!
    integer :: i
!
    ierr = 0
!
    moment = -effm
!
    hs = ht - enrobg
    hu = 0.5d0 * ht - enrobg
!
    dnsinf = 0d0
    dnssup = 0d0
    sigmab = 0d0
!
    ms = abs(moment) - effn * hu
    if (ms .lt. 0d0) then
!         BETON ENTIEREMENT TENDU
        tmp = 0.5d0 * (effn + moment / hu)
        dnsinf = tmp
        dnssup = effn - tmp
    else
!         BETON PARTIELLEMENT COMPRIME
        mub = 1d0 + 2d0 * cequi * ms / (hs * hs * sigaci)
        alpha = 0.7d0
!
        do 10 i = 1, 5, 1
            tmp = 2d0 - 3d0 * mub * alpha + alpha * alpha * alpha
            tmp1 = -3d0 * mub + 3d0 * alpha * alpha
            alpha = alpha - tmp / tmp1
10      continue
!
        tmp1 = 1d0 - alpha
        sigmab = (sigaci / cequi) * tmp1 / alpha
!         PIVOT A (PIVOT C NON TRAITE )
        tmp = 0.5d0 * sigmab * hs * tmp1 + effn
!
        if (tmp .gt. 0d0) then
            if (0d0 .lt. moment) dnsinf = tmp
            if (moment .lt. 0d0) dnssup = tmp
        else
            sigmab = (-effn + 6d0 * abs(effm) / ht) / ht
        endif
        if (sigmab .gt. sigbet) then
            ierr = 1050
            goto 9999
        endif
    endif
!
    dnsinf = dnsinf / sigaci
    dnssup = dnssup / sigaci
!
9999  continue
end subroutine
