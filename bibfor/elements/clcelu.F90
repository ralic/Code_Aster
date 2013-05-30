subroutine clcelu(piva, pivb, effm, effn, ht,&
                  enrobg, sigaci, sigbet, dnsinf, dnssup,&
                  epsilb, ierr)
!______________________________________________________________________
!
!     CC_ELU
!
!      DETERMINATION DES ARMATURES EN FLEXION COMPOSEE, CONDITIONS ELU
!
!      I PIVA        VALEUR DU PIVOT A
!      I PIVB        VALEUR DU PIVOT B = DEFORMATION MAXI. DU BETON
!      I EFFM        MOMENT DE FLEXION
!      I EFFN        EFFORT NORMAL
!      I HT          EPAISSEUR DE LA COQUE
!      I SIGACI      CONTRAINTE ADMISSIBLE DANS L'ACIER
!      I SIGBET      CONTRAINTE ADMISSIBLE DANS LE BETON
!      O DNSINF      DENSITE DE L'ACIER INFERIEUR
!      O DNSSUP      DENSITE DE L'ACIER SUPERIEUR
!      O EPSILB      DEFORMATION DU BETON
!      O IERR        CODE RETOUR (0 = OK)
!
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
!
    implicit none
!
    real(kind=8) :: piva
    real(kind=8) :: pivb
    real(kind=8) :: effm
    real(kind=8) :: effn
    real(kind=8) :: ht
    real(kind=8) :: enrobg
    real(kind=8) :: sigaci
    real(kind=8) :: sigbet
    real(kind=8) :: dnsinf
    real(kind=8) :: dnssup
    real(kind=8) :: epsilb
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
    epsilb = 0d0
!
    ms = abs(moment) - effn * hu
    if (ms .lt. 0d0) then
!         BETON ENTIEREMENT TENDU
        tmp = 0.5d0 * (effn + moment / hu)
        dnsinf = tmp
        dnssup = effn - tmp
    else
!         BETON PARTIELLEMENT COMPRIME
        mub = ms / (hs * hs * sigbet)
        if (mub .lt. 0.48d0) then
            alpha = 1d0 - sqrt(1d0 - 2d0 * mub)
            epsilb = piva * alpha / (1d0 - alpha)
            tmp = ms / (hs * (1d0 - alpha / 2d0)) + effn
!            PIVOT A (PIVOT C NON TRAITE )
            if (tmp .gt. 0d0) then
                if (epsilb .lt. pivb) then
                    if (0d0 .lt. moment) dnsinf = tmp
                    if (moment .lt. 0d0) dnssup = tmp
                else
                    ierr = 1010
                    goto 9999
                endif
            endif
        else
            ierr = 1020
            goto 9999
        endif
    endif
    dnsinf = dnsinf / sigaci
    dnssup = dnssup / sigaci
!
9999  continue
end subroutine
