subroutine xmoini(nh8, nh20, np6, np15, np5,&
                  np13, nt4, nt10, ncpq4, ncpq8,&
                  ncpt3, ncpt6, ndpq4, ndpq8, ndpt3,&
                  ndpt6, nf4, nf8, nf3, nf6,&
                  npf2, npf3, naxt3, naxq4, naxq8,&
                  naxt6, nax2, nax3, nth8, ntp6,&
                  ntp5, ntt4, ntpq4, ntpt3, ntaq4,&
                  ntat3, ntf4, ntf3, ntpf2, ntax2,&
                  nhyq8, nhyt6, nhymq8, nhymt6, nhysq8,&
                  nhyst6, nhydq8, nhydt6, nphm, nhe20,&
                  npe15, npy13, nte10, nhem20, npem15,&
                  npym13, ntem10, nhes20, npes15, npys13,&
                  ntes10, nhed20,nped15, npyd13,&
                  nted10, nbhm, nchm)

!
! aslint: disable=W1504
    implicit none
!
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    integer :: nh8(15), nh20(7), np6(15), np15(7), np5(15), np13(7)
    integer :: nt4(15), nt10(7)
    integer :: ncpq4(15), ncpq8(7), ncpt3(15), ncpt6(7), ndpq4(15)
    integer :: ndpq8(7), ndpt3(15), ndpt6(7), nf4(11), nf8(7), nf3(11)
    integer :: nf6(7), npf2(11), npf3(7)
    integer :: naxt3(7), naxq4(7), naxq8(7), naxt6(7), nax2(7), nax3(7)
    integer :: nth8(7), ntp6(7), ntp5(7), ntt4(7), ntpq4(7), ntpt3(7)
    integer :: ntaq4(7), ntat3(7), ntf4(7), ntf3(7), ntpf2(7), ntax2(7)
!
    integer :: nhyq8(11), nhyt6(11), nhymq8(7), nhymt6(7), nhysq8(7)
    integer :: nhyst6(7), nhydq8(7), nhydt6(7), nphm(11)
    integer :: nhe20(11), nhem20(7), nhed20(7), nhes20(7), npe15(11)
    integer :: npem15(7), npes15(7), nped15(7), npy13(11), npym13(7)
    integer :: npys13(7), npyd13(7), nte10(11), ntes10(7)
    integer :: nted10(7), ntem10(7), nbhm(11), nchm(11)
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
! person_in_charge: samuel.geniaut at edf.fr
!
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM APPELEE PAR MODI_MODELE_XFEM (OP0113)
!
!    BUT : INITIALISER LES COMPTEURS DES NOMBRES D'ELEMENTS
!
! ----------------------------------------------------------------------
!
!
!
!
    integer :: i
!
    call jemarq()
!
    do i = 1, 7
        nh8(i)=0
        nh20(i)=0
        np6(i)=0
        np15(i)=0
        np5(i)=0
        np13(i)=0
        nt4(i)=0
        nt10(i)=0
        ncpq4(i)=0
        ncpq8(i)=0
        ncpt3(i)=0
        ncpt6(i)=0
        ndpq4(i)=0
        ndpq8(i)=0
        ndpt3(i)=0
        ndpt6(i)=0
        nf4(i)=0
        nf8(i)=0
        nf3(i)=0
        nf6(i)=0
        npf2(i)=0
        npf3(i)=0
        naxt3(i)=0
        naxq4(i)=0
        naxq8(i)=0
        naxt6(i)=0
        nax2(i)=0
        nax3(i)=0
        naxt3(i)=0
        nth8(i)=0
        ntp6(i)=0
        ntp5(i)=0
        ntt4(i)=0
        ntpq4(i)=0
        ntpt3(i)=0
        ntaq4(i)=0
        ntat3(i)=0
        ntf4(i)=0
        ntf3(i)=0
        ntpf2(i)=0
        ntax2(i)=0
        nhyq8(i)=0
        nhyt6(i)=0
        nhymq8(i)=0
        nhymt6(i)=0
        nhysq8(i)=0
        nhyst6(i)=0
        nhydq8(i)=0
        nhydt6(i)=0
        nphm(i)=0
        nhe20(i)=0
        nhem20(i)=0
        nhed20(i)=0
        nhes20(i)=0
        npe15(i)=0
        npem15(i)=0
        nped15(i)=0
        npes15(i)=0
        npy13(i)=0
        npym13(i)=0
        npys13(i)=0
        npyd13(i)=0
        nte10(i)=0
        ntem10(i)=0
        nted10(i)=0
        ntes10(i)=0
        nbhm(i)=0
        nchm(i)=0
         
    end do
    do i = 8, 11
        nh8(i)=0
        np6(i)=0
        np5(i)=0
        nt4(i)=0
        ncpt3(i)=0
        ncpq4(i)=0
        ndpq4(i)=0
        ndpt3(i)=0
        nf4(i)=0
        nf3(i)=0
        npf2(i)=0
        nhyq8(i)=0
        nhyt6(i)=0
        nphm(i)=0
        nhe20(i)=0
        npe15(i)=0
        npy13(i)=0
        nte10(i)=0
        nbhm(i)=0
        nchm(i)=0
    end do
    do i = 12, 15
        nh8(i)=0
        np6(i)=0
        np5(i)=0
        nt4(i)=0
        ncpt3(i)=0
        ncpq4(i)=0
        ndpq4(i)=0
        ndpt3(i)=0
    end do
!
!
!
!
    call jedema()
end subroutine
