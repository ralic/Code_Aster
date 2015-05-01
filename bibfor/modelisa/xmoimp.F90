subroutine xmoimp(nh8, nh20, np6, np15, np5,&
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
                  ntes10, nhed20,nped15,npyd13,&
                  nted10, nbhm, nchm)

! aslint: disable=W1504
    implicit none
!
#include "jeveux.h"
#include "asterfort/infdbg.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/utmess.h"
    integer :: nh8(15), nh20(7), np6(15), np15(7), np5(15), np13(7)
    integer :: nt4(15), nt10(7)
    integer :: ncpq4(15), ncpq8(7), ncpt3(15), ncpt6(7), ndpq4(15)
    integer :: ndpq8(7), ndpt3(15), ndpt6(7), nf4(11), nf8(7), nf3(11)
    integer :: nf6(7), npf2(11), npf3(7)
    integer :: naxt3(7), naxq4(7), naxq8(7), naxt6(7), nax2(7), nax3(7)
    integer :: nth8(7), ntp6(7), ntp5(7), ntt4(7), ntpq4(7), ntpt3(7)
    integer :: ntaq4(7), ntat3(7), ntf4(7), ntf3(7), ntpf2(7), ntax2(7)
!
    integer :: nhyq8(7), nhyt6(7), nhymq8(7), nhymt6(7), nhysq8(7)
    integer :: nhyst6(7), nhydq8(7), nhydt6(7), nphm(7)
    integer :: nhe20(7), nhem20(7), nhed20(7), nhes20(7), npe15(7)
    integer :: npem15(7), npes15(7), nped15(7), npy13(7), npym13(7)
    integer :: npys13(7), npyd13(7), nte10(7), ntes10(7)
    integer :: nted10(7), ntem10(7), nbhm(7), nchm(7)
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
!    BUT : IMPRIMER LES ELEMENT X-FEM
!
! ----------------------------------------------------------------------
!
!
!
!
    integer :: ifm, niv, nbelx
!
    call jemarq()
    call infmaj()
    call infdbg('XFEM', ifm, niv)
!
    write(ifm,*)'IMPRESSION DES ELEMENTS X-FEM '
    write(ifm,807)'TYPE','XH','XT','XHT','XHC','XTC','XHTC',&
     &'XH1','XH2','XH3','XH4','XH2C','XH3C','XH4C','XHC3'
!
!     ELEMENTS MECANIQUES
    if (nh8(7) .ne. 0) write(ifm, 810)'HEXA8', nh8(1), nh8(2), nh8(3), nh8(4), nh8(5), nh8(6),&
                       nh8(8), nh8(9), nh8(10), nh8(11), nh8(12), nh8(13), nh8(14), nh8(15)
    if (nh20(7) .ne. 0) write(ifm, 808)'HEXA20', nh20(1), nh20(2), nh20(3), nh20(4), nh20(5),&
                        nh20(6)
    if (np6(7) .ne. 0) write(ifm, 810)'PENTA6', np6(1), np6(2), np6(3), np6(4), np6(5), np6(6),&
                       np6(8), np6(9), np6(10), np6(11), np6(12), np6(13), np6(14), np6(15)
    if (np15(7) .ne. 0) write(ifm, 808)'PENTA15', np15(1), np15(2), np15(3), np15(4), np15(5),&
                        np15(6)
    if (np5(7) .ne. 0) write(ifm, 810)'PYRAM5', np5(1), np5(2), np5(3), np5(4), np5(5), np5(6),&
                       np5(8), np5(9), np5(10), np5(11), np5(12), np5(13), np5(14), np5(15)
    if (np13(7) .ne. 0) write(ifm, 808)'PYRAM13', np13(1), np13(2), np13(3), np13(4), np13(5),&
                        np13(6)
    if (nt4(7) .ne. 0) write(ifm, 810)'TETRA4', nt4(1), nt4(2), nt4(3), nt4(4), nt4(5), nt4(6),&
                       nt4(8), nt4(9), nt4(10), nt4(11), nt4(12), nt4(13), nt4(14), nt4(15)
    if (nt10(7) .ne. 0) write(ifm, 808)'TETRA10', nt10(1), nt10(2), nt10(3), nt10(4), nt10(5),&
                        nt10(6)
    if (ncpq4(7) .ne. 0) write(ifm, 810)'CP QUAD4', ncpq4(1), ncpq4(2), ncpq4(3), ncpq4(4),&
                         ncpq4(5), ncpq4(6), ncpq4(8), ncpq4(9), ncpq4(10), ncpq4(11), ncpq4(12),&
                         ncpq4(13), ncpq4(14), ncpq4(15)
    if (ncpq8(7) .ne. 0) write(ifm, 808)'CP QUAD8', ncpq8(1), ncpq8(2), ncpq8(3), ncpq8(4),&
                         ncpq8(5), ncpq8(6)
    if (ncpt3(7) .ne. 0) write(ifm, 810)'CP TRIA3', ncpt3(1), ncpt3(2), ncpt3(3), ncpt3(4),&
                         ncpt3(5), ncpt3(6), ncpt3(8), ncpt3(9), ncpt3(10), ncpt3(11), ncpt3(12),&
                         ncpt3(13), ncpt3(14), ncpt3(15)
    if (ncpt6(7) .ne. 0) write(ifm, 808)'CP TRIA6', ncpt6(1), ncpt6(2), ncpt6(3), ncpt6(4),&
                         ncpt6(5), ncpt6(6)
    if (ndpq4(7) .ne. 0) write(ifm, 810)'DP QUAD4', ndpq4(1), ndpq4(2), ndpq4(3), ndpq4(4),&
                         ndpq4(5), ndpq4(6), ndpq4(8), ndpq4(9), ndpq4(10), ndpq4(11), ndpq4(12),&
                         ndpq4(13), ndpq4(14)
    if (ndpq8(7) .ne. 0) write(ifm, 808)'DP QUAD8', ndpq8(1), ndpq8(2), ndpq8(3), ndpq8(4),&
                         ndpq8(5), ndpq8(6)
    if (ndpt3(7) .ne. 0) write(ifm, 810)'DP TRIA3', ndpt3(1), ndpt3(2), ndpt3(3), ndpt3(4),&
                         ndpt3(5), ndpt3(6), ndpt3(8), ndpt3(9), ndpt3(10), ndpt3(11), ndpt3(12),&
                         ndpt3(13), ndpt3(14), ndpt3(15)
    if (ndpt6(7) .ne. 0) write(ifm, 808)'DP TRIA6', ndpt6(1), ndpt6(2), ndpt6(3), ndpt6(4),&
                         ndpt6(5), ndpt6(6)
    if (naxq4(7) .ne. 0) write(ifm, 809)'AXI QUAD4', naxq4(1), naxq4(2), naxq4(3), naxq4(4),&
                         naxq4(5), naxq4(6)
!
    if (naxq8(7) .ne. 0) write(ifm, 808)'AXI QUAD8', naxq8(1), naxq8(2), naxq8(3), naxq8(4),&
                         naxq8(5), naxq8(6)
    if (naxt3(7) .ne. 0) write(ifm, 808)'AXI TRIA3', naxt3(1), naxt3(2), naxt3(3), naxt3(4),&
                         naxt3(5), naxt3(6)
!
    if (naxt6(7) .ne. 0) write(ifm, 808)'AXI TRIA6', naxt6(1), naxt6(2), naxt6(3), naxt6(4),&
                         naxt6(5), naxt6(6)
    if (nf4(7) .ne. 0) write(ifm, 809)'FACE4', nf4(1), nf4(2), nf4(3), nf4(4), nf4(5), nf4(6),&
                       nf4(8), nf4(9), nf4(10), nf4(11)
    if (nf8(7) .ne. 0) write(ifm,808)'FACE8' ,nf8(1) ,nf8(2) ,nf8(3)
    if (nf3(7) .ne. 0) write(ifm, 809)'FACE3', nf3(1), nf3(2), nf3(3), nf3(4), nf3(5), nf3(6),&
                       nf3(8), nf3(9), nf3(10), nf3(11)
    if (nf6(7) .ne. 0) write(ifm,808)'FACE6' ,nf6(1) ,nf6(2) ,nf6(3)
    if (npf2(7) .ne. 0) write(ifm, 809)'ARETE 2', npf2(1), npf2(2), npf2(3), npf2(4), npf2(5),&
                        npf2(6), npf2(8), npf2(9), npf2(10), npf2(11)
    if (npf3(7) .ne. 0) write(ifm,808)'ARETE 3' ,npf3(1) ,npf3(2) ,npf3(3)
    if (nax2(7) .ne. 0) write(ifm,808)'ARETE-AXI 2' ,nax2(1) ,nax2(2) ,nax2(3)
    if (nax3(7) .ne. 0) write(ifm,808)'ARETE-AXI 3' ,nax3(1) ,nax3(2) ,nax3(3)
!
!     ELEMENTS HM-XFEM (MECANIQUE)
    if (nhyq8(7) .ne. 0) write(ifm, 810)'HMDP QUAD8', nhyq8(1), nhyq8(2), nhyq8(3), nhyq8(4),&
                         nhyq8(5), nhyq8(6), 0, 0, 0, 0, 0, 0, 0
    if (nhymq8(7) .ne. 0) write(ifm, 810)'HMDP QUAD8M', nhymq8(1), nhymq8(2), nhymq8(3),&
                          nhymq8(4), nhymq8(5), nhymq8(6), 0, 0, 0, 0, 0, 0, 0
    if (nhysq8(7) .ne. 0) write(ifm, 810)'HMDP QUAD8S', nhysq8(1), nhysq8(2), nhysq8(3),&
                          nhysq8(4), nhysq8(5), nhysq8(6), 0, 0, 0, 0, 0, 0, 0
    if (nhydq8(7) .ne. 0) write(ifm, 810)'HMDP QUAD8D', nhydq8(1), nhydq8(2), nhydq8(3),&
                          nhydq8(4), nhydq8(5), nhydq8(6), 0, 0, 0, 0, 0, 0, 0
    if (nhyt6(7) .ne. 0) write(ifm, 810)'HMDP TRIA6', nhyt6(1), nhyt6(2), nhyt6(3), nhyt6(4),&
                         nhyt6(5), nhyt6(6), 0, 0, 0, 0, 0, 0, 0
    if (nhymt6(7) .ne. 0) write(ifm, 810)'HMDP TRIA6M', nhymt6(1), nhymt6(2), nhymt6(3),&
                          nhymt6(4), nhymt6(5), nhymt6(6), 0, 0, 0, 0, 0, 0, 0
    if (nhyst6(7) .ne. 0) write(ifm, 810)'HMDP TRIA6S', nhyst6(1), nhyst6(2), nhyst6(3),&
                          nhyst6(4), nhyst6(5), nhyst6(6), 0, 0, 0, 0, 0, 0, 0
    if (nhydt6(7) .ne. 0) write(ifm, 810)'HMDP TRIA6D', nhydt6(1), nhydt6(2), nhydt6(3),&
                          nhydt6(4), nhydt6(5), nhydt6(6), 0, 0, 0, 0, 0, 0, 0
    if (nphm(7) .ne. 0) write(ifm, 810)'HMDP SEG3', nphm(1), nphm(2), nphm(3), nphm(4), nphm(5),&
                        nphm(6), 0, 0, 0, 0, 0, 0, 0
                                                                                                
    if (nhe20(7) .ne. 0) write(ifm, 810)'HM3D HEXA20', nhe20(1), nhe20(2), nhe20(3), nhe20(4),&
                         nhe20(5), nhe20(6), 0, 0, 0, 0, 0, 0, 0
    if (nhem20(7) .ne. 0) write(ifm, 810)'HM3D HEXA20M', nhem20(1), nhem20(2), nhem20(3),&
                          nhem20(4), nhem20(5), nhem20(6), 0, 0, 0, 0, 0, 0, 0
    if (nhes20(7) .ne. 0) write(ifm, 810)'HM3D HEXA20S', nhes20(1), nhes20(2), nhes20(3),&
                          nhes20(4), nhes20(5), nhes20(6), 0, 0, 0, 0, 0, 0, 0
    if (nhed20(7) .ne. 0) write(ifm, 810)'HM3D HEXA20D', nhed20(1), nhed20(2), nhed20(3),&
                          nhed20(4), nhed20(5), nhed20(6), 0, 0, 0, 0, 0, 0, 0
    if (npe15(7) .ne. 0) write(ifm, 810)'HM3D PENTA15', npe15(1), npe15(2), npe15(3), npe15(4),&
                         npe15(5), npe15(6), 0, 0, 0, 0, 0, 0, 0
    if (npem15(7) .ne. 0) write(ifm, 810)'HM3D PENTA15M', npem15(1), npem15(2), npem15(3),&
                          npem15(4), npem15(5), npem15(6), 0, 0, 0, 0, 0, 0, 0
    if (npes15(7) .ne. 0) write(ifm, 810)'HM3D PENTA15S', npes15(1), npes15(2), npes15(3),&
                          npes15(4), npes15(5), npes15(6), 0, 0, 0, 0, 0, 0, 0
    if (nped15(7) .ne. 0) write(ifm, 810)'HM3D PENTA15D', nped15(1), nped15(2), nped15(3),&
                          nped15(4), nped15(5), nped15(6), 0, 0, 0, 0, 0, 0, 0

    if (npy13(7) .ne. 0) write(ifm, 810)'HM3D PYRAM13', npy13(1), npy13(2), npy13(3), npy13(4),&
                         npy13(5), npy13(6), 0, 0, 0, 0, 0, 0, 0
    if (npym13(7) .ne. 0) write(ifm, 810)'HM3D PYRAM13M', npym13(1), npym13(2), npym13(3),&
                          npym13(4), npym13(5), npym13(6), 0, 0, 0, 0, 0, 0, 0
    if (npys13(7) .ne. 0) write(ifm, 810)'HM3D PYRAM13S', npys13(1), npys13(2), npys13(3),&
                          npys13(4), npys13(5), npys13(6), 0, 0, 0, 0, 0, 0, 0
    if (npyd13(7) .ne. 0) write(ifm, 810)'HM3D PYRAM13D', npyd13(1), npyd13(2), npyd13(3),&
                          npyd13(4), npyd13(5), npyd13(6), 0, 0, 0, 0, 0, 0, 0

    if (nte10(7) .ne. 0) write(ifm, 810)'HM3D TETRA10', nte10(1), nte10(2), nte10(3), nte10(4),&
                         nte10(5), nte10(6), 0, 0, 0, 0, 0, 0, 0
    if (ntem10(7) .ne. 0) write(ifm, 810)'HM3D TETRA10M', ntem10(1), ntem10(2), ntem10(3),&
                          ntem10(4), ntem10(5), ntem10(6), 0, 0, 0, 0, 0, 0, 0 
    if (ntes10(7) .ne. 0) write(ifm, 810)'HM3D TETRA10S', ntes10(1), ntes10(2), ntes10(3),&
                          ntes10(4), ntes10(5), ntes10(6), 0, 0, 0, 0, 0, 0, 0 
    if (nted10(7) .ne. 0) write(ifm, 810)'HM3D TETRA10D', nted10(1), nted10(2), nted10(3),&
                          nted10(4), nted10(5), nted10(6), 0, 0, 0, 0, 0, 0, 0
    if (nbhm(7) .ne. 0) write(ifm, 810)'HM3D FACE8', nbhm(1), nbhm(2), nbhm(3), nbhm(4),&
                         nbhm(5), nbhm(6), 0, 0, 0, 0, 0, 0, 0
    if (nchm(7) .ne. 0) write(ifm, 810)'HM3D FACE6', nchm(1), nchm(2), nchm(3), nchm(4),&
                         nchm(5), nchm(6), 0, 0, 0, 0, 0, 0, 0
!     ELEMENTS THERMIQUES
    if (nth8(7) .ne. 0) write(ifm, 810)'HEXA8', nth8(1), nth8(2), nth8(3), nth8(4), nth8(5),&
                        nth8(6), 0, 0, 0, 0, 0, 0, 0
    if (ntp6(7) .ne. 0) write(ifm, 810)'PENTA6', ntp6(1), ntp6(2), ntp6(3), ntp6(4), ntp6(5),&
                        ntp6(6), 0, 0, 0, 0, 0, 0, 0
    if (ntp5(7) .ne. 0) write(ifm, 810)'PYRAM5', ntp5(1), ntp5(2), ntp5(3), ntp5(4), ntp5(5),&
                        ntp5(6), 0, 0, 0, 0, 0, 0, 0
    if (ntt4(7) .ne. 0) write(ifm, 810)'TETRA4', ntt4(1), ntt4(2), ntt4(3), ntt4(4), ntt4(5),&
                        ntt4(6), 0, 0, 0, 0, 0, 0, 0
    if (ntpq4(7) .ne. 0) write(ifm, 810)'PLAN QUAD4', ntpq4(1), ntpq4(2), ntpq4(3), ntpq4(4),&
                         ntpq4(5), ntpq4(6), 0, 0, 0, 0, 0, 0, 0
    if (ntpt3(7) .ne. 0) write(ifm, 810)'PLAN TRIA3', ntpt3(1), ntpt3(2), ntpt3(3), ntpt3(4),&
                         ntpt3(5), ntpt3(6), 0, 0, 0, 0, 0, 0, 0
!
    if (ntaq4(7) .ne. 0) write(ifm, 810)'AXI QUAD4', ntaq4(1), ntaq4(2), ntaq4(3), ntaq4(4),&
                         ntaq4(5), ntaq4(6), 0, 0, 0, 0, 0, 0, 0
    if (ntat3(7) .ne. 0) write(ifm, 810)'AXI TRIA3', ntat3(1), ntat3(2), ntat3(3), ntat3(4),&
                         ntat3(5), ntat3(6), 0, 0, 0, 0, 0, 0, 0
!
    if (ntf4(7) .ne. 0) write(ifm, 810)'FACE4', ntf4(1), ntf4(2), ntf4(3), ntf4(4), ntf4(5),&
                        ntf4(6), 0, 0, 0, 0, 0, 0, 0
    if (ntf3(7) .ne. 0) write(ifm, 810)'FACE3', ntf3(1), ntf3(2), ntf3(3), ntf3(4), ntf3(5),&
                        ntf3(6), 0, 0, 0, 0, 0, 0, 0
!
    if (ntpf2(7) .ne. 0) write(ifm, 810)'ARETE 2', ntpf2(1), ntpf2(2), ntpf2(3), ntpf2(4),&
                         ntpf2(5), ntpf2(6), 0, 0, 0, 0, 0, 0, 0
!
    if (ntax2(7) .ne. 0) write(ifm, 810)'ARETE-AXI 2', ntax2(1), ntax2(2), ntax2(3), ntax2(4),&
                         ntax2(5), ntax2(6), 0, 0, 0, 0, 0, 0, 0
!
    write(ifm,*)'  '
!
    nbelx = nh8(7) + nh20(7) + np6(7) + np15(7) + np5(7) + np13(7) + nt4(7) + nt10(7) + ncpq4(7) &
            &+ ncpq8(7) + ncpt3(7) + ncpt6(7) + ndpq4(7) + ndpq8(7) + ndpt3(7) + ndpt6(7) + naxq4&
            &(7) + naxq8(7) + naxt3(7) + naxt6(7) + nax2(7) + nax3(7) + npf2(7) + npf3(7) + nf3(7&
            &) + nf6(7) + nth8(7) + ntp6(7) + ntp5(7) + ntt4(7) + ntpq4(7) + ntpt3(7) + ntaq4(7) &
            &+ ntat3(7) + ntf4(7) + ntf3(7) + ntpf2(7) + ntax2(7) + nhyq8(7) + nhyt6(7) + nhymq8(&
            &7) + nhymt6(7) + nhysq8(7) + nhyst6(7) + nhydq8(7) + nhydt6(7) + nphm(7) + nhe20(7) &
            &+ npe15(7) + npy13(7) + nte10(7) + nhem20(7) + npem15(7) + npym13(7) + ntem10(7) + n&
            &hes20(7) + npes15(7) + npys13(7) + ntes10(7) + nhed20(7) + nped15(7) + npyd13(7) + n&
            &ted10(7) +nbhm(7) + nchm(7)
!
    if (nbelx .eq. 0) then
        call utmess('A', 'XFEM_16')
    endif
!
    807 format (5x,a19,2x,a6,2x,a6,2x,a6,2x,a6,2x,a6,2x,a6,&
     &        2x,a6,2x,a6,2x,a6,2x,a6,2x,a6,2x,a6,2x,a6,2x,a6)
    808 format (5x,a19,2x,i6,2x,i6,2x,i6,2x,i6,2x,i6,2x,i6)
    809 format (5x,a19,2x,i6,2x,i6,2x,i6,2x,i6,2x,i6,2x,i6,&
     &        i6,2x,i6,2x,i6,2x,i6,2x,i6)
    810 format (5x,a19,2x,i6,2x,i6,2x,i6,2x,i6,2x,i6,2x,i6,&
     &        i6,2x,i6,2x,i6,2x,i6,2x,i6,2x,i6,2x,i6,2x,i6,2x,i6)
!
    call jedema()
end subroutine
