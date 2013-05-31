subroutine xmoimp(nh8, nh20, np6, np15, np5,&
                  np13, nt4, nt10, ncpq4, ncpq8,&
                  ncpt3, ncpt6, ndpq4, ndpq8, ndpt3,&
                  ndpt6, nf4, nf8, nf3, nf6,&
                  npf2, npf3, naxt3, naxq4, naxq8,&
                  naxt6, nax2, nax3, nth8, ntp6,&
                  ntp5, ntt4, ntpq4, ntpt3, ntaq4,&
                  ntat3, ntf4, ntf3, ntpf2, ntax2)
! aslint: disable=W1504
    implicit none
!
    include 'jeveux.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/infmaj.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/u2mess.h'
    integer :: nh8(14), nh20(7), np6(14), np15(7), np5(14), np13(7)
    integer :: nt4(14), nt10(7)
    integer :: ncpq4(14), ncpq8(7), ncpt3(14), ncpt6(7), ndpq4(14)
    integer :: ndpq8(7), ndpt3(14), ndpt6(7), nf4(11), nf8(7), nf3(11)
    integer :: nf6(7), npf2(11), npf3(7)
    integer :: naxt3(7), naxq4(7), naxq8(7), naxt6(7), nax2(7), nax3(7)
    integer :: nth8(7), ntp6(7), ntp5(7), ntt4(7), ntpq4(7), ntpt3(7)
    integer :: ntaq4(7), ntat3(7), ntf4(7), ntf3(7), ntpf2(7), ntax2(7)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
     &'XH1','XH2','XH3','XH4','XH2C','XH3C','XH4C'
!
!     ELEMENTS MECANIQUES
    if (nh8(7) .ne. 0) write(ifm, 810)'HEXA8', nh8(1), nh8(2), nh8(3), nh8(4), nh8(5), nh8(6),&
                       nh8(8), nh8(9), nh8(10), nh8(11), nh8(12), nh8(13), nh8(14)
    if (nh20(7) .ne. 0) write(ifm, 808)'HEXA20', nh20(1), nh20(2), nh20(3), nh20(4), nh20(5),&
                        nh20(6)
    if (np6(7) .ne. 0) write(ifm, 810)'PENTA6', np6(1), np6(2), np6(3), np6(4), np6(5), np6(6),&
                       np6(8), np6(9), np6(10), np6(11), np6(12), np6(13), np6(14)
    if (np15(7) .ne. 0) write(ifm, 808)'PENTA15', np15(1), np15(2), np15(3), np15(4), np15(5),&
                        np15(6)
    if (np5(7) .ne. 0) write(ifm, 810)'PYRAM5', np5(1), np5(2), np5(3), np5(4), np5(5), np5(6),&
                       np5(8), np5(9), np5(10), np5(11), np5(12), np5(13), np5(14)
    if (np13(7) .ne. 0) write(ifm, 808)'PYRAM13', np13(1), np13(2), np13(3), np13(4), np13(5),&
                        np13(6)
    if (nt4(7) .ne. 0) write(ifm, 810)'TETRA4', nt4(1), nt4(2), nt4(3), nt4(4), nt4(5), nt4(6),&
                       nt4(8), nt4(9), nt4(10), nt4(11), nt4(12), nt4(13), nt4(14)
    if (nt10(7) .ne. 0) write(ifm, 808)'TETRA10', nt10(1), nt10(2), nt10(3), nt10(4), nt10(5),&
                        nt10(6)
    if (ncpq4(7) .ne. 0) write(ifm, 810)'CP QUAD4', ncpq4(1), ncpq4(2), ncpq4(3), ncpq4(4),&
                         ncpq4(5), ncpq4(6), ncpq4(8), ncpq4(9), ncpq4(10), ncpq4(11), ncpq4(12),&
                         ncpq4(13), ncpq4(14)
    if (ncpq8(7) .ne. 0) write(ifm, 808)'CP QUAD8', ncpq8(1), ncpq8(2), ncpq8(3), ncpq8(4),&
                         ncpq8(5), ncpq8(6)
    if (ncpt3(7) .ne. 0) write(ifm, 810)'CP TRIA3', ncpt3(1), ncpt3(2), ncpt3(3), ncpt3(4),&
                         ncpt3(5), ncpt3(6), ncpt3(8), ncpt3(9), ncpt3(10), ncpt3(11), ncpt3(12),&
                         ncpt3(13), ncpt3(14)
    if (ncpt6(7) .ne. 0) write(ifm, 808)'CP TRIA6', ncpt6(1), ncpt6(2), ncpt6(3), ncpt6(4),&
                         ncpt6(5), ncpt6(6)
    if (ndpq4(7) .ne. 0) write(ifm, 810)'DP QUAD4', ndpq4(1), ndpq4(2), ndpq4(3), ndpq4(4),&
                         ndpq4(5), ndpq4(6), ndpq4(8), ndpq4(9), ndpq4(10), ndpq4(11), ndpq4(12),&
                         ndpq4(13), ndpq4(14)
    if (ndpq8(7) .ne. 0) write(ifm, 808)'DP QUAD8', ndpq8(1), ndpq8(2), ndpq8(3), ndpq8(4),&
                         ndpq8(5), ndpq8(6)
    if (ndpt3(7) .ne. 0) write(ifm, 810)'DP TRIA3', ndpt3(1), ndpt3(2), ndpt3(3), ndpt3(4),&
                         ndpt3(5), ndpt3(6), ndpt3(8), ndpt3(9), ndpt3(10), ndpt3(11), ndpt3(12),&
                         ndpt3(13), ndpt3(14)
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
    write(ifm,*)'  '
!
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
    nbelx = nh8(7) + nh20(7) + np6(7) + np15(7) + np5(7) + np13(7) + nt4(7) + nt10(7) + ncpq4(7) &
            &+ ncpq8(7) + ncpt3(7) + ncpt6(7) + ndpq4(7) + ndpq8(7) + ndpt3(7) + ndpt6(7) + naxq4&
            &(7) + naxq8(7) + naxt3(7) + naxt6(7) + nax2(7) + nax3(7) + npf2(7) + npf3(7) + nf3(7&
            &) + nf6(7) + nth8(7) + ntp6(7) + ntp5(7) + ntt4(7) + ntpq4(7) + ntpt3(7) + ntaq4(7) &
            &+ ntat3(7) + ntf4(7) + ntf3(7) + ntpf2(7) + ntax2(7)
!
    if (nbelx .eq. 0) call u2mess('F', 'XFEM_16')
!
    807 format (5x,a19,2x,a6,2x,a6,2x,a6,2x,a6,2x,a6,2x,a6,&
     &        2x,a6,2x,a6,2x,a6,2x,a6,2x,a6,2x,a6,2x,a6)
    808 format (5x,a19,2x,i6,2x,i6,2x,i6,2x,i6,2x,i6,2x,i6)
    809 format (5x,a19,2x,i6,2x,i6,2x,i6,2x,i6,2x,i6,2x,i6,&
     &        i6,2x,i6,2x,i6,2x,i6,2x,i6)
    810 format (5x,a19,2x,i6,2x,i6,2x,i6,2x,i6,2x,i6,2x,i6,&
     &        i6,2x,i6,2x,i6,2x,i6,2x,i6,2x,i6,2x,i6,2x,i6)
!
    call jedema()
end subroutine
