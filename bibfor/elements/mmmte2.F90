subroutine mmmte2(ndim, nnl, nne, nnm, nbcpf,&
                  ndexcl, matrff, matrfe, matrfm, matref,&
                  matrmf)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    integer :: ndim, nne, nnl, nnm, nbcpf
    integer :: ndexcl(10)
    real(kind=8) :: matrff(18, 18)
    real(kind=8) :: matref(27, 18), matrfe(18, 27)
    real(kind=8) :: matrmf(27, 18), matrfm(18, 27)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - CALCUL)
!
! CALCUL DES MATRICES - MODIFICATIONS EXCL_FROT
!
! ----------------------------------------------------------------------
!
!
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  NNE    : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
! IN  NNM    : NOMBRE DE NOEUDS DE LA MAILLE MAITRE
! IN  NNL    : NOMBRE DE NOEUDS DE LAGRANGE
! IN  NBCPF  : NOMBRE DE COMPOSANTES/NOEUD DES LAGR_F
! IN  NDEXCL : TABLEAU DES NOEUDS CONCERNES
! OUT MATRFF : MATRICE ELEMENTAIRE LAGR_F/LAGR_F
! OUT MATRFE : MATRICE ELEMENTAIRE LAGR_F/DEPL_E
! OUT MATRFM : MATRICE ELEMENTAIRE LAGR_F/DEPL_M
! OUT MATREF : MATRICE ELEMENTAIRE DEPL_E/LAGR_F
! OUT MATRMF : MATRICE ELEMENTAIRE DEPL_M/LAGR_F
!
! ----------------------------------------------------------------------
!
    integer :: inoe, inom, inof, idim, jj, ii, l, icmp, i
!
! ----------------------------------------------------------------------
!
    do 10 i = 1, nnl
        if (ndexcl(i) .eq. 1) then
            do 11 l = 1, nbcpf
                if ((l.eq.2) .and. (ndexcl(10).eq.0)) then
                    goto 11
                endif
                ii = nbcpf*(i-1)+l
                matrff(ii,ii) = 1.d0
11          continue
        endif
10  end do
!
    do 20 inof = 1, nnl
        if (ndexcl(inof) .eq. 1) then
            do 21 inoe = 1, nne
                do 22 icmp = 1, nbcpf
                    if ((icmp.eq.2) .and. (ndexcl(10).eq.0)) then
                        goto 22
                    endif
                    do 23 idim = 1, ndim
                        ii = nbcpf*(inof-1)+icmp
                        jj = ndim*(inoe-1)+idim
                        matrfe(ii,jj) = 0.d0
23                  continue
22              continue
21          continue
        endif
20  end do
!
    do 30 inof = 1, nnl
        if (ndexcl(inof) .eq. 1) then
            do 31 inom = 1, nnm
                do 32 icmp = 1, nbcpf
                    if ((icmp.eq.2) .and. (ndexcl(10).eq.0)) then
                        goto 32
                    endif
                    do 33 idim = 1, ndim
                        ii = nbcpf*(inof-1)+icmp
                        jj = ndim*(inom-1)+idim
                        matrfm(ii,jj) = 0.d0
33                  continue
32              continue
31          continue
        endif
30  end do
!
    do 40 inof = 1, nnl
        if (ndexcl(inof) .eq. 1) then
            do 41 inoe = 1, nne
                do 42 icmp = 1, nbcpf
                    if ((icmp.eq.2) .and. (ndexcl(10).eq.0)) then
                        goto 42
                    endif
                    do 43 idim = 1, ndim
                        jj = nbcpf*(inof-1)+icmp
                        ii = ndim*(inoe-1)+idim
                        matref(ii,jj) = 0.d0
43                  continue
42              continue
41          continue
        endif
40  end do
!
    do 50 inof = 1, nnl
        if (ndexcl(inof) .eq. 1) then
            do 51 inom = 1, nnm
                do 52 icmp = 1, nbcpf
                    if ((icmp.eq.2) .and. (ndexcl(10).eq.0)) then
                        goto 52
                    endif
                    do 53 idim = 1, ndim
                        jj = nbcpf*(inof-1)+icmp
                        ii = ndim*(inom-1)+idim
                        matrmf(ii,jj) = 0.d0
53                  continue
52              continue
51          continue
        endif
50  end do
!
end subroutine
