subroutine mmmtas(nbdm, ndim, nnl, nne, nnm,&
                  nbcps, matrcc, matree, matrmm, matrem,&
                  matrme, matrce, matrcm, matrmc, matrec,&
                  matrff, matrfe, matrfm, matrmf, matref,&
                  mmat)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
! aslint: disable=W1504
    implicit none
#include "asterf_types.h"
#include "asterfort/mmmtdb.h"
    integer :: nbdm, ndim, nnl, nne, nnm, nbcps
    real(kind=8) :: matrcc(9, 9)
    real(kind=8) :: matree(27, 27), matrmm(27, 27)
    real(kind=8) :: matrem(27, 27), matrme(27, 27)
    real(kind=8) :: matrce(9, 27), matrcm(9, 27)
    real(kind=8) :: matrec(27, 9), matrmc(27, 9)
    real(kind=8) :: matrff(18, 18)
    real(kind=8) :: matrfe(18, 27), matrfm(18, 27)
    real(kind=8) :: matrmf(27, 18), matref(27, 18)
    real(kind=8) :: mmat(81, 81)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! ASSEMBLAGE DES MATRICES
!
! ----------------------------------------------------------------------
!
!
! IN  NBDM   : NOMBRE DE COMPOSANTES/NOEUD DES DEPL+LAGR_C+LAGR_F
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  NNL    : NOMBRE DE NOEUDS LAGRANGE DE CONTACT
! IN  MATRCC : MATRICE ELEMENTAIRE LAGR_C/LAGR_C
! OUT MMAT   : MATRICE ELEMENTAIRE DE CONTACT/FROTTEMENT
!
! ----------------------------------------------------------------------
!
    integer :: ii, jj, kk, ll
    integer :: nbcpf
    integer :: inoc, inoe, inom, inof, icmp, idim
    integer :: inoe1, inoe2, inom1, inom2, idim1, idim2
    integer :: inoc1, inoc2, inof1, inof2, icmp1, icmp2
    aster_logical :: debug
!
! ----------------------------------------------------------------------
!
    nbcpf = nbcps - 1
    debug = .false.
!
! --- CONTACT/CONTACT
!
    do 10 inoc1 = 1, nnl
        do 11 inoc2 = 1, nnl
            ii = nbdm*(inoc1-1)+ndim+1
            jj = nbdm*(inoc2-1)+ndim+1
            kk = inoc1
            ll = inoc2
            mmat(ii,jj) = mmat(ii,jj)+matrcc(kk,ll)
            if (debug) call mmmtdb(matrcc(kk, ll), 'CC', ii, jj)
 11     continue
 10 end do
!
! --- CONTACT/ESCL
!
    do 20 inoc = 1, nnl
        do 21 inoe = 1, nne
            do 22 idim = 1, ndim
                ii = nbdm*(inoc-1)+ndim+1
                jj = nbdm*(inoe-1)+idim
                kk = inoc
                ll = ndim*(inoe-1)+idim
                mmat(ii,jj) = mmat(ii,jj)+matrce(kk,ll)
                if (debug) call mmmtdb(matrce(kk, ll), 'CE', ii, jj)
 22         continue
 21     continue
 20 end do
!
! --- ESCL/CONTACT
!
    do 23 inoc = 1, nnl
        do 24 inoe = 1, nne
            do 25 idim = 1, ndim
                ii = nbdm*(inoc-1)+ndim+1
                jj = nbdm*(inoe-1)+idim
                kk = inoc
                ll = ndim*(inoe-1)+idim
                mmat(jj,ii) = mmat(jj,ii)+matrec(ll,kk)
                if (debug) call mmmtdb(matrec(ll, kk), 'EC', jj, ii)
 25         continue
 24     continue
 23 end do
!
! --- CONTACT/MAIT
!
    do 30 inoc = 1, nnl
        do 31 inom = 1, nnm
            do 32 idim = 1, ndim
                ii = nbdm*(inoc-1)+ndim+1
                jj = nbdm*nne+ndim*(inom-1)+idim
                kk = inoc
                ll = ndim*(inom-1)+idim
                mmat(ii,jj) = mmat(ii,jj)+matrcm(kk,ll)
                if (debug) call mmmtdb(matrcm(kk, ll), 'CM', ii, jj)
 32         continue
 31     continue
 30 end do
!
! --- MAIT/CONTACT
!
    do 33 inoc = 1, nnl
        do 34 inom = 1, nnm
            do 35 idim = 1, ndim
                ii = nbdm*(inoc-1)+ndim+1
                jj = nbdm*nne+ndim*(inom-1)+idim
                kk = ndim*(inom-1)+idim
                ll = inoc
                mmat(jj,ii) = mmat(jj,ii)+matrmc(kk,ll)
                if (debug) call mmmtdb(matrmc(kk, ll), 'MC', jj, ii)
 35         continue
 34     continue
 33 end do
!
! --- ESCL/ESCL
!
    do 40 inoe1 = 1, nne
        do 41 inoe2 = 1, nne
            do 42 idim2 = 1, ndim
                do 43 idim1 = 1, ndim
                    ii = nbdm*(inoe1-1)+idim1
                    jj = nbdm*(inoe2-1)+idim2
                    kk = ndim*(inoe1-1)+idim1
                    ll = ndim*(inoe2-1)+idim2
                    mmat(ii,jj) = mmat(ii,jj)+matree(kk,ll)
                    if (debug) call mmmtdb(matree(kk, ll), 'EE', ii, jj)
 43             continue
 42         continue
 41     continue
 40 end do
!
! --- MAIT/MAIT
!
    do 50 inom1 = 1, nnm
        do 51 inom2 = 1, nnm
            do 52 idim2 = 1, ndim
                do 53 idim1 = 1, ndim
                    kk = ndim*(inom1-1)+idim1
                    ll = ndim*(inom2-1)+idim2
                    ii = nbdm*nne+ndim*(inom1-1)+idim1
                    jj = nbdm*nne+ndim*(inom2-1)+idim2
                    mmat(ii,jj) = mmat(ii,jj)+matrmm(kk,ll)
                    if (debug) call mmmtdb(matrmm(kk, ll), 'MM', ii, jj)
 53             continue
 52         continue
 51     continue
 50 end do
!
! --- ESCL/MAIT
!
    do 60 inoe = 1, nne
        do 61 inom = 1, nnm
            do 62 idim2 = 1, ndim
                do 63 idim1 = 1, ndim
                    kk = ndim*(inoe-1)+idim1
                    ll = ndim*(inom-1)+idim2
                    ii = nbdm*(inoe-1)+idim1
                    jj = nbdm*nne+ndim*(inom-1)+idim2
                    mmat(ii,jj) = mmat(ii,jj)+matrem(kk,ll)
                    if (debug) call mmmtdb(matrem(kk, ll), 'EM', ii, jj)
 63             continue
 62         continue
 61     continue
 60 end do
!
! --- MAIT/ESCL
!
    do 70 inoe = 1, nne
        do 71 inom = 1, nnm
            do 72 idim2 = 1, ndim
                do 73 idim1 = 1, ndim
                    ii = nbdm*nne+ndim*(inom-1)+idim2
                    jj = nbdm*(inoe-1)+idim1
                    kk = ndim*(inom-1)+idim2
                    ll = ndim*(inoe-1)+idim1
                    mmat(ii,jj) = mmat(ii,jj)+matrme(kk,ll)
                    if (debug) call mmmtdb(matrme(kk, ll), 'ME', ii, jj)
 73             continue
 72         continue
 71     continue
 70 end do
!
    if (nbcpf .eq. 0) goto 999
!
! --- FROTTEMENT/FROTTEMENT
!
    do 110 inof1 = 1, nnl
        do 111 inof2 = 1, nnl
            do 112 icmp1 = 1, nbcpf
                do 113 icmp2 = 1, nbcpf
                    ii = nbdm*(inof1-1)+ndim+1+icmp1
                    jj = nbdm*(inof2-1)+ndim+1+icmp2
                    kk = (ndim-1)*(inof1-1)+icmp1
                    ll = (ndim-1)*(inof2-1)+icmp2
                    mmat(ii,jj) = mmat(ii,jj)+matrff(kk,ll)
                    if (debug) call mmmtdb(matrff(kk, ll), 'FF', ii, jj)
113             continue
112         continue
111     continue
110 end do
!
! --- FROTTEMENT/ESCL
!
    do 120 inof = 1, nnl
        do 121 inoe = 1, nne
            do 122 icmp = 1, nbcpf
                do 123 idim = 1, ndim
                    kk = nbcpf*(inof-1)+icmp
                    ll = ndim*(inoe-1)+idim
                    ii = nbdm*(inof-1)+ndim+1+icmp
                    jj = nbdm*(inoe-1)+idim
                    mmat(ii,jj) = mmat(ii,jj)+matrfe(kk,ll)
                    if (debug) call mmmtdb(matrfe(kk, ll), 'FE', ii, jj)
123             continue
122         continue
121     continue
120 end do
!
! --- ESCL/FROTTEMENT
!
    do 220 inoe = 1, nne
        do 221 inof = 1, nnl
            do 222 icmp = 1, nbcpf
                do 223 idim = 1, ndim
                    kk = ndim*(inoe-1)+idim
                    ll = nbcpf*(inof-1)+icmp
                    ii = nbdm*(inoe-1)+idim
                    jj = nbdm*(inof-1)+ndim+1+icmp
                    mmat(ii,jj) = mmat(ii,jj)+matref(kk,ll)
                    if (debug) call mmmtdb(matrfe(kk, ll), 'FE', ii, jj)
223             continue
222         continue
221     continue
220 end do
!
! --- FROTTEMENT/MAITRE
!
    do 320 inof = 1, nnl
        do 321 inom = 1, nnm
            do 322 icmp = 1, nbcpf
                do 323 idim = 1, ndim
                    kk = nbcpf*(inof-1)+icmp
                    ll = ndim*(inom-1)+idim
                    ii = nbdm*(inof-1)+ndim+1+icmp
                    jj = nbdm*nne+ndim*(inom-1)+idim
                    mmat(ii,jj) = mmat(ii,jj)+matrfm(kk,ll)
                    if (debug) call mmmtdb(matrfm(kk, ll), 'FM', ii, jj)
323             continue
322         continue
321     continue
320 end do
!
! --- MAITRE/FROTTEMENT
!
    do 420 inom = 1, nnm
        do 421 inof = 1, nnl
            do 422 icmp = 1, nbcpf
                do 423 idim = 1, ndim
                    kk = ndim*(inom-1)+idim
                    ll = nbcpf*(inof-1)+icmp
                    ii = nbdm*nne+ndim*(inom-1)+idim
                    jj = nbdm*(inof-1)+ndim+1+icmp
                    mmat(ii,jj) = mmat(ii,jj)+matrmf(kk,ll)
                    if (debug) call mmmtdb(matrmf(kk, ll), 'MF', ii, jj)
423             continue
422         continue
421     continue
420 end do
999 continue
!
end subroutine
