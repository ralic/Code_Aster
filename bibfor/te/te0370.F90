subroutine te0370(option, nomte)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!.......................................................................
!
!     BUT: CALCUL DES MATRICES DE RIGIDITE  ELEMENTAIRES EN MECANIQUE
!          ELEMENTS 2D DE COUPLAGE PESANTEUR-SURFACE LIBRE D'UN FLUIDE
!
!          OPTION : 'RIGI_MECA'
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
    implicit none
#include "jeveux.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/rcvalb.h"
!
    character(len=8) :: fami
    character(len=16) :: nomte, option
    real(kind=8) :: a(2, 2, 27, 27)
    real(kind=8) :: rho
    integer :: igeom, imate
    integer :: i, j, k, l, ik, ijkl, ldec, kdec, ino, jno
    integer :: ndim, nno, ipg, nnos, jgano
    integer :: ipoids, ivf, idfrde, imatuu
    real(kind=8) :: poids
    real(kind=8) :: pesa, jac, zero
    real(kind=8) :: dxde, dxdk, dyde, dydk
    real(kind=8) :: b(54, 54), ul(54), c(1485)
    integer :: ivectu, jcret, nno2, nt2, n1, n2, nn, kpg, spt
    integer :: codret(2)
    character(len=8) :: nompar(2)
    real(kind=8) :: valpar(2)
!
!
!-----------------------------------------------------------------------
    integer :: ideplm, ideplp, npg2
!-----------------------------------------------------------------------
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg2,jpoids=ipoids,jvf=ivf,jdfde=idfrde,jgano=jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    zero = 0.d0
    fami='FPG1'
    kpg=1
    spt=1
    nompar(1) = 'RHO'
    nompar(2) = 'PESA_Z'
!
! --- CARACTERISTIQUES MATERIAUX
!
    call rcvalb(fami, kpg, spt, '+', zi(imate),&
                ' ', 'FLUIDE', 0, ' ', [0.d0],&
                2, nompar, valpar, codret, 1)
    rho = valpar(1)
    pesa = valpar(2)
!
!
!     INITIALISATION DE LA MATRICE
!
    do 112 k = 1, 2
        do 112 l = 1, 2
            do 112 i = 1, nno
                do 112 j = 1, i
                    a(k,l,i,j) = 0.d0
112              continue
!
!     BOUCLE SUR LES POINTS DE GAUSS
!
    do 113 ipg = 1, npg2
!
        kdec = (ipg-1)*nno*ndim
        ldec = (ipg-1)*nno
!
        dxde=0.d0
        dxdk=0.d0
        dyde=0.d0
        dydk=0.d0
        do 100 i = 1, nno
            dxde=dxde+zr(igeom+3*(i-1))*zr(idfrde+kdec+(i-1)*ndim)
            dxdk=dxdk+zr(igeom+3*(i-1))*zr(idfrde+kdec+(i-1)*ndim+1)
            dyde=dyde+zr(igeom+3*(i-1)+1)*zr(idfrde+kdec+(i-1)*ndim)
            dydk=dydk+zr(igeom+3*(i-1)+1)*zr(idfrde+kdec+(i-1)*ndim+1)
!
100      continue
        jac = dxde*dydk - dxdk*dyde
        poids = abs(jac)*zr(ipoids+ipg-1)
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!        CALCUL DU TERME RHO * G * Z * Z   C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
        do 130 ino = 1, nno
            do 140 jno = 1, ino
                a(2,2,ino,jno) = a(2,2,ino,jno) + poids * rho * pesa * zr(ivf+ldec+ino-1) * zr(iv&
                                 &f+ldec+jno-1)
!
140          continue
130      continue
113  end do
!
! PASSAGE DU STOCKAGE RECTANGULAIRE AU STOCKAGE TRIANGULAIRE
!
    ijkl = 0
    ik = 0
    do 160 k = 1, 2
        do 160 l = 1, 2
            do 162 i = 1, nno
                ik = ((2*i+k-3) * (2*i+k-2)) / 2
                do 164 j = 1, i
                    ijkl = ik + 2 * (j-1) + l
                    c(ijkl) = a(k,l,i,j)
164              continue
162          continue
160      continue
!
    nno2 = nno*2
    nt2 = nno* (nno2+1)
    if (option(1:9) .ne. 'FULL_MECA' .and. option(1:9) .ne. 'RIGI_MECA') goto 9998
    if (option .eq. 'RIGI_MECA_HYST') then
        call jevech('PMATUUC', 'E', imatuu)
        do 115 i = 1, nt2
            zc(imatuu+i-1) = dcmplx(c(i),zero)
115      continue
    else
        call jevech('PMATUUR', 'E', imatuu)
        do 114 i = 1, nt2
            zr(imatuu+i-1) = c(i)
114      continue
    endif
9998  continue
!
    if (option .ne. 'FULL_MECA' .and. option .ne. 'RAPH_MECA' .and. option .ne. 'FORC_NODA') &
    goto 9999
    call jevech('PVECTUR', 'E', ivectu)
    call jevech('PDEPLMR', 'L', ideplm)
    call jevech('PDEPLPR', 'L', ideplp)
    do 111 i = 1, nno2
        zr(ivectu+i-1) = 0.d0
        ul(i) = zr(ideplm+i-1) + zr(ideplp+i-1)
111  end do
!
    nn = 0
    do 120 n1 = 1, nno2
        do 121 n2 = 1, n1
            nn = nn + 1
            b(n1,n2) = c(nn)
            b(n2,n1) = c(nn)
121      continue
120  end do
!
    do 131 n1 = 1, nno2
        do 132 n2 = 1, nno2
            zr(ivectu+n1-1) = zr(ivectu+n1-1) + b(n1,n2)*ul(n2)
132      continue
131  end do
!
9999  continue
    if (option(1:9) .eq. 'FULL_MECA' .or. option .eq. 'RAPH_MECA') then
        call jevech('PCODRET', 'E', jcret)
        zi(jcret) = 0
    endif
!
end subroutine
