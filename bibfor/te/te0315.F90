subroutine te0315(option, nomte)
    implicit none
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!.......................................................................
!
!     BUT: CALCUL DES VECTEURS ELEMENTAIRES DE FLUX FLUIDE EN MECANIQUE
!          ELEMENTS ISOPARAMETRIQUES 1D
!
!          OPTION : 'CHAR_THER_ACCE_R 'OU 'CHAR_THER_ACCE_X'
!                    OU 'CHAR_THER_ACCE_Y'
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
#include "jeveux.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/rcvalb.h"
#include "asterfort/vff2dn.h"
!
    integer :: icodre(1)
    character(len=8) :: fami, poum
    character(len=16) :: nomte, option
    real(kind=8) :: poids, nx, ny, norm(2), acloc(2, 3)
    real(kind=8) :: acc(2, 4), flufn(4)
    integer :: ipoids, ivf, idfde, igeom
    integer :: nno, kp, npg, ivectt, imate
    integer :: ldec, kpg, spt
    logical(kind=1) :: laxi
!
!
!-----------------------------------------------------------------------
    integer :: i, iacce, idim, itemp, jgano, k, ndim
    integer :: nnos
    real(kind=8) :: r, rho(1)
!-----------------------------------------------------------------------
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
    laxi = .false.
    if (lteatt('AXIS','OUI')) laxi = .true.
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PVECTTR', 'E', ivectt)
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
!
    call rcvalb(fami, kpg, spt, poum, zi(imate),&
                ' ', 'THER', 0, ' ', [0.d0],&
                1, 'RHO_CP', rho, icodre, 1)
!
    if (option(16:16) .eq. 'R') then
        call jevech('PACCELR', 'L', iacce)
    else
        if ((option(16:16).eq.'X') .or. (option(16:16).eq.'Y')) then
            call jevech('PTEMPER', 'L', itemp)
        endif
    endif
!
    k = 0
    do 20 i = 1, nno
        if (option(16:16) .eq. 'R') then
            do 10 idim = 1, 2
                k = k + 1
                acloc(idim,i) = zr(iacce+k-1)
10          continue
        else if ((option(16:16).eq.'X')) then
            k = k + 1
            acloc(1,i) = zr(itemp+k-1)
            acloc(2,i) = 0.d0
        else if (option(16:16).eq.'Y') then
            k = k + 1
            acloc(1,i) = 0.d0
            acloc(2,i) = zr(itemp+k-1)
        endif
20  end do
!
    do 30 i = 1, nno
        zr(ivectt+i-1) = 0.d0
30  end do
!
!     BOUCLE SUR LES POINTS DE GAUSS
!
    do 70 kp = 1, npg
        ldec = (kp-1)*nno
!
        nx = 0.d0
        ny = 0.d0
!        --- ON CALCULE L ACCEL AU POINT DE GAUSS
        acc(1,kp) = 0.d0
        acc(2,kp) = 0.d0
        do 40 i = 1, nno
            acc(1,kp) = acc(1,kp) + acloc(1,i)*zr(ivf+ldec+i-1)
            acc(2,kp) = acc(2,kp) + acloc(2,i)*zr(ivf+ldec+i-1)
40      continue
!
        call vff2dn(ndim, nno, kp, ipoids, idfde,&
                    zr(igeom), nx, ny, poids)
        norm(1) = nx
        norm(2) = ny
        flufn(kp) = 0.d0
!
! CALCUL DU FLUX FLUIDE NORMAL AU POINT DE GAUSS
!
        flufn(kp) = acc(1,kp)*norm(1) + acc(2,kp)*norm(2)
!
! CAS AXISYMETRIQUE
!
        if (laxi) then
            r = 0.d0
            do 50 i = 1, nno
                r = r + zr(igeom+2* (i-1))*zr(ivf+ldec+i-1)
50          continue
            poids = poids*r
        endif
!
        do 60 i = 1, nno
            zr(ivectt+i-1) = zr(ivectt+i-1) + poids*flufn(kp)*rho(1)*zr( ivf+ldec+i-1)
60      continue
70  end do
!
end subroutine
