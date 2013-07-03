subroutine te0550(option, nomte)
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
    implicit none
!
!     BUT: CALCUL DES MATRICES ELEMENTAIRES EN MECANIQUE
!          CORRESPONDANT A UNE IMPEDANCE IMPOSEE
!          SUR DES FACES D'ELEMENTS ISOPARAMETRIQUES 2D
!
!          OPTION : 'IMPE_ABSO'
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
#include "jeveux.h"
!
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/matini.h"
#include "asterfort/rcvalb.h"
#include "asterfort/vff2dn.h"
    integer :: icodre
    character(len=8) :: fami, poum
    character(len=16) :: nomte, option
    real(kind=8) :: nx, ny, poids, a(6, 6)
    real(kind=8) :: vites(6)
    integer :: ipoids, ivf, idfde, igeom, imate
    integer :: ndi, nno, kp, npg
    integer :: ldec, kpg, spt
    logical :: laxi
!
!
!-----------------------------------------------------------------------
    integer :: i, ii, ivectu, ivien, ivite, j, jgano
    integer :: jj, ndim, nnos
    real(kind=8) :: celer, r, r8b
!-----------------------------------------------------------------------
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
    ndi = 2*nno
    laxi = .false.
    if (lteatt(' ','AXIS','OUI')) laxi = .true.
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PVITPLU', 'L', ivite)
    call jevech('PVITENT', 'L', ivien)
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    call rcvalb(fami, kpg, spt, poum, zi(imate),&
                ' ', 'FLUIDE', 0, ' ', r8b,&
                1, 'CELE_R', celer, icodre, 1)
    if (celer .lt. 1.d-1) goto 110
!
    call jevech('PVECTUR', 'E', ivectu)
!
! --- INITIALISATION DU VECTEUR DE CORRECTION
    do 10 i = 1, ndi
        zr(ivectu+i-1) = 0.d0
10  end do
!
! --- INITIALISATION DE LA MATRICE D'IMPEDANCE
    call matini(6, 6, 0.d0, a)
!
!     BOUCLE SUR LES POINTS DE GAUSS
!
    do 100 kp = 1, npg
        ldec = (kp-1)*nno
        nx = 0.0d0
        ny = 0.0d0
        call vff2dn(ndim, nno, kp, ipoids, idfde,&
                    zr(igeom), nx, ny, poids)
!%
        if (laxi) then
            r = 0.d0
            do 40 i = 1, nno
                r = r + zr(igeom+2* (i-1))*zr(ivf+ldec+i-1)
40          continue
            poids = poids*r
        endif
!%
        do 60 i = 1, nno
!
            do 50 j = 1, nno
                ii = 2*i
                jj = 2*j - 1
!
                a(ii,jj) = a(ii,jj) - poids/celer*zr(ivf+ldec+i-1)* zr(ivf+ldec+j-1)
!
50          continue
60      continue
!
!     CALCUL DE LA VITESSE ABSOLUE
        do 70 i = 1, ndi
            vites(i) = zr(ivite+i-1) + zr(ivien+i-1)
70      continue
!
        do 90 i = 1, ndi
            do 80 j = 1, ndi
                zr(ivectu+i-1) = zr(ivectu+i-1) - a(i,j)*vites(j)
80          continue
90      continue
!
100  end do
110  continue
!
end subroutine
