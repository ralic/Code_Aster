subroutine te0555(option, nomte)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!.......................................................................
    implicit none
!
!     BUT: CALCUL DES MATRICES ELEMENTAIRES EN MECANIQUE
!          CORRESPONDANT A UNE IMPEDANCE IMPOSEE
!          SUR DES FACES D'ELEMENTS ISOPARAMETRIQUES 3D
!
!          OPTION : 'IMPE_ABSO'
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
#include "jeveux.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/matini.h"
#include "asterfort/rcvalb.h"
!
    integer :: icodre(1)
    character(len=8) :: fami, poum
    character(len=16) :: nomres(1)
    character(len=16) :: nomte, option
    real(kind=8) :: nx, ny, nz, sx(9, 9), sy(9, 9), sz(9, 9), jac
    real(kind=8) :: valres(1), celer, a(18, 18), vites(18)
    integer :: ipoids, ivf, idfdx, idfdy, igeom, imate
    integer :: ndim, nno, ndi, ipg, npg2
    integer :: idec, jdec, kdec, ldec
    integer :: ivite, nnos, jgano, kpg, spt
!
!
!-----------------------------------------------------------------------
    integer :: i, ii, ino, ivectu, ivien, j, jj
    integer :: jno, mater
!-----------------------------------------------------------------------
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg2,jpoids=ipoids,jvf=ivf,jdfde=idfdx,jgano=jgano)
    idfdy = idfdx + 1
    ndi = 2*nno
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PVITPLU', 'L', ivite)
    call jevech('PVITENT', 'L', ivien)
!
    mater=zi(imate)
    nomres(1)='CELE_R'
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    call rcvalb(fami, kpg, spt, poum, mater,&
                ' ', 'FLUIDE', 0, ' ', [0.d0],&
                1, nomres, valres, icodre, 1)
    celer = valres(1)
    if (celer .lt. 1.d-1) goto 999
!
    call jevech('PVECTUR', 'E', ivectu)
!
! --- INITIALISATION DU VECTEUR DE CORRECTION
    do 10 i = 1, ndi
        zr(ivectu+i-1) = 0.d0
10  end do
!
!
! --- RECUPERATION DES COORDONNEES DES POINTS DE GAUSS (SI NECESSAIRE)
!
! --- INITIALISATION DE LA MATRICE D'IMPEDANCE
!
    call matini(18, 18, 0.d0, a)
!
!        CALCUL DES PRODUITS VECTORIELS OMI X OMJ
!
    do 1 ino = 1, nno
        i = igeom + 3*(ino-1) -1
        do 2 jno = 1, nno
            j = igeom + 3*(jno-1) -1
            sx(ino,jno) = zr(i+2) * zr(j+3) - zr(i+3) * zr(j+2)
            sy(ino,jno) = zr(i+3) * zr(j+1) - zr(i+1) * zr(j+3)
            sz(ino,jno) = zr(i+1) * zr(j+2) - zr(i+2) * zr(j+1)
 2      continue
 1  continue
!
!        BOUCLE SUR LES POINTS DE GAUSS
!
    do 101 ipg = 1, npg2
        kdec = (ipg-1)*nno*ndim
        ldec = (ipg-1)*nno
!
        nx = 0.0d0
        ny = 0.0d0
        nz = 0.0d0
!
!           CALCUL DE LA NORMALE AU POINT DE GAUSS IPG
!
        do 102 i = 1, nno
            idec = (i-1)*ndim
            do 102 j = 1, nno
                jdec = (j-1)*ndim
!
                nx = nx + zr(idfdx+kdec+idec) * zr(idfdy+kdec+jdec) * sx(i,j)
                ny = ny + zr(idfdx+kdec+idec) * zr(idfdy+kdec+jdec) * sy(i,j)
                nz = nz + zr(idfdx+kdec+idec) * zr(idfdy+kdec+jdec) * sz(i,j)
!
102          continue
!
!           CALCUL DU JACOBIEN AU POINT DE GAUSS IPG
!
        jac = sqrt(nx*nx + ny*ny + nz*nz)
!
!           CALCUL DE LA MATRICE D'IMPEDANCE POUR L'ORDRE 0
!
        do 103 i = 1, nno
!
            do 104 j = 1, nno
!
                ii = 2*i
                jj = 2*j-1
                a(ii,jj)=a(ii,jj) - jac / celer * zr(ipoids+ipg-1) *&
                zr(ivf+ldec+i-1) * zr(ivf+ldec+j-1)
!
104          continue
103      continue
101  continue
!
!     CALCUL DE LA VITESSE ABSOLUE
    do 109 i = 1, ndi
        if (ivien .ne. 0) then
            vites(i) = zr(ivite+i-1) + zr(ivien+i-1)
        else
            vites(i) = zr(ivite+i-1)
        endif
109  end do
!
    do 110 i = 1, ndi
        do 112 j = 1, ndi
            zr(ivectu+i-1)=zr(ivectu+i-1) -a(i,j)*vites(j)
112      continue
110  end do
!
999  continue
end subroutine
