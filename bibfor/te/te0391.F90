subroutine te0391(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/gdjrg0.h"
#include "asterfort/gdmmas.h"
#include "asterfort/jevech.h"
#include "asterfort/pmavec.h"
#include "asterfort/poutre_modloc.h"
#include "asterfort/rcvalb.h"
!
    character(len=16) :: option, nomte
! ......................................................................
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
!
!    - FONCTION REALISEE:  CALCUL MATRICE DE MASSE MEPODTGD
!                          OPTION : 'MASS_MECA'
!                          OPTION : 'M_GAMMA'
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
!
    character(len=8) :: elrefe, fami, poum
    integer :: icodre(1), kpg, spt
    real(kind=8) :: en(3, 2), enprim(3, 2), x00(3, 3), y0(3), rot0(3, 3), rho(1)
    real(kind=8) :: grani(4), mass(18, 18),  zero
    real(kind=8) :: a, xiy, xiz, xjx, pjacob, ajacob
    integer :: nno, nnos, jgano, ndim, npg, nord, ipoids, ivf, idfdk, kp, ne, ic
    integer :: igeom, k0, imate, lorien, imatuu, imat, iacce, ivect, i, j, ico
! ......................................................................
    integer, parameter :: nb_cara = 4
    real(kind=8) :: vale_cara(nb_cara)
    character(len=8) :: noms_cara(nb_cara)
    data noms_cara /'A1','IY1','IZ1','JX1'/
!-----------------------------------------------------------------------
!
    call elref1(elrefe)
    zero = 0.0d0
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfdk,jgano=jgano)
    nord = 6*nno
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
!
    ico = 0
    do kp = 1, npg
        do 10 ne = 1, nno
            ico = ico + 1
            en(ne,kp) = zr(ivf-1+ico)
            enprim(ne,kp) = zr(idfdk-1+ico)
10      continue
    end do
!
    call jevech('PGEOMER', 'L', igeom)
    k0 = igeom - 1
!
    do ne = 1, nno
        do 30 ic = 1, 3
            k0 = k0 + 1
            x00(ic,ne) = zr(k0)
30      continue
    end do
!
    call jevech('PMATERC', 'L', imate)
    call rcvalb(fami, kpg, spt, poum, zi(imate),&
                ' ', 'ELAS', 0, ' ', [0.d0],&
                1, 'RHO', rho, icodre, 1)
!
!     --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---
!     --- LA SECTION EST SUPPOSEE CONSTANTE ---
    call poutre_modloc('CAGNPO', noms_cara, nb_cara, lvaleur=vale_cara)
    a   = vale_cara(1)
    xiy = vale_cara(2)
    xiz = vale_cara(3)
    xjx = vale_cara(4)
!
    grani(1) = rho(1)*xjx
    grani(2) = rho(1)*xiy
    grani(3) = rho(1)*xiz
    grani(4) = rho(1)*a
!
!     --- RECUPERATION DES ORIENTATIONS INITIALES Y0(1), Y0(2), Y0(3)
    call jevech('PCAORIE', 'L', lorien)
    y0(1) = zr(lorien)
    y0(2) = zr(lorien+1)
    y0(3) = zr(lorien+2)
!
    do j = 1, nord
        do 50 i = 1, nord
            mass(i,j) = zero
50      continue
    end do
!
!* BOUCLE SUR LES POINTS DE GAUSS
!
    do kp = 1, npg
        call gdjrg0(kp, nno, enprim, x00, y0,&
                    ajacob, rot0)
        pjacob = zr(ipoids-1+kp)*ajacob
        call gdmmas(kp, nno, pjacob, en, grani,&
                    rot0, mass)
    end do
!
    if (option .eq. 'MASS_MECA') then
        call jevech('PMATUNS', 'E', imatuu)
        imat = imatuu - 1
        do 90 i = 1, nord
            do 80 j = 1, nord
                imat = imat + 1
                zr(imat) = mass(i,j)
80          continue
90      continue
    else if (option.eq.'M_GAMMA') then
        call jevech('PACCELR', 'L', iacce)
        call jevech('PVECTUR', 'E', ivect)
        call pmavec('ZERO', nord, mass, zr(iacce), zr(ivect))
    endif
!
end subroutine
