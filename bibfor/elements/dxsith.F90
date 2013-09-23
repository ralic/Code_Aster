subroutine dxsith(nomte, mater, sigma)
    implicit none
#include "jeveux.h"
#include "asterfort/dmatcp.h"
#include "asterfort/elref5.h"
#include "asterfort/jevech.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
#include "asterfort/vecini.h"
#include "asterfort/verift.h"
    integer :: mater
    real(kind=8) :: sigma(*)
    character(len=16) :: nomte
! ----------------------------------------------------------------------
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
!
!     BUT:
!       CALCUL DES CONTRAINTES VRAIES
!        (==SIGMA_MECA - SIGMA_THER).
!
! ----------------------------------------------------------------------
!
!
!
!
    integer :: nbepsg
    parameter (nbepsg=8)
!
    integer :: ndim, nnoel, nnos, npg, ipoids, icoopg, ivf, idfdx, idfd2, jgano
    integer :: i, j, icou, icpg, igauh, ipg, ipgh, iret, ibid, nbcmp, nbcou
    integer :: npgh
    integer :: jnbspi, itab(8)
!
    real(kind=8) :: d(4, 4), repere(7), inst, zero, epsth(nbepsg)
!
    character(len=4) :: fami
    character(len=8) :: materi
!
    logical :: dkg
!
! ----------------------------------------------------------------------
!
    fami = 'RIGI'
    materi = ' '
    call elref5(' ', fami, ndim, nnoel, nnos,&
                npg, ipoids, icoopg, ivf, idfdx,&
                idfd2, jgano)
!
    zero = 0.0d0
    call vecini(7, zero, repere)
!
    dkg = .false.
!
    nbcmp = 6
!
    if ((nomte.eq.'MEDKTG3') .or. (nomte.eq.'MEDKQG4')) then
        dkg = .true.
    endif
!
! --- RECUPERATION DE L'INSTANT
!     -------------------------
    call tecach('ONN', 'PTEMPSR', 'L', iret, nval=8,&
                itab=itab)
    ibid = itab(1)
    if (iret .eq. 0) then
        inst = zr(ibid)
    else
        inst = zero
    endif
!
! --- RECUPERATION DU NOMBRE DE COUCHE ET DE SOUS-POINT
!     -------------------------------------------------
    if (dkg) then
        nbcou = 1
        npgh = 1
    else
        call jevech('PNBSP_I', 'L', jnbspi)
        npgh = 3
        nbcou = zi(jnbspi-1+1)
        if (nbcou .le. 0) then
            call utmess('F', 'ELEMENTS_46')
        endif
    endif
!
! --- BOUCLE SUR LES POINTS DE GAUSS DE LA SURFACE:
!     ---------------------------------------------
    do 100 ipg = 1, npg
        do 110 icou = 1, nbcou
            do 120 igauh = 1, npgh
                icpg=nbcmp*npgh*nbcou*(ipg-1)+ nbcmp*npgh*(icou-1)+&
                nbcmp*(igauh-1)
!
!         -- INTERPOLATION DE ALPHA EN FONCTION DE LA TEMPERATURE
!         ----------------------------------------------------
                ipgh=npgh*(icou-1)+igauh
                call verift('RIGI', ipg, ipgh, '+', mater,&
                            materi, 'ELAS', 1, epsth( 1), iret)
!
                epsth(2) = epsth(1)
                epsth(3) = zero
                epsth(4) = zero
                epsth(5) = zero
                epsth(6) = zero
!
!           -- CALCUL DE LA MATRICE DE HOOKE
!           --------------------------------
                call dmatcp('RIGI', mater, inst, '+', ipg,&
                            igauh, repere, d)
!
!           -- CALCUL DES CONTRAINTES VRAIES (==SIGMA_MECA - SIGMA_THER)
!           -- AU POINT D'INTEGRATION COURANT
!           ------------------------------------------------------------
                do 130 i = 1, 4
                    do 131 j = 1, 4
                        sigma(icpg+i)=sigma(icpg+i)-epsth(j)*d(i,j)
131                  continue
130              continue
!
120          continue
110      continue
100  end do
!
end subroutine
