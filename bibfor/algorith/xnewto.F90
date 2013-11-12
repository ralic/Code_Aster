subroutine xnewto(elp, name, nno, n,&
                  ndime, ptint, ndim, tabco, pmilie, tabls,&
                  tab, ipp, ip, s, itemax,&
                  epsmax, ksi)
    implicit none
!
#include "jeveux.h"
#include "asterc/r8gaem.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/utmess.h"
#include "asterfort/vecini.h"
#include "asterfort/xdelt2.h"
#include "asterfort/xdelt3.h"
#include "asterfort/xdelt4.h"
#include "asterfort/xdelt5.h"
    integer :: num, ndime, ndim, ipp, ip, nno, n(3)
    real(kind=8) :: s, ptint(*), tabco(*), tabls(*), tab(8, ndim), pmilie(*)
    integer :: itemax
    real(kind=8) :: epsmax, ksi(ndim)
    character(len=6) :: name
    character(len=8) :: elp
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
!             ALGORITHME DE NEWTON POUR CALCULER LES COORDONNEES
!                DE REFERENCE D'UN POINT PT MILIEU D'UNE ARETE
!
!     ENTREE
!       NUM     : NUMERO DE LA FONCTION A RESOUDRE (DANS XDELT1)
!       NDIM    : DIMENSION TOPOLOGIQUE DU MAILLAGE
!       COORSG  : COORDONNEES DES 3 NOEUDS DE L'ARETE
!       S       : ABSCISSE CURVILIGNE DU POINT SUR L'ARETE
!       ITEMAX  : NOMBRE MAXI D'ITERATIONS DE NEWTON
!       EPSMAX  : RESIDU POUR CONVERGENCE DE NEWTON
!       N       : LES INDICES DES NOEUX D'UNE FACE DANS L'ELEMENT PARENT
!       PMILIE  : LES COORDONNES DES POINTS MILIEUX
!
!     SORTIE
!       KSI     : COORDONNEES DE REFERENCE DU POINT
!     --------------------------------------------------------------
!
    real(kind=8) :: eps
    real(kind=8) :: test, epsrel, epsabs, refe, itermin
    integer :: iter, i
    real(kind=8) :: zero
    parameter    (zero=0.d0)
    real(kind=8) :: dist, dmin
    real(kind=8) :: ksi2(ndime),delta(ndime), ksim(ndime)
    data  itermin/2/
!
! ------------------------------------------------------------------
!
    call jemarq()
!
! --- POINT DE DEPART
!
    call vecini(ndime, zero, ksi2)
!
    call vecini(ndime, zero, delta)
    iter = 0
    epsabs = epsmax/100.d0
    epsrel = epsmax
    dmin = r8gaem()
!
! --- DEBUT DE LA BOUCLE
!
20  continue
!-------------------------
!
!     FAIRE TANT QUE
!
!
! --- CALCUL DE LA QUANTITE A MINIMISER
!
    if (name .eq. 'XMILFI') then
        call xdelt2(elp, nno, n, ndime, ksi2,&
                    ptint, ndim, tabco, tabls, ipp, ip,&
                    delta)
!    else if (name.eq. 'XINVAC') then
!        call xdelt1(num, ndim, ksi2(1), tabco, s,&
!                    delta(1))
    else if (name.eq. 'XINTAR') then
        call xdelt3(ndim, ksi2, tabls, delta(1))
    else if (name.eq. 'XCENFI') then
        ASSERT(ndim.eq.3)
        call xdelt4(elp, nno, ndim, ksi2, ptint,&
                    pmilie, tabco, tabls, delta)
    else if (name.eq. 'XMILFA') then
        call xdelt5(elp, nno, n, ndime, ksi2,&
                    tabco, ndim, tab, delta)
    endif
!
! --- ACTUALISATION
!
    do i = 1, ndime
        ksi2(i) = ksi2(i) - delta(i)
   end do
!
    iter = iter + 1
!
    do  i = 1, ndim
        dist = delta(i)*delta(i)
    end do
    dist = sqrt(dist)
!
    if (dist .le. dmin) then
        do  i = 1, ndime
            ksim(i) = ksi2(i)
        end do
    endif
!
! --- CALCUL DE LA REFERENCE POUR TEST DEPLACEMENTS
!
    refe = zero
    do  i = 1, ndime
        refe = refe + ksi2(i)*ksi2(i)
    end do
    if (refe .le. epsrel) then
        refe = 1.d0
        eps = epsabs
    else
        eps = epsrel
    endif
!
! --- CALCUL POUR LE TEST DE CONVERGENCE
!
    test = zero
    do  i = 1, ndime
        test = test + delta(i)*delta(i)
    end do
    test = sqrt(test/refe)
!
! --- EVALUATION DE LA CONVERGENCE
!
    if(iter .le. itermin) goto 20
!
    if ((test.gt.eps) .and. (iter.lt.itemax)) then
        goto 20
    else if ((iter.ge.itemax).and.(test.gt.eps)) then
        call utmess('F', 'XFEM_67')
        do  i = 1, ndime
            ksi2(i) = ksim(i)
        end do
    endif
!
! --- FIN DE LA BOUCLE
!
    do  i = 1, ndime
        ksi2(i)=ksi2(i)-delta(i)
    end do
!
!   GESTION DU CAS NDIME<NDIM
    call vecini(ndim, zero, ksi)
    do  i=1, ndime
        ksi(i)=ksi2(i)
    enddo
    call jedema()
end subroutine
