subroutine xnewto(elrefp, name, n, ndime, ptxx,&
                  ndim, tabco, tabls, ipp, ip,&
                  itemax, epsmax, ksi, dekker)
    implicit none
!
#include "jeveux.h"
#include "asterc/r8gaem.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/utmess.h"
#include "asterfort/vecini.h"
#include "asterfort/xdelt0.h"
#include "asterfort/xdelt2.h"
#include "asterfort/xdelt3.h"
#include "asterfort/xdelt4.h"
#include "asterfort/xintva.h"
    integer :: ndime, ndim, ipp, ip, n(3)
    real(kind=8) :: ptxx(*), tabco(*), tabls(*)
    integer :: itemax
    real(kind=8) :: epsmax, ksi(ndime)
    character(len=6) :: name
    character(len=8) :: elrefp
    integer, intent(in), optional :: dekker
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
    integer :: iter, i, arete
    real(kind=8) :: zero
    parameter    (zero=0.d0)
    real(kind=8) :: dist, dmin, intinf, intsup
    real(kind=8) :: ksi2(ndime), delta(ndime), ksim(ndime)
    data  itermin/3/
!
! ------------------------------------------------------------------
!
    call jemarq()
!
! --- POINT DE DEPART
!
!  ATTENTION: ON SUPPOSE QUE LA FONCTION APPELANTE A DEJA 
!  INITIALISE LE NEWTON EN AMONT
    do i = 1, ndime
        ksi2(i)=ksi(i)
    enddo
!
    call vecini(ndime, zero, delta)
    iter = 0
    epsabs = epsmax/100.d0
    epsrel = epsmax
    dmin = r8gaem()
    dist = 0.d0
!
    if (present(dekker)) then
        call xintva(elrefp, n, ptxx, ndime, intinf,&
                    intsup)
    endif
!
! --- DEBUT DE LA BOUCLE
!
 20 continue
!-------------------------
!
!     FAIRE TANT QUE
!
!
! --- CALCUL DE L'INCREMENT
!
    if (name .eq. 'XMILFI') then
        call xdelt2(elrefp, n, ndime, ksi2, ptxx,&
                    ndim, tabco, tabls, ipp, ip,&
                    delta)
    elseif (name .eq. 'XINTAR') then
        call xdelt3(ndim, ksi2, tabls, delta(1))
    elseif (name .eq. 'XINTFA') then
        call xdelt4(elrefp, ksi2, ptxx, ndim, tabls, delta)
    else if (name .eq. 'XINTER') then
        call xdelt0(elrefp, ndime, tabls, ptxx, ksi2(1),&
                    delta(1), arete)
    else if (name .eq. 'XMIFIS') then
        call xdelt0(elrefp, ndime, tabls, ptxx, ksi2(1),&
                    delta(1))
    else if (name .eq. 'XCENFI') then
        call xdelt0(elrefp, ndime, tabls, ptxx, ksi2(1),&
                    delta(1))
    endif
!
!   ON VERIFIE POUR XINTER QUE LE NEWTON RESTE SUR L ARETE
    if (name .eq. 'XINTER') then
       ASSERT((ksi2(1) .ge. 0.d0) .and. (ksi2(1) .le. 1.d0))
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
!   ON VERIFIE POUR XMIFIS QUE LE NEWTON RESTE DANS LA FACE TRIA
!   DE RECHERCHE, SINON ON ACTIVE LA METHODE DE DEKKER
    if (name .eq. 'XMIFIS') then
        if (present(dekker)) then
            if (ksi2(1) .gt. intsup) then
                ksi2(1) = ksi2(1)+delta(1)
                ksi2(1) = (ksi2(1)+intsup)/2.d0
            else if (ksi2(1).lt.intinf) then
                ksi2(1) = ksi2(1)+delta(1)
                ksi2(1) = (ksi2(1)+intinf)/2.d0
            endif
        endif
    endif
!
    do i = 1, ndime
        dist = dist+delta(i)*delta(i)
    end do
    dist = sqrt(dist)
!
    if (dist .le. dmin) then
        do i = 1, ndime
            ksim(i) = ksi2(i)
        end do
    endif
!
! --- CALCUL DE LA REFERENCE POUR TEST DEPLACEMENTS
!
    refe = zero
    do i = 1, ndime
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
    do i = 1, ndime
        test = test + delta(i)*delta(i)
    end do
    test = sqrt(test/refe)
!
! --- EVALUATION DE LA CONVERGENCE
!
    if (iter .lt. itermin) goto 20
!
    if ((test.gt.eps) .and. (iter.lt.itemax)) then
        goto 20
    else if ((iter.ge.itemax).and.(test.gt.eps)) then
        call utmess('F', 'XFEM_67')
        do i = 1, ndime
            ksi2(i) = ksim(i)
        end do
    endif
!
! --- FIN DE LA BOUCLE
!
    do i = 1, ndime
        ksi2(i)=ksi2(i)-delta(i)
    end do
!
!   GESTION DU CAS NDIME<NDIM
    do i = 1, ndime
        ksi(i)=ksi2(i)
    enddo
!    write(6,*)'CONVERGENCE DE ',name,' EN ',iter,' ITERATIONS'
    call jedema()
end subroutine
