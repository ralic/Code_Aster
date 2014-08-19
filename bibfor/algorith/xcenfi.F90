subroutine xcenfi(elrefp, ndim, ndime, geom, lsn,&
                  pinref, pmiref, cenref, cenfi)
!
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "blas/ddot.h"
#include "asterfort/assert.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/elrfdf.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/provec.h"
#include "asterfort/reerel.h"
#include "asterfort/vecini.h"
#include "asterfort/xcedge.h"
#include "asterfort/xelrex.h"
#include "asterfort/xnewto.h"
#include "asterfort/xnormv.h"
#include "asterfort/xveri0.h"
    integer :: ndim, ndime
    character(len=8) :: elrefp
    real(kind=8) :: lsn(*), geom(*), pinref(*), pmiref(*)
    real(kind=8) :: cenfi(ndim), cenref(ndime)
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
!                      TROUVER LES COORDONNES DU PT MILIEU ENTRE LES
!                      DEUX POINTS D'INTERSECTION
!
!     ENTREE
!       ELP     : TYPE DE L'ELEMENT
!       NDIM    : DIMENSION TOPOLOGIQUE DU MAILLAGE
!       PTINT  : COORDONNÉES DES POINTS D'INTERSECTION
!       JTABCO  : ADRESSE DES COORDONNEES DES NOEUDS DE L'ELEMENT
!       JTABLS  : ADRESSE DES LSN DES NOEUDS DE L'ELEMENT
!       NNO     : NOMBRE DE NOEUX DE L'ELEMENT
!     SORTIE
!       CENFI   : COORDONNES DU PT MILIEU AU CENTRE DE LA FISSURE
!     ----------------------------------------------------------------
!
    real(kind=8) :: epsmax, rbid, crit, maxi, x(81)
    real(kind=8) :: pi1pi2(ndime), pi1pi3(ndime), dff(3,27)
    real(kind=8) :: v(3), ptxx(2*ndime), ksi(ndime), tole, xmi(ndime)
    integer :: ibid, itemax, i, n(3), nno, iret, j
    integer :: pi1, pi2, pi3, pi4, m12, m13, m24, m34, nbnomx
    character(len=6) :: name
    character(len=3) :: edge
    aster_logical :: courbe
    parameter   (tole=1.d-1)
    parameter (nbnomx = 27)
!
! --------------------------------------------------------------------
!
    call jemarq()
!
    call elrefe_info(elrefe=elrefp,fami='RIGI',nno=nno)
!
    itemax=100
    epsmax=1.d-8
    name='XCENFI'
!  CONFERE XSTUDO
    pi1=1
    pi2=2
    pi3=3
    pi4=4
    m12=9
    m13=12
    m34=11
    m24=10
!
    ASSERT( ndime .eq. 3)
!
    do i = 1,ndime
       pi1pi2(i)=pinref(ndime*(pi2-1)+i)-pinref(ndime*(pi1-1)+i)
       pi1pi3(i)=pinref(ndime*(pi3-1)+i)-pinref(ndime*(pi1-1)+i)
    enddo
    call xnormv(ndime, pi1pi2, rbid)
    call xnormv(ndime, pi1pi3, rbid)
    call provec(pi1pi2, pi1pi3, v)
    do i=1,ndime
       ptxx(i)=v(i)
    enddo
!
!   CALCUL D UN POINT DE DEPART POUR LE NEWTON
!===============================================================
!   ON DEFINI UN CRITERE SIMPLE POUR PRENDRE EN COMPTE LA COURBURE
!   CE CRITERE EST EFFICACE QUAND L INTERSECTION DE SURFACE DE L ISO ZERO
!   ET DU SOUS TETRAEDRE FORME UN <<POLYEDRE>> NON CONVEXE
!   SITUATION PLUS PROBABLE LORS DE LA DECOUPE D UN TETRATRAEDRE
!   EN REVANCHE,
!   EN RAFINANT LE MAILLAGE, LA SURFACE DE LA LEVEL-SET DEVIENT PLANE DANS
!   LES SOUS-TETRAS, LE CRITERE CI DESSOUS N A PLUS D INCIDENCE SUR LE CALCUL
!
    courbe=.false.
    maxi=0.d0
    edge=""
!   ARETE I1-I2
    call xcedge(ndime, pinref, pi1, pi2, pmiref,&
                m12, crit)
    if (crit .gt. maxi) then
        maxi=crit
        edge="A12"
    endif
!   ARETE I2-I4
    call xcedge(ndime, pinref, pi2, pi4, pmiref,&
                m24, crit)
    if (crit .gt. maxi) then
        maxi=crit
        edge="A24"
    endif
!   ARETE I3-I4
    call xcedge(ndime, pinref, pi3, pi4, pmiref,&
                m34, crit)
    if (crit .gt. maxi) then
        maxi=crit
        edge="A34"
    endif
!   ARETE I1-I3
    call xcedge(ndime, pinref, pi1, pi3, pmiref,&
                m13, crit)
    if (crit .gt. maxi) then
        maxi=crit
        edge="A13"
    endif
!
    if (maxi .gt. tole) courbe=.true.
!
    if (.not.courbe) then
        do i = 1, ndime
            ptxx(i+ndime)=(pinref(ndime*(pi1-1)+i)+&
                    pinref(ndime*(pi4-1)+i))/2.d0
        enddo
    else
        do i = 1, ndime
            if (edge .eq. "A12" .or. edge .eq. "A34") then
                ptxx(i+ndime)=(pmiref(ndime*(m12-1)+i)+&
                            pmiref(ndime*(m34-1)+i))/2.d0
            else if (edge .eq. "A13" .or. edge .eq. "A24") then
                ptxx(i+ndime)=(pmiref(ndime*(m13-1)+i)+&
                            pmiref(ndime*(m24-1)+i))/2.d0
            else
                ASSERT(.false.)
            endif
        enddo
    endif
!
!    CALCUL DE LA DIRECTION DE RECHERCHE
!    ON CHOISIT LE GRADIENT DE LA LSN AU POINT DE DEPART
!
    do j = 1, ndime
       xmi(j) = ptxx(ndime+j)
       ptxx(j) = 0.d0
    end do
    call elrfdf(elrefp, xmi, ndim*nbnomx, dff, nno,&
                ndim)
!
    do i = 1, nno
       do j = 1, ndime
          ptxx(j) = ptxx(j)+dff(j,i)*lsn(i)
       end do
    end do
!
!!!!!ATTENTION INITIALISATION DU NEWTON:
    call vecini(ndime, 0.d0, ksi)
    call xnewto(elrefp, name, n,&
                ndime, ptxx, ndim, geom, lsn,&
                ibid, ibid, itemax,&
                epsmax, ksi)
!
    do i = 1, ndime
        cenref(i)=ksi(1)*ptxx(i)+ptxx(i+ndime)
    enddo
!
    call xelrex(elrefp, nno, x)
    call xveri0(ndime, elrefp, cenref, iret)
    ASSERT(iret .eq. 0)
!
! --- COORDONNES DU POINT DANS L'ELEMENT REEL
    call reerel(elrefp, nno, ndim, geom, cenref,&
                cenfi)
!
    call jedema()
end subroutine
