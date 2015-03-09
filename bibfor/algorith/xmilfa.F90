subroutine xmilfa(elrefp, ndim, ndime, geom, cnset,&
                  nnose, it, ainter, ip1, ip2,&
                  pm2, typma, pinref, pmiref, ksi,&
                  milfa)
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/conare.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/reerel.h"
#include "asterfort/xelrex.h"
#include "asterfort/xnormv.h"
#include "asterfort/xxmmvd.h"
#include "blas/ddot.h"
    integer :: ip1, ip2, pm2, cnset(*), nnose, it, ndim, ndime
    real(kind=8) :: pinref(*), geom(*), milfa(ndim), ainter(*)
    real(kind=8) :: pmiref(*), ksi(ndime)
    character(len=8) :: elrefp, typma
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
!       NDIM    : DIMENSION TOPOLOGIQUE DU MAILLAGE
!       N       : NUMERO PARENT DES TROIS NOEUDS DE FACE
!       TYPMA   : TYPE DE LA MAILLE (TYPE_MAILLE)
!       IP1     : PREMIER IP DANS DANS UN FACE
!       IP2     : DEUXIEME IP DANS UN FACE
!       PM1A    : PREMIER PM1 DANS UN FACE
!       PM1B    : DEUXIEME PM1 DANS UN FACE
!       PM2     : PM2 DANS UN FACE
!       PINTER  : TABLEAU DES COORDONNÉES DES POINTS D'INTERSECTION
!       TABCO   : TABLEAU DES COORDONNÉES DES POINTS
!       PMILIE  : TABLEAU DES COORDONNÉES DES POINTS MILIEUX EXISTANT
!       AINTER  : INFOS ARETE ASSOCIEE AU POINT D'INTERSECTION
!     SORTIE
!       MILFA   : COORDONNES DU TROISIME TYPE DE PM
!     ----------------------------------------------------------------
!
    integer :: a1, a2, a, b, d, ib, ar(12, 3), nbar, ia, id
    integer :: i, j, zxain, nno
    real(kind=8) :: xref(81), t1(ndime), t2(ndime), sinu, rbid
    real(kind=8) :: t3(ndime), cosv, cosu
    aster_logical :: courbe
!
! --------------------------------------------------------------------
    call jemarq()
    zxain=xxmmvd('ZXAIN')
!   IDENTIFICATION DES NOEUDS DE LA FACE QUADRANGLE DANS LE SOUS TETRA
    call conare(typma, ar, nbar)
    a1=nint(ainter(zxain*(ip1-1)+1))
    a2=nint(ainter(zxain*(ip2-1)+1))
!
    a=0
    b=0
    d=0
!
    do i = 1, 2
        do 31 j = 1, 2
            if (ar(a1,i) .eq. ar(a2,j)) then
                a=ar(a1,3-i)
                b=ar(a2,3-j)
            endif
 31     continue
    end do
    do i = 1, nbar
        do 41 j = 1, 2
            if ((ar(i,j).eq.a) .and. (ar(i,3-j).eq.b)) d=ar(i,3)
 41     continue
    end do
    ASSERT((a*b*d).gt.0)
!   INDICE CORRECPONDANT DANS L ELEMENT PARENT
    ia=cnset(nnose*(it-1)+a)
    ib=cnset(nnose*(it-1)+b)
    id=cnset(nnose*(it-1)+d)
!
    call xelrex(elrefp, nno, xref)
!
    do i = 1, ndime
        ksi(i)=(pinref(ndime*(ip1-1)+i)+xref(ndime*(ib-1)+i))/2.d0
    end do
! --- TEST SI LSN COURBE :
    courbe=.false.
    do i = 1, ndime
        t1(i) = ksi(i)-pinref(ndime*(ip1-1)+i)
        t2(i) = -1.5d0*pinref(ndime*(ip1-1)+i)-5.d-1*pinref(ndime*(ip2-1)+i)+&
                2.d0*pmiref(ndime*(pm2-1)+i)
        t3(i) = xref(ndime*(ia-1)+i)-pinref(ndime*(ip1-1)+i)
    end do
    call xnormv(ndime, t1, rbid)
    call xnormv(ndime, t2, rbid)
    call xnormv(ndime, t3, rbid)
    cosu = ddot(ndime, t1, 1, t2, 1)
    sinu = sqrt(1-cosu**2) 
!   ON CHOISIT UNE CONVENTION DE SIGNE
    cosv = ddot(ndime, t3, 1, t2, 1)
    if (cosv.gt.cosu) sinu = -sinu
! 
!   ON RAJOUTE UNE TOLE POUR EVITER DES DECOUPES TROP POURRIES
    if (sinu.lt.1.d-3) courbe = .true.
!
    if (courbe) then
!   EN DEUXIEME APPROXIMATION: ON CHOISIT LE MILIEU DES "MILIEUX" PM2 ET D
        do i = 1, ndime
            ksi(i)=(pmiref(ndime*(pm2-1)+i)+xref(ndime*(id-1)+i))/2.d0
        enddo
    endif
!
! --- COORDONNES DU POINT DANS L'ELEMENT REEL
!
    do i = 1, ndime
        ASSERT(abs(ksi(i)) .le. 1.d0)
    enddo
    call reerel(elrefp, nno, ndim, geom, ksi,&
                milfa)
    call jedema()
end subroutine
