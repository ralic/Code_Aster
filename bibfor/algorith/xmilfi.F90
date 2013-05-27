subroutine xmilfi(elp, ndim, nno, ptint, jtabco,&
                  jtabls, ipp, ip, milfi)
    implicit none
!
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/reerel.h'
    include 'asterfort/xnewto.h'
    integer :: ndim, nno, jtabco, jtabls, ipp, ip
    character(len=8) :: elp
    real(kind=8) :: milfi(ndim), ptint(*)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! TOLE CRS_1404
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
!       PTINT  : COORDONNÉES DES POINTS D'INTERSECTION
!       JTABCO  : ADRESSE DES COORDONNEES DES NOEUDS DE L'ELEMENT
!       JTABLS  : ADRESSE DES LSN DES NOEUDS DE L'ELEMENT
!       IPP     : NUMERO NOEUD PREMIER POINT INTER
!       IP      : NUMERO NOEUD DEUXIEME POINT INTER
!     SORTIE
!       MILFI   : COORDONNES DU PT MILIEU ENTRE IPP ET IP
!     ----------------------------------------------------------------
!
    real(kind=8) :: ksi(ndim)
    real(kind=8) :: epsmax, rbid
    integer :: ibid, itemax
    character(len=6) :: name
!
! --------------------------------------------------------------------
!
    call jemarq()
!
    itemax=500
    epsmax=1.d-9
    name='XMILFI'
!
!     CALCUL DES COORDONNEES DE REFERENCE
!     DU POINT PAR UN ALGO DE NEWTON
    call xnewto(elp, name, ibid, nno, ndim,&
                ptint, zr(jtabco), jtabls, ipp, ip,&
                rbid, itemax, epsmax, ksi)
!
    call assert(ksi(1).ge.-1.d0 .and. ksi(1).le.1.d0)
    call assert(ksi(2).ge.-1.d0 .and. ksi(2).le.1.d0)
!
! --- COORDONNES DU POINT DANS L'ELEMENT REEL
    call reerel(elp, nno, ndim, zr(jtabco), ksi,&
                milfi)
!
    call jedema()
end subroutine
