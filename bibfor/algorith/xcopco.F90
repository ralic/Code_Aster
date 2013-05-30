subroutine xcopco(jcesd, jcesv, jcesl, ifiss, alias,&
                  ndim, nummae, iface, ksi1, ksi2,&
                  npte, geom)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! TOLE CRS_1404
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/cesexi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/mmnonf.h'
    character(len=8) :: alias
    integer :: nummae, ndim, iface, npte, ifiss
    integer :: jcesd(10), jcesv(10), jcesl(10)
    real(kind=8) :: ksi1
    real(kind=8) :: ksi2
    real(kind=8) :: geom(3)
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM (CONTACT - GRANDS GLISSEMENTS)
!
! CALCUL DES COORDONNEES D'UN POINT DE CONTACT A PARTIR
! DES COORDONNEES PARAMETRIQUES POUR LE CONTACT METHODE CONTINUE
!
! TRAVAIL EFFECTUE EN COLLABORATION AVEC I.F.P.
!
! ----------------------------------------------------------------------
!
!
!  JCES*(4)  : POINTEURS DE LA SD SIMPLE DE CONNECTIVITÉ DES FACETTES
!  JCES*(5)  : POINTEURS DE LA SD SIMPLE DES COOR DES PT D'INTER ESCLAVE
!  IFISS     : NUMÉRO DE FISSURE LOCALE
! IN  ALIAS  : TYPE DE MAILLE DE CONTACT
! IN  NDIM   : DIMENSION DU PROBLÈME
! IN  NUMMAE : INDICE DE LA MAILLE DANS CONTAMA
! IN  KSI1   : COORDONNEE PARAMETRIQUE KSI DU PROJETE
! IN  IFACE  : NUMERO DE LA FACE ESCLAVE
! IN  KSI2   : COORDONNEE PARAMETRIQUE ETA DU PROJETE
! OUT GEOM   : COORDONNEES DU PROJETE (EN 2D Z=0)
!
!
!
!
    integer :: i, j, iad, numpi(3)
    real(kind=8) :: ff(9), coor(27)
!-----------------------------------------------------------------------
!
    call jemarq()
!
!
! --- INITIALISATIONS
!
    do 100 i = 1, ndim
        geom(i) = 0.d0
100  end do
!
! --- RECUPERATION DES NUMEROS LOCAUX DES POINTS D'INTERSECTIONS
!     DE LA FACETTE DANS LA MAILLE
!
    do 150 i = 1, npte
        call cesexi('S', jcesd(4), jcesl(4), nummae, 1,&
                    ifiss, (iface-1)* ndim+i, iad)
        call assert(iad.gt.0)
        numpi(i) = zi(jcesv(4)-1+iad)
150  end do
!
! --- RECUPERATION DES COORDONNES REELLES DES POINTS D'INTERSECTION
!     DE LA FACETTE ESCLAVE
!
    do 200 i = 1, npte
        do 210 j = 1, ndim
            call cesexi('S', jcesd(5), jcesl(5), nummae, 1,&
                        ifiss, ndim*( numpi(i)-1)+j, iad)
            call assert(iad.gt.0)
            coor(ndim*(i-1)+j)=zr(jcesv(5)-1+iad)
210      continue
200  end do
!
! --- CALCUL DU POINT
!
    call mmnonf(ndim, npte, alias, ksi1, ksi2,&
                ff)
    do 300 i = 1, ndim
        do 310 j = 1, npte
            geom(i) = ff(j)*coor((j-1)*ndim+i) + geom(i)
310      continue
300  end do
!
    call jedema()
end subroutine
