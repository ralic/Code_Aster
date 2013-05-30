subroutine te0018(option, nomte)
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
!
!     BUT: CALCUL DES VECTEURS ELEMENTAIRES EN MECANIQUE
!          CORRESPONDANT A UN CHARGEMENT EN PRESSION REPARTIE
!          SUR DES FACES D'ELEMENTS ISOPARAMETRIQUES 3D
!
!          OPTION : 'CHAR_MECA_PRES_R '
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/elref4.h'
    include 'asterfort/jevecd.h'
    include 'asterfort/jevech.h'
    include 'asterfort/nmpr3d.h'
    character(len=16) :: nomte, option
    integer :: ndim, nno, npg, nnos, jgano, kpg, kdec, n
    integer :: ipoids, ivf, idf, igeom, ipres, ires
!                                   9*27*27
    real(kind=8) :: pr, p(27), matr(6561)
!
!
!
!
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idf, jgano)
!
    call jevech('PGEOMER', 'L', igeom)
!     -- SI LA PRESSION N'EST CONNUE SUR AUCUN NOEUD, ON LA PREND=0.
    call jevecd('PPRESSR', ipres, 0.d0)
    call jevech('PVECTUR', 'E', ires)
!
!
!    CALCUL DE LA PRESSION AUX POINTS DE GAUSS (A PARTIR DES NOEUDS)
    do 100 kpg = 0, npg-1
        kdec = kpg*nno
        pr = 0.d0
        do 105 n = 0, nno-1
            pr = pr + zr(ipres+n) * zr(ivf+kdec+n)
105      continue
        p(kpg+1) = pr
100  end do
!
!    CALCUL EFFECTIF DU SECOND MEMBRE
    call nmpr3d(1, nno, npg, zr(ipoids), zr(ivf),&
                zr(idf), zr(igeom), p, zr(ires), matr)
!
end subroutine
