subroutine te0207(option, nomte)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 2007 NECS - BRUNO ZUBER   WWW.NECS.FR
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
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/elref4.h'
    include 'asterfort/jevech.h'
    include 'asterfort/nmfifn.h'
    include 'asterfort/u2mess.h'
    character(len=16) :: nomte, option
!
! ----------------------------------------------------------------------
!     FORCES NODALES DES ELEMENTS DE JOINT 3D, OPTION 'FORC_NODA'
! ----------------------------------------------------------------------
!
!
    integer :: ndim, nno, nnos, npg, nddl
    integer :: ipoids, ivf, idfde, jgano
    integer :: igeom, icont, ivect
! ----------------------------------------------------------------------
!
!
!
! -  FONCTIONS DE FORMES ET POINTS DE GAUSS : ATTENTION CELA CORRESPOND
!    ICI AUX FONCTIONS DE FORMES 2D DES FACES DES MAILLES JOINT 3D
!    PAR EXEMPLE FONCTION DE FORME DU QUAD4 POUR LES HEXA8.
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
    if (nno .gt. 4) call u2mess('F', 'ELEMENTS5_22')
    if (npg .gt. 4) call u2mess('F', 'ELEMENTS5_23')
!
    nddl = 6*nno
!
! - LECTURE DES PARAMETRES
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PCONTMR', 'L', icont)
    call jevech('PVECTUR', 'E', ivect)
!
    call nmfifn(nno, nddl, npg, zr(ipoids), zr(ivf),&
                zr(idfde), zr(igeom), zr(icont), zr(ivect))
!
end subroutine
