subroutine te0024(option, nomte)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/dfdm2d.h'
    include 'asterfort/dfdm3d.h'
    include 'asterfort/elref1.h'
    include 'asterfort/elref4.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jevech.h'
    character(len=16) :: nomte, option
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
! ----------------------------------------------------------------------
! FONCTION REALISEE:  CALCUL DU GRADIENT AUX NOEUDS D'UN CHAMP SCALAIRE
!                      AUX NOEUDS
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
!.......................................................................
!
!
    real(kind=8) :: dfdx(27), dfdy(27), dfdz(27), jac
    real(kind=8) :: gradx, grady, gradz
    character(len=8) :: elp
    integer :: ndim, nno, nnos, npg
    integer :: ino, i
    integer :: ipoids, ivf, idfde, jgan, igeom, ineut
    integer :: igr
!
!
! DEB ------------------------------------------------------------------
!
    call jemarq()
!
    call elref1(elp)
!
!     ON CALCULE LES GRADIENTS SUR TOUS LES NOEUDS DE L'ELEMENT DE REF
    call elref4(elp, 'NOEU', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgan)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PNEUTER', 'L', ineut)
    call jevech('PGNEUTR', 'E', igr)
!
!     BOUCLE SUR LES NOEUDS
    do 100 ino = 1, nno
        if (ndim .eq. 3) then
            call dfdm3d(nno, ino, ipoids, idfde, zr(igeom),&
                        dfdx, dfdy, dfdz, jac)
        else if (ndim .eq. 2) then
            call dfdm2d(nno, ino, ipoids, idfde, zr(igeom),&
                        dfdx, dfdy, jac)
        endif
!
        gradx = 0.0d0
        grady = 0.0d0
        gradz = 0.0d0
!
        do 110 i = 1, nno
!
            gradx = gradx + dfdx(i)*zr(ineut+i-1)
            grady = grady + dfdy(i)*zr(ineut+i-1)
            if (ndim .eq. 3) gradz = gradz + dfdz(i)*zr(ineut+i-1)
!
110      continue
        zr(igr-1+(ino-1)*ndim+1)= gradx
        zr(igr-1+(ino-1)*ndim+2)= grady
        if (ndim .eq. 3) zr(igr-1+(ino-1)*ndim+3)= gradz
!
100  end do
!
    call jedema()
!
! FIN ------------------------------------------------------------------
end subroutine
