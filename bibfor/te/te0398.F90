subroutine te0398(option, nomte)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/dfdm3d.h'
    include 'asterfort/elref4.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jevech.h'
    character(len=16) :: nomte, option
! ----------------------------------------------------------------------
! FONCTION REALISEE:  CALCUL DU GRADIENT AUX NOEUDS D'UN CHAMP SCALAIRE
!                      AUX NOEUDS A 9 COMPOSANTES
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
!.......................................................................
!
!
    real(kind=8) :: dfdx(27), dfdy(27), dfdz(27), jac, gradx, grady, gradz
!
    integer :: ndim, nno, nnos, npg
    integer :: ipoids, ivf, idfde, jgano, igeom, ineut, igr
    integer :: i, kp, ino
!
!
!
! DEB ------------------------------------------------------------------
!
    call jemarq()
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PNEUTER', 'L', ineut)
    call jevech('PGNEUTR', 'E', igr)
!
!   C'EST SUREMENT MIEUX D'INVERSER LES BOUCLES !!
!
!     BOUCLE SUR LES 9 CHAMPS SCALAIRES D'ENTREE
    do 100 i = 1, 9
!
!       BOUCLE SUR LES POINTS DE GAUSS
        do 200 kp = 1, npg
            call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                        dfdx, dfdy, dfdz, jac)
            gradx = 0.0d0
            grady = 0.0d0
            gradz = 0.0d0
            do 210 ino = 1, nno
                gradx = gradx + zr(ineut-1+9*(ino-1)+i) * dfdx(ino)
                grady = grady + zr(ineut-1+9*(ino-1)+i) * dfdy(ino)
                gradz = gradz + zr(ineut-1+9*(ino-1)+i) * dfdz(ino)
210          continue
            zr(igr-1+27*(kp-1)+3*(i-1)+1)= gradx
            zr(igr-1+27*(kp-1)+3*(i-1)+2)= grady
            zr(igr-1+27*(kp-1)+3*(i-1)+3)= gradz
200      continue
!
100  end do
!
    call jedema()
!
! FIN ------------------------------------------------------------------
end subroutine
