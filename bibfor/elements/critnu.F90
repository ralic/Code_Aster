function critnu(zimat, nmnbn, deps, dtg, normm)
    implicit none
! ======================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     DEFINIT LE NOMBRE DE CRITERE DE PLASTICITE ACTIVE
!
! IN  ZIMAT : ADRESSE DE LA LISTE DE MATERIAU CODE
! IN  NMNBN : FORCE - BACKFORCE
! IN  DEPS : INCREMENT DE DEFORMATION DANS LE REPERE ORTHO
! IN  DTG : MATRICE ELASTIQUE
! IN  NORMM : NORME SUR LA FONCTION MP = F(N)
!
! OUT CRITNU : NOMBRE DE CRITERE DE PLASTICITE ACTIVE
!
    include 'asterfort/fplass.h'
    include 'asterfort/gplass.h'
    include 'asterfort/matmul.h'
    include 'asterfort/mppffn.h'
    include 'asterfort/u2mess.h'
    integer :: critnu, zimat, nmprif, j
!
    real(kind=8) :: nmnbn(6), nprnbn(6), nmprpl(2, 3), nmprzf, nmprzg
    real(kind=8) :: deps(6), dtg(6, 6), f1elas, f2elas, g1elas, g2elas, normm
    real(kind=8) :: cp(6)
!
!     PREDICTION ELASTIQUE
    call matmul(dtg, deps, 6, 6, 1,&
                cp)
!
!     TENSEUR DES CONTRAINTES TESTS
    do 10, j = 1,6
    nprnbn(j) = nmnbn(j) + cp(j)
    10 end do
!
!     CALCUL DES MOMENTS LIMITES DE PLASTICITE
    call mppffn(zimat, nprnbn, nmprpl, nmprzf, nmprzg,&
                nmprif, normm)
!
    if (nmprif .gt. 0) then
        critnu=-1
        call u2mess('A', 'ELEMENTS_21')
        goto 20
    endif
!
!     CALCUL DES CRITERES DE PLASTICITE F
    f1elas = fplass(nprnbn,nmprpl,1)
    f2elas = fplass(nprnbn,nmprpl,2)
!
!     CALCUL DES CONDITIONS DE PLASTICITE G
    g1elas = gplass(nprnbn,nmprpl,1)
    g2elas = gplass(nprnbn,nmprpl,2)
!
    if ((f1elas .gt. 0) .or. (g1elas .gt. 0)) then
        if ((f2elas .gt. 0) .or. (g2elas .gt. 0)) then
            critnu = 12
        else
            critnu = 1
        endif
    else if ((f2elas .gt. 0).or.(g2elas .gt. 0)) then
        critnu = 2
    else
        critnu = 0
    endif
!
20  continue
end function
