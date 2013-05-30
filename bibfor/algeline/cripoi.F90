subroutine cripoi(nbm, b, crit)
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
! COUPLAGE FLUIDELASTIQUE, CONFIGURATIONS DU TYPE "COQUE_COAX"
! CALCUL DU POIDS RELATIF DES TERMES EXTRADIAGONAUX DE LA MATRICE B(S)
! PAR RAPPORT AUX TERMES DIAGONAUX
! APPELANT : POIBIJ
!-----------------------------------------------------------------------
!  IN : NBM  : NOMBRE DE MODES RETENUS POUR LE COUPLAGE FLUIDELASTIQUE
!              => ORDRE DE LA MATRICE B(S)  (NB: NBM > 1)
!  IN : B    : MATRICE B(S)
! OUT : CRIT : CRITERE DE POIDS RELATIF DES TERMES EXTRADIAGONAUX
!-----------------------------------------------------------------------
    include 'asterc/r8miem.h'
    include 'asterc/r8nnem.h'
    include 'asterc/r8prem.h'
    include 'asterfort/dcabs2.h'
    include 'asterfort/u2mess.h'
    integer :: nbm
    complex(kind=8) :: b(nbm, nbm)
    real(kind=8) :: crit
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, j
    real(kind=8) :: bii, bij, sommii, sommij, tole, tolr, x
    real(kind=8) :: y
!-----------------------------------------------------------------------
    tole = 100.d0*r8miem()
    tolr = r8prem()
!
!-----1.CALCUL DE LA SOMME DES CARRES DES TERMES DIAGONAUX
!
    sommii = 0.d0
!
    do 10 i = 1, nbm
        bii = dcabs2(b(i,i))
        sommii = sommii + bii*bii
10  end do
!
    if (sommii .lt. tole) then
!
        call u2mess('A', 'ALGELINE_30')
        crit = r8nnem()
!
    else
!
!-------2.CALCUL DE LA SOMME DES CARRES DES TERMES EXTRADIAGONAUX
!
        sommij = 0.d0
!
        do 20 i = 2, nbm
            bij = dcabs2(b(i,1))
            sommij = sommij + bij*bij
20      continue
!
        do 30 j = 2, nbm
            do 31 i = 1, j-1
                bij = dcabs2(b(i,j))
                sommij = sommij + bij*bij
31          continue
            if (j .lt. nbm) then
                do 32 i = j+1, nbm
                    bij = dcabs2(b(i,j))
                    sommij = sommij + bij*bij
32              continue
            endif
30      continue
!
!-------3.CALCUL DU CRITERE
!
        x = sommij/dble(nbm-1)
!
        if (sommii .lt. x*tolr) then
!
            call u2mess('A', 'ALGELINE_31')
            crit = r8nnem()
!
        else
!
            y = x/sommii
            crit = dble(sqrt(y))*100.d0
!
        endif
!
    endif
!
end subroutine
