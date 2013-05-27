subroutine fmodam(neq, vite, valmod, basmod, force)
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
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/r8inir.h'
    integer :: neq
    character(len=24) :: valmod, basmod
    real(kind=8) :: vite(neq), force(neq)
!
! ----------------------------------------------------------------------
!
! ROUTINE DYNA_NON_LINE (CALCUL)
!
! CALCUL DES FORCES D'AMORTISSEMENT MODALE
!
! ----------------------------------------------------------------------
!
!
! IN  NEQ    : NOMBRE D'EQUATIONS DU SYSTEME
! IN  VITE   : VECTEUR VITESSE
! IN  VALMOD : MODES PROPRES
! IN  BASMOD : BASE MODALE
! OUT FORCE  : FORCE D'AMORTISSEMENT MODALE
!
!
!
!
    character(len=8) :: k8bid
    integer :: imode, n1, nbmode
    real(kind=8) :: amor, masgen, puls, somme
    integer :: jvalmo, jbasmo
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    call jelira(valmod, 'LONMAX', nbmode, k8bid)
    nbmode = nbmode/3
    call jeveuo(valmod, 'L', jvalmo)
    call jeveuo(basmod, 'L', jbasmo)
!
    call r8inir(neq, 0.d0, force, 1)
!
    do 10 imode = 1, nbmode
        masgen = zr(jvalmo+3*(imode-1)+1-1)
        puls = zr(jvalmo+3*(imode-1)+2-1)
        amor = zr(jvalmo+3*(imode-1)+3-1)
        somme = 0.d0
        do 11 n1 = 1, neq
            somme = somme + zr(jbasmo+(imode-1)*neq+n1-1)*vite(n1)
11      continue
        do 12 n1 = 1, neq
            force(n1) = force(n1) + 2.d0*amor/masgen/puls**3 *zr( jbasmo+(imode-1)*neq+n1-1)*somm&
                        &e
12      continue
10  end do
!
    call jedema()
end subroutine
