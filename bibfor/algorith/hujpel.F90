subroutine hujpel(etatd, mod, crit, imat, nmat,&
                  materf, angmas, deps, sigd, nvi,&
                  vind, sigf, vinf, iret)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: alexandre.foucault at edf.fr
    implicit   none
!       ----------------------------------------------------------------
!       INTEGRATION ELASTIQUE SUR DT
!       IN  ETATD  :  ETAT MATERIAU A T (ELASTIC OU PLASTIC)
!           MOD    :  MODELISATION
!           CRIT   :  CRITERES LOCAUX LIES AU SCHEMA DE NEWTON
!           IMAT   :  NUMERO MATERIAU
!           NMAT   :  DIMENSION TABLEAU DONNEES MATERIAUX
!           MATERF :  COEFFICIENTS MATERIAU A T+DT
!           DEPS   :  INCREMENT DE DEFORMATION
!           SIGD   :  CONTRAINTE  A T
!           NVI    :  DIMENSION VECTEUR VARIABLES INTERNES
!           VIND   :  VARIABLES INTERNES A T
!       OUT SIGF   :  CONTRAINTE A T+DT
!           VINF   :  VARIABLES INTERNES A T+DT
!           IRET   :  CODE RETOUR (O-->OK / 1-->NOOK)
!       ----------------------------------------------------------------
    include 'asterc/r8vide.h'
    include 'asterfort/hujori.h'
    include 'asterfort/hujpre.h'
    include 'asterfort/lceqvn.h'
    include 'asterfort/u2mess.h'
    integer :: nvi, imat, iret, nmat
    real(kind=8) :: materf(nmat, 2), sigd(6), sigf(6), angmas(3)
    real(kind=8) :: vind(*), vinf(*), deps(6), crit(*)
    character(len=8) :: mod
    character(len=7) :: etatd
!
    integer :: i
    real(kind=8) :: zero, un, bid66(6, 6), matert(22, 2)
    logical :: reorie
    parameter      (zero = 0.d0)
    parameter      (un   = 1.d0)
!       ----------------------------------------------------------------
    if (mod(1:6) .eq. 'D_PLAN') then
        do 10 i = 5, 6
            deps(i) = zero
            sigd(i) = zero
10      continue
    endif
!
    if (( (vind(24) .eq. zero) .or. (vind(24) .eq. -un .and. vind(28) .eq. zero) ) .and.&
        ( (vind(25) .eq. zero) .or. (vind(25) .eq. -un .and. vind(29) .eq. zero) ) .and.&
        ( (vind(26) .eq. zero) .or. (vind(26) .eq. -un .and. vind(30) .eq. zero) ) .and.&
        ( (vind(27) .eq. zero) .or. (vind(27) .eq. -un .and. vind(31) .eq. zero) )) then
        etatd = 'ELASTIC'
    else
        etatd = 'PLASTIC'
    endif
!
! --- ORIENTATION DES CONTRAINTES SELON ANGMAS VERS REPERE LOCAL
    if (angmas(1) .eq. r8vide()) call u2mess('F', 'ALGORITH8_20')
    reorie =(angmas(1).ne.zero) .or. (angmas(2).ne.zero)&
     &          .or. (angmas(3).ne.zero)
    call hujori('LOCAL', 1, reorie, angmas, sigd,&
                bid66)
    call hujori('LOCAL', 1, reorie, angmas, deps,&
                bid66)
!
    do 20 i = 1, 22
        matert(i,1) = materf(i,1)
        matert(i,2) = materf(i,2)
20  continue
!
    call hujpre(etatd, mod, crit, imat, matert,&
                deps, sigd, sigf, vind, iret)
    call lceqvn(nvi, vind, vinf)
!
end subroutine
