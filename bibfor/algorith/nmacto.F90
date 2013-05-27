subroutine nmacto(sddisc, ievdac)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
    include 'jeveux.h'
    include 'asterfort/dieven.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/utdidt.h'
    character(len=19) :: sddisc
    integer :: ievdac
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! RECHERCHE DES EVENEMENTS ACTIVES
!
! ----------------------------------------------------------------------
!
!
! IN  SDDISC : SD DISCRETISATION
! OUT IEVDAC : VAUT IECHEC SI EVENEMENT ACTIVE
!                   0 SINON
!
!
!
!
    real(kind=8) :: r8bid
    integer :: ibid, nechec, iechec
    character(len=8) :: k8bid
    logical :: lacti
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- NOMBRE D'EVENT-DRIVEN : NECHEC
!
    call utdidt('L', sddisc, 'LIST', ibid, 'NECHEC',&
                r8bid, nechec, k8bid)
    lacti = .false.
!
! --- BOUCLE SUR LES EVENT-DRIVEN
! --- DES QU'UN EVENT-DRIVEN EST SATISFAIT, ON SORT
! --- ON NE CHERCHE PAS A VERIFIER LES AUTRES EVENEMENTS
! --- ATTENTION, L'ORDRE D'EVALUATION A DONC UNE IMPORTANCE !
!
    ievdac = 0
    do 100 iechec = 1, nechec
        call dieven(sddisc, iechec, lacti)
        if (lacti) then
            ievdac = iechec
            goto 888
        endif
100  end do
!
888  continue
!
    call jedema()
end subroutine
