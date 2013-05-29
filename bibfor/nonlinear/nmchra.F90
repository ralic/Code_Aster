subroutine nmchra(sddyna, optamo, lcamor)
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
    implicit none
    include 'jeveux.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/ndynlo.h'
    character(len=19) :: sddyna
    character(len=16) :: optamo
    logical :: lcamor
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL)
!
! CHOIX DE REASSEMBLAGE DE LA MATRICE D'AMORTISSEMENT
!
! ----------------------------------------------------------------------
!
!
! IN  SDDISC : SD DISC_INST
! IN  SDDYNA : SD DYNAMIQUE
! OUT OPTAMO : OPTION POUR L'AMORTISSEMENT
! OUT LCAMOR : .TRUE. SI CALCUL MATRICE D'AMORTISSEMENT
!
!
!
!
    logical :: lktan
    integer :: ifm, niv
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE><CALC> CHOIX D''ASSEMBLAGE DE '//&
        'MATRICE AMORTISSEMENT'
    endif
!
! --- INITIALISATIONS
!
    lcamor = .false.
    optamo = 'AMOR_MECA'
!
! --- FONCTIONNALITES ACTIVEES
!
    lktan = ndynlo(sddyna,'RAYLEIGH_KTAN')
!
! --- REACTUALISATION DE LA MATRICE D AMORTISSEMENT DE RAYLEIGH
!
    if (lktan) then
        lcamor = .true.
    endif
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        if (lcamor) then
            write (ifm,*) '<MECANONLINE><CALC> ON ASSEMBLE LA MATRICE' //&
     &                  ' D''AMORTISSEMENT'
        else
            write (ifm,*) '<MECANONLINE><CALC> ON N''ASSEMBLE PAS '//&
            'LA MATRICE D''AMORTISSEMENT'
        endif
    endif
!
    call jedema()
!
end subroutine
