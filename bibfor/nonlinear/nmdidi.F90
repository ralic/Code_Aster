subroutine nmdidi(modele, lischa, depmoi, vedidi)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    include 'jeveux.h'
    include 'asterc/getvid.h'
    include 'asterc/getvis.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/rsexch.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/vecdid.h'
    character(len=19) :: lischa, vedidi
    character(len=24) :: modele
    character(len=19) :: depmoi
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL)
!
! CALCUL DES VECT_ELEM POUR DIRICHLET DIFFERENTIEL
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : MODELE
! IN  LISCHA : SD L_CHARGES
! IN  DEPMOI : DEPLACEMENTS EN T-
! IN  VEDIDI : VECT_ELEM DES DIDI
!
!
!
!
    integer :: numref, n1, nevo, iret
    character(len=19) :: depdid
    character(len=24) :: evol
    integer :: iarg
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- CONSTRUCTION DE LA CONFIGURATION DE REFERENCE
!
    depdid = depmoi
    call getvis('ETAT_INIT', 'NUME_DIDI', 1, iarg, 1,&
                numref, n1)
    call getvid('ETAT_INIT', 'EVOL_NOLI', 1, iarg, 1,&
                evol, nevo)
    if ((n1.gt.0) .and. (nevo.gt.0)) then
        call rsexch(' ', evol, 'DEPL', numref, depdid,&
                    iret)
        if (iret .ne. 0) call u2mesk('F', 'ALGORITH7_20', 1, evol)
    endif
!
! --- CALCUL DES VECT_ELEM
!
    call vecdid(modele, lischa, depdid, vedidi)
!
    call jedema()
end subroutine
