subroutine calico(charz, nomaz, nomoz, ndim, iform,&
                  ligret)
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
    implicit      none
    include 'jeveux.h'
    include 'asterc/getfac.h'
    include 'asterfort/caraco.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/limaco.h'
    include 'asterfort/surfco.h'
    character(len=*) :: charz
    character(len=*) :: nomaz
    character(len=*) :: nomoz
    integer :: ndim, iform
    character(len=19) :: ligret
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT
!
! TRAITEMENT DU CONTACT DANS DEFI_CONTACT
!
! ----------------------------------------------------------------------
!
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  NOMA   : NOM DU MAILLAGE
! IN  NOMO   : NOM DU MODELE
! IN  NDIM   : NOMBRE DE DIMENSIONS DU PROBLEME
! IN  IFORM  : TYPE DE FORMULATION (DISCRETE/CONTINUE/XFEM)
! OUT LIGRET : LIGREL D'ELEMENTS TARDIFS DU CONTACT
!
!
!
!
    character(len=8) :: char, noma, nomo
    character(len=16) :: motfac
    integer :: nzoco
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    nomo = nomoz(1:8)
    char = charz
    noma = nomaz
    nzoco = 0
    motfac = 'ZONE'
!
! --- RECUPERATION DU NOMBRE DE ZONES DE CONTACT (NOMBRE D'OCCURENCES)
!
    call getfac(motfac, nzoco)
!
    if (nzoco .eq. 0) then
        goto 999
    endif
!
! --- RECUPERATION DES CARACTERISTIQUES DU CONTACT
!
    call caraco(char, nomo, motfac, nzoco, iform)
!
! --- LECTURE DES MAILLES DE CONTACT  ET CREATION DES SDS
!
    call limaco(char, motfac, noma, nomo, ndim,&
                nzoco, ligret)
!
! --- IMPRESSIONS SUR LES ZONES/SURFACES/MAILLES/NOEUDS DE CONTACT
!
    call surfco(char, noma)
!
999  continue
!
    call jedema()
!
end subroutine
