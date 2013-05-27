subroutine nmextv(neff, formul, nomcmp, valcmp, nvalcp,&
                  valres)
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
    implicit      none
    include 'jeveux.h'
    include 'asterfort/fointe.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    integer :: neff, nvalcp
    character(len=8) :: formul
    character(len=8) :: nomcmp(*)
    real(kind=8) :: valcmp(*), valres(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (EXTRACTION - UTILITAIRE)
!
! EXTRAIRE LES VALEURS DES COMPOSANTES - APPLICATION DE LA FORMULE
!
! ----------------------------------------------------------------------
!
!
! IN  NEFF   : NOMBRE EFFECTIF DE COMPOSANTES
! IN  FORMUL : NOM DE LA FORMULE (' ' SI PAS FORMULE)
! IN  NOMCMP : NOM DES COMPOSANTES
! IN  VALCMP : VALEURS DES COMPOSANTES
! OUT NVALCP : NOMBRE EFFECTIF DE COMPOSANTES
! OUT VALRES : RESULTAT DE LA FORMULE OU DE L'EXTRACTION
!
!
!
!
    integer :: ieff, icode
    real(kind=8) :: valr
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    nvalcp = 0
!
! --- EVALUATION DE LA FONCTION
!
    if (formul .eq. ' ') then
        do 25 ieff = 1, neff
            valres(ieff) = valcmp(ieff)
25      continue
        nvalcp = neff
    else
        call fointe('FM', formul, neff, nomcmp, valcmp,&
                    valr, icode)
        valres(1) = valr
        nvalcp = 1
    endif
!
    call jedema()
!
end subroutine
