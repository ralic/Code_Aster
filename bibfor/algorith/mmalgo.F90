subroutine mmalgo(indcoi, lvites, lglini, jeu, jeuvit,&
                  lambdc, coefac, ctcsta, mmcvca, scotch,&
                  indcon)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    include 'asterfort/mmstac.h'
    integer :: indcoi, indcon, ctcsta
    logical :: lvites, mmcvca, lglini, scotch
    real(kind=8) :: jeu, jeuvit, lambdc, coefac
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - CONTRAINTES ACTIVES)
!
! TRAITEMENT DES DIFFERENTS CAS
!
! ----------------------------------------------------------------------
!
!
! IN  INDCOI : INDICATEUR DE CONTACT INITIAL
!              - INDCOI = 0: PAS DE CONTACT
!              - INDCOI = 1: CONTACT
! IN  LVITES : .TRUE. SI FORMULATION EN VITESSE
! IN  LGLINI : .TRUE. SI CONTACT GLISSIERE AVEC CONTACT_INIT AU PREMIER
!               PAS DE TEMPS
! IN  JEU    : VALEUR DU JEU
! IN  JEUVIT : VALEUR DU GAP DES VITESSES NORMALES
! IN  LAMBDC : MULTIPLICATEUR DE CONTACT DU POINT DE CONTACT
! IN  COEFAC : COEFFICIENT D'AUGMENTATION DU CONTACT RHO_N
! OUT INDCON : INDICATEUR DE CONTACT FINAL
!              - INDCON = 0: PAS DE CONTACT
!              - INDCON = 1: CONTACT
! OUT MMCVCA : INDICATEUR DE CONVERGENCE POUR BOUCLE DES
!              CONTRAINTES ACTIVES
!               .TRUE. SI LA BOUCLE DES CONTRAINTES ACTIVES A CONVERGE
! OUT CTCSTA : NOMBRE DE POINTS AYANT CHANGE DE STATUT DE CONTACT
! OUT SCOTCH : VAUT .TRUE. SI LE NOEUD EST COLLE DANS LE MODELE
!              DE COMPLIANCE
!
! ----------------------------------------------------------------------
!
!
! --- INITIALISATIONS
!
    scotch = .false.
!
! --- STATUT DE CONTACT
!
    call mmstac(indcoi, lvites, jeu, jeuvit, lambdc,&
                coefac, indcon)
!
! --- CAS GLISSIERE: TOUT EST EN CONTACT
!
    if (lglini) indcon = 1
!
! --- THETA-METHODE
!
    if (indcoi .eq. 1) then
        scotch = .true.
    else
        scotch = .false.
    endif
!
! --- CONVERGENCE ?
!
    if (indcoi .ne. indcon) then
        mmcvca = .false.
        ctcsta = ctcsta+1
    endif
!
end subroutine
