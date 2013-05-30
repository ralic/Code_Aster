subroutine mmmsta(ndim, leltf, lpenaf, loptf, djeut,&
                  dlagrf, coefaf, coefff, tau1, tau2,&
                  lcont, ladhe, lambda, rese, nrese)
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
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jevech.h'
    include 'asterfort/mmtrpr.h'
    integer :: ndim
    real(kind=8) :: dlagrf(2), djeut(3)
    logical :: loptf, lpenaf, leltf
    real(kind=8) :: tau1(3), tau2(3)
    logical :: lcont, ladhe
    real(kind=8) :: rese(3), nrese, lambda
    real(kind=8) :: coefaf, coefff
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - CALCUL)
!
! PREPARATION DES CALCULS - LECTURE DES STATUTS
!
! ----------------------------------------------------------------------
!
!
! IN  NDIM   : DIMENSION DE L'ESPACE
! IN  LPENAF : .TRUE. SI FROTTEMENT PENALISE
! IN  LELTF  : .TRUE. SI ELEMENT DE FROTTEMENT
! IN  LOPTF  : .TRUE. SI OPTION DE FROTTEMENT
! IN  DLAGRF : INCREMENT DEPDEL DES LAGRANGIENS DE FROTTEMENT
! IN  DJEUT  : INCREMENT DEPDEL DU JEU TANGENT
! IN  COEFAF : COEF_AUGM_FROT
! IN  COEFFF : COEFFICIENT DE FROTTEMENT DE COULOMB
! IN  TAU1   : PREMIER VECTEUR TANGENT
! IN  TAU2   : SECOND VECTEUR TANGENT
! OUT LCONT  : .TRUE. SI CONTACT (SU=1)
! OUT LADHE  : .TRUE. SI ADHERENCE
! OUT LAMBDA : VALEUR DU MULT. DE CONTACT (SEUIL FIXE)
! OUT RESE   : SEMI-MULTIPLICATEUR GTK DE FROTTEMENT
!               GTK = LAMBDAF + COEFAF*VITESSE
! OUT NRESE  : NORME DU SEMI-MULTIPLICATEUR GTK DE FROTTEMENT
!
!
!
!
    integer :: jpcf
    integer :: indco
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    lcont = .false.
    ladhe = .false.
    nrese = 0.d0
    rese(1) = 0.d0
    rese(2) = 0.d0
    rese(3) = 0.d0
!
! --- RECUPERATION DES STATUTS
!
    call jevech('PCONFR', 'L', jpcf)
    indco = nint(zr(jpcf-1+12))
!
! --- STATUT DU CONTACT
!
    lcont = indco.eq.1
!
! --- PAS DE FROTTEMENT SI CALCUL OPTION CONTACT
!
    if (.not.loptf) then
        leltf = .false.
    endif
!
! --- STATUT DU CONTACT - CAS DU FROTTEMENT
!
    if (loptf) then
        if (coefff .eq. 0.d0) lcont = .false.
        if (lambda .eq. 0.d0) lcont = .false.
        if (.not.leltf) lcont = .false.
    endif
!
! --- ETAT D'ADHERENCE DU POINT DE CONTACT
!
    if (loptf .and. lcont) then
        call mmtrpr(ndim, lpenaf, djeut, dlagrf, coefaf,&
                    tau1, tau2, ladhe, rese, nrese)
    endif
!
    call jedema()
!
end subroutine
