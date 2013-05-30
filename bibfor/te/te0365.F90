subroutine te0365(option, nomte)
!
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit      none
    include       'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jevech.h'
    include 'asterfort/mmelem.h'
    include 'asterfort/mmlagc.h'
    include 'asterfort/mmmlav.h'
    include 'asterfort/mmmlcf.h'
    include 'asterfort/mmmpha.h'
    include 'asterfort/mmmsta.h'
    include 'asterfort/mmmvas.h'
    include 'asterfort/mmmvex.h'
    include 'asterfort/mmvape.h'
    include 'asterfort/mmvfpe.h'
    include 'asterfort/mmvppe.h'
    include 'asterfort/vecini.h'
    character(len=16) :: option, nomte
!
! ----------------------------------------------------------------------
!
!  CALCUL DES SECONDS MEMBRES DE CONTACT ET DE FROTTEMENT DE COULOMB STD
!        AVEC LA METHODE CONTINUE
!
!  OPTION : 'CHAR_MECA_CONT' (CALCUL DU SECOND MEMBRE DE CONTACT)
!           'CHAR_MECA_FROT' (CALCUL DU SECOND MEMBRE DE
!                              FROTTEMENT STANDARD )
!
!  ENTREES  ---> OPTION : OPTION DE CALCUL
!           ---> NOMTE  : NOM DU TYPE ELEMENT
!
! ----------------------------------------------------------------------
!
    integer :: iddl
    integer :: nne, nnm, nnl
    integer :: nddl, ndim, nbcps, nbdm
    integer :: jvect
    integer :: iresof, iresog
    integer :: ndexfr
    real(kind=8) :: norm(3), tau1(3), tau2(3)
    real(kind=8) :: mprojt(3, 3)
    real(kind=8) :: rese(3), nrese
    real(kind=8) :: wpg, jacobi
    real(kind=8) :: coefff, lambda, lambds
    real(kind=8) :: coefac, coefaf
    real(kind=8) :: jeusup
    real(kind=8) :: dlagrc, dlagrf(2)
    real(kind=8) :: jeu, djeu(3), djeut(3)
    character(len=8) :: typmae, typmam
    character(len=9) :: phasep
    logical :: laxis, leltf
    logical :: lpenac, lpenaf
    logical :: loptf, ldyna, lfovit, lcont
    logical :: ladhe
    logical :: debug
    real(kind=8) :: ffe(9), ffm(9), ffl(9)
!
    real(kind=8) :: vectcc(9)
    real(kind=8) :: vectff(18)
    real(kind=8) :: vectee(27), vectmm(27)
    real(kind=8) :: vtmp(81)
!
    character(len=24) :: typelt
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    call vecini(81, 0.d0, vtmp)
    call vecini(9, 0.d0, vectcc)
    call vecini(18, 0.d0, vectff)
    call vecini(27, 0.d0, vectee)
    call vecini(27, 0.d0, vectmm)
    debug = .false.
!
! --- TYPE DE MAILLE DE CONTACT
!
    typelt = 'POIN_ELEM'
    loptf = option.eq.'CHAR_MECA_FROT'
!
! --- PREPARATION DES CALCULS - INFOS SUR LA MAILLE DE CONTACT
!
    call mmelem(nomte, ndim, nddl, typmae, nne,&
                typmam, nnm, nnl, nbcps, nbdm,&
                laxis, leltf)
!
! --- PREPARATION DES CALCULS - LECTURE DES COEFFICIENTS
!
    call mmmlcf(coefff, coefac, coefaf, lpenac, lpenaf,&
                iresof, iresog, lambds)
!
! --- PREPARATION DES CALCULS - LECTURE FONCTIONNALITES AVANCEES
!
    call mmmlav(ldyna, lfovit, jeusup, ndexfr, coefac,&
                coefaf)
!
! --- PREPARATION DES DONNEES
!
    if (typelt .eq. 'POIN_ELEM') then
!
! ----- CALCUL DES QUANTITES
!
        call mmvppe(typmae, typmam, iresog, ndim, nne,&
                    nnm, nnl, nbdm, laxis, ldyna,&
                    lfovit, jeusup, ffe, ffm, ffl,&
                    norm, tau1, tau2, mprojt, jacobi,&
                    wpg, dlagrc, dlagrf, jeu, djeu,&
                    djeut)
!
! ----- CHOIX DU LAGRANGIEN DE CONTACT
!
        call mmlagc(lambds, dlagrc, iresof, lambda)
!
! ----- STATUTS
!
        call mmmsta(ndim, leltf, lpenaf, loptf, djeut,&
                    dlagrf, coefaf, coefff, tau1, tau2,&
                    lcont, ladhe, lambda, rese, nrese)
!
! ----- PHASE DE CALCUL
!
        call mmmpha(loptf, lcont, ladhe, ndexfr, lpenac,&
                    lpenaf, phasep)
!
    else
        call assert(.false.)
    endif
!
! --- CALCUL FORME FAIBLE FORCE DE CONTACT/FROTTEMENT
!
    if (typelt .eq. 'POIN_ELEM') then
        call mmvfpe(phasep, ndim, nne, nnm, norm,&
                    tau1, tau2, mprojt, wpg, ffe,&
                    ffm, jacobi, jeu, coefac, coefaf,&
                    lambda, coefff, dlagrc, dlagrf, djeu,&
                    rese, nrese, vectee, vectmm)
    else
        call assert(.false.)
    endif
!
! --- CALCUL FORME FAIBLE LOI DE CONTACT/FROTTEMENT
!
    if (typelt .eq. 'POIN_ELEM') then
        call mmvape(phasep, leltf, ndim, nnl, nbcps,&
                    coefac, coefaf, coefff, ffl, wpg,&
                    jeu, jacobi, lambda, tau1, tau2,&
                    mprojt, dlagrc, dlagrf, djeu, rese,&
                    vectcc, vectff)
    else
        call assert(.false.)
    endif
!
! --- MODIFICATIONS EXCLUSION
!
    call mmmvex(nnl, nbcps, ndexfr, vectff)
!
! --- ASSEMBLAGE FINAL
!
    call mmmvas(ndim, nne, nnm, nnl, nbdm,&
                nbcps, vectee, vectmm, vectcc, vectff,&
                vtmp)
!
! --- RECUPERATION DES VECTEURS 'OUT' (A REMPLIR => MODE ECRITURE)
!
    call jevech('PVECTUR', 'E', jvect)
!
! --- RECOPIE VALEURS FINALES
!
    do 60 iddl = 1, nddl
        zr(jvect-1+iddl) = vtmp(iddl)
        if (debug) then
            if (vtmp(iddl) .ne. 0.d0) then
                write(6,*) 'TE0365: ',iddl,vtmp(iddl)
            endif
        endif
60  end do
!
    call jedema()
end subroutine
