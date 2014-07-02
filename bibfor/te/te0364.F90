subroutine te0364(option, nomte)
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
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jevech.h"
#include "asterfort/matini.h"
#include "asterfort/mmelem.h"
#include "asterfort/mmlagc.h"
#include "asterfort/mmmlav.h"
#include "asterfort/mmmlcf.h"
#include "asterfort/mmmpha.h"
#include "asterfort/mmmsta.h"
#include "asterfort/mmmtas.h"
#include "asterfort/mmmtdb.h"
#include "asterfort/mmmtex.h"
#include "asterfort/mmtape.h"
#include "asterfort/mmtfpe.h"
#include "asterfort/mmtgeo.h"
#include "asterfort/mmtppe.h"
!
!
    character(len=16) :: option, nomte
!
! ----------------------------------------------------------------------
!
!
!  CALCUL CALCUL DES MATRICES DE CONTACT ET DE FROTTEMENT
!  DE COULOMB STANDARD  VEC LA METHODE CONTINUE
!
!  OPTION : 'RIGI_CONT' (CALCUL DES MATRICES DE CONTACT )
!           'RIGI_FROT' (CALCUL DES MATRICES DE FROTTEMENT STANDARD)
!
!  ENTREES  ---> OPTION : OPTION DE CALCUL
!           ---> NOMTE  : NOM DU TYPE ELEMENT
!
! ----------------------------------------------------------------------
!
!
! DECLARATION VARIABLES LOCALES
    integer :: i, j, ij
    integer :: nne, nnm, nnl
    integer :: nddl, ndim, nbcps, nbdm
    integer :: jmatt
!
! DECLARATION TYPES RESOLUTION    
    integer :: iresof, iresog
    integer :: ndexfr
    aster_logical :: laxis, leltf
    aster_logical :: lpenac, lpenaf
    aster_logical :: loptf, ldyna, lfovit, lcont
    aster_logical :: ladhe
    aster_logical :: debug
!
! DECLARATION COEFFICIENTS ET TYPE MAILLE        
    real(kind=8) :: coefff, lambda, lambds
    real(kind=8) :: coefac, coefaf
    real(kind=8) :: wpg, jacobi
    real(kind=8) :: norm(3), tau1(3), tau2(3)
    real(kind=8) :: mprojn(3, 3), mprojt(3, 3)
    real(kind=8) :: rese(3), nrese
    real(kind=8) :: jeusup
    real(kind=8) :: dlagrc, dlagrf(2)
    real(kind=8) :: jeu, djeut(3)
!
    character(len=8) :: typmae, typmam
    character(len=9) :: phasep
    real(kind=8) :: ffe(9), ffm(9), ffl(9), dffm(2, 9)
!
    real(kind=8) :: mprt1n(3, 3), mprt2n(3, 3)
    real(kind=8) :: mprt11(3, 3), mprt21(3, 3), mprt22(3, 3)
!
    real(kind=8) :: gene11(3, 3), gene21(3, 3), gene22(3, 3)
    real(kind=8) :: kappa(2, 2), a(2, 2), h(2, 2), ha(2, 2), hah(2, 2)
!
    real(kind=8) :: vech1(3), vech2(3)
!
! DECLARATION MATRICES CONTACT-FROTTEMENT
    real(kind=8) :: mmat(81, 81)
!
    real(kind=8) :: matrcc(9, 9)
!
    real(kind=8) :: matree(27, 27), matrmm(27, 27)
    real(kind=8) :: matnee(27, 27), matnmm(27, 27)
    real(kind=8) :: matfee(27, 27), matfmm(27, 27)
!
    real(kind=8) :: matrem(27, 27), matrme(27, 27)
    real(kind=8) :: matnem(27, 27), matnme(27, 27)
    real(kind=8) :: matfem(27, 27), matfme(27, 27)
!
    real(kind=8) :: matrce(9, 27), matrcm(9, 27)
    real(kind=8) :: matrmc(27, 9), matrec(27, 9)
    real(kind=8) :: matrff(18, 18)
    real(kind=8) :: matrfe(18, 27), matrfm(18, 27)
    real(kind=8) :: matrmf(27, 18), matref(27, 18)
!
!  TYPE ELEMENT
    character(len=24) :: typelt
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS DES MATRICES
!
    call matini(81, 81, 0.d0, mmat)
!
    call matini(9, 9, 0.d0, matrcc)
!
    call matini(27, 27, 0.d0, matree)
    call matini(27, 27, 0.d0, matnee)
    call matini(27, 27, 0.d0, matfee)
!
    call matini(27, 27, 0.d0, matrmm)
    call matini(27, 27, 0.d0, matnmm)
    call matini(27, 27, 0.d0, matfmm)
!
    call matini(27, 27, 0.d0, matrem)
    call matini(27, 27, 0.d0, matnem)
    call matini(27, 27, 0.d0, matfem)
!
    call matini(27, 27, 0.d0, matrme)
    call matini(27, 27, 0.d0, matnme)
    call matini(27, 27, 0.d0, matfme)
!
    call matini(9, 27, 0.d0, matrce)
    call matini(9, 27, 0.d0, matrcm)
    call matini(27, 9, 0.d0, matrec)
    call matini(27, 9, 0.d0, matrmc)
    call matini(18, 18, 0.d0, matrff)
    call matini(18, 27, 0.d0, matrfe)
    call matini(18, 27, 0.d0, matrfm)
    call matini(27, 18, 0.d0, matref)
    call matini(27, 18, 0.d0, matrmf)
!
    debug = .false.
!
! --- TYPE DE MAILLE DE CONTACT
!
    typelt = 'POIN_ELEM'
    loptf = option.eq.'RIGI_FROT'
!
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
        call mmtppe(typmae, typmam, ndim, nne, nnm,&
                    nnl, nbdm, iresog, laxis, ldyna,&
                    jeusup, ffe, ffm, dffm, ffl,&
                    jacobi, wpg, jeu, djeut, dlagrc,&
                    dlagrf, norm, tau1, tau2, mprojn,&
                    mprojt, mprt1n, mprt2n, gene11, gene21,&
                    gene22, kappa, h, vech1, vech2,&
                    a, ha, hah, mprt11, mprt21,&
                    mprt22)
!
!
!
! ----- CHOIX DU LAGRANGIEN DE CONTACT
!
        call mmlagc(lambds, dlagrc, iresof, lambda)
!
! ----- STATUTS
!
!
        call mmmsta(ndim, leltf, lpenaf, loptf, djeut,&
                    dlagrf, coefaf, tau1, tau2, lcont,&
                    ladhe, lambda, rese, nrese)
!
! ----- PHASE DE CALCUL
!
        call mmmpha(loptf, lcont, ladhe, ndexfr, lpenac,&
                    lpenaf, phasep)
!
    else
        ASSERT(.false.)
    endif
!
! --- CALCUL FORME FAIBLE FORCE DE CONTACT/FROTTEMENT
!
    if (typelt .eq. 'POIN_ELEM') then
!
! ----- CONTRIBUTIONS STANDARDS : POINT FIXE/NEWTON GENE /NEWTON PARTIEL
!
        call mmtfpe(phasep, iresof, ndim, nne, nnm,&
                    nnl, nbcps, wpg, jacobi, ffl,&
                    ffe, ffm, norm, tau1, tau2,&
                    mprojn, mprojt, rese, nrese, lambda,&
                    coefff, coefaf, coefac, dlagrf, djeut,&
                    matree, matrmm, matrem, matrme, matrec,&
                    matrmc, matref, matrmf)
!
! ----- CONTRIBUTIONS NON-LINEARITES GEOMETRIQUES NEWTON GENE
!
        if (iresog .eq. 1) then
!    WRITE (6,*) matree(1,1),"valeur sortie MMTFPE" 
            call mmtgeo(phasep, ndim, nne, nnm, mprt1n,&
                        mprt2n, mprojn, mprt11, mprt21, mprt22,&
                        wpg, ffe, ffm, dffm, jacobi,&
                        coefac, jeu, dlagrc, kappa, vech1,&
                        vech2, h, hah, matree, matrmm,&
                        matrem, matrme)
        endif
!
!
!
!
!
!
    else
        ASSERT(.false.)
    endif
!
! --- CALCUL FORME FAIBLE LOI DE CONTACT/FROTTEMENT
!
    if (typelt .eq. 'POIN_ELEM') then
        call mmtape(phasep, leltf, ndim, nnl, nne,&
                    nnm, nbcps, wpg, jacobi, ffl,&
                    ffe, ffm, norm, tau1, tau2,&
                    mprojt, rese, nrese, lambda, coefff,&
                    coefaf, coefac, matrcc, matrff, matrce,&
                    matrcm, matrfe, matrfm)
    else
        ASSERT(.false.)
    endif
!
! --- MODIFICATIONS EXCLUSION
!
    call mmmtex(ndexfr, ndim, nnl, nne, nnm,&
                nbcps, matrff, matrfe, matrfm, matref,&
                matrmf)
!
! --- ASSEMBLAGE FINAL
!
    call mmmtas(nbdm, ndim, nnl, nne, nnm,&
                nbcps, matrcc, matree, matrmm, matrem,&
                matrme, matrce, matrcm, matrmc, matrec,&
                matrff, matrfe, matrfm, matrmf, matref,&
                mmat)
!
    if ((lpenac.and.(option.eq.'RIGI_CONT')) .or.&
        ((option.eq.'RIGI_FROT').and.(iresof.ne.0)) .or.&
        (lpenaf.and.(option.eq.'RIGI_FROT'))) then
!
! --- RECUPERATION DE LA MATRICE 'OUT' NON SYMETRIQUE
!
        call jevech('PMATUNS', 'E', jmatt)
!
! --- FIN DE CHANGEMENT ET COPIE
!
        do 760 j = 1, nddl
            do 750 i = 1, nddl
                ij = j+nddl*(i-1)
                zr(jmatt+ij-1) = mmat(i,j)
                if (debug) then
                    call mmmtdb(mmat(i, j), 'IJ', i, j)
                endif
750         continue
760     continue
    else
!
! --- RECUPERATION DE LA MATRICE 'OUT' SYMETRIQUE
!
        call jevech('PMATUUR', 'E', jmatt)
!
! --- FIN DE CHANGEMENT ET COPIE
!
        do 761 j = 1, nddl
            do 751 i = 1, j
                ij = (j-1)*j/2 + i
                zr(jmatt+ij-1) = mmat(i,j)
                if (debug) then
                    call mmmtdb(mmat(i, j), 'IJ', i, j)
                endif
751         continue
761     continue
!
    endif
!
    call jedema()
end subroutine
