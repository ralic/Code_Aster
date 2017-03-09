subroutine te0365(option, nomte)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: ayaovi-dzifa.kudawoo at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jevech.h"
#include "asterfort/mmelem.h"
#include "asterfort/mmlagc.h"
#include "asterfort/mmmlav.h"
#include "asterfort/mmmlcf.h"
#include "asterfort/mmmpha.h"
#include "asterfort/mmmsta.h"
#include "asterfort/mmmvas.h"
#include "asterfort/mmmvex.h"
#include "asterfort/mmvape.h"
#include "asterfort/mmvfpe.h"
#include "asterfort/mmvppe.h"
#include "asterfort/vecini.h"
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
    integer :: jvect, jpcf
    integer :: iresof, iresog
    integer :: iresof_prev, iresog_prev
    integer :: ndexfr
    integer :: ndexfr_prev
!    
    real(kind=8) :: norm(3)=0.0, tau1(3)=0.0, tau2(3)=0.0
    real(kind=8) :: norm_prev(3)=0.0, tau1_prev(3)=0.0, tau2_prev(3)=0.0
    real(kind=8) :: mprojt(3, 3)=0.0
    real(kind=8) ::  mprojt_prev(3, 3)=0.0
    real(kind=8) :: rese(3)=0.0, nrese=0.0
    real(kind=8) :: rese_prev(3)=0.0, nrese_prev=0.0
    real(kind=8) :: wpg, jacobi
    real(kind=8) :: coefff=0.0, lambda=0.0, lambds=0.0
    real(kind=8) :: lambda_prev=0.0, lambds_prev=0.0
    real(kind=8) :: coefac=0.0, coefaf=0.0
    real(kind=8) :: coefac_prev=0.0, coefaf_prev=0.0
    real(kind=8) :: jeusup=0.0
    real(kind=8) :: jeusup_prev=0.0
    real(kind=8) :: dlagrc=0.0, dlagrf(2)=0.0
    real(kind=8) :: dlagrc_prev=0.0, dlagrf_prev(2)=0.0
    real(kind=8) :: jeu=0.0, djeu(3)=0.0, djeut(3)=0.0
    real(kind=8) :: jeu_prev=0.0, djeut_prev(3)=0.0,djeu_prev(3)=0.0
    character(len=8) :: typmae, typmam
    character(len=9) :: phasep
    character(len=9) :: phasep_prev
    real(kind=8) :: alpha_cont=1.0
!    
    aster_logical :: laxis = .false. , leltf = .false. 
    aster_logical :: lpenac = .false. , lpenaf = .false. 
    aster_logical :: lpenac_prev = .false. , lpenaf_prev = .false. 
    aster_logical :: loptf = .false. , ldyna = .false. , lfovit = .false. , lcont = .false. 
    aster_logical :: lcont_prev = .false. 
    aster_logical :: ladhe = .false. 
    aster_logical :: ladhe_prev  = .false. 
    aster_logical :: l_previous_cont = .false. , l_previous_frot = .false. , l_previous = .false. 
!    
    aster_logical :: debug = .false. 
    real(kind=8) :: ffe(9), ffm(9), ffl(9)
!
    real(kind=8) :: vectcc(9)
    real(kind=8) :: vectcc_prev(9)
    real(kind=8) :: vectff(18)
    real(kind=8) :: vectff_prev(18)
    real(kind=8) :: vectee(27), vectmm(27)
    real(kind=8) :: vectee_prev(27), vectmm_prev(27)
    real(kind=8) :: vtmp(81)
    real(kind=8) :: vtmp_prev(81)
!
    character(len=24) :: typelt
!
! ----------------------------------------------------------------------
!
!
! --- INITIALISATIONS
!
    call vecini(81, 0.d0, vtmp)
    call vecini(81, 0.d0, vtmp_prev)
    call vecini(9, 0.d0, vectcc)
    call vecini(9, 0.d0, vectcc_prev)
    call vecini(18, 0.d0, vectff)
    call vecini(18, 0.d0, vectff_prev)
    call vecini(27, 0.d0, vectee)
    call vecini(27, 0.d0, vectee_prev)
    call vecini(27, 0.d0, vectmm)
    call vecini(27, 0.d0, vectmm_prev)
    debug = .false.
!
! --- TYPE DE MAILLE DE CONTACT
!
    typelt = 'POIN_ELEM'
    loptf = option.eq.'CHAR_MECA_FROT'
    call jevech('PCONFR', 'L', jpcf)
    l_previous_cont = (nint(zr(jpcf-1+30)) .eq. 1 )
    l_previous_frot = (nint(zr(jpcf-1+44)) .eq. 1 ) .and. .false.
    if (option .eq. 'RIGI_CONT') l_previous = l_previous_cont
    if (option .eq. 'RIGI_FROT') l_previous = l_previous_frot
    
!
! --- PREPARATION DES CALCULS - INFOS SUR LA MAILLE DE CONTACT
!
    call mmelem(nomte, ndim, nddl, typmae, nne,&
                typmam, nnm, nnl, nbcps, nbdm,&
                laxis, leltf)
!
! --- PREPARATION DES CALCULS - LECTURE DES COEFFICIENTS 
!                             - LECTURE FONCTIONNALITES AVANCEES
!
    call mmmlcf(coefff, coefac, coefaf, lpenac, lpenaf,&
                iresof, iresog, lambds, .false._1)
                
    call mmmlav(ldyna, lfovit, jeusup, ndexfr, coefac,&
                coefaf)
                
                
    if (l_previous) then
        call mmmlcf(coefff, coefac_prev, coefaf_prev, lpenac_prev, lpenaf_prev,&
                    iresof_prev, iresog_prev, lambds_prev, l_previous)
        call mmmlav(ldyna, lfovit, jeusup_prev, ndexfr_prev, coefac_prev,&
                    coefaf_prev)
        
!        debug = .false.
!        if (debug) then 
        
!            write (6,*) "MMLCF : DEBUGGING PREVIOUS AND CURRENT"
!            write (6,*) "coefac_prev",coefac_prev, "coefac",coefac
!            write (6,*) "coefaf_prev",coefaf_prev,"coefaf",coefaf
!            write (6,*) "lpenac_prev",lpenac_prev,"lpenac",lpenac
!            write (6,*) "lpenaf_prev",lpenaf_prev,"lpenaf",lpenaf
!            write (6,*) "iresof_prev",iresof_prev,"iresof",iresof
!            write (6,*) "iresog_prev",iresog_prev,"iresog",iresog
!            write (6,*) "lambs_prev",lambds_prev,"lambds",lambds
!            write (6,*) "MMLCF : END DEBUGGING PREVIOUS AND CURRENT"
        
!            write (6,*) "MMLAV : DEBUGGING PREVIOUS AND CURRENT"
!            write (6,*) "jeusup_prev",jeusup_prev, "jeusup",jeusup
!            write (6,*) "ndexfr_prev",ndexfr_prev,"ndexfr_prev",ndexfr
!            write (6,*) "MMLAV : END DEBUGGING PREVIOUS AND CURRENT"
            
!        endif
!        debug = .false.
    endif
!
! --- PREPARATION DES DONNEES
!
    if (typelt .eq. 'POIN_ELEM') then
!
! ----- CALCUL DES QUANTITES
!
        call mmvppe(typmae, typmam, iresog, ndim, nne,&
                             nnm, nnl, nbdm, laxis, ldyna,&
                             lfovit,lpenac, jeusup, ffe, ffm, ffl,&
                             norm, tau1, tau2, mprojt, jacobi,&
                             wpg, dlagrc, dlagrf, jeu, djeu,&
                             djeut, .false._1)
                             
        if (l_previous) then
            call      mmvppe(typmae, typmam, iresog, ndim, nne,&
                             nnm, nnl, nbdm, laxis, ldyna,&
                             lfovit,lpenac_prev, jeusup_prev, ffe, ffm, ffl,&
                             norm_prev, tau1_prev, tau2_prev, mprojt_prev, jacobi,&
                             wpg, dlagrc_prev, dlagrf_prev, jeu_prev, djeu_prev,&
                             djeut_prev, l_previous)
!              call modification_quantity_previous(jeu_prev,dlagrc_prev) 
        endif
!
!
!
!  --- PREPARATION DES DONNEES - CHOIX DU LAGRANGIEN DE CONTACT
!
        call mmlagc(lambds, dlagrc, iresof, lambda)
        if (l_previous) then 
            call mmlagc(lambds_prev, dlagrc_prev, iresof_prev, lambda_prev)
!            debug = .false.
!            if (debug) then 
                   
!                write (6,*) "MMLAGC : DEBUGGING PREVIOUS AND CURRENT"
!                write (6,*) "lambds_prev",lambds_prev, "lambds",lambds
!                write (6,*) "dlagrc_prev",dlagrc_prev,"dlagrc",dlagrc
!                write (6,*) "iresof_prev",iresof_prev,"iresof",iresof
!                write (6,*) "lambda_prev",lambda_prev,"lambda",lambda
!                write (6,*) "MMLAGC : END DEBUGGING PREVIOUS AND CURRENT"
                
!            endif
!            debug = .false.
        endif

!
! ----- STATUTS
!
        call mmmsta(ndim, leltf, lpenaf, loptf, djeut,&
                    dlagrf, coefaf, tau1, tau2, lcont,&
                    ladhe, lambda, rese, nrese,.false._1)
!
! ----- PHASE DE CALCUL : current
!
        call mmmpha(loptf, lcont, ladhe, ndexfr, lpenac,&
                    lpenaf, phasep)
                    
        if (l_previous) then
!
! ----- Statuts  : previous
!
            call mmmsta(ndim, leltf, lpenaf_prev, loptf, djeut_prev,&
                        dlagrf_prev, coefaf_prev, tau1_prev, tau2_prev, lcont_prev,&
                        ladhe_prev, lambda_prev, rese_prev, nrese_prev, l_previous)
!            debug = .false.
!            if (debug) then
!                write (6,*) "MMMSTA : DEBUGGING PREVIOUS AND CURRENT"
!                write (6,*) "lpenaf_prev",lpenaf_prev,"lpenaf_prev",lpenaf
!                write (6,*) "djeut_prev",djeut_prev,"djeut_prev",djeut
!                write (6,*) "dlagrf_prev",djeut_prev,"djeut_prev",djeut
!                write (6,*) "coefaf_prev",djeut_prev,"djeut_prev",djeut
!                write (6,*) "tau1_prev",tau1_prev,"tau1",tau1
!                write (6,*) "tau2_prev",tau2_prev,"tau2",tau2
!                write (6,*) "lcont_prev",lcont_prev,"lcont",lcont
!                write (6,*) "ladhe_prev",ladhe_prev,"ladhe",ladhe
!                write (6,*) "lambda_prev",lambda_prev,"lambda",lambda
!                write (6,*) "rese_prev",rese_prev,"rese",rese
!                write (6,*) "nrese_prev",nrese_prev,"nrese",nrese
!                write (6,*) "MMMSTA : END DEBUGGING PREVIOUS AND CURRENT"
!            endif
!            debug = .false.
!
! ----- PHASE DE CALCUL : previous
!
            call mmmpha(loptf, lcont_prev, ladhe_prev, ndexfr_prev, lpenac_prev,&
                        lpenaf_prev, phasep_prev)
!            debug = .false.
!            if (debug) then 
!                write (6,*) "mmmpha : DEBUGGING PREVIOUS AND CURRENT"
!                write (6,*) "lcont_prev",lcont_prev, "lcont",lcont
!                write (6,*) "ladhe_prev",ladhe_prev,"ladhe",ladhe
!                write (6,*) "ndexfr_prev",ndexfr_prev,"ndexfr",ndexfr
!                write (6,*) "lpenac_prev",lpenac_prev,"lpenac",lpenac
!                write (6,*) "lpenaf_prev",lpenaf_prev,"lpenaf",lpenaf
!                write (6,*) "phasep_prev",phasep_prev,"phasep",phasep
!                write (6,*) "mmmpha : END DEBUGGING PREVIOUS AND CURRENT"
!            endif
!            debug = .false.
        endif
!
    else
        ASSERT(.false.)
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
                    
        if (l_previous) then
            call mmvfpe(phasep_prev, ndim, nne, nnm, norm_prev,&
                        tau1_prev, tau2_prev, mprojt_prev, wpg, ffe,&
                        ffm, jacobi, jeu_prev, coefac_prev, coefaf_prev,&
                        lambda_prev, coefff, dlagrc_prev, dlagrf_prev, djeu_prev,&
                        rese_prev, nrese_prev, vectee_prev, vectmm_prev)
        endif
                        
    else
        ASSERT(.false.)
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
                   
        if (l_previous) then
            call mmvape(phasep_prev, leltf, ndim, nnl, nbcps,&
                        coefac_prev, coefaf_prev, coefff, ffl, wpg,&
                        jeu_prev, jacobi, lambda_prev, tau1_prev, tau2_prev,&
                        mprojt_prev, dlagrc_prev, dlagrf_prev, djeu_prev, rese_prev,&
                        vectcc_prev, vectff_prev)
        endif
    else
        ASSERT(.false.)
    endif
!
! --- MODIFICATIONS EXCLUSION
!
    call mmmvex(nnl, nbcps, ndexfr, vectff)
                  
    if (l_previous) then
        call mmmvex(nnl, nbcps, ndexfr, vectff_prev)
    endif
!
! --- ASSEMBLAGE FINAL
!
    call mmmvas(ndim, nne, nnm, nnl, nbdm,&
                nbcps, vectee, vectmm, vectcc, vectff,&
                vtmp)
              
    if (l_previous) then
        call mmmvas(ndim, nne, nnm, nnl, nbdm,&
                    nbcps, vectee_prev, vectmm_prev, vectcc_prev, vectff_prev,&
                    vtmp_prev)
    endif
    
!---------------------------------------------------------------
!-------------- RECOPIE DANS LA BASE DE TRAVAIL ----------------
!---------------------------------------------------------------

    alpha_cont = zr(jpcf-1+31)
!    if (alpha_cont .lt. 1.d0 ) write (6,*) "alpha_cont",alpha_cont
!    l_previous = .true.   .and. (iresog .ne. 1) .and. (.not. loptf)
!
! --- RECUPERATION DES VECTEURS 'OUT' (A REMPLIR => MODE ECRITURE)
!
    call jevech('PVECTUR', 'E', jvect)
!
! --- RECOPIE VALEURS FINALES
!
    do iddl = 1, nddl
        if (l_previous) then 
            zr(jvect-1+iddl) = alpha_cont * vtmp(iddl) &
                             + (1-alpha_cont) * vtmp_prev(iddl)
        else 
            zr(jvect-1+iddl) = 1.0d0 * vtmp(iddl)
        endif
        
!        if (debug) then
!            if (vtmp(iddl) .ne. 0.d0) then
!                write(6,*) 'TE0365: ',iddl,vtmp(iddl)
!            endif
!        endif
    end do
!
end subroutine
