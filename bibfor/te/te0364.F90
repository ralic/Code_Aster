subroutine te0364(option, nomte)
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
    integer :: jmatt, jpcf
!
! DECLARATION TYPES RESOLUTION    
    integer :: iresof, iresog
    integer :: iresof_prev, iresog_prev
    integer :: ndexfr
    integer :: ndexfr_prev
    aster_logical :: laxis, leltf
    aster_logical :: lpenac, lpenaf
    aster_logical :: lpenac_prev, lpenaf_prev
    aster_logical :: loptf, ldyna,  lcont
    aster_logical :: lcont_prev
    aster_logical :: ladhe
    aster_logical :: ladhe_prev
    aster_logical :: debug
!    
    aster_logical :: l_previous_cont, l_previous_frot, l_previous
!
! DECLARATION COEFFICIENTS ET TYPE MAILLE        
    real(kind=8) :: coefff  =  0.0
    real(kind=8) :: lambda = 0.0, lambds = 0.0
    real(kind=8) :: lambda_prev = 0.0 , lambds_prev =0.0
    real(kind=8) :: coefac = 0.0 , coefaf=0.0
    real(kind=8) :: coefac_prev =0.0, coefaf_prev=0.0
    real(kind=8) :: wpg, jacobi
    real(kind=8) :: norm(3) = 0.0 , tau1(3) = 0.0 , tau2(3) =0.0 
    real(kind=8) :: norm_prev(3) = 0.0 , tau1_prev(3) = 0.0 , tau2_prev(3)=0.0
    real(kind=8) :: mprojn(3, 3)=0.0, mprojt(3, 3)=0.0
    real(kind=8) :: mprojn_prev(3, 3)=0.0, mprojt_prev(3, 3)=0.0
    real(kind=8) :: rese(3)=0.0, nrese=0.0
    real(kind=8) :: rese_prev(3)=0.0, nrese_prev=0.0
    real(kind=8) :: jeusup=0.0
    real(kind=8) :: jeusup_prev=0.0
    real(kind=8) :: dlagrc=0.0, dlagrf(2)=0.0
    real(kind=8) :: dlagrc_prev=0.0, dlagrf_prev(2)=0.0
    real(kind=8) :: jeu=0.0, djeut(3)=0.0
    real(kind=8) :: jeu_prev=0.0, djeut_prev(3) = 0.0
    real(kind=8) :: alpha_cont=0.0 , alpha_frot=0.0
!
    character(len=8) :: typmae, typmam
    character(len=9) :: phasep
    character(len=9) :: phasep_prev
    real(kind=8) :: ffe(9), ffm(9), ffl(9), dffm(2, 9)
!
    real(kind=8) :: mprt1n(3, 3)=0.0, mprt2n(3, 3)=0.0
    real(kind=8) :: mprt1n_prev(3, 3)=0.0, mprt2n_prev(3, 3)=0.0
    real(kind=8) :: mprt11(3, 3)=0.0, mprt21(3, 3)=0.0, mprt22(3, 3)=0.0
    real(kind=8) :: mprt11_prev(3, 3)=0.0, mprt21_prev(3, 3)=0.0, mprt22_prev(3, 3)=0.0
!
    real(kind=8) :: gene11(3, 3)=0.0, gene21(3, 3)=0.0, gene22(3, 3)=0.0
    real(kind=8) :: gene11_prev(3, 3)=0.0, gene21_prev(3, 3)=0.0, gene22_prev(3, 3)=0.0
    real(kind=8) :: kappa(2, 2)=0.0, a(2, 2)=0.0, h(2, 2)=0.0, ha(2, 2)=0.0, hah(2, 2)=0.0
    real(kind=8) :: kappa_prev(2, 2)=0., a_prev(2, 2)=0.0, h_prev(2, 2)=0.0
    real(kind=8) :: ha_prev(2, 2)=0.0, hah_prev(2, 2)=0.0
    real(kind=8) :: vech1(3)=0.0, vech2(3)=0.0
    real(kind=8) :: vech1_prev(3)=0.0, vech2_prev(3)=0.0
!
! DECLARATION MATRICES CONTACT-FROTTEMENT
    real(kind=8) :: mmat(81, 81)
    real(kind=8) :: mmat_prev(81, 81)
!
    real(kind=8) :: matrcc(9, 9)
    real(kind=8) :: matrcc_prev(9, 9)
!
    real(kind=8) :: matree(27, 27), matrmm(27, 27)
    real(kind=8) :: matree_prev(27, 27), matrmm_prev(27, 27)
    real(kind=8) :: matnee(27, 27), matnmm(27, 27)
    real(kind=8) :: matnee_prev(27, 27), matnmm_prev(27, 27)
    real(kind=8) :: matfee(27, 27), matfmm(27, 27)
    real(kind=8) :: matfee_prev(27, 27), matfmm_prev(27, 27)
!
    real(kind=8) :: matrem(27, 27), matrme(27, 27)
    real(kind=8) :: matrem_prev(27, 27), matrme_prev(27, 27)
    real(kind=8) :: matnem(27, 27), matnme(27, 27)
    real(kind=8) :: matnem_prev(27, 27), matnme_prev(27, 27)
    real(kind=8) :: matfem(27, 27), matfme(27, 27)
    real(kind=8) :: matfem_prev(27, 27), matfme_prev(27, 27)
!
    real(kind=8) :: matrce(9, 27), matrcm(9, 27)
    real(kind=8) :: matrce_prev(9, 27), matrcm_prev(9, 27)
    real(kind=8) :: matrmc(27, 9), matrec(27, 9)
    real(kind=8) :: matrmc_prev(27, 9), matrec_prev(27, 9)
    real(kind=8) :: matrff(18, 18)
    real(kind=8) :: matrff_prev(18, 18)
    real(kind=8) :: matrfe(18, 27), matrfm(18, 27)
    real(kind=8) :: matrfe_prev(18, 27), matrfm_prev(18, 27)
    real(kind=8) :: matrmf(27, 18), matref(27, 18)
    real(kind=8) :: matrmf_prev(27, 18), matref_prev(27, 18)
!
!  TYPE ELEMENT
    character(len=24) :: typelt
!
! ----------------------------------------------------------------------
!
!
! --- INITIALISATIONS DES MATRICES
!
    call matini(81, 81, 0.d0, mmat)
    call matini(81, 81, 0.d0, mmat_prev)
!
    call matini(9, 9, 0.d0, matrcc)
    call matini(9, 9, 0.d0, matrcc_prev)
!
    call matini(27, 27, 0.d0, matree)
    call matini(27, 27, 0.d0, matree_prev)
    call matini(27, 27, 0.d0, matnee)
    call matini(27, 27, 0.d0, matnee_prev)
    call matini(27, 27, 0.d0, matfee)
    call matini(27, 27, 0.d0, matfee_prev)
!
    call matini(27, 27, 0.d0, matrmm)
    call matini(27, 27, 0.d0, matrmm_prev)
    call matini(27, 27, 0.d0, matnmm)
    call matini(27, 27, 0.d0, matnmm_prev)
    call matini(27, 27, 0.d0, matfmm)
    call matini(27, 27, 0.d0, matfmm_prev)
!
    call matini(27, 27, 0.d0, matrem)
    call matini(27, 27, 0.d0, matrem_prev)
    call matini(27, 27, 0.d0, matnem)
    call matini(27, 27, 0.d0, matnem_prev)
    call matini(27, 27, 0.d0, matfem)
    call matini(27, 27, 0.d0, matfem_prev)
!
    call matini(27, 27, 0.d0, matrme)
    call matini(27, 27, 0.d0, matrme_prev)
    call matini(27, 27, 0.d0, matnme)
    call matini(27, 27, 0.d0, matnme_prev)
    call matini(27, 27, 0.d0, matnme)
    call matini(27, 27, 0.d0, matnme_prev)
    call matini(27, 27, 0.d0, matfme)
    call matini(27, 27, 0.d0, matfme_prev)
!
    call matini(9, 27, 0.d0, matrce)
    call matini(9, 27, 0.d0, matrce_prev)
    call matini(9, 27, 0.d0, matrcm)
    call matini(9, 27, 0.d0, matrcm_prev)
    call matini(27, 9, 0.d0, matrec)
    call matini(27, 9, 0.d0, matrec_prev)
    call matini(27, 9, 0.d0, matrmc)
    call matini(27, 9, 0.d0, matrmc_prev)
    call matini(18, 18, 0.d0, matrff)
    call matini(18, 18, 0.d0, matrff_prev)
    call matini(18, 27, 0.d0, matrfe)
    call matini(18, 27, 0.d0, matrfe_prev)
    call matini(18, 27, 0.d0, matrfm)
    call matini(18, 27, 0.d0, matrfm_prev)
    call matini(27, 18, 0.d0, matref)
    call matini(27, 18, 0.d0, matref_prev)
    call matini(27, 18, 0.d0, matrmf)
    call matini(27, 18, 0.d0, matrmf_prev)
!
    debug = .false.
!
! --- TYPE DE MAILLE DE CONTACT
!
    typelt          = 'POIN_ELEM'
    loptf           = option.eq.'RIGI_FROT'
    call jevech('PCONFR', 'L', jpcf)
    l_previous_cont = (nint(zr(jpcf-1+30)) .eq. 1 )
    l_previous_frot = (nint(zr(jpcf-1+44)) .eq. 1 ) .and. .false.
    if (option .eq. 'RIGI_CONT') l_previous = l_previous_cont
    if (option .eq. 'RIGI_FROT') l_previous = l_previous_frot
!---------------------------------------------------------------
!------------- PREPARATION DES CALCULS -------------------------
!---------------------------------------------------------------

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
    
                
    call mmmlav(ldyna,  jeusup, ndexfr)
                
    if (l_previous) then
        call mmmlcf(coefff, coefac_prev, coefaf_prev, lpenac_prev, lpenaf_prev,&
                    iresof_prev, iresog_prev, lambds_prev, l_previous)
        call mmmlav(ldyna,  jeusup_prev, ndexfr_prev)
        
!        debug = .true.
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
! --- PREPARATION DES DONNEES - Quantités géométriques et mécaniques élémentaires
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
                    mprt22, .false._1)
                    
        if (l_previous) then
            call mmtppe(typmae, typmam, ndim, nne, nnm,&
                        nnl, nbdm, iresog_prev, laxis, ldyna,&
                        jeusup_prev, ffe, ffm, dffm, ffl,&
                        jacobi, wpg, jeu_prev, djeut_prev, dlagrc_prev,&
                        dlagrf_prev, norm_prev, tau1_prev, tau2_prev, mprojn_prev,&
                        mprojt_prev, mprt1n_prev, mprt2n_prev, gene11_prev, gene21_prev,&
                        gene22_prev, kappa_prev, h_prev, vech1_prev, vech2_prev,&
                        a_prev, ha_prev, hah_prev, mprt11_prev, mprt21_prev,&
                        mprt22_prev, .true._1)  
                              
!            debug = .false.
!            if (debug) then 
            
!                write (6,*) "MMTPPE : DEBUGGING PREVIOUS AND CURRENT"
!                write (6,*) "iresog_prev",iresog_prev, "iresog",iresog
!                write (6,*) "jeu_prev",jeu_prev,"jeu",jeu
!                write (6,*) "djeut_prev",djeut_prev,"djeut",djeut
!                write (6,*) "dlagrc_prev",dlagrc,"dlagrc",dlagrc
                
!                write (6,*) "dlagrf_prev",dlagrf_prev,"dlagrf",dlagrf
!                write (6,*) "norm_prev",norm_prev,"norm",norm
!                write (6,*) "tau1_prev",tau1_prev,"tau1",tau1
!                write (6,*) "tau2_prev",tau2_prev,"tau2",tau2
                
!                write (6,*) "mprojn_prev",mprojn_prev,"mprojn",mprojn
!                write (6,*) "mprojt_prev",mprojt_prev,"mprojt",mprojt
!                write (6,*) "mprt1n_prev",mprt1n_prev,"mprt1n",mprt1n
                
!                write (6,*) "mprt11_prev",mprt11_prev,"mprt11",mprt11
!                write (6,*) "mprt21_prev",mprt21_prev,"mprt21",mprt21
!                write (6,*) "mprt22_prev",mprt22_prev,"mprt22",mprt22
                
!                write (6,*) "mprt2n_prev",mprt2n_prev,"mprt2n",mprt2n
!                write (6,*) "gene11_prev",gene11_prev,"gene11",gene11
!                write (6,*) "gene21_prev",gene21_prev,"gene21",gene21
!                write (6,*) "gene22_prev",gene22_prev,"gene22",gene22
                
!                write (6,*) "kappa_prev",kappa_prev,"kappa",kappa
!                write (6,*) "h_prev",h_prev,"h",h
!                write (6,*) "ha_prev",ha_prev,"ha",ha
!                write (6,*) "hah_prev",hah_prev,"hah",hah
                
!                write (6,*) "vech1_prev",vech1_prev,"vech1",vech1
!                write (6,*) "vech2_prev",vech2_prev,"vech2",vech2
                
!                write (6,*) "MMTPPE : END DEBUGGING PREVIOUS AND CURRENT"            
!            endif
!            debug = .false.
        endif

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
! --- PREPARATION DES DONNEES - STATUTS + PHASE élémentaires
!
!

!
! ----- Statuts  : current
!                               
        call mmmsta(ndim, leltf, lpenaf, loptf, djeut,&
                    dlagrf, coefaf, tau1, tau2, lcont,&
                    ladhe, lambda, rese, nrese, .false._1)
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


!---------------------------------------------------------------
!------------- FIN PREPARATION DES CALCULS ---------------------
!---------------------------------------------------------------    


!---------------------------------------------------------------
!------------- CALCULS DES FORMES FAIBLES  ---------------------
!---------------------------------------------------------------  
  
!
! --- CALCUL FORME FAIBLE FORCE DE CONTACT/FROTTEMENT
!
    if (typelt .eq. 'POIN_ELEM') then
!
! ----- CONTRIBUTIONS STANDARDS POINT FIXE/NEWTON GENE /NEWTON PARTIEL :
!       CALCUL DE LA FORCE FAIBLE DE CONTACT-FROTTEMENT
!
        call mmtfpe(phasep, iresof, ndim, nne, nnm,&
                    nnl, nbcps, wpg, jacobi, ffl,&
                    ffe, ffm, norm, tau1, tau2,&
                    mprojn, mprojt, rese, nrese, lambda,&
                    coefff, coefaf, coefac, dlagrf, djeut,&
                    matree, matrmm, matrem, matrme, matrec,&
                    matrmc, matref, matrmf)
                    
        if (l_previous) then
            call mmtfpe(phasep_prev, iresof_prev, ndim, nne, nnm,&
                        nnl, nbcps, wpg, jacobi, ffl,&
                        ffe, ffm, norm, tau1_prev, tau2_prev,&
                        mprojn_prev, mprojt_prev, rese_prev, nrese_prev, lambda_prev,&
                        coefff, coefaf_prev, coefac_prev, dlagrf_prev, djeut_prev,&
                        matree_prev, matrmm_prev, matrem_prev, matrme_prev, matrec_prev,&
                        matrmc_prev, matref_prev, matrmf_prev)
        endif
        
!
! ----- CONTRIBUTIONS NON-LINEARITES GEOMETRIQUES NEWTON GENE
!
        if (iresog .eq. 1) then
            call mmtgeo(phasep, ndim, nne, nnm, mprt1n,&
                        mprt2n, mprojn, mprt11, mprt21, mprt22,&
                        wpg, ffe, ffm, dffm, jacobi,&
                        coefac, jeu, dlagrc, kappa, vech1,&
                        vech2, h, hah, matree, matrmm,&
                        matrem, matrme)
                     
            if (l_previous) then
                call mmtgeo(phasep_prev, ndim, nne, nnm, mprt1n_prev,&
                            mprt2n_prev, mprojn_prev, mprt11_prev, mprt21_prev, mprt22_prev,&
                            wpg, ffe, ffm, dffm, jacobi,&
                            coefac_prev, jeu_prev, dlagrc_prev, kappa_prev, vech1_prev,&
                            vech2_prev, h_prev, hah_prev, matree_prev, matrmm_prev,&
                            matrem_prev, matrme_prev)
            
            endif
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
                    
        if (l_previous) then 
            call mmtape(phasep_prev, leltf, ndim, nnl, nne,&
                        nnm, nbcps, wpg, jacobi, ffl,&
                        ffe, ffm, norm, tau1_prev, tau2_prev,&
                        mprojt_prev, rese_prev, nrese_prev, lambda_prev, coefff,&
                        coefaf_prev, coefac_prev, matrcc_prev, matrff_prev, matrce_prev,&
                        matrcm_prev, matrfe_prev, matrfm_prev)        
        endif
    else
        ASSERT(.false.)
    endif
!
! --- MODIFICATIONS EXCLUSION
!
    call mmmtex(ndexfr, ndim, nnl, nne, nnm,&
                nbcps, matrff, matrfe, matrfm, matref,&
                matrmf)
                
    if (l_previous) then
    call mmmtex(ndexfr, ndim, nnl, nne, nnm,&
                nbcps, matrff_prev, matrfe_prev, matrfm_prev, matref_prev,&
                matrmf_prev)
    endif

!---------------------------------------------------------------
!------------- FIN CALCULS DES FORMES FAIBLES  -----------------
!--------------------------------------------------------------- 

!---------------------------------------------------------------
!-------------- ASSEMBLAGE FINAL -------------------------------
!---------------------------------------------------------------
        call mmmtas(nbdm, ndim, nnl, nne, nnm,&
                    nbcps, matrcc, matree, matrmm, matrem,&
                    matrme, matrce, matrcm, matrmc, matrec,&
                    matrff, matrfe, matrfm, matrmf, matref,&
                    mmat)
             
        if (l_previous) then
            call mmmtas(nbdm, ndim, nnl, nne, nnm,&
                        nbcps, matrcc_prev, matree_prev, matrmm_prev, matrem_prev,&
                        matrme_prev, matrce_prev, matrcm_prev, matrmc_prev, matrec_prev,&
                        matrff_prev, matrfe_prev, matrfm_prev, matrmf_prev, matref_prev,&
                        mmat_prev)
        endif 

!---------------------------------------------------------------
!-------------- FIN ASSEMBLAGE FINAL ---------------------------
!---------------------------------------------------------------


!---------------------------------------------------------------
!-------------- RECOPIE DANS LA BASE DE TRAVAIL ----------------
!---------------------------------------------------------------

    alpha_cont = zr(jpcf-1+28)
    alpha_frot = zr(jpcf-1+42)
!    do compte_l = 1, 81
!       if mmat(compte_l,compte_l) .le. 1.d-50&
!       write (6,*) "diagonal nul" , mmat(compte_l,compte_l)
!    enddo
!    if (alpha_cont .lt. 1.0) write (6,*) "alpha_cont",alpha_cont
!    if (alpha_frot .lt. 1.0) write (6,*) "alpha_frot",alpha_frot
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
        do     j = 1, nddl
            do     i = 1, nddl
                ij = j+nddl*(i-1)
                if (lpenac.and.(option.eq.'RIGI_CONT')) then
                    if (l_previous) then 
                        zr(jmatt+ij-1) = alpha_cont * mmat(i,j) &
                                         + (1-alpha_cont) * mmat_prev(i,j)
                    else 
                        zr(jmatt+ij-1) = 1.0 * mmat(i,j)
                    endif
                else if ((option.eq.'RIGI_FROT').and.(iresof.ne.0)) then 
                    if (l_previous) then 
                        zr(jmatt+ij-1) = alpha_frot * mmat(i,j) &
                                         + (1-alpha_frot) * mmat_prev(i,j)
                    else 
                        zr(jmatt+ij-1) = 1.0 * mmat(i,j)                    
                    endif
                    
                else if (lpenaf.and.(option.eq.'RIGI_FROT')) then 
                    if (l_previous) then 
                        zr(jmatt+ij-1) = alpha_frot * mmat(i,j) &
                                         + (1-alpha_frot) * mmat_prev(i,j)
                    else     
                        zr(jmatt+ij-1) = 1.0 * mmat(i,j)        
                    endif
                endif
                if (debug) then
                    call mmmtdb(mmat(i, j), 'IJ', i, j)
                endif
            enddo
        enddo
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
                if (l_previous) then 
                    if ((option.ne.'RIGI_FROT')) then
                        zr(jmatt+ij-1) = alpha_cont *  mmat(i,j) &
                                         + (1-alpha_cont) * mmat_prev(i,j)
                    else
                        zr(jmatt+ij-1) = 1.0 *  mmat(i,j)
                    endif
                else 
                    zr(jmatt+ij-1) = 1.0 *  mmat(i,j)
                endif
                if (debug) then
                    call mmmtdb(mmat(i, j), 'IJ', i, j)
                endif
751         continue
761     continue
!
    endif
!
end subroutine
