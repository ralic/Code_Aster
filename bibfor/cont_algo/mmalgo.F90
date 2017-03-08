subroutine mmalgo(ds_contact, l_loop_cont, l_frot_zone, l_vite,&
                  l_glis_init, type_adap, zone_index, i_cont_poin, indi_cont_init,&
                  indi_cont_eval, indi_frot_eval, dist_cont_curr, vite_cont_curr, &
                  pres_cont_curr, dist_frot_curr, pres_frot_curr, v_sdcont_cychis,&
                  v_sdcont_cyccoe, v_sdcont_cyceta, indi_cont_curr,indi_frot_curr,&
                  ctcsta, mmcvca, scotch)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterc/r8prem.h"
#include "asterfort/mmstac.h"
#include "asterfort/mm_cycl_detect.h"
#include "asterfort/mm_cycl_trait.h"
#include "asterfort/cfdisi.h"
#include "asterfort/search_optimal_coefficient.h"
#include "asterfort/bussetta_algorithm.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: ayaovi-dzifa.kudawoo at edf.fr
! aslint: disable=W1504
!
    type(NL_DS_Contact), intent(in) :: ds_contact
    aster_logical, intent(in) :: l_loop_cont
    aster_logical, intent(in) :: l_frot_zone
    aster_logical, intent(in) :: l_vite
    aster_logical, intent(in) :: l_glis_init
    integer, intent(in) :: type_adap
    integer, intent(in) :: i_cont_poin
    integer, intent(in) :: zone_index
    integer, intent(inout) :: indi_cont_init
    integer, intent(inout) :: indi_cont_eval
    integer, intent(inout) :: indi_frot_eval
    real(kind=8), intent(inout) :: dist_cont_curr
    real(kind=8), intent(inout) :: vite_cont_curr
    real(kind=8), intent(inout) :: pres_cont_curr
    real(kind=8), intent(inout) :: dist_frot_curr(3)
    real(kind=8), intent(in) :: pres_frot_curr(3)
    real(kind=8), pointer, intent(in) :: v_sdcont_cychis(:)
    real(kind=8), pointer, intent(in) :: v_sdcont_cyccoe(:)
    integer, pointer, intent(in) :: v_sdcont_cyceta(:)
    integer, intent(out) :: indi_cont_curr
    integer, intent(out) :: indi_frot_curr
    integer, intent(out) :: ctcsta
    aster_logical, intent(out) :: mmcvca
    aster_logical, intent(out) :: scotch
!
! --------------------------------------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - CONTRAINTES ACTIVES)
!
! TRAITEMENT DES DIFFERENTS CAS
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_contact       : datastructure for contact management
! In  l_frot_zone      : .true. if friction on zone
! In  l_loop_cont      : .true. if fixed poitn on contact loop
! In  l_coef_adap      : .true. if automatic lagrangian adaptation
! In  l_vite           : .true. if velocity scheme (dynamic)
! In  l_glis_init      : .true. if bilateral contact for first step
! In  i_cont_poin      : contact point index
! In  indi_cont_init   : previous contact status (but not for cycling)
! In  indi_cont_eval   : evaluation of new contact status
! In  indi_frot_eval   : evaluation of new friction status
! In  dist_cont_curr   : current contact gap
! In  vite_cont_curr   : current contact velocity gap
! In  pres_cont_curr   : current contact pressure
! In  dist_frot_curr   : current friction distance
! In  pres_frot_curr   : current friction pressure
! In  v_sdcont_cychis  : pointer to cycling history
! In  v_sdcont_cyccoe  : pointer to coefficient history
! Out indi_cont_curr   : current contact status
! Out indi_frot_curr   : current friction status
! Out mmcvca           : .true. if contact loop converged
! Out ctcsta           : number of contact points has changed their status
! Out scotch           : .true. if contact point glued
!
! --------------------------------------------------------------------------------------------------
!
    integer :: hist_index = 0 
    real(kind=8) :: coef_cont_prev = 0.0, coef_frot_prev=0.0
    real(kind=8) :: coef_cont_curr=0.0, coef_frot_curr=0.0
    real(kind=8) ::  coefficient=0.0
    aster_logical:: coef_found=.false._1,treatment =.true._1
    aster_logical:: l_coef_adap = .false.
    integer      ::  mode_cycl = 0
    real(kind=8) :: pres_frot_prev(3)=0.0, pres_cont_prev=0.0
    real(kind=8) :: dist_frot_prev(3)=0.0, dist_cont_prev=0.0
    integer :: indi_cont_prev=0, indi_frot_prev=0,indi(2)=0,i_reso_cont=0
    real(kind=8) :: coef_frot_mini=0.0, coef_frot_maxi=0.0
    real(kind=8) :: alpha_cont_matr=0.0, alpha_cont_vect=0.0
    real(kind=8) :: alpha_frot_matr=0.0, alpha_frot_vect=0.0
    real(kind=8) :: coef_opt=0.0,pres_cont(2)=0.0, dist_cont(2)=0.0
    real(kind=8) :: coef_bussetta=0.0, dist_max
!
! --------------------------------------------------------------------------------------------------
!
!
! - Initializations
!
    scotch = .false.
    
    l_coef_adap = ((type_adap .eq. 1) .or. (type_adap .eq. 2) .or. &
                  (type_adap .eq. 5) .or. (type_adap .eq. 6 ))
    
    treatment =  ((type_adap .eq. 4) .or. (type_adap .eq. 5) .or. &
                  (type_adap .eq. 6) .or. (type_adap .eq. 7 ))
!
! - Velocity theta-scheme (dynamic)
!
    if (indi_cont_init .eq. 1) then
        scotch = .true.
    else
        scotch = .false.
    endif
    i_reso_cont  = cfdisi(ds_contact%sdcont_defi,'ALGO_RESO_CONT')
!
! - Save old history
!
    if (nint(v_sdcont_cychis(60*(i_cont_poin-1)+24)) .ne. &
        nint(v_sdcont_cychis(60*(i_cont_poin-1)+24+24))) then
!        write (6,*) "la maille maitre a changé : on e fait rien "
        treatment =.false.
    endif
    do hist_index = 1, 24
        v_sdcont_cychis(60*(i_cont_poin-1)+24+hist_index) = &
            v_sdcont_cychis(60*(i_cont_poin-1)+hist_index)
    enddo
!
! - Previous informations
!
    indi_cont_prev = nint(v_sdcont_cychis(60*(i_cont_poin-1)+24+1))
    coef_cont_prev = v_sdcont_cychis(60*(i_cont_poin-1)+24+2)
    pres_cont_prev = v_sdcont_cychis(60*(i_cont_poin-1)+24+3)
    dist_cont_prev = v_sdcont_cychis(60*(i_cont_poin-1)+24+4)
! XXX next value seems uniniatiliased in ssnp121i
    indi_frot_prev = nint(v_sdcont_cychis(60*(i_cont_poin-1)+24+5))
    coef_frot_prev = v_sdcont_cychis(60*(i_cont_poin-1)+24+6)
    pres_frot_prev(1) = v_sdcont_cychis(60*(i_cont_poin-1)+24+7)
    pres_frot_prev(2) = v_sdcont_cychis(60*(i_cont_poin-1)+24+8)
    pres_frot_prev(3) = v_sdcont_cychis(60*(i_cont_poin-1)+24+9)
    dist_frot_prev(1) = v_sdcont_cychis(60*(i_cont_poin-1)+24+10)
    dist_frot_prev(2) = v_sdcont_cychis(60*(i_cont_poin-1)+24+11)
    dist_frot_prev(3) = v_sdcont_cychis(60*(i_cont_poin-1)+24+12)
!
! - Current max/min ratio
!
    coef_frot_mini = v_sdcont_cyccoe(6*(zone_index-1)+5)
    coef_frot_maxi = v_sdcont_cyccoe(6*(zone_index-1)+6)
!
! - Cycling detection
!
    call mm_cycl_detect(ds_contact, l_loop_cont, l_frot_zone, i_cont_poin,&
                        coef_cont_prev,coef_frot_prev, pres_cont_prev,&
                        dist_cont_prev, pres_frot_curr,pres_frot_prev ,& 
                        indi_frot_prev, dist_frot_prev, indi_cont_eval,&
                        indi_frot_eval, dist_cont_curr, pres_cont_curr,&
                        dist_frot_curr,alpha_cont_matr, alpha_cont_vect,&
                        alpha_frot_matr, alpha_frot_vect)
    
!
! - Cycling treatment: automatic adaptation of augmented lagrangian ratio
!
    if (l_coef_adap) then
        call mm_cycl_trait(ds_contact, i_cont_poin, coef_cont_prev, coef_frot_prev,&
                           pres_frot_prev, dist_frot_prev, pres_frot_curr, dist_frot_curr,&
                           indi_cont_eval, indi_frot_eval, indi_cont_curr, coef_cont_curr,&
                           indi_frot_curr, coef_frot_curr)
    else
        coef_cont_curr = coef_cont_prev
        coef_frot_curr = coef_frot_prev
        indi_cont_curr = indi_cont_eval
        indi_frot_curr = indi_frot_eval
    endif
!
! - Saving max/min ratio
!
    if (coef_frot_curr .ge. coef_frot_maxi) coef_frot_maxi = coef_frot_curr
    if (coef_frot_curr .le. coef_frot_mini) coef_frot_mini = coef_frot_curr
    v_sdcont_cyccoe(6*(zone_index-1)+5) = coef_frot_mini
    v_sdcont_cyccoe(6*(zone_index-1)+6) = coef_frot_maxi
!
! - Special treatment if velocity scheme
!
    if (l_vite) then
        if ((indi_cont_eval.eq.0) .and. (vite_cont_curr.le.0.d0)) then
            indi_cont_curr = 0
        endif
    endif
!
! - Special treatment if bilateral contact : every point is in contact
!
    if (l_glis_init) indi_cont_curr = 1
!
! - Save history for automatic cycling algorithm
!
    v_sdcont_cychis(60*(i_cont_poin-1)+1) = indi_cont_curr
    v_sdcont_cychis(60*(i_cont_poin-1)+2) = coef_cont_curr
    v_sdcont_cychis(60*(i_cont_poin-1)+3) = pres_cont_curr
    v_sdcont_cychis(60*(i_cont_poin-1)+4) = dist_cont_curr
    v_sdcont_cychis(60*(i_cont_poin-1)+5) = indi_frot_curr
    v_sdcont_cychis(60*(i_cont_poin-1)+6) = coef_frot_curr
    v_sdcont_cychis(60*(i_cont_poin-1)+7) = pres_frot_curr(1)
    v_sdcont_cychis(60*(i_cont_poin-1)+8) = pres_frot_curr(2)
    v_sdcont_cychis(60*(i_cont_poin-1)+9) = pres_frot_curr(3)
    v_sdcont_cychis(60*(i_cont_poin-1)+10) = dist_frot_curr(1)
    v_sdcont_cychis(60*(i_cont_poin-1)+11) = dist_frot_curr(2)
    v_sdcont_cychis(60*(i_cont_poin-1)+12) = dist_frot_curr(3)
!    v_sdcont_cychis(60*(i_cont_poin-1)+58) = dist_cont_prev                     
    if ((ds_contact%iteration_newton .ge. 3 ) .and. &
        (v_sdcont_cyceta(4*(i_cont_poin-1)+1) .gt. 0 .and. treatment )) then
       
       
!ADAPTATION DE MATRICES, VECTEURS ET COEFF POUR LES TE :
! MATR_PREVIOUS + MATR_CURRENT       
       v_sdcont_cychis(60*(i_cont_poin-1)+57) = 1.0d0
       v_sdcont_cychis(60*(i_cont_poin-1)+59) = alpha_cont_matr
       v_sdcont_cychis(60*(i_cont_poin-1)+56) = alpha_cont_vect
!       coefficient = v_sdcont_cychis(60*(i_cont_poin-1)+2) /1.d4
       coef_found = .false.
       indi(1) = indi_cont_curr
       indi(2) = indi_cont_prev
       pres_cont(1) = pres_cont_curr
       pres_cont(2) = pres_cont_prev
       dist_cont(1) = dist_cont_curr
       dist_cont(2) = dist_cont_prev
       
       call search_optimal_coefficient([1.d-6, 1.d7], &
                                       indi, pres_cont, dist_cont, &
                                       coef_opt,coef_found)
!      write (6,*) "coefficient found" , coef_found                                
       if (coef_found) then
           if (i_reso_cont .ne. 0) then
               indi_cont_curr =  indi(1)
               indi_cont_prev =  indi(2)  
               v_sdcont_cychis(60*(i_cont_poin-1)+1)    = indi_cont_curr
               v_sdcont_cychis(60*(i_cont_poin-1)+24+1) = indi_cont_prev
           endif
           dist_cont_curr =  dist_cont(1)
           dist_cont_prev =  dist_cont(2)
           v_sdcont_cychis(60*(i_cont_poin-1)+2)    = coef_opt
           v_sdcont_cychis(60*(i_cont_poin-1)+24+2) = coef_opt       
           v_sdcont_cychis(60*(i_cont_poin-1)+3)    = pres_cont_curr
           v_sdcont_cychis(60*(i_cont_poin-1)+24+3) = pres_cont_prev
           v_sdcont_cychis(60*(i_cont_poin-1)+4)    = dist_cont_curr
           v_sdcont_cychis(60*(i_cont_poin-1)+24+4) = dist_cont_prev          
!           if (indi_cont_curr .ne. indi_cont_prev) write (6,*) "Traitement NOOK"
       endif
         
    endif

! WARNING CYCLAGE FROTTEMENT    : ADHE_GLIS
                
    if ((ds_contact%iteration_newton .ge. 3 ) .and. &
       (v_sdcont_cyceta(4*(i_cont_poin-1)+2) .ge. 10 ) .and. treatment   ) then   
        
           if (v_sdcont_cyceta(4*(i_cont_poin-1)+1) .eq. 11) then
               v_sdcont_cychis(60*(i_cont_poin-1)+50) = 0.0d0
           else
               v_sdcont_cychis(60*(i_cont_poin-1)+50) = 1.0d0
           endif
         
           if (nint(v_sdcont_cychis(60*(i_cont_poin-1)+50)) .eq. 1)  then
       
               if (  v_sdcont_cyceta(4*(i_cont_poin-1)+2) .eq. 11   ) then  
                  indi_frot_curr = 1
                  v_sdcont_cychis(60*(i_cont_poin-1)+5) = indi_frot_curr
                  alpha_frot_matr = 1.0
                  v_sdcont_cychis(60*(i_cont_poin-1)+54) = alpha_frot_matr         
                  alpha_frot_vect = 1.0
                  v_sdcont_cychis(60*(i_cont_poin-1)+55) = alpha_frot_vect
                       
               elseif (  v_sdcont_cyceta(4*(i_cont_poin-1)+2) .eq. 12   ) then  
                  alpha_frot_matr = 1.0
                  v_sdcont_cychis(60*(i_cont_poin-1)+54) = alpha_frot_matr         
                  alpha_frot_vect = 1.0
                  v_sdcont_cychis(60*(i_cont_poin-1)+55) = alpha_frot_vect
                       
               elseif (  v_sdcont_cyceta(4*(i_cont_poin-1)+2) .eq. 13   ) then  
                  alpha_frot_matr = 1.0
                  v_sdcont_cychis(60*(i_cont_poin-1)+54) = alpha_frot_matr         
                  alpha_frot_vect = 1.0
                  v_sdcont_cychis(60*(i_cont_poin-1)+55) = alpha_frot_vect
                       
               elseif (  v_sdcont_cyceta(4*(i_cont_poin-1)+2) .eq. 14   ) then  
                  alpha_frot_matr = 0.5
                  v_sdcont_cychis(60*(i_cont_poin-1)+54) = alpha_frot_matr         
                  alpha_frot_vect = 0.5
                  v_sdcont_cychis(60*(i_cont_poin-1)+55) = alpha_frot_vect
               endif
           endif    
         
       else 
           v_sdcont_cychis(60*(i_cont_poin-1)+50) = 0.0d0
           v_sdcont_cychis(60*(i_cont_poin-1)+54) = 1.0
           v_sdcont_cychis(60*(i_cont_poin-1)+55) = 1.0
       endif
    
                    
    if ((ds_contact%iteration_newton .ge. 3 ) .and. &
       (v_sdcont_cyceta(4*(i_cont_poin-1)+3) .ge. 10 )  .and. treatment  ) then   
        
         if (v_sdcont_cyceta(4*(i_cont_poin-1)+1) .eq. 11) then
             v_sdcont_cychis(60*(i_cont_poin-1)+50) = 0.0d0
         else
             v_sdcont_cychis(60*(i_cont_poin-1)+50) = 1.0d0
         endif
         
         if     (nint(v_sdcont_cychis(60*(i_cont_poin-1)+50)) .eq. 1)  then
             if (  v_sdcont_cyceta(4*(i_cont_poin-1)+3) .eq. 11   ) then  
           
                alpha_frot_matr = 0.5
                v_sdcont_cychis(60*(i_cont_poin-1)+54) = alpha_frot_matr            
                alpha_frot_vect = 1.0
                v_sdcont_cychis(60*(i_cont_poin-1)+55) = alpha_frot_vect
                      
             elseif (  v_sdcont_cyceta(4*(i_cont_poin-1)+3) .eq. 12   ) then  
           
                alpha_frot_matr = 0.5
                v_sdcont_cychis(60*(i_cont_poin-1)+54) = alpha_frot_matr            
                alpha_frot_vect = 1.0
                v_sdcont_cychis(60*(i_cont_poin-1)+55) = alpha_frot_vect
                      
                      
             elseif (  v_sdcont_cyceta(4*(i_cont_poin-1)+3) .eq. 13   ) then  
           
                alpha_frot_matr = 0.5
                v_sdcont_cychis(60*(i_cont_poin-1)+54) = alpha_frot_matr            
                alpha_frot_vect = 1.0
                v_sdcont_cychis(60*(i_cont_poin-1)+55) = alpha_frot_vect
                
             endif
         endif    
         
       else 
          v_sdcont_cychis(60*(i_cont_poin-1)+50) = 0.0d0
          v_sdcont_cychis(60*(i_cont_poin-1)+54) = 1.0
          v_sdcont_cychis(60*(i_cont_poin-1)+55) = 1.0
       endif

! WARNING CYCLAGE FROTTEMENT: END
! - Convergence ?
!
!    mmcvca =  indi_cont_init .eq. indi_cont_curr
!    if (nint(v_sdcont_cychis(60*(i_cont_poin-1) + 57)) .eq. 1 ) &
    mmcvca =  indi_cont_prev .eq. indi_cont_curr
    if (.not. mmcvca .and. treatment) then
        mode_cycl = 1
        if (mode_cycl .eq. 1 .and. &
            ds_contact%iteration_newton .gt. ds_contact%it_cycl_maxi+3 ) then 
             if (dist_cont_curr .gt. 1.d-6 )  dist_cont_curr = 0.0
             if (pres_cont_curr .gt. 1.d-6 )  pres_cont_curr = -1.d-15
             if (dist_cont_prev .gt. 1.d-6 )  dist_cont_prev = 0.0
             if (pres_cont_prev .gt. 1.d-6 )  pres_cont_prev = -1.d-15
             if (i_reso_cont .ne. 0) then
                 call mmstac(dist_cont_curr, pres_cont_curr,coefficient,indi_cont_curr)
                 call mmstac(dist_cont_prev, pres_cont_prev,coefficient,indi_cont_prev)
                 v_sdcont_cychis(60*(i_cont_poin-1)+1)    = indi_cont_curr
                 v_sdcont_cychis(60*(i_cont_poin-1)+24+1) = indi_cont_prev
             endif       
             v_sdcont_cychis(60*(i_cont_poin-1)+3)    = pres_cont_curr
             v_sdcont_cychis(60*(i_cont_poin-1)+24+3) = pres_cont_prev
             v_sdcont_cychis(60*(i_cont_poin-1)+4)    = dist_cont_curr
             v_sdcont_cychis(60*(i_cont_poin-1)+24+4) = dist_cont_prev      
             v_sdcont_cychis(60*(i_cont_poin-1)+57) = 0.999
             v_sdcont_cychis(60*(i_cont_poin-1)+59) = 0.7
             v_sdcont_cychis(60*(i_cont_poin-1)+56) = 1.0
             v_sdcont_cychis(60*(i_cont_poin-1)+51) = 4.0
             v_sdcont_cychis(60*(i_cont_poin-1)+52) = 4.0
             v_sdcont_cyceta(4*(i_cont_poin-1)+1)   = 10
             v_sdcont_cychis(60*(i_cont_poin-1)+2)    = 1.d2
             v_sdcont_cychis(60*(i_cont_poin-1)+24+2) = 1.d2
             mmcvca =  indi_cont_prev .eq. indi_cont_curr
        endif
!        ctcsta  = ctcsta + 1
    endif
    if (.not. mmcvca ) ctcsta = ctcsta+1 
    mmcvca = mmcvca .and. (ctcsta .eq. 0) 
!
!  Algorithm of Bussetta
!  
    if ((type_adap .eq. 2) .or. (type_adap .eq. 3) .or. &
        (type_adap .eq. 6) .or. (type_adap .eq. 7)) then
        
        coef_bussetta = v_sdcont_cychis(60*(i_cont_poin-1)+2)
        dist_max      = 0.001*ds_contact%arete_min
!        write (6,*) "ADAPTATION DU COEFFICIENT DE PENALISATION : Avant", coef_bussetta
        call bussetta_algorithm(dist_cont_curr, dist_cont_prev,dist_max, coef_bussetta)
        v_sdcont_cychis(60*(i_cont_poin-1)+2) = coef_bussetta
!        write (6,*) "ADAPTATION DU COEFFICIENT DE PENALISATION : Apres", coef_bussetta
        
    endif
end subroutine
