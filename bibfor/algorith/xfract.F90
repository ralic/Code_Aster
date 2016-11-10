subroutine xfract(nvec, nnop, nnops, nddls, nddlm,&
                  ndim, pla, deplp, deplm,&
                  ffp, ffc, dffc, saut, gradpf,&
                  q1, q2, dpf, q1m, q2m, sautm,&
                  gradpfm, pf, ffp2, psup, pinf,&
                  job, jmate, meca, hydr, thmc,&
                  t, dimuel, lamb, jheavn, ncompn,&
                  ifiss, nfiss, nfh, ifa, jheafa,&
                  ncomph, contac, depl0, depl1, lambm, pfm)
    implicit none
!
#include "asterfort/assert.h"
#include "jeveux.h"
#include "asterfort/hmdeca.h"
#include "asterfort/thmrcp.h"    
#include "asterfort/vecini.h"
#include "asterfort/xcalc_saut.h"
#include "asterfort/xcalc_code.h"
#include "asterfort/xcalc_heav.h"
!
! ======================================================================
! person_in_charge: daniele.colombo at ifpen.fr
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ======================================================================
!
! ROUTINE MODELE HM-XFEM (CAS DE LA FRACTURE)
!
! CALCUL DE VARIABLES (MECANIQUE ET HYDRAULIQUE)
!
! ----------------------------------------------------------------------
!
    integer, intent(in) :: jmate, dimuel, nfiss, jheavn, ncompn
    integer, intent(in) :: nvec, nnop, nnops, nddls, nddlm, ndim
    integer, intent(in) :: pla(27), ncomph
    integer, intent(in) :: contac
    real(kind=8), intent(in) :: deplp(dimuel), deplm(dimuel)
    real(kind=8), intent(in) :: ffp(27), ffc(16)
    real(kind=8), intent(in) :: dffc(16,3), ffp2(27)
    character(len=8), intent(in) :: job
    character(len=16), intent(in) :: thmc, meca, hydr
    real(kind=8), intent(out) :: q1m, q2m, gradpfm(3), sautm(3)
    real(kind=8), intent(out) :: saut(3), gradpf(3), lamb(3)
    real(kind=8), intent(out) :: q1, q2, dpf, pf, t, pinf, psup
    real(kind=8), optional, intent(in) :: depl0(dimuel), depl1(dimuel)
    real(kind=8), optional, intent(out) :: lambm(3), pfm
    integer :: i, j, in, pli, hea_fa(2), ifiss, nfh, ifa, jheafa
    real(kind=8) :: t0, ffi, dffi(3)
    integer :: rbid54, ibid, heavn(nnop,5), ifh, dec
    real(kind=8) :: rbid1, rbid2, rbid3, rbid4, rbid5, rbid6
    real(kind=8) :: rbid8, rbid9, rbid10, rbid11, rbid12(6), rbid13, rbid14
    real(kind=8) :: rbid15(3), rbid16(3, 3), rbid17, rbid18, rbid19, rbid20
    real(kind=8) :: rbid21, rbid22, rbid23, rbid24, rbid25, rbid26
    real(kind=8) :: rbid27, rbid28, rbid29, rbid30, rbid31, rbid32
    real(kind=8) :: rbid33, rbid34, rbid35, rbid36, rbid37(3, 3)
    real(kind=8) :: rbid39, rbid40, rbid41, rbid42, rbid43, rbid44
    real(kind=8) :: rbid45, rbid46, rbid47, rbid48, rbid49, rbid50
    real(kind=8) :: rbid52, rbid53, rbid38(3, 3), rbid51(3, 3)
    real(kind=8) :: r7bid(3), rbid
    character(len=16) :: zkbid
    real(kind=8) :: coefi
    aster_logical :: lmultc
!
    ASSERT(nvec.gt.0.and.nvec.le.3)
!
! ======================================================================
! --- INITIALISATION DE VARIABLES --------------------------------------
! ======================================================================
!
    call thmrcp('INITIALI', jmate, thmc, meca, hydr,&
                zkbid, t0, rbid, rbid, rbid,&
                rbid, rbid1, rbid2, rbid3, rbid4,&
                rbid5, rbid6, rbid8, rbid9, rbid10,&
                rbid11, rbid12, rbid13, rbid53, rbid14,&
                rbid15, rbid16, rbid17, rbid18, rbid19,&
                rbid20, rbid21, rbid22, rbid23, rbid24,&
                rbid25, rbid26, rbid27, rbid28, rbid29,&
                rbid30, rbid31, rbid32, rbid33, rbid34,&
                rbid35, rbid36, rbid37, rbid38, rbid39,&
                rbid40, rbid41, rbid42, rbid43, rbid44,&
                rbid45, rbid46, rbid47, rbid48, rbid49,&
                rbid50, rbid51, rbid52, ibid,&
                r7bid, rbid54, ibid)
                
    t=t0
!
!   ========================================================================    
!   CALCUL DES GRANDEURS (DEPMOINS + INCREMENT)
!   ========================================================================
!
    call vecini(3, 0.d0, saut)
    call vecini(3, 0.d0, lamb)
    call vecini(3, 0.d0, gradpf)
    call vecini(3, 0.d0, dffi)
    call vecini(3, 0.d0, sautm)
    call vecini(3, 0.d0, gradpfm)
!
    q1m = 0.d0
    q2m = 0.d0 
    q1 = 0.d0
    q2 = 0.d0
    dpf = 0.d0 
    pf = 0.d0 
    pinf = 0.d0
    psup = 0.d0  
!
!     RECUPERATION DE LA DEFINITION DES DDLS HEAVISIDES
    do in = 1, nnop
      do i = 1 , ncompn
        heavn(in,i) = zi(jheavn-1+ncompn*(in-1)+i)
      enddo
    enddo
!
!   CALCUL DU SAUT DE DEPLACEMENT +
!
    lmultc = nfiss.gt.1
    coefi = xcalc_saut(1,0,1)
    hea_fa(1:2)=0
    if (.not.lmultc) then
      hea_fa(1)=xcalc_code(1,he_inte=[-1])
      hea_fa(2)=xcalc_code(1,he_inte=[+1])
    else
      hea_fa(1) = zi(jheafa-1+ncomph*(ifiss-1)+2*(ifa-1)+1)
      hea_fa(2) = zi(jheafa-1+ncomph*(ifiss-1)+2*(ifa-1)+2)
    endif
!
    do i = 1, nnop
       call hmdeca(i, nddls, nddlm, nnops, in, dec)
       do ifh = 1, nfh
          coefi = xcalc_saut(zi(jheavn-1+ncompn*(i-1)+ifh),&
                             hea_fa(1), &
                             hea_fa(2),&
                             zi(jheavn-1+ncompn*(i-1)+ncompn))
          do j = 1, ndim
             saut(j) = saut(j) - coefi*ffp(i)*deplp(in+(ndim+dec)*ifh+j)
             if (nvec.ge.2) then 
                saut(j) = saut(j) - coefi*ffp(i)*deplm(in+(ndim+dec)*ifh+j)
             endif
             if (present(depl0).and.nvec.ge.3) then 
                saut(j) = saut(j) - coefi*ffp(i)*depl0(in+(ndim+dec)*ifh+j)
             endif
          end do
       end do
    end do
!   CALCUL DE PF
!
    do 1 i = 1, nnops
       pli = pla(i)
       ffi = ffc(i)
       pf = pf + ffi*deplp(pli)
       if (nvec.ge.2) then
          pf = pf + ffi*deplm(pli)
       endif
       if (present(depl0).and.nvec.ge.3) then 
          pf = pf + ffi*depl0(pli)
       endif
1   continue
!   CALCUL DU GRADIENT DE PRE_FLU, DES MULTIPLICATEURS DE LAGRANGE
!   ET DE DPRE_FLU (UTILE POUR LE CALCUL DE LA MASSE VOLUMIQUE) EN +
!
    if (job.ne.'SAUT_LOC') then 
       do 2 i = 1, nnops
          pli = pla(i)
          ffi = ffc(i)
          do j = 1, ndim
             dffi(j)=dffc(i,j)
          end do 
          do j = 1, ndim
             gradpf(j) = gradpf(j) + dffi(j)*deplp(pli)
             if (nvec.ge.2) then 
                gradpf(j) = gradpf(j) + dffi(j)*deplm(pli)
             endif
          end do 
! 
          q1 = q1 + ffi*deplp(pli+1)
          q2 = q2 + ffi*deplp(pli+2)
          if (nvec.ge.2) then 
             q1 = q1 + ffi*deplm(pli+1)
             q2 = q2 + ffi*deplm(pli+2)
          endif
! 
          dpf = dpf + ffi*deplp(pli)
          if (job.eq.'ACTU_VI') then
             dpf = dpf - ffi*deplm(pli)
          endif
!
2      continue
    endif
! 
    if (contac.eq.3) then
       do 3 i = 1, nnops
          pli = pla(i)
          ffi = ffc(i)
          do j = 1, ndim
             lamb(j) = lamb(j) + ffi*deplp(pli+2+j)
             if (nvec.ge.2) then
                lamb(j) = lamb(j) + ffi*deplm(pli+2+j)
             endif
             if (present(depl0).and.nvec.ge.3) then 
                lamb(j) = lamb(j) + ffi*depl0(pli+2+j)
             endif
          end do
3      continue
    endif
!
!   CALCUL DE LA PRESSION DANS LE MASSIF POUR LA CONDITION DE CONTINUITE 
!   DE LA PRESSION MASSIF-FRACTURE (SECOND-MEMBRES UNIQUEMENT)
!
    if (job.eq.'VECTEUR') then 
       do i = 1, nnops
          call hmdeca(i, nddls, nddlm, nnops, in, dec)
!
          pinf = pinf + ffp2(i)*deplp(in+ndim+1)
          psup = psup + ffp2(i)*deplp(in+ndim+1)
          if (nvec.ge.2) then
             pinf = pinf + ffp2(i)*deplm(in+ndim+1)
             psup = psup + ffp2(i)*deplm(in+ndim+1)
          endif
!
          do ifh = 1, nfh
             pinf = pinf + xcalc_heav(heavn(i,ifh),hea_fa(1),heavn(i,5))*&
                    deplp(in+(ndim+1)*(ifh+1))*ffp2(i)
             psup = psup + xcalc_heav(heavn(i,ifh),hea_fa(2),heavn(i,5))*&
                    deplp(in+(ndim+1)*(ifh+1))*ffp2(i)
             if (nvec.ge.2) then
                pinf = pinf + xcalc_heav(heavn(i,ifh),hea_fa(1),heavn(i,5))*&
                       deplm(in+(ndim+1)*(ifh+1))*ffp2(i)
                psup = psup + xcalc_heav(heavn(i,ifh),hea_fa(2),heavn(i,5))*&
                       deplm(in+(ndim+1)*(ifh+1))*ffp2(i)
             endif
          end do
       end do
    endif
!   ========================================================================    
!   CALCUL DES GRANDEURS DEPMOINS SEULEMENT
!   ========================================================================
!
!   CALCUL DU SAUT DE DEPLACEMENT ET LAMBDA -
    if (nvec.eq.3) then
       if (present(depl1)) then 
          do i = 1, nnop
             do ifh = 1, nfh
                coefi = xcalc_saut(zi(jheavn-1+ncompn*(i-1)+ifh),&
                                   hea_fa(1), &
                                   hea_fa(2),&
                                   zi(jheavn-1+ncompn*(i-1)+ncompn))
                call hmdeca(i, nddls, nddlm, nnops, in, dec)
                do j = 1, ndim
                   sautm(j) = sautm(j) - coefi*ffp(i)*depl1(in+(ndim+dec)*ifh+j)
                end do
             end do
          end do
!
          call vecini(3, 0.d0, lambm)
          pfm = 0.d0
!
          do 4 i = 1, nnops
             pli = pla(i)
             ffi = ffc(i)
             if (contac.eq.3) then
                do j = 1, ndim
                   lambm(j) = lambm(j) + ffi*depl1(pli+2+j)
                end do
             endif
             pfm = pfm + ffi*depl1(pli)
4         continue
       endif 
    else if (nvec.eq.2 .or. nvec.eq.1) then 
       do i = 1, nnop
          do ifh = 1, nfh
             coefi = xcalc_saut(zi(jheavn-1+ncompn*(i-1)+ifh),&
                                hea_fa(1), &
                                hea_fa(2),&
                                zi(jheavn-1+ncompn*(i-1)+ncompn))
             call hmdeca(i, nddls, nddlm, nnops, in, dec)
             do j = 1, ndim
                sautm(j) = sautm(j) - coefi*ffp(i)*deplm(in+(ndim+dec)*ifh+j)
             end do
          end do
       end do
    endif
!
!   CALCUL DU GRADIENT DE PRE_FLU, DES MULTIPLICATEURS DE LAGRANGE
!   ET DE DPRE_FLU (UTILE POUR LE CALCUL DE LA MASSE VOLUMIQUE) EN -
!
    if (job.ne.'SAUT_LOC') then
       do 5 i = 1, nnops
          pli = pla(i)
          ffi = ffc(i)
          do j = 1, ndim
             dffi(j)=dffc(i,j)
          end do 
          do j = 1, ndim
             gradpfm(j) = gradpfm(j) + dffi(j)*deplm(pli)
          end do 
          q1m = q1m + ffi*deplm(pli+1)
          q2m = q2m + ffi*deplm(pli+2)
5      continue
    endif
!
end subroutine
