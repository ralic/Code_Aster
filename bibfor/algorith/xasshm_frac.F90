subroutine xasshm_frac(nddls, nddlm, nnop, nnops,&
                       lact, elrefp, elrefc, elc, contac,&
                       dimuel, nptf,&
                       jptint, igeom, jbasec,&
                       jcohes, jcoheo,&
                       nlact, cface, rinstp,&
                       rinstm, crit, fpg, ncompv,&
                       compor, jmate, ndim, idepm, idepd,&
                       pla, algocr, rela, ifa, ipgf, matri,&
                       cohes, coheo, jheavn, ncompn, ifiss,&
                       nfiss, nfh, jheafa, ncomph, pos)
    implicit none 
    
#include "jeveux.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/thmlec.h"
#include "asterfort/xfract.h"
#include "asterfort/xjacf2.h"
#include "asterfort/xjacff.h"
#include "asterfort/xhlan5.h"
#include "asterfort/xhmco3.h"
#include "asterfort/xhmco4.h"
#include "asterfort/xhmsa6.h"
#include "asterfort/xmathm.h"
#include "asterfort/xmmata.h"
#include "asterfort/xmmatb.h"
#include "asterfort/xmmatc.h"
#include "asterfort/xmodfc.h"
#include "asterfort/xmofhm.h"
#include "asterfort/xsautl.h"
#include "asterfort/xvinhm.h"
#include "asterfort/matini.h"
#include "asterfort/vecini.h"
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: daniele.colombo at ifpen.fr
! ======================================================================
!
! ROUTINE MODELE HM-XFEM (CAS DE LA FRACTURE)
!
! CALCUL DES MATRICES POUR LE FRACTURE (OPTION RIGI_CONT)
!
! ----------------------------------------------------------------------

    integer :: nddls, nnop, dimuel, i, ndim, nnops, jheavn
    integer :: nddlm, contac, jmate, ncompv, nvec, pla(27), ncompn
    integer :: nptf, nfiss, jcohes, jcoheo
    integer :: ipgf, ifa, cface(30,6), algocr, idepd, idepm, pos(16)
    integer :: jptint, igeom, jbasec, nlact(2), lact(16), ibid, ino
    integer :: ifiss, nfh, jheafa, ncomph
    real(kind=8) :: dt, ta, ta1, cohes(5), rela, g(3), vihydr(64)
    real(kind=8) :: rinstp, rinstm, crit(*), jac, raug
    real(kind=8) :: ffp(27), ffpc(27), ffc(16), dfdic(nnops,3)
    real(kind=8) :: dfbid(27,3), lamb(3), wsaut(3), wsautm(3)
    real(kind=8) :: nd(3), tau1(3), tau2(3)
    real(kind=8) :: matri(560,560), dffc(16,3), rho11, w11
    real(kind=8) :: saut(3), gradpf(3), q1, q2, dpf, rho110
    real(kind=8) :: q1m, q2m, gradpfm(3), sautm(3)
    real(kind=8) :: w11m, rho11m, alpha(5), coheo(5)
    real(kind=8) :: pf, psup, pinf, ffp2(27), t
    real(kind=8) :: dsidep(6, 6), delta(6), p(3,3), sigma(6)
    real(kind=8) :: am(3), ad(3), r, viscl, unsurk
    real(kind=8) :: rbid1, rbid2, rbid3, rbid4, rbid5, rbid6, rbid7
    real(kind=8) :: rbid9, rbid10, rbid11(3), rbid12(3,3)
    real(kind=8) :: rbid13, rbid14, rbid15, rbid16, rbid17, rbid18
    real(kind=8) :: rbid19, rbid20, rbid21, rbid22, rbid23, rbid24
    real(kind=8) :: rbid25, rbid26, rbid27, rbid28(3,3), rbid29(3,3)
    real(kind=8) :: rbid30, rbid31, rbid32, rbid33, rbid34, rbid35(3,3)
    real(kind=8) :: rbid37, rbid38(3), rbid8(6)
    character(len=8) :: elrefp, elrefc, elc, fpg, job, champ
    character(len=16):: compor(*), thmc, hydr, meca, zkbid

!   DETERMINATION DES CONSTANTES TEMPORELLES (INSTANT+THETA SCHEMA)
    dt = rinstp-rinstm
    ta = crit(4)
    ta1 = 1.d0-ta
    
!   RECUPERATION DES DIFFERENTES RELATIONS DE COMPORTEMENT
    thmc = compor( 8)
    hydr = compor(10)
    meca = compor(11)  

!   INITIALISATION DE LA DIMENSION DE LA MATRICE DE TRAVAIL
!
    call matini(nnops, 3, 0.d0, dfdic)
    call matini(16, 3, 0.d0, dffc)
    call vecini(27, 0.d0, ffp)
    call vecini(27, 0.d0, ffpc)
    call vecini(16, 0.d0, ffc)
    call vecini(27, 0.d0, ffp2)
    call vecini(64, 0.d0, vihydr)
!
!         CALCUL DU PRODUIT DU JACOBIEN AVEC LE jac D'INTEGRATION, DES FONCTIONS
!         DE FORME POUR L'ELEMENT PARENT QUADRATIQUE ET DE LA NORMALE A LA
!         FACETTE
    if (ndim.eq.2) then
      call xjacf2(elrefp, elrefc, elc, ndim, fpg,&
                  jptint, ifa, cface, nptf, ipgf,&
                  nnop, nnops, igeom, jbasec, g, jac,&
                  ffp, ffpc, dfbid, nd, tau1, dfdic)
    elseif (ndim.eq.3) then
      call xjacff(elrefp, elrefc, elc, ndim, fpg,&
                  jptint, ifa, cface, ipgf, nnop,&
                  nnops, igeom, jbasec, g, jac, ffp,&
                  ffpc, dfbid, nd, tau1, tau2, dfdic)
    endif
!
    ffp2(1:nnops) = ffpc(1:nnops)
!
!   CALCUL DES FONCTIONS DE FORME DE CONTACT
    call xmofhm(lact, nlact, nnops, ffpc, ffc)
!   CALCUL DU GRADIENT DES FONCTIONS DE FORME DE CONTACT
!          
    call xmodfc(lact, nlact, nnops, dfdic, dffc, ndim)  
!
    if (algocr.eq.3) then
       if ((rela.eq.3.d0).or.(rela.eq.4.d0)) then 
!
           nvec=2
           job='MATRICE'
           call xfract(nvec, nnop, nnops, nddls, nddlm,&
                       ndim, pla, zr(idepd), zr(idepm),&
                       ffp, ffc, dffc, saut, gradpf,&
                       q1, q2, dpf, q1m, q2m, sautm,&
                       gradpfm, pf, ffp2, psup, pinf,&
                       job, zi(jmate), meca, hydr, thmc,&
                       t, dimuel, lamb, jheavn, ncompn, ifiss,&
                       nfiss, nfh, ifa, jheafa, ncomph,&
                       contac)
!
!          CALCUL DU CHANGEMENT DE BASE POUR LE SAUT DE DEPLACEMENT
!
           call xsautl(ndim, nd, tau1, tau2, saut, sautm, p, am, ad)
!
           do i = 1, ndim
              am(i) = -am(i)
           end do
!
!          CALCUL DE LA VARIABLE INTERNE (MASSE VOLUMIQUE DU LIQUIDE 
!          CIRCULANT DANS LA FRACTURE)
!
           job='MATRICE'                    
           call xvinhm(zi(jmate), thmc, meca, hydr, ndim,&
                       cohes, dpf, saut, sautm, nd, lamb,&
                       w11m, rho11m, alpha, job, t, pf,&
                       rho11, w11, ipgf, rela, dsidep,&
                       delta, r, am)
!
!          CALCUL DES MATRICES (CF. DOC R7.02.18)
           call xmathm(zi(jmate), thmc, meca, hydr, t, ndim,&
                       nnops, nnop, nddls, nddlm, ffc,&
                       pla, nd, jac, ffp, ffp2, dt, ta, saut,&
                       dffc, rho11, gradpf, matri,&
                       dsidep, p, r, jheavn, ncompn, ifiss,&
                       nfiss, nfh, ifa, jheafa, ncomph)
!
!          ACTUALISATION INDICATEUR PREDICTION / CORRECTION
!
           coheo(1)=cohes(1)
           coheo(2)=cohes(2)
           coheo(3)=2.d0
           coheo(4)=cohes(4)
           coheo(5)=cohes(5)
!
       else if (rela.eq.5.d0) then
!
! --- CALCUL DES MATRICES "MORTAR" QUI NE DEPENDENT
! --- PAS DE LA LOI DE COMPORTEMENT
!
           call xhmco4(ndim, nnop, nnops, pla, nd, tau1,&
                       tau2, ffc, nddls, jac, ffp,&
                       nddlm, matri, ifiss, nfiss, nfh,&
                       ifa, jheafa, ncomph, jheavn, ncompn)
!
           nvec=2
           job='MATRICE'
           call xfract(nvec, nnop, nnops, nddls, nddlm,&
                       ndim, pla, zr(idepd), zr(idepm),&
                       ffp, ffc, dffc, saut, gradpf,&
                       q1, q2, dpf, q1m, q2m, sautm,&
                       gradpfm, pf, ffp2, psup, pinf,&
                       job, zi(jmate), meca, hydr, thmc,&
                       t, dimuel, lamb, jheavn, ncompn, ifiss,&
                       nfiss, nfh, ifa, jheafa, ncomph, contac)
!
           do ino = 1, nnops
                champ = 'LAMBDA'
                call xhlan5(ino, idepd, idepm, ibid, lact, ndim,&
                            pla, lamb, nvec, champ, job, dpf)
!
                champ = 'W'
                call xhlan5(ino, idepd, idepm, ibid, lact, ndim,&
                            pla, wsaut, nvec, champ, job, dpf)
!
                champ = 'WM'
                call xhlan5(ino, idepd, idepm, ibid, lact, ndim,&
                            pla, wsautm, nvec, champ, job, dpf)
!
                do i = 1, ncompv
                    cohes(i) = zr(jcohes+ncompv*nnops*(pos(ino)-1)+ncompv*(ino-1)-1+i)
                end do
!
                call xhmsa6(ndim, ipgf, zi(jmate), lamb, wsaut, nd,&
                            tau1, tau2, cohes, job, rela,&
                            alpha, dsidep, sigma, p, am, raug,&
                            thmc, meca, hydr, wsautm, dpf, rho110)
                call xhmco3(ino, ndim, dsidep, pla, p,&
                            ffc, jac, raug, matri)
!
! --- ACTUALISATION INDICATEUR PREDICTION / CORRECTION
!
                coheo(1)=cohes(1)
                coheo(2)=cohes(2)
                coheo(3)=2.d0
                coheo(4)=cohes(4)
                coheo(5)=cohes(5)
!
                do i = 1, ncompv
                    zr(jcoheo+ncompv*nnops*(pos(ino)-1)+ncompv*(ino-1)-1+i) = coheo(i)
                end do
                vihydr(2*nnops*(pos(ino)-1)+2*(ino-1)+1) = alpha(4)
                vihydr(2*nnops*(pos(ino)-1)+2*(ino-1)+2) = alpha(5)
           end do
!
! --- INTERPOLATION DE RHO11 AU POINT DE GAUSS
!
           rho11 = rho110
           do ino = 1, nnops
              rho11 = rho11 + ffc(ino)*vihydr(2*nnops*(pos(ino)-1)+2*(ino-1)+1)
           end do
!
! --- CALCUL DES MATRICES HYDROS
!
           zkbid = 'VIDE'
!
           call thmlec(zi(jmate), thmc, meca, hydr, zkbid,&
                       t, rbid1, rbid2, rbid3, rbid4,&
                       rbid5, rbid6, rbid7, rbid8, rbid9,&
                       rbid10, rbid11, rbid12, rbid13, rbid14,&
                       rbid15, rbid16, rbid17, rbid18, rbid19,&
                       rbid20, rbid21, rbid22, unsurk, rbid23,&
                       rbid24, rbid25, viscl, rbid26, rbid27,&
                       rbid28, rbid29, rbid30, rbid31, rbid32,&
                       rbid33, rbid34, rbid35, rbid37,&
                       rbid38, ibid, ndim)
!
           call xmmatc(ndim, nnops, nddls, nddlm, ffc,&
                       pla, jac, ffp2, matri,&
                       jheavn, ncompn, ifiss, nfiss,&
                       nfh, ifa, jheafa, ncomph)
!
           call xmmatb(ndim, nnops, nddls, nddlm, ffc,&
                       pla, dt, ta, jac, ffp2, matri,&
                       jheavn, ncompn, ifiss, nfiss, nfh,&
                       ifa, jheafa, ncomph)
!
           call xmmata(ndim, nnops, nnop, nddls, nddlm, saut,&
                       nd, pla, ffc, dffc, matri, rho11, viscl,&
                       gradpf, ffp, dt, ta, jac,&
                       unsurk, jheavn, ncompn, ifiss, nfiss,&
                       nfh, ifa, jheafa, ncomph)
!
       endif
    endif
!
end subroutine 
