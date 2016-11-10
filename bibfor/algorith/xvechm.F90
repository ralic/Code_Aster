subroutine xvechm(nnops, ddls, ddlm, ndim, pla,&
                  saut, sautm, nd, ffc, w11, w11m, jac,&
                  q1, dt, ta, q1m, ta1, q2, q2m, dffc,&
                  rho11, gradpf, rho11m, gradpfm, ffp2,&
                  jmate, thmc, meca, hydr, t, vect, ffp,&
                  nnop, delta, lamb, am, r, p, psup,&
                  pinf, pf, ncompn, jheavn, ifiss, nfiss,&
                  nfh, ifa, jheafa, ncomph)

     implicit none
     
#include "asterfort/thmlec.h"
#include "asterfort/xvecha.h"
#include "asterfort/xvechb.h"
#include "asterfort/xvechc.h"
#include "asterfort/xvechu.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!
! ROUTINE MODELE HM-XFEM
! 
! CALCUL DES SECONDS MEMBRES VECT
!
! ----------------------------------------------------------------------

    integer :: nnops, ddls, ddlm, ndim, pla(27), nnop
    integer :: ncompn, jheavn , nfiss, ncomph, ifiss, ifa, jheafa, nfh
    real(kind=8) :: saut(3), sautm(3), nd(3), ffc(16)
    real(kind=8) :: w11, w11m, jac, q1, dt, ta, q1m, ta1
    real(kind=8) :: q2, q2m, dffc(16, 3), rho11, viscl
    real(kind=8) :: gradpf(3), rho11m, gradpfm(3)
    real(kind=8) :: ffp2(27), vect(560), pf
    real(kind=8) :: pinf, psup, ffp(27), delta(6)
    real(kind=8) :: lamb(3), am(3), r, p(3,3)
!
    integer :: ibid, jmate
    real(kind=8) :: rbid1, rbid2, rbid3, rbid4, rbid5, rbid6, rbid7
    real(kind=8) :: rbid9, rbid10, rbid11(3), rbid12(3,3)
    real(kind=8) :: rbid13, rbid14, rbid15, rbid16, rbid17, rbid18
    real(kind=8) :: rbid19, rbid20, rbid21, rbid22, rbid23, rbid24
    real(kind=8) :: rbid25, rbid26, rbid27, rbid28(3,3), rbid29(3,3) 
    real(kind=8) :: rbid30, rbid31, rbid32, rbid33, rbid34, rbid35(3,3)
    real(kind=8) :: rbid37, rbid38(3), t, rbid39, rbid8(6)
    character(len=16) :: thmc, meca, hydr, zkbid
!
    zkbid='VIDE'
!
    call thmlec(jmate, thmc, meca, hydr, zkbid,&
                t, rbid1, rbid2, rbid3, rbid4,&
                rbid5, rbid6, rbid7, rbid8, rbid9,&
                rbid10, rbid11, rbid12, rbid13, rbid14,&
                rbid15, rbid16, rbid17, rbid18, rbid19,&
                rbid20, rbid21, rbid22, rbid39, rbid23,&
                rbid24, rbid25, viscl, rbid26, rbid27,&
                rbid28, rbid29, rbid30, rbid31, rbid32,&
                rbid33, rbid34, rbid35, rbid37,&
                rbid38, ibid, ndim)
!
    call xvecha(ndim, pla, nnops, saut,&
                sautm, nd, ffc, w11, w11m, jac,&
                q1, q1m, q2, q2m, dt, ta, ta1,&
                dffc, rho11, viscl, gradpf, rho11m,&
                gradpfm, vect)
!
    call xvechb(nnops, ddls, ddlm, ndim,&
                ffp2, q1, dt, ta, jac, q1m, ta1,&
                q2, q2m, vect, ncompn, jheavn, ifiss,&
                nfiss, nfh, ifa, jheafa, ncomph)
!
    call xvechc(nnops, pla, ffc, pinf,&
                pf, psup, jac, vect)
!
    call xvechu(ndim, nnop, nnops, ddls, ddlm, pla,&
                lamb, am, delta, r, p, ffp, jac, ffc, vect,&
                ncompn, jheavn, ifiss, nfiss,&
                nfh, ifa, jheafa, ncomph)
!
end subroutine                                      
