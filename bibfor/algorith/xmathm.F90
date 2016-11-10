subroutine xmathm(jmate, thmc, meca, hydr, t, ndim,&
                  nnops, nnop, nddls, nddlm, ffc,&
                  pla, nd, jac, ffp, ffp2, dt, ta, saut,&
                  dffc, rho11, gradpf, mmat,&
                  dsidep, p, r, jheavn, ncompn, ifiss,&
                  nfiss, nfh, ifa, jheafa, ncomph)

    implicit none
    
#include "asterfort/xmmatc.h"
#include "asterfort/xmmatb.h"
#include "asterfort/xmmatu.h"
#include "asterfort/xmmata.h"
#include "asterfort/thmlec.h"  
 
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
! CALCUL DE LA MATRICE MMAT
!
! ----------------------------------------------------------------------
!
    integer :: nnops, nnop, nddls, nddlm, ndim, pla(27)
    integer :: jheavn, ncompn, nfiss, ifiss, nfh, ifa, jheafa, ncomph
    real(kind=8) :: ffc(16), nd(3), jac, ffp(27)
    real(kind=8) :: ffp2(27), mmat(560,560), dt, ta, saut(3)
    real(kind=8) :: dffc(16, 3), unsurk, rho11, viscl
    real(kind=8) :: gradpf(3), dsidep(6,6), p(3,3), r
!
    integer :: ibid, jmate
    real(kind=8) :: rbid1, rbid2, rbid3, rbid4, rbid5, rbid6, rbid7
    real(kind=8) :: rbid9, rbid10, rbid11(3), rbid12(3,3)
    real(kind=8) :: rbid13, rbid14, rbid15, rbid16, rbid17, rbid18
    real(kind=8) :: rbid19, rbid20, rbid21, rbid22, rbid23, rbid24
    real(kind=8) :: rbid25, rbid26, rbid27, rbid28(3,3), rbid29(3,3) 
    real(kind=8) :: rbid30, rbid31, rbid32, rbid33, rbid34, rbid35(3,3)
    real(kind=8) :: rbid37, rbid38(3), t, rbid8(6)
    character(len=16) :: thmc, meca, hydr, zkbid
!
    zkbid = 'VIDE'
!
    call thmlec(jmate, thmc, meca, hydr, zkbid,&
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
    call xmmatu(ndim, nnop, nnops, nddls, nddlm, pla,&
                dsidep, p, r, ffp, jac, ffc, nd, mmat,&
                jheavn, ncompn, ifiss, nfiss, nfh, ifa,&
                jheafa, ncomph)
!
    call xmmatc(ndim, nnops, nddls, nddlm, ffc,&
                pla, jac, ffp2, mmat,&
                jheavn, ncompn, ifiss, nfiss,&
                nfh, ifa, jheafa, ncomph)
!
    call xmmatb(ndim, nnops, nddls, nddlm, ffc,&
                pla, dt, ta, jac, ffp2, mmat,&
                jheavn, ncompn, ifiss, nfiss, nfh,&
                ifa, jheafa, ncomph)
!
    call xmmata(ndim, nnops, nnop, nddls, nddlm, saut,&
                nd, pla, ffc, dffc, mmat, rho11, viscl,&
                gradpf, ffp, dt, ta, jac,&
                unsurk, jheavn, ncompn, ifiss, nfiss,&
                nfh, ifa, jheafa, ncomph)
!
end subroutine
