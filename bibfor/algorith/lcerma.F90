subroutine lcerma(mat, fami, kpg, ksp, poum)
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
    implicit none
    include 'asterfort/rcvalb.h'
    include 'asterfort/rcvarc.h'
    integer :: mat, kpg, ksp
    character(len=1) :: poum
    character(len=*) :: fami
! ----------------------------------------------------------------------
!   ENDOMMAGEMENT FRAGILE A GRADIENT DE VARIABLE INTERNE ENDO_SCALAIRE
!                      LECTURE DES PARAMETRES
! ----------------------------------------------------------------------
! IN  MAT    ADRESSE DU MATERIAU
! IN  FAMI   FAMILLE DE POINTS D'INTEGRATION (SI 'NONE', PAS DE TEMP.)
! IN  KPG    NUMERO DU POINT D'INTEGRATION
! IN  KSP    NUMERO DU SOUS-POINT
! IN  POUM   LECTURE DES PARAMETRES EN DEBUT '-' OU FIN '+' DU PAS
! ----------------------------------------------------------------------
    integer :: nbel, nber, nbnl
    parameter (nbel=2,nber=6,nbnl=2)
    integer :: iok(nbel+nber+nbnl)
    real(kind=8) :: valel(nbel), valer(nber), valnl(nbnl), alpha, temp, tref
    real(kind=8) :: coef
    real(kind=8) :: e, nu, cc, cv
    character(len=8) :: nomel(nbel), nomer(nber), nomnl(nbnl)
! ----------------------------------------------------------------------
    real(kind=8) :: lambda, deuxmu, troisk, rigmin, pc, pr, epsth
    common /lcee/ lambda,deuxmu,troisk,rigmin,pc,pr,epsth
! ----------------------------------------------------------------------
    real(kind=8) :: pk, pm, pp
    common /lces/ pk,pm,pp
! ----------------------------------------------------------------------
    real(kind=8) :: pct, pch, pcs
    common /lcer/ pch,pct,pcs
! ----------------------------------------------------------------------
    data nomel /'E','NU'/
    data nomer /'K','M','P','C_VOLU','C_COMP','COEF_RIG'/
    data nomnl /'C_GRAD_V','PENA_LAG'/
! ----------------------------------------------------------------------
!
! - LECTURE DES PARAMETRES MECANIQUES
!
    call rcvalb(fami, kpg, ksp, poum, mat,&
                ' ', 'ELAS', 0, ' ', 0.d0,&
                nbel, nomel, valel, iok, 2)
    call rcvalb(fami, kpg, ksp, poum, mat,&
                ' ', 'NON_LOCAL', 0, ' ', 0.d0,&
                nbnl, nomnl, valnl, iok, 2)
    call rcvalb(fami, kpg, ksp, poum, mat,&
                ' ', 'ENDO_SCALAIRE', 0, ' ', 0.d0,&
                nber, nomer, valer, iok, 2)
!
    pc = valnl(1)
    pr = valnl(2)
!
    e = valel(1)
    nu = valel(2)
    lambda = e*nu / (1-2*nu) / (1+nu)
    deuxmu = e / (1+nu)
    troisk = e / (1-2*nu)
    coef = troisk/(2*deuxmu)
!
    pk = valer(1)
    pm = valer(2)
    pp = valer(3)
    cv = valer(4)
    cc = valer(5)
    rigmin = valer(6)
    pcs = 0.5d0*e / ( (1-2*nu)*cc + sqrt(coef*cv* (1-2*nu)**2 + (1+nu)**2) )**2
    pch = cv*coef*pcs
    pct = cc*sqrt(pcs)
!
!
! - LECTURE DES PARAMETRES THERMIQUES
!
    if (fami .eq. 'NONE') then
        epsth = 0.d0
        goto 9999
    endif
!
    call rcvalb(fami, kpg, ksp, poum, mat,&
                ' ', 'ELAS', 0, ' ', 0.d0,&
                1, 'ALPHA', alpha, iok, 0)
!
    if (iok(1) .eq. 0) then
        call rcvarc('F', 'TEMP', 'REF', fami, kpg,&
                    ksp, tref, iok)
        call rcvarc('F', 'TEMP', poum, fami, kpg,&
                    ksp, temp, iok)
        epsth = alpha*(temp-tref)
    else
        epsth = 0
    endif
!
9999  continue
end subroutine
