subroutine vpnor1(norm, neq, nbmode, ddlexc, vecpro,&
                  isign, numddl, coef)
    implicit   none
    include 'asterfort/u2mesg.h'
    integer :: nbmode, neq, ddlexc(*), isign, numddl
    real(kind=8) :: vecpro(neq, *), coef(*)
    character(len=*) :: norm
    character(len=24) :: valk
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     NORMALISATION DE VECTEURS D'UN CONCEPT MODE_FLAMB
!     ------------------------------------------------------------------
! IN  NORM   : TYPE DE NORMALISATION
!          = 'AVEC_CMP'
!          = 'EUCL', 'EUCL_TRAN', ...
! IN  NEQ    : NOMBRE D'EQUATIONS
! IN  NBMODE : NOMBRE DE MODES
! IN  DDLEXC : TABLEAU DES DDL EXCLUS
!              = 0 SI EXCLUS
!              = 1 SI NON EXCLUS
! VAR VECPRO : TABLEAU DES VECTEURS PROPRES
! OUT COEF   : COEFFICIENTS
!     ------------------------------------------------------------------
    integer :: im, ie
    real(kind=8) :: xnorm, xx1
! DEB ------------------------------------------------------------------
!
    if (norm .eq. 'AVEC_CMP' .or. norm(1:4) .eq. 'EUCL') then
!
!     --- NORMALISATION SUR LES DDL NON EXCLUS
!
        do 2 im = 1, nbmode
            xnorm = 0.0d0
            if (norm(1:4) .eq. 'EUCL') then
                do 4 ie = 1, neq
                    xx1 = vecpro(ie,im) * ddlexc(ie)
                    xnorm = xnorm + xx1*xx1
 4              continue
                xnorm = sqrt(xnorm)
            else
                do 6 ie = 1, neq
                    xx1 = vecpro(ie,im)*ddlexc(ie)
                    if (abs(xnorm) .lt. abs(xx1)) then
                        xnorm = xx1
                    endif
 6              continue
            endif
            xx1 = 1.0d0 / xnorm
            coef(im) = xx1
            do 8 ie = 1, neq
                vecpro(ie,im) = vecpro(ie,im) * xx1
 8          continue
 2      continue
!
    else
!
        valk = norm
        call u2mesg('F', 'ALGELINE4_77', 1, valk, 0,&
                    0, 0, 0.d0)
!
    endif
!
    if (isign .eq. 0) then
    else if (isign .eq. 1) then
        do 100 im = 1, nbmode
            xx1 = vecpro(numddl,im)
            if (xx1 .lt. 0.0d0) then
                coef(im) = -coef(im)
                do 102 ie = 1, neq
                    vecpro(ie,im) = -vecpro(ie,im)
102              continue
            endif
100      continue
    else if (isign .eq. -1) then
        do 110 im = 1, nbmode
            xx1 = vecpro(numddl,im)
            if (xx1 .gt. 0.0d0) then
                coef(im) = -coef(im)
                do 112 ie = 1, neq
                    vecpro(ie,im) = -vecpro(ie,im)
112              continue
            endif
110      continue
    endif
!
end subroutine
