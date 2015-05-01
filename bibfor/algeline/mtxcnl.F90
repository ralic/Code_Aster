subroutine mtxcnl(cumul, typcst, const, typmat, lmat,&
                  typres, lres, neq)
    implicit none
#include "jeveux.h"
#include "asterfort/utmess.h"
    integer :: lmat, lres
    character(len=*) :: cumul, typcst
    character(len=1) :: typmat, typres
    real(kind=8) :: const(2)
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     MIXAGE DES .CONL    ---> LIRE AVERTISSEMENT CI DESSOUS
!     ------------------------------------------------------------------
!     CECI EST UNE ROUTINE INTERNE VOUS N'AVEZ PAS LE DROIT DE L'APPELER
!     DIRECTEMENT, NI MEME DE CRITIQUER.
!     NEANMOINS SI VOUS VOULEZ LA REECRIRE A VOTRE AISE.
!     ------------------------------------------------------------------
!
    character(len=24) :: valk(3)
!
    real(kind=8) :: un, zero, rcum
    complex(kind=8) :: cun, c8cst
!
!-----------------------------------------------------------------------
    integer :: ival, neq
!-----------------------------------------------------------------------
    zero= 0.d0
    un = 1.d0
    cun = dcmplx(un,0.d0)
!
    if (cumul .eq. 'CUMU') then
        rcum = un
    else
        rcum = zero
        if (typres .eq. 'R') then
            do 10 ival = 0, neq-1
                zr(lres+ival) = un
10          continue
        else if (typres .eq. 'C') then
            do 20 ival = 0, neq-1
                zc(lres+ival) = cun
20          continue
        endif
    endif
!
! --- MATRICE REELLE EN RESULTAT
!
    if (typres .eq. 'R') then
        if (typmat .eq. 'R') then
            if (typcst(1:1) .eq. 'R') then
                do 100 ival = 0, neq-1
                    if (zr(lmat+ival) .ne. un) then
                        zr(lres+ival) = rcum*zr(lres+ival) + const(1)* zr(lmat+ival)
                    endif
100              continue
            else
                valk (1) = typres
                valk (2) = typmat
                valk (3) = typcst(1:1)
                call utmess('F', 'ALGELINE4_25', nk=3, valk=valk)
            endif
        else if (typmat .eq. 'C') then
            if (typcst(1:1) .eq. 'R') then
                do 110 ival = 0, neq-1
                    if (zc(lmat+ival) .ne. cun) then
                        zr(lres+ival) = rcum*zr(lres+ival) + const(1)* dble(zc(lmat+ival))
                    endif
110              continue
            else if (typcst(1:1) .eq. 'C') then
                c8cst = dcmplx(const(1),const(2))
                do 120 ival = 0, neq-1
                    if (zc(lmat+ival) .ne. cun) then
                        zr(lres+ival) = rcum*zr(lres+ival) + dble( c8cst*zc(lmat+ival))
                    endif
120              continue
            else
                valk (1) = typres
                valk (2) = typmat
                valk (3) = typcst(1:1)
                call utmess('F', 'ALGELINE4_25', nk=3, valk=valk)
            endif
        else
            valk (1) = typres
            valk (2) = typmat
            call utmess('F', 'ALGELINE4_27', nk=2, valk=valk)
        endif
!
! --- MATRICE COMPLEXE EN RESULTAT
!
    else if (typres .eq. 'C') then
        if (typmat .eq. 'C') then
            if (typcst(1:1) .eq. 'R') then
                do 200 ival = 0, neq-1
                    if (zc(lmat+ival) .ne. cun) then
                        zc(lres+ival) = rcum*zc(lres+ival) + const(1)* zc(lmat+ival)
                    endif
200              continue
            else if (typcst(1:1) .eq. 'C') then
                c8cst = dcmplx(const(1),const(2))
                do 210 ival = 0, neq-1
                    if (zc(lmat+ival) .ne. cun) then
                        zc(lres+ival) = rcum*zc(lres+ival) + c8cst*zc( lmat+ival)
                    endif
210              continue
            else
                valk (1) = typres
                valk (2) = typmat
                valk (3) = typcst(1:1)
                call utmess('F', 'ALGELINE4_25', nk=3, valk=valk)
            endif
        else if (typmat .eq. 'R') then
            if (typcst(1:1) .eq. 'R') then
                do 220 ival = 0, neq-1
                    if (zr(lmat+ival) .ne. un) then
                        zc(lres+ival) = rcum*zc(lres+ival) + const(1)* zr(lmat+ival)
                    endif
220              continue
            else if (typcst(1:1) .eq. 'C') then
                c8cst = dcmplx(const(1),const(2))
                do 240 ival = 0, neq-1
                    if (zr(lmat+ival) .ne. un) then
                        zc(lres+ival) = rcum*zc(lres+ival) + c8cst*zr( lmat+ival)
                    endif
240              continue
            else
                valk (1) = typres
                valk (2) = typmat
                valk (3) = typcst(1:1)
                call utmess('F', 'ALGELINE4_25', nk=3, valk=valk)
            endif
        else
            valk (1) = typres
            valk (2) = typmat
            call utmess('F', 'ALGELINE4_27', nk=2, valk=valk)
        endif
    else
        valk (1) = typres
        call utmess('F', 'ALGELINE4_31', sk=valk(1))
!
    endif
!
end subroutine
