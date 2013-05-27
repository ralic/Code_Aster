subroutine vpordi(type, iordre, nbpro, valpro, vecpro,&
                  neq, indpro)
    implicit none
    integer :: type, nbpro, neq, indpro(*)
    real(kind=8) :: valpro(*), vecpro(neq, nbpro)
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!     TRIE DES VALEURS (ET DES VECTEURS) PROPRES PAR ORDRE CROISSANT
!     EN INCLUANT LES INDICES
!     ------------------------------------------------------------------
! IN  TYPE   : IS : TYPE DU TRI SUR LES VALEURS.
!        * SI TYPE = 0  TRI EN VALEUR RELATIVE
!        * SI TYPE = 1  TRI EN VALEUR ABSOLUE
! IN  IORDRE : IS : ORDRE DU TRI SUR LES VALEURS.
!        * SI IORDRE = 0  TRI PAR ORDRE CROISSANT
!        * SI IORDRE = 1  TRI PAR ORDRE DECROISSANT
! IN  NBPRO  : IS : NOMBRE DE VALEUR PROPRE
!     VALPRO : R8 : TABLEAU DES VALEURS PROPRES
!     VECPRO : R8 : MATRICE DES VECTEURS PROPRES
!     NEQ    : IS : NOMBRE D'EQUATIONS
!                 SI NEQ < NBPRO ALORS ON NE TRIE PAS DE VECTEURS
!     ------------------------------------------------------------------
    integer :: iperm, inperm
    real(kind=8) :: rperm, eps
!
!
!     --- TRI PAR ORDRE CROISSANT ---
!-----------------------------------------------------------------------
    integer :: i, iordre, j
!-----------------------------------------------------------------------
    eps = 1.d-7
    if (iordre .eq. 0) then
!
        do 10 i = 1, nbpro
            iperm = i
            if (type .eq. 0) then
                rperm = valpro(i)
                do 11 j = i+1, nbpro
                    if (valpro(j) .le. rperm) then
                        iperm = j
                        rperm = valpro(iperm)
                    endif
11              continue
            else if (type .eq. 1) then
                rperm = abs(valpro(i))
                do 12 j = i+1, nbpro
                    if (abs(valpro(j)) .lt. (rperm *(1.d0 -eps))) then
                        iperm = j
                        rperm = abs(valpro(iperm))
                    endif
                    if ((abs(valpro(j))-rperm) .le. (eps*rperm)) then
                        if (((valpro(j)*valpro(iperm)).ge. 0.d0) .and.&
                            ( abs(valpro(j)) .lt. rperm )) then
                            iperm = j
                            rperm = abs(valpro(iperm))
                        endif
                        if (((valpro(j)*valpro(iperm)).lt. 0.d0) .and.&
                            ( valpro(j) .lt. 0.d0 )) then
                            iperm = j
                            rperm = abs(valpro(iperm))
                        endif
                    endif
12              continue
            endif
!
            if (iperm .ne. i) then
                rperm = valpro(iperm)
                inperm = indpro(iperm)
                valpro(iperm) = valpro(i)
                valpro(i) = rperm
                indpro(iperm) = indpro(i)
                indpro(i) = inperm
                if (neq .ge. nbpro) then
                    do 30 j = 1, neq
                        rperm = vecpro(j,i)
                        vecpro(j,i) = vecpro(j,iperm)
                        vecpro(j,iperm) = rperm
30                  continue
                endif
            endif
10      end do
!
    else if (iordre.eq.1) then
!
        do 20 i = 1, nbpro
            iperm = i
            if (type .eq. 0) then
                rperm = valpro(i)
                do 21 j = i+1, nbpro
                    if (valpro(j) .ge. rperm) then
                        iperm = j
                        rperm = valpro(iperm)
                    endif
21              continue
            else if (type .eq. 1) then
                rperm = abs(valpro(i))
                do 22 j = i+1, nbpro
                    if (abs(valpro(j)) .gt. (rperm*(1.d0+eps))) then
                        iperm = j
                        rperm = abs(valpro(iperm))
                    endif
22              continue
                if ((abs(valpro(j))-rperm) .le. (eps*rperm)) then
                    if (((valpro(j)*valpro(iperm)).ge. 0.d0) .and.&
                        ( abs(valpro(j)) .gt. rperm )) then
                        iperm = j
                        rperm = abs(valpro(iperm))
                    endif
                    if (((valpro(j)*valpro(iperm)).lt. 0.d0) .and. ( valpro(j) .lt. 0.d0 )) then
                        iperm = j
                        rperm = abs(valpro(iperm))
                    endif
                endif
            endif
!
            if (iperm .ne. i) then
                rperm = valpro(iperm)
                valpro(iperm) = valpro(i)
                valpro(i) = rperm
                inperm = indpro(iperm)
                indpro(iperm) = indpro(i)
                indpro(i) = inperm
                if (neq .ge. nbpro) then
                    do 40 j = 1, neq
                        rperm = vecpro(j,i)
                        vecpro(j,i) = vecpro(j,iperm)
                        vecpro(j,iperm) = rperm
40                  continue
                endif
            endif
20      end do
!
    endif
!
end subroutine
