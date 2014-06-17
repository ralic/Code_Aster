subroutine csmbr8(nommat, ccll, ccii, neq, vcine,&
                  vsmb)
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
    character(len=*) :: nommat
    real(kind=8) :: vsmb(*), vcine(*)
    integer :: ccll(*), ccii(*), neq
!-----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
! BUT : CALCUL DE LA CONTRIBUTION AU SECOND MEMBRE DES DDLS IMPOSES
!       LORSQU'ILS SONT TRAITEES PAR ELIMINATION (CAS REEL)
! C.F. EXPLICATIONS DANS LA ROUTINE CSMBGG
!-----------------------------------------------------------------------
! IN  NOMMAT K19 : NOM DE LA MATR_ASSE
! IN  CCLL   I(*): TABLEAU .CCLL DE LA MATRICE
! IN  CCII   I(*): TABLEAU .CCII DE LA MATRICE
! IN  NEQ    I   : NOMBRE D'EQUATIONS
! VAR VSMB   R(*): VECTEUR SECOND MEMBRE
! IN  VCINE  R(*): VECTEUR DE CHARGEMENT CINEMATIQUE ( LE U0 DE U = U0
!                 SUR G AVEC VCINE = 0 EN DEHORS DE G )
!-----------------------------------------------------------------------
!     FONCTIONS JEVEUX
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!     VARIABLES LOCALES
!-----------------------------------------------------------------------
    integer ::   nelim, ielim, ieq, j,   ieqg
    integer :: deciel, kterm, nterm, imatd
    real(kind=8) :: coef
    character(len=14) :: nu
    character(len=19) :: mat
    integer, pointer :: ccid(:) => null()
    integer, pointer :: nugl(:) => null()
    real(kind=8), pointer :: ccva(:) => null()
    character(len=24), pointer :: refa(:) => null()
    integer, pointer :: nulg(:) => null()
!-----------------------------------------------------------------------
!     DEBUT
    call jemarq()
!-----------------------------------------------------------------------
    mat = nommat
!
    call jeveuo(mat//'.CCVA', 'L', vr=ccva)
    call jelira(mat//'.CCLL', 'LONMAX', nelim)
    nelim=nelim/3
!
    call jeveuo(mat//'.REFA', 'L', vk24=refa)
    if (refa(11) .eq. 'MATR_DISTR') then
        imatd = 1
        nu = refa(2)(1:14)
        call jeveuo(nu//'.NUML.NULG', 'L', vi=nulg)
        call jeveuo(nu//'.NUML.NUGL', 'L', vi=nugl)
    else
        imatd = 0
    endif
!
    do 20 ielim = 1, nelim
        ieq = ccll(3*(ielim-1)+1)
        nterm = ccll(3*(ielim-1)+2)
        deciel = ccll(3*(ielim-1)+3)
!
        if (imatd .eq. 0) then
            ieqg = ieq
        else
            ieqg = nulg(ieq)
        endif
        coef = vcine(ieqg)
!
        if (coef .ne. 0.d0) then
            do 10 kterm = 1, nterm
                if (imatd .eq. 0) then
                    j=ccii(deciel+kterm)
                else
                    j=nulg(ccii(deciel+kterm))
                endif
                vsmb(j) = vsmb(j) - coef*ccva(deciel+kterm)
10          continue
        endif
!
20  end do
    call jelibe(mat//'.CCVA')
!
    if (imatd .ne. 0) then
        do 40 ieq = 1, neq
            if (nugl(ieq) .eq. 0) vcine(ieq) = 0.d0
40      continue
    endif
!
!
    call jeveuo(mat//'.CCID', 'L', vi=ccid)
    do 30 ieq = 1, neq
        if (ccid(ieq) .eq. 1) then
            vsmb(ieq) = vcine(ieq)
        else
            if (vcine(ieq) .ne. 0.d0) then
                call utmess('F', 'ALGELINE_32')
            endif
        endif
!
30  end do
!
    call jedema()
end subroutine
