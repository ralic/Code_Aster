subroutine utites(label1, label2, type, nbref, refi,&
                  refr, refc, vali, valr, valc,&
                  epsi, crit, ific, llab, ssigne,&
                  ignore, compare)
    implicit       none
#include "asterfort/assert.h"
#include "asterfort/lxlgut.h"
#include "asterfort/tresu_print.h"
#include "asterfort/utmess.h"
    integer, intent(in) :: nbref
    character(len=*), intent(in) :: label1
    character(len=*), intent(in) :: label2
    character(len=*), intent(in) :: type
    integer, intent(in) :: refi(nbref)
    real(kind=8), intent(in) :: refr(nbref)
    complex(kind=8), intent(in) :: refc(nbref)
    integer, intent(in) :: vali
    real(kind=8), intent(in) :: valr
    complex(kind=8), intent(in) :: valc
    real(kind=8), intent(in) :: epsi
    character(len=*), intent(in) :: crit
    integer, intent(in) :: ific
    logical, intent(in) :: llab
    character(len=*), intent(in) :: ssigne
    logical, intent(in), optional :: ignore
    real(kind=8), intent(in), optional :: compare
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
!  IN  : K1  : TYPE   : TYPE DE VALEUR A TESTER 'R', OU 'C'
!  IN  : I   : NBREF  : NOMBRE DE VALEURS DE REFERENCE (=1 SOUVENT)
!  IN  : R8  : REFR   : VALEUR(S) REELLE(S) DE REFERENCE OU NON REGRESSION
!  IN  : C16 : REFC   : VALEUR(S) COMPLEXE(S) DE REFERENCE
!  IN  : R8  : VALR   : VALEUR REELLE CALCULEE ( ASTER )
!  IN  : C16 : VALC   : VALEUR COMPLEXE A TESTER ( ASTER )
!  IN  : K8  : CRIT   : COMPARAISON EN 'RELATIF' OU 'ABSOLU'
!  IN  : R8  : EPSI   : PRECISION ESPEREE
!  IN  : I   : IFIC   : NUMERO LOGIQUE DU FICHIER DE SORTIE
!  IN  : L   : LLAB   : FLAG IMPRESSION LABELS
!  OUT :                IMPRESSION SUR LISTING
! ----------------------------------------------------------------------
    integer :: wali
    real(kind=8) :: walr, ordgrd
    complex(kind=8) :: walc
    integer :: i, imin, minvi, tmpi
    real(kind=8) :: tmpr, minvr, minvc, tmpc
    complex(kind=8) :: vtc
    logical :: skip, isrela
!     ------------------------------------------------------------------
!   variables de travail
    isrela = crit(1:4) .eq. 'RELA'
    wali = vali
    walr = valr
    walc = valc
!
    if (present(ignore)) then
        skip = ignore
    else
        skip = .false.
    endif
    if (present(compare)) then
        ordgrd = compare
    else
        ordgrd = 1.d0
    endif
!
!-----------------
! --- CAS REEL ---
! ----------------
!
    if (type(1:1) .eq. 'R') then
!
        if (ssigne .eq. 'OUI') walr = abs(walr)
        minvr=abs(walr-refr(1))
        imin=1
        do 10 i = 1, nbref-1
            tmpr=abs(walr-refr(i+1))
            if (tmpr .lt. minvr) then
                tmpr=minvr
                imin=i+1
            endif
10      continue
!
        call tresu_print(label1, label2, llab, skip, isrela, &
                         epsi, refr=refr(imin), valr=walr, compare=ordgrd)
!
!-------------------
! --- CAS ENTIER ---
! ------------------
!
    else if (type(1:1) .eq. 'I') then
!
        if (ssigne .eq. 'OUI') wali = abs(wali)
        minvi=abs(wali-refi(1))
        imin=1
        do 20 i = 1, nbref-1
            tmpi=abs(wali-refi(i+1))
            if (tmpi .lt. minvi) then
                tmpi=minvi
                imin=i+1
            endif
20      continue
!
        call tresu_print(label1, label2, llab, skip, isrela, &
                         epsi, refi=refi(imin), vali=wali)
!
!---------------------
! --- CAS COMPLEXE ---
! --------------------
!
    else if (type(1:1) .eq. 'C') then
!
        vtc = refc(1)
        if (ssigne .eq. 'OUI') then
            walc = abs(walc)
            vtc = abs(vtc)
        endif
        minvc=abs(walc-vtc)
        imin=1
!    --- NBREF > 1 N'EST PAS GERE PAR LE SUPERVISEUR...
        do 30 i = 1, nbref-1
            vtc = refc(i+1)
            if (ssigne .eq. 'OUI') vtc = abs(vtc)
            tmpc=abs(walc-vtc)
            if (tmpc .lt. minvc) then
                tmpc=minvc
                imin=i+1
            endif
30      continue
        vtc = refc(imin)
        if (ssigne .eq. 'OUI') vtc = abs(vtc)
!
        call tresu_print(label1, label2, llab, skip, isrela, &
                         epsi, refc=vtc, valc=walc)
!
    endif
!
end subroutine
