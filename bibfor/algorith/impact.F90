subroutine impact(nmtab, nbpt, fn, vn, wk3,&
                  offset, t, elapse, nbchoc, fnmaxa,&
                  fnmmoy, fnmety, npari, lpari, valek)
    implicit   none
#include "asterfort/tbajli.h"
    integer :: nbpt, nbchoc, npari
    real(kind=8) :: fn(*), t(*), vn(*), offset, elapse, wk3(*), fnmaxa, fnmety
    real(kind=8) :: fnmmoy
    character(len=16) :: lpari(*), valek(*)
    character(len=*) :: nmtab
! ----------------------------------------------------------------------
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
! ----------------------------------------------------------------------
!        COMPTAGE DES CHOCS AMV
!
! IN  : NBPT   : NB DE POINTS DU SIGNAL
! IN  : FN     : TABLEAU DU SIGNAL
! IN  : T      : TABLEAU DU TEMPS
! IN  : OFFSET : VALEUR DU SEUIL DE DETECTION D UN CHOC
! IN  : ELAPSE : TEMPS MINIMUM POUR VRAI FIN DE CHOC
! OUT : NBCHOC : NB DE CHOC GLOBAUX ( CRITERE ELAPSE )
! ----------------------------------------------------------------------
!
    integer :: ipar(2), irebo, ichoc, idebut, ifin, nbpas, i, j, idech, nbrebo
    integer :: k
    real(kind=8) :: impuls, para(5), zero, fnmax, tchoc, vinit, dt, tfnmax
    real(kind=8) :: fnmmo2
    complex(kind=8) :: c16b
! ----------------------------------------------------------------------
!
    zero = 0.0d0
    nbchoc = 0
    nbrebo = 0
    fnmax = zero
    fnmaxa = zero
    fnmmoy = zero
    fnmety = zero
    tfnmax = zero
    impuls = zero
    tchoc = zero
    vinit = zero
    irebo = 0
    ichoc = 0
    idebut = 1
    ifin = 1
    dt = t(4) - t(3)
    nbpas = nint ( elapse / dt )
!
    k = 0
    do 10 i = 1, nbpt
!
        if (abs(fn(i)) .le. offset) then
!
!           SI SOUS LE SEUIL DE FORCE
!
            if (irebo .eq. 1) then
!
!              ET QUE ETAIT EN REBOND ALORS COMPTER REBOND
!
                nbrebo = nbrebo + 1
            endif
!
            idech = 0
!
            do 15 j = 1, nbpas
!
!              EST CE QUE C'EST LA FIN D'UN CHOC GLOBAL
!
                if (abs(fn(i+j)) .gt. offset) idech = 1
15          continue
!
            if (idech .eq. 0 .and. ichoc .eq. 1) then
!
!              OUI C'EST LA FIN D'UN CHOC GLOBAL
!
                ifin = i
                tchoc = t(ifin) - t(idebut)
                fnmmoy = fnmmoy + fnmax
!                    FNMMOY EST PROVISOIREMENT LE CUMUL DES FNMAX, ON
!                    DIVISE A LA FIN PAR NBCHOC POUR AVOIR LA MOYENNE
                fnmety = fnmety + fnmax*fnmax
                nbchoc = nbchoc + 1
                ichoc = 0
                k = k + 1
                wk3(k) = fnmax
                para(1) = tfnmax
                para(2) = fnmax
                para(3) = impuls
                para(4) = tchoc
                para(5) = vinit
                ipar(1) = nbchoc
                ipar(2) = nbrebo
                call tbajli(nmtab, npari, lpari, ipar, para,&
                            [c16b], valek, 0)
            endif
!
            irebo = 0
!
        else
!
            if (ichoc .eq. 0) then
!              DEBUT D'UN CHOC GLOBAL
                idebut = i
                vinit = vn(idebut)
                fnmax = zero
                impuls = zero
                nbrebo = 0
            endif
            if (i .eq. 1) then
                impuls = impuls + fn(i)*t(i)/2.d0
            else if (i.lt.nbpt) then
                impuls = impuls + fn(i)*(t(i+1)-t(i-1))/2.d0
            else
                impuls = impuls + fn(i)*t(i)/2.d0
            endif
            if (fn(i) .ge. fnmax) then
                fnmax = fn(i)
                tfnmax = t(i)
            endif
            if (fnmax .ge. fnmaxa) fnmaxa = fnmax
            irebo = 1
            ichoc = 1
!
        endif
!
10  end do
!
    if (nbchoc .ne. 0) then
!      ON PASSE PAR UNE VARIABLE INTERMEDIAIRE FNMMO2
!      POUR EVITER LES PROBLEMES DE PRECISION
        fnmmoy = fnmmoy / nbchoc
        fnmmo2 = fnmmoy*fnmmoy
        fnmety = sqrt(abs(fnmety/nbchoc-fnmmo2))
    else
        k = k + 1
        wk3(k) = fnmax
        fnmmoy = zero
        fnmety = zero
        para(1) = tfnmax
        para(2) = fnmax
        para(3) = impuls
        para(4) = tchoc
        para(5) = vinit
        nbrebo = 0
        ipar(1) = nbchoc
        ipar(2) = nbrebo
        call tbajli(nmtab, npari, lpari, ipar, para,&
                    [c16b], valek, 0)
    endif
!
end subroutine
