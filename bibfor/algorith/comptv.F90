subroutine comptv(nbpt, fn, offset, t, nbchoc,&
                  tchmin, tchmax, tchoct, tchocm, nbrebo,&
                  trebot, trebom)
! ----------------------------------------------------------------------
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
! ----------------------------------------------------------------------
!        COMPTAGE DES CHOCS
!        ALGORITHME TEMPOREL A PAS VARIABLE
!
! IN  : NBPT   : NB DE POINTS DU SIGNAL
! IN  : FN     : TABLEAU DU SIGNAL
! IN  : T      : TABLEAU DU TEMPS
! IN  : OFFSET : VALEUR DU SEUIL DE DETECTION D UN CHOC
! OUT : NBCHOC : NB DE CHOC GLOBAUX ( CRITERE ELAPSE )
! OUT : NBREBO : NB DE REBONDS ( RETOUR AU SEUIL )
! OUT : TCHOCM : TEMPS DE CHOC GLOBAL MOYEN
! OUT : TREBOM : TEMPS DE REBOND MOYEN
! OUT : TCHOCT : TEMPS DE CHOC CUMULE
! ----------------------------------------------------------------------
!
    implicit none
    real(kind=8) :: fn(*), t(*)
!
!-----------------------------------------------------------------------
    integer :: i, ichoc, idebur, idebut, idech, ifin, ifinr
    integer :: irebo, nbchoc, nbpt, nbrebo
    real(kind=8) :: offset, tchmax, tchmin, tchoc, tchocm, tchoct, trebo
    real(kind=8) :: trebom, trebot, zero
!-----------------------------------------------------------------------
    zero = 0.d0
    nbchoc = 0
    nbrebo = 0
    tchocm = zero
    tchoct = zero
    trebom = zero
    trebot = zero
    tchmax = zero
    tchmin = 1.0d20
    irebo = 0
    ichoc = 0
    idebut = 1
    idebur = 1
    ifin = 1
!
    do 10 i = 1, nbpt
!
        if (abs(fn(i)) .le. offset) then
!
            if (irebo .eq. 1) then
                ifinr = i
                trebo = t(ifinr) - t(idebur)
                trebom = trebom + trebo
                nbrebo = nbrebo + 1
            endif
!
            idech = 0
            if (abs(fn(i+1)) .gt. offset) idech =1
!
            if (idech .eq. 0 .and. ichoc .eq. 1) then
!
                ifin = i
                tchoc = t(ifin) - t(idebut)
                tchocm = tchocm + tchoc
!
                if (tchoc .gt. tchmax) tchmax = tchoc
!
                if (tchoc .lt. tchmin) tchmin = tchoc
!
                nbchoc = nbchoc + 1
                ichoc = 0
!
            endif
!
            irebo = 0
!
        else
!
            if (ichoc .eq. 0) idebut = i
!
            if (irebo .eq. 0) idebur = i
            irebo = 1
            ichoc = 1
!
        endif
!
10  continue
!
    tchoct = tchocm
    if (nbchoc .ne. 0) then
        tchocm=tchocm/nbchoc
    else
        tchocm = zero
    endif
!
    trebot = trebom
    if (nbrebo .ne. 0) then
        trebom = trebom / nbrebo
    else
        trebom = zero
    endif
!
end subroutine
