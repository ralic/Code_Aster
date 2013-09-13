subroutine te0589(option, nomte)
    implicit none
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
#include "jeveux.h"
#include "asterfort/elref5.h"
#include "asterfort/tutemp.h"
#include "asterfort/utmess.h"
    character(len=16) :: option, nomte
! ......................................................................
!
!    - FONCTION REALISEE:  CALCUL DU SECOND MEMBRE : TRAVAIL DE LA
!                          DILATATION THERMIQUE
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    integer :: nbrddm, npg, ipoids, ivf
    parameter (nbrddm=156)
    integer :: ndim, nnos, nno, jcoopg, idfdk, jdfd2, jgano
    real(kind=8) :: f(nbrddm), b(4, nbrddm), vout(nbrddm)
    real(kind=8) :: vtemp(nbrddm), pass(nbrddm, nbrddm)
    integer :: m, nbrddl
!
    call elref5(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, jcoopg, ivf, idfdk,&
                jdfd2, jgano)
!
!
    m = 3
    if (nomte .eq. 'MET6SEG3') m = 6
!
!     FORMULE GENERALE
!
    nbrddl = nno* (6+3+6* (m-1))
!
!     VERIFS PRAGMATIQUES
!
    if (nbrddl .gt. nbrddm) then
        call utmess('F', 'ELEMENTS4_40')
    endif
    if (nomte .eq. 'MET3SEG3') then
        if (nbrddl .ne. 63) then
            call utmess('F', 'ELEMENTS4_41')
        endif
    else if (nomte.eq.'MET6SEG3') then
        if (nbrddl .ne. 117) then
            call utmess('F', 'ELEMENTS4_41')
        endif
    else if (nomte.eq.'MET3SEG4') then
        if (nbrddl .ne. 84) then
            call utmess('F', 'ELEMENTS4_41')
        endif
    else
        call utmess('F', 'ELEMENTS4_42')
    endif
!
    call tutemp(option, nomte, nbrddl, f, b,&
                vout, pass, vtemp)
end subroutine
