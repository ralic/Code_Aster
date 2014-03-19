subroutine te0585(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/tuforc.h"
#include "asterfort/utmess.h"
!
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
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
!
!    - FONCTION REALISEE:  CALCUL DES OPTIONS FORC_NODA ET
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    integer :: nbrddm
    parameter (nbrddm=156)
    real(kind=8) :: b(4, nbrddm), f(nbrddm)
    real(kind=8) :: vin(nbrddm), vout(nbrddm), mat(nbrddm, 4)
    real(kind=8) :: vtemp(nbrddm), pass(nbrddm, nbrddm)
!
!     CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
!
    integer :: ndim, nnos, nno, jcoopg, idfdk, jdfd2, jgano
    integer :: npg, ipoids, ivf
    integer :: m, nbrddl
!
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jcoopg=jcoopg,jvf=ivf,jdfde=idfdk,&
  jdfd2=jdfd2,jgano=jgano)
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
    call tuforc(option, nomte, nbrddl, b, f,&
                vin, vout, mat, pass, vtemp)
end subroutine
