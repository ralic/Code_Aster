subroutine te0584(option, nomte)
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
#include "asterfort/elrefe_info.h"
#include "asterfort/tusief.h"
#include "asterfort/utmess.h"
    character(len=16) :: option, nomte
! ......................................................................
!
!    - FONCTION REALISEE:  CALCUL DES OPTIONS EPSI_ELGA
!                          ET SIEF_ELGA POUR UN TUYAU DROIT
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
!
    integer :: nbrddm
    parameter (nbrddm=156)
    real(kind=8) :: b(4, nbrddm)
    real(kind=8) :: vin(nbrddm), mat(4, nbrddm)
    real(kind=8) :: vtemp(nbrddm), pass(nbrddm, nbrddm)
    integer :: m, nbrddl
    integer :: ndim, nnos, nno, jcoopg, idfdk, jdfd2, jgano
    integer :: npg, ipoids, ivf
!
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jcoopg=jcoopg,jvf=ivf,jdfde=idfdk,&
  jdfd2=jdfd2,jgano=jgano)
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
    call tusief(option, nomte, nbrddl, b, vin,&
                mat, pass, vtemp)
end subroutine
