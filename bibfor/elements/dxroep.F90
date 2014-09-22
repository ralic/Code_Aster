subroutine dxroep(rho, epais)
    implicit none
#include "jeveux.h"
#include "asterc/r8maem.h"
#include "asterfort/jevech.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvala.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
    real(kind=8) :: rho, epais
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     APPEL DES MASSE VOLUMIQUE DU MATERIAU ET EPAISSEUR DE LA PLAQUE
!     ------------------------------------------------------------------
    integer :: jmate, nbv, jcoqu, iadzi, iazk24
    real(kind=8) :: r8bid, valres(2)
    integer :: icodre(2)
    character(len=24) :: valk(2)
    character(len=8) :: nomail
    character(len=16) :: nomres(2)
    character(len=32) :: phenom
! --DEB
!
    r8bid = 0.d0
    call jevech('PMATERC', 'L', jmate)
!
    call rccoma(zi(jmate), 'ELAS', 1, phenom, icodre(1))
!
    if (phenom .eq. 'ELAS_COQMU') then
        nomres(1) = 'HOM_19'
        nomres(2) = 'HOM_20'
        nbv = 2
        call rcvala(zi(jmate), ' ', phenom, 0, ' ', [r8bid], nbv, nomres, valres, icodre, 1)
        epais = valres(1)
        rho = valres(2)
        if (rho .eq. r8maem()) then
            call tecael(iadzi, iazk24)
            nomail = zk24(iazk24-1+3)(1:8)
            valk (1) = 'RHO'
            valk (2) = nomail
            call utmess('F', 'ELEMENTS4_81', nk=2, valk=valk)
        endif
!
    elseif (phenom .eq. 'ELAS'      .or. phenom .eq. 'ELAS_COQUE' .or.&
            phenom .eq. 'ELAS_ISTR' .or. phenom .eq. 'ELAS_ORTH'  .or.&
            phenom .eq. 'ELAS_GLRC' .or. phenom .eq. 'ELAS_DHRC') then
        nomres(1) = 'RHO'
        nbv = 1
        call rcvala(zi(jmate), ' ', phenom, 0, ' ', [r8bid], nbv, nomres, valres, icodre, 1)
        rho = valres(1)
        call jevech('PCACOQU', 'L', jcoqu)
        epais = zr(jcoqu)
!
    else
        call utmess('F', 'ELEMENTS_50')
    endif
!
end subroutine
