subroutine rcevo1(nommat, fatizh, sm, para, symax)
    implicit none
#include "jeveux.h"
#include "asterc/r8vide.h"
#include "asterfort/rccome.h"
#include "asterfort/rcvale.h"
#include "asterfort/utmess.h"
    real(kind=8) :: sm, para(*), symax
    logical :: fatizh
    character(len=8) :: nommat
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     OPERATEUR POST_RCCM, TYPE_RESU_MECA='EVOLUTION'
!     LECTURE DU MOT CLE SIMPLE "MATER"
!
!     ------------------------------------------------------------------
!
    integer :: nbpar
    real(kind=8) :: valres(3), erefe(1), e(1), rbid, tsm(1)
    integer :: icodre(3)
    character(len=8) :: nompar, nomval(3)
    character(len=16) :: phenom
! DEB ------------------------------------------------------------------
!
    rbid = 0.d0
    nbpar = 0
    nompar = ' '
    nomval(1) = 'SM'
    call rcvale(nommat, 'RCCM', nbpar, nompar, [rbid],&
                1, nomval, tsm, icodre, 2)
    sm=tsm(1)
!
    para(1) = r8vide()
    para(2) = r8vide()
    para(3) = r8vide()
    if (fatizh) then
        call rccome(nommat, 'FATIGUE', phenom, icodre(1))
        if (icodre(1) .eq. 1) then
            call utmess('F', 'POSTRCCM_7', sk='FATIGUE')
        endif
        call rccome(nommat, 'ELAS', phenom, icodre(1))
        if (icodre(1) .eq. 1) then
            call utmess('F', 'POSTRCCM_7', sk='ELAS')
        endif
!
        nomval(1) = 'M_KE'
        nomval(2) = 'N_KE'
        call rcvale(nommat, 'RCCM', nbpar, nompar, [rbid],&
                    2, nomval, valres, icodre, 2)
        para(1) = valres(1)
        para(2) = valres(2)
!
        nomval(1) = 'E_REFE'
        call rcvale(nommat, 'FATIGUE', nbpar, nompar, [rbid],&
                    1, nomval, erefe(1), icodre, 2)
!
        nomval(1) = 'E'
        call rcvale(nommat, 'ELAS', nbpar, nompar, [rbid],&
                    1, nomval, e(1), icodre, 2)
        para(3) = erefe(1) / e(1)
    endif
!
    if (symax .eq. r8vide()) then
        nomval(1) = 'SY_02'
        call rcvale(nommat, 'RCCM', nbpar, nompar, [rbid],&
                    1, nomval, valres, icodre, 0)
        if (icodre(1) .eq. 0) symax = valres(1)
    endif
!
end subroutine
