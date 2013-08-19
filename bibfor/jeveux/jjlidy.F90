subroutine jjlidy(iadyn, iadmi)
! person_in_charge: j-pierre.lefebvre at edf.fr
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
    implicit none
#include "jeveux_private.h"
#include "asterc/hpdeallc.h"
#include "asterfort/assert.h"
    integer :: iadyn, iadmi
! ----------------------------------------------------------------------
! MISE A JOUR DU COMPTEUR DES SEGMENTS DE VALEURS U ET LIBERATION
! DU SEGMENT DE VALEURS
!
! IN  IADYN  : ADRESSE DYNAMIQUE DU SEGMENT DE VALEUR
! IN  IADMI  : ADRESSE DU PREMIER MOT DU SEGMENT DE VALEUR
!
! ----------------------------------------------------------------------
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
! ----------------------------------------------------------------------
    real(kind=8) :: mxdyn, mcdyn, mldyn, vmxdyn, vmet, lgio
    common /r8dyje/ mxdyn ,mcdyn, mldyn, vmxdyn, vmet, lgio(2)
    integer :: ldyn, lgdyn, nbdyn, nbfree
    common /idynje/  ldyn , lgdyn , nbdyn , nbfree
    integer :: istat
    common /istaje/  istat(4)
    integer :: lbis, lois, lols, lor8, loc8
    common /ienvje/  lbis , lois , lols , lor8 , loc8
    real(kind=8) :: svuse, smxuse
    common /statje/  svuse,smxuse
    integer :: lundef, idebug
    common /undfje/  lundef,idebug
! ----------------------------------------------------------------------
    integer :: iet, lgs, lgsv, k
! DEB ------------------------------------------------------------------
    if (iadyn .ne. 0) then
        iet = iszon(jiszon+iadmi-1)
        lgs = iszon(jiszon+iadmi-4) - iadmi + 4
        lgsv= iszon(jiszon+iadmi-4) - iadmi - 4
        do 100 k = 1, lgsv
            iszon(jiszon+iadmi+k-1) = lundef
100      continue
        if (iet .eq. istat(2)) then
            svuse = svuse - lgs
            ASSERT(lgs .gt. 0)
            smxuse = max(smxuse,svuse)
        endif
        mcdyn = mcdyn - lgs
        mldyn = mldyn + lgs
        call hpdeallc(iadyn, nbfree)
    endif
! FIN ------------------------------------------------------------------
end subroutine
