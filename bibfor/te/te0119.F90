subroutine te0119(option, nomte)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/jevech.h'
    include 'asterfort/teattr.h'
    include 'asterfort/tecael.h'
    include 'asterfort/u2mesk.h'
    character(len=16) :: option, nomte
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! person_in_charge: jacques.pellet at edf.fr
! ======================================================================
!  BUT:  CALCUL DE L'OPTION VERI_CARA_ELEM
! ......................................................................
!
    character(len=8) :: alias8
    character(len=24) :: valk(3)
    integer :: j1, ibid, iadzi, iazk24
    real(kind=8) :: excent
    character(len=3) :: cmod
!     ------------------------------------------------------------------
!
!
!     1. RECUPERATION DU CODE DE LA MODELISATION (CMOD) :
!     ---------------------------------------------------
    call teattr(' ', 'S', 'ALIAS8', alias8, ibid)
    cmod=alias8(3:5)
!
!
!     2. VERIFICATION QUE L'EXCENTREMENT EST NUL POUR
!        CERTAINES MODELISATIONS:
!     --------------------------------------------------
    if (cmod .eq. 'Q4G' .or. cmod .eq. 'DTG' .or. cmod .eq. 'CQ3') then
        call jevech('PCACOQU', 'L', j1)
        if (cmod .eq. 'Q4G' .or. cmod .eq. 'DTG') then
            excent=zr(j1-1+5)
        else
            excent=zr(j1-1+6)
        endif
        if (excent .ne. 0.d0) then
            call tecael(iadzi, iazk24)
            valk(1)=zk24(iazk24-1+3)(1:8)
            call u2mesk('F', 'CALCULEL2_31', 1, valk)
        endif
    endif
!
!
end subroutine
