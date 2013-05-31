subroutine mlnmin(nu, nomp01, nomp02, nomp03, nomp04,&
                  nomp05, nomp06, nomp07, nomp08, nomp09,&
                  nomp10, nomp11, nomp12, nomp13, nomp14,&
                  nomp15, nomp16, nomp17, nomp18, nomp19,&
                  nomp20)
!     ------------------------------------------------------------------
! person_in_charge: olivier.boiteau at edf.fr
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
! aslint: disable=W1504
    implicit none
    character(len=14) :: nu
    character(len=24) :: nomp01, nomp02, nomp03, nomp04, nomp05, nomp06, nomp07
    character(len=24) :: nomp08, nomp09, nomp10, nomp11, nomp12, nomp13, nomp14
    character(len=24) :: nomp15, nomp16, nomp17, nomp18, nomp19, nomp20
    nomp01 = nu(1:14)//'.MLTF.DESC'
    nomp02 = nu(1:14)//'.MLTF.DIAG'
    nomp03 = nu(1:14)//'.MLTF.ADRE'
    nomp04 = nu(1:14)//'.MLTF.SUPN'
    nomp05 = nu(1:14)//'.MLTF.PARE'
    nomp06 = nu(1:14)//'.MLTF.FILS'
    nomp07 = nu(1:14)//'.MLTF.FRER'
    nomp08 = nu(1:14)//'.MLTF.LGSN'
    nomp09 = nu(1:14)//'.MLTF.LFRN'
    nomp10 = nu(1:14)//'.MLTF.NBAS'
    nomp11 = nu(1:14)//'.MLTF.DEBF'
    nomp12 = nu(1:14)//'.MLTF.DEFS'
    nomp13 = nu(1:14)//'.MLTF.ADPI'
    nomp14 = nu(1:14)//'.MLTF.ANCI'
    nomp15 = nu(1:14)//'.MLTF.NBLI'
    nomp16 = nu(1:14)//'.MLTF.LGBL'
    nomp17 = nu(1:14)//'.MLTF.NCBL'
    nomp18 = nu(1:14)//'.MLTF.DECA'
    nomp19 = nu(1:14)//'.MLTF.NOUV'
    nomp20 = nu(1:14)//'.MLTF.SEQU'
end subroutine
