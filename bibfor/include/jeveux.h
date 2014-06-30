#ifndef JEVEUX_H
#define JEVEUX_H
!
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
!
!---------- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
! aslint: disable=W1304
    volatile           zi4, zi, zr, zc, zl
    volatile           zk8, zk16, zk24, zk32, zk80
!
    integer(kind=4)           :: zi4
    common  / i4vaje / zi4(1)
    integer :: zi
    common  / ivarje / zi(1)
    real(kind=8)              :: zr
    common  / rvarje / zr(1)
    complex(kind=8)          :: zc
    common  / cvarje / zc(1)
    logical(kind=1)          :: zl
    common  / lvarje / zl(1)
    character(len=8)         :: zk8
    character(len=16)                :: zk16
    character(len=24)                         :: zk24
    character(len=32)                                  :: zk32
    character(len=80)                                           :: zk80
    common  / kvarje / zk8(1), zk16(1), zk24(1), zk32(1), zk80(1)
!---------- FIN  COMMUNS NORMALISES  JEVEUX ----------------------------
#endif
