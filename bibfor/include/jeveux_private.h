#ifndef JEVEUX_PRIVATE_H
#define JEVEUX_PRIVATE_H
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
! --------- DEBUT COMMONS INTERNES JEVEUX
! aslint: disable=W1304,C1002
    volatile         iacce
    volatile         ltyp,    long,    date,    iadd,    iadm
    volatile         lono,    hcod,    cara,    luti,    imarq
    volatile         genr,    type,    docu,    orig,    rnom
    volatile         indir
    volatile         iusadi
    volatile         iszon
    volatile         i4zon
    volatile         lszon
    volatile         r8zon
    volatile         k1zon
!
    integer :: iacce
    common /iacced/  iacce(1)
    integer :: ltyp   , long   , date   , iadd   , iadm
    integer :: lono   , hcod   , cara   , luti   , imarq
    common /iatrje/  ltyp(1), long(1), date(1), iadd(1), iadm(1),&
     &                 lono(1), hcod(1), cara(1), luti(1), imarq(1)
    character(len=1)       :: genr   , type
    character(len=4)       :: docu
    character(len=8)       :: orig
    character(len=32)      :: rnom
    common /katrje/  genr(8), type(8), docu(2), orig(1), rnom(1)
    integer :: indir
    common /kindir/  indir(1)
    integer :: iusadi
    common /kusadi/  iusadi(1)
!
    integer :: iszon(1)
    integer(kind=4)         :: i4zon(1)
    logical(kind=1)          lszon(1)
    real(kind=8)            :: r8zon(1)
    character(len=1)       :: k1zon
    common /kzonje/  k1zon(8)
    equivalence    ( iszon(1), k1zon(1), r8zon(1), lszon(1), i4zon(1))
!---------- FIN COMMONS INTERNES JEVEUX
#endif
