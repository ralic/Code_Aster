subroutine liscpy(lischa, lisch2, base)
!
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
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/liscad.h'
    include 'asterfort/lisccr.h'
    include 'asterfort/liscli.h'
    include 'asterfort/lisnch.h'
    character(len=19) :: lischa, lisch2
    character(len=1) :: base
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (LISTE_CHARGES)
!
! COPIE DE LA SD LISTE_CHARGES
!
! ----------------------------------------------------------------------
!
! IN  LISCHA : NOM DE LA SD LISTE_CHARGES SOURCE
! IN  BASE   : BASE DE CREATION DE LA SD DESTINATION
! OUT LISCH2 : NOM DE LA SD LISTE_CHARGES DESTINATION
!
!
!
!
    character(len=24) :: charge, infcha, fomult
    integer :: jalich, jinfch, jalifc
    integer :: nchar, ich, ich2, nchar2, ival, ityp
    character(len=8) :: nomcha, nomfct
! --- NOMBRE MAXIMUM DE TYPE_INFO
    integer :: nbinmx, nbinfo
    parameter   (nbinmx=99)
    character(len=24) :: lisinf(nbinmx)
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES SD
!
    charge = lischa(1:19)//'.LCHA'
    infcha = lischa(1:19)//'.INFC'
    fomult = lischa(1:19)//'.FCHA'
!
    call jeveuo(charge, 'L', jalich)
    call jeveuo(infcha, 'L', jinfch)
    call jeveuo(fomult, 'L', jalifc)
!
    if (zi(jinfch) .eq. 0) then
        call lisccr(lisch2, 1, base)
        call jeveuo(lisch2(1:19)//'.INFC', 'E', jinfch)
        zi(jinfch) = 0
        goto 999
    else
        call lisnch(lischa, nchar)
        nchar2 = nchar
    endif
!
    do 24 ich = 1, nchar
        ityp = zi(jinfch+nchar+ich)
        if (ityp .eq. 10) then
            nchar2 = nchar2-1
        endif
24  end do
!
    call assert(nchar2.gt.0)
    call assert(nchar2.le.nchar)
!
    call lisccr(lisch2, nchar2, base)
!
    ich2 = 1
    do 25 ich = 1, nchar
        nomcha = zk24(jalich+ich-1) (1:8)
        ityp = zi(jinfch+nchar+ich)
        if (ityp .ne. 10) then
            nbinfo = nbinmx
            call liscli(lischa, ich, nomcha, nomfct, nbinfo,&
                        lisinf, ival)
            call liscad(lisch2, ich2, nomcha, nomfct, nbinfo,&
                        lisinf, ival)
!
            ich2 = ich2 + 1
        endif
25  end do
!
    call assert(nchar2.eq.(ich2-1))
!
999  continue
    call jedema()
end subroutine
