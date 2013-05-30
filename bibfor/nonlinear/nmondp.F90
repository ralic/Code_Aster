subroutine nmondp(lischa, londe, chondp, nondp)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit      none
    include 'jeveux.h'
!
    include 'asterc/getfac.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/wkvect.h'
    integer :: nondp
    character(len=24) :: chondp
    character(len=19) :: lischa
    logical :: londe
!
!
!
!
    integer :: jinf, ialich, nchar, ich, iondp, nond
    character(len=24) :: infoch, charge
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    londe = .false.
    infoch = lischa(1:19)//'.INFC'
    charge = lischa(1:19)//'.LCHA'
    nondp = 0
    call jeveuo(infoch, 'L', jinf)
    call jeveuo(charge, 'L', ialich)
    call getfac('EXCIT', nchar)
    do 30 ich = 1, nchar
        if (zi(jinf+nchar+ich) .eq. 6) then
            nondp = nondp + 1
        endif
30  end do
!
! --- RECUPERATION DES DONNEES DE CHARGEMENT PAR ONDE PLANE
    chondp = '&&NMONDP.ONDP'
    if (nondp .eq. 0) then
        call wkvect(chondp, 'V V K8', 1, iondp)
    else
        londe = .true.
        call wkvect(chondp, 'V V K8', nondp, iondp)
        nond = 0
        do 40 ich = 1, nchar
            if (zi(jinf+nchar+ich) .eq. 6) then
                nond = nond + 1
                zk8(iondp+nond-1) = zk24(ialich+ich-1)
            endif
40      continue
    endif
    call jedema()
end subroutine
