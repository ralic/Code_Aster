subroutine nmcsol(lischa, sddyna, lviss)
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
    implicit     none
    include      'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/ndynkk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    logical :: lviss
    character(len=19) :: lischa, sddyna
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (INITIALISATION)
!
! DETERMINE LA PRESENCE DE CHARGES FORCE_SOL
!
! ----------------------------------------------------------------------
!
!
! IN  LISCHA : SD LISTE DES CHARGES
! IN  SDDYNA : SD DYNAMIQUE
! OUT LVISS  : .TRUE SI PRESENCE D'UNE CHARGE FORCE_SOL
!
! ----------------------------------------------------------------------
!
    character(len=8) :: cnfsol
    character(len=24) :: charge, infcha
    integer :: jalich, jinfch
    integer :: nchar, ichar, nfsol
    character(len=24) :: nchsol
    integer :: jchsol
    character(len=15) :: sdexso
    character(len=19) :: sdexsz
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    lviss = .false.
    nfsol = 0
    cnfsol = ' '
    call ndynkk(sddyna, 'SDEXSO', sdexsz)
    sdexso = sdexsz(1:15)
!
! --- ACCES SD LISTE_CHARGES
!
    charge = lischa(1:19)//'.LCHA'
    infcha = lischa(1:19)//'.INFC'
    call jeveuo(charge, 'E', jalich)
    call jeveuo(infcha, 'E', jinfch)
    nchar = zi(jinfch)
!
! --- DETECTION CHARGE
!
    do 30 ichar = 1, nchar
        if (zi(jinfch+nchar+ichar) .eq. 20) then
            nfsol = nfsol + 1
            cnfsol = zk24(jalich+ichar-1)(1:8)
        endif
30  end do
!
! --- ACTIVATION CHARGE
!
    if (nfsol .eq. 0) then
        lviss = .false.
    else if (nfsol.eq.1) then
        lviss = .true.
    else
        call u2mess('F', 'DYNAMIQUE_9')
    endif
!
! --- NOM DE _LA_CHARGE
!
    if (lviss) then
        nchsol = sdexso(1:15)//'.CHAR'
        call wkvect(nchsol, 'V V K8', 1, jchsol)
        zk8(jchsol) = cnfsol
    endif
!
    call jedema()
!
end subroutine
