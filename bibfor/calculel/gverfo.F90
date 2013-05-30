subroutine gverfo(charg, ier)
    implicit none
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    character(len=19) :: charg
    integer :: ier
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
!     BUT : VERIFIE SI LE CHARGMENT FONCTION EST DE TYPE 'FORMULE'
!           ROUTINE APPELEE PAR GCHARG (OPERATEUR CALC_G)
!
!
!     IN :    CHARG   : NOM DE LA CHARGE (ex: NOMCHA//'.CHME.F3D3D')
!
!     OUT:    IER     : =1 : PRESENCE D'UN CHARGMENT 'FORMULE'
!                       =0 : SINON
!
! ======================================================================
! ----------------------------------------------------------------------
    integer :: ival, nbvale, in, iprol
    character(len=8) :: k8b
    character(len=19) :: nch19
!
    call jemarq()
!
    call jeveuo(charg//'.VALE', 'L', ival)
    call jelira(charg//'.VALE', 'LONMAX', nbvale, k8b)
!
    ier=0
    do 10 in = 1, nbvale
        if (zk8(ival+in-1)(1:7) .ne. '&FOZERO' .and. zk8(ival+in-1)(1:7) .ne. '       '&
            .and. zk8(ival+in-1)(1:6) .ne. 'GLOBAL') then
            nch19=zk8(ival+in-1)
            call jeveuo(nch19//'.PROL', 'L', iprol)
            if (zk24(iprol)(1:8) .eq. 'INTERPRE') then
                ier=1
                goto 9999
            endif
        endif
10  end do
!
9999  continue
!
    call jedema()
!
end subroutine
