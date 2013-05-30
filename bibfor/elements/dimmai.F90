subroutine dimmai(typem, dimma)
! ======================================================================
! COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=*) :: typem
    integer :: dimma
!
    if (typem(1:2) .eq. 'TE' .or. typem(1:2) .eq. 'PE' .or. typem(1:2) .eq. 'HE') then
        dimma=3
    else if (typem(1:3).eq.'QUA' .or. typem(1:3).eq.'TRI') then
        dimma=2
    else if (typem(1:3).eq.'SEG') then
        dimma=1
    else
!  POUR DES CONDITIONS AUX LIMITES PONCTUELLES
        dimma=0
!        CALL U2MESG('F','VOLUFINI_1',1,TYPEM(1:4),0,0,0,0.D0)
    endif
end subroutine
