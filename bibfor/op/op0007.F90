subroutine op0007()
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
!                           OPERATEUR :     AFFE_CHAR_MECA
!
!      MOTS-CLES ACTUELLEMENT TRAITES:
!
!        VOIR ROUTINE CHARME
!
!
!
    include 'jeveux.h'
    include 'asterc/getres.h'
    include 'asterfort/charme.h'
    include 'asterfort/infmaj.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/wkvect.h'
    character(len=4) :: type
    character(len=8) :: char
    character(len=16) :: concep, oper
!
!-----------------------------------------------------------------------
    integer :: iatype
!-----------------------------------------------------------------------
    call jemarq()
!
    call infmaj()
!
    call getres(char, concep, oper)
    call wkvect(char//'.TYPE', 'G V K8', 1, iatype)
    if (oper .eq. 'AFFE_CHAR_MECA') then
        type = 'REEL'
        zk8(iatype) = 'MECA_RE'
    else if (oper .eq. 'AFFE_CHAR_MECA_F') then
        type = 'FONC'
        zk8(iatype) = 'MECA_FO'
    else if (oper .eq. 'AFFE_CHAR_MECA_C') then
        type = 'COMP'
        zk8(iatype) = 'MECA_RI'
    endif
!
    call charme(type)
!
    call jedema()
end subroutine
