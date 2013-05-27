subroutine op0068()
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
!                           OPERATEUR :     AFFE_CHAR_ACOU
!
!      MOTS-CLES ACTUELLEMENT TRAITES:
!        PRES_IMPO
!        VITE_FACE
!        IMPE_FACE
!
!
!
    include 'jeveux.h'
    include 'asterc/getres.h'
    include 'asterfort/charac.h'
    include 'asterfort/infmaj.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/wkvect.h'
    character(len=8) :: char
    character(len=16) :: type, oper
!
!-----------------------------------------------------------------------
    integer :: iatype
!-----------------------------------------------------------------------
    call jemarq()
    call infmaj()
!
    call getres(char, type, oper)
    call wkvect(char//'.TYPE', 'G V K8', 1, iatype)
    zk8(iatype)='ACOU_RE'
    call charac('REEL')
!
    call jedema()
end subroutine
