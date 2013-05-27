subroutine op0117()
    implicit none
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: nicolas.greffet at edf.fr
!     BUT: RECUPERER LE NUMERO DE COMPONENT VENANT DE YACS
!     ON LE MET DANS UN OBJET JEVEUX &ADR_YACS
    include 'jeveux.h'
    include 'asterc/getvis.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/wkvect.h'
    integer :: icompo
    integer :: ibid
!
    integer :: zyacs
    character(len=24) :: ayacs
!
!
!
!
    integer :: iarg
!
!
!
!
    call jemarq()
!
!
    call getvis(' ', 'COMPO', 0, iarg, 1,&
                icompo, ibid)
!
!
!     ICOMPO MIS EN COMMON ASTER
!     --------------------------
    ayacs='&ADR_YACS'
    call wkvect(ayacs, 'G V I', 1, zyacs)
    zi(zyacs)=icompo
!
    call jedema()
!
end subroutine
