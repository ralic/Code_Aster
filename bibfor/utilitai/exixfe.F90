subroutine exixfe(modele, iret)
    implicit none
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jemarq.h'
    character(len=*) :: modele
    integer :: iret
! ----------------------------------------------------------------------
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
!----------------------------------------------------------------------
!
!     BUT:
!         DETECTER SI LE MODELE EST UNE MODELISATION XFEM.
!
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
! IN   MODELE : NOM DU MODELE
!
!      SORTIE :
!-------------
! OUT  IRET : 0 -> LA MODELISATION N'EST PAS XFEM
!             1 -> LA MODELISATION EST XFEM
! ......................................................................
!
!
!      CHARACTER*32 JEEXIN
!
!
    integer :: ifiss
!
! ......................................................................
!
    call jemarq()
!
    iret=0
!
    call jeexin(modele(1:8)//'.FISS', ifiss)
    if (ifiss .ne. 0) iret=1
!
    call jedema()
!
end subroutine
