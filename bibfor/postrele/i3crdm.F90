subroutine i3crdm(descm)
    implicit none
!
    include 'jeveux.h'
    include 'asterfort/jecreo.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveut.h'
    include 'asterfort/wkvect.h'
    character(len=24) :: descm
!
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     CREATION DU POINTEUR SUR LES DESCRIPTEURS DE MAILLES
!     ------------------------------------------------------------------
! IN  DESCM  : K : NOM OJB DES POINTEURS SUR LES DESCRIPTEURS DE MAILLES
!     ------------------------------------------------------------------
!     REMARQUE : MAILLES DECRITES = PENTA, HEXA, TETRA
!     ------------------------------------------------------------------
!
    character(len=8) :: kbid
!
!
    integer :: adescm, atetra, ahexa, apenta
!
!======================================================================
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call jemarq()
    call wkvect(descm, 'V V I', 3, adescm)
    call jecreo('&&I3DESCM.DESC.TETRA', 'V V I')
    call jeecra('&&I3DESCM.DESC.TETRA', 'LONMAX', 38, kbid)
    call jeecra('&&I3DESCM.DESC.TETRA', 'LONUTI', 38, kbid)
    call jeveut('&&I3DESCM.DESC.TETRA', 'E', atetra)
    call jecreo('&&I3DESCM.DESC.PENTA', 'V V I')
    call jeecra('&&I3DESCM.DESC.PENTA', 'LONMAX', 38, kbid)
    call jeecra('&&I3DESCM.DESC.PENTA', 'LONUTI', 38, kbid)
    call jeveut('&&I3DESCM.DESC.PENTA', 'E', apenta)
    call jecreo('&&I3DESCM.DESC.HEXA ', 'V V I')
    call jeecra('&&I3DESCM.DESC.HEXA ', 'LONMAX', 38, kbid)
    call jeecra('&&I3DESCM.DESC.HEXA ', 'LONUTI', 38, kbid)
    call jeveut('&&I3DESCM.DESC.HEXA ', 'E', ahexa)
    zi(adescm + 1-1) = atetra
    zi(adescm + 2-1) = apenta
    zi(adescm + 3-1) = ahexa
    zi(atetra + 1 -1) = 4
    zi(atetra + 2 -1) = 4
    zi(atetra + 3 -1) = 3
    zi(atetra + 4 -1) = 3
    zi(atetra + 5 -1) = 3
    zi(atetra + 6 -1) = 3
    zi(atetra + 7 -1) = 0
    zi(atetra + 8 -1) = 0
    zi(atetra + 9 -1) = 1
    zi(atetra + 10-1) = 1
    zi(atetra + 11-1) = 1
    zi(atetra + 12-1) = 2
    zi(atetra + 13-1) = 0
    zi(atetra + 14-1) = 0
    zi(atetra + 15-1) = 3
    zi(atetra + 16-1) = 4
    zi(atetra + 17-1) = 2
    zi(atetra + 18-1) = 3
    zi(atetra + 19-1) = 0
    zi(atetra + 20-1) = 0
    zi(atetra + 21-1) = 2
    zi(atetra + 22-1) = 3
    zi(atetra + 23-1) = 4
    zi(atetra + 24-1) = 4
    zi(atetra + 25-1) = 0
    zi(atetra + 26-1) = 0
    zi(atetra + 27-1) = 0
    zi(atetra + 28-1) = 0
    zi(atetra + 29-1) = 0
    zi(atetra + 30-1) = 0
    zi(atetra + 31-1) = 0
    zi(atetra + 32-1) = 0
    zi(atetra + 33-1) = 4
    zi(atetra + 34-1) = 2
    zi(atetra + 35-1) = 3
    zi(atetra + 36-1) = 1
    zi(atetra + 37-1) = 0
    zi(atetra + 38-1) = 0
    zi(apenta + 1 -1) = 5
    zi(apenta + 2 -1) = 6
    zi(apenta + 3 -1) = 3
    zi(apenta + 4 -1) = 4
    zi(apenta + 5 -1) = 4
    zi(apenta + 6 -1) = 3
    zi(apenta + 7 -1) = 4
    zi(apenta + 8 -1) = 0
    zi(apenta + 9 -1) = 1
    zi(apenta + 10-1) = 1
    zi(apenta + 11-1) = 1
    zi(apenta + 12-1) = 4
    zi(apenta + 13-1) = 2
    zi(apenta + 14-1) = 0
    zi(apenta + 15-1) = 3
    zi(apenta + 16-1) = 4
    zi(apenta + 17-1) = 2
    zi(apenta + 18-1) = 5
    zi(apenta + 19-1) = 3
    zi(apenta + 20-1) = 0
    zi(apenta + 21-1) = 2
    zi(apenta + 22-1) = 6
    zi(apenta + 23-1) = 5
    zi(apenta + 24-1) = 6
    zi(apenta + 25-1) = 6
    zi(apenta + 26-1) = 0
    zi(apenta + 27-1) = 0
    zi(apenta + 28-1) = 3
    zi(apenta + 29-1) = 4
    zi(apenta + 30-1) = 0
    zi(apenta + 31-1) = 5
    zi(apenta + 32-1) = 0
    zi(apenta + 33-1) = 4
    zi(apenta + 34-1) = 2
    zi(apenta + 35-1) = 3
    zi(apenta + 36-1) = 1
    zi(apenta + 37-1) = 1
    zi(apenta + 38-1) = 0
    zi(ahexa + 1 -1) = 6
    zi(ahexa + 2 -1) = 8
    zi(ahexa + 3 -1) = 4
    zi(ahexa + 4 -1) = 4
    zi(ahexa + 5 -1) = 4
    zi(ahexa + 6 -1) = 4
    zi(ahexa + 7 -1) = 4
    zi(ahexa + 8 -1) = 4
    zi(ahexa + 9 -1) = 1
    zi(ahexa + 10-1) = 1
    zi(ahexa + 11-1) = 1
    zi(ahexa + 12-1) = 5
    zi(ahexa + 13-1) = 2
    zi(ahexa + 14-1) = 3
    zi(ahexa + 15-1) = 4
    zi(ahexa + 16-1) = 5
    zi(ahexa + 17-1) = 2
    zi(ahexa + 18-1) = 6
    zi(ahexa + 19-1) = 3
    zi(ahexa + 20-1) = 4
    zi(ahexa + 21-1) = 3
    zi(ahexa + 22-1) = 8
    zi(ahexa + 23-1) = 6
    zi(ahexa + 24-1) = 7
    zi(ahexa + 25-1) = 7
    zi(ahexa + 26-1) = 8
    zi(ahexa + 27-1) = 2
    zi(ahexa + 28-1) = 4
    zi(ahexa + 29-1) = 5
    zi(ahexa + 30-1) = 8
    zi(ahexa + 31-1) = 6
    zi(ahexa + 32-1) = 7
    zi(ahexa + 33-1) = 7
    zi(ahexa + 34-1) = 7
    zi(ahexa + 35-1) = 7
    zi(ahexa + 36-1) = 3
    zi(ahexa + 37-1) = 8
    zi(ahexa + 38-1) = 5
    call jedema()
end subroutine
