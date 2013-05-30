subroutine dgmode(mode, imodel, ilong, nec, dg)
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
!          TROUVER LE DESCRIPTEUR-GRANDEUR ASSOCIE A UN MODE LOCAL
!          DE CARTE, CHAM_NO, OU CHAMELEM, SOUS FORME "IDEN"
!      ENTREE:
!          MODE  : NUMERO DE MODE LOCAL
!      SORTIE:
!          NEC   : NOMBRE D'ENTIERS-CODES
!          DG    : DESCRIPTEUR GRANDEUR DIMENSIONNE A NEC
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/nbec.h'
    integer :: dg(*), nec, mode, ilong, imodel
!
!-----------------------------------------------------------------------
    integer :: i, jmod, nbpt
!-----------------------------------------------------------------------
    call jemarq()
    nec = 0
    dg(1) = 0
!
    jmod = imodel+zi(ilong-1+mode)-1
    nec = nbec(zi(jmod-1+2))
    if (zi(jmod) .le. 3) then
        nbpt = zi(jmod-1+4)
        if (abs(nbpt) .lt. 10000) then
            do 1 i = 1, nec
                dg(i) = zi(jmod-1+4+i)
 1          continue
        endif
    endif
    call jedema()
end subroutine
