subroutine maxblo(nomob, xmax)
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
!
!***********************************************************************
!    P. RICHARD     DATE 13/10/92
!-----------------------------------------------------------------------
!  BUT:      < MAXIMUM D'UN BLOC DE REELS >
!
!   DTERMINER LE MAXIMUM DES TERMES D'UN BLOC
!
!-----------------------------------------------------------------------
!
! NOM----- / /:
!
! NOMOB    /I/: NOM K32 DE L'OBJET REEL
! XMAX     /M/: MAXIMUM REEL
!
!
!
!
!
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    character(len=*) :: nomob
    character(len=1) :: k1bid
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, llblo, nbterm
    real(kind=8) :: xmax
!-----------------------------------------------------------------------
    call jemarq()
    call jeveuo(nomob(1:32), 'L', llblo)
    call jelira(nomob(1:32), 'LONMAX', nbterm, k1bid)
!
    do 10 i = 1, nbterm
        xmax=max(xmax,abs(zr(llblo+i-1)))
10  end do
!
!
    call jedema()
end subroutine
