subroutine i2sens(chemin, nbrma2, limail, nbrma, connex,&
                  typmai)
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
    implicit none
    include 'asterfort/i2extf.h'
    integer :: nbrma, nbrma2
    integer :: chemin(nbrma2), limail(nbrma)
    character(len=*) :: connex, typmai
!
!-----------------------------------------------------------------------
    integer :: i, j, mi, mj, nid, nig, njd
    integer :: njg
!-----------------------------------------------------------------------
    i = chemin(1)
    mi = limail(i)
    chemin(1) = mi
    call i2extf(mi, 1, connex, typmai, nig,&
                nid)
    do 10 i = 2, nbrma
        j = chemin(i)
        mj = limail(j)
        call i2extf(mj, 1, connex, typmai, njg,&
                    njd)
!
        if (mi .gt. 0) then
            if (nid .eq. njd) then
                mj = -mj
            endif
!
        else if (mi.lt.0) then
            if (nig .eq. njd) then
                mj = -mj
            endif
!
        endif
!
        mi = mj
        nig = njg
        nid = njd
        chemin(i) = mi
10  end do
end subroutine
