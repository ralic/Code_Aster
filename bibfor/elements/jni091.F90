subroutine jni091(elrefe, nmaxob, liobj, nbobj)
    implicit none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/wkvect.h'
    character(len=8) :: elrefe
! ----------------------------------------------------------------------
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
! person_in_charge: jacques.pellet at edf.fr
! ----------------------------------------------------------------------
!
    integer :: iret, npg1
    integer :: l, ll, i1, i
    integer :: mzr, nbobj, nmaxob
    real(kind=8) :: x3(3), xi3, ff(3)
    character(len=24) :: demr, liobj(nmaxob)
! DEB -----------------------------------------------------------------
!
    call assert(elrefe(1:6).eq.'THCOSE')
    demr = '&INEL.'//elrefe//'.DEMR'
!
    nbobj = 1
    call assert(nmaxob.gt.nbobj)
    liobj(1) = demr
!
    call jeexin(demr, iret)
    if (iret .ne. 0) goto 30
!
!
! --------- NPG1 POINTS POUR INTEGRER LES FONCTIONS D'INTERPOLATION
!           (EPAISSEUR)
    npg1 = 3
!
    x3(1) = -0.774596669241483D0
    x3(2) = 0.d0
    x3(3) = 0.774596669241483D0
!
! --------- 16 PLACES MEMOIRES RESERVEES AU CAS OU ON PREND 4 PTS DE
!             GAUSS (AVEC 3 PTS 9 PLACES AURAIENT SUFFI)
!
    call wkvect(demr, 'V V R', 16, mzr)
!
    do 20 i = 1, npg1
        xi3 = x3(i)
!
        ff(1) = 1 - xi3*xi3
        ff(2) = -xi3* (1-xi3)/2.d0
        ff(3) = xi3* (1+xi3)/2.d0
!
        ll = 3* (i-1)
        do 10 l = 1, 3
            i1 = ll + l
            zr(mzr-1+i1) = ff(l)
10      continue
20  end do
!
    zr(mzr-1+13) = 0.555555555555556D0
    zr(mzr-1+14) = 0.888888888888889D0
    zr(mzr-1+15) = 0.555555555555556D0
!
!
30  continue
!
end subroutine
