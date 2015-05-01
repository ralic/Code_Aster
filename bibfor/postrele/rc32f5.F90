subroutine rc32f5(nbp12, nbp23, nbp13, nbsigr, nbsg1,&
                  nbsg2, nbsg3, saltij)
    implicit   none
    integer :: nbp12, nbp23, nbp13, nbsigr, nbsg1, nbsg2, nbsg3
    real(kind=8) :: saltij(*)
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!     SI IL N'EXISTE PAS DE SITUATION DE PASSAGE ENTRE 2 GROUPES,
!     ON MET LES TERMES CROISES DE SALT A ZERO
!
!     ------------------------------------------------------------------
    integer :: i1, i2, isl
!     ------------------------------------------------------------------
!
    if (nbp12 .eq. 0) then
!           BLOC 1_2
        do 100 i1 = 1, nbsg1
            isl = (i1-1)*nbsigr + nbsg1
            do 102 i2 = 1, nbsg2
                saltij(isl+i2) = 0.d0
102          continue
100      continue
!           BLOC 2_1
        do 104 i1 = 1, nbsg2
            isl = nbsigr*nbsg1 + (i1-1)*nbsigr
            do 106 i2 = 1, nbsg1
                saltij(isl+i2) = 0.d0
106          continue
104      continue
    endif
!
    if (nbp23 .eq. 0) then
!           BLOC 2_3
        do 110 i1 = 1, nbsg2
            isl = nbsigr*nbsg1 + (i1-1)*nbsigr + (nbsg1+nbsg2)
            do 112 i2 = 1, nbsg3
                saltij(isl+i2) = 0.d0
112          continue
110      continue
!           BLOC 3_2
        do 114 i1 = 1, nbsg3
            isl = nbsigr*(nbsg1+nbsg2) + (i1-1)*nbsigr + nbsg1
            do 116 i2 = 1, nbsg2
                saltij(isl+i2) = 0.d0
116          continue
114      continue
    endif
!
    if (nbp13 .eq. 0) then
!           BLOC 1_3
        do 120 i1 = 1, nbsg1
            isl = (i1-1)*nbsigr + (nbsg1+nbsg2)
            do 122 i2 = 1, nbsg3
                saltij(isl+i2) = 0.d0
122          continue
120      continue
!           BLOC 3_1
        do 124 i1 = 1, nbsg3
            isl = nbsigr*(nbsg1+nbsg2) + nbsigr*(i1-1)
            do 126 i2 = 1, nbsg1
                saltij(isl+i2) = 0.d0
126          continue
124      continue
    endif
!
end subroutine
