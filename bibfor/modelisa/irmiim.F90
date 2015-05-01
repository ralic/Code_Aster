subroutine irmiim(ifmis, ifreq, nfreq, nbno, tabrig)
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
    implicit none
#include "jeveux.h"
#include "asterfort/wkvect.h"
    integer :: ifmis, nbno, i1, i2, ifreq, nfreq
    character(len=24) :: tabrig
!
    character(len=72) :: texte
    real(kind=8) :: a(3)
!
!-----------------------------------------------------------------------
    integer :: i, j, jrig, nbmode, nsaut
!-----------------------------------------------------------------------
    nbmode = 3*nbno
    call wkvect(tabrig, 'V V R', nbmode*nbmode, jrig)
    rewind ifmis
    read(ifmis,'(A72)') texte
    if (texte(1:4) .eq. 'XXXX') goto 4
    do 1 i1 = 1, nbmode
        do 1 i2 = 1, nbmode
            nsaut = nfreq
            if (i1 .eq. 1 .and. i2 .eq. 1) nsaut = ifreq
            do 2 i = 1, nsaut
                read(ifmis,'(A72)') texte
 2          continue
            read(ifmis,*) (a(j),j=1,3)
            zr(jrig+(i2-1)*nbmode+i1-1) = a(2)
 1      continue
 4  continue
end subroutine
