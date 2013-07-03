subroutine irgnte(ifi, nbordr, coord, connex, point,&
                  njvmai, nbmai, cnsv, partie, jtype,&
                  cnsd)
    implicit none
!
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    integer :: ifi, nbordr, connex(*), point(*), cnsv(*), cnsd(*), jtype
    real(kind=8) :: coord(*)
    character(len=*) :: njvmai, partie
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
!
!     IMPRESSION D'UN CHAM_NO AU FORMAT GMSH :
!
!     ------------------------------------------------------------------
!
    integer :: imai, ima, ipoin, listno(8), j, jcnsv, jcnsd, ncmp, jmai, ior
    integer :: inoe, nbno, nbmai
    real(kind=8) :: zero
!     ------------------------------------------------------------------
!
    call jemarq()
!
    zero = 0.0d0
!
    call jeveuo(njvmai, 'L', jmai)
    if (njvmai(10:12) .eq. 'POI') then
        nbno = 1
    else if (njvmai(10:12).eq.'SEG') then
        nbno = 2
    else if (njvmai(10:12).eq.'TRI') then
        nbno = 3
    else if (njvmai(10:12).eq.'QUA') then
        nbno = 4
    else if (njvmai(10:12).eq.'TET') then
        nbno = 4
    else if (njvmai(10:12).eq.'PYR') then
        nbno = 5
    else if (njvmai(10:12).eq.'PRI') then
        nbno = 6
    else if (njvmai(10:12).eq.'HEX') then
        nbno = 8
    endif
!
    do 10 imai = 1, nbmai
!
        ima = zi(jmai-1+imai)
!
        ipoin = point(ima)
!
        do 20 inoe = 1, nbno
!
            listno(inoe) = connex(ipoin + inoe - 1 )
!
20      continue
!
        do 12 j = 1, 3
            write(ifi,1000) (coord(3*(listno(inoe)-1)+j),inoe=1,nbno)
12      continue
!
        do 14 ior = 1, nbordr
!
            jcnsv = cnsv(ior)
            jcnsd = cnsd(ior)
            ncmp = zi(jcnsd-1+2)
            if (zk8(jtype-1+ior) .eq. 'R') then
!
                do 16 inoe = 1, nbno
!
                    if (njvmai(10:12) .eq. 'SEG' .or. njvmai(10:12) .eq. 'TRI' .or.&
                        njvmai(10:12) .eq. 'QUA') then
!
                        write(ifi,1000) zr(jcnsv-1+(listno(inoe)-1)*&
                        ncmp+1), zr(jcnsv-1+(listno(inoe)-1)*ncmp+4),&
                        zero, zr(jcnsv-1+(listno(inoe)-1)*ncmp+4),&
                        zr(jcnsv-1+(listno(inoe)-1)*ncmp+2), zero,&
                        zero ,zero ,zr(jcnsv-1+(listno(inoe)-1)*ncmp+&
                        3)
!
                        elseif (njvmai(10:12).eq.'PYR'.or. njvmai(10:12)&
                    .eq.'PRI'.or.njvmai(10:12).eq.'HEX') then
!
                        write(ifi,1000) zr(jcnsv-1+(listno(inoe)-1)*&
                        ncmp+1), zr(jcnsv-1+(listno(inoe)-1)*ncmp+4),&
                        zr(jcnsv-1+(listno(inoe)-1)*ncmp+5), zr(jcnsv-&
                        1+(listno(inoe)-1)*ncmp+4), zr(jcnsv-1+(&
                        listno(inoe)-1)*ncmp+2), zr(jcnsv-1+(listno(&
                        inoe)-1)*ncmp+6), zr(jcnsv-1+(listno(inoe)-1)*&
                        ncmp+5), zr(jcnsv-1+(listno(inoe)-1)*ncmp+6),&
                        zr(jcnsv-1+(listno(inoe)-1)*ncmp+3)
!
                    endif
!
16              continue
!
            else if (zk8(jtype-1+ior).eq.'C') then
!
                if (partie .eq. 'REEL') then
!
                    do 26 inoe = 1, nbno
!
                        if (njvmai(10:12) .eq. 'SEG' .or. njvmai(10:12) .eq. 'TRI' .or.&
                            njvmai(10:12) .eq. 'QUA') then
!
                            write(ifi,1000) zr(jcnsv-1+(listno(inoe)-&
                            1)*ncmp+1), zr(jcnsv-1+(listno(inoe)-1)*&
                            ncmp+4), zero, zr(jcnsv-1+(listno(inoe)-1)&
                            *ncmp+4), zr(jcnsv-1+(listno(inoe)-1)*&
                            ncmp+2), zero, zero ,zero ,zr(jcnsv-1+(&
                            listno(inoe)-1)*ncmp+3)
!
                            elseif (njvmai(10:12).eq.'PYR'.or. njvmai(10:&
                        12).eq.'PRI'.or.njvmai(10:12).eq.'HEX') then
!
                            write(ifi,1000) zr(jcnsv-1+(listno(inoe)-&
                            1)*ncmp+1), zr(jcnsv-1+(listno(inoe)-1)*&
                            ncmp+4), zr(jcnsv-1+(listno(inoe)-1)*ncmp+&
                            5), zr(jcnsv-1+(listno(inoe)-1)*ncmp+4),&
                            zr(jcnsv-1+(listno(inoe)-1)*ncmp+2),&
                            zr(jcnsv-1+(listno(inoe)-1)*ncmp+6),&
                            zr(jcnsv-1+(listno(inoe)-1)*ncmp+5),&
                            zr(jcnsv-1+(listno(inoe)-1)*ncmp+6),&
                            zr(jcnsv-1+(listno(inoe)-1)*ncmp+3)
!
                        endif
!
26                  continue
                else if (partie.eq.'IMAG') then
                    do 36 inoe = 1, nbno
!
                        if (njvmai(10:12) .eq. 'SEG' .or. njvmai(10:12) .eq. 'TRI' .or.&
                            njvmai(10:12) .eq. 'QUA') then
!
                            write(ifi,1000) zr(jcnsv-1+(listno(inoe)-&
                            1)*ncmp+1), zr(jcnsv-1+(listno(inoe)-1)*&
                            ncmp+4), zero, zr(jcnsv-1+(listno(inoe)-1)&
                            *ncmp+4), zr(jcnsv-1+(listno(inoe)-1)*&
                            ncmp+2), zero, zero ,zero ,zr(jcnsv-1+(&
                            listno(inoe)-1)*ncmp+3)
!
                            elseif (njvmai(10:12).eq.'PYR'.or. njvmai(10:&
                        12).eq.'PRI'.or.njvmai(10:12).eq.'HEX') then
!
                            write(ifi,1000) zr(jcnsv-1+(listno(inoe)-&
                            1)*ncmp+1), zr(jcnsv-1+(listno(inoe)-1)*&
                            ncmp+4), zr(jcnsv-1+(listno(inoe)-1)*ncmp+&
                            5), zr(jcnsv-1+(listno(inoe)-1)*ncmp+4),&
                            zr(jcnsv-1+(listno(inoe)-1)*ncmp+2),&
                            zr(jcnsv-1+(listno(inoe)-1)*ncmp+6),&
                            zr(jcnsv-1+(listno(inoe)-1)*ncmp+5),&
                            zr(jcnsv-1+(listno(inoe)-1)*ncmp+6),&
                            zr(jcnsv-1+(listno(inoe)-1)*ncmp+3)
                        endif
36                  continue
                endif
            endif
14      continue
10  end do
!
    call jelibe(njvmai)
!
    call jedema()
!
    1000 format(1p,9(e15.7e3,1x))
!
end subroutine
