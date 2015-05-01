subroutine utcono(mcfac, mocle, iocc, nomail, ndim,&
                  coor, iret)
    implicit none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/utmess.h"
#include "asterfort/utnono.h"
!
    integer :: iocc, ndim, iret
    real(kind=8) :: coor(*)
    character(len=8) :: nomail
    character(len=*) :: mcfac, mocle(3)
! ----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!
! ----------------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: n1, n2, n3, numno, i, ier, jcoor
    integer :: vali(2)
    character(len=8) :: k8b, noeud
    character(len=16) :: concep, cmd
    character(len=24) :: coord, nomnoe, nomgrn
    character(len=24) :: valk(3)
!     ------------------------------------------------------------------
    call jemarq()
    iret = 0
!
    call getvr8(mcfac, mocle(1), iocc=iocc, nbval=0, nbret=n1)
    if (n1 .ne. 0) then
        call getvr8(mcfac, mocle(1), iocc=iocc, nbval=ndim, vect=coor,&
                    nbret=n1)
        if (n1 .lt. ndim) then
            call getres(k8b, concep, cmd)
            valk (1) = mcfac
            vali (1) = iocc
            call utmess('F+', 'MODELISA9_23', sk=valk(1), si=vali(1))
            if (ndim .eq. 2) then
                call utmess('F+', 'MODELISA9_24')
            else
                call utmess('F+', 'MODELISA9_25')
            endif
            vali (1) = abs(n1)
            vali (2) = ndim
            valk (1) = mocle(1)
            call utmess('F', 'MODELISA9_26', sk=valk(1), ni=2, vali=vali)
        endif
        iret = 1
        goto 9999
    endif
!
    coord = nomail//'.COORDO    .VALE'
    nomnoe = nomail//'.NOMNOE         '
    call jeveuo(coord, 'L', jcoor)
!
    call getvtx(mcfac, mocle(2), iocc=iocc, nbval=0, nbret=n2)
    if (n2 .ne. 0) then
        call getvtx(mcfac, mocle(2), iocc=iocc, scal=noeud, nbret=n2)
        call jenonu(jexnom(nomnoe, noeud), numno)
        if (numno .eq. 0) then
            call getres(k8b, concep, cmd)
            valk (1) = mcfac
            valk (2) = mocle(2)
            valk (3) = noeud
            vali (1) = iocc
            call utmess('F', 'MODELISA9_27', nk=3, valk=valk, si=vali(1))
        endif
        do 10 i = 1, ndim
            coor(i) = zr(jcoor+3*(numno-1)+i-1)
10      continue
        iret = 1
        goto 9999
    endif
!
    call getvtx(mcfac, mocle(3), iocc=iocc, scal=k8b, nbret=n3)
    if (n3 .ne. 0) then
        call getvtx(mcfac, mocle(3), iocc=iocc, scal=nomgrn, nbret=n3)
        call utnono(' ', nomail, 'NOEUD', nomgrn, k8b,&
                    ier)
        if (ier .eq. 10) then
            call getres(k8b, concep, cmd)
            valk (1) = mcfac
            valk (2) = mocle(3)
            valk (3) = nomgrn
            vali (1) = iocc
            call utmess('F', 'MODELISA9_28', nk=3, valk=valk, si=vali(1))
        else if (ier .eq. 1) then
            call getres(k8b, concep, cmd)
            valk (1) = mcfac
            valk (2) = k8b
            vali (1) = iocc
            call utmess('A', 'MODELISA9_29', nk=2, valk=valk, si=vali(1))
        endif
        call jenonu(jexnom(nomnoe, k8b), numno)
        do 20 i = 1, ndim
            coor(i) = zr(jcoor+3*(numno-1)+i-1)
20      continue
        iret = 1
        goto 9999
    endif
!
9999  continue
    call jedema()
end subroutine
