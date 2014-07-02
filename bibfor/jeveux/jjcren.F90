subroutine jjcren(nomlu, icre, iret)
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
#include "asterf_types.h"
#include "jeveux_private.h"
#include "asterfort/jjarep.h"
#include "asterfort/jxhcod.h"
#include "asterfort/utmess.h"
    character(len=*) :: nomlu
    integer :: icre, iret
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, icla, iclasi, in, iref, isg, j
    integer :: jcara, jdate, jdocu, jgenr, jhcod, jiadd, jiadm
    integer :: jlong, jlono, jltyp, jluti, jmarq, jorig, jrnom
    integer :: jtype, lorep, lorepa, n, ne, nfic
!-----------------------------------------------------------------------
    parameter  ( n = 5 )
    common /jiatje/  jltyp(n), jlong(n), jdate(n), jiadd(n), jiadm(n),&
     &                 jlono(n), jhcod(n), jcara(n), jluti(n), jmarq(n)
    common /jkatje/  jgenr(n), jtype(n), jdocu(n), jorig(n), jrnom(n)
!     ------------------------------------------------------------------
    integer :: nrhcod, nremax, nreuti
    common /icodje/  nrhcod(n) , nremax(n) , nreuti(n)
    character(len=2) :: dn2
    character(len=5) :: classe
    character(len=8) :: nomfic, kstout, kstini
    common /kficje/  classe    , nomfic(n) , kstout(n) , kstini(n) ,&
     &                 dn2(n)
    integer :: nbcla
    common /nficje/  nbcla
    integer :: iclas, iclaos, iclaco, idatos, idatco, idatoc
    common /iatcje/  iclas ,iclaos , iclaco , idatos , idatco , idatoc
    character(len=24) :: nomco
    character(len=32) :: nomuti, nomos, nomoc, bl32
    common /nomcje/  nomuti , nomos , nomco , nomoc , bl32
!     ------------------------------------------------------------------
    character(len=32) :: clel, cle, d32, valk(3)
    aster_logical :: linser, rinser
    integer :: iclain, idatin, iin
    data             d32 /'$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'/
! DEB ------------------------------------------------------------------
    if (icre .ne. 0) then
        if (nomlu(1:1) .eq. ' ') then
            call utmess('F', 'JEVEUX_19')
        endif
    endif
500 continue
    iclasi = iclas
    iret = 0
    linser = .false.
    rinser = .false.
    nfic = nbcla
    lorepa = 0
    do 10 icla = 1, nfic
        if (classe(icla:icla) .ne. ' ') then
            lorep = nrhcod(icla)
            clel = nomlu
            if (lorep .ne. lorepa) then
                iref = jxhcod (clel,lorep)
                lorepa = lorep
            endif
            ne = 1
            i = iref
  5         continue
            if (hcod(jhcod(icla)+i) .eq. 0 .and. .not. rinser) then
                if (icre .eq. 1 .or. icre .eq. 2) then
                    if (icla .eq. iclas) then
                        if (nreuti(icla) .ge. nremax(icla)) then
                            call jjarep(icla, 2*nremax(icla))
                            goto 500
                        endif
                        linser = .true.
                        j = nreuti(icla) + 1
                        idatin = j
                        iclain = icla
                        iin = i
                        isg = 1
                    endif
                else
                    if (icla .eq. nfic) then
                        iret = 0
                    endif
                endif
            else
                j = hcod(jhcod(icla)+i)
                cle = '!'
                if (j .ne. 0) cle = rnom(jrnom(icla)+abs(j))
                if (cle .eq. clel) then
                    if (icre .eq. 1 .or. icre .eq. 2) then
                        valk(1)=clel
                        valk(2)=nomfic(icla)
                        call utmess('F', 'JEVEUX_10', nk=2, valk=valk)
                    else
                        if (icre .eq. -1 .or. icre .eq. -2) then
                            hcod(jhcod(icla) + i ) = -j
                            rnom(jrnom(icla) + j ) = '?'
                        endif
                        if (genr(jgenr(icla)+j) .ne. 'X') then
                            iclaos = icla
                            idatos = j
                            iret = 1
                        else
                            iclaco = icla
                            idatco = j
                            iret = 2
                        endif
                        goto 15
                    endif
                else
                    if (j .lt. 0 .and. .not. rinser) then
                        if (icre .eq. 1 .or. icre .eq. 2) then
                            if (icla .eq. iclas) then
                                linser = .true.
                                rinser = .true.
                                idatin = -j
                                iclain = icla
                                iin = i
                                isg = 0
                            endif
                        endif
                    endif
                    if (ne .eq. 1) in = jxhcod (clel,lorep-2)
                    ne = ne + 1
                    i = 1 + mod (i+in,lorep)
                    if (ne .le. lorep) then
                        j = hcod(jhcod(icla)+i)
                        if (j .eq. 0 .and. rinser) goto 10
                        goto 5
                    else
                        if (icre .eq. 1 .or. icre .eq. 2) then
                            if (icla .eq. iclas) then
                                call jjarep(icla, 2*nremax(icla))
                                lorep = nrhcod(icla)
                                goto 500
                            endif
                        else if (icla .eq. nfic) then
                            iret = 0
                        endif
                    endif
                endif
            endif
        endif
 10 end do
    if (linser) then
        iclas = iclain
        if (icre .eq. 1) then
            iclaos = iclain
            idatos = idatin
            iret = 1
        else if (icre .eq. 2) then
            iclaco = iclain
            idatco = idatin
            iret = 2
        endif
        nreuti(iclas) = nreuti(iclas) + isg
        hcod(jhcod(iclas)+iin) = idatin
        rnom(jrnom(iclas)+idatin) = nomlu
    endif
 15 continue
    if (iret .eq. 1) then
        nomos = nomlu
        if (iclasi .ne. iclaos) then
            nomco = d32
            nomoc = d32
        endif
    else if (iret .eq. 2) then
        nomco = nomlu
        nomoc = d32
        if (iclasi .ne. iclaco) then
            nomos = d32
        endif
    endif
! FIN ------------------------------------------------------------------
end subroutine
