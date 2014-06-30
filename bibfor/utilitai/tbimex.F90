subroutine tbimex(table, ifr, nparim, lipaim, formaz,&
                  formar)
! aslint: disable=W1303
    implicit none
#include "jeveux.h"
#include "asterfort/codent.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lxlgut.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    integer :: ifr, nparim
    character(len=*) :: table, lipaim(*), formaz, formar
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!      IMPRESSION D'UNE TABLE AU FORMAT "EXCEL" OU "AGRAF"
! ----------------------------------------------------------------------
! IN  : TABLE  : NOM D'UNE STRUCTURE "TABLE"
! IN  : IFR    : NUMERO D'UNITE LOGIQUE D'IMPRESSION
! IN  : NPARIM : NOMBRE DE PARAMETRES D'IMPRESSION
! IN  : LIPAIM : LISTE DES PARAMETRES D'IMPRESSION
! IN  : FORMAT : FORMAT D'IMPRESSION DE LA TABLE
! IN  : FORMAR : FORMAT D'IMPRESSION DES REELS
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
    integer ::  i, j, ipar, jvale, jlogq, ideb, ifin
    integer :: nblign,  ilon, ilm, id, if, ir, ilmp, iaj, nbpara, npara
    integer :: nparaf
    logical(kind=1) :: erreur
    character(len=1) :: bacs
    character(len=3) :: type
    character(len=4) :: chfin
    character(len=8) :: format, form1
    character(len=16) :: formr
    character(len=19) :: nomtab
    character(len=24) :: nomjv, nomjvl, inpar, knpar
    character(len=24) :: valk
    character(len=2000) :: chaine, chain2
    integer, pointer :: log_para(:) => null()
    integer, pointer :: nom_para(:) => null()
    integer, pointer :: val_para(:) => null()
    character(len=24), pointer :: tblp(:) => null()
    integer, pointer :: tbnp(:) => null()
!     ------------------------------------------------------------------
!
    call jemarq()
!
    nomtab = table
    format = formaz
    bacs = char(92)
!
    ilon = lxlgut( formar )
    formr = '('//formar(1:ilon)//')'
    id = 0
    if = 0
    do 2 i = 1, ilon-1
        if (formar(i:i) .eq. 'D' .or. formar(i:i) .eq. 'E' .or. formar(i:i) .eq. 'F' .or.&
            formar(i:i) .eq. 'G') then
            id = i+1
        else if (formar(i:i) .eq. '.') then
            if = i-1
        endif
 2  end do
    if (id .eq. if .and. id .ne. 0) then
        read(formar(id:if),'(I1)') ir
    else if (id+1 .eq. if) then
        read(formar(id:if),'(I2)') ir
    else
        ir = 12
    endif
!
    call jeveuo(nomtab//'.TBNP', 'L', vi=tbnp)
    nbpara = tbnp(1)
    nblign = tbnp(2)
!
    call jeveuo(nomtab//'.TBLP', 'L', vk24=tblp)
!
!     --- ON RECHERCHE LA LONGUEUR LA PLUS LONGUE ---
!     --- ON STOCKE LES POINTEURS POUR NE PLUS FAIRE DE JEVEUO ---
!
    AS_ALLOCATE(vi=nom_para, size=nparim)
    AS_ALLOCATE(vi=val_para, size=nparim)
    AS_ALLOCATE(vi=log_para, size=nparim)
    erreur = .false.
    npara = 0
    ilmp = 0
    do 10 i = 1, nparim
        inpar = lipaim(i)
        do 12 j = 1, nbpara
            knpar = tblp(1+4*(j-1) )
            nomjv = tblp(1+4*(j-1)+2)
            nomjvl = tblp(1+4*(j-1)+3)
            if (inpar .eq. knpar) then
                npara = npara + 1
                nom_para(npara) = j
                call jeveuo(nomjv, 'L', val_para(npara))
                call jeveuo(nomjvl, 'L', log_para(npara))
                ilon = lxlgut( inpar )
                ilmp = max ( ilon , ilmp )
                goto 10
            endif
12      continue
        erreur = .true.
        valk = inpar
        call utmess('A', 'UTILITAI6_89', sk=valk)
10  end do
    if (erreur) then
        call utmess('F', 'PREPOST_60')
    endif
!
    nparaf = npara
    chaine = ' '
    chain2 = ' '
    ideb = 2
    do 20 i = 1, npara
        ipar = nom_para(i)
        type = tblp(1+4*(ipar-1)+1)
        ilon = lxlgut( tblp(1+4*(ipar-1)) )
        if (type(1:3) .eq. 'K80') then
            iaj = ( 80 - ilon ) / 2
            ifin = ideb + 80 - 1
            if (ifin .gt. 1999) then
                ifin = ideb
                nparaf = i - 1
                goto 22
            endif
            chaine(ideb+iaj:ifin) = tblp(1+4*(ipar-1))
            chain2(ideb+iaj:ifin) = type
            if (format .eq. 'AGRAF') ifin = ifin + 1
        else if (type(1:1) .eq. 'I') then
            ilm = max ( 12 , ilmp )
            iaj = ( ilm - ilon ) / 2
            ifin = ideb + ilm - 1
            if (ifin .gt. 1999) then
                ifin = ideb
                nparaf = i - 1
                goto 22
            endif
            chaine(ideb+iaj:ifin) = tblp(1+4*(ipar-1))
            chain2(ideb+iaj:ifin) = type
        else if (type(1:1) .eq. 'R') then
            ilm = max ( ir , ilmp )
            iaj = ( ilm - ilon ) / 2
            ifin = ideb + ilm - 1
            if (ifin .gt. 1999) then
                ifin = ideb
                nparaf = i - 1
                goto 22
            endif
            chaine(ideb+iaj:ifin) = tblp(1+4*(ipar-1))
            chain2(ideb+iaj:ifin) = type
        else if (type(1:1) .eq. 'C') then
            ilm = 2 * ir + 1
            ilm = max ( ilm , ilmp )
            iaj = ( ilm - ilon ) / 2
            ifin = ideb + ilm - 1
            if (ifin .gt. 1999) then
                ifin = ideb
                nparaf = i - 1
                goto 22
            endif
            chaine(ideb+iaj+8:ifin) = tblp(1+4*(ipar-1))
            chain2(ideb+iaj:ifin) = type
        else if (type(1:2) .eq. 'K8') then
            ilm = max ( 8 , ilmp )
            iaj = ( ilm - ilon ) / 2
            ifin = ideb + ilm - 1
            if (ifin .gt. 1999) then
                ifin = ideb
                nparaf = i - 1
                goto 22
            endif
            chaine(ideb+iaj:ifin) = tblp(1+4*(ipar-1))
            chain2(ideb+iaj:ifin) = type
            if (format .eq. 'AGRAF') ifin = ifin + 1
        else if (type(1:3) .eq. 'K16') then
            ilm = max ( 16 , ilmp )
            iaj = ( ilm - ilon ) / 2
            ifin = ideb + ilm - 1
            if (ifin .gt. 1999) then
                ifin = ideb
                nparaf = i - 1
                goto 22
            endif
            chaine(ideb+iaj:ifin) = tblp(1+4*(ipar-1))
            chain2(ideb+iaj:ifin) = type
            if (format .eq. 'AGRAF') ifin = ifin + 1
        else if (type(1:3) .eq. 'K24') then
            iaj = ( 24 - ilon ) / 2
            ifin = ideb + 24 - 1
            if (ifin .gt. 1999) then
                ifin = ideb
                nparaf = i - 1
                goto 22
            endif
            chaine(ideb+iaj:ifin) = tblp(1+4*(ipar-1))
            chain2(ideb+iaj:ifin) = type
            if (format .eq. 'AGRAF') ifin = ifin + 1
        else if (type(1:3) .eq. 'K32') then
            iaj = ( 32 - ilon ) / 2
            ifin = ideb + 32 - 1
            if (ifin .gt. 1999) then
                ifin = ideb
                nparaf = i - 1
                goto 22
            endif
            chaine(ideb+iaj:ifin) = tblp(1+4*(ipar-1))
            chain2(ideb+iaj:ifin) = type
            if (format .eq. 'AGRAF') ifin = ifin + 1
        endif
!
        ideb = ifin + 2
20  end do
22  continue
    if (nparaf .ne. npara) then
        call utmess('A', 'UTILITAI4_84')
    endif
    call codent(ifin, 'G', chfin)
    form1 = '(A'//chfin//')'
    write(ifr,form1) chaine(1:ifin)
!
    if (format .eq. 'ASTER') then
        write(ifr,form1) chain2(1:ifin)
    endif
!
    do 30 i = 1, nblign
        chaine = ' '
        ideb = 2
        do 32 j = 1, nparaf
            ipar = nom_para(j)
            type = tblp(1+4*(ipar-1)+1)
            jvale = val_para(j)
            jlogq = log_para(j)
            if (zi(jlogq+i-1) .eq. 1) then
                if (type(1:1) .eq. 'I') then
                    ilm = max ( ilmp , 12 )
                    ifin = ideb + ilm - 1
                    write(chaine(ideb:ifin),'(I12)') zi(jvale+i-1)
                    ideb = ifin + 2
                else if (type(1:1) .eq. 'R') then
                    ilm = max ( ilmp , ir )
                    ifin = ideb + ilm - 1
                    write(chaine(ideb:ifin),formr) zr(jvale+i-1)
                    ideb = ifin + 2
                else if (type(1:1) .eq. 'C') then
                    ilm = 2 * ir + 1
                    ilm = max ( ilm , ilmp ) / 2
                    ifin = ideb + ilm - 1
                    write(chaine(ideb:ifin),formr) zc(jvale+i-1)
                    ifin = ideb + ilm - 1
                    write(chaine(ideb:ifin),formr) zc(jvale+i-1)
                    ideb = ifin + 2
                else if (type(1:3) .eq. 'K80') then
                    if (format .eq. 'AGRAF') then
                        chaine(ideb:ideb) = bacs
                        ideb = ideb + 1
                    endif
                    ifin = ideb + 80 - 1
                    chaine(ideb:ifin) = zk80(jvale+i-1)
                    ideb = ifin + 2
                else if (type(1:3) .eq. 'K32') then
                    if (format .eq. 'AGRAF') then
                        chaine(ideb:ideb) = bacs
                        ideb = ideb + 1
                    endif
                    ifin = ideb + 32 - 1
                    chaine(ideb:ifin) = zk32(jvale+i-1)
                    ideb = ifin + 2
                else if (type(1:3) .eq. 'K24') then
                    if (format .eq. 'AGRAF') then
                        chaine(ideb:ideb) = bacs
                        ideb = ideb + 1
                    endif
                    ifin = ideb + 24 - 1
                    chaine(ideb:ifin) = zk24(jvale+i-1)
                    ideb = ifin + 2
                else if (type(1:3) .eq. 'K16') then
                    if (format .eq. 'AGRAF') then
                        chaine(ideb:ideb) = bacs
                        ideb = ideb + 1
                    endif
                    ilm = max ( ilmp , 16 )
                    ifin = ideb + ilm - 1
                    chaine(ideb:ifin) = zk16(jvale+i-1)
                    ideb = ifin + 2
                else if (type(1:2) .eq. 'K8') then
                    if (format .eq. 'AGRAF') then
                        chaine(ideb:ideb) = bacs
                        ideb = ideb + 1
                    endif
                    ilm = max ( ilmp , 8 )
                    ifin = ideb + ilm - 1
                    chaine(ideb:ifin) = zk8(jvale+i-1)
                    ideb = ifin + 2
                endif
            else
                if (type(1:3) .eq. 'K80') then
                    ifin = ideb + 80 - 1
                    ideb = ideb + 39
                else if (type(1:1) .eq. 'I') then
                    ilm = max ( ilmp , 12 )
                    ifin = ideb + ilm - 1
                    ideb = ideb + 5
                else if (type(1:1) .eq. 'R') then
                    ilm = max ( ilmp , ir )
                    ifin = ideb + ilm - 1
                    ideb = ideb + 5
                else if (type(1:1) .eq. 'C') then
                    ifin = ideb + 25 - 1
                    ideb = ideb + 11
                else if (type(1:2) .eq. 'K8') then
                    ilm = max ( ilmp , 8 )
                    ifin = ideb + ilm - 1
                    ideb = ideb + 5
                else if (type(1:3) .eq. 'K16') then
                    ilm = max ( ilmp , 16 )
                    ifin = ideb + ilm - 1
                    ideb = ideb + 7
                else if (type(1:3) .eq. 'K24') then
                    ifin = ideb + 24 - 1
                    ideb = ideb + 11
                else if (type(1:3) .eq. 'K32') then
                    ifin = ideb + 32 - 1
                    ideb = ideb + 15
                endif
                if (format .eq. 'AGRAF') then
                    ideb = ideb - 1
                    chaine(ideb:ideb+1) = bacs//'-'
                else
                    chaine(ideb:ideb) = '-'
                endif
                ideb = ifin + 2
            endif
32      continue
        call codent(ifin, 'G', chfin)
        form1 = '(A'//chfin//')'
        write(ifr,form1) chaine(1:ifin)
30  end do
!
    AS_DEALLOCATE(vi=nom_para)
    AS_DEALLOCATE(vi=val_para)
    AS_DEALLOCATE(vi=log_para)
!
    call jedema()
!
!
end subroutine
