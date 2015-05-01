subroutine i3imas(epsi, nil, tete, queue, succ,&
                  prec, desc, desctm, sgt, conex,&
                  vlc, coordo, sdrp1d, sdrpom, nbsgte)
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/i3crk3.h"
#include "asterfort/i3fmvn.h"
#include "asterfort/i3idks.h"
#include "asterfort/i3lchs.h"
#include "asterfort/i3pdm3.h"
#include "asterfort/i3trip.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jeveut.h"
#include "asterfort/jexnum.h"
#include "asterfort/juveca.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: nil, tete, queue, succ(*), prec(*), desc(*), desctm(*)
    integer :: conex(*), vlc(*), nbsgte
    real(kind=8) :: coordo(*), sgt(*), epsi
    character(len=24) :: sdrp1d, sdrpom
!
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!     INTERSECTION MAILLAGE SEGMENT
!     CALCUL DES SD REPERAGE_1D ET REPERAGE_OMEGA
!     ------------------------------------------------------------------
! IN  EPSI   : R : PRECISION
! IN  NIL    : I : POINTEUR A  NIL                 -+
! IN  TETE   : I : POINTEUR DE TETE                 !
! IN  QUEUE  : I : POINTEUR DE QUEUE                !--> SD L_MAILLE
! IN  SUCC   : I : POINTEUR SUR LES SUCCESSEURS     !
! IN  PREC   : I : POINTEUR SUR LES PREDECESSEURS   !
! IN  DESC   : I : CHAMP INFO = POINTEUR T_DESCTM  -+
! IN  DESCTM : I : POINTEUR SUR T_TYPE_MAILLE_3D
! IN  SGT    : R : COORDONNES DES EXTREMITEES DU SGT
! IN  CONEX  : I : TABLE DE CONNECTIVITE
! IN  VLC    : I : VECTEUR DE LONGUEURS CUMULEES SUR LA CONNECTIVITE
! IN  COORDO : R : TABLE DES COORDONEES
! OUT SDRP1D : K : PREFIXE DE LA SD REPERAGE_1D INTERMEDIARE
! OUT SDRPOM : K : PREFIXE DE LA SD REPERAGE_OM INTERMEDIARE
! OUT NBSGTE : K : NOMBRE DE SEGMENTS ELEMENTAIRES TROUVES
!     ------------------------------------------------------------------
!     L_MAILLE SUPPOSEE NON VIDE IE : TETE <> NIL
!     ------------------------------------------------------------------
!
!
    character(len=24) :: r1d1, r1d2, r1d3
    character(len=24) :: rom1, rom2, rom3, rom4, rom5, rom6, rom7, rom8, rom9
    integer :: mxsgel, ptsgel, maille, alstpt, nbsgel, iret, nbpt
    integer :: a1, a2, f1, f2, tf1, tf2, i1, i2, i, j, lnd(4), nbnd, lma(20)
    integer :: arom1, arom2, arom3, arom4, arom5, arom6, arom7, arom8
    integer :: ar1d1, ar1d2, ar1d3, n1, nbma, arom9
    real(kind=8) :: zero, un, t1, t2
    aster_logical :: fini, find, atrv, btrv, adansm, bdansm
!
!======================================================================
!
!CC   MXSGEL =  100
!-----------------------------------------------------------------------
    integer :: j1, j2
!-----------------------------------------------------------------------
    call jemarq()
    mxsgel = 10
    ptsgel = 0
    un = 1.0d0
    zero = 0.0d0
    fini = .false.
    find = .false.
    atrv = .false.
    btrv = .false.
    adansm = .false.
    bdansm = .false.
    call wkvect('&&I3IMAS.LISTE.POINT', 'V V I', 6, alstpt)
    call jecreo('&&I3IMAS.LSTPT.ABSC.SGT ', 'V V R')
    call jeecra('&&I3IMAS.LSTPT.ABSC.SGT ', 'LONMAX', 14)
    call jeecra('&&I3IMAS.LSTPT.ABSC.SGT ', 'LONUTI', 14)
    call jeveut('&&I3IMAS.LSTPT.ABSC.SGT ', 'E', zi(alstpt+1-1))
    call jecreo('&&I3IMAS.LSTPT.FACE     ', 'V V I')
    call jeecra('&&I3IMAS.LSTPT.FACE     ', 'LONMAX', 14)
    call jeecra('&&I3IMAS.LSTPT.FACE     ', 'LONUTI', 14)
    call jeveut('&&I3IMAS.LSTPT.FACE     ', 'E', zi(alstpt+2-1))
    call jecreo('&&I3IMAS.LSTPT.ARETE    ', 'V V I')
    call jeecra('&&I3IMAS.LSTPT.ARETE    ', 'LONMAX', 14)
    call jeecra('&&I3IMAS.LSTPT.ARETE    ', 'LONUTI', 14)
    call jeveut('&&I3IMAS.LSTPT.ARETE    ', 'E', zi(alstpt+3-1))
    call jecreo('&&I3IMAS.LSTPT.TYPE.FACE', 'V V I')
    call jeecra('&&I3IMAS.LSTPT.TYPE.FACE', 'LONMAX', 14)
    call jeecra('&&I3IMAS.LSTPT.TYPE.FACE', 'LONUTI', 14)
    call jeveut('&&I3IMAS.LSTPT.TYPE.FACE', 'E', zi(alstpt+4-1))
    call jecreo('&&I3IMAS.LSTPT.COORD.REF', 'V V R')
    call jeecra('&&I3IMAS.LSTPT.COORD.REF', 'LONMAX', 28)
    call jeecra('&&I3IMAS.LSTPT.COORD.REF', 'LONUTI', 28)
    call jeveut('&&I3IMAS.LSTPT.COORD.REF', 'E', zi(alstpt+5-1))
    call jecreo('&&I3IMAS.LSTPT.ORDRE    ', 'V V I')
    call jeecra('&&I3IMAS.LSTPT.ORDRE    ', 'LONMAX', 14)
    call jeecra('&&I3IMAS.LSTPT.ORDRE    ', 'LONUTI', 14)
    call jeveut('&&I3IMAS.LSTPT.ORDRE    ', 'E', zi(alstpt+6-1))
    r1d1 = sdrp1d(1:13)//'.SGTEL.ORIG'
    r1d2 = sdrp1d(1:13)//'.SGTEL.EXTR'
    r1d3 = sdrp1d(1:13)//'.SGTEL.TYPE'
    call jeexin(r1d1, iret)
    if (iret .eq. 0) then
        call wkvect(r1d1, 'V V R', mxsgel, ar1d1)
        call wkvect(r1d2, 'V V R', mxsgel, ar1d2)
        call wkvect(r1d3, 'V V I', mxsgel, ar1d3)
    else
        call jeveuo(r1d1, 'E', ar1d1)
        call jeveuo(r1d2, 'E', ar1d2)
        call jeveuo(r1d3, 'E', ar1d3)
    endif
    rom1 = sdrpom(1:13)//'.MAIL      '
    rom2 = sdrpom(1:13)//'.FACE .ORIG'
    rom3 = sdrpom(1:13)//'.FACE .EXTR'
    rom4 = sdrpom(1:13)//'.CREFM.ORIG'
    rom5 = sdrpom(1:13)//'.CREFM.EXTR'
    rom6 = sdrpom(1:13)//'.ARETE.ORIG'
    rom7 = sdrpom(1:13)//'.ARETE.EXTR'
    rom8 = sdrpom(1:13)//'.CREFF.ORIG'
    rom9 = sdrpom(1:13)//'.CREFF.EXTR'
    call jeexin(rom2, iret)
    if (iret .eq. 0) then
        call wkvect(rom2, 'V V I', mxsgel, arom2)
        call wkvect(rom3, 'V V I', mxsgel, arom3)
        call wkvect(rom4, 'V V R', 3*mxsgel, arom4)
        call wkvect(rom5, 'V V R', 3*mxsgel, arom5)
        call wkvect(rom6, 'V V I', mxsgel, arom6)
        call wkvect(rom7, 'V V I', mxsgel, arom7)
        call wkvect(rom8, 'V V R', 2*mxsgel, arom8)
        call wkvect(rom9, 'V V R', 2*mxsgel, arom9)
    else
        call jeveuo(rom2, 'E', arom2)
        call jeveuo(rom3, 'E', arom3)
        call jeveuo(rom4, 'E', arom4)
        call jeveuo(rom5, 'E', arom5)
        call jeveuo(rom6, 'E', arom6)
        call jeveuo(rom7, 'E', arom7)
        call jeveuo(rom8, 'E', arom8)
        call jeveuo(rom9, 'E', arom9)
        call jelira(rom1, 'NMAXOC', mxsgel)
        call jedetr(rom1)
    endif
    call jecrec(rom1, 'V V I', 'NU', 'DISPERSE', 'VARIABLE',&
                mxsgel)
    fini = ( (tete .eq. nil) .or. fini .or. find )
 10 continue
    if (.not. fini) then
        maille = tete
        call i3idks(epsi, maille, desc, desctm, sgt,&
                    conex(vlc(maille)), coordo, nbpt, zi(alstpt))
        if (abs(nbpt) .gt. 0) then
            call i3trip(zi(alstpt), abs(nbpt))
            j1 = zi(zi(alstpt+6-1)+1-1)
            j2 = zi(zi(alstpt+6-1)+abs(nbpt)-1)
            t1 = zr(zi(alstpt+1-1)+j1-1)
            t2 = zr(zi(alstpt+1-1)+j2-1)
            adansm = ( abs(t1) .le. epsi )
            bdansm = ( abs(t2-un) .le. epsi )
            atrv = ( adansm .or. atrv )
            btrv = ( bdansm .or. btrv )
        endif
        nbsgel = max(0,abs(nbpt)-1)
        if (nbpt .eq. 0) then
            if ((.not. atrv) .and. (.not. btrv)) then
                call i3pdm3(epsi, maille, desc, desctm, conex(vlc(maille) ),&
                            coordo, sgt, adansm)
                atrv = adansm
                if (adansm) then
                    call i3pdm3(epsi, maille, desc, desctm, conex(vlc( maille)),&
                                coordo, sgt(4), bdansm)
                    btrv = bdansm
                    if (bdansm) then
                        find = .true.
                        nbsgel = 1
                        nbpt = 2
                        zr(zi(alstpt + 1-1) + 1-1) = zero
                        zi(zi(alstpt + 2-1) + 1-1) = 0
                        zi(zi(alstpt + 3-1) + 1-1) = 0
                        zi(zi(alstpt + 4-1) + 1-1) = 0
                        zr(zi(alstpt + 5-1) + 1-1) = zero
                        zr(zi(alstpt + 5-1) + 2-1) = zero
                        zi(zi(alstpt + 6-1) + 1-1) = 1
                        zr(zi(alstpt + 1-1) + 2-1) = un
                        zi(zi(alstpt + 2-1) + 2-1) = 0
                        zi(zi(alstpt + 3-1) + 2-1) = 0
                        zi(zi(alstpt + 4-1) + 2-1) = 0
                        zr(zi(alstpt + 5-1) + 3-1) = zero
                        zr(zi(alstpt + 5-1) + 4-1) = zero
                        zi(zi(alstpt + 6-1) + 2-1) = 2
                    else
                        btrv = .false.
                        atrv = .false.
                    endif
                endif
            endif
        endif
        if (nbpt .gt. 0) then
            if ((.not. adansm) .and. (.not. atrv)) then
                call i3pdm3(epsi, maille, desc, desctm, conex(vlc(maille) ),&
                            coordo, sgt, adansm)
                atrv = ( adansm .or. atrv )
                if (adansm) then
                    zr(zi(alstpt + 1-1) + nbpt+1-1) = zero
                    zi(zi(alstpt + 2-1) + nbpt+1-1) = 0
                    zi(zi(alstpt + 3-1) + nbpt+1-1) = 0
                    zi(zi(alstpt + 4-1) + nbpt+1-1) = 0
                    zr(zi(alstpt + 5-1) + 2*nbpt+1-1) = zero
                    zr(zi(alstpt + 5-1) + 2*nbpt+2-1) = zero
                    zi(zi(alstpt + 6-1) + nbpt+1-1) = nbpt + 1
                    nbsgel = nbsgel + 1
                    nbpt = nbpt + 1
                endif
            endif
            if ((.not. bdansm) .and. (.not. btrv)) then
                call i3pdm3(epsi, maille, desc, desctm, conex(vlc(maille) ),&
                            coordo, sgt(4), bdansm)
                btrv = ( bdansm .or. btrv )
                if (bdansm) then
                    zr(zi(alstpt + 1-1) + nbpt+1-1) = un
                    zi(zi(alstpt + 2-1) + nbpt+1-1) = 0
                    zi(zi(alstpt + 3-1) + nbpt+1-1) = 0
                    zi(zi(alstpt + 4-1) + nbpt+1-1) = 0
                    zr(zi(alstpt + 5-1) + 2*nbpt+1-1) = zero
                    zr(zi(alstpt + 5-1) + 2*nbpt+2-1) = zero
                    zi(zi(alstpt + 6-1) + nbpt+1-1) = nbpt + 2
                    nbsgel = nbsgel + 1
                    nbpt = nbpt + 1
                endif
            endif
        endif
        call i3trip(zi(alstpt), abs(nbpt))
        if ((ptsgel + nbsgel) .gt. mxsgel) then
            call jecrec('&&I3IMAS.XD.TMP', 'V V I', 'NU', 'DISPERSE', 'VARIABLE',&
                        mxsgel)
!CC         MXSGEL = MXSGEL + 100
            mxsgel = mxsgel + 10
            call juveca(r1d1, mxsgel)
            call jeveuo(r1d1, 'E', ar1d1)
            call juveca(r1d2, mxsgel)
            call jeveuo(r1d2, 'E', ar1d2)
            call juveca(r1d3, mxsgel)
            call jeveuo(r1d3, 'E', ar1d3)
            call juveca(rom2, mxsgel)
            call jeveuo(rom2, 'E', arom2)
            call juveca(rom3, mxsgel)
            call jeveuo(rom3, 'E', arom3)
            call juveca(rom4, 3*mxsgel)
            call jeveuo(rom4, 'E', arom4)
            call juveca(rom5, 3*mxsgel)
            call jeveuo(rom5, 'E', arom5)
            call juveca(rom6, mxsgel)
            call jeveuo(rom6, 'E', arom6)
            call juveca(rom7, mxsgel)
            call jeveuo(rom7, 'E', arom7)
            call juveca(rom8, 2*mxsgel)
            call jeveuo(rom8, 'E', arom8)
            call juveca(rom9, 2*mxsgel)
            call jeveuo(rom9, 'E', arom9)
            do 20 i = 1, ptsgel, 1
                call jelira(jexnum(rom1, i), 'LONMAX', n1)
                call jeveuo(jexnum(rom1, i), 'L', i2)
                call jecroc(jexnum('&&I3IMAS.XD.TMP', i))
                call jeecra(jexnum('&&I3IMAS.XD.TMP', i), 'LONMAX', n1)
                call jeveuo(jexnum('&&I3IMAS.XD.TMP', i), 'E', i1)
                do 21 j = 1, n1, 1
                    zi(i1 + j-1) = zi(i2 + j-1)
 21             continue
 20         continue
            call jedetr(rom1)
            call jecrec(rom1, 'V V I', 'NU', 'DISPERSE', 'VARIABLE',&
                        mxsgel)
            do 30 i = 1, ptsgel, 1
                call jelira(jexnum('&&I3IMAS.XD.TMP', i), 'LONMAX', n1)
                call jeveuo(jexnum('&&I3IMAS.XD.TMP', i), 'L', i2)
                call jecroc(jexnum(rom1, i))
                call jeecra(jexnum(rom1, i), 'LONMAX', n1)
                call jeveuo(jexnum(rom1, i), 'E', i1)
                do 31 j = 1, n1, 1
                    zi(i1 + j-1) = zi(i2 + j-1)
 31             continue
 30         continue
            call jedetr('&&I3IMAS.XD.TMP')
        endif
        do 100 i = 1, nbsgel, 1
            j1 = zi(zi(alstpt+6-1)+i-1)
            j2 = zi(zi(alstpt+6-1)+i )
            t1 = zr(zi(alstpt+1-1)+j1-1)
            t2 = zr(zi(alstpt+1-1)+j2-1)
            f1 = zi(zi(alstpt+2-1)+j1-1)
            f2 = zi(zi(alstpt+2-1)+j2-1)
!
! SI UN NUMERO DE FACE EST NUL ALORS ON NE TRAITE PAS
! LE SEGMENT ELEMENTAIRE (ON NE TRAITE QUE LES SEGMENTS
! REPERABLES PAR DES NUMEROS DE FACE NON NULS)
! ON EMET UNE ALARME DANS CE CAS
            if (f1 .eq. 0) then
                call utmess('A', 'PREPOST6_44')
                goto 100
            endif
            if (f2 .eq. 0) then
                call utmess('A', 'PREPOST6_45')
                goto 100
            endif
!
            a1 = zi(zi(alstpt+3-1)+j1-1)
            a2 = zi(zi(alstpt+3-1)+j2-1)
            tf1 = zi(zi(alstpt+4-1)+j1-1)
            tf2 = zi(zi(alstpt+4-1)+j2-1)
            if ((f1 .eq. f2) .and. (f1 .gt. 0)) then
                n1 = desctm(desc(maille))
                nbnd = zi(n1-1 + 2 + f1)
                if ((a1 .eq. a2) .and. (a1 .gt. 0)) then
                    i1 = zi(n1-1 + 8+f1+(a1-1)*6)
                    i2 = max(1,mod(a1+1,nbnd+1))
                    lnd(1) = conex(vlc(maille)+i1-1)
                    i1 = zi(n1-1 + 8+f1+(i2-1)*6)
                    lnd(2) = conex(vlc(maille)+i1-1)
                    nbnd = 2
                    i1 =-1
                    i2 = 1
                else
                    do 110 j = 1, nbnd, 1
                        i1 = zi(n1-1 + 8+f1+(j-1)*6)
                        lnd(j) = conex(vlc(maille)-1+i1)
110                 continue
                    i1 = 2
                    i2 = 2
                endif
                call i3fmvn(nil, desc, succ, prec, desctm,&
                            maille, conex, vlc, lnd, nbnd,&
                            i1, nbma, lma)
                if ((tf1 .eq. -1) .and. ( tf2 .eq. -1)) then
                    if (nbma .le. 1) then
                        i2 = 0
                    else
                        i2 = 3
                        nbma = 1
                        lma(1) = lma(2)
                    endif
                endif
            else
                i2 = 3
                lma(1) = maille
                nbma = 1
            endif
            zr(ar1d1 + ptsgel ) = t1
            zr(ar1d2 + ptsgel ) = t2
            zi(ar1d3 + ptsgel ) = i2
            zi(arom2 + ptsgel ) = f1
            zi(arom3 + ptsgel ) = f2
            zi(arom6 + ptsgel ) = a1
            zi(arom7 + ptsgel ) = a2
            zr(arom8 + 2* ptsgel ) = zr(zi(alstpt+5-1)+2*(j1-1))
            zr(arom8 + 2* ptsgel +1) = zr(zi(alstpt+5-1)+2*(j1-1)+1)
            zr(arom9 + 2* ptsgel ) = zr(zi(alstpt+5-1)+2*(j2-1))
            zr(arom9 + 2* ptsgel +1) = zr(zi(alstpt+5-1)+2*(j2-1)+1)
            call i3crk3(desc(maille), f1, zr(zi(alstpt+5-1)+2*(j1-1)), zr(arom4 + 3*ptsgel))
            call i3crk3(desc(maille), f2, zr(zi(alstpt+5-1)+2*(j2-1)), zr(arom5 + 3*ptsgel))
            call jecroc(jexnum(rom1, ptsgel+1))
            call jeecra(jexnum(rom1, ptsgel+1), 'LONMAX', nbma)
            call jeveuo(jexnum(rom1, ptsgel+1), 'E', arom1)
            do 120 j = 1, nbma, 1
                zi(arom1 + j-1) = lma(j)
120         continue
            ptsgel = ptsgel + 1
            call i3lchs(nil, tete, queue, succ, prec,&
                        lma(2), nbma-1)
100     continue
        lma(1) = maille
        nbma = 1
        call i3lchs(nil, tete, queue, succ, prec,&
                    lma, nbma)
        fini = ( (tete .eq. nil) .or. fini .or. find )
        goto 10
    endif
    nbsgte = ptsgel
    call jedetr('&&I3IMAS.LISTE.POINT')
    call jedetr('&&I3IMAS.LSTPT.ABSC.SGT ')
    call jedetr('&&I3IMAS.LSTPT.FACE     ')
    call jedetr('&&I3IMAS.LSTPT.ARETE    ')
    call jedetr('&&I3IMAS.LSTPT.TYPE.FACE')
    call jedetr('&&I3IMAS.LSTPT.COORD.REF')
    call jedetr('&&I3IMAS.LSTPT.ORDRE    ')
    call jedema()
end subroutine
