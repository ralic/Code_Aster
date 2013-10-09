subroutine vtcop1(chin, chout, kstop, codret)
    implicit none
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/utmess.h"
#include "asterfort/vrrefe.h"
#include "asterfort/wkvect.h"
    character(len=*) :: chin, chout
    character(len=1) :: kstop
    integer :: codret
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: nicolas.sellenet at edf.fr
!     APPELLE PAR LA ROUTINE CHAPEAU VTCOPY
!     RECOPIE LES VALEURS DU CHAM_NO CHIN DANS LE CHAM_NO CHOUT
!     CETTE ROUTINE PERMET DE CHANGER LA NUMEROTATION D'UN CHAM_NO
!
!     PRECAUTIONS D'EMPLOI :
!     - LES CHAM_NOS DOIVENT EXISTER.
!     - LES DDLS DE "LAGRANGE" SONT MIS A ZERO DANS CHOUT.
!
!     ------------------------------------------------------------------
!
!
!
    integer ::  iret, ieq1, ieq2, neq1, jvale1, jvale2, jtrav2
    integer :: neq2, jdesc1, jdesc2, jrefe1, jrefe2, jdeeq1, jdeeq2
    integer :: nnomx, ncpmx, jtrav1, nuno2, nucp2, nuno1, nucp1, jdeeq
    integer :: jcmpgd, ncmpmx, icmp
    character(len=1) :: typ1, typ2
    character(len=8) :: nomgd
    character(len=24) :: valk(4)
    character(len=19) :: ch1, ch2, pfchno
!
!     ------------------------------------------------------------------
!
    call jemarq()
    codret = 0
    ch1 = chin
    ch2 = chout
!
    call vrrefe(ch1, ch2, iret)
!
!
!     1. SI LES 2 CHAMPS ONT LES MEMES NUMEROTATIONS :
!     -------------------------------------------------
    if (iret .eq. 0) then
        call jelira(ch1//'.VALE', 'TYPE', cval=typ1)
        call jelira(ch1//'.VALE', 'LONMAX', neq1)
        call jeveuo(ch1//'.VALE', 'L', jvale1)
        call jelira(ch2//'.VALE', 'TYPE', cval=typ2)
        call jeveuo(ch2//'.VALE', 'E', jvale2)
        call dismoi('PROF_CHNO', ch2, 'CHAM_NO', repk=pfchno)
        call jeveuo(pfchno//'.DEEQ', 'L', jdeeq)
        if (typ1 .eq. typ2) then
            if (typ1 .eq. 'R') then
                do 10 ieq1 = 0, neq1-1
                    if (zi(jdeeq-1+2*ieq1+2) .le. 0) then
                        zr(jvale2+ieq1) = 0.d0
                    else
                        zr(jvale2+ieq1) = zr(jvale1+ieq1)
                    endif
 10             continue
            else if (typ1 .eq. 'C') then
                do 12 ieq1 = 0, neq1-1
                    if (zi(jdeeq-1+2*ieq1+2) .le. 0) then
                        zc(jvale2+ieq1) = dcmplx(0.d0,0.d0)
                    else
                        zc(jvale2+ieq1) = zc(jvale1+ieq1)
                    endif
 12             continue
            else
                valk(1) = ch1
                valk(2) = ch2
                valk(3) = typ1
                call utmess('F', 'ALGELINE3_93', nk=3, valk=valk)
            endif
        else
            if (typ1 .eq. 'R' .and. typ2 .eq. 'C') then
                do 14 ieq1 = 0, neq1-1
                    if (zi(jdeeq-1+2*ieq1+2) .le. 0) then
                        zc(jvale2+ieq1) = dcmplx(0.d0,0.d0)
                    else
                        zc(jvale2+ieq1) = zr(jvale1+ieq1)
                    endif
 14             continue
            else
                valk(1) = ch1
                valk(2) = typ1
                valk(3) = ch2
                valk(4) = typ2
                call utmess('F', 'ALGELINE3_94', nk=4, valk=valk)
            endif
        endif
        goto 9999
    endif
!
!
!     2. SI LES 2 CHAMPS N'ONT PAS LES MEMES NUMEROTATIONS :
!     ------------------------------------------------------
    call jelira(ch1//'.VALE', 'TYPE', cval=typ1)
    call jelira(ch1//'.VALE', 'LONMAX', neq1)
    call jeveuo(ch1//'.VALE', 'L', jvale1)
    call jelira(ch2//'.VALE', 'TYPE', cval=typ2)
    call jelira(ch2//'.VALE', 'LONMAX', neq2)
    call jeveuo(ch2//'.VALE', 'E', jvale2)
!
    call jeveuo(ch1//'.DESC', 'L', jdesc1)
    call jeveuo(ch2//'.DESC', 'L', jdesc2)
    if ((zi(jdesc1-1+2).lt.0) .or. (zi(jdesc2-1+2).lt.0)) then
        call utmess('F', 'ALGELINE3_95')
    endif
!
    call jeveuo(ch1//'.REFE', 'L', jrefe1)
    call jeveuo(ch2//'.REFE', 'L', jrefe2)
    if (zk24(jrefe1)(1:8) .ne. zk24(jrefe2)(1:8)) then
        valk(1) = ch1
        valk(2) = ch2
        valk(3) = zk24(jrefe1)(1:8)
        valk(4) = zk24(jrefe2)(1:8)
        call utmess('F', 'CALCULEL2_1', nk=4, valk=valk)
    endif
    call jeveuo(zk24(jrefe1-1+2)(1:19)//'.DEEQ', 'L', jdeeq1)
    call jeveuo(zk24(jrefe2-1+2)(1:19)//'.DEEQ', 'L', jdeeq2)
!
!
!     2.1 ON CHERCHE LE NUMERO DE CMP LE PLUS GRAND ET
!     LE NUMERO DE NOEUD LE PLUS GRAND DANS CH2->.DEEQ2 :
!     ---------------------------------------------------------------
!
    nnomx=0
    ncpmx=0
    do 16 ieq2 = 1, neq2
        nnomx= max(nnomx,zi(jdeeq2-1+2*(ieq2-1)+1))
        ncpmx= max(ncpmx,zi(jdeeq2-1+2*(ieq2-1)+2))
 16 end do
!
!
!     2.2 ON REMPLIT UN OBJET DE TRAVAIL :
!     ------------------------------------
    call wkvect('&&VTCOP1.TRAV1', 'V V I', nnomx*ncpmx, jtrav1)
    call wkvect('&&VTCOP1.TRAV2', 'V V L', neq2, jtrav2)
    do 18 ieq2 = 1, neq2
        nuno2=zi(jdeeq2-1+2*(ieq2-1)+1)
        nucp2=zi(jdeeq2-1+2*(ieq2-1)+2)
        if (nucp2 .gt. 0) zi(jtrav1-1+(nuno2-1)*ncpmx+nucp2)=ieq2
        zl(jtrav2-1+ieq2)=.false.
 18 end do
!
!
!     2.3 ON RECOPIE LES VALEURS DE CH1 DANS CH2 :
!     -------------------------------------------
    if (typ1 .eq. typ2) then
        if (typ1 .eq. 'R') then
            do 20 ieq1 = 1, neq1
                nuno1=zi(jdeeq1-1+2*(ieq1-1)+1)
                nucp1=zi(jdeeq1-1+2*(ieq1-1)+2)
                if ((nucp1.gt.0) .and. (nuno1.le.nnomx) .and. ( nucp1.le.ncpmx)) then
                    ieq2=zi(jtrav1-1+(nuno1-1)*ncpmx+nucp1)
                    if (ieq2 .gt. 0) then
                        zl(jtrav2-1+ieq2)=.true.
                        zr(jvale2-1+ieq2)=zr(jvale1-1+ieq1)
                    endif
                endif
 20         continue
        else if (typ1 .eq. 'C') then
            do 22 ieq1 = 1, neq1
                nuno1=zi(jdeeq1-1+2*(ieq1-1)+1)
                nucp1=zi(jdeeq1-1+2*(ieq1-1)+2)
                if ((nucp1.gt.0) .and. (nuno1.le.nnomx)) then
                    ieq2=zi(jtrav1-1+(nuno1-1)*ncpmx+nucp1)
                    if (ieq2 .gt. 0) then
                        zl(jtrav2-1+ieq2)=.true.
                        zc(jvale2-1+ieq2)=zc(jvale1-1+ieq1)
                    endif
                endif
 22         continue
        else
            valk(1) = ch1
            valk(2) = ch2
            valk(3) = typ1
            call utmess('F', 'ALGELINE3_93', nk=3, valk=valk)
        endif
!
    else if (typ1 .eq. 'R' .and. typ2 .eq. 'C') then
        do 24 ieq1 = 1, neq1
            nuno1=zi(jdeeq1-1+2*(ieq1-1)+1)
            nucp1=zi(jdeeq1-1+2*(ieq1-1)+2)
            if ((nucp1.gt.0) .and. (nuno1.le.nnomx)) then
                ieq2=zi(jtrav1-1+(nuno1-1)*ncpmx+nucp1)
                if (ieq2 .gt. 0) then
                    zl(jtrav2-1+ieq2)=.true.
                    zc(jvale2-1+ieq2)=zr(jvale1-1+ieq1)
                endif
            endif
 24     continue
!
    else
        valk(1) = ch1
        valk(2) = typ1
        valk(3) = ch2
        valk(4) = typ2
        call utmess('F', 'ALGELINE3_94', nk=4, valk=valk)
    endif
!
!     A CAUSE DE LA SOUS-STRUCTURATION STATIQUE, ON DOIT AJOUTER
!     UNE GLUTE POUR OUBLIER LA COMPOSANTE 'LAGR' POUR LA VERIF
    call dismoi('NOM_GD', ch2, 'CHAM_NO', repk=nomgd)
    call jeveuo(jexnom('&CATA.GD.NOMCMP', nomgd), 'L', jcmpgd)
    call jelira(jexnom('&CATA.GD.NOMCMP', nomgd), 'LONMAX', ncmpmx)
    icmp=-200
    icmp=indik8(zk8(jcmpgd),'LAGR',1,ncmpmx)
!
    do 19 ieq2 = 1, neq2
        nuno2=zi(jdeeq2-1+2*(ieq2-1)+1)
        nucp2=zi(jdeeq2-1+2*(ieq2-1)+2)
!       NUCP2.NE.ICMP == GLUTE POUR LA SOUS-STRUCTURATION STATIQUE
        if (nucp2 .gt. 0 .and. nucp2 .ne. icmp .and. .not.zl(jtrav2+ieq2-1)) then
            if (kstop .eq. 'F') then
                ASSERT(.false.)
            else
                codret = 1
            endif
        endif
 19 end do
    call jedetr('&&VTCOP1.TRAV1')
    call jedetr('&&VTCOP1.TRAV2')
!
9999 continue
    call jedema()
end subroutine
