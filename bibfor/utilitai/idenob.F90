function idenob(obj1, obj2)
    implicit none
    logical :: idenob
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
    character(len=*) :: obj1, obj2
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
! ----------------------------------------------------------------------
!  BUT : DETERMINER L'IDENTITE DE 2 OBJETS JEVEUX
!  IN  K*    OBJ1   : NOM DU 1ER OBJET JEVEUX
!  IN  K*    OBJ2   : NOM DU 2EME OBJET JEVEUX
!
!     RESULTAT:
!       IDENOB : .TRUE.    SI OBJ1(*) == OBJ2(*)
!                    OU SI OBJ1 ET OBJ2 SONT INEXISTANTS TOUS LES 2
!                .FALSE.   SINON
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
    character(len=24) :: ob1, ob2, k241, k242
    character(len=1) :: typ1, typ2, kbid
    character(len=4) :: genr1, genr2, genr, xous1, xous2, xous, type
    integer :: iret1, iret2, ltyp1, ltyp2, ltyp, l, l1, l2, k, iad1, iad2, ibid
    integer :: iobj, nbobj
!
! -DEB------------------------------------------------------------------
!
    call jemarq()
    ob1 = obj1
    ob2 = obj2
    idenob = .true.
!
    if (ob1 .eq. ob2) goto 220
!
!
    call jeexin(ob1, iret1)
    call jeexin(ob2, iret2)
    if (iret1 .eq. 0) then
        if (iret2 .gt. 0) goto 210
        goto 220
    else
        if (iret2 .eq. 0) goto 210
    endif
!
!
!     -- DETERMINATION DE TYPE: R/C/K8,...
!     -------------------------------------
    call jelira(ob1, 'TYPE', ibid, typ1)
    call jelira(ob2, 'TYPE', ibid, typ2)
    if (typ1 .ne. typ2) then
        goto 210
    endif
!
    if (typ1 .eq. 'K') then
        call jelira(ob1, 'LTYP', ltyp1, kbid)
        call jelira(ob2, 'LTYP', ltyp2, kbid)
        if (ltyp1 .ne. ltyp2) then
            goto 210
        else
            ltyp = ltyp1
        endif
!
!
        if (ltyp .eq. 8) then
            type = 'K8'
        else if (ltyp.eq.16) then
            type = 'K16'
        else if (ltyp.eq.24) then
            type = 'K24'
        else if (ltyp.eq.32) then
            type = 'K32'
        else if (ltyp.eq.80) then
            type = 'K80'
        endif
    else
        type = typ1
    endif
!
!
!
!     -- DETERMINATION DE XOUS ET GENR
!     -------------------------------------
    call jelira(ob1, 'XOUS', ibid, xous1)
    call jelira(ob2, 'XOUS', ibid, xous2)
    if (xous1 .ne. xous2) then
        goto 210
    else
        xous = xous1
    endif
!
    call jelira(ob1, 'GENR', ibid, genr1)
    call jelira(ob2, 'GENR', ibid, genr2)
    if (genr1 .ne. genr2) then
        goto 210
    else
        genr = genr1
    endif
!
!
!
!     3- ON COMPARE LE CONTENU DES OBJETS
!     -------------------------------------
!
!     3.1 : CAS DES OBJETS SIMPLES :
!     ------------------------------
    if (xous .eq. 'S') then
        call assert((genr.eq.'V').or.(genr.eq.'N'))
        if (genr .eq. 'V') then
!
            call jelira(ob1, 'LONMAX', l1, kbid)
            call jelira(ob2, 'LONMAX', l2, kbid)
            if (l1 .ne. l2) then
                goto 210
            endif
!
            call jelira(ob1, 'LONUTI', l1, kbid)
            call jelira(ob2, 'LONUTI', l2, kbid)
            if (l1 .ne. l2) then
                goto 210
            else
                l = l1
            endif
!
            call jeveuo(ob1, 'L', iad1)
            call jeveuo(ob2, 'L', iad2)
!
            if (type .eq. 'R') then
                do 10,k = 1,l
                if (zr(iad1-1+k) .ne. zr(iad2-1+k)) goto 210
10              continue
!
            else if (type.eq.'I') then
                do 20,k = 1,l
                if (zi(iad1-1+k) .ne. zi(iad2-1+k)) goto 210
20              continue
!
            else if (type.eq.'C') then
                do 30,k = 1,l
                if (zc(iad1-1+k) .ne. zc(iad2-1+k)) goto 210
30              continue
!
            else if (type.eq.'L') then
                do 40,k = 1,l
                if (.not. (zl(iad1-1+k).or. (.not. zl(iad2-1+k)))) goto 210
40              continue
!
            else if (type.eq.'K8') then
                do 50,k = 1,l
                if (zk8(iad1-1+k) .ne. zk8(iad2-1+k)) goto 210
50              continue
!
            else if (type.eq.'K16') then
                do 60,k = 1,l
                if (zk16(iad1-1+k) .ne. zk16(iad2-1+k)) goto 210
60              continue
!
            else if (type.eq.'K24') then
                do 70,k = 1,l
                if (zk24(iad1-1+k) .ne. zk24(iad2-1+k)) goto 210
70              continue
!
            else if (type.eq.'K32') then
                do 80,k = 1,l
                if (zk32(iad1-1+k) .ne. zk32(iad2-1+k)) goto 210
80              continue
!
            else if (type.eq.'K80') then
                do 90,k = 1,l
                if (zk80(iad1-1+k) .ne. zk80(iad2-1+k)) goto 210
90              continue
!
            else
                call assert(.false.)
            endif
!
!
        else if (genr.eq.'N') then
!       ------------------------------
            call jelira(ob1, 'NOMMAX', l1, kbid)
            call jelira(ob2, 'NOMMAX', l2, kbid)
            if (l1 .ne. l2) then
                goto 210
            endif
!
            call jelira(ob1, 'NOMUTI', l1, kbid)
            call jelira(ob2, 'NOMUTI', l2, kbid)
            if (l1 .ne. l2) then
                goto 210
            else
                l = l1
            endif
!
            do 100,k = 1,l
            call jenuno(jexnum(ob1, k), k241)
            call jenuno(jexnum(ob2, k), k242)
            if (k241 .ne. k242) goto 210
100          continue
!
!
!
        endif
!
!
!     3.2 : CAS DES COLLECTIONS :
!     ------------------------------
    else
        if (genr .eq. 'V') then
!
            call jelira(ob1, 'NMAXOC', l1, kbid)
            call jelira(ob2, 'NMAXOC', l2, kbid)
            if (l1 .ne. l2) then
                goto 210
            endif
!
            call jelira(ob1, 'NUTIOC', l1, kbid)
            call jelira(ob2, 'NUTIOC', l2, kbid)
            if (l1 .ne. l2) then
                goto 210
            endif
            nbobj = l1
!
            do 200,iobj = 1,nbobj
!
            call jelira(jexnum(ob1, iobj), 'LONMAX', l1, kbid)
            call jelira(jexnum(ob2, iobj), 'LONMAX', l2, kbid)
            if (l1 .ne. l2) then
                goto 210
            endif
!
            call jelira(jexnum(ob1, iobj), 'LONUTI', l1, kbid)
            call jelira(jexnum(ob2, iobj), 'LONUTI', l2, kbid)
            if (l1 .ne. l2) then
                goto 210
            else
                l = l1
            endif
!
            call jeveuo(jexnum(ob1, iobj), 'L', iad1)
            call jeveuo(jexnum(ob2, iobj), 'L', iad2)
!
            if (type .eq. 'R') then
                do 110,k = 1,l
                if (zr(iad1-1+k) .ne. zr(iad2-1+k)) goto 210
110              continue
!
            else if (type.eq.'I') then
                do 120,k = 1,l
                if (zi(iad1-1+k) .ne. zi(iad2-1+k)) goto 210
120              continue
!
            else if (type.eq.'C') then
                do 130,k = 1,l
                if (zc(iad1-1+k) .ne. zc(iad2-1+k)) goto 210
130              continue
!
            else if (type.eq.'L') then
                do 140,k = 1,l
                if (.not. (zl(iad1-1+k).or. (.not.zl(iad2-1+k) ))) goto 210
140              continue
!
            else if (type.eq.'K8') then
                do 150,k = 1,l
                if (zk8(iad1-1+k) .ne. zk8(iad2-1+k)) goto 210
150              continue
!
            else if (type.eq.'K16') then
                do 160,k = 1,l
                if (zk16(iad1-1+k) .ne. zk16(iad2-1+k)) goto 210
160              continue
!
            else if (type.eq.'K24') then
                do 170,k = 1,l
                if (zk24(iad1-1+k) .ne. zk24(iad2-1+k)) goto 210
170              continue
!
            else if (type.eq.'K32') then
                do 180,k = 1,l
                if (zk32(iad1-1+k) .ne. zk32(iad2-1+k)) goto 210
180              continue
!
            else if (type.eq.'K80') then
                do 190,k = 1,l
                if (zk80(iad1-1+k) .ne. zk80(iad2-1+k)) goto 210
190              continue
!
            else
                call assert(.false.)
            endif
200          continue
!
        else
            call assert(.false.)
        endif
    endif
!
    goto 220
!
210  continue
    idenob = .false.
!
220  continue
!
!
    call jedema()
end function
