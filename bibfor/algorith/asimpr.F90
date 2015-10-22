subroutine asimpr(nbsup, tcosup, nomsup)
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/getvis.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    integer :: nbsup, tcosup(nbsup, *)
    character(len=8) :: nomsup(nbsup, *)
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
!     ------------------------------------------------------------------
!     COMMANDE : COMB_SISM_MODAL
!      IMPRIME LES RESULTATS DES REPONSES PRIMAIRES ET SECONDAIRES
!     ------------------------------------------------------------------
!
    character(len=4) :: typdir
    character(len=8) :: noeu, cmp, noref
    character(len=8) :: knum, kdir
    character(len=19) :: knoeu, didi, lnore, nbnor, ordr
    character(len=80) :: nomcas, chainq, chainl, chaina
!
!-----------------------------------------------------------------------
    integer :: ia, ibid, ic, icas, id, idep
    integer :: ii, il, ino, iocc, iordr, iq, is
    integer :: jcas, jdir, jkno, jno,  jnref
    integer :: jord, jref,  lnod, nbno, nboc, ncas
    integer :: ndep, nucas, nume
    real(kind=8) :: epsima, vale
    character(len=3):: ki
    character(len=24):: charcas
    character(len=8), pointer :: noeuds(:) => null()
    integer, pointer :: type(:) => null()
!-----------------------------------------------------------------------
    epsima = r8vide()
    call jemarq()
    knoeu = '&&OP0109.NOM_SUPPOR'
    didi = '&&OP0109.DIRECTION'
    lnore = '&&ASENAP.NOREF'
    nbnor = '&&ASENAP.NREF'
    ordr ='&&ASECON.NORD'
!
    call utmess('I', 'SEISME_66')
    call jeveuo(knoeu, 'L', jkno)
    call jeveuo(didi, 'L', jdir)
    call jeveuo(lnore, 'L', jnref)
    call jeveuo(nbnor, 'L', jref)
    call jeveuo(ordr, 'L', jord)
!
!  CAS DES IMPRESSIONS POUR LA REPONSE PRIMAIRE
!
    chainq = ' '
    chainl = ' '
    chaina = ' '
!
    chainq(1 : 4) ='QUAD'
    chainl(1 : 4) ='LINE'
    chaina(1 : 4) ='ABS '
    iq = 1
    il = 1
    ia = 1
    AS_ALLOCATE(vk8=noeuds, size=3*nbsup)
    ino = 1
    do 40 id = 1, 3
        do 42 is = 1, nbsup
            if (tcosup(is,1) .eq. 1) then
                do 44 ii = 1, ino
                    if (nomsup(is,id) .eq. noeuds(ii)) then
                        goto 42
                    endif
44              continue
                noeuds(ino) = nomsup(is,id)
                chainq(iq+5 : iq+12)= noeuds(ino)
                ino = ino+1
                iq = iq+9
            else if (tcosup(is,1).eq.2) then
                do 46 ii = 1, ino
                    if (nomsup(is,id) .eq. noeuds(ino)) goto 42
46              continue
                noeuds(ino)= nomsup(is,id)
                chainl(il+5 : il+12)= nomsup(is,id)
                ino = ino+1
                il = il+9
            else if (tcosup(is,1).eq.3) then
                do 48 ii = 1, ino
                    if (nomsup(is,id) .eq. noeuds(ino)) goto 42
48              continue
                chaina(ia+5 : ia+12)= nomsup(is,id)
                noeuds(ino)= nomsup(is,id)
                ino = ino+1
                ia = ia+9
            endif
42      continue
40  end do
!
    if (iq .ne. 1) then
        call utmess('I', 'SEISME_67', sk=chainq)
    endif
    if (il .ne. 1) then
        call utmess('I', 'SEISME_67', sk=chainl)
    endif
    if (ia .ne. 1) then
        call utmess('I', 'SEISME_67', sk=chaina)
    endif
!
!
!
!  CAS DES IMPRESSIONS POUR LA REPONSE SECONDAIRE
!
    call getfac('COMB_DEPL_APPUI', nboc)
    call getfac('DEPL_MULT_APPUI', ndep)
    call jeveuo('&&ASENAP.TYPE', 'L', vi=type)
    noref = '-'
    call utmess('I', 'SEISME_68')
    do 10 iocc = 1, nboc
        call jelira(jexnum('&&ASENAP.LISTCAS', iocc), 'LONMAX', ncas)
        call jeveuo(jexnum('&&ASENAP.LISTCAS', iocc), 'L', jcas)
        do 20 icas = 1, ncas
            nucas = zi(jcas+icas-1)
            do 30 idep = 1, ndep
                call getvis('DEPL_MULT_APPUI', 'NUME_CAS', iocc=idep, scal=nume, nbret=ibid)
                if (nume .eq. nucas) then
                    call getvtx('DEPL_MULT_APPUI', 'NOM_CAS', iocc=idep, scal=nomcas, nbret=ibid)
                    knum = 'N       '
                    call codent(nucas, 'D0', knum(2:8))
                    kdir = 'D       '
                    call codent(nucas, 'D0', kdir(2:8))
                    call jelira(jexnom('&&ASENAP.LINOEU', knum), 'LONMAX', nbno)
                    call jeveuo(jexnom('&&ASENAP.LINOEU', knum), 'L', jno)
                    lnod =3*nbno
                    call jelira(jexnom('&&ASENAP.LIDIR', kdir), 'LONMAX', lnod)
                    call jeveuo(jexnom('&&ASENAP.LIDIR', kdir), 'L', jdir)
                    if (zi(jref+icas-1) .eq. 1) noref = zk8(jnref+icas- 1)
                    do 12 ino = 1, nbno
                        noeu =zk8(jno+ino-1)
                        if (zr(jdir+3*(ino-1)) .ne. epsima) then
                            cmp = 'DX'
                            vale = zr(jdir+3*(ino-1))
                            call utmess('I', 'SEISME_69',si=nucas, sr=vale,&
                                        nk=4, valk=[noeu,cmp, noref,nomcas])
                        endif
                        if (zr(jdir+3*(ino-1)+1) .ne. epsima) then
                            cmp = 'DY'
                            vale = zr(jdir+3*(ino-1)+1)
                            call utmess('I', 'SEISME_69',si=nucas, sr=vale,&
                                        nk=4, valk=[noeu,cmp, noref,nomcas])
                        endif
                        if (zr(jdir+3*(ino-1)+2) .ne. epsima) then
                            cmp = 'DZ'
                            vale = zr(jdir+3*(ino-1)+2)
                            call utmess('I', 'SEISME_69',si=nucas, sr=vale,&
                                        nk=4, valk=[noeu,cmp, noref,nomcas])
                        endif
12                  continue
                endif
30          continue
20      continue
10  end do
    do iocc = 1, nboc
        call jelira(jexnum('&&ASENAP.LISTCAS', iocc), 'LONMAX', ncas)
        call jeveuo(jexnum('&&ASENAP.LISTCAS', iocc), 'L', jcas)
        call utmess('I', 'SEISME_70')
        if (type(iocc) .eq. 1) typdir = 'QUAD'
        if (type(iocc) .eq. 2) typdir = 'LINE'
        if (type(iocc) .eq. 3) typdir = 'ABS'
        iordr = zi(jord+iocc-1)
        charcas = typdir
        do ic = 1, ncas
            icas = zi(jcas+ic-1)
            write( ki, '(i3)' )  icas
            charcas = charcas(1:6+4*(ic-1))//ki//' '
        enddo
        call utmess('I', 'SEISME_71', si=iordr, sk=charcas)
    end do
    iordr = zi(jord+nboc)
    typdir ='QUAD'
    call utmess('I', 'SEISME_72')
    call utmess('I', 'SEISME_71', si=iordr, sk=typdir)
!
!
! --- MENAGE
!
    call jedetr('&&ASECON.NORD')
    AS_DEALLOCATE(vk8=noeuds)
!
    call jedema()
end subroutine
