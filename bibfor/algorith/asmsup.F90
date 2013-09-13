subroutine asmsup(masse, meca, nbmode, neq, nbsup,&
                  nsupp, nomsup, ndir, reasup, tcosup,&
                  nume, lordr)
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/posddl.h"
#include "asterfort/pteddl.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsutnc.h"
#include "asterfort/typddl.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: nbmode, neq, nbsup, ndir(*), nsupp(*), tcosup(nbsup, *), lordr(*)
    real(kind=8) :: reasup(nbsup, nbmode, *)
    character(len=8) :: masse, meca, nomsup(nbsup, *)
    character(len=14) :: nume
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     COMMANDE : COMB_SISM_MODAL POUR MULTI-SUPPORT UNIQUEMENT
!        VERIFIE QUE LES MODES STATIQUES SONT DEFINIS AUX SUPPORTS,
!                    OPTION REAC_NODA CALCULEE DANS LES MODES MECANIQUES
!        RECUPERATION DES TYPES DE COMBINAISON DES SUPPORTS,
!                     DES DEPLACEMENTS DES SUPPORTS
!     ------------------------------------------------------------------
! IN  : MASSE  : MATRICE DE MASSE DE LA STRUCTURE
! IN  : MECA   : MODES MECANIQUES DE LA STRUCTURE
! IN  : NBMODE : NOMBRE DE MODES MECANIQUES
! IN  : NEQ    : NOMBRE D'EQUATIONS
! IN  : NBSUP  : NOMBRE DE SUPPORTS DE LA STRUCTURE
! IN  : NSUPP  : MAX DU NOMBRE DE SUPPORT PAR DIRECTION
! IN  : NOMSUP : VECTEUR DES NOMS DES SUPPORTS
! IN  : NDIR   : DIRECTION DES EXCITATIONS
! OUT : REASUP : VECTEUR DES REACTIONS MODALES AUX SUPPORTS
! OUT : TCOSUP : VECTEUR DES TYPES DE RECOMBINAISON DES SUPPORTS
!                TCOSUP(I) = 1 : COMBINAISON QUADRATIQUE
!                TCOSUP(I) = 2 : COMBINAISON LINEAIRE
!                TCOSUP(I) = 3 : COMBINAISON ABSOLUE
!     ------------------------------------------------------------------
    integer :: ibid, id, iddl, ier, igr, im, in, ino, ioc, iret, is, jddl1
    integer :: jddl2, jdgn, jgrn, jnoe, lvale, nba, nbb, n1, nbbd, nbl, nbliai
    integer :: nbocc, nbtrou, ngr, nno, nt, vali(2)
    character(len=4) :: ctyp, dir(3)
    character(len=8) :: k8b, noma, noeu, nomcmp(3)
    character(len=15) :: motfac
    character(len=16) :: nomsy
    character(len=19) :: cham19
    character(len=24) :: obj1, obj2, valk(2), grnoeu
!     ------------------------------------------------------------------
    data  dir / 'X' , 'Y' , 'Z' /
    data  nomcmp / 'DX' , 'DY' , 'DZ' /
!     ------------------------------------------------------------------
!
    call jemarq()
!
    call dismoi('F', 'NOM_MAILLA', masse, 'MATR_ASSE', ibid,&
                noma, ier)
    obj1 = noma//'.GROUPENO'
    obj2 = noma//'.NOMNOE'
    ier = 0
!
!     --- VERIFICATION DES SUPPORTS ---
    call wkvect('&&ASMSUP.POSITION.DDL1', 'V V I', neq, jddl1)
    call wkvect('&&ASMSUP.POSITION.DDL2', 'V V I', neq, jddl2)
    call typddl('BLOQ', nume, neq, zi(jddl1), nba,&
                nbb, nbl, nbliai)
    do 10 id = 1, 3
        if (ndir(id) .eq. 1) then
            call pteddl('NUME_DDL', nume, 1, nomcmp(id), neq,&
                        zi(jddl2))
            nbbd = 0
            do 12 in = 1, neq
                nbbd = nbbd + ( zi(jddl1+in-1) * zi(jddl2+in-1) )
12          continue
            if (nsupp(id) .ne. nbbd) then
                ier = ier + 1
                valk(1) = dir(id)
                vali(1) = nbbd
                vali(2) = nsupp(id)
                call utmess('E', 'SEISME_23', sk=valk(1), ni=2, vali=vali)
            endif
        endif
10  end do
    call jedetr('&&ASMSUP.POSITION.DDL1')
    call jedetr('&&ASMSUP.POSITION.DDL2')
!
!     --- VERIFICATION DE L'OPTION "REAC_NODA" ---
    nomsy = 'REAC_NODA'
    call rsutnc(meca, nomsy, 0, k8b, ibid,&
                nbtrou)
    if (nbtrou .eq. 0) then
        ier = ier + 1
        valk(1) = meca
        valk(2) = nomsy
        call utmess('E', 'SEISME_24', nk=2, valk=valk)
        goto 9999
    endif
!
!     --- RECUPERATION DES REACTIONS NODALES ---
    do 60 im = 1, nbmode
        call rsexch('F', meca, nomsy, lordr(im), cham19,&
                    iret)
        call jeveuo(cham19//'.VALE', 'L', lvale)
        do 62 id = 1, 3
            if (ndir(id) .eq. 1) then
                do 64 is = 1, nsupp(id)
                    noeu = nomsup(is,id)
                    call posddl('NUME_DDL', nume, noeu, nomcmp(id), ino,&
                                iddl)
                    reasup(is,im,id) = zr(lvale+iddl-1)
64              continue
            endif
62      continue
60  end do
!
!     --- RECUPERATION DES COMBINAISONS DES SUPPORTS ---
    motfac = 'GROUP_APPUI'
    call getfac(motfac, nbocc)
    if (nbocc .eq. 0) then
        motfac = 'COMB_MULT_APPUI'
        call getfac(motfac, nbocc)
    endif
    do 39 id = 1, 3
        do 40 is = 1, nbsup
            tcosup(is,id) = 1
40      continue
39  end do
    do 42 ioc = 1, nbocc
        ctyp = ' '
        if (motfac .eq. 'GROUP_APPUI') then
            ctyp = 'QUAD'
            nt = 0
        else
            call getvtx(motfac, 'TYPE_COMBI', iocc=ioc, scal=ctyp, nbret=n1)
            call getvtx(motfac, 'TOUT', iocc=ioc, scal=k8b, nbret=nt)
        endif
        if (ctyp .ne. 'QUAD') then
            if (nt .ne. 0) then
                do 44 id = 1, 3
                    do 45 is = 1, nbsup
                        if (ctyp .eq. 'LINE') tcosup(is,id) = 2
45                  continue
44              continue
            else
                call getvtx(motfac, 'NOEUD', iocc=ioc, nbval=0, nbret=n1)
                if (n1 .ne. 0) then
                    nno = -n1
                    call wkvect('&&ASMSUP.NOEUD', 'V V K8', nno, jnoe)
                    call getvtx(motfac, 'NOEUD', iocc=ioc, nbval=nno, vect=zk8(jnoe),&
                                nbret=n1)
                    do 46 ino = 1, nno
                        noeu = zk8(jnoe+ino-1)
                        call jenonu(jexnom(obj2, noeu), iret)
                        if (iret .eq. 0) then
                            ier = ier + 1
                            valk(1) = noeu
                            valk(2) = noma
                            call utmess('E', 'SEISME_1', nk=2, valk=valk)
                            goto 46
                        endif
                        do 48 is = 1, nbsup
                            do 49 id = 1, 3
                                if (nomsup(is,id) .eq. noeu) then
                                    if (ctyp .eq. 'LINE') tcosup(is,id) = 2
                                endif
49                          continue
48                      continue
46                  continue
                    call jedetr('&&ASMSUP.NOEUD')
                else
                    call getvtx(motfac, 'GROUP_NO', iocc=ioc, nbval=0, nbret=n1)
                    if (n1 .ne. 0) then
                        ngr = -n1
                        call wkvect('&&ASMSUP.GROUP_NO', 'V V K24', ngr, jgrn)
                        call getvtx(motfac, 'GROUP_NO', iocc=ioc, nbval=ngr, vect=zk24(jgrn),&
                                    nbret=n1)
                        do 50 igr = 1, ngr
                            grnoeu = zk24(jgrn+igr-1)
                            call jeexin(jexnom(obj1, grnoeu), iret)
                            if (iret .eq. 0) then
                                ier = ier + 1
                                valk(1) = grnoeu
                                valk(2) = noma
                                call utmess('E', 'SEISME_2', nk=2, valk=valk)
                                goto 50
                            else
                                call jelira(jexnom(obj1, grnoeu), 'LONUTI', nno)
                                call jeveuo(jexnom(obj1, grnoeu), 'L', jdgn)
                                do 52 ino = 1, nno
                                    call jenuno(jexnum(obj2, zi(jdgn+ ino-1)), noeu)
                                    do 54 is = 1, nbsup
                                        do 55 id = 1, 3
                                            if (nomsup(is,id) .eq. noeu) then
                                                if (ctyp .eq. 'LINE') tcosup(is, id) = &
                                                                      2
                                            endif
55                                      continue
54                                  continue
52                              continue
                            endif
50                      continue
                        call jedetr('&&ASMSUP.GROUP_NO')
                    endif
                endif
            endif
        endif
42  end do
!
9999  continue
    if (ier .ne. 0) then
        call utmess('F', 'SEISME_6')
    endif
!
    call jedema()
end subroutine
