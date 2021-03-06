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
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    integer :: nbmode, neq, nbsup, ndir(*), nsupp(*), tcosup(nbsup, *), lordr(*)
    real(kind=8) :: reasup(nbsup, nbmode, *)
    character(len=8) :: masse, meca, nomsup(nbsup, *)
    character(len=14) :: nume
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
    integer :: id, iddl, ier, igr, im, in, ino, ioc, iret, is, jddl1
    integer :: jddl2, jdgn,    nba, nbb, n1, nbbd, nbl, nbliai
    integer :: nbocc, nbtrou, ngr, nno, nt, vali(2), tabord(1)
    character(len=4) :: ctyp, dir(3)
    character(len=8) :: k8b, noma, noeu, nomcmp(3)
    character(len=15) :: motfac
    character(len=16) :: nomsy
    character(len=19) :: cham19
    character(len=24) :: obj1, obj2, valk(2), grnoeu
    character(len=24), pointer :: group_no(:) => null()
    character(len=8), pointer :: noeud(:) => null()
    real(kind=8), pointer :: vale(:) => null()
!     ------------------------------------------------------------------
    data  dir / 'X' , 'Y' , 'Z' /
    data  nomcmp / 'DX' , 'DY' , 'DZ' /
!     ------------------------------------------------------------------
!
    call jemarq()
!
    call dismoi('NOM_MAILLA', masse, 'MATR_ASSE', repk=noma)
    obj1 = noma//'.GROUPENO'
    obj2 = noma//'.NOMNOE'
    ier = 0
!
!     --- VERIFICATION DES SUPPORTS ---
    call wkvect('&&ASMSUP.POSITION.DDL1', 'V V I', neq, jddl1)
    call wkvect('&&ASMSUP.POSITION.DDL2', 'V V I', neq, jddl2)
    call typddl('BLOQ', nume, neq, zi(jddl1), nba,&
                nbb, nbl, nbliai)
    do id = 1, 3
        if (ndir(id) .eq. 1) then
            call pteddl('NUME_DDL', nume, 1, nomcmp(id), neq,&
                        list_equa = zi(jddl2))
            nbbd = 0
            do in = 1, neq
                nbbd = nbbd + ( zi(jddl1+in-1) * zi(jddl2+in-1) )
            end do
            if (nsupp(id) .ne. nbbd) then
                ier = ier + 1
                valk(1) = dir(id)
                vali(1) = nbbd
                vali(2) = nsupp(id)
                call utmess('E', 'SEISME_23', sk=valk(1), ni=2, vali=vali)
            endif
        endif
    end do
    call jedetr('&&ASMSUP.POSITION.DDL1')
    call jedetr('&&ASMSUP.POSITION.DDL2')
!
!     --- VERIFICATION DE L'OPTION "REAC_NODA" ---
    nomsy = 'REAC_NODA'
    call rsutnc(meca, nomsy, 0, k8b, tabord,&
                nbtrou)
    if (nbtrou .eq. 0) then
        ier = ier + 1
        valk(1) = meca
        valk(2) = nomsy
        call utmess('E', 'SEISME_24', nk=2, valk=valk)
        goto 999
    endif
!
!     --- RECUPERATION DES REACTIONS NODALES ---
    do im = 1, nbmode
        call rsexch('F', meca, nomsy, lordr(im), cham19,&
                    iret)
        call jeveuo(cham19//'.VALE', 'L', vr=vale)
        do id = 1, 3
            if (ndir(id) .eq. 1) then
                do is = 1, nsupp(id)
                    noeu = nomsup(is,id)
                    call posddl('NUME_DDL', nume, noeu, nomcmp(id), ino,&
                                iddl)
                    reasup(is,im,id) = vale(iddl)
                end do
            endif
        end do
    end do
!
!     --- RECUPERATION DES COMBINAISONS DES SUPPORTS ---
    motfac = 'GROUP_APPUI'
    call getfac(motfac, nbocc)
    if (nbocc .eq. 0) then
        motfac = 'COMB_MULT_APPUI'
        call getfac(motfac, nbocc)
    endif
    do id = 1, 3
        do is = 1, nbsup
            tcosup(is,id) = 1
        end do
    end do
    do ioc = 1, nbocc
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
                do id = 1, 3
                    do is = 1, nbsup
                        if (ctyp .eq. 'LINE') tcosup(is,id) = 2
                    end do
                end do
            else
                call getvtx(motfac, 'NOEUD', iocc=ioc, nbval=0, nbret=n1)
                if (n1 .ne. 0) then
                    nno = -n1
                    AS_ALLOCATE(vk8=noeud, size=nno)
                    call getvtx(motfac, 'NOEUD', iocc=ioc, nbval=nno, vect=noeud,&
                                nbret=n1)
                    do ino = 1, nno
                        noeu = noeud(ino)
                        call jenonu(jexnom(obj2, noeu), iret)
                        if (iret .eq. 0) then
                            ier = ier + 1
                            valk(1) = noeu
                            valk(2) = noma
                            call utmess('E', 'SEISME_1', nk=2, valk=valk)
                            goto 46
                        endif
                        do is = 1, nbsup
                            do id = 1, 3
                                if (nomsup(is,id) .eq. noeu) then
                                    if (ctyp .eq. 'LINE') tcosup(is,id) = 2
                                endif
                            end do
                        end do
 46                     continue
                    end do
                    AS_DEALLOCATE(vk8=noeud)
                else
                    call getvtx(motfac, 'GROUP_NO', iocc=ioc, nbval=0, nbret=n1)
                    if (n1 .ne. 0) then
                        ngr = -n1
                        AS_ALLOCATE(vk24=group_no, size=ngr)
                        call getvtx(motfac, 'GROUP_NO', iocc=ioc, nbval=ngr, vect=group_no,&
                                    nbret=n1)
                        do igr = 1, ngr
                            grnoeu = group_no(igr)
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
                                do ino = 1, nno
                                    call jenuno(jexnum(obj2, zi(jdgn+ ino-1)), noeu)
                                    do is = 1, nbsup
                                        do id = 1, 3
                                            if (nomsup(is,id) .eq. noeu) then
                                                if (ctyp .eq. 'LINE') tcosup(is, id) = &
                                                                      2
                                            endif
                                        end do
                                    end do
                                end do
                            endif
 50                         continue
                        end do
                        AS_DEALLOCATE(vk24=group_no)
                    endif
                endif
            endif
        endif
    end do
!
999 continue
    if (ier .ne. 0) then
        call utmess('F', 'SEISME_6')
    endif
!
    call jedema()
end subroutine
