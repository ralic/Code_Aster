subroutine varinonu(compor_, sdresu_, nbma, lima, nbvari, novari, nuvari)
    implicit none
!
#include "jeveux.h"
!
#include "asterc/lccree.h"
#include "asterc/lcinfo.h"
#include "asterc/lcdiscard.h"
#include "asterc/lcvari.h"
#include "asterfort/assert.h"
#include "asterfort/carces.h"
#include "asterfort/cesred.h"
#include "asterfort/cesexi.h"
#include "asterfort/codent.h"
#include "asterfort/dismoi.h"
#include "asterfort/detrsd.h"
#include "asterfort/jeexin.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/indk16.h"
#include "asterfort/utmess.h"

    character(len=*), intent(in) :: compor_
    character(len=*), intent(in) :: sdresu_
    integer, intent(in) :: nbma, nbvari, lima(nbma)
    character(len=16), intent(in) :: novari(nbvari)
    character(len=8), intent(out) ::  nuvari(nbvari)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: jacques.pellet at edf.fr
! -------------------------------------------------------------------
! But : Etablir la correspondance entre les noms "mecaniques" de variables internes
! et leurs "numeros"  : 'V1', 'V7', ...
!
!  entrees:
!    compor_: nom de la carte de comportement (ou ' ')
!    sdresu_: nom d'une sd_resultat (ou ' ')
!       il faut fournir un et un seul des 2 arguments compor_ et sdresu_
!    nbma   : longueur de la liste lima
!    lima   : liste des numeros de mailles concernees
!    nbvari : longueur des listes novari et nuvari
!    novari : liste des noms "mecaniques" des variables internes
! sorties:
!    nuvari : liste des "numeros" des variables internes
! -------------------------------------------------------------------
!
    integer :: iexi,ima,kcmp,iad1,numlc,nbpt,nbsp,nbvaric,kvari,inum,numa
    integer :: j_comp_l, j_comp_d, iret
    integer :: nbvamx
    aster_logical :: dbg=.false.
    parameter ( nbvamx = 200)

    character(len=8) :: licmp(2)
    character(len=16) :: lcompo(2), comco2, liste_vari(nbvamx),relcom,deform
    character(len=19) :: compor_s, compor_s2, compor
    character(len=16), pointer :: cesv(:) => null()
    character(len=24) :: valk(4)
!--------------------------------------------------------------------------------

    call jemarq()

    if (compor_ .ne.' ') then
        ASSERT(sdresu_ .eq. ' ')
        compor=compor_
    else
        ASSERT(sdresu_ .ne. ' ')
        call dismoi('COMPOR_1', sdresu_, 'RESULTAT', repk=compor)
        ASSERT(compor.ne.' ')
    endif



!   1. Verification que toutes les mailles de lima sont affectees par le
!      meme comportement :
!      => relcom, deform
!   ---------------------------------------------------------------------
    compor_s='&&VARINONU.CS'
    compor_s2='&&VARINONU.CS2'


!   -- Pour gagner du temps, on ne cree pas compor_s2 a chaque appel :
    call jeexin(compor_s2//'.CESD',iexi)
    if (iexi.eq.0) then
        call carces(compor, 'ELEM', ' ', 'V', compor_s, 'A', iret)
        ASSERT(iret.eq.0)
        licmp(1)='RELCOM'
        licmp(2)='DEFORM'
        call cesred(compor_s, 0, [0], 2, licmp(1),'V', compor_s2)
        call detrsd('CHAM_ELEM_S', compor_s)
    endif
    call jeveuo(compor_s2//'.CESD', 'L', j_comp_d)
    call jeveuo(compor_s2//'.CESV', 'L', vk16=cesv)
    call jeveuo(compor_s2//'.CESL', 'L', j_comp_l)

    do ima=1,nbma
        numa=lima(ima)
        nbpt = zi(j_comp_d-1+5+4* (numa-1)+1)
        nbsp = zi(j_comp_d-1+5+4* (numa-1)+2)
        ASSERT(nbpt.eq.1)
        ASSERT(nbsp.eq.1)
        call cesexi('C', j_comp_d, j_comp_l, numa,1,1,1,iad1)
        ASSERT(iad1.gt.0)
        if (ima.eq.1) then
            relcom=cesv(iad1)
            deform=cesv(iad1+1)
        else
            if ((cesv(iad1).ne.relcom).or.(cesv(iad1+1).ne.deform)) then
                valk(1)=relcom
                valk(2)=deform
                valk(3)=cesv(iad1)
                valk(4)=cesv(iad1+1)
                call utmess('F', 'EXTRACTION_23', nk=4, valk=valk)
            endif
        endif
    enddo


!   2. Calcul de la liste des variables internes pour (relcom, deform) :
!      => liste_vari
!   --------------------------------------------------------------------
    lcompo(1)=relcom
    lcompo(2)=deform
    call lccree(2, lcompo, comco2)
    call lcinfo(comco2, numlc, nbvaric)
    if (dbg) write(6,*) 'varinonu numlc=',numlc
    if (dbg) write(6,*) 'varinonu nbvaric=',nbvaric
    ASSERT(nbvaric.le.nbvamx)
    call lcvari(comco2, nbvaric, liste_vari)
    if (dbg) write(6,*) 'varinonu liste_vari(:)=',liste_vari(1:nbvaric)
    call lcdiscard(comco2)


!   3. Calcul de nuvari :
!   ---------------------
    do kvari=1,nbvari
        inum = indk16(liste_vari, novari(kvari), 1, nbvaric )
        if (inum.eq.0) then
            call utmess('F','EXTRACTION_22',sk=novari(kvari))
        endif
        nuvari(kvari)='V'
        call codent(inum, 'G', nuvari(kvari)(2:8))
        if (dbg) write(6,*) 'varinonu nnuvari=',nuvari(kvari)
    enddo

    call jedema()

end subroutine
