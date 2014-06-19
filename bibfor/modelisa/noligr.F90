subroutine noligr(noma,ligrz, igrel, numel, nb, li,&
                  lk, code, irepe, inema, nbno,&
                  typlaz,jlgns,&
                  rapide, jliel0, jlielc, jnema0, jnemac)
    implicit none
#include "jeveux.h"
#include "asterfort/jecroc.h"
#include "asterfort/jelira.h"
#include "asterfort/jeecra.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/jexatr.h"
#include "asterfort/jeimpo.h"
#include "asterfort/poslag.h"
#include "asterfort/utmess.h"
#include "asterfort/assert.h"
!
    character(len=8),intent(in) :: noma
    character(len=*),intent(in) :: ligrz
    integer,intent(in) :: igrel
    integer,intent(in) :: numel
    integer,intent(in) :: nb
    integer,intent(in) :: li(*)
    character(len=*),intent(in) :: lk(*)
    integer,intent(in) :: code
    integer,intent(in) :: irepe
    integer,intent(inout) :: inema
    integer,intent(inout) :: nbno(*)
    character(len=*),intent(in) :: typlaz
    integer,intent(in) :: jlgns

!   -- arguments optionnels pour gagner du CPU :
    character(len=3), intent(in), optional ::  rapide
    integer, intent(in), optional ::  jliel0
    integer, intent(in), optional ::  jlielc
    integer, intent(in), optional ::  jnema0
    integer, intent(in), optional ::  jnemac


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
!
! but: remplir le ligrel ligr
!  1.  adjonction du grel dans le ligrel
!  2.  stockage des mailles et noeuds supplementaires:
!    2.1 stockage des mailles supplementaires dans .nema
!    2.2 stockage des numeros des mailles supplementaires dans .liel
!
! arguments d'entree:
!      ligr : nom du ligrel
!      igrel: numero du grel
!      numel:numero du type_element
!      nb   :nombre de noeuds de la liste
!      li   :liste de numeros de noeuds (au nombre de nb)
!      lk   :liste de noms de noeuds (au nombre de -nb)
!      code : 1 ==> une maille "poi1" par noeud
!                   (typiquement: force_nodale)
!           : 2 ==> une maille "seg2" et un noeud tardif par noeud
!                   (typiquement: ddl_impo    )
!           : 3 ==> une maille "seg2" par noeud, et un seul noeud tardif
!                   son numero est incremente par la routine appelante
!                   (typiquement: liaison_ddl )
!   alteration : 2 ddl de lagrange =>
!           : 2 ==> une maille "seg3" et 2 noeuds tardifs par noeud
!                   (typiquement: ddl_impo    )
!           : 3 ==> une maille "seg3" par noeud, et 2 noeuds tardifs
!                   par liaison
!                   son numero est incremente par la routine appelante
!                   (typiquement: liaison_ddl )
!           : 4 ==> une maille "seg3" par noeud, et 2 noeuds tardifs
!                   par liaison sur nb liaisons du meme type
!                   les numero des noeuds tardifs sont geres par la
!                   routine appelante ( pour relation i noeud tardif 1
!                   de num : -nbno(i)+1 et noeud tardif 2 -nbno(i) )
!                   (typiquement: liaison_ddl_group )
!      irepe:nombre de repetitions de la boucle par noeud
!      inema:numero  de la derniere maille tardive dans ligr
!      nbno :numero  du  dernier noeud tardif dans ligr ou liste de nume
!            ro de noeuds tardifs(code 4)
!      typlag:type des multiplicateurs de lagrange associes a la
!             relation
!          : '12'  ==>  le premier lagrange est avant le noeud physique
!                       le second lagrange est apres
!          : '22'  ==>  le premier lagrange est apres le noeud physique
!                       le second lagrange est apres
!
!     Les arguments suivants sont facultatifs :
!     ---------------------------------------------
!     Ils ne sont pas documentes. Il ne doivent etre renseignes que dans le
!     cas ou la routine noligr est appelee de (trop) nombreuses fois.
!     Exemple d'utilisation : aflrch.F90
!
!     rapide: 'OUI' / 'NON'
!     jliel0, jlielc : adresses pour l'objet l'objet ligrel.LIEL
!     jnema0, jnemac : adresses pour l'objet l'objet ligrel.NEMA
!
!     Attention : si rapide='OUI', il faut que l'appelant fasse appel a
!                 jeecra / NUTIOC apres le denier appel a noligr
!                 (objets .LIEL et .NEMA)

!     sorties:
!        on enrichit le contenu de la carte chin
!------------------------------------------------------------------------
    character(len=8) :: typlag
    character(len=19) :: ligr
    integer :: absnb
    character(len=24) :: liel, nema
    integer :: ic, ilag1, ilag2,jnema, jnema02, jnemac2
    integer :: jliel, jliel02, jlielc2
    integer :: k, kligr, lonigr, nunoeu, lgnema
    logical :: lrapid
    integer, save :: iprem=0 , numpoi, numse3
!-----------------------------------------------------------------------
    iprem=iprem+1
    if (iprem.eq.1) then
        call jenonu(jexnom('&CATA.TM.NBNO', 'POI1'), numpoi)
        call jenonu(jexnom('&CATA.TM.NBNO', 'SEG3'), numse3)
    endif

    typlag = typlaz
    call poslag(typlag, ilag1, ilag2)

    ligr=ligrz
    liel=ligr//'.LIEL'
    nema=ligr//'.NEMA'


!   -- gestion des derniers arguments facultatifs (pour gains CPU) :
!   ----------------------------------------------------------------
    lrapid=.false.
    if (present(rapide)) then
        if (rapide.eq.'OUI') lrapid=.true.
    endif

    if (.not.lrapid) then
        call jeveuo(liel, 'E', jliel02)
        call jeveuo(jexatr(liel, 'LONCUM'), 'E', jlielc2)
        call jeveuo(nema, 'E', jnema02)
        call jeveuo(jexatr(nema, 'LONCUM'), 'E', jnemac2)
    else
        ASSERT(present(jliel0))
        ASSERT(present(jlielc))
        ASSERT(present(jnema0))
        ASSERT(present(jnemac))
        jliel02=jliel0
        jlielc2=jlielc
        jnema02=jnema0
        jnemac2=jnemac
    endif

    absnb=abs(nb)
    ASSERT(code.ge.1 .and. code.le.4)

!   -- lgnema : longueur d'un objet de la collection .NEMA :
    if (code .eq. 1) then
        lgnema=2
    else
        lgnema=4
    endif


    lonigr = absnb*irepe + 1
    if (.not.lrapid) then
        call jeecra(jexnum(liel,igrel), 'LONMAX', ival=lonigr)
        call jecroc(jexnum(liel,igrel))
    else
        if (igrel.eq.1) zi(jlielc2)=1
        if (inema.eq.1) zi(jnemac2)=1
        zi(jlielc2-1+igrel+1)=zi(jlielc2-1+igrel)+lonigr
    endif


    kligr = 0
    jliel=jliel02-1+zi(jlielc2-1+igrel)
    do 130 ic = 1, irepe
        do 110 k = 1, absnb
            kligr = kligr + 1
            inema = inema + 1
            zi(jliel-1+kligr) = -inema

            if (nb .lt. 0) then
                call jenonu(jexnom(noma//'.NOMNOE', lk(k)), nunoeu)
            else
                nunoeu=li(k)
            endif

            jnema=jnema02-1+zi(jnemac2-1+inema)
            if (.not.lrapid) then
                call jeecra(jexnum(nema,inema), 'LONMAX', ival=lgnema)
                call jecroc(jexnum(nema,inema))
            else
                zi(jnemac2-1+inema+1)=zi(jnemac2-1+inema)+lgnema
            endif

            if (code .eq. 1) then
                zi(jnema-1+1) = nunoeu
                zi(jnema-1+2) = numpoi

            else if (code.eq.2) then
                zi(jnema-1+1) = nunoeu
                nbno(1) = nbno(1) + 1
                zi(jnema-1+2) = -nbno(1)
                nbno(1) = nbno(1) + 1
                zi(jnema-1+3) = -nbno(1)
                zi(jnema-1+4) = numse3

            else if (code.eq.3) then
                zi(jnema-1+1) = nunoeu
                zi(jnema-1+2) = -nbno(1)+1
                zi(jnema-1+3) = -nbno(1)
                zi(jnema-1+4) = numse3
                ASSERT(jlgns.ne.1)
                zi(jlgns+nbno(1)-2) = ilag1
                zi(jlgns+nbno(1)-1) = ilag2

            else if (code.eq.4) then
                zi(jnema-1+1) = nunoeu
                zi(jnema-1+2) = -nbno(k)+1
                zi(jnema-1+3) = -nbno(k)
                zi(jnema-1+4) = numse3

            else
                ASSERT(.false.)
            endif
110      continue
130  end do
     zi(jliel-1+kligr+1) = numel

end subroutine
