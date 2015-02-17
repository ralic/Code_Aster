subroutine chpond(tych, dejain, chin, cesout, cespoi,&
                  ligrel, carele)
!
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/celces.h"
#include "asterfort/cesvar.h"
#include "asterfort/celfpg.h"
#include "asterfort/cesexi.h"
#include "asterfort/cesred.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/megeom.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    character(len=8), intent(in) :: carele
    character(len=*), intent(in) :: ligrel
    character(len=19), intent(in) :: chin, cesout, cespoi
    character(len=4), intent(in) :: tych, dejain

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
!
!     operateur   post_elem
!
!     but : calcule un champ elxx pondere du poids des "points"
!          (poids*jacobien)
!
!     in  chin      : champ a ponderer   (cham_elem   /elxx)
!     in  tych      : type du champ (elno/elga/elem)
!     in  ligrel    : nom du ligrel
!     in  dejain    : pour les champs elem : deja_integre=oui/non
!     out cesout    : chin + ponderation (cham_elem_s /elxx)
!     in/out cespoi : ponderation        (cham_elem_s /elxx)
!                     + objet .PDSM (poids des mailles)
!     attention : cespoi n'est pas recalcule s'il existe deja.
!                 => gain de cpu dans une boucle sur les numeros d'ordre.
!                 mais il faut penser a le detruire quand on change de ligrel.
!-------------------------------------------------------------------------------
!
    integer :: iret, nbchin, nbma, nbpt, nbsp, nbcmp, joutl, joutd
    integer :: nbspmx
    integer :: iad1, iad2, iad3, isp, ima, icmp, ipt, jchsl, jchsd, iexi
    integer :: jpoid, jpoil, jpoic, jch2, jch1, iret1, iret2, jpdsm
    real(kind=8) :: poids
    parameter(nbchin=6)
    character(len=8) :: lpain(nbchin), lpaout(1), noma
    character(len=19) :: chins, ligr19
    character(len=24) :: chgeom, lchin(nbchin), lchout(2), vefch1
    character(len=24) :: vefch2
    integer, pointer :: repe(:) => null()
    real(kind=8), pointer :: chsv(:) => null()
    real(kind=8), pointer :: outv(:) => null()
    real(kind=8), pointer :: poiv(:) => null()
!-----------------------------------------------------------------
    call jemarq()

    ligr19=ligrel
    call dismoi('NOM_MAILLA', ligr19, 'LIGREL', repk=noma)
    call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbma)
    call jeveuo(ligr19//'.REPE', 'L', vi=repe)


!   -- CALCUL DU CHAMP CESPOI
!      (UNIQUEMENT AU PREMIER NUMERO D'ORDRE RENCONTRE)
    call jeexin(cespoi//'.CESV', iret)
    if (iret .eq. 0) then
!
        chgeom=noma//'.COORDO'
        lchin(1)=chgeom(1:19)
        lpain(1)='PGEOMER'
        lchin(2)= carele//'.CANBSP'
        lpain(2)='PNBSP_I'
        lchin(3)= carele//'.CAFIBR'
        lpain(3)='PFIBRES'
        lchin(4)= carele//'.CARORIEN'
        lpain(4)='PCAORIE'
        lchin(5)= carele//'.CARGEOPO'
        lpain(5)='PCAGEPO'
        lchin(6)=carele//'.CARCOQUE'
        lpain(6)='PCACOQU'
        lchout(1)='&&CHPOND.PGCOOR'
        lpaout(1)='PCOORPG'
!
!
        if (carele.ne.' ') then
           call cesvar(carele, ' ', ligr19, lchout(1))
        endif
!
        call calcul('S', 'COOR_ELGA', ligr19, 6, lchin,&
                    lpain, 1, lchout, lpaout, 'V', 'OUI')
!
!       -- verification sur les champs cespoi et chin :
!          (MEME FAMILLE DE PG & MEME ELEMENT DE REFERENCE)
        if (tych .eq. 'ELGA') then
            vefch1='&&CHPOND.FPGCHIN'
            vefch2='&&CHPOND.FPGCOOR'
            call celfpg(chin, vefch1, iret1)
            ASSERT(iret1.eq.0)
            call celfpg(lchout(1), vefch2, iret2)
            ASSERT(iret2.eq.0)
            call jeveuo(vefch1, 'L', jch1)
            call jeveuo(vefch2, 'L', jch2)
            do ima = 1, nbma
!               -- il ne faut verifier que les mailles affectees de chin:
                if (zk16(jch1+ima-1) .eq. ' ') cycle

!               -- il ne faut verifier que les mailles postraitees:
                if (repe(2*(ima-1)+1).eq.0) cycle

                if (ima.eq.36) then
                endif

                if (zk16(jch1+ima-1) .ne. zk16(jch2+ima-1)) then
                    call utmess('F', 'CALCULEL2_4', sk=zk16(jch1+ima- 1))
                endif
            end do
        endif
!
        call celces(lchout(1), 'V', cespoi)
        call cesred(cespoi, 0, [0], 1, 'W',&
                    'V', cespoi)
!
    endif

!   -- CREATION ET RECUPERATION DES POINTEURS
    call jeveuo(cespoi//'.CESV', 'L', vr=poiv)
    call jeveuo(cespoi//'.CESL', 'L', jpoil)
    call jeveuo(cespoi//'.CESD', 'L', jpoid)
    call jeveuo(cespoi//'.CESC', 'L', jpoic)

    chins='&&CHPOND.CHINS'
    call celces(chin, 'V', chins)
    call jeveuo(chins//'.CESV', 'L', vr=chsv)
    call jeveuo(chins//'.CESL', 'L', jchsl)
    call jeveuo(chins//'.CESD', 'L', jchsd)

!   -- CREATION ET REMPLISSAGE DES CHAMPS OUT
    call copisd('CHAM_ELEM_S', 'V', chins, cesout)
!
    call jeveuo(cesout//'.CESV', 'E', vr=outv)
    call jeveuo(cesout//'.CESL', 'E', joutl)
    call jeveuo(cesout//'.CESD', 'E', joutd)
!
    nbma = zi(jpoid-1+1)

!   -- calcul du volume des mailles (si elem ou elno) :
    if (tych .ne. 'ELGA') then
        call jeexin(cespoi//'.PDSM', iexi)
        if (iexi .eq. 0) then
            nbspmx = 0
            do ima = 1, nbma
              if(zi(jpoid-1+5+4*(ima-1)+2).gt.nbspmx) then
                 nbspmx = zi(jpoid-1+5+4*(ima-1)+2)
              end if
            end do
            call wkvect(cespoi//'.PDSM', 'V V R', nbma*nbspmx, jpdsm)

            do ima = 1, nbma
                if (repe(2*(ima-1)+1).eq.0) cycle

                nbpt   =zi(jpoid-1+5+4*(ima-1)+1)
                nbsp   =zi(jpoid-1+5+4*(ima-1)+2)
                do isp = 1, nbsp
                   do ipt = 1, nbpt
                      call cesexi('C', jpoid, jpoil, ima, ipt,&
                                  isp, 1, iad2)
                      ASSERT(iad2.gt.0)
                      zr(jpdsm-1+(ima-1)*nbsp+isp)=zr(jpdsm-1+(ima-1)*nbsp+isp)+&
                                                      poiv(iad2)
                   end do
                end do
            end do
        else
            call jeveuo(cespoi//'.PDSM', 'L', jpdsm)
        endif
    endif
!
!
!   -- ponderation du champ par les poids des points :
    do ima = 1, nbma
        if (repe(2*(ima-1)+1).eq.0) cycle

        nbpt =zi(jchsd-1+5+4*(ima-1)+1)
        nbsp =zi(jchsd-1+5+4*(ima-1)+2)
        nbcmp=zi(jchsd-1+5+4*(ima-1)+3)
        do ipt = 1, nbpt
            if (tych .eq. 'ELGA') then
                call cesexi('S', jpoid, jpoil, ima, ipt,&
                            1, 1, iad2)
                ASSERT(iad2.gt.0)
                poids=poiv(iad2)
            else if (tych.eq.'ELEM') then
                ASSERT(nbpt.eq.1)
                if (dejain .eq. 'NON') then
                    poids=zr(jpdsm-1+ima)
                else
                    poids=1.d0
                endif
            else if (tych.eq.'ELNO') then
                ASSERT(nbpt.gt.0)
                poids=zr(jpdsm-1+ima)/nbpt
            endif
!
            do isp = 1, nbsp
                do icmp = 1, nbcmp
                    call cesexi('C', jchsd, jchsl, ima, ipt,&
                                isp, icmp, iad1)
                    call cesexi('C', joutd, joutl, ima, ipt,&
                                isp, icmp, iad3)
                    if (iad1 .le. 0) then
                        goto 40
                    else
                        ASSERT(iad3.gt.0)
                        if (tych.eq.'ELNO') then
                           outv(iad3)=chsv(iad1)*zr(jpdsm-1+(ima-1)*nbsp+isp)/nbpt
                        else
                           outv(iad3)=chsv(iad1)*poids
                        endif
                    endif
 40                 continue
                end do
            end do
        end do
    end do

    call detrsd('CHAM_ELEM', '&&CHPOND.PGCOOR')
    call detrsd('CHAM_ELEM_S', '&&CHPOND.CHINS')
    call jedetr('&&CHPOND.FPGCHIN')
    call jedetr('&&CHPOND.FPGCOOR')
!
    call jedema()
!
end subroutine
