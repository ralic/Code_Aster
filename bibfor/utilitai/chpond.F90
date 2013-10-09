subroutine chpond(tych, dejain, chin, cesout, cespoi,&
                  modele)
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/celces.h"
#include "asterfort/celfpg.h"
#include "asterfort/cesexi.h"
#include "asterfort/cesred.h"
#include "asterfort/copisd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/megeom.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    character(len=8) :: modele
    character(len=19) :: chin, cesout, cespoi
    character(len=4) :: tych, dejain
!    -------------------------------------------------------------------
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
!     OPERATEUR   POST_ELEM
!     TRAITEMENT DU MOT CLE-FACTEUR "INTEGRALE"
!     ROUTINE D'APPEL : PEECAL
!
!     BUT : CALCULE UN CHAMP ELXX PONDERE DU POIDS DES "POINTS"
!          (POIDS*JACOBIEN)
!
!     IN  CHIN      : CHAMP A PONDERER   (CHAM_ELEM   /ELXX)
!     IN  TYCH      : TYPE DU CHAMP (ELNO/ELGA/ELEM)
!     IN  MODELE    : NOM DU MODELE
!     IN  DEJAIN    : POUR LES CHAMPS ELEM : DEJA_INTEGRE=OUI/NON
!     OUT CESOUT    : CHIN + PONDERATION (CHAM_ELEM_S /ELXX)
!     IN/OUT CESPOI : PONDERATION        (CHAM_ELEM_S /ELXX)
!                     + OBJET .PDSM (POIDS DES MAILLES)
!     ------------------------------------------------------------------
!
    integer :: iret, nbchin, nbma, nbpt, nbsp, nbcmp, joutv, joutl, joutd
    integer :: iad1, iad2, iad3, isp, ima, icmp, ipt, jchsv, jchsl, jchsd, iexi
    integer :: jpoiv, jpoid, jpoil, jpoic, jch2, jch1, iret1, iret2, jpdsm
    integer :: indma
    real(kind=8) :: poids
    parameter(nbchin=2)
    character(len=8) :: lpain(nbchin), lpaout(1), noma, valk
    character(len=19) :: chins
    character(len=24) :: ligrel, chgeom, lchin(nbchin), lchout(2), vefch1
    character(len=24) :: vefch2
    logical :: peecal, ltest
!
    call jemarq()
!
    call dismoi('NOM_LIGREL', chin, 'CHAM_ELEM', repk=ligrel)
!
    call dismoi('NOM_MAILLA', modele, 'MODELE', repk=noma)
    call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbma)
    call jeexin('&&PEECAL.IND.MAILLE', iret)
    peecal=.true.
    if (iret .eq. 0) then
        peecal=.false.
    else
        call jeveuo('&&PEECAL.IND.MAILLE', 'L', indma)
    endif
!
!
! --- CALCUL DU CHAMP CESPOI
!    (UNIQUEMENT AU PREMIER NUMERO D'ORDRE RENCONTRE)
!
    call jeexin(cespoi//'.CESV', iret)
!
    if (iret .eq. 0) then
!
        call megeom(modele, chgeom)
        lchin(1)=chgeom(1:19)
        lpain(1)='PGEOMER'
        lchout(1)='&&PEECAL.PGCOOR'
        lpaout(1)='PCOORPG'
!
        call calcul('S', 'COOR_ELGA', ligrel, 1, lchin,&
                    lpain, 1, lchout, lpaout, 'V',&
                    'OUI')
!
!       --- VERIFICATION SUR LES CHAMPS CESPOI ET CHIN :
!       (MEME FAMILLE DE PG & MEME ELEMENT DE REFERENCE)
        if (tych .eq. 'ELGA') then
            vefch1='&&PEECAL.FPGCHIN'
            vefch2='&&PEECAL.FPGCOOR'
            call celfpg(chin, vefch1, iret1)
            ASSERT(iret1.eq.0)
            call celfpg(lchout(1), vefch2, iret2)
            ASSERT(iret2.eq.0)
            call jeveuo(vefch1, 'L', jch1)
            call jeveuo(vefch2, 'L', jch2)
            do ima = 1, nbma
!           -- IL NE FAUT VERIFIER QUE LES MAILLES AFFECTEES DE CHIN:
                if (zk16(jch1+ima-1) .eq. ' ') goto 5
!           -- IL NE FAUT VERIFIER QUE LES MAILLES POSTRAITEES:
                ltest = .false.
                if (peecal) then
                    if (zi(indma+ima-1) .eq. 1) ltest=.true.
                endif
                if ((.not.peecal) .or. ltest) then
!           -- SI LE CHAMP COOR_ELGA N'EST PAS CALCULE ON S'ARRETE:
                    if (zk16(jch2+ima-1) .eq. ' ') then
                        valk=zk16(jch1+ima-1)(1:8)
                        call utmess('F', 'UTILITAI8_63', sk=valk, si=ima)
                    endif
                    if (zk16(jch1+ima-1) .ne. zk16(jch2+ima-1)) then
                        call utmess('F', 'CALCULEL2_4', sk=zk16(jch1+ima- 1))
                    endif
                endif
  5             continue
            end do
            call jedetr(vefch1)
            call jedetr(vefch2)
        endif
!
        call celces(lchout(1), 'V', cespoi)
        call cesred(cespoi, 0, [0], 1, 'W',&
                    'V', cespoi)
!
    endif
!
! --- CREATION ET RECUPERATION DES POINTEURS
!
    call jeveuo(cespoi//'.CESV', 'L', jpoiv)
    call jeveuo(cespoi//'.CESL', 'L', jpoil)
    call jeveuo(cespoi//'.CESD', 'L', jpoid)
    call jeveuo(cespoi//'.CESC', 'L', jpoic)
!
    chins='&&CHPOND.CHINS'
    call celces(chin, 'V', chins)
    call jeveuo(chins//'.CESV', 'L', jchsv)
    call jeveuo(chins//'.CESL', 'L', jchsl)
    call jeveuo(chins//'.CESD', 'L', jchsd)
!
! --- CREATION ET REMPLISSAGE DES CHAMPS OUT
!
    call copisd('CHAM_ELEM_S', 'V', chins, cesout)
!
    call jeveuo(cesout//'.CESV', 'E', joutv)
    call jeveuo(cesout//'.CESL', 'E', joutl)
    call jeveuo(cesout//'.CESD', 'E', joutd)
!
    nbma = zi(jpoid-1+1)
!     -- CALCUL DU VOLUME DES MAILLES (SI ELEM ET ELNO) :
    if (tych .ne. 'ELGA') then
        call jeexin(cespoi//'.PDSM', iexi)
        if (iexi .eq. 0) then
            call wkvect(cespoi//'.PDSM', 'V V R', nbma, jpdsm)
            do ima = 1, nbma
                ltest = .false.
                if (peecal) then
                    if (zi(indma+ima-1) .eq. 1) ltest=.true.
                endif
                if ((.not.peecal) .or. ltest) then
                    nbpt=zi(jpoid-1+5+4*(ima-1)+1)
                    do ipt = 1, nbpt
                        call cesexi('C', jpoid, jpoil, ima, ipt,&
                                    1, 1, iad2)
                        ASSERT(iad2.gt.0)
                        zr(jpdsm-1+ima)=zr(jpdsm-1+ima)+zr(jpoiv-1+&
                        iad2)
                    end do
                endif
            end do
        else
            call jeveuo(cespoi//'.PDSM', 'L', jpdsm)
        endif
    endif
!
!
!     -- PONDERATION DU CHAMP PAR LES POIDS DES POINTS :
    do ima = 1, nbma
        if (.not.peecal .or. (peecal .and. zi(indma+ima-1).eq.1)) then
            nbpt =zi(jchsd-1+5+4*(ima-1)+1)
            nbsp =zi(jchsd-1+5+4*(ima-1)+2)
            nbcmp=zi(jchsd-1+5+4*(ima-1)+3)
            do ipt = 1, nbpt
                if (tych .eq. 'ELGA') then
                    call cesexi('S', jpoid, jpoil, ima, ipt,&
                                1, 1, iad2)
                    ASSERT(iad2.gt.0)
                    poids=zr(jpoiv-1+iad2)
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
!             SI IAD1 EST NEGATIF OU NUL ALORS IL N'Y A
!             RIEN A REMPLIR ON PASSE A LA CMP SUIVANTE
                        if (iad1 .le. 0) then
                            goto 40
                        else
                            ASSERT(iad3.gt.0)
                            zr(joutv-1+iad3)=zr(jchsv-1+iad1)*poids
                        endif
 40                     continue
                    end do
                end do
            end do
        endif
    end do
!
    call jedema()
!
end subroutine
