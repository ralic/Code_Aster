subroutine pascou(mate, carele, sddyna, sddisc)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/calcul.h"
#include "asterfort/celces.h"
#include "asterfort/cesexi.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mecara.h"
#include "asterfort/megeom.h"
#include "asterfort/ndynlo.h"
#include "asterfort/ndynre.h"
#include "asterfort/utdidt.h"
#include "asterfort/utmess.h"
!
    character(len=24) :: mate, carele
    character(len=19) :: sddyna, sddisc
!
! ----------------------------------------------------------------------
!
! ROUTINE DYNA_NON_LINE (UTILITAIRE)
!
! EVALUATION DU PAS DE TEMPS DE COURANT POUR LE MODELE
!
! ----------------------------------------------------------------------
!
!
! IN  MATE   : CHAMP MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  SDDYNA : SD DEDIEE A LA DYNAMIQUE (CF NDLECT)
! IN  SDDISC : SD DISCRETISATION
!
!
!
!
    integer :: ibid, jcesd, jcesl, n1, i
    integer :: nbma, ima, iad, nbinst, nbmcfl
    real(kind=8) :: dtcou, valeur, phi, r8b
    aster_logical :: booneg, boopos
    character(len=6) :: nompro
    character(len=8) :: k8bid, mo, lpain(3), lpaout(1), stocfl, maicfl, mail
    character(len=19) :: chams
    character(len=24) :: chgeom, ligrel, lchin(3), lchout(1), chcara(18)
    real(kind=8), pointer :: ditr(:) => null()
    real(kind=8), pointer :: cesv(:) => null()
!
! ---------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    nompro ='OP0070'
!
    call getvid(' ', 'MODELE', scal=mo, nbret=ibid)
!
    ligrel=mo//'.MODELE'
!
    lpain(1)='PMATERC'
    lchin(1)=mate
!
! --- RECUPERATION DU CHAMP GEOMETRIQUE
    call megeom(mo, chgeom)
!
    lpain(2)='PGEOMER'
    lchin(2)=chgeom
!
! --- CHAMP DE CARACTERISTIQUES ELEMENTAIRES
    call mecara(carele(1:8), chcara)
!
    if (carele(1:8) .ne. ' ') then
        lpain(3)='PCACOQU'
        lchin(3)=chcara(7)
    endif
!
    lpaout(1)='PCOURAN'
    lchout(1)='&&'//nompro//'.PAS_COURANT'
!
    call calcul('S', 'PAS_COURANT', ligrel, 3, lchin,&
                lpain, 1, lchout, lpaout, 'V',&
                'OUI')
!
!     PASSAGE D'UN CHAM_ELEM EN UN CHAM_ELEM_S
    chams ='&&'//nompro//'.CHAMS'
!
    call celces(lchout(1), 'V', chams)
!
    call jeveuo(chams//'.CESD', 'L', jcesd)
!
    call jelira(mo//'.MAILLE', 'LONMAX', nbma)
    call jeveuo(chams//'.CESL', 'L', jcesl)
    call jeveuo(chams//'.CESV', 'L', vr=cesv)
!
!     INITIALISATION DE DTCOU
!
    dtcou = -1.d0
!
! A L'ISSUE DE LA BOUCLE :
! BOONEG=TRUE SI L'ON N'A PAS PU CALCULER DTCOU POUR AU MOINS UN ELMNT
! BOOPOS=TRUE SI L'ON A CALCULE DTCOU POUR AU MOINS UN ELEMENT
    booneg = .false.
    boopos = .false.
    nbmcfl = 1
    do ima = 1, nbma
        call cesexi('C', jcesd, jcesl, ima, 1,&
                    1, 1, iad)
        if (iad .gt. 0) then
            valeur = cesv(iad)
        else if (iad.eq.0) then
            goto 10
        endif
        if (valeur .lt. 0) then
            booneg = .true.
        else
            boopos = .true.
            if (dtcou .gt. 0) then
                if (valeur .le. dtcou) then
                    dtcou = valeur
                    nbmcfl = ima
                endif
            else
                dtcou = valeur
            endif
        endif
 10     continue
    end do
!
    call getvtx('SCHEMA_TEMPS', 'STOP_CFL', iocc=1, scal=stocfl, nbret=n1)
!
! BOOPOS=TRUE SI L'ON A CALCULE DTCOU POUR AU MOINS UN ELEMENT
    if (boopos) then
        if (booneg) then
            call utmess('A', 'DYNAMIQUE_3')
        endif
!
!       VERIFICATION DE LA CONFORMITE DE LA LISTE D'INSTANTS
        call utdidt('L', sddisc, 'LIST', ibid, 'NBINST',&
                    r8b, nbinst, k8bid)
        call jeveuo(sddisc//'.DITR', 'L', vr=ditr)
!
        call dismoi('NOM_MAILLA', mo, 'MODELE', repk=mail)
        call jenuno(jexnum(mail//'.NOMMAI', nbmcfl), maicfl)
!
!
        if (ndynlo(sddyna,'DIFF_CENT')) then
            dtcou = dtcou / (2.d0)
            call utmess('I', 'DYNAMIQUE_5', sk=maicfl, sr=dtcou)
        else
            if (ndynlo(sddyna,'TCHAMWA')) then
                phi=ndynre(sddyna,'PHI')
                dtcou = dtcou/(phi*2.d0)
                call utmess('I', 'DYNAMIQUE_6', sk=maicfl, sr=dtcou)
            else
                call utmess('F', 'DYNAMIQUE_1')
            endif
        endif
!
        do i = 1, nbinst-1
            if (ditr(i+1)-ditr(i) .gt. dtcou) then
                if (stocfl(1:3) .eq. 'OUI') then
                    call utmess('F', 'DYNAMIQUE_2')
                else
                    call utmess('A', 'DYNAMIQUE_2')
                endif
            endif
        end do
!
    else if (stocfl(1:3).eq.'OUI') then
        call utmess('F', 'DYNAMIQUE_4')
    else if (stocfl(1:3).eq.'NON') then
        call utmess('A', 'DYNAMIQUE_4')
    endif
!
    call jedema()
!
end subroutine
