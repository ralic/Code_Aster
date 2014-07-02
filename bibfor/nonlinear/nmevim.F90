subroutine nmevim(sdimpr, sddisc, sderro, nombcl)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmacto.h"
#include "asterfort/nmerge.h"
#include "asterfort/nmimpx.h"
#include "asterfort/nmlecv.h"
#include "asterfort/nmltev.h"
#include "asterfort/utdidt.h"
#include "asterfort/utmess.h"
    character(len=24) :: sdimpr, sderro
    character(len=19) :: sddisc
    character(len=4) :: nombcl
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (SD ERREUR)
!
! EMISSION MESSAGE EVENEMENT
!
! ----------------------------------------------------------------------
!
! IN  SDIMPR : SD AFFICHAGE
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  SDERRO : SD ERREUR
! IN  NOMBCL : NOM DE LA BOUCLE
!               'NEWT' - BOUCLE DE NEWTON
!               'FIXE' - BOUCLE DE POINT FIXE
!               'INST' - BOUCLE SUR LES PAS DE TEMPS
!
! ----------------------------------------------------------------------
!
    aster_logical :: lacti, cvbouc, lerrei, llign, lldcbo
    integer :: ievdac
    real(kind=8) :: r8bid
    integer :: ibid
    character(len=16) :: nomevd, action
    integer :: ieven, zeven
    character(len=24) :: errinf
    integer :: jeinfo
    character(len=24) :: erraac, erreni, errmsg
    integer :: jeeact, jeeniv, jeemsg
    integer :: icode
    character(len=9) :: teven
    character(len=24) :: meven
!
! ----------------------------------------------------------------------
!
    call nmlecv(sderro, nombcl, cvbouc)
    call nmltev(sderro, 'ERRI', nombcl, lerrei)
    call nmerge(sderro, 'INTE_BORN', lldcbo)
!
! --- DEVRA-T-ON AFFICHER UNE LIGNE DE SEPARATION ?
!
    llign = (.not.cvbouc.and..not.lerrei.and..not.lldcbo)
!
! --- ACCES SDERRO
!
    errinf = sderro(1:19)//'.INFO'
    call jeveuo(errinf, 'L', jeinfo)
    zeven = zi(jeinfo-1+1)
!
    erraac = sderro(1:19)//'.EACT'
    erreni = sderro(1:19)//'.ENIV'
    errmsg = sderro(1:19)//'.EMSG'
    call jeveuo(erraac, 'L', jeeact)
    call jeveuo(erreni, 'L', jeeniv)
    call jeveuo(errmsg, 'L', jeemsg)
!
! --- EMISSION DES MESSAGES RELATIFS AUX EVENEMENTS INTRINSEQUES
!
    do 10 ieven = 1, zeven
        icode = zi(jeeact-1+ieven)
        teven = zk16(jeeniv-1+ieven)(1:9)
        meven = zk24(jeemsg-1+ieven)
        if ((teven(1:4).eq.'EVEN') .and. (icode.eq.1)) then
            if (meven .ne. ' ') then
                if (llign) call nmimpx(sdimpr)
                if (meven .eq. 'MECANONLINE10_1') then
                    call utmess('I', 'MECANONLINE10_1')
                else if (meven.eq.'MECANONLINE10_2') then
                    call utmess('I', 'MECANONLINE10_2')
                else if (meven.eq.'MECANONLINE10_3') then
                    call utmess('I', 'MECANONLINE10_3')
                else if (meven.eq.'MECANONLINE10_4') then
                    call utmess('I', 'MECANONLINE10_4')
                else if (meven.eq.'MECANONLINE10_5') then
                    call utmess('I', 'MECANONLINE10_5')
                else if (meven.eq.'MECANONLINE10_6') then
                    call utmess('I', 'MECANONLINE10_6')
                else if (meven.eq.'MECANONLINE10_7') then
                    call utmess('I', 'MECANONLINE10_7')
                else if (meven.eq.'MECANONLINE10_8') then
                    call utmess('I', 'MECANONLINE10_8')
                else if (meven.eq.'MECANONLINE10_9') then
                    call utmess('I', 'MECANONLINE10_9')
                else if (meven.eq.'MECANONLINE10_10') then
                    call utmess('I', 'MECANONLINE10_10')
                else if (meven.eq.'MECANONLINE10_11') then
                    call utmess('I', 'MECANONLINE10_11')
                else if (meven.eq.'MECANONLINE10_12') then
                    call utmess('I', 'MECANONLINE10_12')
                else if (meven.eq.'MECANONLINE10_20') then
                    call utmess('I', 'MECANONLINE10_20')
                else if (meven.eq.'MECANONLINE10_24') then
                    call utmess('I', 'MECANONLINE10_24')
                else if (meven.eq.'MECANONLINE10_25') then
                    if (cvbouc .and. nombcl .eq. 'NEWT') then
                        call utmess('A', 'MECANONLINE10_25')
                    endif
                else
                    ASSERT(.false.)
                endif
            endif
        endif
 10 end do
!
! --- EMISSION DES MESSAGES RELATIFS AUX EVENEMENTS UTILISATEURS
!
    call nmacto(sddisc, ievdac)
    lacti = ievdac.gt.0
    if (lacti) then
        call utdidt('L', sddisc, 'ECHE', ievdac, 'NOM_EVEN',&
                    r8bid, ibid, nomevd)
        call utdidt('L', sddisc, 'ECHE', ievdac, 'ACTION',&
                    r8bid, ibid, action)
        if (nomevd .eq. 'COLLISION') then
            if (llign) call nmimpx(sdimpr)
            call utmess('I', 'MECANONLINE10_21')
        else if (nomevd.eq.'INTERPENETRATION') then
            if (llign) call nmimpx(sdimpr)
            call utmess('I', 'MECANONLINE10_22')
        else if (nomevd.eq.'DIVE_RESI') then
            if (llign) call nmimpx(sdimpr)
            call utmess('I', 'MECANONLINE10_23')
        else if (nomevd.eq.'DELTA_GRANDEUR') then
            if (llign) call nmimpx(sdimpr)
            call utmess('I', 'MECANONLINE10_24')
        endif
    endif
!
end subroutine
