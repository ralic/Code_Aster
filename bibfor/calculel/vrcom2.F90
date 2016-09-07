subroutine vrcom2(compop, varmoi, ligrep, from_lire_resu)
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
    implicit none
#include "jeveux.h"
#include "asterfort/alchml.h"
#include "asterfort/assert.h"
#include "asterfort/carces.h"
#include "asterfort/celces.h"
#include "asterfort/cescel.h"
#include "asterfort/cesexi.h"
#include "asterfort/cesred.h"
#include "asterfort/cestas.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
    character(len=*) :: compop, varmoi, ligrep
    aster_logical :: from_lire_resu
! ----------------------------------------------------------------------
! BUT: MODIFIER VARMOI POUR LE RENDRE COHERENT AVEC COMPOP
!
!      COMPOP EST LA CARTE DE COMPOPTEMENT A L'INSTANT "+"
!      VARMOI EST LE CHAMP DE VARIABLES INTERNES A L'INSTANT "-"
!      LIGREP EST LE LIGREL DU MODELE DE L'INSTANT "+"
!
! ------------------------------------------------------------------
!     ARGUMENTS:
! COMPOP          IN/JXIN  K19 : CARTE DE COMPOPTEMENT "+"
! VARMOI          IN/JXVAR K19 : SD CHAM_ELEM   (VARI_R) "-"
! LIGREP          IN/JXIN  K19 : SD LIGREL "+"
! FROM_LIRE_RESU  IN : LOGIQUE SIGNALANT QUE L'APPELENT EST LA COMMANDE
!                 LIRE_RESU POUR TRAITER LES VARIABLES INTERNES
!-----------------------------------------------------------------------
!
!     ------------------------------------------------------------------
    integer :: iad1, iad2, nbma, nbpg2, nbsp1, nbsp2, nbcm2, ipg, isp, icm
    integer :: nbcm1
    integer :: ima, iret
    integer :: iadp, jcoppl, jcoppd, jcoppv
    integer :: action
    integer :: jcev1d,  jcev1l
    integer :: jcev2d,  jcev2l, nncp, ibid
    character(len=19) :: cesv1, cesv2, coto, copp
    character(len=19) :: varplu
    character(len=1) :: base
    real(kind=8), pointer :: cev1v(:) => null()
    real(kind=8), pointer :: cev2v(:) => null()
!     ------------------------------------------------------------------
    call jemarq()
!
!
    base='G'
    if (varmoi(1:2).eq.'&&') base='V'
    if (varmoi(1:2).ne.'&&' .and. .not. from_lire_resu )  then
        call utmess('A', 'COMPOR2_23')
        goto 99
    endif
!
!
!     1- ON TRANSFORME VARMOI EN CHAM_ELEM_S (CESV1)
!     --------------------------------------------------
    cesv1='&&VRCOM2.CESV1'
    call celces(varmoi, 'V', cesv1)
    call cestas(cesv1)
    call jeveuo(cesv1//'.CESD', 'L', jcev1d)
    call jeveuo(cesv1//'.CESV', 'L', vr=cev1v)
    call jeveuo(cesv1//'.CESL', 'L', jcev1l)
    nbma=zi(jcev1d-1+1)
!
!
!     2- ON CREE 1 CHAM_ELEM_S VIERGE AUX BONNES DIMENSIONS (CESV2):
!     ---------------------------------------------------------------
    varplu='&&VRCOM2.VARPLU'
    call alchml(ligrep, 'RAPH_MECA', 'PVARIPR', 'V', varplu,&
                iret, compop)
    cesv2='&&VARCOM2.CESV2'
    call celces(varplu, 'V', cesv2)
    call detrsd('CHAM_ELEM', varplu)
!
!
!
!     3- ON RECOPIE DE CESV1 VERS CESV2 :
!     -----------------------------------
    call jeveuo(cesv2//'.CESD', 'L', jcev2d)
    call jeveuo(cesv2//'.CESV', 'E', vr=cev2v)
    call jeveuo(cesv2//'.CESL', 'E', jcev2l)
!
    coto='&&VRCOM2.COTO'
    copp='&&VRCOM2.COPP'
!
    call carces(compop, 'ELEM', ' ', 'V', coto,&
                'A', iret)
    call cesred(coto,0,[0],1,'RELCOM',&
                'V', copp)
    call detrsd('CHAM_ELEM_S', coto)
!
    call jeveuo(copp//'.CESD', 'L', jcoppd)
    call jeveuo(copp//'.CESV', 'L', jcoppv)
    call jeveuo(copp//'.CESL', 'L', jcoppl)
!
    do ima=1,nbma
        nbpg2=zi(jcev2d-1+5+4*(ima-1)+1)
        nbsp2=zi(jcev2d-1+5+4*(ima-1)+2)
        nbcm2=zi(jcev2d-1+5+4*(ima-1)+3)
!
!
!       -- SI NBSP2=0, C'EST QUE LA MAILLE N'EXISTE PLUS
!          DANS LE MODELE :
        if (nbsp2 .eq. 0) goto 40
!
        call cesexi('C', jcoppd, jcoppl, ima, 1,&
                    1, 1, iadp)
        if (iadp .le. 0) goto 40
!
        nbsp1=zi(jcev1d-1+5+4*(ima-1)+2)
        nbcm1=zi(jcev1d-1+5+4*(ima-1)+3)
!
!       -- PARFOIS LE COMPORTEMENT EST AFFECTE SUR LES MAILLES
!          DE BORD ALORS QUE CES ELEMENTS N'ONT PAS DE VARIABLES
!          INTERNES (I.E. ILS IGNORENT RAPH_MECA).
!          ON NE VEUT PAS FAIRE D'ERREUR <F> :
        if ((nbsp1.eq.0) .and. (nbcm1.eq.0)) goto 40
!
        ASSERT(nbsp2.eq.nbsp1)
!
        if (nbcm1 .eq. nbcm2) then
            action=1
        else
            action=2
        endif
!
        do ipg=1,nbpg2
            do isp=1,nbsp2
                do icm=1,nbcm2
                    call cesexi('S', jcev2d, jcev2l, ima, ipg,&
                                isp, icm, iad2)
                    ASSERT(iad2.gt.0)
                    zl(jcev2l-1+iad2)=.true.
                    if (action .eq. 1 .or. &
                        ((action.eq.2) .and. icm.le.nbcm1 .and. from_lire_resu)) then
                            call cesexi('S', jcev1d, jcev1l, ima, ipg, isp, icm, iad1)
                            ASSERT(iad1.gt.0)
                            cev2v(iad2)=cev1v(iad1)
                    else
                        cev2v(iad2)=0.d0
                    endif
                end do
            end do
        end do
 40     continue
    end do
!
!
!     4- ON TRANSFORME CESV2 EN CHAM_ELEM (VARMOI)
!     --------------------------------------------------
    call detrsd('CHAM_ELEM', varmoi)
    call cescel(cesv2, ligrep, 'RAPH_MECA', 'PVARIMR', 'OUI', nncp, base, varmoi, 'F', ibid)
!
!     4. MENAGE :
!     -----------
    call detrsd('CHAM_ELEM_S', cesv1)
    call detrsd('CHAM_ELEM_S', cesv2)
    call detrsd('CHAM_ELEM_S', copp)
99  continue
    call jedema()
end subroutine
