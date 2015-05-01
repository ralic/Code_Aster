subroutine ssmage(nomu, option)
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
    implicit none
!
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/assmam.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/memame.h"
#include "asterfort/rcmfmc.h"
#include "asterfort/sdmpic.h"
#include "asterfort/ssmau2.h"
#include "asterfort/ualfcr.h"
#include "asterfort/utmess.h"
    character(len=8) :: nomu
    character(len=9) :: option
! ----------------------------------------------------------------------
!     BUT: TRAITER LE MOT CLEF "MASS_MECA" (RESP. "AMOR_MECA")
!             DE L'OPERATEUR MACR_ELEM_STAT
!           CALCULER LA MATRICE DE MASSE (OU AMORTISSEMENT)
!           CONDENSEE DU MACR_ELEM_STAT.
!
!     IN: NOMU   : NOM DU MACR_ELEM_STAT
!         OPTION : 'MASS_MECA' OU 'AMOR_MECA'
!
!     OUT: LES OBJETS SUIVANTS DU MACR_ELEM_STAT SONT CALCULES:
!           / NOMU.MAEL_MASS_VALE (SI MASS_MECA)
!           / NOMU.MAEL_AMOR_VALE (SI AMOR_MECA)
!
! ----------------------------------------------------------------------
!
!
    integer :: nchaci
    real(kind=8) :: time
    character(len=1) :: base
    character(len=8) :: nomo, cara, materi, matel, promes
    character(len=14) :: nu
    character(len=19) :: matas
    character(len=24) :: mate, compor
!-----------------------------------------------------------------------
    integer :: iarefm
    integer, pointer :: desm(:) => null()
    real(kind=8), pointer :: varm(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
!
! --- ON CREER LES OBJETS DE TRAVAIL SUR LA VOLATILE
    base = 'V'
!
    call jeveuo(nomu//'.REFM', 'E', iarefm)
    nomo = zk8(iarefm-1+1)
    cara = zk8(iarefm-1+4)
    materi = zk8(iarefm-1+3)
!
    if (materi .eq. '        ') then
        mate = ' '
    else
        call rcmfmc(materi, mate)
    endif
    nu= zk8(iarefm-1+5)
    if (nu(1:8) .ne. nomu) then
        ASSERT(.false.)
    endif
!
    matel = '&&MATEL'
    if (option .eq. 'MASS_MECA') then
        matas = nomu//'.MASSMECA'
    else if (option.eq.'AMOR_MECA') then
        matas = nomu//'.AMORMECA'
    else
        ASSERT(.false.)
    endif
!
    call jeveuo(nomu//'.DESM', 'L', vi=desm)
    nchaci = desm(6)
!
!
    call jeveuo(nomu//'.VARM', 'L', vr=varm)
    time = varm(2)
!
!     -- CALCULS MATRICES ELEMENTAIRES DE MASSE (OU AMORTISSEMENT):
    if (option .eq. 'MASS_MECA') then
        compor = ' '
        call memame('MASS_MECA  ', nomo, nchaci, zk8(iarefm-1+9+1), mate,&
                    cara, .true._1, time, compor, matel,&
                    base)
    else if (option.eq.'AMOR_MECA') then
        call dismoi('NOM_PROJ_MESU', nomu, 'MACR_ELEM_STAT', repk=promes)
!     --  CAS MODIFICATION STRUCTURALE : CREATION MATRICE PAR SSMAU2
        if (promes .eq. ' ') then
            call utmess('F', 'SOUSTRUC_69')
        endif
    else
        call utmess('F', 'SOUSTRUC_69')
    endif
!
!        -- ASSEMBLAGE:
    if (option .eq. 'MASS_MECA') then
        call assmam('G', matas, 1, matel, [1.d0],&
                    nu, 'ZERO', 1)
!       -- IL FAUT COMPLETER LA MATRICE SI LES CALCULS SONT DISTRIBUES:
        call sdmpic('MATR_ASSE', matas)
        call ualfcr(matas, 'G')
    endif
    call ssmau2(nomu, option)
!
!        -- MISE A JOUR DE .REFM(7) OU REFM(8)
    if (option .eq. 'MASS_MECA') then
        zk8(iarefm-1+7)='OUI_MASS'
    else if (option.eq.'AMOR_MECA') then
        zk8(iarefm-1+8)='OUI_AMOR'
    else
        call utmess('F', 'SOUSTRUC_69')
    endif
!
    if (option .eq. 'MASS_MECA') then
        call jedetr(matel)
    endif
    call jedema()
!
end subroutine
