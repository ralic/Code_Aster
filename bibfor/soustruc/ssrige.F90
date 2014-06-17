subroutine ssrige(nomu)
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
    implicit none
!
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
#include "asterfort/assmam.h"
#include "asterfort/crnslv.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/merime.h"
#include "asterfort/numddl.h"
#include "asterfort/promor.h"
#include "asterfort/rcmfmc.h"
#include "asterfort/sdmpic.h"
#include "asterfort/smosli.h"
#include "asterfort/ssriu1.h"
#include "asterfort/ssriu2.h"
    character(len=8) :: nomu
! ----------------------------------------------------------------------
!     BUT: TRAITER LE MOT CLEF "RIGI_MECA" DE L'OPERATEUR MACR_ELEM_STAT
!          CALCULER LA MATRICE DE RIGIDITE CONDENSEE DU MACR_ELEM_STAT.
!
!     IN: NOMU   : NOM DU MACR_ELEM_STAT
!
!     OUT: LES OBJETS SUIVANTS DU MACR_ELEM_STAT SONT CALCULES:
!          .PHI_IE ET .KP_EE
!
! ----------------------------------------------------------------------
!
!
    integer :: nchaci
    real(kind=8) :: rtbloc
    character(len=1) :: base
    character(len=8) :: nomo, cara, materi
    character(len=14) :: nu
    character(len=19) :: matel, matas
    character(len=24) :: mate
!
!-----------------------------------------------------------------------
    integer :: iarefm, ibid
    real(kind=8) :: time
    integer, pointer :: desm(:) => null()
    real(kind=8), pointer :: varm(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    nu = nomu
    base = 'V'
!
    matel = '&&MATEL'
    matas = nomu//'.RIGIMECA'
!
    call jeveuo(nomu//'.DESM', 'L', vi=desm)
    nchaci = desm(6)
!
    call jeveuo(nomu//'.REFM', 'E', iarefm)
    nomo = zk8(iarefm-1+1)
    cara = zk8(iarefm-1+4)
    materi = zk8(iarefm-1+3)
    if (materi .eq. '        ') then
        mate = ' '
    else
        call rcmfmc(materi, mate)
    endif
!
    call jeveuo(nomu//'.VARM', 'L', vr=varm)
    time = varm(2)
!
!        -- CALCULS MATRICES ELEMENTAIRES DE RIGIDITE:
    call merime(nomo, nchaci, zk8(iarefm-1+9+1), mate, cara,&
                .true., time, ' ', matel, ibid,&
                base)
!
!        -- NUME_DDL:
    call numddl(nu, 'GG', 1, matel, 'RCMK')
    call crnslv(nu, 'LDLT', 'RCMK', 'G')
!
!        -- ON MET LES DDLS INTERNES AVANT LES EXTERNES
!           AVANT DE CONSTRUIRE LE PROFIL :
    call ssriu1(nomu)
    call promor(nu, 'G')
    rtbloc=varm(1)
    call smosli(nu//'.SMOS', nu//'.SLCS', 'G', rtbloc)
!
!        -- ASSEMBLAGE:
    call assmam('G', matas, 1, matel, [1.d0],&
                nu, 'ZERO', 1)
!     -- IL FAUT COMPLETER LA MATRICE SI LES CALCULS SONT DISTRIBUES:
    call sdmpic('MATR_ASSE', matas)
!
!
    call ssriu2(nomu)
!
!        -- MISE A JOUR DE .REFM(5) ET REFM(6)
    zk8(iarefm-1+5)=nu(1:8)
    zk8(iarefm-1+6)='OUI_RIGI'
!
!
    call jedetr(nomu//'      .NEWN')
    call jedetr(nomu//'      .OLDN')
    call jedetr(nu//'     .ADNE')
    call jedetr(nu//'     .ADLI')
    call jedetr(matas(1:19)//'.LILI')
    call detrsd('MATR_ELEM', matel)
!
    call jedema()
end subroutine
