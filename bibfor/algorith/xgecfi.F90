subroutine xgecfi(modele, depgeo)
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
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/calcul.h"
#include "asterfort/celces.h"
#include "asterfort/cescre.h"
#include "asterfort/cesexi.h"
#include "asterfort/copisd.h"
#include "asterfort/dbgcal.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/infdbg.h"
#include "asterfort/inical.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=8) :: modele
    character(len=19) :: depgeo
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM-GG
!
! CREATION DES SD CONTENANT LA GEOMETRIE DES
!                FACETTES DE CONTACT (ESCLAVES ET MAITRE)
!
! ----------------------------------------------------------------------
!
!
! ----------------------------------------------------------------------
! ROUTINE SPECIFIQUE A L'APPROCHE <<GRANDS GLISSEMENTS AVEC XFEM>>,
! TRAVAIL EFFECTUE EN COLLABORATION AVEC I.F.P.
! ----------------------------------------------------------------------
!
! IN  MODELE : NOM DU MODELE
! IN  DEPGEO : CHAMP DE DEPLACEMENTS
!
!
!
!
    integer :: nbout, nbin
    parameter    (nbout=2, nbin=7)
    character(len=8) :: lpaout(nbout), lpain(nbin), licmp(2), noma
    character(len=19) :: lchout(nbout), lchin(nbin)
!
    character(len=16) :: option
    character(len=19) :: ligrel, pinter, faclon, newges, newgem
    character(len=19) :: gesclo, ltno, fissno, heavfa
    character(len=1) :: base
    aster_logical :: debug
    integer :: ifmdbg, nivdbg, iret, nbma, ima
    integer :: jcesd, jcesl, iad
    integer, pointer :: cesv(:) => null()
    character(len=8), pointer :: lgrf(:) => null()
    integer, pointer :: cesd2(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('PRE_CALCUL', ifmdbg, nivdbg)
!
! --- INITIALISATIONS
!
    if (nivdbg .ge. 2) then
        debug = .true.
    else
        debug = .false.
    endif
    base = 'V'
    option = 'GEOM_FAC'
!
! --- INITIALISATION DES CHAMPS POUR CALCUL
!
    call inical(nbin, lpain, lchin, nbout, lpaout,&
                lchout)
    ligrel = modele//'.MODELE'
    pinter = modele//'.TOPOFAC.PI'
    faclon = modele//'.TOPOFAC.LO'
    gesclo = modele//'.TOPOFAC.OE'
    ltno = modele//'.LTNO'
    fissno = modele(1:8)//'.FISSNO'
    heavfa = modele(1:8)//'.TOPOFAC.HE'
    newges = modele//'.TOPOFAC.GE'
    newgem = modele//'.TOPOFAC.GM'
    call jeexin(newges//'.CESD', iret)
    if (iret .eq. 0) then
!
! --- RECOPIE DU NOMBRE DE SOUS POINTS DE TOPOFAC.OE DANS TOPOFAC.GE/M
!
        call jeveuo(modele//'.MODELE    .LGRF', 'L', vk8=lgrf)
        noma = lgrf(1)
        call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbma)
        call celces(gesclo, 'V', '&&XGECFI.GESCLO')
        call jeveuo('&&XGECFI.GESCLO    .CESD', 'L', vi=cesd2)
        licmp(1) = 'NPG_DYN'
        licmp(2) = 'NCMP_DYN'
        call cescre('V', newges, 'ELEM', noma, 'DCEL_I',&
                    2, licmp, [0], [-1], [-2])
        call jeveuo(newges//'.CESD', 'L', jcesd)
        call jeveuo(newges//'.CESV', 'E', vi=cesv)
        call jeveuo(newges//'.CESL', 'E', jcesl)
        do ima = 1, nbma
            call cesexi('S', jcesd, jcesl, ima, 1,&
                        1, 1, iad)
            zl(jcesl-1-iad) = .true.
            cesv(1-1-iad) = cesd2(5+4*(ima-1)+2)
        end do
        call copisd('CHAM_ELEM_S', 'V', newges, newgem)
        call detrsd('CHAM_ELEM_S', '&&XGECFI.GESCLO')
    endif
!
! --- CREATION DES LISTES DES CHAMPS IN ET OUT
!
    lpain(1) = 'PDEPLA'
    lchin(1) = depgeo
    lpain(2) = 'PPINTER'
    lchin(2) = pinter
    lpain(3) = 'PLONGCO'
    lchin(3) = faclon
    lpain(4) = 'PGESCLO'
    lchin(4) = gesclo
    lpain(5) = 'PLST'
    lchin(5) = ltno
    lpain(6) = 'PFISNO'
    lchin(6) = fissno
    lpain(7) = 'PHEAVFA'
    lchin(7) = heavfa
!
    lpaout(1) = 'PNEWGES'
    lchout(1) = newges
    lpaout(2) = 'PNEWGEM'
    lchout(2) = newgem
!
! --- APPEL A CALCUL
!
    call calcul('C', option, ligrel, nbin, lchin,&
                lpain, nbout, lchout, lpaout, base,&
                'OUI')
    if (debug) then
        call dbgcal(option, ifmdbg, nbin, lpain, lchin,&
                    nbout, lpaout, lchout)
    endif
!
    call jedema()
end subroutine
