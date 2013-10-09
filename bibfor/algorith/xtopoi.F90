subroutine xtopoi(noma, modele)
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
! person_in_charge: samuel.geniaut at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/calcul.h"
#include "asterfort/cescre.h"
#include "asterfort/cesexi.h"
#include "asterfort/dbgcal.h"
#include "asterfort/dismoi.h"
#include "asterfort/infdbg.h"
#include "asterfort/inical.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=8) :: noma, modele
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM (METHODE XFEM - PREPARATION)
!
! AJOUTER À LA SD FISS_XFEM LES DONNÉES TOPOLOGIQUES CONCERNANT
! LA DÉCOUPE DES ÉLÉMENTS POUR L'INTÉGRATION
!
! ----------------------------------------------------------------------
!
!
!  IN  MODELE : NOM DE L'OBJET MODELE
!  I/O FISS   : NOM DE LA SD FISS_XFEM
!
!
!
!
!
    integer :: nbout, nbin
    parameter    (nbout=5, nbin=4)
    character(len=8) :: lpaout(nbout), lpain(nbin), licmp(2)
    character(len=19) :: lchout(nbout), lchin(nbin)
!
    integer :: jnoma
    character(len=19) :: ligrel, chgeom
    character(len=19) :: pintto, cnseto, heavto, loncha, pmilto
    logical :: debug
    integer :: ifm, niv, ifmdbg, nivdbg, jnbsp, ima, nbma
    integer :: jcesd, jcesv, jcesl, iad
    character(len=16) :: option
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('XFEM', ifm, niv)
    call infdbg('PRE_CALCUL', ifmdbg, nivdbg)
!
! --- INITIALISATIONS
!
    ligrel = modele//'.MODELE'
    option = 'TOPOSE'
    if (nivdbg .ge. 2) then
        debug = .true.
    else
        debug = .false.
    endif
    call jeveuo(modele//'.MODELE    .LGRF', 'L', jnoma)
    chgeom = zk8(jnoma)//'.COORDO'
!
! --- INITIALISATION DES CHAMPS POUR CALCUL
!
    call inical(nbin, lpain, lchin, nbout, lpaout,&
                lchout)
!
! --- RECUPERATION DES DONNEES XFEM (TOPOSE)
!
    pintto = modele(1:8)//'.TOPOSE.PIN'
    cnseto = modele(1:8)//'.TOPOSE.CNS'
    heavto = modele(1:8)//'.TOPOSE.HEA'
    loncha = modele(1:8)//'.TOPOSE.LON'
    pmilto = modele(1:8)//'.TOPOSE.PMI'
!
    licmp(1) = 'NPG_DYN'
    licmp(2) = 'NCMP_DYN'
    call cescre('V', heavto, 'ELEM', noma, 'DCEL_I',&
                2, licmp, [0], [-1], [-2])
    call jeveuo(heavto//'.CESD', 'L', jcesd)
    call jeveuo(heavto//'.CESV', 'E', jcesv)
    call jeveuo(heavto//'.CESL', 'E', jcesl)
!
! --- RECUPERATION DU NOMBRE DE FISSURES VUES
!
    call jeveuo('&&XTYELE.NBSP', 'L', jnbsp)
!
! --- REMPLISSAGE DES SOUS POINT DE HEAVTO
!
    call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbma)
    do 10 ima = 1, nbma
        call cesexi('S', jcesd, jcesl, ima, 1,&
                    1, 1, iad)
        zl(jcesl-1-iad) = .true.
        zi(jcesv-1-iad) = zi(jnbsp-1+ima)
!
 10 end do
!
! --- CREATION DES LISTES DES CHAMPS IN
!
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom
    lpain(2) = 'PLEVSET'
    lchin(2) = modele(1:8)//'.LNNO'
    lpain(3) = 'PGRADLN'
    lchin(3) = modele(1:8)//'.GRLNNO'
    lpain(4) = 'PFISCO'
    lchin(4) = modele(1:8)//'.FISSCO'
!
! --- CREATION DES LISTES DES CHAMPS OUT
!
    lpaout(1) = 'PPINTTO'
    lchout(1) = pintto
    lpaout(2) = 'PCNSETO'
    lchout(2) = cnseto
    lpaout(3) = 'PHEAVTO'
    lchout(3) = heavto
    lpaout(4) = 'PLONCHA'
    lchout(4) = loncha
    lpaout(5) = 'PPMILTO'
    lchout(5) = pmilto
!
! --- APPEL A CALCUL
!
    call calcul('C', option, ligrel, nbin, lchin,&
                lpain, nbout, lchout, lpaout, 'G',&
                'OUI')
!
    if (debug) then
        call dbgcal(option, ifmdbg, nbin, lpain, lchin,&
                    nbout, lpaout, lchout)
    endif
!
    call jedema()
end subroutine
