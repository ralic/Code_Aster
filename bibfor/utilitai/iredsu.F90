subroutine iredsu(macr, form, ifc, versio)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/irecri.h"
#include "asterfort/irmad0.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/rsexch.h"
#include "asterfort/rslipa.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    integer :: ifc, versio
    character(len=*) :: macr, form
! ----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     BUT:
!       IMPRESSION D'UN CONCEPT MACR_ELEM_DYNA AU FORMAT "IDEAS"
!       ATTENTION: le dataset 481 est en minuscules.
!
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
! IN   MACR      : NOM DU CONCEPT MACR_ELEM_DYNA
! IN   FORM      : FORMAT D'ECRITURE
! IN   IFC       : UNITE LOGIQUE D'ECRITURE
! IN   VERSIO    : VERSION D'IMPRESSION
!
!      SORTIE :
!-------------
!
! ......................................................................
!
!
!
!
    integer :: nive
    integer :: i, icol, idrx, idry, idrz, idx, idy, idz
    integer :: iero, ifor, im, imat
    integer :: in, ind, inoe, inoeu, iord, iret, is, is2, ityp, i2
    integer :: j, k, m2, nbordr, nstat
    integer :: jnoeu
    integer :: nbnoeu, nbmodt, nbmode, nbmods, ntriar, ntriam
!
    real(kind=8) :: zero
!
    character(len=1) :: b, cecr
    character(len=8) :: k8b, macrel, noma, noeu, cmp, formar
    character(len=16) :: nomsym
    character(len=19) :: basemo, noch19
    character(len=24) :: manono
    character(len=80) :: titre
!
    aster_logical :: f, lbid
    real(kind=8), pointer :: mass_gene(:) => null()
    real(kind=8), pointer :: mass_jonc(:) => null()
    character(len=24), pointer :: mode_stat(:) => null()
    character(len=8), pointer :: noeuds(:) => null()
    real(kind=8), pointer :: part_infe(:) => null()
    real(kind=8), pointer :: part_supe(:) => null()
    real(kind=8), pointer :: rigi_gene(:) => null()
    real(kind=8), pointer :: rigi_jonc(:) => null()
    real(kind=8), pointer :: mael_raid_vale(:) => null()
    real(kind=8), pointer :: mael_raid_vali(:) => null()
    character(len=24), pointer :: mael_refe(:) => null()
    integer, pointer :: ordr(:) => null()
    real(kind=8), pointer :: mael_mass_vale(:) => null()
    real(kind=8), pointer :: mael_mass_vali(:) => null()
!
!-----------------------------------------------------------------------
!
    call jemarq()
!
    zero = 0.d0
    iero = 0
    cecr = 'L'
    b = ' '
    f = .false.
    macrel = macr
    formar = '1PE12.5'
    nive = 3
!
    call jeveuo(macrel//'.MAEL_REFE', 'L', vk24=mael_refe)
    basemo = mael_refe(1)
    noma = mael_refe(2)
    manono = noma//'.NOMNOE'
    call dismoi('NB_NO_MAILLA', noma, 'MAILLAGE', repi=nbnoeu)
    call rslipa(basemo, 'NOEUD_CMP', '&&IREDSU.LINOEU', jnoeu, nbmodt)
!
    do im = 1, nbmodt
        if (zk16(jnoeu+im-1) .ne. ' ') goto 12
    end do
 12 continue
    nbmode = im - 1
    nbmods = nbmodt - nbmode
!
!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!                    --- IMPRESSION DES DDL DE JONCTION ---
!
!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    if (nbmods .ne. 0) then
        AS_ALLOCATE(vk8=noeuds, size=nbnoeu)
        inoeu = 1
        noeuds(1) = zk16(jnoeu+nbmode)
        do im = 2, nbmods
            noeu = zk16(jnoeu+nbmode+im-1)
            do j = 1, inoeu
                if (noeu .eq. noeuds(j)) goto 20
            end do
            inoeu = inoeu + 1
            noeuds(inoeu) = noeu
 20         continue
        end do
        if (versio .eq. 5) then
            write (ifc,'(A)') '    -1'
            write (ifc,'(A)') '   481'
            write (ifc,'(I10)') 1
            write (ifc,'(40A2)') 'Ju', 'nc'
            write (ifc,'(A)') '    -1'
            ind = 1
            icol = 7
            write (ifc,'(A)') '    -1'
            write (ifc,'(A)') '   757'
            write (ifc,'(2I10)') ind
            write (ifc,'(A)') 'DDL JONCTION'
            do in = 1, inoeu
                noeu = noeuds(in)
                call jenonu(jexnom(manono, noeu), inoe)
                idx = 0
                idy = 0
                idz = 0
                idrx = 0
                idry = 0
                idrz = 0
                do im = 1, nbmods
                    if (noeu .eq. zk16(jnoeu+nbmode+im-1)(1:8)) then
                        cmp = zk16(jnoeu+nbmode+im-1)(9:16)
                        if (cmp .eq. 'DX      ') then
                            idx = 1
                        else if (cmp .eq. 'DY      ') then
                            idy = 1
                        else if (cmp .eq. 'DZ      ') then
                            idz = 1
                        else if (cmp .eq. 'DRX     ') then
                            idrx = 1
                        else if (cmp .eq. 'DRY     ') then
                            idry = 1
                        else if (cmp .eq. 'DRZ     ') then
                            idrz = 1
                        endif
                    endif
                end do
                write (ifc,'(2I10,6I2)') inoe, icol, idx, idy, idz,&
                idrx, idry, idrz
            end do
            write (ifc,'(A)') '    -1'
        endif
    endif
!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!                 --- IMPRESSION DES MODES DYNAMIQUES ---
!                 --- IMPRESSION DES MODES STATIQUES ---
!
!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    nomsym = 'DEPL'
    call jeveuo(basemo//'.ORDR', 'L', vi=ordr)
    call jelira(basemo//'.ORDR', 'LONMAX', nbordr)
    AS_ALLOCATE(vk24=mode_stat, size=nbordr)
    nstat = 0
    do i = 1, nbordr
        iord = ordr(i)
        if (zk16(jnoeu+i-1) .ne. ' ') then
            call rsexch(' ', basemo, 'DEPL', iord, noch19,&
                        iret)
            if (iret .eq. 0) then
                nstat = nstat + 1
                mode_stat(nstat) = noch19
            endif
        else
            write (ifc,'(A)') '    -1'
            write (ifc,'(A)') '   481'
            write (ifc,'(I10)') 1
            write (ifc,'(40A2)') 'Ph', 'i_', 'a '
            write (ifc,'(A)') '    -1'
            titre = 'MODE DYNAMIQUE'
            call irecri(basemo, form, ifc, titre, lbid,&
                        1, 'DEPL', ' ', iero, k8b,&
                        1, [iord], .true._1, b, iero,&
                        cecr, k8b, f, 0, [0],&
                        0, [0], iero, k8b, f,&
                        zero, f, zero, f, f,&
                        formar, nive, versio, 2)
        endif
    end do
    if (nstat .ne. 0) then
        write (ifc,'(A)') '    -1'
        write (ifc,'(A)') '   481'
        write (ifc,'(I10)') 1
        write (ifc,'(40A2)') 'Ps', 'i_', 'a '
        write (ifc,'(A)') '    -1'
        call irmad0(ifc, versio, nstat, mode_stat, nomsym)
    endif
    AS_DEALLOCATE(vk24=mode_stat)
!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!                 --- IMPRESSION DES MATRICES MODALES ---
!
!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    AS_ALLOCATE(vr=mass_gene, size=nbmode*nbmode)
    AS_ALLOCATE(vr=rigi_gene, size=nbmode*nbmode)
    if (nbmods .ne. 0) then
        AS_ALLOCATE(vr=mass_jonc, size=nbmods*nbmods)
        AS_ALLOCATE(vr=rigi_jonc, size=nbmods*nbmods)
        AS_ALLOCATE(vr=part_supe, size=nbmode*nbmods)
        AS_ALLOCATE(vr=part_infe, size=nbmode*nbmods)
    endif
!
    call jelira(macrel//'.MAEL_MASS_VALE','NMAXOC',ntriam)
    call jeveuo(jexnum(macrel//'.MAEL_MASS_VALE', 1), 'L', vr=mael_mass_vale)
    if (ntriam.gt.1) then
     call jeveuo(jexnum(macrel//'.MAEL_MASS_VALE', 2), 'L', vr=mael_mass_vali)
    else
     call jeveuo(jexnum(macrel//'.MAEL_MASS_VALE', 1), 'L', vr=mael_mass_vali)
    end if
!   call jeveuo(macrel//'.MAEL_MASS_VALE', 'L', vr=mael_mass_vale)
!
    call jelira(macrel//'.MAEL_RAID_VALE','NMAXOC',ntriar)
    call jeveuo(jexnum(macrel//'.MAEL_RAID_VALE', 1), 'L', vr=mael_raid_vale) 
    if (ntriar.gt.1) then 
     call jeveuo(jexnum(macrel//'.MAEL_RAID_VALE', 2), 'L', vr=mael_raid_vali)
    else
     call jeveuo(jexnum(macrel//'.MAEL_RAID_VALE', 1), 'L', vr=mael_raid_vali)
    end if
!   call jeveuo(macrel//'.MAEL_RAID_VALE', 'L', vr=mael_raid_vale)
!
    do im = 1, nbmode
        do i = 1, im
            k =im*(im-1)/2 + i
            mass_gene(1+i-1+(im-1)*nbmode) = mael_mass_vale(k)
            mass_gene(1+im-1+(i-1)*nbmode) = mael_mass_vale(k)
            rigi_gene(1+i-1+(im-1)*nbmode) = mael_raid_vale(k)
            rigi_gene(1+im-1+(i-1)*nbmode) = mael_raid_vale(k)
        end do
    end do
    do is = nbmode+1, nbmodt
        do im = 1, nbmode
            k = is*(is-1)/2 + im
            is2 = is - nbmode
            part_supe(1+is2-1+(im-1)*nbmods) = mael_mass_vale(k)
            part_infe(1+is2-1+(im-1)*nbmods) = mael_mass_vale(k)
        end do
        do i = nbmode+1, is
            k = is*(is-1)/2 + i
            i2 = i - nbmode
            is2 = is - nbmode
            mass_jonc(1+i2-1+(is2-1)*nbmods) = mael_mass_vale(k)
            mass_jonc(1+is2-1+(i2-1)*nbmods) = mael_mass_vale(k)
            rigi_jonc(1+i2-1+(is2-1)*nbmods) = mael_raid_vale(k)
            rigi_jonc(1+is2-1+(i2-1)*nbmods) = mael_raid_vale(k)
        end do
    end do
!
    m2 = nbmode * nbmode
    if (versio .eq. 5) then
        ityp = 4
        ifor = 1
        icol = 2
!
!        --- MASSE GENERALISEE ---
        write (ifc,'(A)') '    -1'
        write (ifc,'(A)') '   481'
        write (ifc,'(I10)') 1
        write (ifc,'(40A2)') 'Mg', 'en', '_a'
        write (ifc,'(A)') '    -1'
        imat = 131
        write (ifc,'(A)') '    -1'
        write (ifc,'(A)') '   252'
        write (ifc,'(I10)') imat
        write (ifc,'(5I10)') ityp, ifor, nbmode, nbmode, icol
        write (ifc,1000) (mass_gene(1+i) , i= 0, m2-1 )
        write (ifc,'(A)') '    -1'
!
!        --- RAIDEUR GENERALISEE ---
        write (ifc,'(A)') '    -1'
        write (ifc,'(A)') '   481'
        write (ifc,'(I10)') 1
        write (ifc,'(40A2)') 'Kg', 'en', '_a'
        write (ifc,'(A)') '    -1'
        imat = 139
        write (ifc,'(A)') '    -1'
        write (ifc,'(A)') '   252'
        write (ifc,'(I10)') imat
        write (ifc,'(5I10)') ityp, ifor, nbmode, nbmode, icol
        write (ifc,1000) (rigi_gene(1+i) , i= 0, m2-1 )
        write (ifc,'(A)') '    -1'
!
        if (nbmods .ne. 0) then
            m2 = nbmods * nbmods
!
!          --- MASSE CONDENSEE A LA JONCTION ---
            write (ifc,'(A)') '    -1'
            write (ifc,'(A)') '   481'
            write (ifc,'(I10)') 1
            write (ifc,'(40A2)') 'Mb', 'ar', '_a'
            write (ifc,'(A)') '    -1'
            imat = 134
            write (ifc,'(A)') '    -1'
            write (ifc,'(A)') '   252'
            write (ifc,'(I10)') imat
            write (ifc,'(5I10)') ityp, ifor, nbmods, nbmods, icol
            write (ifc,1000) (mass_jonc(1+i) , i= 0, m2-1 )
            write (ifc,'(A)') '    -1'
!
!          --- RIGIDITE CONDENSEE A LA JONCTION ---
            write (ifc,'(A)') '    -1'
            write (ifc,'(A)') '   481'
            write (ifc,'(I10)') 1
            write (ifc,'(40A2)') 'Kb', 'ar', '_a'
            write (ifc,'(A)') '    -1'
            imat = 142
            write (ifc,'(A)') '    -1'
            write (ifc,'(A)') '   252'
            write (ifc,'(I10)') imat
            write (ifc,'(5I10)') ityp, ifor, nbmods, nbmods, icol
            write (ifc,1000) (rigi_jonc(1+i) , i= 0, m2-1 )
            write (ifc,'(A)') '    -1'
!
            m2 = nbmode * nbmods
!
!          --- FACTEUR DE PARTICIPATION INFERIEUR ---
            write (ifc,'(A)') '    -1'
            write (ifc,'(A)') '   481'
            write (ifc,'(I10)') 1
            write (ifc,'(40A2)') 'Lm', 'at', '_a'
            write (ifc,'(A)') '    -1'
            imat = 132
            write (ifc,'(A)') '    -1'
            write (ifc,'(A)') '   252'
            write (ifc,'(I10)') imat
            write (ifc,'(5I10)') ityp, ifor, nbmode, nbmods, icol
            write (ifc,1000) (part_infe(1+i) , i= 0, m2-1 )
            write (ifc,'(A)') '    -1'
!
        endif
    endif
!
    1000 format( 1p, 4d20.12 )
!
! --- MENAGE
!
    call jedetr('&&IREDSU.LINOEU')
    AS_DEALLOCATE(vk8=noeuds)
    AS_DEALLOCATE(vr=mass_gene)
    AS_DEALLOCATE(vr=rigi_gene)
    AS_DEALLOCATE(vr=mass_jonc)
    AS_DEALLOCATE(vr=rigi_jonc)
    AS_DEALLOCATE(vr=part_supe)
    AS_DEALLOCATE(vr=part_infe)
!
    call jedema()
end subroutine
