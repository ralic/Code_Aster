subroutine iredsu(macr, form, ifc, versio)
    implicit none
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
#include "asterfort/rsexch.h"
#include "asterfort/rslipa.h"
#include "asterfort/wkvect.h"
!
    integer :: ifc, versio
    character(len=*) :: macr, form
! ----------------------------------------------------------------------
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
    integer :: ie, iero, ifor, im, imat
    integer :: in, ind, inoe, inoeu, iord, iret, is, is2, ityp, i2
    integer :: j, k, m2, nbordr, nstat
    integer :: jmasg, jmasj, jmst, jordr, jnoeu, jpars, jpari
    integer :: jrefe, jrigj, jrigg
    integer :: knoeu, kmass, krigi
    integer :: nbnoeu, nbmodt, nbmode, nbmods
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
    logical :: f, lbid
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
    call jeveuo(macrel//'.MAEL_REFE', 'L', jrefe)
    basemo = zk24(jrefe)
    noma = zk24(jrefe+1)
    manono = noma//'.NOMNOE'
    call dismoi('F', 'NB_NO_MAILLA', noma, 'MAILLAGE', nbnoeu,&
                k8b, ie)
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
        call wkvect('&&IREDSU.NOEUDS', 'V V K8', nbnoeu, knoeu)
        inoeu = 1
        zk8(knoeu) = zk16(jnoeu+nbmode)
        do im = 2, nbmods
            noeu = zk16(jnoeu+nbmode+im-1)
            do j = 1, inoeu
                if (noeu .eq. zk8(knoeu+j-1)) goto 20
            end do
            inoeu = inoeu + 1
            zk8(knoeu+inoeu-1) = noeu
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
                noeu = zk8(knoeu+in-1)
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
    call jeveuo(basemo//'.ORDR', 'L', jordr)
    call jelira(basemo//'.ORDR', 'LONMAX', nbordr)
    call wkvect('&&IREDSU.MODE_STAT', 'V V K24', nbordr, jmst)
    nstat = 0
    do i = 1, nbordr
        iord = zi(jordr+i-1)
        if (zk16(jnoeu+i-1) .ne. ' ') then
            call rsexch(' ', basemo, 'DEPL', iord, noch19,&
                        iret)
            if (iret .eq. 0) then
                nstat = nstat + 1
                zk24(jmst+nstat-1) = noch19
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
                        1, [iord], .true., b, iero,&
                        cecr, k8b, f, 0, [0],&
                        0, [0], iero, k8b, f,&
                        zero, f, zero, f, f,&
                        formar, nive, versio)
        endif
    end do
    if (nstat .ne. 0) then
        write (ifc,'(A)') '    -1'
        write (ifc,'(A)') '   481'
        write (ifc,'(I10)') 1
        write (ifc,'(40A2)') 'Ps', 'i_', 'a '
        write (ifc,'(A)') '    -1'
        call irmad0(ifc, versio, nstat, zk24(jmst), nomsym)
    endif
    call jedetr('&&IREDSU.MODE_STAT')
!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!                 --- IMPRESSION DES MATRICES MODALES ---
!
!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    call wkvect('&&IREDSU.MASS_GENE', 'V V R', nbmode*nbmode, jmasg)
    call wkvect('&&IREDSU.RIGI_GENE', 'V V R', nbmode*nbmode, jrigg)
    if (nbmods .ne. 0) then
        call wkvect('&&IREDSU.MASS_JONC', 'V V R', nbmods*nbmods, jmasj)
        call wkvect('&&IREDSU.RIGI_JONC', 'V V R', nbmods*nbmods, jrigj)
        call wkvect('&&IREDSU.PART_SUPE', 'V V R', nbmode*nbmods, jpars)
        call wkvect('&&IREDSU.PART_INFE', 'V V R', nbmode*nbmods, jpari)
    endif
!
    call jeveuo(macrel//'.MAEL_MASS_VALE', 'L', kmass)
    call jeveuo(macrel//'.MAEL_RAID_VALE', 'L', krigi)
    do im = 1, nbmode
        do i = 1, im
            k =im*(im-1)/2 + i
            zr(jmasg+i-1+(im-1)*nbmode) = zr(kmass+k-1)
            zr(jmasg+im-1+(i-1)*nbmode) = zr(kmass+k-1)
            zr(jrigg+i-1+(im-1)*nbmode) = zr(krigi+k-1)
            zr(jrigg+im-1+(i-1)*nbmode) = zr(krigi+k-1)
        end do
    end do
    do is = nbmode+1, nbmodt
        do im = 1, nbmode
            k = is*(is-1)/2 + im
            is2 = is - nbmode
            zr(jpars+is2-1+(im-1)*nbmods) = zr(kmass+k-1)
            zr(jpari+is2-1+(im-1)*nbmods) = zr(kmass+k-1)
        end do
        do i = nbmode+1, is
            k = is*(is-1)/2 + i
            i2 = i - nbmode
            is2 = is - nbmode
            zr(jmasj+i2-1+(is2-1)*nbmods) = zr(kmass+k-1)
            zr(jmasj+is2-1+(i2-1)*nbmods) = zr(kmass+k-1)
            zr(jrigj+i2-1+(is2-1)*nbmods) = zr(krigi+k-1)
            zr(jrigj+is2-1+(i2-1)*nbmods) = zr(krigi+k-1)
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
        write (ifc,1000) (zr(jmasg+i) , i= 0, m2-1 )
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
        write (ifc,1000) (zr(jrigg+i) , i= 0, m2-1 )
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
            write (ifc,1000) (zr(jmasj+i) , i= 0, m2-1 )
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
            write (ifc,1000) (zr(jrigj+i) , i= 0, m2-1 )
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
            write (ifc,1000) (zr(jpari+i) , i= 0, m2-1 )
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
    call jedetr('&&IREDSU.NOEUDS')
    call jedetr('&&IREDSU.MASS_GENE')
    call jedetr('&&IREDSU.RIGI_GENE')
    call jedetr('&&IREDSU.MASS_JONC')
    call jedetr('&&IREDSU.RIGI_JONC')
    call jedetr('&&IREDSU.PART_SUPE')
    call jedetr('&&IREDSU.PART_INFE')
!
    call jedema()
end subroutine
