subroutine nmmoam(sdammz, nbmoda)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
#include "jeveux.h"
#include "asterc/getexm.h"
#include "asterc/r8pi.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mginfo.h"
#include "asterfort/mrmult.h"
#include "asterfort/mtdscr.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/zerlag.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
#include "blas/dcopy.h"
    character(len=*) :: sdammz
    integer :: nbmoda
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! CREATION SD AMORTISSEMENT MODAL
!
! ----------------------------------------------------------------------
!
!
! IN  SDAMMO : SD DEDIEE A L'AMORTISSEMENT MODAL
!               OUT - VALMOD - VALEURS MODALES
!                       1/ MASSES GENERALISEES
!                       2/ PULSATIONS PROPRES
!                       3/ AMORTISSEMENT MODAL
!               OUT - BASMOD - BASE MODALE
! OUT NBMODA : NOMBRE DE MODES PRIS POUR l'AMORTISSEMENT
!
!
!
!
!
!
!
    character(len=8) :: k8bid, modmec, listam
    character(len=14) :: numddl
    character(len=24) :: deeq
    character(len=24) :: matric, nomcha
    character(len=24) :: sdammo
    real(kind=8) :: pi
    integer :: iret, iam, imode, vali(3)
    integer :: na, nb, n, nm
    integer :: nbmd, neq, nbmax, nbamor
    integer :: iddeeq, lmat
    integer :: jvalmo, jbasmo, jamor,  jamo2, jmasg, jfreq
    integer :: exiam
    real(kind=8), pointer :: vect1(:) => null()
    real(kind=8), pointer :: mor(:) => null()
    real(kind=8), pointer :: val(:) => null()
!
! ---------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    exiam = 0
    pi = r8pi()
    nbmoda = 0
    sdammo = sdammz
!
! --- MATRICE DES MODES MECA
!
    call getvid('AMOR_MODAL', 'MODE_MECA', iocc=1, scal=modmec, nbret=nbmd)
    if (nbmd .eq. 0) then
        call utmess('F', 'ALGORITH17_20')
    endif
!
! --- INFORMATIONS SUR MATRICE DES MODES MECANIQUES
!
    call mginfo(modmec, numddl, nbmoda, neq)
    deeq = numddl//'.NUME.DEEQ'
    call jeveuo(deeq, 'L', iddeeq)
!
! --- ALLOCATION DESCRIPTEUR DE LA MATRICE
!
    call dismoi('REF_RIGI_PREM', modmec, 'RESU_DYNA', repk=matric)
    call mtdscr(matric(1:8))
    call jeveuo(matric(1:19)//'.&INT', 'E', lmat)
!
! --- NOMBRE DE MODES
!
    call getvis('AMOR_MODAL', 'NB_MODE', iocc=1, scal=nbmax, nbret=nm)
    if (nbmax .ne. nbmoda) then
        vali(1) = nbmoda
        vali(2) = nbmax
        vali(3) = min(nbmoda,nbmax)
        call utmess('I', 'MECANONLINE5_30', ni=3, vali=vali)
        nbmoda = min(nbmoda,nbmax)
    endif
!
! --- RECUPERATION DES AMORTISSEMENTS
!
    call wkvect('&&NMMOAM.AMORTISSEMENT', 'V V R', nbmoda, jamor)
!
    na = 0
    nb = 0
    call getvr8('AMOR_MODAL', 'AMOR_REDUIT', iocc=1, nbval=0, nbret=na)
    exiam = getexm('AMOR_MODAL','LIST_AMOR')
    if (exiam .eq. 1) then
        call getvid('AMOR_MODAL', 'LIST_AMOR', iocc=1, nbval=0, nbret=nb)
    endif
!
!     VERIFICATION QU'UNE LISTE D'AMORTISSEMENTS EST FOURNIE
    if (na .eq. 0 .and. nb .eq. 0) then
        call utmess('F', 'ALGORITH17_21')
    endif
!
    if (na .ne. 0 .or. nb .ne. 0) then
        if (na .ne. 0) then
            nbamor = -na
            call getvr8('AMOR_MODAL', 'AMOR_REDUIT', iocc=1, nbval=nbamor, vect=zr(jamor),&
                        nbret=na)
        else
            call getvid('AMOR_MODAL', 'LIST_AMOR', iocc=1, scal=listam, nbret=n)
            call jelira(listam//'           .VALE', 'LONMAX', ival=nbamor)
            call jeveuo(listam//'           .VALE', 'L', vr=mor)
            do iam = 1, nbmoda
                zr(jamor+iam-1) = mor(iam)
            end do
        endif
!
        if (nbamor .gt. nbmoda) then
            call utmess('A', 'MECANONLINE5_19')
        endif
        if (nbamor .lt. nbmoda) then
            call wkvect('&&NMMOAM.AMORTISSEMEN2', 'V V R', nbmoda, jamo2)
            do iam = 1, nbamor
                zr(jamo2+iam-1) = zr(jamor+iam-1)
            end do
            do iam = nbamor+1, nbmoda
                zr(jamo2+iam-1) = zr(jamor+nbamor-1)
            end do
            nbamor = nbmoda
            jamor = jamo2
        endif
    endif
!
! --- CREATION VALEURS MODALES
! ---  1/ MASSES GENERALISEES
! ---  2/ PULSATIONS PROPRES
! ---  3/ AMORTISSEMENT MODAL
!
    call wkvect(sdammo(1:19)//'.VALM', 'V V R', 3*nbmoda, jvalmo)
    do imode = 1, nbmoda
        call rsadpa(modmec, 'L', 1, 'MASS_GENE', imode,&
                    0, sjv=jmasg, styp=k8bid)
        zr(jvalmo+3*(imode-1)+1-1) = zr(jmasg)
        call rsadpa(modmec, 'L', 1, 'FREQ', imode,&
                    0, sjv=jfreq, styp=k8bid)
        zr(jvalmo+3*(imode-1)+2-1) = zr(jfreq)*2.d0*pi
        zr(jvalmo+3*(imode-1)+3-1) = zr(jamor+imode-1)
    end do
!
! --- CREATION BASE MODALE
!
    call wkvect(sdammo(1:19)//'.BASM', 'V V R', nbmoda*neq, jbasmo)
    AS_ALLOCATE(vr=vect1, size=neq)
    do imode = 1, nbmoda
        call rsexch('F', modmec, 'DEPL', imode, nomcha,&
                    iret)
        call jeveuo(nomcha(1:19)//'.VALE', 'L', vr=val)
        call dcopy(neq, val, 1, vect1, 1)
        call zerlag(neq, zi(iddeeq), vectr=vect1)
        call mrmult('ZERO', lmat, vect1, zr(jbasmo+(imode-1)*neq), 1,&
                    .true.)
        call zerlag(neq, zi(iddeeq), vectr=zr(jbasmo+(imode-1)*neq))
    end do
!
! --- MENAGE
!
    call jedetr('&&NMMOAM.AMORTISSEMENT')
    call jedetr('&&NMMOAM.AMORTISSEMEN2')
    AS_DEALLOCATE(vr=vect1)
!
    call jedema()
end subroutine
