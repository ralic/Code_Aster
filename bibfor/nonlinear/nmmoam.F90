subroutine nmmoam(sdammz, nbmoda)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    include 'jeveux.h'
    include 'asterc/getexm.h'
    include 'asterc/getvid.h'
    include 'asterc/getvis.h'
    include 'asterc/getvr8.h'
    include 'asterc/r8pi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/mginfo.h'
    include 'asterfort/mrmult.h'
    include 'asterfort/mtdscr.h'
    include 'asterfort/rsadpa.h'
    include 'asterfort/rsexch.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    include 'asterfort/zerlag.h'
    include 'blas/dcopy.h'
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
    real(kind=8) :: pi, r8bid
    complex(kind=8) :: cbid
    integer :: iret, iam, imode, vali(3), iadrif
    integer :: na, nb, n, nm
    integer :: nbmd, neq, nbmax, nbamor
    integer :: iddeeq, lmat, iamor, ltvec
    integer :: jvalmo, jbasmo, jamor, jval, jamo2, jmasg, jfreq
    integer :: exiam
    integer :: iarg
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
    call getvid('AMOR_MODAL', 'MODE_MECA', 1, iarg, 1,&
                modmec, nbmd)
    if (nbmd .eq. 0) then
        call u2mess('F', 'ALGORITH17_20')
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
    call jeveuo(modmec//'           .REFD', 'L', iadrif)
    matric = zk24(iadrif)(1:8)
    call mtdscr(matric(1:8))
    call jeveuo(matric(1:19)//'.&INT', 'E', lmat)
!
! --- NOMBRE DE MODES
!
    call getvis('AMOR_MODAL', 'NB_MODE', 1, iarg, 1,&
                nbmax, nm)
    if (nbmax .ne. nbmoda) then
        vali(1) = nbmoda
        vali(2) = nbmax
        vali(3) = min(nbmoda,nbmax)
        call u2mesg('I', 'MECANONLINE5_30', 0, k8bid, 3,&
                    vali, 0, r8bid)
        nbmoda = min(nbmoda,nbmax)
    endif
!
! --- RECUPERATION DES AMORTISSEMENTS
!
    call wkvect('&&NMMOAM.AMORTISSEMENT', 'V V R', nbmoda, jamor)
!
    na = 0
    nb = 0
    call getvr8('AMOR_MODAL', 'AMOR_REDUIT', 1, iarg, 0,&
                r8bid, na)
    exiam = getexm('AMOR_MODAL','LIST_AMOR')
    if (exiam .eq. 1) then
        call getvid('AMOR_MODAL', 'LIST_AMOR', 1, iarg, 0,&
                    k8bid, nb)
    endif
!
!     VERIFICATION QU'UNE LISTE D'AMORTISSEMENTS EST FOURNIE
    if (na .eq. 0 .and. nb .eq. 0) then
        call u2mess('F', 'ALGORITH17_21')
    endif
!
    if (na .ne. 0 .or. nb .ne. 0) then
        if (na .ne. 0) then
            nbamor = -na
            call getvr8('AMOR_MODAL', 'AMOR_REDUIT', 1, iarg, nbamor,&
                        zr(jamor), na)
        else
            call getvid('AMOR_MODAL', 'LIST_AMOR', 1, iarg, 1,&
                        listam, n)
            call jelira(listam//'           .VALE', 'LONMAX', nbamor, k8bid)
            call jeveuo(listam//'           .VALE', 'L', iamor)
            do 30 iam = 1, nbmoda
                zr(jamor+iam-1) = zr(iamor+iam-1)
30          continue
        endif
!
        if (nbamor .gt. nbmoda) then
            call u2mess('A', 'MECANONLINE5_19')
        endif
        if (nbamor .lt. nbmoda) then
            call wkvect('&&NMMOAM.AMORTISSEMEN2', 'V V R', nbmoda, jamo2)
            do 40 iam = 1, nbamor
                zr(jamo2+iam-1) = zr(jamor+iam-1)
40          continue
            do 42 iam = nbamor+1, nbmoda
                zr(jamo2+iam-1) = zr(jamor+nbamor-1)
42          continue
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
    do 10 imode = 1, nbmoda
        call rsadpa(modmec, 'L', 1, 'MASS_GENE', imode,&
                    0, jmasg, k8bid)
        zr(jvalmo+3*(imode-1)+1-1) = zr(jmasg)
        call rsadpa(modmec, 'L', 1, 'FREQ', imode,&
                    0, jfreq, k8bid)
        zr(jvalmo+3*(imode-1)+2-1) = zr(jfreq)*2.d0*pi
        zr(jvalmo+3*(imode-1)+3-1) = zr(jamor+imode-1)
10  end do
!
! --- CREATION BASE MODALE
!
    call wkvect(sdammo(1:19)//'.BASM', 'V V R', nbmoda*neq, jbasmo)
    call wkvect('&&NMMOAM.VECT1', 'V V R', neq, ltvec)
    do 11 imode = 1, nbmoda
        call rsexch('F', modmec, 'DEPL', imode, nomcha,&
                    iret)
        call jeveuo(nomcha(1:19)//'.VALE', 'L', jval)
        call dcopy(neq, zr(jval), 1, zr(ltvec), 1)
        call zerlag('R', zr(ltvec), cbid, neq, zi(iddeeq))
        call mrmult('ZERO', lmat, zr(ltvec), zr(jbasmo+(imode-1)*neq), 1,&
                    .true.)
        call zerlag('R', zr(jbasmo+(imode-1)*neq), cbid, neq, zi(iddeeq))
11  end do
!
! --- MENAGE
!
    call jedetr('&&NMMOAM.AMORTISSEMENT')
    call jedetr('&&NMMOAM.AMORTISSEMEN2')
    call jedetr('&&NMMOAM.VECT1')
!
    call jedema()
end subroutine
