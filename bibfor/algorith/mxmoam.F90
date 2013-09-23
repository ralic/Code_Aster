subroutine mxmoam(sddyna, nbmodp)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mginfo.h"
#include "asterfort/ndynin.h"
#include "asterfort/ndynkk.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/zerlag.h"
#include "blas/dcopy.h"
!
    character(len=19) :: sddyna
    integer :: nbmodp
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - EXPLICITE)
!
! PROJECTION MODALE EN EXPLICITE
!
! ----------------------------------------------------------------------
!
!
! IN  SDDYNA : SD DEDIEE A LA DYNAMIQUE (CF NDLECT)
! OUT NBMODP : NOMBRE DE MODES DE PROJECTION
!
!
!
!
    integer :: nbmd, nbmg, neq, nbmax, nbrg, nbag
    integer :: nbgene
    integer :: iddeeq, jval
    integer :: jlifge, jfge
    integer :: ldblo, ldblo1, ldblo2
    integer :: imode, ifonc, imode2
    integer :: iret, ibid, nf, lpar, vali(3)
    character(len=8) :: k8bid
    character(len=8) :: modmec, magene, amgene, rigene
    character(len=14) :: numddl
    character(len=19) :: fmodal, valfon
    integer :: jfmoda, jvalfo
    character(len=19) :: depgem, vitgem, accgem
    integer :: jdepgm, jvitgm, jaccgm
    character(len=19) :: depgep, vitgep, accgep
    integer :: jdepgp, jvitgp, jaccgp
    character(len=19) :: basmod
    integer :: jbasmo
    character(len=19) :: riggen, masgen, amogen, fongen, forgen
    integer :: jrigge, jmasge, jamoge, jfonge, jforge
    character(len=19) :: accgcn
    integer :: jacccn
    character(len=24) :: deeq
    character(len=24) :: nomcha
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- NOM DES OBJETS POUR PROJECTION MODALE
!
    call ndynkk(sddyna, 'PRMO_DEPGEM', depgem)
    call ndynkk(sddyna, 'PRMO_VITGEM', vitgem)
    call ndynkk(sddyna, 'PRMO_ACCGEM', accgem)
    call ndynkk(sddyna, 'PRMO_DEPGEP', depgep)
    call ndynkk(sddyna, 'PRMO_VITGEP', vitgep)
    call ndynkk(sddyna, 'PRMO_ACCGEP', accgep)
    call ndynkk(sddyna, 'PRMO_BASMOD', basmod)
    call ndynkk(sddyna, 'PRMO_MASGEN', masgen)
    call ndynkk(sddyna, 'PRMO_AMOGEN', amogen)
    call ndynkk(sddyna, 'PRMO_RIGGEN', riggen)
    call ndynkk(sddyna, 'PRMO_FONGEN', fongen)
    call ndynkk(sddyna, 'PRMO_FORGEN', forgen)
    call ndynkk(sddyna, 'PRMO_ACCGCN', accgcn)
    call ndynkk(sddyna, 'PRMO_VALFON', valfon)
    call ndynkk(sddyna, 'PRMO_FMODAL', fmodal)
!
! --- MATRICE DES MODES MECA
!
    call getvid('PROJ_MODAL', 'MODE_MECA', iocc=1, scal=modmec, nbret=nbmd)
    if (nbmd .eq. 0) then
        ASSERT(.false.)
    endif
!
! --- MASSE, RIGIDITE ET AMORTISSEMENT GENERALISES
!
    call getvid('PROJ_MODAL', 'MASS_GENE', iocc=1, scal=magene, nbret=nbmg)
    call getvid('PROJ_MODAL', 'RIGI_GENE', iocc=1, scal=rigene, nbret=nbrg)
    call getvid('PROJ_MODAL', 'AMOR_GENE', iocc=1, scal=amgene, nbret=nbag)
!
! --- IL FAUT MASS_GENE _ET_ RIGI_GENE (VOIR CAPY)
!
    if ((nbmg.gt.0) .and. (nbrg.eq.0)) then
        ASSERT(.false.)
    endif
!
! --- INFORMATIONS SUR MATRICE DES MODES MECANIQUES
!
    call mginfo(modmec, numddl, nbmodp, neq)
    deeq = numddl//'.NUME.DEEQ'
    call jeveuo(deeq, 'L', iddeeq)
!
! --- NOMBRE DE MODES
!
    call getvis('PROJ_MODAL', 'NB_MODE', iocc=1, scal=nbmax, nbret=ibid)
    if (nbmax .ne. nbmodp) then
        vali(1) = nbmodp
        vali(2) = nbmax
        vali(3) = min(nbmodp,nbmax)
        call utmess('I', 'MECANONLINE5_29', ni=3, vali=vali)
        nbmodp = min(nbmodp,nbmax)
    endif
!
! --- CREATION VECTEUR DES FORCES MODALES
!
    call wkvect(fmodal, 'V V R', nbmodp, jfmoda)
!
! --- CREATION MASSES GENERALISEES
!
    call wkvect(masgen, 'V V R', nbmodp, jmasge)
!
! --- CREATION BASE MODALE
!
    call wkvect(basmod, 'V V R', nbmodp*neq, jbasmo)
!
! --- SI MASS_GENE NON DONNE
!
    if (nbmg .eq. 0) then
!
! ---   ON RECUPERE MODES DANS MODE_MECA
!
        do 61 imode = 1, nbmodp
            call rsexch('F', modmec, 'DEPL', imode, nomcha,&
                        iret)
            call jeveuo(nomcha(1:19)//'.VALE', 'L', jval)
            call dcopy(neq, zr(jval), 1, zr(jbasmo+(imode-1)*neq), 1)
            call zerlag(neq, zi( iddeeq), vectr=zr(jbasmo+(imode-1)*neq))
61      continue
!
! ---   ON RECUPERE MASSES GENERALISEES DANS MODE_MECA
!
        do 10 imode = 1, nbmodp
            call rsadpa(modmec, 'L', 1, 'MASS_GENE', imode,&
                        0, sjv=lpar, styp=k8bid)
            zr(jmasge+imode-1) = zr(lpar)
10      continue
!
! --- CREATION ACCELERATION DE REFERENCE
!
        call wkvect(accgcn, 'V V R', nbmodp, jacccn)
    else
!
! --- CREATION DEPL/VITE/ACCE GENERALISES T- ET T+
!
        call wkvect(accgem, 'V V R', nbmodp, jaccgm)
        call wkvect(accgep, 'V V R', nbmodp, jaccgp)
        call wkvect(vitgem, 'V V R', nbmodp, jvitgm)
        call wkvect(vitgep, 'V V R', nbmodp, jvitgp)
        call wkvect(depgem, 'V V R', nbmodp, jdepgm)
        call wkvect(depgep, 'V V R', nbmodp, jdepgp)
!
! --- ON RECUPERE MODES DANS MODE_MECA
!
        do 67 imode = 1, nbmodp
            call rsexch('F', modmec, 'DEPL', imode, nomcha,&
                        iret)
            call jeveuo(nomcha(1:19)//'.VALE', 'L', jval)
            call dcopy(neq, zr(jval), 1, zr(jbasmo+(imode-1)*neq), 1)
            call zerlag(neq, zi( iddeeq), vectr=zr(jbasmo+(imode-1)*neq))
67      continue
!
! --- CREATION RIGIDITES GENERALISEES
!
        call wkvect(riggen, 'V V R', nbmodp*nbmodp, jrigge)
!
! --- CREATION AMORTISSEMENTS GENERALISES
!
        call wkvect(amogen, 'V V R', nbmodp*nbmodp, jamoge)
!
! --- CREATION FORCES/FONC_MULT GENERALISEES
!
        nbgene = ndynin(sddyna,'NBRE_EXCIT_GENE')
        if (nbgene .ne. 0) then
            call wkvect(fongen, 'V V K24', nbgene, jfonge)
            call wkvect('&&MXMOAM.LIFOGE', 'V V K24', nbgene, jlifge)
            call wkvect(forgen, 'V V R', nbgene*nbmodp, jforge)
        endif
!
! --- CREATION VECTEUR DE RESOLUTION FORCES
!
        if (nbgene .ne. 0) then
            call wkvect(valfon, 'V V R', nbgene, jvalfo)
        endif
!
! --- RECUPERATION MASSES GENERALISEES
!
        call jeveuo(jexnum(magene//'           .VALM', 1), 'L', ldblo)
        do 20 imode = 1, nbmodp
            zr(jmasge+imode-1) = zr(ldblo+imode-1)
20      continue
!
! --- RECUPERATION RIGIDITES GENERALISEES
!
        call jeveuo(jexnum(rigene//'           .VALM', 1), 'L', ldblo1)
        do 30 imode = 1, nbmodp
            do 40 imode2 = 1, imode
                zr(jrigge+(imode-1)*nbmodp+imode2-1) = zr( ldblo1+ imode*(imode-1 )/2+imode2-1 )
                zr(jrigge+(imode2-1)*nbmodp+imode-1) = zr( ldblo1+ imode*(imode-1 )/2+imode2-1 )
40          continue
30      continue
!
! --- RECUPERATION AMORTISSEMENTS GENERALISES
!
        call jeveuo(jexnum(amgene//'           .VALM', 1), 'L', ldblo2)
        do 31 imode = 1, nbmodp
            do 41 imode2 = 1, imode
                if (nbag .ne. 0) then
                    zr(jamoge+(imode-1)*nbmodp+imode2-1) = zr(ldblo2+ imode*( imode-1)/2+imode2-1&
                                                           )
                    zr(jamoge+(imode2-1)*nbmodp+imode-1) = zr(ldblo2+ imode*( imode-1)/2+imode2-1&
                                                           )
                endif
41          continue
31      continue
!
! --- RECUPERATION FORCES/FONC_MULT GENERALISEES
!
        if (nbgene .ne. 0) then
            do 11 ifonc = 1, nbgene
                call getvid('EXCIT_GENE', 'FONC_MULT', iocc=ifonc, scal=zk24(jfonge+ifonc-1),&
                            nbret=nf)
                call getvid('EXCIT_GENE', 'VECT_GENE', iocc=ifonc, scal=zk24(jlifge+ifonc-1),&
                            nbret=nf)
                call jeveuo(zk24(jlifge+ifonc-1)(1:19)//'.VALE', 'L', jfge)
                do 12 imode = 1, nbmodp
                    zr(jforge+(ifonc-1)*nbmodp+imode-1) = zr(jfge+ imode-1)
12              continue
11          continue
        endif
    endif
!
! --- MENAGE
!
    call jedetr('&&MXMOAM.LIFOGE')
    call jedema()
end subroutine
