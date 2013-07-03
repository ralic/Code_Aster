subroutine ndloam(sddyna, result, evonol, nume)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterc/getvid.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/ndynin.h"
#include "asterfort/ndynkk.h"
#include "asterfort/r8inir.h"
#include "asterfort/rsadpa.h"
#include "asterfort/u2mess.h"
#include "blas/dcopy.h"
    character(len=19) :: sddyna
    character(len=8) :: result
    integer :: nume
    logical :: evonol
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (DYNAMIQUE)
!
! LECTURE DES DEPL/VITE/ACCEL GENERALISES DANS SD_RESULT
!
! ----------------------------------------------------------------------
!
!
! IN  RESULT : NOM UTILISATEUR DU CONCEPT RESULTAT
! IN  SDDYNA : SD DYNAMIQUE
!
!
!
!
    character(len=8) :: ctype
    integer :: iret
    integer :: nbmodp
    character(len=24) :: trgene
    integer :: jtrgen
    logical :: linit
    character(len=19) :: depgem, vitgem, accgem
    integer :: jdepgm, jvitgm, jaccgm
    character(len=19) :: depgep, vitgep, accgep
    integer :: jdepgp, jvitgp, jaccgp
    character(len=19) :: dgen, vgen, agen
    character(len=19) :: depgen, vitgen, accgen
    integer :: jrestd, jrestv, jresta
    integer :: ifm, niv
    integer :: nocc1, nocc2, nocc3, nocc
    integer :: iarg
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... LECTURE PROJ. MODALE'
    endif
!
! --- INITIALISATIONS
!
    linit = .false.
    ctype = 'K24'
!
! --- OBJETS PROJECTION MODALE
!
    nbmodp = ndynin(sddyna,'NBRE_MODE_PROJ')
    call ndynkk(sddyna, 'PRMO_DEPGEM', depgem)
    call ndynkk(sddyna, 'PRMO_VITGEM', vitgem)
    call ndynkk(sddyna, 'PRMO_ACCGEM', accgem)
    call ndynkk(sddyna, 'PRMO_DEPGEP', depgep)
    call ndynkk(sddyna, 'PRMO_VITGEP', vitgep)
    call ndynkk(sddyna, 'PRMO_ACCGEP', accgep)
    call jeveuo(accgem, 'E', jaccgm)
    call jeveuo(accgep, 'E', jaccgp)
    call jeveuo(vitgem, 'E', jvitgm)
    call jeveuo(vitgep, 'E', jvitgp)
    call jeveuo(depgem, 'E', jdepgm)
    call jeveuo(depgep, 'E', jdepgp)
!
! --- SI PAS RE-ENTRANT: ON PART DE ZERO
!
    if (.not.evonol) then
        linit = .true.
        call getvid('PROJ_MODAL', 'DEPL_INIT_GENE', 1, iarg, 1,&
                    depgen, nocc1)
        call getvid('PROJ_MODAL', 'VITE_INIT_GENE', 1, iarg, 1,&
                    vitgen, nocc2)
        call getvid('PROJ_MODAL', 'ACCE_INIT_GENE', 1, iarg, 1,&
                    accgen, nocc3)
        nocc = nocc1+nocc2+nocc3
        if (nocc .ne. 0) linit = .false.
    else
!
! --- EXISTENCE DU PARAMETRE DANS SD_RESULTAT
!
        call rsadpa(result, 'L', 1, 'TRAN_GENE_NOLI', nume,&
                    1, jtrgen, ctype)
        trgene = zk24(jtrgen)
        call jeexin(trgene(1:18)//'D', iret)
        if (iret .eq. 0) then
            call u2mess('A', 'MECANONLINE5_31')
            linit = .true.
        else
            dgen = trgene(1:18)//'D'
            vgen = trgene(1:18)//'V'
            agen = trgene(1:18)//'A'
        endif
    endif
!
! --- INITIALISATION OU LECTURE
!
    if (linit) then
        call r8inir(nbmodp, 0.d0, zr(jaccgm), 1)
        call r8inir(nbmodp, 0.d0, zr(jaccgp), 1)
        call r8inir(nbmodp, 0.d0, zr(jvitgm), 1)
        call r8inir(nbmodp, 0.d0, zr(jvitgp), 1)
        call r8inir(nbmodp, 0.d0, zr(jdepgm), 1)
        call r8inir(nbmodp, 0.d0, zr(jdepgp), 1)
    else
        if (evonol) then
            call jeveuo(dgen, 'L', jrestd)
            call jeveuo(vgen, 'L', jrestv)
            call jeveuo(agen, 'L', jresta)
            call dcopy(nbmodp, zr(jrestd), 1, zr(jdepgm), 1)
            call dcopy(nbmodp, zr(jrestd), 1, zr(jdepgp), 1)
            call dcopy(nbmodp, zr(jrestv), 1, zr(jvitgm), 1)
            call dcopy(nbmodp, zr(jrestv), 1, zr(jvitgp), 1)
            call dcopy(nbmodp, zr(jresta), 1, zr(jaccgm), 1)
            call dcopy(nbmodp, zr(jresta), 1, zr(jaccgp), 1)
        else
            call r8inir(nbmodp, 0.d0, zr(jaccgm), 1)
            call r8inir(nbmodp, 0.d0, zr(jaccgp), 1)
            call r8inir(nbmodp, 0.d0, zr(jvitgm), 1)
            call r8inir(nbmodp, 0.d0, zr(jvitgp), 1)
            call r8inir(nbmodp, 0.d0, zr(jdepgm), 1)
            call r8inir(nbmodp, 0.d0, zr(jdepgp), 1)
            if (nocc1 .ne. 0) then
                call jeveuo(depgen//'.VALE', 'L', jrestd)
                call dcopy(nbmodp, zr(jrestd), 1, zr(jdepgm), 1)
                call dcopy(nbmodp, zr(jrestd), 1, zr(jdepgp), 1)
            endif
            if (nocc2 .ne. 0) then
                call jeveuo(vitgen//'.VALE', 'L', jrestv)
                call dcopy(nbmodp, zr(jrestv), 1, zr(jvitgm), 1)
                call dcopy(nbmodp, zr(jrestv), 1, zr(jvitgp), 1)
            endif
            if (nocc3 .ne. 0) then
                call jeveuo(accgen//'.VALE', 'L', jresta)
                call dcopy(nbmodp, zr(jresta), 1, zr(jaccgm), 1)
                call dcopy(nbmodp, zr(jresta), 1, zr(jaccgp), 1)
            endif
        endif
    endif
!
    call jedema()
!
end subroutine
