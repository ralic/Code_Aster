subroutine nmmuap(sddyna)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/ndynin.h"
#include "asterfort/ndynkk.h"
#include "asterfort/trmult.h"
#include "asterfort/wkvect.h"
#include "asterfort/zerlag.h"
    character(len=19) :: sddyna
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (DYNAMIQUE - INITIALISATIONS)
!
! LECTURE DONNEES MULTI-APPUIS
!
! ----------------------------------------------------------------------
!
!
! IN  SDDYNA : SD DYNAMIQUE
!
!
!
!
    character(len=8) :: k8bid, rep, modsta, mailla
    character(len=14) :: numddl
    character(len=24) :: deeq, matric
    integer :: nbmd, neq, na, nd, nbexci, nf, nv
    integer :: i
    integer :: iddeeq
    character(len=19) :: mafdep, mafvit, mafacc, mamula, mapsid
    integer :: jnodep, jnovit, jnoacc, jmltap, jpsdel
!
! ---------------------------------------------------------------------
!
    call jemarq()
!
! --- LECTURE INFO. MATRICE MODES STATIQUES
!
    call getvid(' ', 'MODE_STAT', scal=modsta, nbret=nbmd)
    call dismoi('REF_RIGI_PREM', modsta, 'RESU_DYNA', repk=matric)
    call dismoi('NOM_MAILLA', matric, 'MATR_ASSE', repk=mailla)
    call dismoi('NOM_NUME_DDL', matric, 'MATR_ASSE', repk=numddl)
    deeq = numddl//'.NUME.DEEQ'
    call jeveuo(deeq, 'L', iddeeq)
    call dismoi('NB_EQUA', matric, 'MATR_ASSE', repi=neq)
!
! --- LECTURE EFFORTS MULTI-APPUIS
!
    nbexci = ndynin(sddyna,'NBRE_EXCIT')
    call ndynkk(sddyna, 'MUAP_MAFDEP', mafdep)
    call ndynkk(sddyna, 'MUAP_MAFVIT', mafvit)
    call ndynkk(sddyna, 'MUAP_MAFACC', mafacc)
    call ndynkk(sddyna, 'MUAP_MAMULA', mamula)
    call ndynkk(sddyna, 'MUAP_MAPSID', mapsid)
!
    call wkvect(mafdep, 'V V K8', nbexci, jnodep)
    call wkvect(mafvit, 'V V K8', nbexci, jnovit)
    call wkvect(mafacc, 'V V K8', nbexci, jnoacc)
    call wkvect(mamula, 'V V I', nbexci, jmltap)
    call wkvect(mapsid, 'V V R8', nbexci*neq, jpsdel)
!
    do i = 1, nbexci
        call getvtx('EXCIT', 'MULT_APPUI', iocc=i, scal=rep, nbret=nd)
        if (rep(1:3) .eq. 'OUI') then
            zi(jmltap+i-1) = 1
!
! --- ACCELERATIONS AUX APPUIS
!
            call getvid('EXCIT', 'ACCE', iocc=i, scal=k8bid, nbret=na)
            if (na .ne. 0) then
                call getvid('EXCIT', 'ACCE', iocc=i, scal=zk8(jnoacc+i-1), nbret=na)
            endif
!
! --- FONCTIONS MULTIPLICATRICES DES ACCE. AUX APPUIS
!
            call getvid('EXCIT', 'FONC_MULT', iocc=i, scal=k8bid, nbret=nf)
            if (nf .ne. 0) then
                call getvid('EXCIT', 'FONC_MULT', iocc=i, scal=zk8(jnoacc+i- 1), nbret=nf)
            endif
!
! --- VITESSES AUX APPUIS
!
            call getvid('EXCIT', 'VITE', iocc=i, scal=zk8(jnovit+i-1), nbret=nv)
!
! --- DEPLACEMENTS AUX APPUIS
!
            call getvid('EXCIT', 'DEPL', iocc=i, scal=zk8(jnodep+i-1), nbret=nd)
!
! --- CREE ET CALCULE LE VECTEUR PSI*DIRECTION
!
            call trmult(modsta, i, mailla, neq, iddeeq,&
                        zr(jpsdel+(i-1)* neq))
!
! --- MISE A ZERO DES DDL DE LAGRANGE
!
            call zerlag(neq, zi(iddeeq), vectr=zr(jpsdel+(i-1)*neq))
        endif
    end do
!
    call jedema()
end subroutine
