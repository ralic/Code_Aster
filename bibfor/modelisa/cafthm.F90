subroutine cafthm(char, noma, ligrmo, fonree)
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/alcart.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/exixfe.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nocart.h"
#include "asterfort/reliem.h"
#include "asterfort/utmess.h"
    character(len=4) :: fonree
    character(len=8) :: char, noma
    character(len=19) :: ligrmo
! ======================================================================
! ======================================================================
! person_in_charge: sylvie.granet at edf.fr
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! BUT : STOCKAGE DES FLUX THERMO-HYDRAULIQUES DANS UNE CARTE ALLOUEE
!       SUR LE LIGREL DU MODELE (MODELISATIONS THM)
!
! ARGUMENTS D'ENTREE:
!      CHAR   : NOM UTILISATEUR DU RESULTAT DE CHARGE
!      LIGRMO : NOM DU LIGREL DE MODELE
!      NOMA   : NOM DU MAILLAGE
!      FONREE : FONC OU REEL
! ======================================================================
! ======================================================================
    integer :: n1, n2, n3, nflux, jvalv,  iocc
    integer :: nbtou, nbma, jma, ncmp
    integer :: iret, nfiss, jnfis
    character(len=8) :: k8b, mod, typmcl(2)
    character(len=16) :: motclf, motcle(2), modeli
    character(len=19) :: carte
    character(len=24) :: mesmai, lismai
    character(len=8), pointer :: vncmp(:) => null()
! ======================================================================
    call jemarq()
!
    motclf = 'FLUX_THM_REP'
    call getfac(motclf, nflux)
    if (nflux .eq. 0) goto 99
!
    mod = ligrmo(1:8)
    call dismoi('MODELISATION', mod, 'MODELE', repk=modeli)
!
    carte = char//'.CHME.FLUX '
!
    call exixfe(ligrmo(1:8), iret)
!
    if (fonree .eq. 'REEL') then
       call alcart('G', carte, noma, 'FTHM_R')
    else if (fonree.eq.'FONC') then
       call alcart('G', carte, noma, 'FTHM_F')
    else
       ASSERT(.false.)
    endif
!
    call jeveuo(carte//'.NCMP', 'E', vk8=vncmp)
    call jeveuo(carte//'.VALV', 'E', jvalv)
!
! --- STOCKAGE DE FORCES NULLES SUR TOUT LE MAILLAGE
!
    ncmp = 3
    vncmp(1) = 'PFLU1'
    vncmp(2) = 'PFLU2'
    vncmp(3) = 'PTHER'
!
    if (fonree .eq. 'REEL') then
        zr(jvalv) = 0.d0
        zr(jvalv+1) = 0.d0
        zr(jvalv+2) = 0.d0
    else
        zk8(jvalv) = '&FOZERO'
        zk8(jvalv+1) = '&FOZERO'
        zk8(jvalv+2) = '&FOZERO'
    endif
    call nocart(carte, 1, ncmp)
!
    mesmai = '&&CAFTHM.MAILLES_INTE'
    lismai = '&&CAFTHM.NUM_MAILLES'
    motcle(1) = 'GROUP_MA'
    motcle(2) = 'MAILLE'
    typmcl(1) = 'GROUP_MA'
    typmcl(2) = 'MAILLE'
!
! --- STOCKAGE DANS LA CARTE
!
    do iocc = 1, nflux
        if (fonree .eq. 'REEL') then
            call getvr8(motclf, 'FLUN_HYDR1', iocc=iocc, scal=zr(jvalv), nbret=n1)
            call getvr8(motclf, 'FLUN_HYDR2', iocc=iocc, scal=zr(jvalv+1), nbret=n2)
            call getvr8(motclf, 'FLUN', iocc=iocc, scal=zr(jvalv+2), nbret=n3)
        else
            call getvid(motclf, 'FLUN_HYDR1', iocc=iocc, scal=zk8(jvalv), nbret=n1)
            call getvid(motclf, 'FLUN_HYDR2', iocc=iocc, scal=zk8(jvalv+1), nbret=n2)
            call getvid(motclf, 'FLUN', iocc=iocc, scal=zk8(jvalv+2), nbret=n3)
        endif
!
! --- TEST SUR LES CAL
!
!
        call getvtx(motclf, 'TOUT', iocc=iocc, scal=k8b, nbret=nbtou)
!
        nfiss = 0
        if (iret.ne.0) then
            call jeveuo(mod//'.NFIS', 'L', jnfis)
            nfiss = zi(jnfis)
        endif
!
        if (nbtou .ne. 0) then
!
            call nocart(carte, 1, ncmp)
        else 
            if (nfiss.ne.0) then
!           LES FLUX POUR LA DEUXIEME PRESSION PRE2 ET POUR LA THERMIQUE
!           NE SONT PAS AUTORISES EN HM-XFEM
               if ((n2.ne.0).and.(n3.ne.0)) call utmess('F', 'XFEM_48')
            endif
            call reliem(ligrmo, noma, 'NU_MAILLE', motclf, iocc,&
                        2, motcle, typmcl, mesmai, nbma)
            if (nbma .ne. 0) then
                call jeveuo(mesmai, 'L', jma)
                call nocart(carte, 3, ncmp, mode='NUM', nma=nbma,&
                            limanu=zi(jma))
                call jedetr(mesmai)
            endif
        endif
!
    end do
 99 continue
!
!
    call jedema()
end subroutine
