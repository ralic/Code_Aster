subroutine bmnodi(basmdz, intfz, nmintz, numint, nbdef,&
                  ivcord, nbdif)
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
    implicit none
!
!***********************************************************************
!    P. RICHARD     DATE 09/0491/
!-----------------------------------------------------------------------
!  BUT:       < BASE MODALE NUMERO ORDRE DES DEFORMEES INTERFACE>
!
!  SI BASE MODALE DONNEE:
!  ----------------------
!    RENDRE UN VECTEUR DONNANT LES NUMEROS ORDRE DES DEFORMEES (DANS
!  UN CONCEPT BASE MODALE) ASSOCIEES AU DDL D'UNE INTERFACE
!
!  SI PAS DE BASE MODALE DONNEE:
!  ----------------------
!    RENDRE UN VECTEUR DONNANT LES NUMEROS ORDRE DES DEFORMEES A CALCULE
!  RELATIF A UNE INTERFACE D'UNE INTERF_DYNA
!
!  L'INTERFACE EST DONNEE SOIT PAR SON NOM SOIT PAR SON NUMERO
!
!-----------------------------------------------------------------------
!
! BASMDZ   /I/: NOM UTILISATEUR DE LA BASE MODALE OU BLANC
! INTFZ    /I/: NOM UTILISATEUR DE L'INTERF_DYNA OU BLANC
! NMINTZ   /I/: NOM DE L'INTERFACE
! NUMINT   /I/: NUMERO DE L'INTERFACE
! NBDEF    /I/: NOMBRE DE NUMERO ORDRE ATTENDUS
! IVCORD   /O/: VECTEUR DES NUMEROS D'ORDRE A REMPLIR
! NBDIF    /0/: NOMBRE ATTENDU - NOMBRE TROUVE
!
!
!
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/isdeco.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
!
!
!
    character(len=*), intent(in) :: basmdz, nmintz, intfz
    integer, intent(in) :: numint, nbdef
    integer, intent(out) :: ivcord(nbdef)
    integer, intent(inout) :: nbdif
!
    character(len=8) :: basmod, nomint, intf, blanc, intfb
    character(len=24) :: noeint, idesc
    character(len=24) :: valk(3)
    integer :: vali
    integer :: idec(300)
    character(len=10) :: typbas(3)
    integer :: i, inoe, iordef, j, lldes, llnoe
    integer :: nbnot, nbcmp, nbec, nbmod, nbnoe
!-----------------------------------------------------------------------
    data typbas/'CLASSIQUE','CYCLIQUE','RITZ'/
!-----------------------------------------------------------------------
!
!
    call jemarq()
    basmod = basmdz
    nomint = nmintz
    intf = intfz
    blanc='        '
    if (basmod .eq. blanc .and. intf .eq. blanc) then
        valk (1) = basmod
        valk (2) = intf
        call utmess('F', 'ALGORITH12_26', nk=2, valk=valk)
    endif
!
    nbdif=nbdef
    nbmod=0
!
!
!-------------RECUPERATION DU TYPE DE BASE ET INTERF_DYNA------------
!
    if (basmod .ne. blanc) then
        call dismoi('TYPE_BASE', basmod, 'RESU_DYNA', repk=idesc, arret='C')
        call dismoi('NB_MODES_DYN', basmod, 'RESULTAT', repi=nbmod)
        if (idesc(1:9) .ne. 'CLASSIQUE') then
            valk (1) = basmod
            valk (2) = idesc
            valk (3) = typbas(1)
            call utmess('F', 'ALGORITH12_27', nk=3, valk=valk)
        endif
!
        call dismoi('REF_INTD_PREM', basmod, 'RESU_DYNA', repk=intfb, arret='C')
        if (intf .ne. blanc .and. intf .ne. intfb) then
            valk (1) = basmod
            valk (2) = intfb
            valk (3) = intf
            call utmess('F', 'ALGORITH12_28', nk=3, valk=valk)
        else
            intf=intfb
        endif
    endif
!
!
!----------------RECUPERATION DONNEES GRANDEUR SOUS-JACENTE-------------
!
    call dismoi('NB_CMP_MAX', intf, 'INTERF_DYNA', repi=nbcmp)
    call dismoi('NB_EC', intf, 'INTERF_DYNA', repi=nbec)
!
!
!
!----------------RECUPERATION EVENTUELLE DU NUMERO INTERFACE------------
!
    if (numint .lt. 1) then
        if (nomint .eq. '          ') then
            valk (1) = nomint
            vali = numint
            call utmess('F', 'ALGORITH12_29', sk=valk(1), si=vali)
        else
            call jenonu(jexnom(intf//'.IDC_NOMS', nomint), numint)
        endif
    endif
!
!----------RECUPERATION DU NOMBRE DE NOEUD DE L' INTERFACES-------------
!
    noeint=intf//'.IDC_LINO'
!
    call jelira(jexnum(noeint, numint), 'LONMAX', nbnoe)
    call jeveuo(jexnum(noeint, numint), 'L', llnoe)
!
!-------------RECUPERATION DU DESCRIPTEUR DES DEFORMEES-----------------
!
!
    call jeveuo(intf//'.IDC_DEFO', 'L', lldes)
    call jelira(intf//'.IDC_DEFO', 'LONMAX', nbnot)
    nbnot = nbnot/(2+nbec)
!
!-----------RECUPERATION DES NUMERO ORDRE DEFORMEES --------------------
!
!
! RECUPERATION NUMERO ORDRE  DEFORMEES
!
    do i = 1, nbnoe
        inoe=zi(llnoe+i-1)
        iordef=zi(lldes+nbnot+inoe-1)+nbmod
        call isdeco(zi(lldes+2*nbnot+(inoe-1)*nbec+1-1), idec, nbcmp)
!
        do j = 1, nbcmp
            if (idec(j) .gt. 0) then
                nbdif=nbdif-1
                if (nbdif .ge. 0) ivcord(nbdef-nbdif)=iordef
                iordef=iordef+1
            endif
        end do
!
    end do
!
    nbdif=-nbdif
!
    call jedema()
end subroutine
