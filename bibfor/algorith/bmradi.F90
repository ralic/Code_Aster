subroutine bmradi(basmod, intf, nomint, numint, nbddl,&
                  ivddl, nbdif)
! aslint: disable=W1306
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
    implicit none
!
!***********************************************************************
!    P. RICHARD     DATE 25/04/91/
!-----------------------------------------------------------------------
!  BUT:     < BASE MODALE RANG DDL ASSOCIES AUX DEFORMEES INTERFACE >
!
!    RENDRE UN VECTEUR DONNANT LES RANGS DES DDL ASSOCIES AUX DEFORMEES
!   CORRESPONDANT A UNE INTERFACE ( DEFINIE DANS UN CONCEPT DE BASE
!   MODALE) DANS UN CONCEPT BASE MODALE) ASSOCIEES AU DDL D'UNE
!    INTERFACE
!  L'INTERFACE EST DONNEE SOIT PAR SON NOM SOIT PAR SON NUMERO
!
! ARRET AVEC MESSAGE SI DIMENSION EN ENTREE DU VECTEUR EST INSUFFISANTE
! RENVOI LE NOMBRE DE DDL TROUVES SI CELUI CI EST PLUS PETIT QUE CELUI
!  DONNE EN ENTREE
!-----------------------------------------------------------------------
!
! BASMOD   /I/: NOM UTILISATEUR DE LA BASE MODALE OU BLANC
! INTF   /I/: NOM UTILISATEUR DE LA LISTE INTERFACE  OU BLANC
! NOMINT   /I/: NOM DE L'INTERFACE
! NUMINT   /I/: NUMERO DE L'INTERFACE
! NBDDL    /I/: NOMBRE DE RANGS ATTENDUS
! IVDDL    /I/: VECTEUR DES RANGS DES DDL
! NBDIF    /O/: NOMBRE ATTENDU - NOMBRE TROUVE
!
!
!
#include "jeveux.h"
!
#include "asterfort/cheddl.h"
#include "asterfort/dismoi.h"
#include "asterfort/isdeco.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/u2mesg.h"
!
!
!-----------------------------------------------------------------------
    integer :: i, inoe, iran, iret, j, lldeeq, lldes
    integer :: llnoe, nbcmp, nbcpmx, nbddl, nbdif, nbec, ibid
    integer :: nbnoe, nbnot, neq, numint, nunoe
!-----------------------------------------------------------------------
    parameter (nbcpmx=300)
    character(len=8) :: basmod, nomint, intf
    character(len=8) :: k8bid
    character(len=19) :: numddl
    character(len=24) :: noeint
    character(len=24) :: valk(2)
    integer :: ivddl(nbddl), idec(nbcpmx)
    integer :: vali
!
!-----------------------------------------------------------------------
!
!
    call jemarq()
    nbdif=nbddl
!
!---------------RECUPERATION INTERF_DYNA ET NUME_DDL-----------------
!                 SI DONNEE BASE MODALE OU INTERF_DYNA
!
    if (basmod(1:1) .ne. ' ') then
!
        call dismoi('F', 'REF_INTD_PREM', basmod, 'RESU_DYNA', ibid, intf, iret)
        if (intf .eq. ' ') then
            valk (1) = basmod
            call u2mesg('F', 'ALGORITH12_30', 1, valk, 0,&
                        0, 0, 0.d0)
        endif
        call dismoi('F', 'NUME_DDL', basmod, 'RESU_DYNA', ibid, numddl, iret)
    else
        if (intf(1:1) .ne. ' ') then
            call dismoi('F', 'REF_MASS_PREM', basmod, 'RESU_DYNA', ibid, numddl, iret)
        else
            valk (1) = basmod
            valk (2) = intf
            call u2mesg('F', 'ALGORITH12_31', 2, valk, 0,&
                        0, 0, 0.d0)
        endif
    endif
!
!
!
!------------RECUPERATION DONNEE GRANDEUR SOUS-JACENTE------------------
!
    call dismoi('F', 'NB_CMP_MAX', intf, 'INTERF_DYNA', nbcmp,&
                k8bid, iret)
    call dismoi('F', 'NB_EC', intf, 'INTERF_DYNA', nbec,&
                k8bid, iret)
!
!----------------RECUPERATION EVENTUELLE DU NUMERO INTERFACE------------
!
    if (numint .lt. 1) then
        if (nomint .eq. '          ') then
            valk (1) = nomint
            vali = numint
            call u2mesg('F', 'ALGORITH12_29', 1, valk, 1,&
                        vali, 0, 0.d0)
        else
            call jenonu(jexnom(intf//'.IDC_NOMS', nomint), numint)
        endif
    endif
!
!
!-----------RECUPERATION DU NOMBRE DE DDL PHYSIQUES ASSEMBLES-----------
!
!
    call dismoi('F', 'NB_EQUA', numddl, 'NUME_DDL', neq,&
                k8bid, iret)
!
!
!------------RECUPERATION DU NOMBRE DE NOEUD DE L'INTERFACES------------
!
    noeint=intf//'.IDC_LINO'
!
    call jelira(jexnum(noeint, numint), 'LONMAX', nbnoe)
    call jeveuo(jexnum(noeint, numint), 'L', llnoe)
!
!
!-------------RECUPERATION DU DESCRIPTEUR DES DEFORMEES-----------------
!
    call jeveuo(intf//'.IDC_DEFO', 'L', lldes)
    call jelira(intf//'.IDC_DEFO', 'LONMAX', nbnot)
    nbnot = nbnot/(2+nbec)
!
!----------------RECUPERATION ADRESSE DEEQ------------------------------
!
!----ON AJOUT .NUME POUR OBTENIR LE PROF_CHNO
    numddl(15:19)='.NUME'
    call jeveuo(numddl//'.DEEQ', 'L', lldeeq)
!
!------RECUPERATION DES NUMERO ORDRE DEFORMEES (COLONNES)---------------
!-----ET RANGS DES DDL D'INTERFACE (LIGNES) DANS VECTEUR ASSEMBLE-------
!
! RECUPERATION RANG DDL
!
    do 20 i = 1, nbnoe
        inoe=zi(llnoe+i-1)
        nunoe=zi(lldes+inoe-1)
        call isdeco(zi(lldes+2*nbnot+(inoe-1)*nbec+1-1), idec, nbcmp)
        do 30 j = 1, nbcmp
            if (idec(j) .gt. 0) then
                nbdif=nbdif-1
                if (nbdif .ge. 0) then
                    call cheddl(zi(lldeeq), neq, nunoe, j, iran,&
                                1)
                    ivddl(nbddl-nbdif)=iran
                endif
            endif
30      continue
20  continue
!
    nbdif=-nbdif
!
    call jedema()
end subroutine
