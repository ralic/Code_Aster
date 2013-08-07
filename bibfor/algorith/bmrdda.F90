subroutine bmrdda(basmod, intf, nomint, numint, nbddl,&
                  ivddl, nbdif, ord, nliais)
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
!    P. RICHARD     DATE 09/0491/
!-----------------------------------------------------------------------
!  BUT:   < BASE MODALE RANGS DDL ACTIF A INTERFACE>
!
! RENDRE LA LISTE DES RANGS DES  DDL ACTIFS POUR  (DANS
!  UN CONCEPT BASE MODALE) UNE INTERFACE
!  L'INTERFACE EST DONNEE SOIT PAR SON NOM SOIT PAR SON NUMERO
!
!-----------------------------------------------------------------------
!
! BASMOD   /I/: NOM UTILISATEUR DE LA BASE MODALE
! INTF     /I/: NOM UTILISATEUR DE LA LIST_INTEFACE
! NOMINT   /I/: NOM DE L'INTERFACE
! NUMINT   /I/: NUMERO DE L'INTERFACE
! NBDDL    /I/: NOMBRE DE DDL ACTIF ATTENDU
! IVDDL    /O/: LISTE DES RANGS DES DDL
! NBDDL    /M/: NOMBRE ATTENDU - NOMBRE TROUVE
!
!
!
#include "jeveux.h"
!
#include "asterfort/cheddl.h"
#include "asterfort/codent.h"
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
    integer :: nbcpmx, nbddl, nbdif, llref, numint, iret, i, j, nbec, nbcmp, neq
    integer :: nunoe, iran, lldesc, ord, nliais, llint3, llint4, llact, llnoe
    integer :: lldeeq, nbnoe, inoe
    parameter (nbcpmx=300)
    integer :: idec(nbcpmx), ivddl(nbddl)
    character(len=4) :: nliai
    character(len=8) :: basmod, nomint, intf, temp
    character(len=8) :: k8bid
    character(len=19) :: numddl
    character(len=24) :: noeint, actint, ordol, ordod
    character(len=24) :: valk(2)
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
!   SI ON A DONNE UNE BASE MODALE
!
    if (basmod(1:1) .ne. ' ') then
!
        call jeveuo(basmod//'           .REFD', 'L', llref)
        intf=zk24(llref+4)(1:8)
        if (intf .eq. ' ') then
            valk (1) = basmod
            call u2mesg('F', 'ALGORITH12_30', 1, valk, 0,&
                        0, 0, 0.d0)
        endif
        numddl=zk24(llref+3)(1:19)
!
!  SI ON A DONNE UNE LIST_INTERFACE
!
    else
        if (intf(1:1) .ne. ' ') then
            call jeveuo(intf//'.IDC_REFE', 'L', llref)
            numddl=zk24(llref+1)(1:19)
        else
            valk (1) = basmod
            valk (2) = intf
            call u2mesg('F', 'ALGORITH12_31', 2, valk, 0,&
                        0, 0, 0.d0)
        endif
    endif
!
!--------------RECUPERATION DONNEE GRANDEUR SOUS-JACENTE----------------
!
    call dismoi('F', 'NB_CMP_MAX', intf, 'INTERF_DYNA', nbcmp,&
                k8bid, iret)
    call dismoi('F', 'NB_EC', intf, 'INTERF_DYNA', nbec,&
                k8bid, iret)
!
!----------------RECUPERATION EVENTUELLE DU NUMERO INTERFACE------------
!
    if (nomint .ne. '             ') then
        call jenonu(jexnom(intf//'.IDC_NOMS', nomint), numint)
    endif
!
!-----------RECUPERATION DU NOMBRE DE DDL PHYSIQUES ASSEMBLES-----------
!
!
    call dismoi('F', 'NB_EQUA', numddl, 'NUME_DDL', neq,&
                k8bid, iret)
!
!--------------------RECUPERATION DE LA LISTE DDL ACTIF-----------------
!
    actint=intf//'.IDC_DDAC'
    call jeveuo(jexnum(actint, numint), 'L', llact)
!
!----------RECUPERATION DU NOMBRE DE NOEUD DE L' INTERFACES-------------
!
    noeint=intf//'.IDC_LINO'
!
    call jeveuo(jexnum(noeint, numint), 'L', llnoe)
    call jelira(jexnum(noeint, numint), 'LONMAX', nbnoe)
!
!---------------RECUPERATION DU DESCRIPTEUR DES DEFORMEES---------------
!
    call jeveuo(intf//'.IDC_DEFO', 'L', lldesc)
!
!------------------RECUPERATION ADRESSE DEEQ----------------------------
!
!----ON AJOUT .NUME POUR OBTENIR LE PROF_CHNO
    numddl(15:19)='.NUME'
    call jeveuo(numddl//'.DEEQ', 'L', lldeeq)
!
!------------------------RECUPERATION DES RANG--------------------------
!
    do 20 i = 1, nbnoe
        if (ord .eq. 0) then
            inoe=zi(llnoe+i-1)
            nunoe=zi(lldesc+inoe-1)
            call isdeco(zi(llact+(i-1)*nbec+1-1), idec, nbcmp)
        else
            temp='&&OP0126'
            call codent(nliais, 'D', nliai)
            ordol=temp//'      .LINO.'//nliai
            call jeveuo(ordol, 'L', llint3)
            ordod=temp//'      .LDAC.'//nliai
            call jeveuo(ordod, 'L', llint4)
            inoe=zi(llint3+i-1)
            nunoe=zi(lldesc+inoe-1)
            call isdeco(zi(llint4+(i-1)*nbec+1-1), idec, nbcmp)
        endif
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
20  end do
!
    nbdif=-nbdif
!
    call jedema()
end subroutine
