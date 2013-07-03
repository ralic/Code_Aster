subroutine bmnoin(basmdz, intfz, nmintz, numint, nbnoi,&
                  numnoe, nbdif)
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
    implicit none
#include "jeveux.h"
!
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/u2mesg.h"
    character(len=8) :: intf, nomint, basmod
    character(len=*) :: intfz, nmintz, basmdz
!
!***********************************************************************
!    P. RICHARD     DATE 09/0491/
!-----------------------------------------------------------------------
!  BUT:   < BASE MODALE NOEUDS D'INTERFACE>
!
!   RENDRE LES NUMERO MAILLAGE DES NOEUDS DE L'INTERFACE
!  DANS UNE LIST_INTERFACE OU UNE BASE_MODALE
!
!
!-----------------------------------------------------------------------
!
! BASMDZ   /I/: NOM UTILISATEUR DE LA BASE MODALEE
! INTFZ    /I/: NOM UTILISATEUR DE LA LISTE INTERFACE
! NMINTZ   /I/: NOM DE L'INTERFACE
! NUMINT   /I/: NUMERO DE L'INTERFACE
! NBNOI    /I/: NOMBRE DE NOEUDS ATTENDUS
! NUMNOE   /O/: VECTEUR DES NUMERO DE NOEUDS
! NBDIF    /O/: NOMBRE NOEUDS TROUVES - NOMBRE NOEUDS ATTENDUS
!
!
!
!
!
!
    character(len=24) :: noeint
    character(len=24) :: valk(2)
    integer :: numnoe(nbnoi)
    character(len=1) :: k1bid
!
!-----------------------------------------------------------------------
!
!
!---------------RECUPERATION INTERF_DYNA ET NUME_DDL-----------------
!                 SI DONNEE BASE MODALE OU INTERF_DYNA
!
!-----------------------------------------------------------------------
    integer :: i, inoe, lldes, llint, llref, nbdif, nbeffi
    integer :: nbnoe, nbnoi, numcou, numint
!-----------------------------------------------------------------------
    call jemarq()
    intf = intfz
    nomint = nmintz
    basmod = basmdz
!
    if (basmod(1:1) .ne. ' ') then
        call jeveuo(basmod//'           .REFD', 'L', llref)
        intf=zk24(llref+4)(1:8)
        if (intf .eq. '        ') then
            valk (1) = basmod
            call u2mesg('F', 'ALGORITH12_30', 1, valk, 0,&
                        0, 0, 0.d0)
        endif
    else
        if (intf(1:1) .eq. ' ') then
            valk (1) = basmod
            valk (2) = intf
            call u2mesg('F', 'ALGORITH12_31', 2, valk, 0,&
                        0, 0, 0.d0)
        endif
    endif
!
!----------------RECUPERATION EVENTUELLE DU NUMERO INTERFACE------------
!
    if (nomint .ne. '             ') then
        call jenonu(jexnom(intf//'.IDC_NOMS', nomint), numint)
    endif
!
!
!----------RECUPERATION DU NOMBRE DE NOEUD DE L' INTERFACES-------------
!
    noeint=intf//'.IDC_LINO'
!
    call jelira(jexnum(noeint, numint), 'LONMAX', nbnoe, k1bid)
    call jeveuo(jexnum(noeint, numint), 'L', llint)
!
!----------------------TEST SUR NOMBRE DE NOEUDS ATTENDU----------------
!
    if (nbnoi .eq. 0) then
        nbdif=nbnoe
        goto 9999
    else
        nbeffi=min(nbnoi,nbnoe)
        nbdif=nbnoi
    endif
!
!------------RECUPERATION DU DESCRIPTEUR DE DEFORMEES-------------------
!
!
    call jeveuo(intf//'.IDC_DEFO', 'L', lldes)
!
!------------------------COMPTAGE DES DDL-------------------------------
!
!
!  COMPTAGE
!
    do 20 i = 1, nbeffi
        inoe=zi(llint+i-1)
        numcou=zi(lldes+inoe-1)
        nbdif=nbdif-1
        if (nbdif .ge. 0) numnoe(nbnoi-nbdif)=numcou
20  end do
!
    nbdif=-nbdif
!
!
9999  continue
    call jedema()
end subroutine
