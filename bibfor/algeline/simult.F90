subroutine simult()
    implicit none
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!
!     OPERATEUR :   CALC_CHAR_SEISME
!
!     CREE LE VECTEUR SECOND MEMBRE DANS LE CAS D'UN CALCUL SISMIQUE
!     STRUCTURE : MULTI-APPUI
!
!     ------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/compno.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvem.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/simul2.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    real(kind=8) :: xnorm, depl(6)
    character(len=8) :: masse, modsta, mailla, nomnoe
    character(len=16) :: type, nomcmd
    character(len=19) :: resu
    character(len=24) :: magrno, manono
    character(len=8) :: kbid
    integer :: iarg
!     ------------------------------------------------------------------
!
!     --- RECUPERATION DES ARGUMENTS DE LA COMMANDE ---
!
!-----------------------------------------------------------------------
    integer :: i,  idno, ii, in, ldgn
    integer :: nb, nbd, nbdir, nbgr, nbno, nbv
    character(len=24), pointer :: group_no(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    magrno = ' '
    manono = ' '
    resu = ' '
    call getres(resu, type, nomcmd)
!
!     --- MATRICE DE MASSE ---
!
    call getvid(' ', 'MATR_MASS', scal=masse, nbret=nbv)
    call dismoi('NOM_MAILLA', masse, 'MATR_ASSE', repk=mailla)
!
!     --- QUELLE EST LA DIRECTION ? ---
!
    call getvr8(' ', 'DIRECTION', nbval=0, nbret=nbd)
    nbdir = -nbd
    call getvr8(' ', 'DIRECTION', nbval=nbdir, vect=depl, nbret=nbd)
!
!     --- ON NORMALISE LE VECTEUR ---
    xnorm = 0.d0
    do i = 1, nbdir
        xnorm = xnorm + depl(i) * depl(i)
    end do
    xnorm = sqrt(xnorm)
    if (xnorm .lt. 0.d0) then
        call utmess('F', 'ALGORITH9_81')
    endif
    do i = 1, nbdir
        depl(i) = depl(i) / xnorm
    end do
!
!     --- ON RECUPERE LES MODES STATIQUES ---
!
    call getvid(' ', 'MODE_STAT', scal=modsta, nbret=nbv)
!
!     --- ON RECUPERE LES POINTS D'ANCRAGE ---
!
    call getvem(mailla, 'NOEUD', ' ', 'NOEUD', 0,&
                iarg, 0, kbid, nbno)
    if (nbno .ne. 0) then
!
!        --- ON RECUPERE UNE LISTE DE NOEUD ---
        nbno = -nbno
        call wkvect('&&SIMULT.NOEUD', 'V V K8', nbno, idno)
        call getvem(mailla, 'NOEUD', ' ', 'NOEUD', 0,&
                    iarg, nbno, zk8(idno), nbv)
    else
!
!        --- ON RECUPERE UNE LISTE DE GROUP_NO ---
        call getvem(mailla, 'GROUP_NO', ' ', 'GROUP_NO', 0,&
                    iarg, 0, kbid, nbgr)
        nbgr = -nbgr
        AS_ALLOCATE(vk24=group_no, size=nbgr)
        call getvem(mailla, 'GROUP_NO', ' ', 'GROUP_NO', 0,&
                    iarg, nbgr, group_no, nbv)
!
!        --- ECLATE LE GROUP_NO EN NOEUD ---
        call compno(mailla, nbgr, group_no, nbno)
        call wkvect('&&SIMULT.NOEUD', 'V V K8', nbno, idno)
        magrno = mailla//'.GROUPENO'
        manono = mailla//'.NOMNOE'
        ii = -1
        do i = 1, nbgr
            call jelira(jexnom(magrno, group_no(i)), 'LONUTI', nb)
            call jeveuo(jexnom(magrno, group_no(i)), 'L', ldgn)
            do in = 0, nb-1
                call jenuno(jexnum(manono, zi(ldgn+in)), nomnoe)
                ii = ii + 1
                zk8(idno+ii) = nomnoe
            end do
        end do
    endif
    call simul2(resu, nomcmd, masse, modsta, nbdir,&
                depl, zk8(idno), nbno)
!
! --- MENAGE
    call jedetr('&&SIMULT.NOEUD')
    AS_DEALLOCATE(vk24=group_no)
!
    call jedema()
end subroutine
