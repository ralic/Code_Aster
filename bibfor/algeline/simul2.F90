subroutine simul2(resu, nomcmd, masse, modsta, nbdir,&
                  dir, nomnoe, nbno)
    implicit none
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mrmult.h"
#include "asterfort/mtdscr.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsorac.h"
#include "asterfort/rsvpar.h"
#include "asterfort/typddl.h"
#include "asterfort/utmess.h"
#include "asterfort/vtcrem.h"
#include "asterfort/wkvect.h"
!
    integer :: nbdir, nbno
    real(kind=8) :: dir(*)
    character(len=*) :: resu, nomcmd, masse, modsta, nomnoe(*)
    character(len=19) :: mass2
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!
!     OPERATEUR :   CALC_CHAR_SEISME
!
!     CREE LE VECTEUR SECOND MEMBRE DANS LE CAS D'UN CALCUL SISMIQUE
!     STRUCTURE : MULTI-APPUI
!
!     ------------------------------------------------------------------
!
    integer :: lmat, neq, ibid, iordr(1), ier
    real(kind=8) :: r8b, epsi
    character(len=8) :: k8b, cmp(6), crit
    character(len=24) :: valk(3)
    character(len=14) :: nume
    character(len=16) :: acces
    character(len=19) :: resu2, chamno
    complex(kind=8) :: c16b
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, id, idchm, iddl, idmst, idve, ie
    integer :: in, iret, nba, nbb, nbl, nbliai, nbtrou
!
    real(kind=8) :: xd
!-----------------------------------------------------------------------
    data cmp / 'DX' , 'DY' , 'DZ' , 'DRX' , 'DRY' , 'DRZ' /
!     ------------------------------------------------------------------
    call jemarq()
    resu2 = resu
    epsi = r8prem()
    ier = 0
!
    call mtdscr(masse)
    mass2=masse
    call jeveuo(mass2//'.&INT', 'E', lmat)
    call dismoi('F', 'NB_EQUA', masse, 'MATR_ASSE', neq,&
                k8b, ie)
    call dismoi('F', 'NOM_NUME_DDL', masse, 'MATR_ASSE', ibid,&
                nume, ie)
    call wkvect('&&SIMUL2.VECTEUR', 'V V R', neq, idve)
!
!     --- CREATION DU CHAM_NO RESULTAT ---
!
    call vtcrem(resu2, masse, 'G', 'R')
    call jeveuo(resu2//'.VALE', 'E', idchm)
!
!     --- ON BOUCLE SUR LES NOEUDS ---
!
    do 10 id = 1, nbdir
        xd = dir(id)
        if (abs(xd) .gt. epsi) then
            do 20 in = 1, nbno
                acces(1:8 ) = nomnoe(in)
                acces(9:16) = cmp(id)
!
!              --- ON RECUPERE LE MODE STATIQUE ASSOCIE AU NOEUD ---
                call rsorac(modsta, 'NOEUD_CMP', ibid, r8b, acces,&
                            c16b, epsi, crit, iordr, 1,&
                            nbtrou)
                if (nbtrou .ne. 1) then
                    ier = ier + 1
                    valk (1) = acces(1:8)
                    valk (2) = acces(9:16)
                    call utmess('E', 'ALGELINE5_41', nk=2, valk=valk)
                    goto 20
                endif
                call rsvpar(modsta, iordr(1), 'TYPE_DEFO', ibid, r8b,&
                            'DEPL_IMPO', iret)
                if (iret .ne. 100) then
                    ier = ier + 1
                    valk (1) = 'MODE_MECA'
                    valk (2) = acces(1:8)
                    valk (3) = acces(9:16)
                    call utmess('E', 'ALGELINE5_42', nk=3, valk=valk)
                    goto 20
                endif
                call rsexch(' ', modsta, 'DEPL', iordr(1), chamno,&
                            iret)
                if (iret .ne. 0) then
                    ier = ier + 1
                    valk (1) = chamno
                    valk (2) = acces(1:8)
                    valk (3) = acces(9:16)
                    call utmess('E', 'ALGELINE5_43', nk=3, valk=valk)
                    goto 20
                else
                    call jeveuo(chamno//'.VALE', 'L', idmst)
!
!                 --- ON EFFECTUE LE PRODUIT  MASSE * CHAM_NO ---
                    do 22 i = 0, neq-1
                        zr(idve+i) = -xd * zr(idmst+i)
22                  continue
                    call jelibe(chamno//'.VALE')
                    call mrmult('CUMU', lmat, zr(idve), zr(idchm), 1,&
                                .true.)
                endif
20          continue
        endif
10  end do
    if (ier .ne. 0) then
        call utmess('F', 'ALGELINE5_40')
    endif
!
    call wkvect('&&SIMUL2.DDL.BLOQUE', 'V V I', neq, iddl)
    call typddl('BLOQ', nume, neq, zi(iddl), nba,&
                nbb, nbl, nbliai)
!
    do 30 in = 0, neq-1
        zr(idchm+in) = ( 1 - zi(iddl+in) ) * zr(idchm+in)
30  end do
!
! --- MENAGE
    call jelibe(resu2//'.VALE')
    call jedetr('&&SIMUL2.VECTEUR')
    call jedetr('&&SIMUL2.DDL.BLOQUE')
!
    call jedema()
end subroutine
