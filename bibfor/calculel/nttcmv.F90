subroutine nttcmv(model , mate  , cara_elem, list_load, nume_dof,&
                  solver, time  , tpsthe   , tpsnp1   , reasvt  ,&
                  reasmt, creas , vtemp    , vtempm   , vec2nd  ,&
                  matass, maprec, cndirp   , cnchci   , cnchtp)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/asasve.h"
#include "asterfort/ascavc.h"
#include "asterfort/ascova.h"
#include "asterfort/asmatr.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mecact.h"
#include "asterfort/medith.h"
#include "asterfort/mertth.h"
#include "asterfort/metnth.h"
#include "asterfort/preres.h"
#include "asterfort/vechth.h"
#include "asterfort/vedith.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=24), intent(in) :: model
    character(len=24), intent(in) :: mate
    character(len=24), intent(in) :: cara_elem
    character(len=19), intent(in) :: list_load
    character(len=24), intent(in) :: nume_dof
    character(len=19), intent(in) :: solver
    character(len=24), intent(in) :: time
    aster_logical :: reasvt, reasmt
    real(kind=8) :: tpsthe(6), tpsnp1
    character(len=1) :: creas
    character(len=19) :: maprec
    character(len=24) :: timemo
    character(len=24) :: vtemp, vtempm, vec2nd
    character(len=24) :: matass, cndirp, cnchci, cnchtp
!
! --------------------------------------------------------------------------------------------------
!
! COMMANDE THER_MOBI_NLINE : ACTUALISATION
!   - DES VECTEURS CONTRIBUANT AU SECOND MEMBRE
!   - DE LA MATRICE ASSEMBLEE (EVENTUELLEMENT)
!
! --------------------------------------------------------------------------------------------------
!
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ibid, k, iret, ierr, nbmat, jmet
    integer :: jmer, jmed, j2nd, lonch
    character(len=1) :: typres
    character(len=8) :: nomcmp(6)
    character(len=24) :: ligrmo, merigi, mediri, tlimat(3)
    character(len=24) :: vediri, vechtp, vadirp, vachtp, metrnl
    real(kind=8), pointer :: chtp(:) => null()
    real(kind=8), pointer :: dirp(:) => null()
    character(len=24) :: lload_name, lload_info, lload_func
!
    data typres /'R'/
    data nomcmp /'INST    ','DELTAT  ','THETA   ','KHI     ',&
                 'R       ','RHO     '/
    data merigi        /'&&METRIG           .RELR'/
    data mediri        /'&&METDIR           .RELR'/
    data metrnl        /'&&METNTH           .RELR'/
    data vediri        /'&&VETDIR           .RELR'/
    data vechtp        /'&&VETCHA           .RELR'/
    data timemo        /'&&OP0171.TIMEMO'/
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    vadirp = '&&VATDIR'
    vachtp = '&&VATCHA'
    creas = ' '
    lload_name = list_load(1:19)//'.LCHA'
    lload_info = list_load(1:19)//'.INFC'
    lload_func = list_load(1:19)//'.FCHA'
!
! ======================================================================
!         VECTEURS (CHARGEMENTS) CONTRIBUANT AU SECOND MEMBRE
! ======================================================================
!
    if (reasvt) then
!
! --- (RE)ACTUALISATION DU CHAMP CONSTANT EN ESPACE : TIME
!
        ligrmo = model(1:8)//'.MODELE'
        call mecact('V', time, 'MODELE', ligrmo, 'INST_R',&
                    ncmp=6, lnomcmp=nomcmp, vr=tpsthe)
!
!       ON CREE CETTE CARTE IDENTIQUE A TIME MAIS AVEC 1-THETA=1
!       A LA PLACE DE THETA POUR PERMETTRE LE CALCUL DE LA CHARGE
!       D'ECHANGE_PAROI
!
        tpsthe(3) = 1.d0
        call mecact('V', timemo, 'MODELE', ligrmo, 'INST_R',&
                    ncmp=6, lnomcmp=nomcmp, vr=tpsthe)
        tpsthe(3) = 0.d0
!
! --- TEMPERATURES IMPOSEES                                  ---> CNDIRP
!
        call vedith(model, list_load, time, vediri)
        call asasve(vediri, nume_dof, typres, vadirp)
        call ascova('D', vadirp, lload_func, 'INST', tpsthe(1),&
                    typres, cndirp)
        call jeveuo(cndirp(1:19)//'.VALE', 'E', vr=dirp)
!
! --- CHARGES CINEMATIQUES                                   ---> CNCHCI
!
        cnchci = ' '
        call ascavc(lload_name, lload_info, lload_func, nume_dof, tpsnp1,&
                    cnchci)
!
! --- CHARGEMENTS THERMIQUES                                 ---> CNCHTP
!            RQ : POUR LE CALCUL THERMIQUE, LES ARGUMENTS VTEMPP,
!                 VTEMPD ET THETA SONT INUTILISES.
!
        call vechth('MOVE' , model, lload_name, lload_info, cara_elem,&
                    mate   , time , vtemp , vechtp,&
                    time_move_ = timemo)
        call asasve(vechtp, nume_dof, typres, vachtp)
        call ascova('D', vachtp, lload_func, 'INST', tpsthe(1),&
                    typres, cnchtp)
        call jeveuo(cnchtp(1:19)//'.VALE', 'E', vr=chtp)
        call jelira(cnchtp(1:19)//'.VALE', 'LONMAX', lonch)
!
! --- SECOND MEMBRE COMPLET                                  ---> VEC2ND
!
        call jeveuo(vec2nd(1:19)//'.VALE', 'E', j2nd)
        do k = 1, lonch
            zr(j2nd+k-1) = chtp(k) + dirp(k)
        end do
!
    endif
!
! ======================================================================
!              MATRICE ASSEMBLEE
! ======================================================================
!
    if (reasmt) then
!
! --- (RE)CALCUL DE LA MATRICE DES DIRICHLET POUR L'ASSEMBLER
!
        call medith(model, list_load, mediri)
        call jeveuo(mediri, 'L', jmed)
!
! --- (RE)ASSEMBLAGE DE LA MATRICE ET CALCUL DES "REACTIONS D'APPUI"
!
        creas = 'M'
        call mertth(model, lload_name, lload_info, cara_elem, mate,&
                    time, vtemp, vtempm, merigi)
!
        call metnth(model, lload_name, cara_elem, mate, time,&
                    vtempm, metrnl)
!
        nbmat = 0
        call jeveuo(merigi, 'L', jmer)
        if (zk24(jmer)(1:8) .ne. '        ') then
            nbmat = nbmat + 1
            tlimat(nbmat) =merigi(1:19)
        endif
!
        call jeexin(metrnl, iret)
        if (iret .gt. 0) then
            call jeveuo(metrnl, 'L', jmet)
            if (zk24(jmet)(1:8) .ne. '        ') then
                nbmat = nbmat + 1
                tlimat(nbmat) =metrnl(1:19)
            endif
        endif
!
        if (zk24(jmed)(1:8) .ne. '        ') then
            nbmat = nbmat + 1
            tlimat(nbmat) =mediri(1:19)
        endif
!
! --- ASSEMBLAGE DE LA MATRICE
!
        call asmatr(nbmat, tlimat, ' ', nume_dof, solver,&
                    list_load, 'ZERO', 'V', 1, matass)
!
! --- DECOMPOSITION OU CALCUL DE LA MATRICE DE PRECONDITIONNEMENT
!
        call preres(solver, 'V', ierr, maprec, matass,&
                    ibid, -9999)
!
    endif
!-----------------------------------------------------------------------
    call jedema()
end subroutine
