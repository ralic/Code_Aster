subroutine nxacmv(model , mate  , cara_elem, list_load, nume_dof,&
                  solver, lostat, time     , tpsthe   , reasvc  ,&
                  reasvt, reasmt, reasrg   , reasms   , creas   ,&
                  vtemp , vhydr , varc_curr, tmpchi   , tmpchf  ,&
                  vec2nd, vec2ni, matass   , maprec   , cndirp  ,&
                  cnchci, mediri, compor)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/asasve.h"
#include "asterfort/ascavc.h"
#include "asterfort/ascova.h"
#include "asterfort/asmatr.h"
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mecact.h"
#include "asterfort/memsth.h"
#include "asterfort/mergth.h"
#include "asterfort/merxth.h"
#include "asterfort/preres.h"
#include "asterfort/vechnl.h"
#include "asterfort/vechth.h"
#include "asterfort/vedith.h"
#include "asterfort/vetnth.h"
#include "asterfort/vrcins.h"
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
! person_in_charge: jessica.haelewyn at edf.fr
! aslint: disable=W1504
!
    character(len=24), intent(in) :: model
    character(len=24), intent(in) :: mate
    character(len=24), intent(in) :: cara_elem
    character(len=19), intent(in) :: list_load
    character(len=24), intent(in) :: nume_dof
    character(len=19), intent(in) :: solver
    character(len=24), intent(in) :: time
    character(len=19), intent(in) :: varc_curr
    aster_logical :: reasvc, reasvt, reasmt, reasrg, reasms, lostat
    real(kind=8) :: tpsthe(6)
    character(len=1) :: creas
    character(len=19) :: maprec
    character(len=24) :: vtemp, vec2nd, vec2ni, vhydr, compor, tmpchi
    character(len=24) :: tmpchf, matass, cndirp, cnchci, cnchtp, cntntp, cntnti
    character(len=24) :: cnchnl
!
! --------------------------------------------------------------------------------------------------
!
! COMMANDE THER_LINEAIRE : ACTUALISATION EVENTUELLE
!   - DES VECTEURS CONTRIBUANT AU SECOND MEMBRE
!   - DE LA MATRICE ASSEMBLEE
! COMMANDE THER_NON_LINE : ACTUALISATION EVENTUELLE
!   - DES VECTEURS CONTRIBUANT AU SECOND MEMBRE (RESIDU)
!   - DE LA MATRICE TANGENTE ASSEMBLEE
!
! OUT VEC2ND  : VECTEUR ASSEMBLE SECOND MEMBRE = L1(V,T-)
! OUT VEC2NI  : VECTEUR ASSEMBLE SECOND MEMBRE = L1(V,T-)
!                         AVEC RHOCP.T- ET NON PLUS H(T-)
! OUT MATASS,MAPREC  MATRICE DE RIGIDITE ASSEMBLEE
! OUT CNDIRP  : VECTEUR ASSEMBLE DES DIRICHLETS
! OUT CNCHCI  : ????????
! IN  VTEMP   : CHAMP DE LA TEMPERATURE A L'INSTANT PRECEDENT
!
! --------------------------------------------------------------------------------------------------
!
    character(len=6) :: nompro
    parameter ( nompro = 'NXACMV' )
    integer :: ibid, k, ierr, nbmat, lonch, loncm1, jmer, jmed, jmem, jtn, jtni
    integer :: jnchtp, jndirp, jnchnl, jntntp, jntnti, j2nd, j2nd1, j2nd2, j2nd3
    integer :: j2ni, j2ni1, j2ni2, j2ni3, iret, typcum, ifm, niv
    character(len=1) :: typres
    character(len=2) :: codret
    character(len=8) :: k8bid, nomcmp(6)
    character(len=16) :: k16bid, option, nomcmd
    character(len=24) :: ligrmo, merigi, memass, mediri, tlimat(3), bidon
    character(len=24) :: vediri, vechtp, vetntp, vetnti, vadirp, vachtp, vechtn
    character(len=24) :: vachtn, vtemp2
    aster_logical :: llin
    character(len=24) :: lload_name, lload_info, lload_func
!
    data typres /'R'/
    data nomcmp /'INST    ','DELTAT  ','THETA   ','KHI     ',&
     &             'R       ','RHO     '/
    data memass /'&&METMAS           .RELR'/
    data merigi /'&&METRIG           .RELR'/
    data vediri /'&&VETDIR           .RELR'/
    data vechtp /'&&VETCHA           .RELR'/
    data vechtn /'&&VENCHA           .RELR'/
    data vetnti,vetntp /'&&VETNTI           .RELR',&
     &                    '&&VETNTH           .RELR'/
    data cnchtp,cntntp,cntnti,cnchnl  /4*' '/
    data bidon  /' '/
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call infniv(ifm, niv)
!
!====
! 1. PREALABLE
!====
! RECUPERATION DU NOM DE LA CMD
    call getres(k8bid, k16bid, nomcmd)
    vadirp = '&&'//nompro//'VATDIR'
    vachtp = '&&'//nompro//'VATCHA'
    vachtn = '&&'//nompro//'VANCHA'
    creas = ' '
    lload_name = list_load(1:19)//'.LCHA'
    lload_info = list_load(1:19)//'.INFC'
    lload_func = list_load(1:19)//'.FCHA'
!
! DETERMINATION DU TYPE DE CALCUL (LINEAIRE OU NON)
    if (nomcmd(1:13) .eq. 'THER_LINEAIRE') then
        llin = .true.
    else
        llin = .false.
    endif
!
    if (niv .eq. 2) then
        write(ifm,*)
        write(ifm,*)'*******************************************'
        write(ifm,*)' CALCUL DE SECOND MEMBRE THERMIQUE: NXACMV'
        write(ifm,*)
        write(ifm,*)' INST                 :',tpsthe(1)
        write(ifm,*)' CALCUL LINEAIRE      :',llin
        write(ifm,*)' REASVT/REASVC        :',reasvt,reasvc
        write(ifm,*)' REASMT/REASRG/REASMS :',reasmt,reasrg,reasms
        write(ifm,*)' VTEMP                :',vtemp
        write(ifm,*)
    endif
! ======================================================================
! 1.      VECTEURS (CHARGEMENTS) CONTRIBUANT AU SECOND MEMBRE
!            REACTUALISES AU DEBUT DE CHAQUE PAS DE TEMPS
! ======================================================================
!
!     VARIABLES DE COMMANDE
    call vrcins(model, mate, cara_elem, tpsthe(1), varc_curr,&
                codret)
!
    if (reasvt) then
!
! 1.1. ==> (RE)ACTUALISATION DU CHAMP CONSTANT EN ESPACE : TIME
!
        ligrmo = model(1:8)//'.MODELE'
        call mecact('V', time, 'MODELE', ligrmo, 'INST_R',&
                    ncmp=6, lnomcmp=nomcmp, vr=tpsthe)
!
! 1.2. ==> TEMPERATURES IMPOSEES (DIRICHLET)                 ---& CNDIRP
        call vedith(model, list_load, time, vediri)
        call asasve(vediri, nume_dof, typres, vadirp)
        call ascova('D', vadirp, lload_func, 'INST', tpsthe(1),&
                    typres, cndirp)
        call jeveuo(cndirp(1:19)//'.VALE', 'L', jndirp)
!
! 1.3. ==> CHARGES CINEMATIQUES                              ---& CNCHCI
!
        cnchci = ' '
        call ascavc(lload_name, lload_info, lload_func, nume_dof, tpsthe(1),&
                    cnchci)
!
! 1.4. ==> CONTRIBUTION DU CHAMP A L'INSTANT PRECEDENT       ---& CNTNTP
! --- IDEM AVEC RHOCP ET NON ENTHALPIE                       ---& CNTNTI
!
        if (.not.lostat) then
            if (llin) then
! THERMIQUE LINEAIRE -------------------------------------
                option = 'CHAR_THER_EVOL  '
            else
! THERMIQUE NON LINEAIRE -----------------------------------
                option = 'CHAR_THER_EVOLNI'
            endif
!
! CALCULS ELEMENTAIRES ET SOMMATION DANS LES VECT_ELEM VETNTP
            call vetnth(option, model, cara_elem, mate, time,&
                        vtemp, compor, tmpchi, tmpchf, vhydr,&
                        vetntp, vetnti, varc_curr)
!
            call asasve(vetntp, nume_dof, typres, cntntp)
            call jeveuo(cntntp, 'L', jtn)
            call jeveuo(zk24(jtn)(1:19)//'.VALE', 'L', jntntp)
!
            if (.not.llin) then
                call asasve(vetnti, nume_dof, typres, cntnti)
                call jeveuo(cntnti, 'L', jtni)
                call jeveuo(zk24(jtni)(1:19)//'.VALE', 'L', jntnti)
            endif
!
        endif
! FIN DU IF REASVT
    endif
!
! ======================================================================
! 2.      VECTEURS (CHARGEMENTS) CONTRIBUANT AU SECOND MEMBRE
!        REACTUALISES AU DEBUT DE CHAQUE ITERATION DE COUPLAGE
!                  ET SOMMATION DES SECONDS MEMBRES
! ======================================================================
!
    if (reasvc) then
!
! ----- Neumann loads elementary vectors (second member)
!
        call vechth('STAT', model, lload_name, lload_info, cara_elem,&
                    mate  , time , vtemp     , vechtp,&
                    varc_curr_ = varc_curr)
!
! ----- Neumann loads assembled vector (second member)
!
        call asasve(vechtp, nume_dof, typres, vachtp)
        call ascova('D', vachtp, lload_func, 'INST', tpsthe(1),&
                    typres, cnchtp)
        call jeveuo(cnchtp(1:19)//'.VALE', 'L', jnchtp)
!
! 2.2. ==> CHARGEMENTS THERMIQUES NON LINEAIRES EN TEMPERATURE -& CNCHNL
        if (.not.llin) then
            call vechnl(model, lload_name, lload_info, cara_elem, time,&
                        vtemp, vechtn)
            call asasve(vechtn, nume_dof, typres, vachtn)
            call ascova('D', vachtn, bidon, 'INST', tpsthe(1),&
                        typres, cnchnl)
            call jeveuo(cnchnl(1:19)//'.VALE', 'L', jnchnl)
        endif
!
        if (lostat) then
            call jedetr(vechtp)
            call jedetr(vechtn)
        endif
!
! 2.3. ==> SECOND MEMBRE COMPLET
!
! 2.3.1. ==> RECHERCHE DU TYPE DE CUMUL
!            ON DISTINGUE LE GROS PAS DE TEMPS NUMERO 0 ET LES SUIVANTS
!
        if (lostat) then
!
! 2.3.1.1. ==> THERMIQUE LINEAIRE AU GROS PAS DE TEMPS NUMERO 0
!
            if (llin) then
!
! CALCUL STATIONNAIRE:
! ==> ON ASSEMBLE LES SECONDS MEMBRES DE DIRICHLET ET DE CHARGE
                typcum = 2
                j2nd1 = jnchtp
                j2nd2 = jndirp
!
! 2.3.1.2. ==> THERMIQUE NON LINEAIRE AU GROS PAS DE TEMPS NUMERO 0
            else
! ==> ON ASSEMBLE LES SECONDS MEMBRES DE CHARGE LINEAIRE ET NON-LINEAIRE
                typcum = 2
                j2nd1 = jnchtp
                j2nd2 = jnchnl
            endif
!
        else
!
! 2.3.1.3. ==> THERMIQUE LINEAIRE AUX GROS PAS DE TEMPS SUIVANTS
!
            if (llin) then
!
! CALCUL TRANSITOIRE:
! ==> ON ASSEMBLE LES SECONDS MEMBRES DE DIRICHLET, D'IMPLICITATION
!     ET DE CHARGE
                typcum = 3
                j2nd1 = jnchtp
                j2nd2 = jndirp
                j2nd3 = jntntp
!
! 2.3.1.4. ==> THERMIQUE NON LINEAIRE AUX GROS PAS DE TEMPS SUIVANTS
!
            else
! CALCUL TRANSITOIRE
!  ==> ON ASSEMBLE LES SECONDS MEMBRES DE CHARGE LINEAIRE, NON-LINEAIRE
!      ET D'IMPLICITATION (EN RHO_CP DANS J2NI POUR PRE-ITERATION DE
!      PREDICTION ET EN BETA DANS J2ND POUR ITERATIONS DE NEWTON)
                typcum = 33
                j2nd1 = jnchtp
                j2nd2 = jnchnl
                j2nd3 = jntntp
                j2ni1 = jnchtp
                j2ni2 = jnchnl
                j2ni3 = jntnti
            endif
! FIN IF LOSTAT
        endif
!
! --- SECOND MEMBRE COMPLET                                  ---& VEC2ND
! --- SECOND MEMBRE COMPLET                                  ---& VEC2NI
!
! 2.3.2. ==> ADRESSES ET LONGUEURS DU/DES SECOND(S) MEMBRE(S)
!
        call jeveuo(vec2nd(1:19)//'.VALE', 'E', j2nd)
        if (.not.llin) call jeveuo(vec2ni(1:19)//'.VALE', 'E', j2ni)
        call jelira(vec2nd(1:19)//'.VALE', 'LONMAX', lonch)
        loncm1 = lonch - 1
!
! 2.3.3. ==> CUMUL DES DIFFERENTS TERMES
!
        if (typcum .eq. 2) then
            do k = 0, loncm1
                zr(j2nd+k) = zr(j2nd1+k) + zr(j2nd2+k)
            end do
        else if (typcum.eq.3) then
            do k = 0, loncm1
                zr(j2nd+k) = zr(j2nd1+k) + zr(j2nd2+k) + zr(j2nd3+k)
            end do
        else if (typcum.eq.33) then
            do k = 0, loncm1
                zr(j2nd+k) = zr(j2nd1+k) + zr(j2nd2+k) + zr(j2nd3+k)
                zr(j2ni+k) = zr(j2ni1+k) + zr(j2ni2+k) + zr(j2ni3+k)
            end do
        else
            ASSERT(.false.)
        endif
! FIN IF REASVC
    endif
!
! ======================================================================
! 3.            MATRICE ASSEMBLEE
! ======================================================================
!
    if (reasmt .or. reasrg .or. reasms) then
!
! 3.1. ==> (RE)CALCUL DES MATRICES ELEMENTAIRES
!
! 3.1.1. ==> (RE)CALCUL DE LA MATRICE TANGENTE EN NON-LINEAIRE
!
        if (.not.llin) then
!
            creas = 'M'
            vtemp2 = vtemp
            call merxth(model, lload_name, lload_info, cara_elem, mate,&
                        time, vtemp2, merigi, compor, varc_curr,&
                        tmpchi, tmpchf)
!
! 3.1.2. ==> (RE)CALCUL DES MATRICES DE MASSE ET DE RIGIDITE EN LINEAIRE
!
        else
!
            if (reasms) then
                call memsth(model, cara_elem, mate, time, varc_curr,&
                            memass)
            endif
!
            if (reasrg) then
                call mergth(model, lload_name, lload_info, cara_elem, mate,&
                            time, varc_curr, merigi)
            endif
!
        endif
!
! 3.2. ==> ASSEMBLAGE DE LA MATRICE
!
        nbmat = 0
!
        call jeexin(merigi, iret)
        if (iret .gt. 0) then
            call jeveuo(merigi, 'L', jmer)
            if (zk24(jmer)(1:8) .ne. '        ') then
                nbmat = nbmat + 1
                tlimat(nbmat) = merigi
            endif
        endif
!
        call jeexin(mediri, iret)
        if (iret .gt. 0) then
            call jeveuo(mediri, 'L', jmed)
            if (zk24(jmed)(1:8) .ne. '        ') then
                nbmat = nbmat + 1
                tlimat(nbmat) = mediri
            endif
        endif
!
        call jeexin(memass, iret)
        if (iret .gt. 0) then
            call jeveuo(memass, 'L', jmem)
            if (zk24(jmem) (1:8) .ne. '        ') then
                nbmat = nbmat + 1
                tlimat(nbmat) =memass
            endif
        endif
!
        call asmatr(nbmat, tlimat, ' ', nume_dof, solver,&
                    lload_info, 'ZERO', 'V', 1, matass)
!
! 3.3. ==> DECOMPOSITION OU CALCUL DE LA MATRICE DE PRECONDITIONNEMENT
!
        call preres(solver, 'V', ierr, maprec, matass,&
                    ibid, -9999)
!
    endif
!
!-----------------------------------------------------------------------
    call jedema()
end subroutine
