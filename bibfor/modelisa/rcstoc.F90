subroutine rcstoc(nommat, nomrc, noobrc, nbobj, valr, valc,&
                  valk, nbr, nbc, nbk)
! aslint: disable=
    implicit none
#include "jeveux.h"
#include "asterc/getmjm.h"
#include "asterc/gettco.h"
#include "asterfort/assert.h"
#include "asterfort/focain.h"
#include "asterfort/foverf.h"
#include "asterfort/gcncon.h"
#include "asterfort/getvc8.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/lxlgut.h"
#include "asterfort/tbexp2.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/indk16.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"

    integer :: nbr, nbc, nbk, nbobj
    real(kind=8) :: valr(*)
    complex(kind=8) :: valc(*)
    character(len=8) :: nommat
    character(len=19) :: noobrc
    character(len=16) :: valk(*)
    character(len=32) :: nomrc

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

! But: Stocker dans les 3 tableaux VALR, VALC et VALK les reels, les
!      complexes et les k16 caracterisant la loi de comportement de nom nomrc
!      Creer si necessaire (ORDRE_PARAM) les objets .ORDR et .KORD et les remplir
!
!  in  nommat : nom utilisateur du materiau
!  in  nomrc  : nom de la relation de comportement (mot cle facteur)
!  in  nbobj  : nombre de mcsimps
!  out valr   : vecteur des valeurs reelles
!  out valk   : vecteur des k16
!  out valc   : vecteur des complexes
!  out nbr    : nombre de reels
!  out nbc    : nombre de complexes
!  out nbk    : nombre de concepts (fonction, trc, table, liste)
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: valr8, e1, ei, precma, valrr(4)
    character(len=8) :: valtx
    character(len=8) :: valch, nomcle(5)
    character(len=8) :: table
    character(len=19) :: rdep, nomfct, nomint
    character(len=8) :: num_lisv
    character(len=16) :: nom_lisv
    character(len=24) :: prol1, prol2, valkk(2)
    character(len=16) :: typeco
    complex(kind=8) :: valc8
    integer ::   ibk, nbmax, vali, itrou, n1, posi, kr, kc, kf
    integer :: i, k, ii,  jrpv, jvale, nbcoup, n, jlisvr, jlisvf,nmcs
    integer :: iret, nbfct, nbpts, jprol, nbptm, lpro1, lpro2
    character(len=32), pointer :: nomobj(:) => null()
    character(len=8), pointer :: typobj(:) => null()
    character(len=24), pointer :: prol(:) => null()
    character(len=16), pointer :: ordr(:) => null()
    integer, pointer :: kord(:) => null()
    aster_logical :: lordre, lverif
! ----------------------------------------------------------------------
!
    call jemarq()
    AS_ALLOCATE(vk8=typobj, size=nbobj)
    AS_ALLOCATE(vk32=nomobj, size=nbobj)

    nbr = 0
    nbc = 0
    nbk = 0


!   -- 1. Recuperation de la liste des mots cles utilises
!         et de leurs types associes => nomobj(:) et typboj(:)
!   -------------------------------------------------------------------------
    call getmjm(nomrc, 1, nbobj, nomobj, typobj, nmcs)


!   -- 2. Le mot cle ORDRE_PARAM est special (mot cle cache).
!         Il donne l'ordre des mots cles simples pour l'acces via la routine rcvalt.F90.
!         On le scrute et on le recopie dans .ORDR et puis on le retire de la liste.
!   ------------------------------------------------------------------------------------
    itrou=0
    lordre=.false.
    do i = 1, nmcs
        if (nomobj(i).eq.'ORDRE_PARAM') then
            itrou=i
            lordre=.true.
            call getvtx(nomrc, 'ORDRE_PARAM', iocc=1, nbval=0, vect=ordr, nbret=n1)
            ASSERT(n1.lt.0)
            call wkvect(noobrc//'.ORDR', 'G V K16', -n1, vk16=ordr)
            call getvtx(nomrc, 'ORDRE_PARAM', iocc=1, nbval=-n1, vect=ordr, nbret=n1)
            ASSERT(n1.gt.0)
        endif
    enddo
    if (itrou.gt.0) then
        do i = 1, nmcs
            if (i.gt.itrou) then
                nomobj(i-1)=nomobj(i)
                typobj(i-1)=typobj(i)
            endif
        enddo
        nmcs=nmcs-1
    endif


!   -- 3. On verifie que les mots cles simples ont moins de 16 carateres
!   ---------------------------------------------------------------------
    do i = 1, nmcs
        ASSERT (nomobj(i).ne.' ')
        ASSERT (typobj(i).ne.' ')
        if (lxlgut(nomobj(i)) .gt. 16) then
            call utmess('F','MODELISA9_84', sk=nomobj(i))
        endif
    enddo


!   -- 4. On modifie typobj pour remplacer 'R8' par 'LR8',
!         'C8' par 'LC8' et 'CO' par 'LFO' quand ce sont des listes.
!   -------------------------------------------------------------------------
    do i = 1, nmcs
        if (typobj(i)(1:2) .eq. 'R8') then
            call getvr8(nomrc, nomobj(i), iocc=1, scal=valr8, nbret=n)
            if (n.lt.0) then
                typobj(i)='LR8'
            else
                ASSERT(n.eq.1)
            endif
        elseif (typobj(i)(1:2) .eq. 'C8') then
            call getvc8(nomrc, nomobj(i), iocc=1, scal=valc8, nbret=n)
            if (n.lt.0) then
                typobj(i)='LC8'
            else
                ASSERT(n.eq.1)
            endif
        elseif (typobj(i)(1:2) .eq. 'CO') then
            call getvid(nomrc, nomobj(i), iocc=1, scal=valch, nbret=n)
            call gettco(valch, typeco)
            if (typeco == ' ') then
                call utmess('F', 'FONCT0_71', sk=nomobj(i))
            endif
            if (typeco.eq.'TABLE_SDASTER') cycle
            lverif=(typeco(1:8).eq.'FONCTION' .or. typeco(1:5).eq.'NAPPE' &
                    .or. typeco(1:7).eq.'FORMULE')
            ASSERT(lverif)
            if (n.lt.0) then
                typobj(i)='LFO'
            else
                ASSERT(n.eq.1)
            endif
        elseif (typobj(i)(1:2) .eq. 'TX') then
            call getvtx(nomrc, nomobj(i), iocc=1, scal=valch, nbret=n)
            ASSERT(n.eq.1)
        else
            ASSERT(.false.)
        endif
    enddo


!   -- 5. Si le mot cle ORDRE_PARAM est fourni, on verifie que TOUS les mots cles fournis
!         font partie de la liste et on cree l'objet .KORD :
!            .KORD(1) : n1 : nombre total des mots cles MSIMP possibles de MFACT
!                            (=longueur de .ORDR)
!            .KORD(2) : nmcs : nombre de mots cles MSIMP rellement utilises
!            .KORD(2+i) : i   (pour i=1,nmcs)
!                 posi : indice du mot cles dans  .ORDR
!            .KORD(2+nmcs+i) : kr   (pour i=1,nmcs)
!                - si kr > 0 :  est l'indice du mot cle dans .VALR
!                - sinon : le mot cle n'est pas reel
!            .KORD(2+2*nmcs+i) : kc   (pour i=1,nmcs)
!                - si kc > 0 :  est l'indice du mot cle dans .VALC
!                - sinon : le mot cle n'est pas complexe
!            .KORD(2+3*nmcs+i) : kf   (pour i=1,nmcs)
!                - si kf > 0 :  est l'indice du mot cle dans .VALK(nmcs:)
!                - sinon : le mot cle n'est pas un concept (fonction, TRC, liste)
!
!   ------------------------------------------------------------------------------------------------
    if (lordre) then
        call wkvect(noobrc//'.KORD', 'G V I', 2+4*nmcs, vi=kord)
        kord(1)=n1
        kord(2)=nmcs

        kr=0
        kc=0
        kf=0
        do i = 1, nmcs
            posi = indk16(ordr,nomobj(i),1,n1)
            if (posi.eq.0) then
                valkk(1)=nomrc
                valkk(2)=nomobj(i)
                call utmess('F','MODELISA6_81',nk=2,valk=valkk)
            else
                kord(2+i)=posi
            endif
            if (typobj(i) .eq. 'R8') then
                kr=kr+1
                kord(2+nmcs+i)=kr
            elseif (typobj(i) .eq. 'C8') then
                kc=kc+1
                kord(2+2*nmcs+i)=kc
            else
                kf=kf+1
                kord(2+3*nmcs+i)=kf
                ASSERT(typobj(i) .eq. 'CO')
            endif
        enddo
        ASSERT(kc.eq.0)
        ASSERT(kr+kf.eq.nmcs)
    endif


!   -- 6. Glute META_MECA*, BETON_DOUBLE_DP, RUPT_FRAG et CZM_LAB_MIX :
!         On traite les TX qu'on convertit en R8
!   --------------------------------------------------------------------
    do i = 1, nmcs
        if (typobj(i)(1:2) .eq. 'TX') then
            if (nomrc(1:9) .eq. 'ELAS_META') then
                call getvtx(nomrc, nomobj(i), iocc=1, scal=valtx, nbret=n)
                if (n .eq. 1) then
                    if (nomobj(i) .eq. 'PHASE_REFE' .and. valtx .eq. 'CHAUD') then
                        nbr = nbr + 1
                        valr(nbr) = 1.d0
                        valk(nbr) = nomobj(i)(1:16)
                        elseif( nomobj(i).eq.'PHASE_REFE' .and.&
                    valtx.eq.'FROID') then
                        nbr = nbr + 1
                        valr(nbr) = 0.d0
                        valk(nbr) = nomobj(i)(1:16)
                    endif
                endif
            else if (nomrc .eq. 'BETON_DOUBLE_DP') then
                call getvtx(nomrc, nomobj(i), iocc=1, scal=valtx, nbret=n)
                if (n .eq. 1) then
                    if (nomobj(i) .eq. 'ECRO_COMP_P_PIC' .or. nomobj(i) .eq.&
                        'ECRO_TRAC_P_PIC') then
                        nbr = nbr + 1
                        valk(nbr) = nomobj(i)(1:16)
                        if (valtx .eq. 'LINEAIRE') then
                            valr(nbr) = 0.d0
                        else
                            valr(nbr) = 1.d0
                        endif
                    endif
                endif
                elseif ((nomrc(1:9).eq.'RUPT_FRAG') .or.(&
            nomrc.eq.'CZM_LAB_MIX')) then
                call getvtx(nomrc, nomobj(i), iocc=1, scal=valtx, nbret=n)
                if (n .eq. 1) then
                    if (nomobj(i) .eq. 'CINEMATIQUE') then
                        nbr = nbr + 1
                        valk(nbr) = nomobj(i)(1:16)
                        if (valtx .eq. 'UNILATER') then
                            valr(nbr) = 0.d0
                        else if (valtx.eq.'GLIS_1D') then
                            valr(nbr) = 1.d0
                        else if (valtx.eq.'GLIS_2D') then
                            valr(nbr) = 2.d0
                        else
                            ASSERT(.false.)
                        endif
                    else
                        ASSERT(.false.)
                    endif
                endif
            endif
        endif
    end do


!   -- 7. Stockage des informations dans VALK, VALR et VALC :
!   ---------------------------------------------------------

!   -- 7.1 on traite les reels
!   --------------------------
    do i = 1, nmcs
        if (typobj(i)(1:3) .eq. 'R8 ') then
            call getvr8(nomrc, nomobj(i), iocc=1, scal=valr8, nbret=n)
            ASSERT(n.eq.1)
            nbr = nbr + 1
            valr(nbr) = valr8
            valk(nbr) = nomobj(i)(1:16)
        endif
    end do


!   -- 7.2 on traite les complexes
!   ------------------------------
    do i = 1, nmcs
        if (typobj(i)(1:3) .eq. 'C8 ') then
            call getvc8(nomrc, nomobj(i), iocc=1, scal=valc8, nbret=n)
            ASSERT(n.eq.1)
            nbc = nbc + 1
            valc(nbr+nbc) = valc8
            valk(nbr+nbc) = nomobj(i)(1:16)
        endif
    end do


!   -- 3.3 on traite ensuite les concepts CO puis les listes (LR8/LC8/LFO):
!   ------------------------------------------------------------------------

!   -- 7.3.1 : on stocke le nom des parametres concernes :
    do i = 1, nmcs
        if (typobj(i)(1:3) .eq. 'CO ') then
            call getvid(nomrc, nomobj(i), iocc=1, scal=valch, nbret=n)
            if (n .eq. 1) then
                nbk = nbk + 1
                valk(nbr+nbc+nbk) = nomobj(i)(1:16)
            else
                ASSERT(.false.)
                ASSERT(n.eq.0)
            endif
        endif
    end do

    do i = 1, nmcs
        if ((typobj(i) .eq. 'LR8') .or. (typobj(i) .eq. 'LC8') .or. (typobj(i) .eq. 'LFO')) then
            nbk = nbk + 1
            valk(nbr+nbc+nbk) = nomobj(i)(1:16)
        endif
    end do

!   -- 7.3.2 : on stocke le nom des structures de donnees : fonctions, TRC, listes
    ibk = 0
    do i = 1, nmcs
        if (typobj(i)(1:3) .eq. 'CO ') then
            call getvid(nomrc, nomobj(i), iocc=1, scal=valch, nbret=n)
            if (n .eq. 1) then
                ibk = ibk + 1
                valk(nbr+nbc+nbk+ibk) = valch
            else
                ASSERT(n.eq.0)
            endif
        endif
    end do

    do i = 1, nmcs
        if ((typobj(i) .eq. 'LR8') .or. (typobj(i) .eq. 'LC8') .or. (typobj(i) .eq. 'LFO')) then
            call gcncon('.', num_lisv)
            nom_lisv=nommat//num_lisv
            if (typobj(i) .eq. 'LR8') then
                call getvr8(nomrc, nomobj(i), iocc=1, scal=valr8, nbret=n)
                ASSERT(n.lt.0)
                n=-n
                call wkvect(nom_lisv//'.LISV_R8','G V R',n,jlisvr)
                call getvr8(nomrc, nomobj(i), iocc=1, nbval=n, vect=zr(jlisvr))
            elseif (typobj(i) .eq. 'LC8') then
                ASSERT(.false.)
            elseif (typobj(i) .eq. 'LFO') then
                call getvid(nomrc, nomobj(i), iocc=1, scal=valtx, nbret=n)
                ASSERT(n.lt.0)
                n=-n
                call wkvect(nom_lisv//'.LISV_FO','G V K8',n,jlisvf)
                call getvid(nomrc, nomobj(i), iocc=1, nbval=n, vect=zk8(jlisvf))
            endif
            ibk = ibk + 1
            valk(nbr+nbc+nbk+ibk) = nom_lisv
        endif
    end do


!   -- 7. creation d'une fonction pour stocker r(p) :
!   -------------------------------------------------
    if (( nomrc(1:8) .eq. 'TRACTION' ) .or. ( nomrc(1:13) .eq. 'META_TRACTION' )) then
        if (nomrc(1:8) .eq. 'TRACTION') then
            nomcle(1)(1:4)='SIGM'
        endif
        if (nomrc(1:13) .eq. 'META_TRACTION') then
            nomcle(1)(1:7)='SIGM_F1'
            nomcle(2)(1:7)='SIGM_F2'
            nomcle(3)(1:7)='SIGM_F3'
            nomcle(4)(1:7)='SIGM_F4'
            nomcle(5)(1:7)='SIGM_C '
        endif
        nbmax = 0
        do 149 ii = 1, nbk
            do 150 i = 1, nbk
                if ((valk(nbr+nbc+i)(1:6) .eq. 'SIGM  ') .or.&
                    (valk(nbr+nbc+i)(1:7) .eq. 'SIGM_F1') .or.&
                    (valk(nbr+nbc+i)(1:7) .eq. 'SIGM_F2') .or.&
                    (valk(nbr+nbc+i)(1:7) .eq. 'SIGM_F3') .or.&
                    (valk(nbr+nbc+i)(1:7) .eq. 'SIGM_F4') .or.&
                    (valk(nbr+nbc+i)(1:7) .eq. 'SIGM_C ')) then
                    nomfct = valk(nbr+nbc+nbk+i)
                    goto 151
                endif
150          continue
            call utmess('F', 'MODELISA6_70', sk=nomcle(ii))
151          continue
!
            call jeveuo(nomfct//'.PROL', 'L', vk24=prol)
            if (prol(1)(1:1) .eq. 'F') then
                call jelira(nomfct//'.VALE', 'LONMAX', nbptm)
                if (nomrc(1:8) .eq. 'TRACTION') then
                    if (nbptm .lt. 4) then
                        call utmess('F', 'MODELISA6_71', sk=nomcle(ii))
                    endif
                endif
                if (nomrc(1:13) .eq. 'META_TRACTION') then
                    if (nbptm .lt. 2) then
                        call utmess('F', 'MODELISA6_72', sk=nomcle(ii))
                    endif
                endif
                nbcoup = nbptm / 2
                if (nbptm .ge. nbmax) nbmax = nbptm
!
                call jeveuo(nomfct//'.VALE', 'L', jrpv)
                if (zr(jrpv) .le. 0.d0) then
                    valkk (1) = nomcle(ii)
                    valkk (2) = nomfct
                    valrr (1) = zr(jrpv)
                    call utmess('F', 'MODELISA9_59', nk=2, valk=valkk, sr=valrr(1))
                endif
                if (zr(jrpv+nbptm/2) .le. 0.d0) then
                    valkk (1) = nomcle(ii)
                    valkk (2) = nomfct
                    valrr (1) = zr(jrpv+nbptm/2)
                    call utmess('F', 'MODELISA9_60', nk=2, valk=valkk, sr=valrr(1))
                endif
!        VERIF ABSCISSES CROISSANTES (AU SENS LARGE)
                iret=2
                call foverf(zr(jrpv), nbcoup, iret)
                iret = 0
                e1 = zr(jrpv+nbcoup) / zr(jrpv)
                precma = 1.d-10
!
                do 200 i = 1, nbcoup-1
                    ei = (zr(jrpv+nbcoup+i) - zr(jrpv+nbcoup+i-1) ) / ( zr(jrpv+i) - zr(jrpv+i-1)&
                         )
                    if (ei .gt. e1) then
                        iret = iret + 1
                        valkk (1) = nomcle(ii)
                        valrr (1) = e1
                        valrr (2) = ei
                        valrr (3) = zr(jrpv+i)
                        call utmess('E', 'MODELISA9_61', sk=valkk(1), nr=3, valr=valrr)
                    else if ((e1-ei)/e1 .le. precma) then
                        valkk (1) = nomcle(ii)
                        valrr (1) = e1
                        valrr (2) = ei
                        valrr (3) = precma
                        valrr (4) = zr(jrpv+i)
                        call utmess('A', 'MODELISA9_62', sk=valkk(1), nr=4, valr=valrr)
                    endif
200              continue
                if (iret .ne. 0) then
                    call utmess('F', 'MODELISA6_73')
                endif
!
            else if (prol(1)(1:1) .eq. 'N') then
                call jelira(nomfct//'.VALE', 'NUTIOC', nbfct)
                nbptm = 0
                do 160 k = 1, nbfct
                    call jelira(jexnum(nomfct//'.VALE', k), 'LONMAX', nbpts)
                    nbcoup = nbpts / 2
                    if (nbpts .ge. nbmax) nbmax = nbpts
                    if (nomrc(1:8) .eq. 'TRACTION') then
                        if (nbpts .lt. 4) then
                            call utmess('F', 'MODELISA6_74')
                        endif
                    endif
                    if (nomrc(1:13) .eq. 'META_TRACTION') then
                        if (nbpts .lt. 2) then
                            call utmess('F', 'MODELISA6_75', sk=nomcle( ii))
                        endif
                    endif
                    call jeveuo(jexnum(nomfct//'.VALE', k), 'L', jrpv)
                    if (zr(jrpv) .le. 0.d0) then
                        vali = k
                        valkk (1) = nomcle(ii)
                        valkk (2) = nomfct
                        valrr (1) = zr(jrpv)
                        call utmess('F', 'MODELISA9_63', nk=2, valk=valkk, si=vali,&
                                    sr=valrr(1))
                    endif
                    if (zr(jrpv+nbpts/2) .le. 0.d0) then
                        vali = k
                        valkk (1) = nomcle(ii)
                        valkk (2) = nomfct
                        valrr (1) = zr(jrpv+nbpts/2)
                        call utmess('F', 'MODELISA9_64', nk=2, valk=valkk, si=vali,&
                                    sr=valrr(1))
                    endif

!                   verif abscisses croissantes (au sens large)
                    iret=2
                    call foverf(zr(jrpv), nbcoup, iret)
                    iret = 0
                    e1 = zr(jrpv+nbcoup) / zr(jrpv)
                    do 210 i = 1, nbcoup-1
                        ei = (&
                             zr(jrpv+nbcoup+i) - zr(jrpv+nbcoup+i-1) ) / ( zr(jrpv+i) - zr(jrpv+i&
                             &-1)&
                             )
                        if (ei .gt. e1) then
                            iret = iret + 1
                            valkk (1) = nomcle(ii)
                            valrr (1) = e1
                            valrr (2) = ei
                            valrr (3) = zr(jrpv+i)
                            call utmess('E', 'MODELISA9_65', sk=valkk(1), nr=3, valr=valrr)
                        endif
210                  continue
                    if (iret .ne. 0) then
                        call utmess('F', 'MODELISA6_73')
                    endif
160              continue
!
            else
                call utmess('F', 'MODELISA6_76')
            endif
149      continue
!
        rdep = nommat//'.&&RDEP'
        call wkvect(rdep//'.PROL', 'G V K24', 6, jprol)
        zk24(jprol ) = 'FONCTION'
        zk24(jprol+1) = 'LIN LIN '
        zk24(jprol+2) = 'EPSI    '
        zk24(jprol+3) = prol(4)
        call wkvect(rdep//'.VALE', 'G V R', 2*nbmax, jvale)
    endif


!   -- 8. Creation si necessaire d'une fonction pour stocker beta
!         (enthalpie volumique) calculee a partir de RHO_CP
!   ---------------------------------------------------------------
    if (nomrc(1:8) .eq. 'THER_NL') then
        do 650 i = 1, nbk
            if (( valk(nbr+nbc+i)(1:4) .eq. 'BETA' )) then
                nomfct = valk(nbr+nbc+nbk+i)
!               -- il n'y a rien a faire, on travaille directement avec beta
                goto 651
            endif
650      continue
        do 660 i = 1, nbk
            if (( valk(nbr+nbc+i)(1:6) .eq. 'RHO_CP' )) then
                nomfct = valk(nbr+nbc+nbk+i)
                goto 661
            endif
660      continue
        goto 651
661      continue
        call gcncon('_', nomint)
        call focain('TRAPEZE', nomfct, 0.d0, nomint, 'G')
!
!       -- si prolongement constant pour rho_cp : on affecte prol lineaire a beta
!
        prol1 = nomfct//'.PROL'
        call jeveuo(prol1, 'L', lpro1)
        prol2 = nomint//'.PROL'
        ASSERT(lxlgut(nomint).le.24)
        call jeveuo(prol2, 'E', lpro2)
        if (zk24(lpro1+4)(1:1) .eq. 'C') zk24(lpro2+4)(1:1)='L'
        if (zk24(lpro1+4)(2:2) .eq. 'C') zk24(lpro2+4)(2:2)='L'
!
        do 670 i = nbk, 1, -1
            valk(nbr+nbc+nbk+i+1) = valk(nbr+nbc+nbk+i)
670     continue
        nbk = nbk + 1
        valk(nbr+nbc+ nbk) = 'BETA    '
        valk(nbr+nbc+2*nbk) = nomint(1:16)
651     continue
    endif


!   -- 9. Verification des noms des parametres des tables TRC :
!   -----------------------------------------------------------
    if (nomrc(1:10) .eq. 'META_ACIER') then
        do 720 i = 1, nbk
            if (valk(nbr+nbc+i)(1:3) .eq. 'TRC') then
                call getvid(nomrc, 'TRC', iocc=1, scal=table, nbret=n)
                call tbexp2(table, 'VITESSE')
                call tbexp2(table, 'PARA_EQ')
                call tbexp2(table, 'COEF_0')
                call tbexp2(table, 'COEF_1')
                call tbexp2(table, 'COEF_2')
                call tbexp2(table, 'COEF_3')
                call tbexp2(table, 'COEF_4')
                call tbexp2(table, 'COEF_5')
                call tbexp2(table, 'NB_POINT')
                call tbexp2(table, 'Z1')
                call tbexp2(table, 'Z2')
                call tbexp2(table, 'Z3')
                call tbexp2(table, 'TEMP')
                call tbexp2(table, 'SEUIL')
                call tbexp2(table, 'AKM')
                call tbexp2(table, 'BKM')
                call tbexp2(table, 'TPLM')
                call tbexp2(table, 'DREF')
                call tbexp2(table, 'A')
            endif
720      continue
    endif

    AS_DEALLOCATE(vk8=typobj)
    AS_DEALLOCATE(vk32=nomobj)

    call jedema()
end subroutine
