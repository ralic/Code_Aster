subroutine rcstoc(nommat, nomrc, nbobj, valr, valc,&
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
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    integer :: nbr, nbc, nbk, nbobj
    real(kind=8) :: valr(*)
    complex(kind=8) :: valc(*)
    character(len=8) :: nommat
    character(len=16) :: valk(*)
    character(len=32) :: nomrc
! ----------------------------------------------------------------------
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
! ----------------------------------------------------------------------
!     BUT: STOCKER DANS LES DEUX TABLEAUX VALR ET VALK LES REELS
!          ET LES K8 CARACTERISANT LA LOI DE COMPORTEMENT DE NOM NOMRC
!
!  IN  NOMMAT : NOM UTILISATEUR DU MATERIAU
!  IN  NOMRC  : NOM DE LA R.C.
!  IN  NBOBJ  : NOMBRE DE MCSIMPS
!  OUT VALR   : VECTEUR DES VALEURS REELLES
!  OUT VALK   : VECTEUR DES K8
!  OUT VALC   : VECTEUR DES COMPLEXES
!  OUT NBR    : NOMBRE DE REELS
!  OUT NBC    : NOMBRE DE COMPLEXES
!  OUT NBK    : NOMBRE DE CONCEPTS (FONCTION, TRC, TABLE, ... )
!
! ----------------------------------------------------------------------
!
!
!
    real(kind=8) :: valr8, e1, ei, precma, valrr(4)
    character(len=8) :: valtx
    character(len=8) :: valch, nomcle(5)
    character(len=8) :: table
    character(len=19) :: rdep, nomfct, nomint
    character(len=24) :: prol1, prol2, valkk(2)
    character(len=16) :: typeco
    complex(kind=8) :: valc8
    integer ::   ibk, nbmax, vali
    integer :: i, k, ii,  jrpv, jvale, nbcoup, n
    integer :: iret, nbfct, nbpts, jprol, nbptm, lpro1, lpro2
    character(len=32), pointer :: nomobj(:) => null()
    character(len=8), pointer :: typobj(:) => null()
    character(len=24), pointer :: prol(:) => null()
! ----------------------------------------------------------------------
!
    call jemarq()
    AS_ALLOCATE(vk8=typobj, size=nbobj)
    AS_ALLOCATE(vk32=nomobj, size=nbobj)
    call getmjm(nomrc, 1, nbobj, nomobj, typobj, n)
!
!
    nbr = 0
    nbc = 0
    nbk = 0
!
! --- 0- GLUT META_MECA*, BETON_DOUBLE_DP, RUPT_FRAG ET CZM_LAB_MIX :
! --- ON TRAITE LES TX QU ON CONVERTIT EN REELS
!
    do i = 1, nbobj
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
!
! --- 1- ON TRAITE LES REELS
!
    do i = 1, nbobj
        if (typobj(i)(1:3) .eq. 'R8 ') then
            call getvr8(nomrc, nomobj(i), iocc=1, scal=valr8, nbret=n)
            if (n .eq. 1) then
                nbr = nbr + 1
                valr(nbr) = valr8
                valk(nbr) = nomobj(i)(1:16)
            endif
        endif
    end do
!
!
! --- 2- ON TRAITE LES COMPLEXES
!
    do i = 1, nbobj
        if (typobj(i)(1:3) .eq. 'C8 ') then
            call getvc8(nomrc, nomobj(i), iocc=1, scal=valc8, nbret=n)
            if (n .eq. 1) then
                nbc = nbc + 1
                valc(nbr+nbc) = valc8
                valk(nbr+nbc) = nomobj(i)(1:16)
            endif
        endif
    end do
!
!
! --- 3- ON TRAITE ENSUITE LES CONCEPTS
!
    do i = 1, nbobj
        if (typobj(i)(1:3) .eq. 'CO ') then
            call getvid(nomrc, nomobj(i), iocc=1, scal=valch, nbret=n)
            if (n .eq. 1) then
                nbk = nbk + 1
                if (lxlgut(nomobj(i)) .gt. 16) then
                    call utmess('A','MODELISA9_84', sk=nomobj(i))
                endif   
                valk(nbr+nbc+nbk) = nomobj(i)(1:16)
            endif
        endif
    end do
!
    ibk = 0
    do i = 1, nbobj
        if (typobj(i)(1:3) .eq. 'CO ') then
            call getvid(nomrc, nomobj(i), iocc=1, scal=valch, nbret=n)
            if (n .eq. 1) then
                call gettco(valch, typeco)
                ibk = ibk + 1
                valk(nbr+nbc+nbk+ibk) = valch
            endif
        endif
   end do
!
! --- 4- CREATION D'UNE FONCTION POUR STOCKER R(P)
!
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
!         VERIF ABSCISSES CROISSANTES (AU SENS LARGE)
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
!
! --- 6 CREATION SI NECESSAIRE D'UNE FONCTION POUR STOCKER BETA
!       (ENTHALPIE VOLUMIQUE) CALCULEE A PARTIR DE RHO_CP
!
    if (nomrc(1:8) .eq. 'THER_NL') then
        do 650 i = 1, nbk
            if (( valk(nbr+nbc+i)(1:4) .eq. 'BETA' )) then
                nomfct = valk(nbr+nbc+nbk+i)
!
! IL N'Y A RIEN A FAIRE, ON TRAVAILLE DIRECTEMENT AVEC BETA
!
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
! SI PROLONGEMENT CONSTANT POUR RHO_CP : ON AFFECTE PROL LINEAIRE A BETA
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
!
! --- 7 VERIFICATION DES NOMS DES PARAMETRES DES TABLES
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
!
    AS_DEALLOCATE(vk8=typobj)
    AS_DEALLOCATE(vk32=nomobj)
! FIN ------------------------------------------------------------------
    call jedema()
end subroutine
