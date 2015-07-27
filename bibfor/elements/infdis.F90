subroutine infdis(quest, ivale, rvale, kvale)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! --------------------------------------------------------------------------------------------------
!
!              Interroge la carte des 'cinfdi' des discrets
!              Informations annexes sur les discrets
!
!              Tous les discrets sont concernés
!
! --------------------------------------------------------------------------------------------------
!
!  IN
!     QUEST : INFORMATION QUE L'ON SOUHAITE RECUPERER
!        RECUPERE DES INFORMATIONS STOCKEES DANS LA CARTE
!           =  REP[K|M|A]  : REPERE
!           =  SYM[K|M|A]  : SYMETRIQUE
!           =  DIS[K|M|A]  : TYPE DE MATRICE AFFECTEE AU DISCRET
!           =  ETAK        : COEFFICIENT AMORTISSEMENT HYSTERETIQUE
!           =  TYDI        : TYPE DU DISCRET
!        POUR FACILITER LA VIE
!           =  SKMA        : TOUTES LES MATRICES SONT-ELLES SYMETRIQUES
!        INFORMATIONS ANNEXES SUR LES DISCRETS
!           =  DIMC        : TAILLE DE LA CARTE
!           =  DMXM        : TAILLE MAXI DES MATRICES D'UN DISCRET
!           =  CODE        : LE CODE DU DISCRET A PARTIR DE KVALE
!           =  INIT        : VALEUR INITIALE DE KVALE
!     KVALE : SI QUEST=CODE, DOIT CONTENIR LE NOM DU DISCRET
!             SI QUEST=INIT, DOIT CONTENIR LE PARAMETRE A INITIALISER
!
!  OUT
!     IVALE : SI REP[K|M|A] : REPERE GLOBAL(=1) OU LOCAL(=2)
!           : SI SYM[K|M|A] : MATRICE SYMETRIQUE(=1), NON-SYSMETRE(=2)
!           : SI DIS[K|M|A] : MATRICE AFFECTEE(=1), NON AFFECTEE(=0)
!           : SI SKMA : TOUTES LES MATRICES SONT SYMETRIQUE=3 SINON >3
!           : SI TYDI : LE CODE DU DISCRET STOKE DANS LA CARTE
!           : SI CODE : LE CODE ENTIER DU DISCRET
!     RVALE : SI ETAK : COEFFICIENT AMORTISSEMENT HYSTERETIQUE
!
! --------------------------------------------------------------------------------------------------
! person_in_charge: jean-luc.flejou at edf.fr
!
implicit none
!
    character(len=4) :: quest
    character(len=*) :: kvale
    integer :: ivale
    real(kind=8) :: rvale
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jevech.h"
#include "asterfort/jeveuo.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
!
! --------------------------------------------------------------------------------------------------
    integer :: nbelem, ii, jdc, jj, kk, iadzi, iazk24, icoord
    parameter  (nbelem=8)
    integer :: lenmnd(nbelem), lenmdd(nbelem)
    character(len=13) :: elemnd(nbelem), elemdd(nbelem)
    character(len=20) :: caracz
! --------------------------------------------------------------------------------------------------
    character(len=8) :: nommai, mailla
    integer :: nbnoeu
! --------------------------------------------------------------------------------------------------
    data elemnd /'_DIS_T_N     ', '_DIS_TR_N    ', '_DIS_T_L     ', '_DIS_TR_L    ',&
                 '2D_DIS_T_N   ', '2D_DIS_TR_N  ', '2D_DIS_T_L   ', '2D_DIS_TR_L  ' /
    data lenmnd /8, 9, 8, 9,10,11,10,11/
!
    data elemdd /'_DIS_T_D_N   ', '_DIS_TR_D_N  ', '_DIS_T_D_L   ', '_DIS_TR_D_L  ', &
                 '2D_DIS_T_D_N ', '2D_DIS_TR_D_N', '2D_DIS_T_D_L ', '2D_DIS_TR_D_L'/
    data lenmdd /10,11,10,11,12,13,12,13/
! --------------------------------------------------------------------------------------------------
!     Ordre de stockage dans la carte : CINFDI
!     0     1     2     3     4     5     6     7     8     9     10
!     REPK  REPM  REPA  SYMK  SYMM  SYMA  DISK  DISM  DISA  ETAK  TYDI
! --------------------------------------------------------------------------------------------------
    caracz = ' '
    if (quest .eq. 'DIMC') then
        ivale = 11
        rvale = 11.0d0
        goto 999
    else if (quest .eq. 'DMXM') then
        ivale = 144
        rvale = 144.0d0
        goto 999
    else if (quest .eq. 'DUMP') then
        call tecael(iadzi, iazk24)
        nommai = zk24(iazk24-1+3)(1:8)
        nbnoeu = zi(iadzi+1)
        call utmess(kvale(1:1)//'+', 'DISCRETS_30', sk=nommai, si=nbnoeu)
        mailla = zk24(iazk24)(1:8)
        call jeveuo(mailla//'.COORDO    .VALE', 'L', icoord)
        do jj = 1, nbnoeu
            ii = zi(iadzi+1+jj)
            if (jj .eq. nbnoeu) then
                call utmess(kvale(1:1), 'DISCRETS_31', sk=zk24(iazk24-1+ 3+jj), nr=3,&
                            valr=zr(icoord+3*(ii-1)))
            else
                call utmess(kvale(1:1)//'+', 'DISCRETS_31', sk=zk24( iazk24-1+3+jj), nr=3,&
                            valr=zr(icoord+3*(ii-1)))
            endif
        enddo
        goto 999
    else if (quest .eq. 'CODE') then
        caracz = kvale
        kk=len( caracz )
        do ii = kk, 1, -1
            if (caracz(ii:ii) .ne. ' ') then
                kk=ii
                goto 995
            endif
        enddo
        ASSERT(.false.)
995     continue
        ivale = 0
        rvale = 0.0d0
        do ii = 1, nbelem
            jj=lenmnd(ii)
            if (kk .ge. jj) then
                if (caracz(kk-jj+1:kk) .eq. elemnd(ii)) then
                    ivale = ii
                    rvale = ivale
                    goto 999
                endif
            endif
        enddo
        do ii = 1, nbelem
            jj=lenmdd(ii)
            if (kk .ge. jj) then
                if (caracz(kk-jj+1:kk) .eq. elemdd(ii)) then
                    ivale = ii
                    rvale = ivale
                    goto 999
                endif
            endif
        enddo
        ASSERT(ivale.ne.0)
    else if (quest .eq. 'INIT') then
        caracz = kvale
        if (caracz(1:3) .eq. 'REP') then
            ivale = 1
        else if (caracz(1:3) .eq. 'SYM') then
            ivale = 1
        else if (caracz(1:3) .eq. 'DIS') then
            ivale = 0
        else if (caracz .eq. 'ETAK') then
            ivale = 0
        else if (caracz .eq. 'TYDI') then
            ivale = 0
        else
            ASSERT(.false.)
        endif
        rvale = ivale
        goto 999
    endif
!
    rvale = 0.0d0
    ivale = 0
    call jevech('PCINFDI', 'L', jdc)
    if (quest .eq. 'REPK') then
        rvale = zr(jdc)
    else if (quest .eq. 'REPM') then
        rvale = zr(jdc+1)
    else if (quest .eq. 'REPA') then
        rvale = zr(jdc+2)
!
    else if (quest .eq. 'SYMK') then
        rvale = zr(jdc+3)
    else if (quest .eq. 'SYMM') then
        rvale = zr(jdc+4)
    else if (quest .eq. 'SYMA') then
        rvale = zr(jdc+5)
!
    else if (quest .eq. 'DISK') then
        rvale = zr(jdc+6)
    else if (quest .eq. 'DISM') then
        rvale = zr(jdc+7)
    else if (quest .eq. 'DISA') then
        rvale = zr(jdc+8)
!
    else if (quest .eq. 'ETAK') then
        rvale = zr(jdc+9)
        goto 999
!
    else if (quest .eq. 'TYDI') then
        rvale = zr(jdc+10)
!
    else if (quest .eq. 'SKMA') then
        rvale = zr(jdc+3)+zr(jdc+4)+zr(jdc+5)
    else
        ASSERT(.false.)
    endif
!
    ivale = nint(rvale)
999 continue
end subroutine
