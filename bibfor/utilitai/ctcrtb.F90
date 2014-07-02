subroutine ctcrtb(nomtb, tych, resu, nkcha, typac,&
                  toucmp, nbcmp, nbval, nkcmp, ndim)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/carces.h"
#include "asterfort/celces.h"
#include "asterfort/cnocns.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/tbcrsv.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    integer :: nbcmp, ndim, nbval
    character(len=4) :: tych
    character(len=8) :: nomtb, typac, resu
    character(len=24) :: nkcha, nkcmp
    aster_logical :: toucmp
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ----- OPERATEUR CREA_TABLE , MOT-CLE FACTEUR RESU   --------------
!
!        BUT : CREATION DE LA TABLE
!
!        IN     : TYCH   (K4)  : TYPE DE CHAMP (=NOEU,ELNO,ELGA)
!                 NKCHA (K24)  : OBJET DES NOMS DE CHAMP
!                 RESU  (K8)   : NOM DU RESULTAT (SI RESULTAT,SINON ' ')
!                 NKCMP  (K24) : OBJET DES NOMS DE COMPOSANTES
!                 TOUCMP (L)   : INDIQUE SI TOUT_CMP EST RENSEIGNE
!                 NBCMP (I)    : NOMBRE DE COMPOSANTES LORSQUE
!                                NOM_CMP EST RENSEIGNE, 0 SINON
!                 TYPAC (K8)   : ACCES (ORDRE,MODE,FREQ,INST)
!                 NBVAL (I)    : NOMBRE DE VALEURS D'ACCES
!                 NDIM  (I)    : DIMENSION GEOMETRIQUE
!        IN/OUT : NOMTB (K24)  : OBJET TABLE
!
! ----------------------------------------------------------------------
!
    integer :: nbpara, n, jkcha, jcesd, jcesc
    integer :: kk, i, j, jcmp, iret
    character(len=19) :: chamns, chames
    character(len=16), pointer :: table_parak(:) => null()
    character(len=8), pointer :: table_typek(:) => null()
    integer, pointer :: cnsd(:) => null()
    character(len=8), pointer :: cnsc(:) => null()
!     ------------------------------------------------------------------
!
    call jemarq()
    ASSERT(tych(1:2).eq.'EL'.or.tych.eq.'CART'.or.tych.eq.'NOEU')
!
! --- 0. INITIALISATION
!     -----------------
    chamns='&&CTCRTB.CHAM_NO_S'
    chames='&&CTCRTB.CHAM_EL_S'
    call jeveuo(nkcmp, 'L', jcmp)
!
!
!     ----------------------------------------------------
! --- 1. DETERMINATION DU NOMBRE DE PARAMETRES DE LA TABLE
!     ----------------------------------------------------
    kk=0
    if (resu .eq. ' ') then
        kk=kk+1
    else
        kk=kk+2
    endif
!
    if (resu .ne. ' ') then
        if (typac .ne. 'ORDRE') then
            kk=kk+1
        endif
        kk=kk+1
    endif
!
    if (tych .eq. 'NOEU') then
!        -- NOEUD
        kk=kk+1
    endif
!
    if (tych .eq. 'ELNO') then
!        -- MAILLE + NOEUD + SOUS_POINT
        kk=kk+3
    endif
!
    if (tych .eq. 'ELGA') then
!        -- MAILLE + POINT + SOUS_POINT
        kk=kk+3
    endif
!
    if (tych .eq. 'ELEM') then
!        -- MAILLE + SOUS_POINT
        kk=kk+2
    endif
!
    if (tych .eq. 'CART') then
!        -- MAILLE
        kk=kk+1
    endif
!
!     -- COOR_X, ...
    kk=kk+1
    if (ndim .ge. 2) then
        kk=kk+1
    endif
    if (ndim .eq. 3) then
        kk=kk+1
    endif
!
!     -- CMPS :
    n=nbcmp
    call jeveuo(nkcha, 'L', jkcha)
!     -- JE NE COMPRENDS PAS LA BOUCLE I=1,NBVAL (J. PELLET)
    do 60 i = 1, nbval
        if (zk24(jkcha+i-1)(1:18) .ne. '&&CHAMP_INEXISTANT') then
            if (toucmp) then
                if (tych .eq. 'NOEU') then
                    call cnocns(zk24(jkcha+i-1), 'V', chamns)
                    call jeveuo(chamns//'.CNSD', 'L', vi=cnsd)
                    call jeveuo(chamns//'.CNSC', 'L', vk8=cnsc)
                    n=cnsd(2)
                else if (tych(1:2).eq.'EL') then
                    call celces(zk24(jkcha+i-1), 'V', chames)
                    call jeveuo(chames//'.CESD', 'L', jcesd)
                    call jeveuo(chames//'.CESC', 'L', jcesc)
                    n=zi(jcesd+1)
                else if (tych.eq.'CART') then
                    call carces(zk24(jkcha+i-1), 'ELEM', ' ', 'V', chames,&
                                ' ', iret)
                    ASSERT(iret.eq.0)
                    call jeveuo(chames//'.CESD', 'L', jcesd)
                    call jeveuo(chames//'.CESC', 'L', jcesc)
                    n=zi(jcesd+1)
                else
                    ASSERT(.false.)
                endif
            endif
        endif
 60 end do
    kk=kk+n
!
    nbpara=kk
!
!
!    ------------------------------------------------------------------
! --- 2. DETERMINATION DES NOMS ET DES TYPES DES PARAMETRES DE LA TABLE
!        DE LA TABLE
!     ------------------------------------------------------------------
    AS_ALLOCATE(vk16=table_parak, size=nbpara)
    AS_ALLOCATE(vk8=table_typek, size=nbpara)
!
    kk=0
    if (resu .eq. ' ') then
        table_parak(kk+1)='CHAM_GD'
        table_typek(kk+1)='K8'
        kk=kk+1
    else
        table_parak(kk+1)='RESULTAT'
        table_typek(kk+1)='K8'
        kk=kk+1
        table_parak(kk+1)='NOM_CHAM'
        table_typek(kk+1)='K16'
        kk=kk+1
    endif
!
    if (resu .ne. ' ') then
        if (typac .ne. 'ORDRE') then
            table_parak(kk+1)=typac
            table_typek(kk+1)='R'
            if (typac .eq. 'MODE') table_typek(kk+1)='I'
            kk=kk+1
        endif
        table_parak(kk+1)='NUME_ORDRE'
        table_typek(kk+1)='I'
        kk=kk+1
    endif
!
    if (tych(1:2) .eq. 'EL' .or. tych .eq. 'CART') then
        table_parak(kk+1)='MAILLE'
        table_typek(kk+1)='K8'
        kk=kk+1
    endif
    if (tych .eq. 'ELNO' .or. tych .eq. 'NOEU') then
        table_parak(kk+1)='NOEUD'
        table_typek(kk+1)='K8'
        kk=kk+1
    else if (tych.eq.'ELGA') then
        table_parak(kk+1)='POINT'
        table_typek(kk+1)='I'
        kk=kk+1
    endif
    if (tych(1:2) .eq. 'EL') then
!        -- TOUS LES CHAMPS ELXX PEUVENT AVOIR DES SOUS_POINT :
        table_parak(kk+1)='SOUS_POINT'
        table_typek(kk+1)='I'
        kk=kk+1
    endif
!
    table_parak(kk+1)='COOR_X'
    table_typek(kk+1)='R'
    kk=kk+1
    if (ndim .ge. 2) then
        table_parak(kk+1)='COOR_Y'
        table_typek(kk+1)='R'
        kk=kk+1
    endif
    if (ndim .eq. 3) then
        table_parak(kk+1)='COOR_Z'
        table_typek(kk+1)='R'
        kk=kk+1
    endif
    if (toucmp) then
        if (tych .eq. 'NOEU') then
            do 90 j = 1, n
                table_parak(kk+1)=cnsc(j)
                table_typek(kk+1)='R'
                kk=kk+1
 90         continue
        else if (tych(1:2).eq.'EL'.or.tych.eq.'CART') then
            do 91 j = 1, n
                table_parak(kk+1)=zk8(jcesc+j-1)
                table_typek(kk+1)='R'
!
                kk=kk+1
 91         continue
        endif
    else
        do 95 j = 1, n
            table_parak(kk+1)=zk8(jcmp+j-1)
            table_typek(kk+1)='R'
            kk=kk+1
 95     continue
    endif
!
!    ------------------------------------------------------------------
! --- 3. CREATION DE LA TABLE
!     ------------------------------------------------------------------
    call tbcrsv(nomtb, 'G', nbpara, table_parak, table_typek,&
                0)
!
!
    AS_DEALLOCATE(vk16=table_parak)
    AS_DEALLOCATE(vk8=table_typek)
!
    call jedema()
!
end subroutine
