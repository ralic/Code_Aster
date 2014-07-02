subroutine numer2(nuposs, nbligr, vligr, moloc, solveu,&
                  base, nu, nequa)
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
! person_in_charge: jacques.pellet at edf.fr
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/detrsd.h"
#include "asterfort/idenob.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedupo.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nueffe.h"
#include "asterfort/nugllo.h"
#include "asterfort/promor.h"
#include "asterfort/wkvect.h"
    character(len=*) :: moloc, vligr(*), solveu, base, nu, nuposs
    integer :: nbligr
    integer :: nequa
! ----------------------------------------------------------------------
! BUT CREER UN NUME_DDL POUR UNE LISTE DE LIGRELS ET UNE GRANDEUR DONNEE
! ----------------------------------------------------------------------
! IN  K14  NUPOSS  : NOM D'UN NUME_DDL CANDIDAT (OU ' ')
!                    SI NUPOSS != ' ', ON  REGARDE SI LE PROF_CHNO
!                    DE NUPOSS EST CONVENABLE.
! IN      I    NBLIGR: NOMBRE DE LIGRELS DANS VLIGR
! IN/JXIN V(K19) VLIGR : LISTE DES NOMS DES LIGRELS
! IN      K8   MOLOC : MODE_LOCAL PERMETTANT DE CHOISIR LES DDLS
!                      A NUMEROTER.
!              SI MOLOC=' ', ON DEDUIT MOLOC DU PHENOMENE
!                      ATTACHE AUX LIGRELS (USAGE D'UN DISMOI TORDU)
!              SINON ON UTILISE LE MOLOC DONNE EN ARGUMENT
! IN/JXIN K19  SOLVEU: SOLVEUR
! IN      K2   BASE  : BASE(1:1) : BASE POUR CREER LE NUME_DDL
!                    (SAUF LE PROF_CHNO)
!                  : BASE(2:2) : BASE POUR CREER LE PROF_CHNO
! VAR/JXOUT K14 NU : NOM DU NUME_DDL.
!                    SI NUPOSS !=' ', NU PEUT ETRE MODIFIE (NU=NUPOSS)
! OUT  I NEQUA: NOMBRE D'EQUATIONS DU SOUS-DOMAINE (EXPLOITE QU'EN DD)
!   -------------------------------------------------------------------
!     ASTER INFORMATIONS:
!----------------------------------------------------------------------
!
!
    integer :: i, jlligr, jnslv
!
    aster_logical :: l1, l2, l3, l4
    character(len=19) :: solve2
    character(len=2) :: bas2
    character(len=14) :: nu1, nu2
    character(len=24) :: lligr, method
    character(len=24), pointer :: slvk(:) => null()
!
! DEB ------------------------------------------------------------------
!
    call jemarq()
    solve2 = solveu
    bas2 = base
    nu1=nu
    nu2=nuposs
!
    call detrsd('NUME_DDL', nu1)
!
    call jeveuo(solve2//'.SLVK', 'L', vk24=slvk)
    method = slvk(4)
!
!
    lligr = '&&NUMER2.LISTE_LIGREL'
    call wkvect(lligr, 'V V K24', nbligr, jlligr)
    do i = 1, nbligr
        zk24(jlligr-1+i) = vligr(i)
    end do
!
    call nueffe(lligr, bas2, nu1, method, moloc,&
                solve2, nequa)
!
    if (slvk(10)(1:3) .eq. 'OUI') then
        call nugllo(nu1, bas2, solve2)
    endif
!
!     -- ON ESSAYE D'ECONOMISER LE PROF_CHNO :
    if (nu2 .ne. ' ') then
!
        l1=idenob(nu1//'.NUME.DEEQ',nu2//'.NUME.DEEQ')
        l2=idenob(nu1//'.NUME.LILI',nu2//'.NUME.LILI')
        l3=idenob(nu1//'.NUME.NUEQ',nu2//'.NUME.NUEQ')
        l4=idenob(nu1//'.NUME.PRNO',nu2//'.NUME.PRNO')
!
        if (l1 .and. l2 .and. l3 .and. l4) then
            call detrsd('NUME_DDL', nu1)
            call jedupo(nu1//'     .ADNE', 'V', nu2//'     .ADNE', .false._1)
            call jedupo(nu1//'     .ADLI', 'V', nu2//'     .ADLI', .false._1)
            call jedetr(nu1//'     .ADLI')
            call jedetr(nu1//'     .ADNE')
            nu1=nu2
        endif
    endif
!
    call promor(nu1, bas2(1:1))
    call jedetr(nu1//'     .ADLI')
    call jedetr(nu1//'     .ADNE')
!
    call jedetr(lligr)
!
!
! --- CREATION DE L'OBJET .NSLV :
!     -------------------------------------
    call jedetr(nu1//'.NSLV')
    call wkvect(nu1//'.NSLV', bas2(1:1)//' V K24', 1, jnslv)
    zk24(jnslv-1+1)=solve2
!
!     CALL CHEKSD('sd_nume_ddl',NU1,IRET)
!
    nu=nu1
    call jedema()
end subroutine
