subroutine verif_loi_mater(mater)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! --------------------------------------------------------------------------------------------------
!
!               VÉRIFICATION D'INFORMATIONS DONNÉES POUR LES LDC
!
!   Lois traitées
!       - DIS_CONTACT
!
! --------------------------------------------------------------------------------------------------
!
    implicit none
    character(len=8) :: mater
!
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rccome.h"
#include "asterfort/utmess.h"
#include "asterfort/indk16.h"
!
! --------------------------------------------------------------------------------------------------
!
    integer :: cc, nbcrme, iret, nbr, nbk
!
    character(len=11) :: k11
    character(len=19) :: noobrc
    character(len=32) :: nomrc
!
    real(kind=8), pointer       :: matr(:) => null()
    character(len=16), pointer  :: matk(:) => null()
    character(len=32), pointer  :: vnomrc(:) => null()
!
    integer :: icoulomb, ia_nor, ia_tan
    logical :: alarme
!
! --------------------------------------------------------------------------------------------------
!   Nombre de relations de comportement
    call jelira(mater//'.MATERIAU.NOMRC', 'LONMAX', nbcrme)
!
!   Tableau des relations de comportement
    call jeveuo(mater//'.MATERIAU.NOMRC', 'L', vk32=vnomrc)
!
!   Boucle sur les matériaux
    do cc = 1, nbcrme
        nomrc = vnomrc(cc)
        if ( nomrc .eq. 'DIS_CONTACT' ) then
            call rccome(mater, nomrc, iret, iarret=1, k11_ind_nomrc=k11)
            noobrc = mater//k11
!           Récupération des noms et valeurs des paramètres
            call jelira(noobrc//'.VALR', 'LONUTI', nbr)
            call jelira(noobrc//'.VALK', 'LONUTI', nbk)
            ASSERT( nbr.eq.nbk )
            call jeveuo(noobrc//'.VALR', 'L', vr  =matr)
            call jeveuo(noobrc//'.VALK', 'L', vk16=matk)
!           Vérifications
!               Si COULOMB et (AMOR_NOR ou AMOR_TAN) différents de 0 ==> <A>
            icoulomb = indk16(matk,'COULOMB',1,nbk)
            if (icoulomb .ne. 0) then
                if ( abs(matr(icoulomb)).gt.r8prem() ) then
                    ia_nor = indk16(matk,'AMOR_NOR',1,nbk)
                    ia_tan = indk16(matk,'AMOR_TAN',1,nbk)
                    alarme = .False.
                    if (ia_nor.ne.0) then
                        if ( abs(matr(ia_nor)).gt.r8prem() ) alarme = .True.
                    endif
                    if (ia_tan.ne.0) then
                        if ( abs(matr(ia_tan)).gt.r8prem() ) alarme = .True.
                    endif
                    if ( alarme ) then
                        call utmess('A', 'DISCRETS_52')
                    endif
                endif
            endif
        endif
    enddo

end subroutine
