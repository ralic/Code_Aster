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
!       - DIS_CONTACT                responsable : jean-luc.flejou at edf.fr
!       - DIS_ECRO_TRAC              responsable : jean-luc.flejou at edf.fr
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
    integer :: cc, kk, nbcrme, iret, jvalk, nbr, nbc, nbk2, nbk
    integer             :: jprol, jvale, nbvale
    real(kind=8)        :: dx, fx, dfx, raidex
    logical             :: OkFct
    character(len=19)   :: nomfon
!
    character(len=11) :: k11
    character(len=19) :: noobrc
    character(len=24) :: chprol, chvale, chpara
    character(len=24) :: valk(2)
    character(len=32) :: nomrc
!
    real(kind=8), pointer       :: matr(:) => null()
    character(len=16), pointer  :: matk(:) => null()
    character(len=32), pointer :: vnomrc(:) => null()
!
    integer :: icoulomb, ia_nor, ia_tan
    logical :: alarme
!
    real(kind=8) :: precis
    parameter (precis=1.0e-08)
!
! --------------------------------------------------------------------------------------------------
!   Nombre de relations de comportement
    call jelira(mater//'.MATERIAU.NOMRC', 'LONMAX', nbcrme)
!
!   Tableau des relations de comportement
    call jeveuo(mater//'.MATERIAU.NOMRC', 'L', vk32=vnomrc)
!
!   Boucle sur les relations de comportement
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
        elseif ( nomrc .eq. 'DIS_ECRO_TRAC' ) then
            call rccome(mater, vnomrc(cc), iret, iarret=1, k11_ind_nomrc=k11)
            noobrc = mater//k11
!           Récupération des pointeurs, ainsi que du nombre de RCK
            call jeveuo(noobrc//'.VALK', 'L', jvalk)
            call jelira(noobrc//'.VALR', 'LONUTI', nbr)
            call jelira(noobrc//'.VALC', 'LONUTI', nbc)
            call jelira(noobrc//'.VALK', 'LONUTI', nbk2)
            nbk=(nbk2-nbr-nbc)/2
            ASSERT( nbr .eq. 0 )
            ASSERT( nbc .eq. 0 )
            ASSERT( nbk .eq. 1 )
            ASSERT( zk16(jvalk) .eq. 'FX' )
!           Quelques vérifications sur la fonction
!               interpolation LIN LIN
!               paramètre 'DX'
!               prolongée à gauche ou à droite exclue
!               avoir 2 points minimum
!               FX et DX sont positifs, point n°1:(DX=0, FX=0)
!               dFx >0 , dDx >0
            nomfon = zk16(jvalk+1)(1:8)
            chprol = nomfon//'.PROL'
            chvale = nomfon//'.VALE'
            chpara = nomfon//'.PARA'
!           Adresses
            call jeveuo(chprol, 'L', jprol)
            call jeveuo(chvale, 'L', jvale)
            call jelira(chvale, 'LONMAX', nbvale)
            nbvale = nbvale/2
!           vérifications sur les valeurs de la fonction
            dx = zr(jvale)
            fx = zr(jvale+nbvale)
            OkFct = (zk24(jprol)(1:8) .eq. 'FONCTION')
            OkFct = OkFct .and. (zk24(jprol+1)(1:3) .eq. 'LIN')
            OkFct = OkFct .and. (zk24(jprol+1)(5:7) .eq. 'LIN')
            OkFct = OkFct .and. (zk24(jprol+2)(1:2) .eq. 'DX')
            OkFct = OkFct .and. (zk24(jprol+4)(1:2) .eq. 'EE')
            OkFct = OkFct .and. (nbvale .ge. 3 )
            OkFct = OkFct .and. (dx .ge. 0.0d0 ) .and. (dx .le. precis)
            OkFct = OkFct .and. (fx .ge. 0.0d0 ) .and. (fx .le. precis)
            if ( OkFct ) then
                cik1: do kk = 1, nbvale-1
                    if ( ( zr(jvale+kk) .le. dx ) .or. &
                         ( zr(jvale+nbvale+kk) .le. fx ) ) then
                        OkFct = .false.
                        exit cik1
                    endif
                    if ( kk .eq. 1 ) then
                        raidex = (zr(jvale+nbvale+kk) - fx)/(zr(jvale+kk) - dx)
                    else
                        dfx = (zr(jvale+nbvale+kk) - fx)/(zr(jvale+kk) - dx)
                        if ( dfx .gt. raidex ) then
                            OkFct = .false.
                            exit cik1
                        endif
                    endif
                    dx = zr(jvale+kk)
                    fx = zr(jvale+nbvale+kk)
                enddo cik1
            endif
            if (.not. OkFct ) then
                valk(1) = 'DIS_ECRO_TRAC'
                valk(2) = 'FX=f(DX)'
                call utmess('F', 'DISCRETS_62', nk=2, valk=valk)
            endif
        endif
    enddo

end subroutine
