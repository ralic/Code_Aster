subroutine aceama(nomu, noma, lmax, nbocc)
    implicit none
#include "jeveux.h"
#include "asterc/getvr8.h"
#include "asterfort/alcart.h"
#include "asterfort/eulnau.h"
#include "asterfort/getvem.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nocart.h"
#include "asterfort/wkvect.h"
    integer :: lmax, nbocc
    character(len=8) :: nomu, noma
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
!     AFFE_CARA_ELEM
!     AFFECTATION DES CARACTERISTIQUES POUR L'ELEMENT COQUE
! ----------------------------------------------------------------------
! IN  : NOMU   : NOM UTILISATEUR DE LA COMMANDE
! IN  : NOMA   : NOM DU MAILLAGE
! IN  : LMAX   : LONGUEUR
! IN  : NBOCC  : NOMBRE D'OCCURENCES DU MOT CLE COQUE
! ----------------------------------------------------------------------
    real(kind=8) :: ang(3), orig(3), angeul(3)
    character(len=19) :: cartma
    character(len=24) :: tmpnma, tmpvma
    integer :: iarg
!     ------------------------------------------------------------------
!
! --- CONSTRUCTION DES CARTES ET ALLOCATION
!-----------------------------------------------------------------------
    integer :: i, ioc, jdcc, jdls, jdvc, naxe, neul
    integer :: ng, nm, norig, nrep, jdls2
!-----------------------------------------------------------------------
    call jemarq()
    cartma = nomu//'.CARMASSI'
    tmpnma = cartma//'.NCMP'
    tmpvma = cartma//'.VALV'
!
    call alcart('G', cartma, noma, 'CAMASS')
    call jeveuo(tmpnma, 'E', jdcc)
    call jeveuo(tmpvma, 'E', jdvc)
!
    call wkvect('&&TMPMASSIF', 'V V K24', lmax, jdls)
    call wkvect('&&TMPMASSIF2', 'V V K8', lmax, jdls2)
!
!     STOCKAGE DE VALEURS NULLES SUR TOUT LE MAILLAGE
!
    zk8(jdcc) = 'C'
    zk8(jdcc+1) = 'ALPHA'
    zk8(jdcc+2) = 'BETA'
    zk8(jdcc+3) = 'KAPPA'
    zk8(jdcc+4) = 'X'
    zk8(jdcc+5) = 'Y'
    zk8(jdcc+6) = 'Z'
!
    zr(jdvc ) = 1.d0
    zr(jdvc+1) = 0.d0
    zr(jdvc+2) = 0.d0
    zr(jdvc+3) = 0.d0
    zr(jdvc+4) = 0.d0
    zr(jdvc+5) = 0.d0
    zr(jdvc+6) = 0.d0
!
    call nocart(cartma, 1, ' ', 'NOM', 0,&
                ' ', 0, ' ', 7)
!
! --- LECTURE DES VALEURS ET AFFECTATION DANS LA CARTE CARTMA
    do 10 ioc = 1, nbocc
        ang(1) = 0.d0
        ang(2) = 0.d0
        ang(3) = 0.d0
        orig(1) = 0.d0
        orig(2) = 0.d0
        orig(3) = 0.d0
        call getvem(noma, 'GROUP_MA', 'MASSIF', 'GROUP_MA', ioc,&
                    iarg, lmax, zk24(jdls), ng)
        call getvem(noma, 'MAILLE', 'MASSIF', 'MAILLE', ioc,&
                    iarg, lmax, zk8(jdls2), nm)
        call getvr8('MASSIF', 'ANGL_REP', ioc, iarg, 3,&
                    ang(1), nrep)
        call getvr8('MASSIF', 'ANGL_EULER', ioc, iarg, 3,&
                    angeul(1), neul)
        call getvr8('MASSIF', 'ANGL_AXE', ioc, iarg, 2,&
                    ang(1), naxe)
        call getvr8('MASSIF', 'ORIG_AXE', ioc, iarg, 3,&
                    orig(1), norig)
!
        if (nrep .ne. 0) then
            zr(jdvc ) = 1.d0
            zr(jdvc+1) = ang(1)
            zr(jdvc+2) = ang(2)
            zr(jdvc+3) = ang(3)
            zr(jdvc+4) = 0.d0
            zr(jdvc+5) = 0.d0
            zr(jdvc+6) = 0.d0
        else if (neul.ne.0) then
            call eulnau(angeul, ang)
            zr(jdvc ) = 2.d0
            zr(jdvc+1) = ang(1)
            zr(jdvc+2) = ang(2)
            zr(jdvc+3) = ang(3)
            zr(jdvc+4) = angeul(1)
            zr(jdvc+5) = angeul(2)
            zr(jdvc+6) = angeul(3)
        else
            zr(jdvc ) = -1.d0
            zr(jdvc+1) = ang(1)
            zr(jdvc+2) = ang(2)
            zr(jdvc+3) = ang(2)
            zr(jdvc+4) = orig(1)
            zr(jdvc+5) = orig(2)
            zr(jdvc+6) = orig(3)
        endif
!
! ---    "GROUP_MA" = TOUTES LES MAILLES DE LA LISTE DE GROUPES MAILLES
        if (ng .gt. 0) then
            do 20 i = 1, ng
                call nocart(cartma, 2, zk24(jdls+i-1), ' ', 0,&
                            ' ', 0, ' ', 7)
20          continue
        endif
!
! ---    "MAILLE" = TOUTES LES MAILLES DE LA LISTE DE MAILLES
!
        if (nm .gt. 0) then
            call nocart(cartma, 3, ' ', 'NOM', nm,&
                        zk8(jdls2), 0, ' ', 7)
        endif
!
10  end do
!
    call jedetr('&&TMPMASSIF')
    call jedetr('&&TMPMASSIF2')
    call jedetr(tmpnma)
    call jedetr(tmpvma)
!
    call jedema()
end subroutine
