subroutine xpofon(modele, mftot, nftot, nfcomf, ngfon)
    implicit none
!
#include "jeveux.h"
!
#include "asterfort/dismoi.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/ltnotb.h"
#include "asterfort/u2mess.h"
    character(len=8) :: modele
    integer :: mftot, nftot, nfcomf, ngfon
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     BUT : RECUPERER POUR L'IMPRESSION LES CARACTERISTIQUES DES FONDS
!           DE FISSURE
!   IN
!       MODELE : MODELE FISSURE
!       MAXFEM : MAILLAGE FISSURE
!   OUT
!       MFTOT  : NOMBRE TOTAL DE MAILLES DE FONDS DE FISSURES
!       NFTOT  : NOMBRE TOTAL DE NOEUDS DE FONDS DE FISSURES
!       NFCOMF : NOMBRE TOTAL DE CONNEXIONS DANS LES MAILLES
!       NGFON  : NOMBRE TOTAL DE FOND DE FISSURES
!     =================================================================
!     ------------------------------------------------------------------
    integer :: jnom, jtail, jnfond, jbnfmu
    integer :: ibid, iret, ifiss, ifon
    integer :: ndim, nfiss, nfond, nbnol
    character(len=8) :: k8b, fiss, mo, malini
    character(len=19) :: nomta1, nomta2
    character(len=24) :: nom
!
!
!     INITIALISATIONS
    mftot = 0
    nftot = 0
    nfcomf = 0
    ngfon = 0
!
!     RECUPERATION DU NOMBRE DE FISSURES
    mo = modele
    nom = mo//'.FISS                   '
    call jeveuo(nom, 'L', jnom)
    call jelira(nom, 'LONUTI', nfiss, k8b)
!
!     RECUPERATION DES CARACTERISTIQUES DU MAILLAGE INITIAL
    call dismoi('F', 'NOM_MAILLA', mo, 'MODELE', ibid,&
                malini, iret)
    call dismoi('F', 'DIM_GEOM', mo, 'MODELE', ndim,&
                k8b, iret)
    if (.not.(ndim.eq.2.or.ndim.eq.3)) call u2mess('F', 'MODELISA2_6')
!
    if (ndim .eq. 2) then
!
        do 10 ifiss = 1, nfiss
            fiss = zk8(jnom)
            call jeexin(fiss//'.FONDFISS', iret)
            if (iret .ne. 0) then
                call ltnotb(fiss, 'NB_FOND_FISS', nomta1)
                call jeveuo(nomta1//'.0001', 'L', jnfond)
                nfond = zi(jnfond)
                ngfon = ngfon + nfond
                nftot = nftot + nfond
                mftot = mftot + nfond
                nfcomf = nfcomf + nfond
            endif
10      continue
!
    else if (ndim.eq.3) then
!
        do 11 ifiss = 1, nfiss
!
            fiss = zk8(jnom)
            call jeexin(fiss//'.FONDFISS', iret)
            if (iret .ne. 0) then
!
                call jeveuo(fiss//'.FONDMULT', 'L', jbnfmu)
!
                call ltnotb(fiss, 'NB_FOND_FISS', nomta1)
                call jeveuo(nomta1//'.0001', 'L', jnfond)
!
                call ltnotb(fiss, 'FOND_FISS', nomta2)
                call jeveuo(nomta2//'.TBNP', 'L', jtail)
!
                nfond = zi(jnfond)
                ngfon = ngfon + nfond
                do 20 ifon = 1, nfond
                    nbnol = zi(jbnfmu + 2*ifon -1) - zi(jbnfmu + 2* ifon -2)
                    if (nbnol .ne. 0) then
                        mftot = mftot + nbnol
                        nfcomf = nfcomf + 2*nbnol
                    else
!               SI MAILLE POI1 EN 3D
                        mftot = mftot + 1
                        nfcomf = nfcomf + 1
                    endif
20              continue
                nftot = nftot + zi(jtail + 1)
!
            endif
11      continue
    endif
!
end subroutine
