subroutine calyrg(iocc, ndim, noma, lnuno2, geom2,&
                  mrota, lrota)
    implicit none
!
#include "jeveux.h"
#include "asterc/getvr8.h"
#include "asterc/r8dgrd.h"
#include "asterfort/codent.h"
#include "asterfort/dismoi.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/matrot.h"
#include "asterfort/parotr.h"
#include "asterfort/u2mesk.h"
#include "asterfort/wkvect.h"
    integer :: iocc, ndim
    real(kind=8) :: mrota(3, 3)
    logical :: lrota
    character(len=8) :: noma
    character(len=*) :: lnuno2, geom2
!
!-----------------------------------------------------------------------
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
!
!     BUT : TRAITEMENT DES MOTS CLES LIAISON_MAIL :
!             TRAN/CENTRE/ANGL_NAUT
!
!     IN : IOCC (I) : NUMERO D'OCCURENCE DU MOT CLE FACTEUR
!     IN : NDIM (I) : DIMENSION DE L'ESPACE (2 OU 3)
!     IN : NOMA (K8): NOM DU MAILLAGE
!     IN/JXIN  : LNUNO2 (K*) : NOM D'UN OBJET QUI CONTIENT LA
!                               LISTE DES NUMEROS DES NOEUDS_2
!     IN/JXOUT : GEOM2 (K24) : NOM D'UN OBJET QUI CONTIENDRA LES
!            COORDONNEES DES NOEUDS DU GROUP_NO_2, TRANSFORMEES PAR
!            LA TRANSFORMATION GEOMETRIQUE DONNEE PAR L'UTILISATEUR
!     OUT : MROTA (R(3,3)) : MATRICE DE ROTATION DE LA TRANSFORMATION
!     OUT : LROTA (L) : .TRUE.  : IL EXISTE UNE ROTATION
!                       .FALSE. : IL N'EXISTE PAS DE ROTATION
!-----------------------------------------------------------------------
!
!
    integer :: ntran, nangl, ncentr, nangmx, iageom, jnuno2
    integer :: igeom2, nbno2, ino2, nuno2, nnomx, ier, k, kk
    real(kind=8) :: tran(3), angl(3), centr(3), coor2(3), zero, un
    character(len=16) :: motfac
    character(len=24) :: valk(2)
    character(len=1) :: kb
    integer :: iarg
!
! ----------------------------------------------------------------------
!
! --- INITIALISATIONS :
!     ---------------
    zero = 0.0d0
    un = 1.0d0
    motfac = 'LIAISON_CYCL'
    lrota = .false.
!
    do 10 k = 1, 3
!
        tran(k) = zero
        angl(k) = zero
        centr(k) = zero
!
        do 20 kk = 1, 3
            if (k .eq. kk) then
                mrota(k,k) = un
            else
                mrota(k,kk) = zero
                mrota(kk,k) = zero
            endif
20      continue
10  end do
!
! --- LECTURE DE L'ISOMETRIE DE TRANSFORMATION SI ELLE EXISTE :
!     -------------------------------------------------------
    call getvr8(motfac, 'TRAN', iocc, iarg, ndim,&
                tran, ntran)
    if (ntran .lt. 0) then
        call codent(ndim, 'G', kb)
        valk(1) = motfac
        valk(2) = kb
        call u2mesk('F', 'MODELISA3_14', 2, valk)
    endif
!
    if (ndim .eq. 3) then
        nangmx = 3
    else
        nangmx = 1
    endif
    call getvr8(motfac, 'ANGL_NAUT', iocc, iarg, nangmx,&
                angl, nangl)
    if (nangl .lt. 0) then
        call codent(nangmx, 'G', kb)
        valk(1) = motfac
        valk(2) = kb
        call u2mesk('F', 'MODELISA3_15', 2, valk)
    endif
    do 30 k = 1, 3
        angl(k) = angl(k)*r8dgrd()
30  end do
!
    call getvr8(motfac, 'CENTRE', iocc, iarg, ndim,&
                centr, ncentr)
    if (ncentr .lt. 0) then
        call codent(ndim, 'G', kb)
        valk(1) = motfac
        valk(2) = kb
        call u2mesk('F', 'MODELISA3_16', 2, valk)
    endif
!
! --- DETERMINATION DE LA MATRICE DE ROTATION DE LA TRANSFORMATION :
!     ------------------------------------------------------------
    if (nangl .ge. 1) then
        call matrot(angl, mrota)
        lrota = .true.
    endif
!
! --- DETERMINATION DES COORDONNEES TRANSFORMEES :
!     ------------------------------------------
    call dismoi('F', 'NB_NO_MAILLA', noma, 'MAILLAGE', nnomx,&
                kb, ier)
    call wkvect(geom2, 'V V R', 3*nnomx, igeom2)
    call jeveuo(noma//'.COORDO    .VALE', 'L', iageom)
!
    call jeveuo(lnuno2, 'L', jnuno2)
    call jelira(lnuno2, 'LONUTI', nbno2, kb)
!
    do 40 ino2 = 1, nbno2
        nuno2 = zi(jnuno2+ino2-1)
        call parotr(noma, iageom, nuno2, 0, centr,&
                    mrota, tran, coor2)
        do 50 k = 1, 3
            zr(igeom2+3*(nuno2-1)+k-1) = coor2(k)
50      continue
40  end do
!
end subroutine
