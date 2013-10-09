subroutine fonno7(noma, cnxinv, ndim, na, vecdir,&
                  hmax)
    implicit none
#include "jeveux.h"
#include "asterc/r8pi.h"
#include "asterc/r8prem.h"
#include "asterfort/conare.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "blas/ddot.h"
!
    integer :: na, ndim
    real(kind=8) :: vecdir(3), hmax
    character(len=8) :: noma
    character(len=19) :: cnxinv
!
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
!
!       ----------------------------------------------------------------
!       DETERMINATION DE LA TAILLE MAXIMALE DES MAILLES CONNECTEES AU
!       NOEUD DU FOND
!       ----------------------------------------------------------------
!    ENTREES
!       NOMA   : NOM DU MAILLAGE
!       CNXINV : CONNECTIVITE INVERSE
!       NDIM   : DIMENSION DU MODELE
!       NA     : NUMERO DU NOEUD SOMMET COURANT
!       VECDIR : VECTEUR TANGENT
!    SORTIE
!       HMAX  : TAILLE MAXIMALE DES MAILLES
!
!
    integer :: adra, ar(12, 3)
    integer :: iatyma, iar, ima, ino1, ino2, ityp
    integer :: jcncin, jconx1, jconx2, jcoor, jdrvlc, k
    integer :: nbar, nbmaca, ndime, nno, nno1, nno2, numac
    real(kind=8) :: coor(3), vect(3), p, cos70, cosinu, normv
    character(len=8) :: type
!     -----------------------------------------------------------------
!
    call jemarq()
!
!     RECUPERATION DES DONNEES SUR LE MAILLAGE
    call jeveuo(noma//'.CONNEX', 'L', jconx1)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', jconx2)
    call jeveuo(noma//'.TYPMAIL', 'L', iatyma)
    call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
!
!     RECUPERATION DES COORDONNNES DE NA
    coor(1) = zr(jcoor-1 + (na-1)*3 + 1)
    coor(2) = zr(jcoor-1 + (na-1)*3 + 2)
    coor(3) = zr(jcoor-1 + (na-1)*3 + 3)
!
!     RECUPERATION DE LA CONNECTIVITE INVERSE
    call jeveuo(jexatr(cnxinv, 'LONCUM'), 'L', jdrvlc)
    call jeveuo(jexnum(cnxinv, 1), 'L', jcncin)
!
!     MAILLES CONNECTEES A NA
    adra = zi(jdrvlc-1 + na)
    nbmaca = zi(jdrvlc-1 + na+1) - zi(jdrvlc-1 + na)
!
    hmax=r8prem()
!
    do ima = 1, nbmaca
!       NUMERO DE LA MAILLE
        numac = zi(jcncin-1 + adra+ima-1)
        ityp = iatyma-1+numac
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(ityp)), type)
        call dismoi('DIM_TOPO', type, 'TYPE_MAILLE', repi=ndime)
!
!       ON ZAPPE LES MAILLES DE BORDS
        if (ndime .ne. ndim) goto 10
!
        call conare(type, ar, nbar)
!
!       BOUCLE SUR LE NOMBRE D'ARETES DE LA MAILLE NUMAC
        do iar = 1, nbar
!
            ino1 = ar(iar,1)
            nno1 = zi(jconx1-1 + zi(jconx2+numac-1) +ino1-1)
            ino2 = ar(iar,2)
            nno2 = zi(jconx1-1 + zi(jconx2+numac-1) +ino2-1)
!
            if (na .eq. nno1) then
                nno = nno2
            else if (na.eq.nno2) then
                nno = nno1
            else
                goto 100
            endif
!
!          VECTEUR REPRESENTANT L'ARETE NA-NNO
            do k = 1, ndim
                vect(k) = zr(jcoor-1+ (nno-1)*3+k) - coor(k)
            end do
!
!          PROJECTION DE L'ARETE SUR LE VECTEUR TANGENT
            p = ddot(ndim,vect,1,vecdir,1)
!
!          FILTRAGE DES ARETES A PRENDRE EN COMPTE:
!          L'ANGLE ENTRE VECT ET VECDIR DOIT ETRE <60
            normv = sqrt(ddot(ndim,vect,1,vect,1))
!
            cosinu = p/normv
            cos70 = cos(70*r8pi()/180.d0)
            if (abs(cosinu) .lt. cos70) goto 100
!
!          ON PREND LE MAX DES PROJECTIONS
            p = abs(p)
            if (p .ge. hmax) hmax = p
!
100         continue
        end do
!
 10     continue
    end do
!
    if (hmax .le. r8prem()) then
        call utmess('A', 'RUPTURE0_49')
    endif
!
    call jedema()
end subroutine
