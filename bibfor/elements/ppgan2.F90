subroutine ppgan2(jgano, nbsp, ncmp, vpg, vno)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
!
    integer :: jgano, nbsp, ncmp
    real(kind=8) :: vno(*), vpg(*)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
!
!     PASSAGE DES VALEURS POINTS DE GAUSS -> VALEURS AUX NOEUDS
!     POUR LES TYPE_ELEM AYANT 1 ELREFA
! ----------------------------------------------------------------------
!     IN     JGANO  ADRESSE DANS ZR DE LA MATRICE DE PASSAGE
!            NBSP   NOMBRE DE SOUS-POINTS
!            NCMP   NOMBRE DE COMPOSANTES
!            VPG    VECTEUR DES VALEURS AUX POINTS DE GAUSS
!     OUT    VNO    VECTEUR DES VALEURS AUX NOEUDS
!----------------------------------------------------------------------
    integer :: ino, isp, ipg, icmp, nno, nno2, iadzi, iazk24, npg, jmat, ima
    integer :: iatyma
    integer :: vali(2)
    real(kind=8) :: s
    character(len=8) :: ma, typema
    character(len=24) :: valk(2)
!
! DEB ------------------------------------------------------------------
!
    nno = nint(zr(jgano-1+1))
    npg = nint(zr(jgano-1+2))
    ASSERT(nno*npg.gt.0)
!
    call tecael(iadzi, iazk24)
    nno2 = zi(iadzi+1)
    if (nno2 .lt. nno) then
!       -- POUR CERTAINS ELEMENTS XFEM, IL EST NORMAL QUE NNO < NNO2 :
!          CE SONT DES ELEMENTS QUADRATIQUES QUI SE FONT PASSER POUR DES
!          ELEMENTS LINEAIRES
        ima = zi(iadzi)
        ma = zk24(iazk24)(1:8)
        call jeveuo(ma//'.TYPMAIL', 'L', iatyma)
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(iatyma-1+ima)), typema)
        valk (1) = zk24(iazk24-1+3)(1:8)
        valk (2) = typema
        vali (1) = nno2
        vali (2) = nno
        call utmess('F', 'ELEMENTS4_90', nk=2, valk=valk, ni=2,&
                    vali=vali)
    endif
!
! --- PASSAGE DES POINTS DE GAUSS AUX NOEUDS SOMMETS PAR MATRICE
!     V(NOEUD) = P * V(GAUSS)
!
    jmat = jgano + 2
    do 40 icmp = 1, ncmp
        do 30 ino = 1, nno
            do 20 isp = 1, nbsp
                s = 0.d0
                do 10 ipg = 1, npg
                    s = s + zr(jmat-1+(ino-1)*npg+ipg) * vpg((ipg-1)* ncmp*nbsp+(isp-1)*ncmp+icmp&
                        )
10              continue
                vno((ino-1)*ncmp*nbsp+(isp-1)*ncmp+icmp) = s
20          continue
30      continue
40  end do
!
end subroutine
