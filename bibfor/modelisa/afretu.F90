subroutine afretu(iprno, lonlis, klisno, noepou, noma,&
                  vale1, nbcoef, idec, coef, nomddl,&
                  typlag, lisrel)
    implicit none
#include "jeveux.h"
#include "asterfort/afrela.h"
#include "asterfort/dismoi.h"
#include "asterfort/imprel.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    integer :: lonlis, iprno(*), idec, nbcoef
    real(kind=8) :: coef(nbcoef)
    character(len=2) :: typlag
    character(len=8) :: klisno(lonlis), noepou, noma, nomddl(nbcoef)
    character(len=24) :: vale1
    character(len=19) :: lisrel
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! -------------------------------------------------------
!     RACCORD (COQUE OU 3D)_TUYAU : UNE RELATION LINEAIRE
!
    integer :: ino, ival, idch1, nbterm, i, nbec

    character(len=8) :: betaf
    character(len=16) :: motfac
    character(len=24) :: noeuma
    real(kind=8) :: beta
    complex(kind=8) :: betac
    complex(kind=8), pointer :: coec(:) => null()
    real(kind=8), pointer :: coer(:) => null()
    integer, pointer :: dime(:) => null()
    real(kind=8), pointer :: direct(:) => null()
    character(len=8), pointer :: lisddl(:) => null()
    character(len=8), pointer :: lisno(:) => null()
!
    call jemarq()
!
    motfac = 'LIAISON_ELEM'
    betaf = '&FOZERO'
    beta = 0.0d0
    betac = (0.0d0,0.0d0)
    noeuma = noma//'.NOMNOE'
    motfac = 'LIAISON_ELEM'
    call jeveuo(vale1, 'L', idch1)
    call dismoi('NB_EC', 'DEPL_R', 'GRANDEUR', repi=nbec)
    if (nbec .gt. 10) then
        call utmess('F', 'MODELISA_94')
    endif
!
! --- CREATION DES TABLEAUX DE TRAVAIL NECESSAIRES A L'AFFECTATION
! --- NBTERM : MAJORANT DU NOMBRE DE TERMES DANS UNE RELATION
!
    nbterm = 3*lonlis + nbcoef
! ---     VECTEUR DU NOM DES NOEUDS
    AS_ALLOCATE(vk8=lisno, size=nbterm)
! ---     VECTEUR DU NOM DES DDLS
    AS_ALLOCATE(vk8=lisddl, size=nbterm)
! ---     VECTEUR DES COEFFICIENTS REELS
    AS_ALLOCATE(vr=coer, size=nbterm)
! ---     VECTEUR DES COEFFICIENTS COMPLEXES
    AS_ALLOCATE(vc=coec, size=nbterm)
! ---     VECTEUR DES DIRECTIONS DES DDLS A CONTRAINDRE
    AS_ALLOCATE(vr=direct, size=3*nbterm)
! ---     VECTEUR DES DIMENSIONS DE CES DIRECTIONS
    AS_ALLOCATE(vi=dime, size=nbterm)
!
!        RELATIONS ENTRE LES NOEUDS DE COQUE ET LE NOEUD NOEPOU
!        DE TUYAU SUR LE DDL NOMDDL
!
    do i = 1, lonlis
        call jenonu(jexnom(noeuma, klisno(i)), ino)
!           ADRESSE DE LA PREMIERE CMP DU NOEUD INO DANS LES CHAMNO
        ival = iprno((ino-1)* (nbec+2)+1)
!
        lisno(1+3* (i-1)+1-1) = klisno(i)
        lisno(1+3* (i-1)+2-1) = klisno(i)
        lisno(1+3* (i-1)+3-1) = klisno(i)
!
        lisddl(1+3* (i-1)+1-1) = 'DX'
        lisddl(1+3* (i-1)+2-1) = 'DY'
        lisddl(1+3* (i-1)+3-1) = 'DZ'
!
! RACCORD  3D_TUYAU : IDEC=0 DANS TOUS LES APPELS A AFRETU
! RACCORD COQ_TUYAU : IDEC=0 OU 3 DANS LES APPELS A AFRETU
!
        coer(1+3* (i-1)+1-1) = zr(idch1+ival-1+idec+0)
        coer(1+3* (i-1)+2-1) = zr(idch1+ival-1+idec+1)
        coer(1+3* (i-1)+3-1) = zr(idch1+ival-1+idec+2)
    end do
!
    do i = 1, nbcoef
        lisno(1+3*lonlis+i-1) = noepou
        lisddl(1+3*lonlis+i-1) = nomddl(i)
        coer(1+3*lonlis+i-1) = coef(i)
    end do
!
    call afrela(coer, coec, lisddl, lisno, dime,&
                direct, nbterm, beta, betac, betaf,&
                'REEL', 'REEL', typlag, 0.d0, lisrel)
    call imprel(motfac, nbterm, coer, lisddl, lisno,&
                beta)
!
!
! --- MENAGE
!
    AS_DEALLOCATE(vk8=lisno)
    AS_DEALLOCATE(vk8=lisddl)
    AS_DEALLOCATE(vr=coer)
    AS_DEALLOCATE(vc=coec)
    AS_DEALLOCATE(vr=direct)
    AS_DEALLOCATE(vi=dime)
!
    call jedema()
end subroutine
