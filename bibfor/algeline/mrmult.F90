subroutine mrmult(cumul, lmat, vect, xsol, nbvect,&
                  prepos)
    implicit none
#include "jeveux.h"
#include "asterfort/asmpi_comm_vect.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mrmmvr.h"
#include "asterfort/mtdsc2.h"
#include "asterfort/mtmchc.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
    character(len=*) :: cumul
    integer :: lmat, nbvect
    real(kind=8) :: vect(*), xsol(*)
    logical :: prepos, prepo2
!     ------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!    EFFECTUE LE PRODUIT D'UNE MATRICE PAR N VECTEURS REELS. LE RESULTAT
!    EST STOCKE DANS N VECTEURS REELS
!     ATTENTION:
!       - MATRICE SYMETRIQUE OU NON, REELLE.
!       - LES VECTEURS INPUT ET OUTPUT REELS DOIVENT ETRE DISTINCTS
!       - POUR LES DDLS ELIMINES PAR AFFE_CHAR_CINE, ON NE PEUT PAS
!         CALCULER XSOL. CES DDLS SONT MIS A ZERO.
!     ------------------------------------------------------------------
! IN  CUMUL  : K4 :
!              / 'ZERO' : XSOL =        MAT*VECT
!              / 'CUMU' : XSOL = XSOL + MAT*VECT
!
! IN  LMAT  :I:  DESCRIPTEUR DE LA MATRICE
! IN  VECT  :R: VECTEUR(S) A MULTIPLIER PAR LA MATRICE
! VAR XSOL  :R: VECTEUR(S) SOLUTION(S)
!               SI CUMUL = 'ZERO' ALORS XSOL EST EN MODE OUT
! IN  NBVECT: I : NOMBRE DE VECTEURS A MULTIPLIER (ET DONC DE SOLUTIONS)
!     ------------------------------------------------------------------
    character(len=3) :: kmpic, kmatd
    character(len=19) :: matas
    integer :: neq,  neql,  jsmhc, jsmdi
    logical :: lmatd
    real(kind=8), pointer :: vectmp(:) => null()
    real(kind=8), pointer :: xtemp(:) => null()
    character(len=24), pointer :: refa(:) => null()
!     ---------------------------------------------------------------
!
    prepo2=prepos
    call jemarq()
    ASSERT(cumul.eq.'ZERO' .or. cumul.eq.'CUMU')
    matas=zk24(zi(lmat+1))(1:19)
    ASSERT(zi(lmat+3).eq.1)
    call jeveuo(matas//'.REFA', 'L', vk24=refa)
    if (refa(3) .eq. 'ELIMF') call mtmchc(matas, 'ELIML')
    neq=zi(lmat+2)
    AS_ALLOCATE(vr=vectmp, size=neq)
!
    call jeveuo(refa(2)(1:14)//'.SMOS.SMHC', 'L', jsmhc)
    call mtdsc2(zk24(zi(lmat+1)), 'SMDI', 'L', jsmdi)
    call dismoi('MPI_COMPLET', matas, 'MATR_ASSE', repk=kmpic)
!
!
!     1.  MATRICE MPI_INCOMPLET :
!     ----------------------------
    if (kmpic .eq. 'NON') then
        if (cumul .eq. 'CUMU') then
            AS_ALLOCATE(vr=xtemp, size=nbvect*neq)
            call dcopy(nbvect*neq, xsol, 1, xtemp, 1)
        endif
!
        call dismoi('MATR_DISTR', matas, 'MATR_ASSE', repk=kmatd)
        if (kmatd .eq. 'OUI') then
            lmatd=.true.
            neql=zi(lmat+5)
        else
            lmatd=.false.
            neql=0
        endif
        call mrmmvr('ZERO', lmat, zi(jsmdi), zi4(jsmhc), lmatd,&
                    neq, neql, vect, xsol, nbvect,&
                    vectmp, prepo2)
!       ON DOIT COMMUNIQUER POUR OBTENIR LE PRODUIT MAT-VEC 'COMPLET'
        call asmpi_comm_vect('MPI_SUM', 'R', nbval=nbvect*neq, vr=xsol)
!
        if (cumul .eq. 'CUMU') then
            call daxpy(nbvect*neq, 1.d0, xtemp, 1, xsol,&
                       1)
            AS_DEALLOCATE(vr=xtemp)
        endif
!
!
!     2.  MATRICE MPI_COMPLET :
!     ----------------------------
    else
        lmatd=.false.
        neql=0
        call mrmmvr(cumul, lmat, zi(jsmdi), zi4(jsmhc), lmatd,&
                    neq, neql, vect, xsol, nbvect,&
                    vectmp, prepo2)
    endif
!
!
    AS_DEALLOCATE(vr=vectmp)
    call jedema()
end subroutine
