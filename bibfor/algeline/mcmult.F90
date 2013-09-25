subroutine mcmult(cumul, lmat, vect, xsol, nbvect,&
                  prepos)
    implicit none
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mcmmvc.h"
#include "asterfort/mcmmvr.h"
#include "asterfort/mtdsc2.h"
#include "asterfort/mtmchc.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    character(len=*) :: cumul
    integer :: lmat, nbvect
    complex(kind=8) :: vect(*), xsol(*)
    logical :: prepos, prepo2
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     EFFECTUE LE PRODUIT D'UNE MATRICE PAR N VECTEURS COMPLEXES.
!     LE RESULTAT EST STOCKE DANS N VECTEURS COMPLEXES
!     ATTENTION:
!       - MATRICE SYMETRIQUE OU NON, REELLE OU COMPLEXE
!       - VECTEURS INPUT ET OUTPUT COMPLEXES ET DISTINCTS
!       - POUR LES DDLS ELIMINES PAR AFFE_CHAR_CINE, ON NE PEUT PAS
!         CALCULER XSOL. CES DDLS SONT MIS A ZERO.
!     ------------------------------------------------------------------
! IN  CUMUL  : K4 :
!              / 'ZERO' : XSOL =        MAT*VECT
!              / 'CUMU' : XSOL = XSOL + MAT*VECT
!
! IN  LMAT  : I : DESCRIPTEUR DE LA MATRICE
! IN  VECT  :R/C: VECTEURS A MULTIPLIER PAR LA MATRICE
! VAR XSOL  :R/C: VECTEUR(S) SOLUTION(S)
!               SI CUMUL = 'ZERO' ALORS XSOL EST EN MODE OUT
! IN  NBVECT: I : NOMBRE DE VECTEURS A MULTIPLIER (ET DONC DE SOLUTIONS)
!     ------------------------------------------------------------------
    character(len=3) :: kmpic
    character(len=19) :: matas
    integer :: ibid, jrefa, jsmdi, jsmhc, jvtemp, neq
!
    call jemarq()
    prepo2=prepos
    matas=zk24(zi(lmat+1))(1:19)
    call jeveuo(matas//'.REFA', 'L', jrefa)
    if (zk24(jrefa-1+3) .eq. 'ELIMF') call mtmchc(matas, 'ELIML')
!
    call dismoi('F', 'MPI_COMPLET', matas, 'MATR_ASSE', ibid,&
                kmpic, ibid)
    if (kmpic .ne. 'OUI') then
        call utmess('F', 'CALCULEL6_54')
    endif
!
    call jeveuo(zk24(jrefa-1+2)(1:14)//'.SMOS.SMHC', 'L', jsmhc)
    neq=zi(lmat+2)
    call wkvect('&&MCMULT.VECTMP', 'V V C', neq, jvtemp)
!
!
!     SELON REEL OU COMPLEXE :
    if (zi(lmat+3) .eq. 1) then
!
        call mtdsc2(zk24(zi(lmat+1)), 'SMDI', 'L', jsmdi)
        call mcmmvr(cumul, lmat, zi(jsmdi), zi4(jsmhc), neq,&
                    vect, xsol, nbvect, zc(jvtemp), prepo2)
!
    else if (zi(lmat+3) .eq. 2) then
!
!     MATRICE COMPLEXE
        call mtdsc2(zk24(zi(lmat+1)), 'SMDI', 'L', jsmdi)
        call mcmmvc(cumul, lmat, zi(jsmdi), zi4(jsmhc), neq,&
                    vect, xsol, nbvect, zc(jvtemp), prepo2)
!
    else
!
        call utmess('F', 'ALGELINE_66')
!
    endif
!
    call jedetr('&&MCMULT.VECTMP')
    call jedema()
end subroutine
