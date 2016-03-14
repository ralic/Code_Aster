subroutine cfaca1(nbliac, ajliai, &
                  sdcont_defi, sdcont_solv, solveu,&
                  lmat)
!
implicit none
!
#include "jeveux.h"
#include "asterfort/calatm.h"
#include "asterfort/cfdisi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/nmrldb.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=24) :: sdcont_defi, sdcont_solv
    character(len=19) :: solveu
    integer :: nbliac
    integer :: lmat
    integer :: ajliai
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! Discrete methods - Compute A.C-1.AT by solving system
!
! --------------------------------------------------------------------------------------------------
!
! ROUTINE REALISANT LE CALCUL DE A.C-1.AT PAR RESOLUTION DE C.X=A(I)
!     A(I) -> I-EME COLONNE DE A
!     X    -> I-EME COLONNE DE C-1.A
!
! IN  DEFICO : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  SOLVEU : SD SOLVEUR
! IN  LMAT   : DESCRIPTEUR DE LA MATR_ASSE DU SYSTEME MECANIQUE
! IN  NBLIAC : NOMBRE DE LIAISONS ACTIVES
! I/O AJLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
!              LIAISON CORRECTE DU CALCUL
!              DE LA MATRICE DE CONTACT ACM1AT
!
! --------------------------------------------------------------------------------------------------
!
    integer :: lg, il
    integer :: lliac, jdecal, nbddl
    integer :: neq, lgbloc, tampon
    integer :: nbsm, npas
    integer :: nrest, ipas, kk, iliac,  npast
    character(len=19) :: liac, cm1a
    integer :: jliac, jcm1a
    character(len=24) :: appoin, apddl, apcoef
    integer :: japptr, japddl, japcoe
    character(len=24) :: chsecm
    character(len=19) :: cncin0
    integer, pointer :: vect(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Get objects
!
    cm1a   = sdcont_solv(1:14)//'.CM1A'
    appoin = sdcont_solv(1:14)//'.APPOIN'
    apddl  = sdcont_solv(1:14)//'.APDDL'
    liac   = sdcont_solv(1:14)//'.LIAC'
    apcoef = sdcont_solv(1:14)//'.APCOEF'
    call jeveuo(appoin, 'L', japptr)
    call jeveuo(apddl, 'L', japddl)
    call jeveuo(liac, 'L', jliac)
    call jeveuo(apcoef, 'L', japcoe)
!
! --- NOMBRE D'EQUATIONS DU SYSTEME
!
    neq = zi(lmat+2)
!
! --- CHARGEMENT CINEMATIQUE NUL
!
    cncin0 = sdcont_solv(1:14)//'.CIN0'
    lgbloc = cfdisi(sdcont_defi,'NB_RESOL')
!
    nbsm = nbliac - ajliai
    if (lgbloc .gt. nbsm) lgbloc = nbsm
    npas = nbsm / lgbloc
    nrest = nbsm - lgbloc*npas
!
    if (nrest .gt. 0) then
        npast = npas + 1
    else
        npast = npas
    endif
    chsecm='&&CFACA1.TAMPON'
    call wkvect(chsecm, ' V V R ', neq*lgbloc, tampon)
!
    do ipas = 1, npast
        lg = lgbloc
        if (npast .ne. npas .and. (ipas.eq.npast)) lg = nrest
!
        do kk = 1, neq*lg
            zr(tampon-1+kk) = 0.0d0
        end do
        do il = 1, lg
            iliac = lgbloc* (ipas-1) + il + ajliai
            lliac = zi(jliac+iliac-1)
            jdecal = zi(japptr+lliac-1)
            nbddl = zi(japptr+lliac) - zi(japptr+lliac-1)
            call calatm(neq, nbddl, 1.d0, zr(japcoe+jdecal), zi(japddl+ jdecal),&
                        zr(tampon+neq*(il-1)))
        end do
        call nmrldb(solveu, lmat, zr(tampon), lg, cncin0)
        do il = 1, lg
            iliac = lgbloc* (ipas-1) + il + ajliai
            lliac = zi(jliac+iliac-1)
            call jeveuo(jexnum(cm1a, lliac), 'E', jcm1a)
            do kk = 1, neq
                zr(jcm1a-1+kk) = zr(tampon-1+neq* (il-1)+kk)
            end do
            call jelibe(jexnum(cm1a, lliac))
        end do
    end do

    ajliai = nbliac
!
    call jedetr('&&CFACA1.TAMPON')
    AS_DEALLOCATE(vi=vect)
!
    call jedema()
!
end subroutine
