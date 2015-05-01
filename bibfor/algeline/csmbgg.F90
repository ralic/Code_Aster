subroutine csmbgg(lmat, vsmb, vcine, cvsmb, cvcine,&
                  type)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/csmbc8.h"
#include "asterfort/csmbmc.h"
#include "asterfort/csmbmd.h"
#include "asterfort/csmbr8.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    real(kind=8) :: vsmb(*), vcine(*)
    complex(kind=8) :: cvsmb(*), cvcine(*)
    integer :: lmat
    character(len=*) :: type
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
! BUT : CALCUL DE LA CONTRIBUTION AU SECOND MEMBRE DES DDLS IMPOSEES
!       LORSQU'ILS SONT TRAITEES PAR ELIMINATION
!
!        ! K    K   ! L POUR NON IMPOSE I POUR IMPOSE
! K  =   !  LL   LI !
!        !  T       !
!        ! K    K   !
!        !  LI   II !
!
!  LE TRAITEMENT PAR ELIMINATION CONSISTE A RESOUDRE
!
!    ! K    0 !   ! X  !   ! 1  -K   !   ! F  !
!    !  LL    !   !  L !   !      IL !   !  I !
!    !        ! * !    ! = !         ! * !    ! <=> K' X = F'
!    ! 0    1 !   ! X  !   ! 0    1  !   ! U  !
!    !        !   !  I !   !         !   !  0 !
!  ON A LMAT  :DESCRIPTEUR DE K' CAR DANS L'ASSEMBLAGE ON ASSEMBLE
!              DIRECTEMENT K'   KIL SE TROUVE DANS .CCVA DE K'
!       VSMB  :EN IN (FI,0)  EN OUT = F'
!       VCINE : (0,U0)
! REMARQUES :
!  SI LMAT (7) = 0  ALORS GOTO 9999
!-----------------------------------------------------------------------
! !!!ATTENTION!!! LA MATRICE LE VECTEUR SECOND MEMBRE ET LE VECTEUR
!   CINEMATIQUE DOIVENT TOUS LES TROIS ETRE DE MEME TYPE (R/C)
!-----------------------------------------------------------------------
! IN  LMAT  I   : DESCRIPTEUR DE LA MATRICE SUR LAQUELLE ON A EFFECTUE
!                 LES ELIMINATIONS
! VAR VSMB  SCA : VECTEUR SECOND MEMBRE
! IN  VCINE SCA : VECTEUR DE CHARGEMENT CINEMATIQUE ( LE U0 DE U = U0
!                 SUR G AVEC VCINE = 0 EN DEHORS DE G )
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!     VARIABLES LOCALES
!-----------------------------------------------------------------------
    integer :: neq, nimpo,   eccll
    integer, pointer :: ccll(:) => null()
    integer, pointer :: ccii(:) => null()
!-----------------------------------------------------------------------
!     DEBUT
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call jemarq()
!-----------------------------------------------------------------------
    neq = zi(lmat+2)
!
    if (zi(lmat+3) .eq. 1) then
        call csmbmd(zk24(zi(lmat+1)), neq, vsmb)
    else if (zi(lmat+3).eq.2) then
        call csmbmc(zk24(zi(lmat+1)), neq, cvsmb)
    endif
!
    nimpo = zi(lmat+7)
    if (nimpo .eq. 0) goto 10
!
    call jeexin(zk24(zi(lmat+1))(1:19)//'.CCLL', eccll)
    if (eccll .eq. 0) goto 10
!
    call jeveuo(zk24(zi(lmat+1))(1:19)//'.CCLL', 'L', vi=ccll)
    call jeveuo(zk24(zi(lmat+1))(1:19)//'.CCII', 'L', vi=ccii)
!
!     ------------------------------------------------------------------
!
    if (zi(lmat+3) .eq. 1) then
!
!        --- SYSTEME REEL:
        ASSERT(type.eq.'R')
        call csmbr8(zk24(zi(lmat+1)), ccll, ccii, neq, vcine,&
                    vsmb)
!
    else if (zi(lmat+3).eq.2) then
!
!        --- SYSTEME COMPLEXE:
        ASSERT(type.eq.'C')
        call csmbc8(zk24(zi(lmat+1)), ccll, ccii, neq, cvcine,&
                    cvsmb)
    endif
!
10  continue
!
    call jedema()
end subroutine
