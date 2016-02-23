subroutine apcoor(mail, jcoor, jtypma, numa, coorma,&
                  nbma, typma, ndim  )
   
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
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
!
    integer, intent(in) :: numa
    integer, intent(out) :: nbma
    integer, intent(in) :: jcoor
    integer, intent(in) :: jtypma
    character(len=8), intent(in) :: mail
    character(len=8), intent(out) :: typma
    real(kind=8), intent(out) :: coorma(27)
    integer, intent(out) :: ndim
! ----------------------------------------------------------------------
!     Récuparation des coordonnées actualisées d'une maille à partir
!     de son numero (Traitement du contact)  
! ----------------------------------------------------------------------
! 
!    
! ----------------------------------------------------------------------
!
    integer ::jmaco, ndco
    integer ::ind1,ind2
! ----------------------------------------------------------------------
!
   

    coorma(1:27)=0.d0
    call jeveuo(jexnum(mail//'.CONNEX',numa),'L',jmaco)
! --- CAS 2D
    if (zi(jtypma+numa-1) .eq. 2) then
        typma = 'SE2'
        nbma = 2
        ndim = 2  
    elseif (zi(jtypma+numa-1) .eq. 4) then
        typma = 'SE3'
        nbma = 3
        ndim = 2      
! --- CAS 3D
    elseif (zi(jtypma+numa-1).eq. 7) then
        typma = 'TR3'
        nbma = 3
        ndim = 3
    elseif (zi(jtypma+numa-1).eq. 9) then
        typma = 'TR6'
        nbma = 6
        ndim = 3
    elseif (zi(jtypma+numa-1).eq. 12) then
        typma = 'QU4'
        nbma = 4
        ndim = 3
    elseif (zi(jtypma+numa-1).eq. 14) then
        typma = 'QU8'
        nbma = 8
        ndim = 3  
    elseif (zi(jtypma+numa-1).eq. 16) then
        typma = 'QU9'
        nbma = 9
        ndim = 3
    else
        ASSERT(.false.)
    endif
! --- Recopie ---------------------------------------------------------
    do ind1=1, nbma
        ndco=zi(jmaco+ind1-1)
        do ind2=1,ndim
            coorma((ind1-1)*3+ind2) = zr(jcoor+(ndco-1)*3+ind2-1)
        end do
    end do
    
end subroutine
