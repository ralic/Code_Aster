subroutine gtvois(mail  , lima  , nbma  , numa, typma,&
                  cninv , nbvois, voisin)
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
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/utlisi.h"
#include "asterfort/jelira.h"
#include "asterfort/assert.h"
!
    character(len=8), intent(in) :: mail
    character(len=24), intent(in) :: cninv
    integer, intent(in) :: nbma
    integer, intent(in) :: lima(nbma)
    integer, intent(in) :: numa
    character(len=8), intent(in) :: typma
    integer, intent(in) :: nbvois
    integer, intent(out) :: voisin(4)
! ----------------------------------------------------------------------
!        MAILLE VOISINE D'UNE MAILLE DONNE (ROUTINE CONTACT LAC) 
!                  (POUR LE GROUPE DE MAILLE LIMA)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
    integer :: nbno, lino(4), jnuno, no1, no2
    integer :: jmano1, jmano2
    integer :: nmano1, nmano2
    integer :: ntrou, matrou(2)
    integer :: indsu(4)
    integer :: ind, ndim
!    
! --- Initialisation ---------------------------------------------------
!
    call jemarq()
    voisin(1:4) = 0   
! ----------------------------------------------------------------------
!        Recuperation des noeuds de la maille numa
! ----------------------------------------------------------------------
    call jeveuo(jexnum(mail//'.CONNEX',numa),'L',jnuno)
! ---- CAS 2D
    if (typma .eq. 'SE2' .or. typma .eq. 'SE3') then
        nbno = 2
        ndim = 1 
! ---- CAS 3D
    elseif (typma .eq. 'TR3' .or. typma .eq. 'TR6') then
        nbno = 3
        ndim = 2
    elseif (typma .eq. 'QU4' .or. typma .eq. 'QU8' .or. typma .eq. 'QU9') then
        nbno = 4
        ndim = 2
    else
        ASSERT(.false.)
    end if    
    do ind=1, nbno
        lino(ind) = zi(jnuno+ind-1)
    end do 
    do ind=2, nbno
        indsu(ind-1) = ind
    end do
    indsu(nbno) = 1
! ----------------------------------------------------------------------
!        Recherche des voisins
! ----------------------------------------------------------------------
    if (ndim.eq.2) then
        do ind = 1,nbvois
            ntrou=0
            no1 = lino(ind)
            no2 = lino(indsu(ind))
            call jelira(jexnum(cninv,no1),'LONMAX',nmano1)
            call jelira(jexnum(cninv,no2),'LONMAX',nmano2)
            call jeveuo(jexnum(cninv,no1),'L',jmano1)
            call jeveuo(jexnum(cninv,no2),'L',jmano2)
! 
            call utlisi('INTER',zi(jmano1),nmano1,zi(jmano2),nmano2,&
                        matrou , 2,ntrou)
            ASSERT(ntrou .le. 2)
            ASSERT(ntrou .ge. 1)
            if (ntrou .eq. 2) then
                if (lima(matrou(1)) .eq. numa) then
                    voisin(ind) = lima(matrou(2))
                else
                    voisin(ind) = lima(matrou(1))
                end if    
            end if
        end do
    elseif (ndim .eq. 1) then
        do ind = 1,nbvois
            ntrou=0
            no1 = lino(ind)
            call jelira(jexnum(cninv,no1),'LONMAX',nmano1)          
            call jeveuo(jexnum(cninv,no1),'L',jmano1)
! 
            ASSERT(nmano1 .le. 2)
            if (nmano1 .eq. 2) then
                if (lima(zi(jmano1+1-1)) .eq. numa) then
                    voisin(ind) = lima(zi(jmano1+2-1))
                else
                    voisin(ind) = lima(zi(jmano1+1-1))
                end if    
            end if
        end do
    end if

    call jedema()
end subroutine

