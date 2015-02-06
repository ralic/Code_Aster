subroutine xfem_store_pc(matass, base, nonu, neq, deeq,&
                         nbnoxfem, nbnomax, ino_xfem, ieq_loc, neq_mloc,&
                         maxi_ddl, iglob_ddl, deca, tab_mloc, pc, kstruct)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!-----------------------------------------------------------------------
! BUT : CREATION D UNE MATR_ASSE A PARTIR D UNE MATRICE BLOC <DENSE>
!-----------------------------------------------------------------------
!
! ARGUMENTS :
!------------
!
!
!  SORTIE :
!     - PC : NOM DE MATR_ASSE
!-----------------------------------------------------------------------
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jexnum.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mtdefs.h"
#include "asterfort/wkvect.h"
!-----------------------------------------------------------------------
!
    character(len=19) :: matass, pc
    character(len=14) :: nonu
    character(len=1) :: base
    character(len=5) :: kstruct
    integer :: neq, nbnoxfem, maxi_ddl, deca, nbnomax
    integer :: ino_xfem(nbnomax), deeq(*)
    integer :: ieq_loc(neq), neq_mloc(nbnoxfem), iglob_ddl(maxi_ddl*nbnoxfem)
    real(kind=8) :: tab_mloc(deca*nbnoxfem)
!
!-----------------------------------------------------------------------
    character(len=1) :: kbid
    character(len=14) :: nu_pc
    character(len=24), pointer :: refa_pc(:) => null()
    integer :: cumul_ilig, jcoll, jvale_sup, jvale_inf, nunoj
    integer :: ipos, decaj, jsmhc_pc, jsmdi_pc, jadr, iexi, nvale
!-----------------------------------------------------------------------
!
    call jemarq()
!
    ASSERT(pc(1:11) .eq. pc)
    nu_pc=pc(1:11)//'_NU'
!
!    - PREMIERE PASSE : CALCUL DE L ESPACE <DENSE> NECESSAIRE 
!                          POUR STOCKER LES MATRICES LOCALES TRIANFULAIRES SUPERIEURES
!
    if (kstruct.eq.'D_P_B') then
      cumul_ilig=0
      do jcoll=1,neq  
        if (ieq_loc(jcoll) .eq. 0) then 
          cumul_ilig=cumul_ilig+1
         else
          cumul_ilig=cumul_ilig+ieq_loc(jcoll)
         endif
      enddo
      nvale=2
    elseif (kstruct.eq.'DIAGO') then
      cumul_ilig=neq
      nvale=1
    else
       ASSERT(.false.)
    endif
!
!    - DEUXIEME PASSE : ALLOCATION ET ECRITURE
!
    call jeexin(pc//'.REFA', iexi)
    if ( iexi .gt. 0) call detrsd('MATR_ASSE', pc)
    call mtdefs(pc, matass, base, ' ')
    call jedetr(pc//'.VALM')
    call jecrec(pc//'.VALM', base//' V R', 'NU', 'DISPERSE', 'CONSTANT', nvale)
! TRIANGULAIRE SUPERIEURE
    call jecroc(jexnum(pc//'.VALM', 1))
    call jeecra(pc//'.VALM', 'LONMAX', cumul_ilig, kbid)
    call jeveuo(jexnum(pc//'.VALM', 1), 'E', jvale_sup)
! TRIANGULAIRE INFERIEURE
    if (nvale.eq.2) then
      call jecroc(jexnum(pc//'.VALM', 2))
      call jeveuo(jexnum(pc//'.VALM', 2), 'E', jvale_inf)
    endif
!
    call jeveuo(pc//'.REFA', 'E', vk24=refa_pc)
!
    call jeexin(nu_pc, iexi)
    if ( iexi .gt. 0) call detrsd('NUME_DDL', nu_pc)
    call copisd('NUME_DDL', base, nonu, nu_pc)  
    refa_pc(2)=nu_pc
    call jedetr(nu_pc//'.SMOS.SMDI')
    call wkvect(nu_pc//'.SMOS.SMDI', base//' V I', neq, jsmdi_pc)
    jadr=0
    do jcoll=1,neq
       if (ieq_loc(jcoll) .eq. 0 .or. kstruct.eq.'DIAGO') then
          jadr=jadr+1
       else
          jadr=jadr+ieq_loc(jcoll)
       endif
       zi(jsmdi_pc-1+jcoll)=jadr
    enddo
    ASSERT( jadr .eq. cumul_ilig )
!   - ALLOCATION DU .SMOS.SMHC
    call jedetr(nu_pc//'.SMOS.SMHC')
    call wkvect(nu_pc//'.SMOS.SMHC', base//' V S', cumul_ilig, jsmhc_pc)
!
    decaj=0
    do jcoll=1,neq  
       if (ieq_loc(jcoll) .eq. 0) then 
          decaj=decaj+1
          zi4(jsmhc_pc-1+decaj)=int(jcoll,4)
          zr(jvale_sup-1+decaj)=1.d0
          if (nvale.eq.2) zr(jvale_inf-1+decaj)=1.d0
       else
!
          nunoj=ino_xfem(deeq(2*(jcoll-1)+1))
          if (kstruct.eq.'D_P_B') then
            do ipos=1,ieq_loc(jcoll)
              decaj=decaj+1
              zi4(jsmhc_pc-1+decaj)=int(iglob_ddl(maxi_ddl*(nunoj-1)+ipos),4)
              zr(jvale_sup-1+decaj)=tab_mloc(deca*(nunoj-1)+ neq_mloc(nunoj)*&
                                    (ipos-1)+ieq_loc(jcoll))
              zr(jvale_inf-1+decaj)=tab_mloc(deca*(nunoj-1)+ neq_mloc(nunoj)*&
                                    (ieq_loc(jcoll)-1)+ipos)
            enddo
          else
            decaj=decaj+1
            zi4(jsmhc_pc-1+decaj)=int(jcoll,4)
            zr(jvale_sup-1+decaj)=tab_mloc(deca*(nunoj-1)+ieq_loc(jcoll))
          endif
!
       endif
    enddo
!
    ASSERT( decaj .eq. cumul_ilig )
!
    call jedema()
!
end subroutine
