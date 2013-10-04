subroutine mm_cycl_detect(sd_cont_defi, sd_cont_solv)
!
    implicit     none
!
#include "jeveux.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mmcyc1.h"
#include "asterfort/mmcyc2.h"
#include "asterfort/mm_cycl_d3.h"
#include "asterfort/mmcyc4.h"
#include "asterfort/mminfi.h"
#include "asterfort/mminfl.h"
#include "asterfort/mminfm.h"
#include "asterfort/mmnpoi.h"
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=24), intent(in) :: sd_cont_defi, sd_cont_solv
!
! ----------------------------------------------------------------------
!
! Contact (continue method)
!
! Cycling detection
!
! ----------------------------------------------------------------------
!
! In  sd_cont_solv : data structure for contact solving
! In  sd_cont_defi : data structure from contact definition 
!
! ----------------------------------------------------------------------
!
    integer :: ifm, niv
    character(len=24) :: tabfin
    integer :: jtabf
    integer :: ztabf
    integer :: indi_cont, indi_frot
    integer :: nbmae, nptm
    logical :: lveri
    integer :: nzoco
    logical :: lfrot, lboucc
    integer :: izone, iptm, imae, iptc
    integer :: jdecme, posmae
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
    if (niv .ge. 2) write (ifm,*) '<CONTACT> ... Cycling detection'
!
! - Acces to contact objects
!
    tabfin = sd_cont_solv(1:14)//'.TABFIN'
    call jeveuo(tabfin, 'L', jtabf)
    ztabf = cfmmvd('ZTABF')
!
! - Initializations
!
    nzoco = cfdisi(sd_cont_defi,'NZOCO')
    lfrot = cfdisl(sd_cont_defi,'FROTTEMENT')
    lboucc = cfdisl(sd_cont_defi,'CONT_BOUCLE')
!
! - Loop on contact zones
!
    iptc = 1
    do izone = 1, nzoco
!
! ----- Contact options on zone
!
        lveri = mminfl(sd_cont_defi,'VERIF' ,izone )
        nbmae = mminfi(sd_cont_defi,'NBMAE' ,izone )
        jdecme = mminfi(sd_cont_defi,'JDECME',izone )
!
! ----- No contact solving: break
!
        if (lveri) goto 25
!
! ----- Loop on salve meshes
!
        do imae = 1, nbmae
!
! --------- Slave mesh informations
!
            posmae = jdecme + imae
            call mminfm(posmae, sd_cont_defi, 'NPTM', nptm)
!
! --------- Loop on contact points
!
            do iptm = 1, nptm
!
! ------------- Current status
!
                indi_cont = nint(zr(jtabf+ztabf*(iptc-1)+22))
                indi_frot = nint(zr(jtabf+ztabf*(iptc-1)+23))
!
! ------------- DETECTION DU CYCLE DE TYPE CONTACT/PAS CONTACT
!
                call mmcyc1(sd_cont_solv, iptc, indi_cont)
!
! ------------- DETECTION DU CYCLE DE TYPE ADHERENT/GLISSANT
!
                if (lfrot) call mmcyc2(sd_cont_solv, iptc, indi_cont, indi_frot)
!
! ------------- Detection of cycling: sliding forward/backward
!
                if (lfrot) call mm_cycl_d3(sd_cont_defi, sd_cont_solv, iptc, indi_cont,indi_frot)
!
! ------------- DETECTION DU CYCLE DE TYPE FLIP-FLOP HISTORIQUE
!
                if (lboucc) call mmcyc4(sd_cont_solv, iptc, indi_cont)
!
! ------------- Next active contact point
!
                iptc = iptc + 1
            end do
        end do
25      continue
    end do
!
    call jedema()
end subroutine
