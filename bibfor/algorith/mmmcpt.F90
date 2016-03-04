subroutine mmmcpt(noma, ds_measure, ds_contact, cnsinr)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mminfi.h"
#include "asterfort/mminfl.h"
#include "asterfort/mminfm.h"
#include "asterfort/nmrvai.h"
#include "asterfort/wkvect.h"
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
    character(len=8) :: noma
    type(NL_DS_Measure), intent(inout) :: ds_measure
    character(len=19) :: cnsinr
    type(NL_DS_Contact), intent(in) :: ds_contact
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - POST-TRAITEMENT)
!
! DECOMPTE DES LIAISONS
!
! ----------------------------------------------------------------------
!
! IN  NOMA   : NOM DU MAILLAGE
! IO  ds_measure       : datastructure for measure and statistics management
! In  ds_contact       : datastructure for contact management
! IN  CNSINR : CHAM_NO_S POUR L'ARCHIVAGE DU CONTACT
!
!
    integer :: iptc, izone, imae, iptm
    integer :: nptm, nbmae, nzoco, nbno
    integer :: ztabf, zresu
    integer :: numnoe
    integer :: posmae
    integer :: jdecme
    integer :: cont
    character(len=24) :: tabfin
    integer :: jtabf
    aster_logical :: lveri, lnoeu
    integer :: jcnslr
    integer :: nbliac, nbliaf
    character(len=24) :: dejcal
    integer :: jdejca
    real(kind=8), pointer :: cnsv(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    nzoco = cfdisi(ds_contact%sdcont_defi,'NZOCO' )
    nbliac = 0
    nbliaf = 0
!
! --- SD TEMPORAIRE POUR VERIF NOEUDS DEJA CALCULES
!
    call dismoi('NB_NO_MAILLA', noma, 'MAILLAGE', repi=nbno)
    dejcal = '&&XMMRES.DEJCAL'
    call wkvect(dejcal, 'V V I', nbno, jdejca)
!
! --- TOUTES LES ZONES EN INTEGRATION AUX NOEUDS ?
!
    lnoeu = cfdisl(ds_contact%sdcont_defi,'ALL_INTEG_NOEUD')
    if (.not.lnoeu) then
        nbliac = 0
        nbliaf = 0
        goto 999
    endif
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    tabfin = ds_contact%sdcont_solv(1:14)//'.TABFIN'
    call jeveuo(tabfin, 'L', jtabf)
!
    ztabf = cfmmvd('ZTABF')
    zresu = cfmmvd('ZRESU')
!
! --- ACCES AU CHAM_NO_S POUR LE CONTACT
!
    call jeveuo(cnsinr(1:19)//'.CNSV', 'L', vr=cnsv)
    call jeveuo(cnsinr(1:19)//'.CNSL', 'L', jcnslr)
!
! --- BOUCLE SUR LES ZONES
!
    iptc = 1
    do izone = 1, nzoco
!
! --- OPTIONS SUR LA ZONE DE CONTACT
!
        lveri = mminfl(ds_contact%sdcont_defi,'VERIF' ,izone )
        nbmae = mminfi(ds_contact%sdcont_defi,'NBMAE' ,izone )
        jdecme = mminfi(ds_contact%sdcont_defi,'JDECME',izone )
!
! ----- MODE VERIF: ON SAUTE LES POINTS
!
        if (lveri) then
            goto 25
        endif
!
! ----- BOUCLE SUR LES MAILLES ESCLAVES
!
        do imae = 1, nbmae
!
! ------- POSITION DE LA MAILLE ESCLAVE
!
            posmae = jdecme + imae
!
! ------- NOMBRE DE POINTS SUR LA MAILLE ESCLAVE
!
            call mminfm(posmae, ds_contact%sdcont_defi, 'NPTM', nptm)
!
! ------- BOUCLE SUR LES POINTS
!
            do iptm = 1, nptm
!
! --------- INFOS
!
                numnoe = nint(zr(jtabf+ztabf*(iptc-1)+24))
                if (numnoe .gt. 0) then
                    if (zi(jdejca+numnoe-1) .eq. 0) then
                        cont = nint(cnsv(1+zresu*(numnoe-1)+1 -1))
                        if (cont .ge. 1) then
                            nbliac = nbliac + 1
                            if (cont .eq. 1) then
                                nbliaf = nbliaf + 1
                            endif
                        endif
                        zi(jdejca+numnoe-1) = 1
                    endif
                endif
!
! --------- LIAISON DE CONTACT SUIVANTE
!
                iptc = iptc + 1
            end do
        end do
 25     continue
    end do
!
999 continue
!
    call jedetr(dejcal)
    call nmrvai(ds_measure, 'Cont_NCont', input_count = nbliac)
    call nmrvai(ds_measure, 'Cont_NFric', input_count = nbliaf)
!
    call jedema()
end subroutine
