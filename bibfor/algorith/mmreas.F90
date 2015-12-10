subroutine mmreas(mesh, ds_contact, hval_incr)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/cfnumm.h"
#include "asterfort/mmfield_prep.h"
#include "asterfort/detrsd.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mmelty.h"
#include "asterfort/mmextm.h"
#include "asterfort/mminfi.h"
#include "asterfort/mminfl.h"
#include "asterfort/mminfm.h"
#include "asterfort/mmvalp_scal.h"
#include "asterfort/nmchex.h"
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: mesh
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=19), intent(in) :: hval_incr(*)
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! Continue method - Update triggers for friction
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  ds_contact       : datastructure for contact management
! In  hval_incr        : hat-variable for incremental values fields
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: ztabf
    integer :: ibid
    integer :: posmae, jdecme, nummae
    integer :: iptc
    integer :: izone, imae, iptm
    integer :: nne, nbmae, nptm
    integer :: ndimg, nzoco
    aster_logical :: lveri
    real(kind=8) :: lambdc, ksipc1, ksipc2
    real(kind=8) :: mlagc(9)
    character(len=8) :: aliase
    character(len=19) :: cnslbd, depplu
    character(len=24) :: tabfin
    integer :: jtabf
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ... MISE A JOUR DES SEUILS DE FROTTEMENT'
    endif
!
! --- INITIALISATIONS
!
    ndimg = cfdisi(ds_contact%sdcont_defi,'NDIM' )
    nzoco = cfdisi(ds_contact%sdcont_defi,'NZOCO')
    ibid = 0
!
! --- RECUPERATION DES QCQS DONNEES
!
    tabfin = ds_contact%sdcont_solv(1:14)//'.TABFIN'
    call jeveuo(tabfin, 'E', jtabf)
    ztabf = cfmmvd('ZTABF')
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(hval_incr, 'VALINC', 'DEPPLU', depplu)
!
! --- TRANSFORMATION DEPPLU EN CHAM_NO_S ET REDUCTION SUR LES LAGRANGES
!
    cnslbd = '&&REACLM.CNSLBD'
    call mmfield_prep(depplu, cnslbd,&
                      l_sort_ = .true._1, nb_cmp_ = 1, list_cmp_ = ['LAGS_C  '])
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
        lveri = mminfl(ds_contact%sdcont_defi,'VERIF' ,izone )
        if (lveri) then
            goto 25
        endif
!
! ----- BOUCLE SUR LES MAILLES ESCLAVES
!
        do imae = 1, nbmae
!
! ------- NUMERO ABSOLU DE LA MAILLE ESCLAVE
!
            posmae = jdecme + imae
            call cfnumm(ds_contact%sdcont_defi, posmae, nummae)
!
! ------- INFOS SUR LA MAILLE
!
            call mmelty(mesh, nummae, aliase, nne)
!
! ------- MULTIPLICATEURS DE CONTACT SUR LES NOEUDS DE LA MAILLE ESCLAVE
!
            call mmextm(ds_contact%sdcont_defi, cnslbd, posmae, mlagc)
!
! ------- NOMBRE DE POINTS SUR LA MAILLE ESCLAVE
!
            call mminfm(posmae, ds_contact%sdcont_defi, 'NPTM', nptm)
!
! ------- BOUCLE SUR LES POINTS
!
            do iptm = 1, nptm
!
! --------- COORDONNEES ACTUALISEES DU POINT DE CONTACT
!
                ksipc1 = zr(jtabf+ztabf*(iptc-1)+3 )
                ksipc2 = zr(jtabf+ztabf*(iptc-1)+4 )
!
! --------- MULTIPLICATEUR DE LAGRANGE DE CONTACT DU POINT
!
                call mmvalp_scal(ndimg, aliase, nne, ksipc1,&
                                 ksipc2, mlagc, lambdc)
!
! --------- SAUVEGARDE
!
                zr(jtabf+ztabf*(iptc-1)+16) = lambdc
!
! --------- LIAISON DE CONTACT SUIVANTE
!
                iptc = iptc + 1
            end do
        end do
 25     continue
    end do
!
    call detrsd('CHAM_NO_S', cnslbd)
    call jedema()
end subroutine
