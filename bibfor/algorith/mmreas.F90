subroutine mmreas(noma, defico, resoco, valinc)
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
    implicit none
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
    character(len=8) :: noma
    character(len=24) :: defico, resoco
    character(len=19) :: valinc(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - FROTTEMENT)
!
! REACTUALISATION DES SEUILS DE FROTTEMENT PAR LES MULTIPLICATEURS
! DE CONTACT
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  RESOCO : SD DE RESOLUTION DU CONTACT
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
!
! ----------------------------------------------------------------------
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
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ... MISE A JOUR DES SEUILS DE FROTTEMENT'
    endif
!
! --- INITIALISATIONS
!
    ndimg = cfdisi(defico,'NDIM' )
    nzoco = cfdisi(defico,'NZOCO')
    ibid = 0
!
! --- RECUPERATION DES QCQS DONNEES
!
    tabfin = resoco(1:14)//'.TABFIN'
    call jeveuo(tabfin, 'E', jtabf)
    ztabf = cfmmvd('ZTABF')
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(valinc, 'VALINC', 'DEPPLU', depplu)
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
        lveri = mminfl(defico,'VERIF' ,izone )
        nbmae = mminfi(defico,'NBMAE' ,izone )
        jdecme = mminfi(defico,'JDECME',izone )
!
! ----- MODE VERIF: ON SAUTE LES POINTS
!
        lveri = mminfl(defico,'VERIF' ,izone )
        if (lveri) then
            goto 25
        endif
!
! ----- BOUCLE SUR LES MAILLES ESCLAVES
!
        do 20 imae = 1, nbmae
!
! ------- NUMERO ABSOLU DE LA MAILLE ESCLAVE
!
            posmae = jdecme + imae
            call cfnumm(defico, posmae, nummae)
!
! ------- INFOS SUR LA MAILLE
!
            call mmelty(noma, nummae, aliase, nne)
!
! ------- MULTIPLICATEURS DE CONTACT SUR LES NOEUDS DE LA MAILLE ESCLAVE
!
            call mmextm(defico, cnslbd, posmae, mlagc)
!
! ------- NOMBRE DE POINTS SUR LA MAILLE ESCLAVE
!
            call mminfm(posmae, defico, 'NPTM', nptm)
!
! ------- BOUCLE SUR LES POINTS
!
            do 30 iptm = 1, nptm
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
 30         continue
 20     continue
 25     continue
    end do
!
    call detrsd('CHAM_NO_S', cnslbd)
    call jedema()
end subroutine
