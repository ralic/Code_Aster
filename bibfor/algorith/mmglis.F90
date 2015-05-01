subroutine mmglis(defico, resoco)
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
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mminfi.h"
#include "asterfort/mminfl.h"
#include "asterfort/mminfm.h"
    character(len=24) :: defico, resoco
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES CONTINUES - ALGORITHME)
!
! GESTION DE LA GLISSIERE
!
! ----------------------------------------------------------------------
!
!     ON MET LE POINT EN GLISSIERE SI LGLISS=.TRUE. ET
!     SI LA CONVERGENCE EN CONTRAINTE ACTIVE EST ATTEINTE
!
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  RESOCO : SD DE RESOLUTION DU CONTACT
!
!
!
!
    integer :: ifm, niv
    integer :: ztabf
    character(len=24) :: tabfin
    integer :: jtabf
    integer :: nzoco, nbmae, nptm
    aster_logical :: lveri, lgliss
    integer :: izone, imae, iptc, iptm
    integer :: xs
    integer :: posmae, jdecme
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ... GESTION GLISSIERE'
    endif
!
! --- ACCES SD CONTACT
!
    tabfin = resoco(1:14)//'.TABFIN'
    call jeveuo(tabfin, 'L', jtabf)
    ztabf = cfmmvd('ZTABF')
!
! --- INITIALISATIONS
!
    nzoco = cfdisi(defico,'NZOCO')
    iptc = 1
!
! --- BOUCLE SUR LES ZONES
!
    do 10 izone = 1, nzoco
!
! ----- MODE VERIF: ON SAUTE LES POINTS
!
        lveri = mminfl(defico,'VERIF' ,izone )
        if (lveri) then
            goto 25
        endif
!
! --- OPTIONS SUR LA ZONE DE CONTACT
!
        lveri = mminfl(defico,'VERIF' ,izone )
        nbmae = mminfi(defico,'NBMAE' ,izone )
        jdecme = mminfi(defico,'JDECME' ,izone )
        lgliss = mminfl(defico,'GLISSIERE_ZONE' ,izone )
!
! ----- BOUCLE SUR LES MAILLES ESCLAVES
!
        do 20 imae = 1, nbmae
!
! ------- NUMERO ABSOLU DE LA MAILLE ESCLAVE
!
            posmae = jdecme + imae
!
! ------- NOMBRE DE POINTS SUR LA MAILLE ESCLAVE
!
            call mminfm(posmae, defico, 'NPTM', nptm)
!
! ------- BOUCLE SUR LES POINTS
!
            if (lgliss) then
                do 30 iptm = 1, nptm
                    xs = nint(zr(jtabf+ztabf*(iptc-1)+22))
                    if (xs .eq. 1) then
                        zr(jtabf+ztabf*(iptc-1)+17) = 1.d0
                    endif
!
! --------- LIAISON DE CONTACT SUIVANTE
!
                    iptc = iptc + 1
 30             continue
            else
                iptc = iptc + nptm
            endif
 20     continue
 25     continue
 10 end do
!
    call jedema()
end subroutine
