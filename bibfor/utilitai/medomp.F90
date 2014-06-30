subroutine medomp(result, modele, mate, carele, nh)
!
    implicit none
!
#include "jeveux.h"
#include "asterc/getexm.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rcmfmc.h"
#include "asterfort/rslesd.h"
#include "asterfort/rsutnu.h"
#include "asterfort/utmess.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=8) :: modele, carele, result
    character(len=24) :: mate
    integer :: nh
!
! ----------------------------------------------------------------------
!
!  OPERATEUR POST_ELEM
!
!  SAISIE ET VERIFICATION DE LA COHERENCE DES DONNEES MECANIQUES
!  DU PROBLEME
!
! ----------------------------------------------------------------------
!
! IN  RESULT : NOM DE LA SD RESULTAT
! OUT MODELE : NOM DU MODELE
! OUT MATE   : MATERIAU CODE
! OUT CARELE : CARACTERISTIQUES ELEMENTAIRES
! OUT NH     : MODE DE FOURIER
!
! ----------------------------------------------------------------------
!
    integer :: iexcit, iret
    integer :: nbordr, iordr, jordr, numord, inuord, numlu
    integer :: n1, n2, n3
    real(kind=8) :: prec
    character(len=8) :: materi, modnew
    character(len=16) :: repons
    character(len=19) :: knum, k19bid
    character(len=8) :: crit
    logical(kind=1) :: lrdm, lmater
    integer :: lfour
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    materi = ' '
    modele = ' '
    carele = ' '
    mate = ' '
    nbordr = 0
    numord = 0
    nh = 0
!
    if (result(1:1) .eq. ' ') then
!
! ----- RECUPERATION DU MODELE DANS LA COMMANDE
!
        call getvid(' ', 'MODELE', scal=modele, nbret=n1)
        if (n1 .eq. 0) then
            call utmess('F', 'POSTELEM_20')
        endif
        call dismoi('EXI_RDM', modele, 'MODELE', repk=repons)
        lrdm = repons.eq.'OUI'
        call dismoi('BESOIN_MATER', modele, 'MODELE', repk=repons)
        lmater = repons.eq.'OUI'
!
! ----- RECUPERATION DU CARA_ELEM DANS LA COMMANDE
!
        call getvid(' ', 'CARA_ELEM', scal=carele, nbret=n2)
        if ((n2.eq.0) .and. lrdm) then
            call utmess('A', 'CALCULEL3_39')
        endif
!
! ----- RECUPERATION DU CHAM_MATER DANS LA COMMANDE
!
        call getvid(' ', 'CHAM_MATER', scal=materi, nbret=n3)
        if ((n3.eq.0) .and. lmater) then
            call utmess('A', 'CALCULEL3_40')
        endif
!
    else
!
        call getvis(' ', 'NUME_ORDRE', scal=numlu, nbret=inuord)
!
! ----- L'UTILISATEUR N'A PAS FOURNI DE NUMERO D'ORDRE :
! ----- RECUPERATION DU PREMIER NUMERO D'ORDRE DANS LA SD RESULTAT
!
        knum = '&&MEDOMP.NUME_ORDRE'
        if (inuord .eq. 0) then
            call getvr8(' ', 'PRECISION', scal=prec, nbret=n1)
            call getvtx(' ', 'CRITERE', scal=crit, nbret=n2)
            call rsutnu(result, ' ', 0, knum, nbordr,&
                        prec, crit, iret)
            call jeveuo(knum, 'L', jordr)
            numlu = zi(jordr)
        endif
!
! ----- VERIFICATION DE L'UNICITE DU MODELE DANS LE RESULTAT
!
        numord = numlu
        call rslesd(result, numord, modele, materi, carele,&
                    k19bid, iexcit)
        do iordr = 2, nbordr
            numord = zi(jordr+iordr-1)
            call rslesd(result, numord, modnew, materi, carele,&
                        k19bid, iexcit)
            if (modnew .ne. modele) then
                call utmess('F', 'POSTELEM_23')
            endif
        end do
        call jedetr(knum)
!
! ----- RECUPERATION MODELE, MATERIAU ET CARA_ELEM DANS LA SD RESULTAT
!
        call rslesd(result, numlu, modele, materi, carele,&
                    k19bid, iexcit)
!
    endif
!
! --- CODAGE DU MATERIAU
!
    if (materi .ne. ' ') call rcmfmc(materi, mate)
!
! --- MODE FOURIER SI NECESSAIRE
!
    lfour = getexm(' ','MODE_FOURIER')
    if (lfour .eq. 1) then
        call getvis(' ', 'MODE_FOURIER', scal=nh, nbret=n1)
    endif
!
    call jedema()
end subroutine
