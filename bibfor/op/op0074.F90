subroutine op0074()
    implicit none
! ----------------------------------------------------------------------
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
!     OPERATEUR: DYNA_TRAN_MODAL
!
! ----------------------------------------------------------------------
!
!
!
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/gettco.h"
#include "asterfort/cresol.h"
#include "asterfort/getvid.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mdtr74.h"
#include "asterfort/mdveri.h"
#include "asterfort/resu74.h"
#include "asterfort/ssdt74.h"
    character(len=8) :: matgen, nomres, tran
    character(len=14) :: numgen
    character(len=19) :: solveu
    character(len=16) :: typrep, nomcmd, typres
    integer :: iarg
!
!     --- ETAPE DE VERIFICATIONS
!
!-----------------------------------------------------------------------
    integer :: jrefe, ndt, nm
!-----------------------------------------------------------------------
    call jemarq()
    call infmaj()
!
    call mdveri()
!
!     --- RECUPERATION NOM DE LA COMMANDE ---
!
    call getres(nomres, typres, nomcmd)
    call getvid('ETAT_INIT', 'RESULTAT', iocc=1, scal=tran, nbret=ndt)
    if (ndt .ne. 0) then
!        --- TEST SI REPRISE AVEC NOM DE CONCEPT IDENTIQUE ---
        if (tran .eq. nomres) nomres='&&OP0074'
    endif
!
!     --- DETERMINATION DU TYPE DE CALCUL ---
!
    call getvid(' ', 'MATR_MASS', scal=matgen, nbret=nm)
    call jeveuo(matgen//'           .REFA', 'L', jrefe)
    numgen = zk24(jrefe+1)(1:14)
    call jeveuo(numgen//'.NUME.REFN', 'L', jrefe)
    call gettco(zk24(jrefe), typrep)
!
!
!
!     --- CREATION DU SOLVEUR ---
!
    solveu='&&OP0074.SOLVEUR'
    call cresol(solveu)
!
!
!       --- CALCUL PAR SUPERPOSITION SUR BASE MODALE ---
!
    if (typrep .eq. 'MODE_MECA       ' .or. typrep .eq. 'MODE_GENE       ') then
        call mdtr74(nomres)
    endif
!
!       --- CALCUL PAR SOUS-STRUCTURATION DIRECTE ---
!
    if (typrep .eq. 'MODELE_GENE     ') then
        call ssdt74(nomres, nomcmd)
    endif
!
!
!     --- CAS DE REPRISE AVEC LE MEME NOM DE CONCEPT ---
!
    if (nomres .eq. '&&OP0074') call resu74(tran, nomres)
!
    call jedema()
end subroutine
