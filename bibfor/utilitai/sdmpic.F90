subroutine sdmpic(typesd, nomsd)
    implicit none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mpicm2.h"
    character(len=*) :: nomsd, typesd
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
!
! ----------------------------------------------------------------------
!  BUT : "COMPLETER" LE CALCUL D'UNE STRUCTURE DE DONNEES INCOMPLETEMENT
!        CALCULEE  DU FAIT DE L'UTILISATION DE CALCULS PARALLELES (MPI)
!
!  LA ROUTINE ECHANGE LES MORCEAUX CALCULES PARTIELLEMENT SUR LES
!  DIFFERENTS PROCESSEURS (MPI_ALL_REDUCE)
!
! ----------------------------------------------------------------------
! IN TYPESD (K*) :  TYPE DE LA SD A COMPLETER
! IN NOMSD  (K*) :  NOM DE LA SD A COMPLETER
! ----------------------------------------------------------------------
    character(len=24) :: noms2, types2
    character(len=19) :: k19
    character(len=8) :: kmpic
    integer :: ifm, niv, ibid, jcelk, iexi, jrefa, jnoli
! ----------------------------------------------------------------------
!
    call jemarq()
    call infniv(ifm, niv)
!
    noms2 = nomsd
    types2 = typesd
!
!
    if (types2 .eq. 'CHAM_ELEM') then
!     ----------------------------------
        k19=noms2
        call dismoi('F', 'MPI_COMPLET', k19, 'CHAM_ELEM', ibid,&
                    kmpic, ibid)
        if (kmpic .eq. 'OUI') goto 9999
        call mpicm2('MPI_SUM', k19//'.CELV')
        call jeveuo(k19//'.CELK', 'E', jcelk)
        zk24(jcelk-1+7)='MPI_COMPLET'
!
!
    else if (types2.eq.'RESUELEM') then
!     ----------------------------------
        k19=noms2
        call dismoi('F', 'MPI_COMPLET', k19, 'RESUELEM', ibid,&
                    kmpic, ibid)
        if (kmpic .eq. 'OUI') goto 9999
        call mpicm2('MPI_SUM', k19//'.RESL')
        call jeveuo(k19//'.NOLI', 'E', jnoli)
        zk24(jnoli-1+3)='MPI_COMPLET'
!
!
    else if (types2.eq.'MATR_ASSE') then
!     ----------------------------------
        k19=noms2
        call dismoi('F', 'MPI_COMPLET', k19, 'MATR_ASSE', ibid,&
                    kmpic, ibid)
        if (kmpic .eq. 'OUI') goto 9999
        call mpicm2('MPI_SUM', k19//'.VALM')
!
        call jeexin(k19//'.CCVA', iexi)
        if (iexi .gt. 0) call mpicm2('MPI_SUM', k19//'.CCVA')
!
        call jeveuo(k19//'.REFA', 'E', jrefa)
        zk24(jrefa-1+11)='MPI_COMPLET'
!
    else
        ASSERT(.false.)
    endif
!
9999  continue
    call jedema()
end subroutine
