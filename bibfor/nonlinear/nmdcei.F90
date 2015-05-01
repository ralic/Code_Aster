subroutine nmdcei(sddisc, numins, newins, nbini, nbins,&
                  typext, dt0)
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
#include "jeveux.h"
#include "asterc/r8maem.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/juveca.h"
#include "asterfort/utdidt.h"
    integer :: nbins, numins, nbini
    character(len=19) :: sddisc
    real(kind=8) :: newins(nbins)
    real(kind=8) :: dt0
    character(len=4) :: typext
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - DECOUPE)
!
! EXTENSION DE LA LISTE D'INSTANTS
!
! ----------------------------------------------------------------------
!
!
! IN  SDDISC : SD DISCRETISATION
! IN  NEWINS : VECTEUR DES NOUVEAUX INSTANTS A AJOUTER (LONGUEUR NBINS)
! IN  NUMINS : NUMERO D'INSTANT
! IN  NBINS  : NOMBRE DE PAS DE TEMPS A INSERER
! IN  NBINI  : ANCIEN NOMBRE D'INSTANTS
! IN  TYPEXT : TYPE D'EXTENSION
!               'ADAP' - ADAPTATION DU PAS DE TEMPS
!               'DECO' - DECOUPAGE DU PAS DE TEMPS
! OUT DT0    : NOUVEAU DELTAT
!
!
!
!
    integer :: ipas, nbnew, ibid
    character(len=24) :: tpsdit
    integer :: jtemps
    character(len=8) :: k8bid
    real(kind=8) :: inst, deltat, r8bid, dtmin
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES SD LISTE D'INSTANTS
!
    tpsdit = sddisc(1:19)//'.DITR'
!
! --- NOUVEAU NOMBRE D'INSTANTS
!
    nbnew = nbini + nbins
!
! --- ALLONGEMENT DE LA LISTE D'INSTANTS
!
    call juveca(tpsdit, nbnew)
    call jeveuo(tpsdit, 'E', jtemps)
!
! --- RECOPIE DE LA PARTIE HAUTE DE LA LISTE
!
    do 10 ipas = nbini, numins+1, -1
        inst = zr(jtemps+ipas-1)
        zr(jtemps+ipas+nbins-1) = inst
10  end do
!
! --- INSERTION DES INSTANTS SUPPLEMENTAIRES
!
    do 20 ipas = 1, nbins
        inst = newins(ipas)
        if (typext .eq. 'DECO') then
            zr(jtemps+ipas+numins-1) = inst
        else if (typext.eq.'ADAP') then
            zr(jtemps+ipas+numins) = inst
        else
            ASSERT(.false.)
        endif
20  end do
!
! --- VALEUR DU NOUVEAU DELTAT
!
    dt0 = zr(jtemps-1+numins+1)-zr(jtemps-1+numins)
!
! --- NOUVEL INTERVALLE DE TEMPS MINIMAL : DTMIN
!
    dtmin = r8maem()
    do 25 ipas = 1, nbnew-1
        deltat = zr(jtemps-1+ipas+1) - zr(jtemps-1+ipas)
        dtmin = min(deltat,dtmin)
25  end do
!
! --- ENREGISTREMENT INFOS
!
    call utdidt('E', sddisc, 'LIST', ibid, 'NBINST',&
                r8bid, nbnew, k8bid)
    call utdidt('E', sddisc, 'LIST', ibid, 'DTMIN',&
                dtmin, ibid, k8bid)
!
    call jedema()
!
end subroutine
