subroutine ace_crea_carte(infdonn,infcarte)
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
!
! --------------------------------------------------------------------------------------------------
!   AFFE_CARA_ELEM
!
!       Création automatique des cartes utilisées par AFFE_CARA_ELEM
!
!           Utilisation de jeveut
!
! --------------------------------------------------------------------------------------------------
! person_in_charge: jean-luc.flejou at edf.fr
!
    use cara_elem_parameter_module
    use cara_elem_info_type
    use cara_elem_carte_type
    implicit none
    type (cara_elem_info)  :: infdonn
    type (cara_elem_carte) :: infcarte(*)
!
#include "jeveux.h"
#include "asterfort/alcart.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/infdis.h"
#include "asterfort/in_liste_entier.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveut.h"
#include "asterfort/nocart.h"
!
! --------------------------------------------------------------------------------------------------
    integer             :: ii, jj, ixci, ibid, indx
    real(kind=8)        :: r8bid
    character(len=6)    :: kjj
    character(len=8)    :: nomu,noma,k8bid
    character(len=24)   :: tmpcinf, tmpvinf
!
    integer             :: adr_cmp, adr_val, nbr_cmp
    character(len=19)   :: nom_carte
!
    character(len=5), parameter :: kma(3) =(/ 'K    ', 'M    ', 'A    ' /)
! --------------------------------------------------------------------------------------------------
    call jemarq()
!
    nomu = infdonn%nomu
    noma = infdonn%maillage
!
    do ii = 1 , ACE_NB_CARTE
        nom_carte = nomu//ACE_CARTE(1 + (ii-1)*ACE_NB_CARTE_CMP)
        tmpcinf = nom_carte//'.NCMP'
        tmpvinf = nom_carte//'.VALV'
!       Si la carte n'existe pas on la crée
        call jeexin(tmpcinf, ixci)
        if (ixci .eq. 0) then
            call alcart('G', nom_carte, noma, ACE_CARTE(2 + (ii-1)*ACE_NB_CARTE_CMP))
        else
            ASSERT( .false. )
        endif
        call jeveut(tmpcinf, 'E', adr_cmp)
        call jeveut(tmpvinf, 'E', adr_val)
!       Remplissage des cartes par les valeurs par défaut
        nbr_cmp = 0
        if ( ii .eq. ACE_CAR_DINFO ) then
!           Par défaut pour K, M, A : repère global, matrice symétrique, pas affectée
            call infdis('DIMC', nbr_cmp, r8bid, k8bid)
            do jj = 1, 3
                zk8(adr_cmp+jj-1) = 'REP'//kma(jj)
                call infdis('INIT', ibid, zr(adr_val+jj-1), zk8(adr_cmp+jj-1))
                zk8(adr_cmp+jj+2) = 'SYM'//kma(jj)
                call infdis('INIT', ibid, zr(adr_val+jj+2), zk8(adr_cmp+jj+2))
                zk8(adr_cmp+jj+5) = 'DIS'//kma(jj)
                call infdis('INIT', ibid, zr(adr_val+jj+5), zk8(adr_cmp+jj+5))
            enddo
            zk8(adr_cmp+9)  = 'ETAK    '
            call infdis('INIT', ibid, zr(adr_val+9), zk8(adr_cmp+9))
            zk8(adr_cmp+10) = 'TYDI    '
            call infdis('INIT', ibid, zr(adr_val+10), zk8(adr_cmp+10))
            call nocart(nom_carte, 1, nbr_cmp)
        else if ( in_liste_entier(ii, [ACE_CAR_DISCK,ACE_CAR_DISCM,ACE_CAR_DISCA],indx) ) then
!           Affectation systématique de valeurs nulles dans les cartes pour pouvoir calculer
!           les matrices K, M, A dans tous les cas dans le repère global par défaut.
!
!           Cas particulier de l'amortissement, pour la dynamique
!               - La routine "nmamab" fait un dismoi(EXI_AMOR, CARA_ELEM) et retourne NON si
!                 toutes les valeurs sont nulles ==> Pas d'amortissement ou amortissement NUL
!               - La routine "ndlect" appelle "nmamab"
!           On peut donc créer et garder le "nocart" sur tout le maillage
            call infdis('DMXM', nbr_cmp, r8bid, k8bid)
            do jj = 1, nbr_cmp
                call codent(jj, 'G', kjj)
                zr(adr_val+jj-1) = 0.d0
                zk8(adr_cmp+jj-1) = kma(indx)(1:1)//kjj
            enddo
            call nocart(nom_carte, 1, nbr_cmp)
        endif
        infcarte(ii)%nom_carte = nom_carte
        infcarte(ii)%nbr_cmp   = nbr_cmp
        infcarte(ii)%adr_cmp   = adr_cmp
        infcarte(ii)%adr_val   = adr_val
    enddo
!
    call jedema()
end subroutine

