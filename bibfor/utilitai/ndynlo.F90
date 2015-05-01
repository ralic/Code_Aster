function ndynlo(sddyna, chainz)
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
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/ndynin.h"
    aster_logical :: ndynlo
    character(len=19) :: sddyna
    character(len=*) :: chainz
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (UTILITAIRE)
!
! INTERROGE SDDYNA POUR RENVOYER UN BOOLEEN
!
! ----------------------------------------------------------------------
!
!
! OUT NDYNLO : TRUE SI <CHAINE> FAIT PARTIE DES PROPRIETES DE LA SD
!               FALSE SINON
! IN  SDDYNA : NOM DE LA SD DEDIEE A LA DYNAMIQUE
! IN  CHAINE : PROPRIETE EVENTUELLE DE LA SD DYNAC
!                    = 'DIFF_CENT','TCHAMWA','NEWMARK','THETA_METHODE',
!                      'HHT_NON_MODIFIE','HHT_MODIFIE','KRENK',
!                      'IMPLICITE', 'EXPLICITE',
!                       'STATIQUE', 'DYNAMQIUE'
!                      'MAT_AMORT','MASS_DIAG',
!                      'MULTI_APPUI','AMOR_MODAL','PROJ_MODAL',
!
!
!
!
    character(len=24) :: tsch, losd
    integer :: jtsch, jlosd
    character(len=24) :: chaine
    character(len=16) :: typsch
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    ndynlo = .false.
    chaine = chainz
!
! --- ACCES OBJET PRINCIPAL SDDYNA
!
    if (sddyna .eq. ' ') then
        if (chaine(1:8) .eq. 'STATIQUE') then
            ndynlo = .true.
        else
            ndynlo = .false.
        endif
        goto 9999
    else
        tsch = sddyna(1:15)//'.TYPE_SCH'
        call jeveuo(tsch, 'L', jtsch)
    endif
    typsch = zk16(jtsch+1-1)
!
    if (typsch(1:8) .eq. 'STATIQUE') then
        if (chaine(1:8) .eq. 'STATIQUE') then
            ndynlo = .true.
        else
            ndynlo = .false.
        endif
        goto 9999
    else
        if (chaine(1:9) .eq. 'DYNAMIQUE') then
            ndynlo = .true.
            goto 9999
        endif
    endif
!
    losd = sddyna(1:15)//'.INFO_SD'
    call jeveuo(losd, 'L', jlosd)
!
    if (chaine(1:8) .eq. 'STATIQUE') then
        ndynlo = .false.
    else if (chaine(1:9).eq.'DIFF_CENT') then
        if (zk16(jtsch+7-1)(1:12) .eq. 'DIFF_CENTREE') then
            ndynlo = .true.
        endif
    else if (chaine(1:7).eq.'TCHAMWA') then
        if (zk16(jtsch+8-1)(1:7) .eq. 'TCHAMWA') then
            ndynlo = .true.
        endif
    else if (chaine(1:7).eq.'NEWMARK') then
        if (zk16(jtsch+2-1)(1:7) .eq. 'NEWMARK') then
            ndynlo = .true.
        endif
!
    else if (chaine(1:15).eq.'FAMILLE_NEWMARK') then
        if ((zk16(jtsch+2-1)(1:7).eq.'NEWMARK') .or.&
            (zk16(jtsch+7-1)( 1:12).eq.'DIFF_CENTREE') .or.&
            (zk16(jtsch+8-1)(1:7) .eq.'TCHAMWA') .or.&
            (zk16(jtsch+5-1)(1:11).eq.'HHT_COMPLET') .or. (zk16(jtsch+3-1)(1:3).eq.'HHT')) then
            ndynlo = .true.
        else
            ndynlo = .false.
        endif
!
    else if (chaine(1:13).eq.'THETA_METHODE') then
        if (zk16(jtsch+4-1)(1:13) .eq. 'THETA_METHODE') then
            if (chaine(14:18) .eq. '_DEPL') then
                if (ndynin(sddyna,'FORMUL_DYNAMIQUE') .eq. 1) then
                    ndynlo = .true.
                else if (ndynin(sddyna,'FORMUL_DYNAMIQUE').eq.2) then
                    ndynlo = .false.
                else
                    ASSERT(.false.)
                endif
            else if (chaine(14:18).eq.'_VITE') then
                if (ndynin(sddyna,'FORMUL_DYNAMIQUE') .eq. 2) then
                    ndynlo = .true.
                else if (ndynin(sddyna,'FORMUL_DYNAMIQUE').eq.1) then
                    ndynlo = .false.
                else
                    ASSERT(.false.)
                endif
            else
                ndynlo = .true.
            endif
        else
            ndynlo = .false.
        endif
!
!
    else if (chaine(1:11).eq.'HHT_COMPLET') then
        if (zk16(jtsch+5-1)(1:11) .eq. 'HHT_COMPLET') then
            ndynlo = .true.
        endif
    else if (chaine(1:3).eq.'HHT') then
        if (zk16(jtsch+3-1)(1:3) .eq. 'HHT') then
            ndynlo = .true.
        endif
    else if (chaine(1:5).eq.'KRENK') then
        if (zk16(jtsch+9-1)(1:5) .eq. 'KRENK') then
            ndynlo = .true.
        endif
    else if (chaine(1:9).eq.'IMPLICITE') then
        if (zk16(jtsch+2-1)(1:7) .eq. 'NEWMARK' .or. zk16(jtsch+4-1)(1: 13) .eq.&
            'THETA_METHODE' .or. zk16(jtsch+5-1)(1:11) .eq. 'HHT_COMPLET' .or.&
            zk16(jtsch+3-1)(1:3) .eq. 'HHT' .or. zk16(jtsch+9-1)(1:5) .eq. 'KRENK') then
            ndynlo = .true.
        endif
    else if (chaine(1:9).eq.'EXPLICITE') then
        if (zk16(jtsch+7-1)(1:12) .eq. 'DIFF_CENTREE' .or. zk16(jtsch+8- 1)(1:7) .eq.&
            'TCHAMWA') then
            ndynlo = .true.
        endif
!
!
!
    else if (chaine(1:9).eq.'MAT_AMORT') then
        ndynlo = zl(jlosd+1-1)
    else if (chaine(1:11).eq.'MULTI_APPUI') then
        ndynlo = zl(jlosd+2-1)
    else if (chaine(1:10).eq.'AMOR_MODAL') then
        ndynlo = zl(jlosd+3-1)
    else if (chaine(1:9).eq.'MASS_DIAG') then
        ndynlo = zl(jlosd+4-1)
    else if (chaine(1:10).eq.'PROJ_MODAL') then
        ndynlo = zl(jlosd+5-1)
    else if (chaine(1:9 ).eq.'IMPE_ABSO') then
        ndynlo = zl(jlosd+6-1)
    else if (chaine(1:10).eq.'ONDE_PLANE') then
        ndynlo = zl(jlosd+7-1)
    else if (chaine(1:12).eq.'FORCE_FLUIDE') then
        ndynlo = zl(jlosd+8-1)
    else if (chaine(1:9).eq.'EXPL_GENE') then
        ndynlo = zl(jlosd+9-1)
    else if (chaine(1:6).eq.'NREAVI') then
        ndynlo = zl(jlosd+12-1)
    else if (chaine(1:13).eq.'RAYLEIGH_KTAN') then
        ndynlo = zl(jlosd+13-1)
    else if (chaine(1:15).eq.'COEF_MASS_SHIFT') then
        ndynlo = zl(jlosd+14-1)
    else if (chaine(1:8).eq.'VECT_ISS') then
        ndynlo = zl(jlosd+15-1)
    else if (chaine(1:13).eq.'AMOR_RAYLEIGH') then
        ndynlo = zl(jlosd+16-1)
    else if (chaine(1:11).eq.'FORMUL_DEPL') then
        if (ndynin(sddyna,'FORMUL_DYNAMIQUE') .eq. 1) then
            ndynlo = .true.
        else
            ndynlo = .false.
        endif
    else if (chaine(1:11).eq.'FORMUL_VITE') then
        if (ndynin(sddyna,'FORMUL_DYNAMIQUE') .eq. 2) then
            ndynlo = .true.
        else
            ndynlo = .false.
        endif
    else if (chaine(1:11).eq.'FORMUL_ACCE') then
        if (ndynin(sddyna,'FORMUL_DYNAMIQUE') .eq. 3) then
            ndynlo = .true.
        else
            ndynlo = .false.
        endif
    else if (chaine.eq.'MULTI_PAS') then
        if ((zk16(jtsch+5-1)(1:11).eq.'HHT_COMPLET')) then
            ndynlo = .true.
            goto 9999
        else if ((zk16(jtsch+4-1)(1:13).eq.'THETA_METHODE')) then
            ndynlo = .true.
            goto 9999
        else if ((zk16(jtsch+9-1)(1:5).eq.'KRENK')) then
            ndynlo = .true.
            goto 9999
        else
            ndynlo = .false.
        endif
!
        if ((ndynin(sddyna,'FORMUL_DYNAMIQUE').eq.2) .and.&
            (&
            ( zk16( jtsch+4-1)(1:13).eq.'THETA_METHODE') .or.&
            (zk16(jtsch+9-1)(1:5) .eq.'KRENK')&
            )) then
            ndynlo = .true.
        else
            ndynlo = .false.
        endif
!
    else
        ASSERT(.false.)
    endif
!
9999 continue
!
    call jedema()
!
end function
